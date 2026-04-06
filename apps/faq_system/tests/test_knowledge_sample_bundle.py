"""FAQ 多文書サンプル bundle の抽出・関連展開テスト."""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from apps.faq_system.backend.mcp.backends.base import (
    RetrievedDocument,
    RetrievalQuery,
    RetrievalResult,
)
from apps.faq_system.backend.mcp.tools.knowledge_search import KnowledgeSearchTool
from apps.faq_system.backend.rag.parsers import FileParser


_FIXTURE_DIR = Path(__file__).resolve().parent / "fixtures" / "knowledge_samples" / "hr_travel_bundle"


def test_sample_bundle_files_exist() -> None:
    """サンプル bundle の必須ファイルが揃っている."""
    expected_files = {
        "travel_policy_official_2025.pdf",
        "travel_faq_exceptions.docx",
        "travel_allowance_matrix.xlsx",
        "travel_policy_update_notice_2026.txt",
        "query_evaluation.json",
    }
    assert {path.name for path in _FIXTURE_DIR.iterdir() if path.is_file()} >= expected_files


@pytest.mark.parametrize(
    ("filename", "expected_snippets"),
    [
        (
            "travel_policy_official_2025.pdf",
            ["Meal allowance", "after 22:00", "latest effective notice wins"],
        ),
        (
            "travel_faq_exceptions.docx",
            ["hotel payment statement", "Duplicate meal claims are not allowed", "latest update notice"],
        ),
        (
            "travel_allowance_matrix.xlsx",
            ["| Manager | Tokyo | Yes | 17000", "| Director | 4000 | 8000 |"],
        ),
        (
            "travel_policy_update_notice_2026.txt",
            ["19000 JPY", "2026-04-01", "payment statement"],
        ),
    ],
)
def test_sample_bundle_parser_extracts_required_facts(
    filename: str,
    expected_snippets: list[str],
) -> None:
    """4 形式 parser がサンプル文書の必須フレーズを抽出する."""
    result = FileParser.parse_file(_FIXTURE_DIR / filename)
    for snippet in expected_snippets:
        assert snippet in result.content


def test_query_evaluation_references_existing_sources() -> None:
    """評価クエリ定義が既存サンプル文書だけを参照している."""
    payload = json.loads((_FIXTURE_DIR / "query_evaluation.json").read_text("utf-8"))
    available_sources = {path.name for path in _FIXTURE_DIR.iterdir() if path.is_file()}
    assert len(payload) >= 6
    for item in payload:
        assert item["expected_facts"]
        assert item["required_sources"]
        for source_name in item["required_sources"]:
            assert source_name in available_sources


class _FakePipeline:
    """group expansion を検証する簡易 pipeline."""

    async def execute(self, query: RetrievalQuery) -> RetrievalResult:
        group_id = query.filters.get("document_group_id")
        if group_id == "group-hr-travel":
            return RetrievalResult(
                query=query.query,
                total_found=3,
                documents=[
                    RetrievedDocument(
                        doc_id="doc-primary",
                        content="Primary content",
                        score=0.99,
                        source="travel_policy_official_2025.pdf",
                        metadata={"document_group_id": group_id},
                    ),
                    RetrievedDocument(
                        doc_id="doc-update",
                        content="Update notice content",
                        score=0.88,
                        source="travel_policy_update_notice_2026.txt",
                        metadata={"document_group_id": group_id},
                    ),
                    RetrievedDocument(
                        doc_id="doc-faq",
                        content="FAQ content",
                        score=0.81,
                        source="travel_faq_exceptions.docx",
                        metadata={"document_group_id": group_id},
                    ),
                ],
            )
        return RetrievalResult(query=query.query, total_found=0, documents=[])


@pytest.mark.asyncio
async def test_related_expansion_returns_group_documents_without_duplicates() -> None:
    """同一 document_group_id の補助文書が重複なく展開される."""
    tool = KnowledgeSearchTool()
    primary_docs = [
        RetrievedDocument(
            doc_id="doc-primary",
            content="Primary content",
            score=0.99,
            source="travel_policy_official_2025.pdf",
            metadata={"document_group_id": "group-hr-travel"},
        )
    ]

    related_docs = await tool._expand_related_documents(
        primary_docs=primary_docs,
        pipeline=_FakePipeline(),
        query_text="Tokyo manager lodging cap after the update",
        primary_top_k=1,
    )

    assert [doc.doc_id for doc in related_docs] == ["doc-update", "doc-faq"]
    assert all(doc.metadata["expanded_from_group"] == "group-hr-travel" for doc in related_docs)
