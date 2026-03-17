"""ContentDraftAgent - コンテンツドラフト生成."""

from __future__ import annotations

from typing import Any

from apps.Legacy_modernization_geo_platform.agents._models import (
    ContentDraftInput,
    ContentDraftOutput,
)
from apps.Legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    ContentDraftArtifact,
    ContentDraftPage,
    FAQEntry,
)

from kernel.agents.resilient_agent import ResilientAgent


class ContentDraftAgent(ResilientAgent[ContentDraftInput, ContentDraftOutput]):
    """決定論的ページドラフトを生成."""

    name = "ContentDraft"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: ContentDraftInput) -> ContentDraftOutput:
        request = input_data.request
        task_id = input_data.task_id
        blueprint = input_data.blueprint
        evidence_matrix = input_data.evidence_matrix
        legacy_semantics = input_data.legacy_semantics
        pages: list[ContentDraftPage] = []
        question = (
            blueprint.pages[0].primary_question
            if blueprint.pages
            else "段階移行は可能か"
        )
        evidence_lines = [
            f"- 根拠: {entry.title} / {entry.publisher} / {entry.source_url}"
            for entry in evidence_matrix.entries[:4]
        ]
        semantics_lines = [
            f"- {item}" for item in legacy_semantics.business_rules
        ]
        for page in blueprint.pages:
            body_markdown = "\n".join(
                [
                    f"# {page.title}",
                    "",
                    "## 課題の背景",
                    "旧システム刷新では、保守要員不足・調査難易度・一括刷新リスクが同時に発生します。",
                    "",
                    "## 段階移行の考え方",
                    "既存資産を可視化し、業務ルールを抽出してから API・バッチ・画面を順次刷新します。",
                    "",
                    "## 比較観点",
                    "全面刷新と比較すると、段階刷新は業務停止リスクを抑えつつ投資判断を分割できます。",
                    "",
                    "## 根拠サマリー",
                    *evidence_lines,
                    "",
                    "## 業務意味の維持ポイント",
                    *semantics_lines,
                    "",
                    "## 次の一手",
                    f"{question} を起点に、対象資産の棚卸しと優先順位付けから着手します。",
                ],
            )
            faq_entries = [
                FAQEntry(
                    question=question,
                    answer="段階移行は現行業務の安定運用と並行して実施できます。",
                ),
                FAQEntry(
                    question="一括刷新との違いは何ですか",
                    answer="投資とリスクを工程ごとに分割できる点です。",
                ),
                FAQEntry(
                    question="どこから調査を始めるべきですか",
                    answer="高頻度バッチ、帳票、外部連携を優先して可視化します。",
                ),
            ]
            json_ld = {
                "@context": "https://schema.org",
                "@type": "FAQPage",
                "inLanguage": "ja-JP",
                "mainEntity": [
                    {
                        "@type": "Question",
                        "name": entry.question,
                        "acceptedAnswer": {
                            "@type": "Answer",
                            "text": entry.answer,
                        },
                    }
                    for entry in faq_entries
                ],
            }
            pages.append(
                ContentDraftPage(
                    slug=page.slug,
                    title=page.title,
                    summary=(
                        f"{(request.targets.legacy_stacks or ['旧システム'])[0]} の刷新を、"
                        "診断・業務ルール抽出・段階移行で進めるための実務ガイド。"
                    ),
                    body_markdown=body_markdown,
                    cta=page.cta,
                    faq_entries=faq_entries,
                    json_ld=json_ld,
                ),
            )
        artifact = ContentDraftArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:content",
                stage="content_composition",
            ),
            pages=pages,
            target_language=blueprint.target_language,
            evidence=[
                item.model_dump(mode="json")
                for item in evidence_matrix.entries[:6]
            ],
            unknowns=list(evidence_matrix.unknowns),
        )
        return ContentDraftOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> ContentDraftInput:
        return ContentDraftInput.model_validate(input_data)
