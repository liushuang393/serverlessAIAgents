#!/usr/bin/env python3
"""FAQ sample bundle の live 評価スクリプト."""

from __future__ import annotations

import json
import os
import sys
import time
import uuid
from pathlib import Path
from typing import Any

import httpx


BASE_URL = os.getenv("FAQ_EVAL_BASE_URL", "http://127.0.0.1:8005")
BUNDLE_DIR = Path(__file__).resolve().parent / "fixtures" / "knowledge_samples" / "hr_travel_bundle"
BUNDLE_FILES = [
    "travel_policy_official_2025.pdf",
    "travel_faq_exceptions.docx",
    "travel_allowance_matrix.xlsx",
    "travel_policy_update_notice_2026.txt",
]
QUERY_EVALUATION_PATH = BUNDLE_DIR / "query_evaluation.json"


def _assert_ok(response: httpx.Response, context: str) -> None:
    if response.is_success:
        return
    raise RuntimeError(f"{context} failed: {response.status_code} {response.text}")


def _contains_all(text: str, values: list[str]) -> bool:
    lowered = text.lower()
    return all(value.lower() in lowered for value in values)


def _contains_any(text: str, values: list[str]) -> bool:
    lowered = text.lower()
    return any(value.lower() in lowered for value in values)


def _missing_values(text: str, values: list[str]) -> list[str]:
    lowered = text.lower()
    return [value for value in values if value.lower() not in lowered]


def main() -> int:
    collection_name = f"live_eval_hr_travel_{uuid.uuid4().hex[:10]}"
    document_group_id = f"bundle-{uuid.uuid4().hex[:8]}"
    queries = json.loads(QUERY_EVALUATION_PATH.read_text(encoding="utf-8"))

    with httpx.Client(base_url=BASE_URL, timeout=120.0) as client:
        login = client.post(
            "/api/auth/login",
            json={"username": "admin", "password": "admin123"},
        )
        _assert_ok(login, "login")
        token = login.json()["access_token"]
        client.headers.update({"Authorization": f"Bearer {token}"})

        create = client.post(
            "/api/collections",
            json={
                "collection_name": collection_name,
                "display_name": "HR Travel Live Eval",
                "description": "Multi-document live evaluation bundle",
                "chunk_strategy": "sentence",
                "chunk_size": 500,
                "chunk_overlap": 80,
                "retrieval_method": "hybrid",
                "reranker": "bm25",
                "top_k": 8,
                "min_similarity": 0.15,
            },
        )
        _assert_ok(create, "create collection")

        try:
            for filename in BUNDLE_FILES:
                with (BUNDLE_DIR / filename).open("rb") as handle:
                    response = client.post(
                        f"/api/collections/{collection_name}/documents",
                        data={
                            "auto_index": "true",
                            "document_group_id": document_group_id,
                            "scenario_id": "hr_travel_policy_2026",
                            "tags": "travel,policy,reimbursement",
                        },
                        files={"file": (filename, handle)},
                    )
                _assert_ok(response, f"upload {filename}")

            deadline = time.time() + 180
            while time.time() < deadline:
                listed = client.get(f"/api/collections/{collection_name}/documents")
                _assert_ok(listed, "list documents")
                documents = listed.json().get("documents", [])
                statuses = [doc.get("status") for doc in documents]
                if len(documents) == 4 and all(status == "indexed" for status in statuses):
                    break
                time.sleep(2)
            else:
                raise RuntimeError("documents did not reach indexed state within timeout")

            total = len(queries)
            passed = 0
            print(f"Collection: {collection_name}")
            print(f"Document group: {document_group_id}")
            print(f"Queries: {total}")
            print("")

            for query_spec in queries:
                response = client.post(
                    f"/api/collections/{collection_name}/test-query",
                    json={"query": query_spec["query"], "top_k": 8},
                )
                _assert_ok(response, f"query {query_spec['id']}")
                payload = response.json()
                answer = str(payload.get("answer", ""))
                documents = payload.get("documents", [])
                related_documents = payload.get("related_documents", [])
                diagnostics = payload.get("diagnostics", {})
                combined_text = "\n".join(
                    [answer]
                    + [str(doc.get("content", "")) for doc in documents]
                    + [str(doc.get("content", "")) for doc in related_documents]
                )
                sources = "\n".join(
                    [str(doc.get("source", "")) for doc in documents]
                    + [str(doc.get("source", "")) for doc in related_documents]
                )

                facts_ok = _contains_all(combined_text, query_spec["expected_facts"])
                sources_ok = _contains_all(sources, query_spec["required_sources"])
                forbidden_hit = _contains_any(answer, query_spec["forbidden_facts"])
                missing_expected_facts = _missing_values(answer, query_spec["expected_facts"])
                missing_required_sources = _missing_values(sources, query_spec["required_sources"])
                query_passed = facts_ok and sources_ok and not forbidden_hit
                passed += int(query_passed)

                failure_classification = "passed"
                if not query_passed:
                    if missing_required_sources:
                        failure_classification = (
                            "related miss"
                            if query_spec.get("requires_related") and not related_documents
                            else "retrieval miss"
                        )
                    else:
                        failure_classification = "synthesis miss"

                print(f"[{'PASS' if query_passed else 'FAIL'}] {query_spec['id']}")
                print(f"  query: {query_spec['query']}")
                print(f"  facts_ok={facts_ok} sources_ok={sources_ok} forbidden_hit={forbidden_hit}")
                print(f"  retrieval_mode: {diagnostics.get('retrieval_method', 'unknown')}")
                print(f"  primary_sources: {diagnostics.get('primary_sources', [doc.get('source') for doc in documents])}")
                print(
                    "  related_sources: "
                    f"{diagnostics.get('related_sources', [doc.get('source') for doc in related_documents])}"
                )
                print(f"  missing_expected_facts: {missing_expected_facts}")
                print(f"  missing_required_sources: {missing_required_sources}")
                print(f"  failure_classification: {failure_classification}")
                print(f"  answer: {answer[:240]}")
                print(
                    "  sources: "
                    f"{[doc.get('source') for doc in documents + related_documents]}"
                )
                print("")

            print(f"Summary: {passed}/{total} passed")
            return 0 if passed == total else 1
        finally:
            client.delete(f"/api/collections/{collection_name}")


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Exception as exc:  # pragma: no cover - live script
        print(f"ERROR: {exc}", file=sys.stderr)
        raise
