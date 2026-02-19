"""Framework API prefix 契約テスト.

旧 prefix は 404、新 prefix は 404 以外を返すことを検証する。
"""

from __future__ import annotations

import pytest
from fastapi.testclient import TestClient


@pytest.mark.parametrize(
    ("method", "new_path", "old_path", "payload"),
    [
        ("GET", "/api/studios/framework/agents", "/api/agents", None),
        ("GET", "/api/studios/framework/skills", "/api/skills", None),
        ("GET", "/api/studios/framework/rag/overview", "/api/rag/overview", None),
        ("GET", "/api/studios/framework/mcp/config", "/api/mcp/config", None),
        ("GET", "/api/studios/framework/gallery/featured", "/api/gallery/featured", None),
        ("GET", "/api/studios/framework/components", "/api/components", None),
        ("GET", "/api/studios/framework/dashboard/demo-tenant", "/api/dashboard/demo-tenant", None),
        ("POST", "/api/studios/framework/publish/deploy", "/api/publish/deploy", {}),
    ],
)
def test_framework_prefix_contract(
    phase3_test_client: TestClient,
    method: str,
    new_path: str,
    old_path: str,
    payload: dict | None,
) -> None:
    """旧 prefix を廃止し、新 prefix を正規経路として維持する."""
    client = phase3_test_client

    if method == "GET":
        old_resp = client.get(old_path)
        new_resp = client.get(new_path)
    else:
        old_resp = client.post(old_path, json=payload)
        new_resp = client.post(new_path, json=payload)

    assert old_resp.status_code == 404
    assert new_resp.status_code != 404
