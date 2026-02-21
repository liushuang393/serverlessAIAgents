"""Plugin manifest schema tests."""

from __future__ import annotations

import json
from pathlib import Path

from apps.platform.schemas.plugin_manifest_schemas import PluginManifest


def test_plugin_manifest_validation() -> None:
    """標準 plugin_manifest フィールドを検証できる."""
    manifest = PluginManifest.model_validate(
        {
            "id": "official.test-pack",
            "version": "1.0.0",
            "type": "tool",
            "capabilities": ["connector.test"],
            "risk_tier": "medium",
            "side_effects": ["network.egress"],
            "required_permissions": ["network.egress"],
            "signature": {
                "algorithm": "ed25519",
                "issuer": "agentflow-official",
                "key_id": "key-1",
            },
            "compatibility": {
                "kernel": ">=2.0.0",
                "product_lines": ["faq"],
            },
            "tests_required": ["unit", "integration"],
        },
    )
    assert manifest.id == "official.test-pack"
    assert manifest.risk_tier == "medium"
    assert manifest.compatibility.kernel == ">=2.0.0"


def test_plugin_manifests_kernel_compatibility_matches_v2() -> None:
    """公式 plugin manifest の kernel 互換が 2.0.0 以上であること."""
    root = Path.cwd()
    manifests = sorted((root / "plugins").glob("*/plugin_manifest.json"))
    assert len(manifests) >= 1

    for path in manifests:
        raw = json.loads(path.read_text("utf-8"))
        manifest = PluginManifest.model_validate(raw)
        assert manifest.compatibility.kernel.startswith(">=")
        assert manifest.compatibility.kernel == ">=2.0.0"
