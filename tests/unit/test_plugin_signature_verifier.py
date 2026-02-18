# -*- coding: utf-8 -*-
"""Plugin sidecar 署名検証の単体テスト."""

from __future__ import annotations

import base64
import json
from pathlib import Path

from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PrivateKey
from cryptography.hazmat.primitives.serialization import Encoding, PublicFormat

from agentflow.governance.plugin_signature import (
    PluginSignatureVerifier,
    canonical_manifest_bytes,
)


def _manifest_template(*, algorithm: str = "ed25519", key_id: str = "k1") -> dict[str, object]:
    return {
        "id": "official.test-pack",
        "version": "1.0.0",
        "type": "tool",
        "capabilities": ["connector.test"],
        "risk_tier": "medium",
        "side_effects": ["network.egress"],
        "required_permissions": ["network.egress"],
        "signature": {
            "algorithm": algorithm,
            "issuer": "test-issuer",
            "key_id": key_id,
        },
        "compatibility": {
            "kernel": ">=2.0.0",
            "product_lines": ["framework"],
        },
        "tests_required": ["unit"],
    }


def _write_manifest(path: Path, manifest: dict[str, object]) -> None:
    path.write_text(
        json.dumps(manifest, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )


def _write_trust_store(path: Path, public_key_base64: str) -> None:
    payload = {
        "test-issuer": {
            "k1": {
                "algorithm": "ed25519",
                "public_key_base64": public_key_base64,
            }
        }
    }
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def test_verify_manifest_signature_verified(tmp_path: Path) -> None:
    """正しい sidecar 署名を検証できる."""
    plugin_dir = tmp_path / "plugins" / "official.test-pack"
    plugin_dir.mkdir(parents=True)
    manifest_path = plugin_dir / "plugin_manifest.json"
    sig_path = plugin_dir / "plugin_manifest.sig"
    trust_store_path = tmp_path / "plugins" / "trust_store.json"

    private_key = Ed25519PrivateKey.generate()
    public_key = base64.b64encode(
        private_key.public_key().public_bytes(encoding=Encoding.Raw, format=PublicFormat.Raw),
    ).decode("ascii")
    manifest = _manifest_template()
    _write_manifest(manifest_path, manifest)
    signature = private_key.sign(canonical_manifest_bytes(manifest))
    sig_path.write_text(base64.b64encode(signature).decode("ascii") + "\n", encoding="utf-8")
    _write_trust_store(trust_store_path, public_key)

    verifier = PluginSignatureVerifier(trust_store_path=trust_store_path)
    result = verifier.verify_manifest(manifest=manifest, manifest_path=manifest_path)
    assert result.status == "verified"


def test_verify_manifest_signature_missing_sig(tmp_path: Path) -> None:
    """sidecar 不在は missing_sig."""
    plugin_dir = tmp_path / "plugins" / "official.test-pack"
    plugin_dir.mkdir(parents=True)
    manifest_path = plugin_dir / "plugin_manifest.json"
    trust_store_path = tmp_path / "plugins" / "trust_store.json"
    trust_store_path.parent.mkdir(parents=True, exist_ok=True)

    private_key = Ed25519PrivateKey.generate()
    public_key = base64.b64encode(
        private_key.public_key().public_bytes(encoding=Encoding.Raw, format=PublicFormat.Raw),
    ).decode("ascii")
    manifest = _manifest_template()
    _write_manifest(manifest_path, manifest)
    _write_trust_store(trust_store_path, public_key)

    verifier = PluginSignatureVerifier(trust_store_path=trust_store_path)
    result = verifier.verify_manifest(manifest=manifest, manifest_path=manifest_path)
    assert result.status == "missing_sig"


def test_verify_manifest_signature_bad_signature(tmp_path: Path) -> None:
    """公開鍵不一致は bad_signature."""
    plugin_dir = tmp_path / "plugins" / "official.test-pack"
    plugin_dir.mkdir(parents=True)
    manifest_path = plugin_dir / "plugin_manifest.json"
    sig_path = plugin_dir / "plugin_manifest.sig"
    trust_store_path = tmp_path / "plugins" / "trust_store.json"

    sign_key = Ed25519PrivateKey.generate()
    verify_key = Ed25519PrivateKey.generate()
    public_key = base64.b64encode(
        verify_key.public_key().public_bytes(encoding=Encoding.Raw, format=PublicFormat.Raw),
    ).decode("ascii")
    manifest = _manifest_template()
    _write_manifest(manifest_path, manifest)
    signature = sign_key.sign(canonical_manifest_bytes(manifest))
    sig_path.write_text(base64.b64encode(signature).decode("ascii") + "\n", encoding="utf-8")
    _write_trust_store(trust_store_path, public_key)

    verifier = PluginSignatureVerifier(trust_store_path=trust_store_path)
    result = verifier.verify_manifest(manifest=manifest, manifest_path=manifest_path)
    assert result.status == "bad_signature"


def test_verify_manifest_signature_parse_error_on_invalid_base64(tmp_path: Path) -> None:
    """sidecar base64 不正は parse_error."""
    plugin_dir = tmp_path / "plugins" / "official.test-pack"
    plugin_dir.mkdir(parents=True)
    manifest_path = plugin_dir / "plugin_manifest.json"
    sig_path = plugin_dir / "plugin_manifest.sig"
    trust_store_path = tmp_path / "plugins" / "trust_store.json"
    trust_store_path.parent.mkdir(parents=True, exist_ok=True)

    private_key = Ed25519PrivateKey.generate()
    public_key = base64.b64encode(
        private_key.public_key().public_bytes(encoding=Encoding.Raw, format=PublicFormat.Raw),
    ).decode("ascii")
    manifest = _manifest_template()
    _write_manifest(manifest_path, manifest)
    sig_path.write_text("not-base64!!!\n", encoding="utf-8")
    _write_trust_store(trust_store_path, public_key)

    verifier = PluginSignatureVerifier(trust_store_path=trust_store_path)
    result = verifier.verify_manifest(manifest=manifest, manifest_path=manifest_path)
    assert result.status == "parse_error"


def test_verify_manifest_signature_unsupported_algorithm(tmp_path: Path) -> None:
    """未対応アルゴリズムは unsupported_algorithm."""
    plugin_dir = tmp_path / "plugins" / "official.test-pack"
    plugin_dir.mkdir(parents=True)
    manifest_path = plugin_dir / "plugin_manifest.json"
    trust_store_path = tmp_path / "plugins" / "trust_store.json"
    trust_store_path.parent.mkdir(parents=True, exist_ok=True)

    private_key = Ed25519PrivateKey.generate()
    public_key = base64.b64encode(
        private_key.public_key().public_bytes(encoding=Encoding.Raw, format=PublicFormat.Raw),
    ).decode("ascii")
    manifest = _manifest_template(algorithm="rsa")
    _write_manifest(manifest_path, manifest)
    _write_trust_store(trust_store_path, public_key)

    verifier = PluginSignatureVerifier(trust_store_path=trust_store_path)
    result = verifier.verify_manifest(manifest=manifest, manifest_path=manifest_path)
    assert result.status == "unsupported_algorithm"


def test_verify_manifest_signature_missing_key(tmp_path: Path) -> None:
    """issuer/key_id 不一致は missing_key."""
    plugin_dir = tmp_path / "plugins" / "official.test-pack"
    plugin_dir.mkdir(parents=True)
    manifest_path = plugin_dir / "plugin_manifest.json"
    sig_path = plugin_dir / "plugin_manifest.sig"
    trust_store_path = tmp_path / "plugins" / "trust_store.json"
    trust_store_path.parent.mkdir(parents=True, exist_ok=True)

    private_key = Ed25519PrivateKey.generate()
    public_key = base64.b64encode(
        private_key.public_key().public_bytes(encoding=Encoding.Raw, format=PublicFormat.Raw),
    ).decode("ascii")
    manifest = _manifest_template(key_id="missing-key")
    _write_manifest(manifest_path, manifest)
    signature = private_key.sign(canonical_manifest_bytes(manifest))
    sig_path.write_text(base64.b64encode(signature).decode("ascii") + "\n", encoding="utf-8")
    _write_trust_store(trust_store_path, public_key)

    verifier = PluginSignatureVerifier(trust_store_path=trust_store_path)
    result = verifier.verify_manifest(manifest=manifest, manifest_path=manifest_path)
    assert result.status == "missing_key"
