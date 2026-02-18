"""Plugin manifest 署名検証ユーティリティ.

P1 では sidecar 方式 (`plugin_manifest.sig`) を使用し、
manifest 本体 JSON を canonical 化したバイト列に対して検証する。
"""

from __future__ import annotations

import base64
import json
import os
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal

try:  # pragma: no cover - import error branch is environment dependent
    from cryptography.exceptions import InvalidSignature
    from cryptography.hazmat.primitives.asymmetric.ed25519 import Ed25519PublicKey
except Exception:  # noqa: BLE001
    InvalidSignature = Exception  # type: ignore[assignment]
    Ed25519PublicKey = None  # type: ignore[assignment]


SignatureStatus = Literal[
    "verified",
    "missing_sig",
    "missing_key",
    "bad_signature",
    "unsupported_algorithm",
    "parse_error",
]

_DEFAULT_TRUST_STORE_PATH = Path("plugins") / "trust_store.json"
_TRUST_STORE_ENV = "AGENTFLOW_PLUGIN_TRUST_STORE"
_SUPPORTED_ALGORITHM = "ed25519"


@dataclass(slots=True)
class SignatureVerificationResult:
    """署名検証結果."""

    status: SignatureStatus
    reason: str

    @property
    def verified(self) -> bool:
        """署名が有効かどうか."""
        return self.status == "verified"


def canonical_manifest_bytes(manifest: dict[str, Any]) -> bytes:
    """manifest を canonical JSON バイト列へ変換する."""
    normalized = json.dumps(
        manifest,
        sort_keys=True,
        separators=(",", ":"),
        ensure_ascii=False,
    )
    return normalized.encode("utf-8")


class PluginSignatureVerifier:
    """Plugin manifest 署名検証."""

    def __init__(self, *, trust_store_path: Path | None = None) -> None:
        self._trust_store_path = self._resolve_trust_store_path(trust_store_path)
        self._trust_store_error: str | None = None
        self._trust_store = self._load_trust_store(self._trust_store_path)
        if self._trust_store is None:
            self._trust_store = {}

    @staticmethod
    def _resolve_trust_store_path(path: Path | None) -> Path:
        if path is not None:
            return path
        env_path = os.getenv(_TRUST_STORE_ENV)
        if env_path:
            return Path(env_path)
        return Path.cwd() / _DEFAULT_TRUST_STORE_PATH

    def _load_trust_store(
        self,
        path: Path,
    ) -> dict[str, dict[str, dict[str, str]]] | None:
        if not path.is_file():
            self._trust_store_error = f"trust store が見つかりません: {path}"
            return {}
        try:
            payload = json.loads(path.read_text("utf-8"))
        except Exception as exc:  # noqa: BLE001
            self._trust_store_error = f"trust store 読み込み失敗: {exc}"
            return None
        if not isinstance(payload, dict):
            self._trust_store_error = "trust store 形式不正: object が必要です"
            return None
        return payload  # type: ignore[return-value]

    def verify_manifest(
        self,
        *,
        manifest: dict[str, Any],
        manifest_path: Path,
    ) -> SignatureVerificationResult:
        """manifest + sidecar `.sig` を検証する."""
        if self._trust_store_error:
            return SignatureVerificationResult("parse_error", self._trust_store_error)

        signature_meta = manifest.get("signature")
        if not isinstance(signature_meta, dict):
            return SignatureVerificationResult(
                "parse_error",
                "manifest.signature が未定義または object ではありません",
            )

        algorithm = str(signature_meta.get("algorithm", "")).strip().lower()
        issuer = str(signature_meta.get("issuer", "")).strip()
        key_id = str(signature_meta.get("key_id", "")).strip()
        if not issuer or not key_id:
            return SignatureVerificationResult(
                "missing_key",
                "manifest.signature の issuer/key_id が不足しています",
            )

        if algorithm != _SUPPORTED_ALGORITHM:
            return SignatureVerificationResult(
                "unsupported_algorithm",
                f"未対応アルゴリズムです: {algorithm or '<empty>'}",
            )

        if Ed25519PublicKey is None:
            return SignatureVerificationResult(
                "parse_error",
                "ed25519 検証に必要な cryptography が利用できません",
            )

        sidecar_path = manifest_path.parent / "plugin_manifest.sig"
        if not sidecar_path.is_file():
            return SignatureVerificationResult(
                "missing_sig",
                f"sidecar 署名が見つかりません: {sidecar_path.name}",
            )

        try:
            signature_value = sidecar_path.read_text("utf-8").strip()
            signature_bytes = base64.b64decode(signature_value, validate=True)
        except Exception as exc:  # noqa: BLE001
            return SignatureVerificationResult("parse_error", f"sidecar 署名の解釈失敗: {exc}")

        key_entry = self._resolve_key_entry(issuer=issuer, key_id=key_id)
        if isinstance(key_entry, SignatureVerificationResult):
            return key_entry

        key_algorithm = str(key_entry.get("algorithm", "")).strip().lower()
        if key_algorithm != _SUPPORTED_ALGORITHM:
            return SignatureVerificationResult(
                "unsupported_algorithm",
                f"trust store 側アルゴリズム未対応: {key_algorithm or '<empty>'}",
            )

        encoded_public_key = str(key_entry.get("public_key_base64", "")).strip()
        if not encoded_public_key:
            return SignatureVerificationResult(
                "missing_key",
                "trust store に public_key_base64 がありません",
            )

        try:
            public_key_bytes = base64.b64decode(encoded_public_key, validate=True)
            verifier = Ed25519PublicKey.from_public_bytes(public_key_bytes)
            verifier.verify(signature_bytes, canonical_manifest_bytes(manifest))
        except InvalidSignature:
            return SignatureVerificationResult("bad_signature", "署名検証に失敗しました")
        except Exception as exc:  # noqa: BLE001
            return SignatureVerificationResult("parse_error", f"署名検証処理失敗: {exc}")

        return SignatureVerificationResult("verified", "ok")

    def _resolve_key_entry(
        self,
        *,
        issuer: str,
        key_id: str,
    ) -> dict[str, str] | SignatureVerificationResult:
        issuer_ring = self._trust_store.get(issuer)
        if not isinstance(issuer_ring, dict):
            return SignatureVerificationResult(
                "missing_key",
                f"trust store に issuer が存在しません: {issuer}",
            )
        entry = issuer_ring.get(key_id)
        if not isinstance(entry, dict):
            return SignatureVerificationResult(
                "missing_key",
                f"trust store に key_id が存在しません: {issuer}/{key_id}",
            )
        return entry  # type: ignore[return-value]


__all__ = [
    "PluginSignatureVerifier",
    "SignatureStatus",
    "SignatureVerificationResult",
    "canonical_manifest_bytes",
]
