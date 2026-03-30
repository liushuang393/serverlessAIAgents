"""Builder draft persistence."""

from __future__ import annotations

import json
import uuid
from datetime import UTC, datetime
from pathlib import Path
from typing import Any


_DEFAULT_STORAGE_PATH = Path.cwd() / "control_plane" / "data" / "builder_drafts.json"
_VALID_STATUSES = {"draft", "validated", "generated", "materialized", "failed"}


def _utc_now() -> str:
    return datetime.now(UTC).isoformat()


class BuilderDraftStore:
    """Builder draft を JSON へ永続化する軽量ストア."""

    def __init__(self, storage_path: Path | None = None) -> None:
        self._storage_path = storage_path or _DEFAULT_STORAGE_PATH
        self._storage_path.parent.mkdir(parents=True, exist_ok=True)
        self._drafts: dict[str, dict[str, Any]] = {}
        self._load()

    def list_drafts(self) -> list[dict[str, Any]]:
        """更新時刻の降順で draft を返す."""
        return sorted(
            (dict(item) for item in self._drafts.values()),
            key=lambda item: str(item.get("updated_at", "")),
            reverse=True,
        )

    def get_draft(self, draft_id: str) -> dict[str, Any] | None:
        """単一 draft を返す."""
        payload = self._drafts.get(draft_id)
        return dict(payload) if payload is not None else None

    def create_draft(self, payload: dict[str, Any]) -> dict[str, Any]:
        """新規 draft を保存する."""
        draft_id = str(payload.get("id") or uuid.uuid4())
        record = self._normalize_record({"id": draft_id, **payload})
        self._drafts[draft_id] = record
        self._persist()
        return dict(record)

    def update_draft(self, draft_id: str, patch: dict[str, Any]) -> dict[str, Any]:
        """既存 draft を更新する."""
        current = self._drafts.get(draft_id)
        if current is None:
            msg = f"Draft not found: {draft_id}"
            raise KeyError(msg)

        merged = {**current, **patch, "id": draft_id}
        record = self._normalize_record(merged)
        self._drafts[draft_id] = record
        self._persist()
        return dict(record)

    def delete_draft(self, draft_id: str) -> None:
        """draft を削除する."""
        if draft_id not in self._drafts:
            msg = f"Draft not found: {draft_id}"
            raise KeyError(msg)
        del self._drafts[draft_id]
        self._persist()

    def _load(self) -> None:
        if not self._storage_path.exists():
            return
        payload = json.loads(self._storage_path.read_text(encoding="utf-8"))
        if not isinstance(payload, list):
            return
        for item in payload:
            if not isinstance(item, dict):
                continue
            record = self._normalize_record(item, preserve_updated=True)
            self._drafts[record["id"]] = record

    def _persist(self) -> None:
        serialized = json.dumps(self.list_drafts(), ensure_ascii=False, indent=2) + "\n"
        self._storage_path.write_text(serialized, encoding="utf-8")

    def _normalize_record(
        self,
        payload: dict[str, Any],
        *,
        preserve_updated: bool = False,
    ) -> dict[str, Any]:
        status = str(payload.get("status", "draft")).strip().lower() or "draft"
        if status not in _VALID_STATUSES:
            status = "draft"

        updated_at = payload.get("updated_at")
        if not preserve_updated or not isinstance(updated_at, str) or not updated_at.strip():
            updated_at = _utc_now()

        return {
            "id": str(payload.get("id", uuid.uuid4())),
            "name": str(payload.get("name", "")).strip(),
            "template_id": str(payload.get("template_id", "")).strip(),
            "goal": str(payload.get("goal", "")).strip(),
            "spec_kind": str(payload.get("spec_kind", "agent")).strip().lower() or "agent",
            "spec": payload.get("spec") if isinstance(payload.get("spec"), dict) else {},
            "generated_files": (
                payload.get("generated_files")
                if isinstance(payload.get("generated_files"), dict)
                else {}
            ),
            "status": status,
            "updated_at": updated_at,
        }


_builder_draft_store: BuilderDraftStore | None = None


def init_builder_draft_store(storage_path: Path | None = None) -> BuilderDraftStore:
    """BuilderDraftStore シングルトンを初期化する."""
    global _builder_draft_store
    if _builder_draft_store is None:
        _builder_draft_store = BuilderDraftStore(storage_path=storage_path)
    return _builder_draft_store


def get_builder_draft_store() -> BuilderDraftStore:
    """BuilderDraftStore シングルトンを取得する."""
    if _builder_draft_store is None:
        msg = "BuilderDraftStore が未初期化です。init_builder_draft_store() を先に呼んでください。"
        raise RuntimeError(msg)
    return _builder_draft_store


__all__ = [
    "BuilderDraftStore",
    "get_builder_draft_store",
    "init_builder_draft_store",
]
