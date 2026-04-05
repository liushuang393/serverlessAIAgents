"""Pydantic モデル定義（リクエスト / コマンド / 承認）."""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field, field_validator


class MigrationOptions(BaseModel):
    """移行実行オプション."""

    verification_mode: str = "strict"

    @field_validator("verification_mode")
    @classmethod
    def _normalize_verification_mode(cls, value: str) -> str:
        normalized = value.strip().lower()
        if normalized not in {"strict", "fast"}:
            return "strict"
        return normalized


class MigrationRequest(BaseModel):
    """移行開始リクエスト."""

    source_code: str
    migration_type: str = "cobol-to-java"
    options: MigrationOptions = Field(default_factory=MigrationOptions)
    expected_outputs: dict[str, Any] = Field(default_factory=dict)
    module: str | None = None


class ApprovalDecisionRequest(BaseModel):
    """承認 API の互換入力アダプター."""

    approved: bool
    comment: str | None = None


# 後方互換: 旧 API 名を維持する
ApprovalRequest = ApprovalDecisionRequest


class TaskCommandRequest(BaseModel):
    """ヒューマンコマンド API リクエスト."""

    command: str
    request_id: str | None = None
    actor: str | None = None
    comment: str | None = None
    fact: dict[str, Any] | None = None
    modifications: dict[str, Any] = Field(default_factory=dict)
