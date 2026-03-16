"""文脈共有契約."""

from __future__ import annotations

from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class ContextPack(ContractModel):
    """Agent 間で受け渡す構造化文脈."""

    context_id: str = Field(..., description="文脈識別子")
    flow_id: str | None = Field(default=None, description="フロー識別子")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    summary: str = Field(default="", description="要約")
    inputs: dict[str, Any] = Field(default_factory=dict, description="入力値")
    artifacts: list[str] = Field(default_factory=list, description="関連成果物")
    policies: list[str] = Field(default_factory=list, description="適用ポリシー識別子")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")
