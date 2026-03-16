"""プロトコル横断契約."""

from __future__ import annotations

from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class ProtocolMessage(ContractModel):
    """MCP / A2A / AG-UI / A2UI を跨ぐ標準メッセージ."""

    protocol: str = Field(..., description="プロトコル名")
    message_type: str = Field(..., description="メッセージ種別")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    flow_id: str | None = Field(default=None, description="フロー識別子")
    payload: dict[str, Any] = Field(default_factory=dict, description="ペイロード")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")
