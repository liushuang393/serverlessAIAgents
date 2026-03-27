"""ツール呼び出し契約."""

from __future__ import annotations

from enum import StrEnum
from typing import Any, Protocol, runtime_checkable

from pydantic import Field

from contracts.base import ContractModel


class ToolCallStatus(StrEnum):
    """ツール呼び出し状態."""

    PENDING = "pending"
    RUNNING = "running"
    SUCCESS = "success"
    FAILED = "failed"
    TIMEOUT = "timeout"
    FALLBACK = "fallback"


class ToolRequest(ContractModel):
    """ツール呼び出し要求."""

    tool_call_id: str = Field(..., description="ツール呼び出し識別子")
    name: str = Field(..., description="ツール名")
    arguments: dict[str, Any] = Field(default_factory=dict, description="引数")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    flow_id: str | None = Field(default=None, description="フロー識別子")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")


class ToolResult(ContractModel):
    """ツール実行結果."""

    tool_call_id: str = Field(..., description="対応するツール呼び出し識別子")
    role: str = Field(default="tool", description="メッセージロール")
    content: str = Field(default="", description="実行結果文字列")
    name: str = Field(..., description="ツール名")
    status: ToolCallStatus = Field(default=ToolCallStatus.SUCCESS, description="実行状態")
    execution_time_ms: float = Field(default=0.0, ge=0.0, description="実行時間")
    error: str | None = Field(default=None, description="エラー詳細")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    artifact_ids: list[str] = Field(default_factory=list, description="生成成果物")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")


@runtime_checkable
class ToolExecutorService(Protocol):
    """ツール実行器の契約 Protocol.

    kernel.tools.KernelToolExecutor など具体実装はこの Protocol を満たす。
    harness 層はこの Protocol のみに依存し、kernel 具体型を import しない。
    """

    async def execute(self, request: ToolRequest) -> ToolResult:
        """ツール呼び出しを実行して結果を返す."""
        ...
