"""追跡契約."""

from __future__ import annotations

from datetime import UTC, datetime
from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class TraceRecord(ContractModel):
    """実行トレースの標準記録."""

    trace_id: str = Field(..., description="トレース識別子")
    span_id: str = Field(..., description="スパン識別子")
    parent_span_id: str | None = Field(default=None, description="親スパン識別子")
    name: str = Field(..., description="スパン名")
    status: str = Field(default="ok", description="状態")
    start_time: datetime = Field(default_factory=lambda: datetime.now(UTC), description="開始時刻")
    end_time: datetime | None = Field(default=None, description="終了時刻")
    attributes: dict[str, Any] = Field(default_factory=dict, description="属性")
    events: list[dict[str, Any]] = Field(default_factory=list, description="イベント一覧")
