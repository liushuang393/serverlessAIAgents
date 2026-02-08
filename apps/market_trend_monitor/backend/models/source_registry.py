# -*- coding: utf-8 -*-
"""情報源台帳モデル.

情報源の信頼度・利用条件・有効性を管理します。
"""

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from pydantic import BaseModel, Field

from apps.market_trend_monitor.backend.models.schemas import SourceType


@dataclass
class SourceRegistryEntry:
    """情報源台帳エントリー."""

    id: str
    name: str
    source_type: SourceType
    base_url: str
    reliability_score: float = 0.5
    enabled: bool = True
    terms_url: str | None = None
    last_checked_at: datetime | None = None
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "name": self.name,
            "source_type": self.source_type.value,
            "base_url": self.base_url,
            "reliability_score": self.reliability_score,
            "enabled": self.enabled,
            "terms_url": self.terms_url,
            "last_checked_at": self.last_checked_at.isoformat() if self.last_checked_at else None,
            "metadata": self.metadata,
        }


class SourceRegistrySchema(BaseModel):
    """情報源台帳スキーマ（API用）."""

    id: str
    name: str
    source_type: str
    base_url: str
    reliability_score: float = Field(default=0.5, ge=0.0, le=1.0)
    enabled: bool = True
    terms_url: str | None = None
    last_checked_at: str | None = None
    metadata: dict[str, Any] = Field(default_factory=dict)
