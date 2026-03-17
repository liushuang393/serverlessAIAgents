"""Control-plane publish オーケストレーション."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


@dataclass(frozen=True, slots=True)
class PublishRequest:
    """公開要求の最小契約."""

    app_name: str
    target: str
    metadata: dict[str, Any] = field(default_factory=dict)


class PublishService:
    """公開要求を配信計画へ変換する."""

    def build_summary(self, request: PublishRequest) -> dict[str, Any]:
        """公開操作の要約を返す。"""
        return {
            "app_name": request.app_name,
            "target": request.target,
            "metadata": request.metadata,
        }
