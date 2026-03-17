"""Control-plane deploy 計画サービス."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


@dataclass(frozen=True, slots=True)
class DeploymentPlan:
    """デプロイ計画."""

    target: str
    steps: tuple[str, ...]
    metadata: dict[str, Any] = field(default_factory=dict)


class DeploymentService:
    """簡易デプロイ計画を生成する."""

    def plan(self, *, target: str, steps: list[str], metadata: dict[str, Any] | None = None) -> DeploymentPlan:
        """ターゲット別の配信計画を返す。"""
        return DeploymentPlan(target=target, steps=tuple(steps), metadata=metadata or {})
