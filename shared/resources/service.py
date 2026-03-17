"""Layer 2 の共有リソース管理サービス.

テナント・アプリ単位のリソースクォータと使用量を管理する。
Kernel / Harness がリソース制限判定に利用する。
"""

from __future__ import annotations

from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class ResourceQuota(ContractModel):
    """リソースクォータ定義."""

    resource_type: str = Field(..., description="リソース種別 (例: token, api_call, storage)")
    limit: float = Field(..., description="上限値")
    current_usage: float = Field(default=0.0, ge=0.0, description="現在使用量")
    unit: str = Field(default="count", description="単位")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")

    @property
    def remaining(self) -> float:
        """残量を返す."""
        return max(0.0, self.limit - self.current_usage)

    @property
    def is_exceeded(self) -> bool:
        """クォータ超過かどうか."""
        return self.current_usage >= self.limit

    @property
    def usage_ratio(self) -> float:
        """使用率 (0.0–1.0)."""
        if self.limit <= 0:
            return 1.0
        return min(1.0, self.current_usage / self.limit)


class ResourceManager:
    """テナント・スコープ別のリソースクォータ管理.

    キーは ``(scope_id, resource_type)`` のタプルで管理する。
    """

    def __init__(self) -> None:
        self._quotas: dict[tuple[str, str], ResourceQuota] = {}

    def set_quota(
        self,
        scope_id: str,
        resource_type: str,
        limit: float,
        *,
        unit: str = "count",
    ) -> ResourceQuota:
        """クォータを設定する."""
        quota = ResourceQuota(
            resource_type=resource_type,
            limit=limit,
            unit=unit,
        )
        self._quotas[(scope_id, resource_type)] = quota
        return quota

    def get_quota(self, scope_id: str, resource_type: str) -> ResourceQuota | None:
        """クォータを取得する."""
        return self._quotas.get((scope_id, resource_type))

    def consume(
        self,
        scope_id: str,
        resource_type: str,
        amount: float = 1.0,
    ) -> bool:
        """リソースを消費する. クォータ超過時は False を返す."""
        quota = self._quotas.get((scope_id, resource_type))
        if quota is None:
            return True  # クォータ未設定時は無制限
        if quota.current_usage + amount > quota.limit:
            return False
        self._quotas[(scope_id, resource_type)] = quota.model_copy(
            update={"current_usage": quota.current_usage + amount},
        )
        return True

    def check_available(
        self,
        scope_id: str,
        resource_type: str,
        amount: float = 1.0,
    ) -> bool:
        """消費せずに利用可能かを判定する."""
        quota = self._quotas.get((scope_id, resource_type))
        if quota is None:
            return True
        return quota.current_usage + amount <= quota.limit

    def reset(self, scope_id: str, resource_type: str) -> None:
        """使用量をリセットする."""
        key = (scope_id, resource_type)
        if key in self._quotas:
            self._quotas[key] = self._quotas[key].model_copy(
                update={"current_usage": 0.0},
            )

    def list_quotas(self, scope_id: str) -> list[ResourceQuota]:
        """指定スコープの全クォータを返す."""
        return [
            quota
            for (sid, _), quota in self._quotas.items()
            if sid == scope_id
        ]

