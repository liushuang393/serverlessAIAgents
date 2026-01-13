# -*- coding: utf-8 -*-
"""テナントコンテキスト.

テナントの情報とリソース制限を管理。
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


class IsolationLevel(str, Enum):
    """テナント分離レベル.

    - SHARED: 共有インフラ、論理的分離
    - DEDICATED: 専用リソース、物理的分離
    """

    SHARED = "shared"
    DEDICATED = "dedicated"


@dataclass
class ResourceLimits:
    """リソース制限.

    テナントごとの使用量制限を定義。
    """

    # API レート制限
    max_requests_per_minute: int = 60
    max_requests_per_day: int = 10000

    # トークン制限
    max_tokens_per_request: int = 8000
    max_tokens_per_day: int = 1000000

    # 同時実行制限
    max_concurrent_tasks: int = 10
    max_agents_per_task: int = 10

    # ストレージ制限
    max_artifacts_size_mb: int = 100
    max_checkpoints: int = 50

    # メモリ制限
    max_context_tokens: int = 16000
    max_messages_per_session: int = 1000

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "max_requests_per_minute": self.max_requests_per_minute,
            "max_requests_per_day": self.max_requests_per_day,
            "max_tokens_per_request": self.max_tokens_per_request,
            "max_tokens_per_day": self.max_tokens_per_day,
            "max_concurrent_tasks": self.max_concurrent_tasks,
            "max_agents_per_task": self.max_agents_per_task,
            "max_artifacts_size_mb": self.max_artifacts_size_mb,
            "max_checkpoints": self.max_checkpoints,
            "max_context_tokens": self.max_context_tokens,
            "max_messages_per_session": self.max_messages_per_session,
        }

    @classmethod
    def default(cls) -> ResourceLimits:
        """デフォルト制限."""
        return cls()

    @classmethod
    def unlimited(cls) -> ResourceLimits:
        """無制限（開発用）."""
        return cls(
            max_requests_per_minute=999999,
            max_requests_per_day=999999999,
            max_tokens_per_request=999999,
            max_tokens_per_day=999999999,
            max_concurrent_tasks=999,
            max_agents_per_task=999,
            max_artifacts_size_mb=10000,
            max_checkpoints=10000,
            max_context_tokens=999999,
            max_messages_per_session=999999,
        )

    @classmethod
    def enterprise(cls) -> ResourceLimits:
        """エンタープライズ制限（高い制限）."""
        return cls(
            max_requests_per_minute=300,
            max_requests_per_day=100000,
            max_tokens_per_request=32000,
            max_tokens_per_day=10000000,
            max_concurrent_tasks=50,
            max_agents_per_task=20,
            max_artifacts_size_mb=1000,
            max_checkpoints=500,
            max_context_tokens=64000,
            max_messages_per_session=10000,
        )


@dataclass
class TenantContext:
    """テナントコンテキスト.

    テナントの識別情報、設定、リソース制限を保持。
    すべてのストレージ操作に自動的にテナントプレフィックスを付与。

    Example:
        >>> tenant = TenantContext(tenant_id="acme-corp")
        >>> print(tenant.storage_prefix)  # "tenant:acme-corp:"
        >>> print(tenant.prefixed_key("users"))  # "tenant:acme-corp:users"
    """

    # 識別情報
    tenant_id: str
    organization_id: str | None = None
    organization_name: str | None = None

    # 分離レベル
    isolation_level: IsolationLevel = IsolationLevel.SHARED

    # リソース制限
    resource_limits: ResourceLimits = field(default_factory=ResourceLimits.default)

    # メタデータ
    metadata: dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.now)

    # カスタム設定
    config: dict[str, Any] = field(default_factory=dict)

    @property
    def storage_prefix(self) -> str:
        """ストレージキープレフィックス."""
        return f"tenant:{self.tenant_id}:"

    def prefixed_key(self, key: str) -> str:
        """テナントプレフィックス付きキーを生成.

        Args:
            key: 元のキー

        Returns:
            プレフィックス付きキー
        """
        if key.startswith(self.storage_prefix):
            return key
        return f"{self.storage_prefix}{key}"

    def strip_prefix(self, key: str) -> str:
        """テナントプレフィックスを除去.

        Args:
            key: プレフィックス付きキー

        Returns:
            元のキー
        """
        if key.startswith(self.storage_prefix):
            return key[len(self.storage_prefix):]
        return key

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "tenant_id": self.tenant_id,
            "organization_id": self.organization_id,
            "organization_name": self.organization_name,
            "isolation_level": self.isolation_level.value,
            "resource_limits": self.resource_limits.to_dict(),
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat(),
            "config": self.config,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> TenantContext:
        """辞書から作成."""
        limits_data = data.get("resource_limits", {})
        if isinstance(limits_data, dict):
            limits = ResourceLimits(**limits_data)
        else:
            limits = ResourceLimits.default()

        return cls(
            tenant_id=data["tenant_id"],
            organization_id=data.get("organization_id"),
            organization_name=data.get("organization_name"),
            isolation_level=IsolationLevel(data.get("isolation_level", "shared")),
            resource_limits=limits,
            metadata=data.get("metadata", {}),
            config=data.get("config", {}),
        )
