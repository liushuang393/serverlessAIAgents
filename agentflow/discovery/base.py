"""Agent Discovery 基底クラス・型定義.

Agent 発見機構の抽象インターフェースと共通データモデル。
"""

from abc import ABC, abstractmethod
from datetime import UTC, datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


def _utcnow() -> datetime:
    """UTC 現在時刻を取得（タイムゾーン対応）."""
    return datetime.now(UTC)


class AgentStatus(str, Enum):
    """Agent のステータス."""

    HEALTHY = "healthy"
    """正常稼働中"""

    UNHEALTHY = "unhealthy"
    """異常状態"""

    DEGRADED = "degraded"
    """パフォーマンス低下"""

    UNKNOWN = "unknown"
    """ステータス不明"""


class LoadBalanceStrategy(str, Enum):
    """負荷分散戦略."""

    ROUND_ROBIN = "round_robin"
    """ラウンドロビン方式"""

    RANDOM = "random"
    """ランダム選択"""

    LEAST_CONNECTIONS = "least_conn"
    """最小接続数"""

    WEIGHTED = "weighted"
    """重み付け選択"""


class AgentEntry(BaseModel):
    """Agent 登録エントリ.

    Attributes:
        agent_id: Agent の一意な識別子
        name: Agent 名
        endpoint: Agent の接続先 URL
        capabilities: Agent がサポートする機能リスト
        status: 現在のステータス
        metadata: 追加メタデータ
        weight: 負荷分散の重み（WEIGHTED 戦略用）
        registered_at: 登録日時
        last_heartbeat: 最終ハートビート日時
    """

    agent_id: str = Field(..., description="Agent の一意な識別子")
    name: str = Field(..., description="Agent 名")
    endpoint: str = Field(..., description="Agent の接続先 URL")
    capabilities: list[str] = Field(default_factory=list, description="サポートする機能")
    status: AgentStatus = Field(default=AgentStatus.UNKNOWN, description="ステータス")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")
    weight: int = Field(default=1, ge=1, description="負荷分散の重み")
    registered_at: datetime = Field(default_factory=_utcnow, description="登録日時")
    last_heartbeat: datetime = Field(default_factory=_utcnow, description="最終ハートビート")


class AgentDiscoveryBase(ABC):
    """Agent 発見機構の抽象基底クラス.

    全ての AgentDiscovery 実装はこのクラスを継承する必要がある。
    """

    @abstractmethod
    async def register(self, entry: AgentEntry) -> None:
        """Agent を登録.

        Args:
            entry: 登録する AgentEntry
        """
        ...

    @abstractmethod
    async def unregister(self, agent_id: str) -> None:
        """Agent を登録解除.

        Args:
            agent_id: 登録解除する Agent の ID
        """
        ...

    @abstractmethod
    async def discover(
        self,
        capability: str | None = None,
        status: AgentStatus | None = None,
    ) -> list[AgentEntry]:
        """Agent を検索.

        Args:
            capability: フィルタする機能（指定時はその機能を持つ Agent のみ）
            status: フィルタするステータス

        Returns:
            条件に一致する AgentEntry のリスト
        """
        ...

    @abstractmethod
    async def get(self, agent_id: str) -> AgentEntry | None:
        """Agent を取得.

        Args:
            agent_id: 取得する Agent の ID

        Returns:
            AgentEntry または None
        """
        ...

    @abstractmethod
    async def heartbeat(self, agent_id: str) -> bool:
        """ハートビートを送信.

        Args:
            agent_id: ハートビートを送信する Agent の ID

        Returns:
            成功した場合は True
        """
        ...

    @abstractmethod
    async def select(
        self,
        capability: str,
        strategy: LoadBalanceStrategy = LoadBalanceStrategy.ROUND_ROBIN,
    ) -> AgentEntry | None:
        """負荷分散戦略に従って Agent を選択.

        Args:
            capability: 必要な機能
            strategy: 負荷分散戦略

        Returns:
            選択された AgentEntry または None
        """
        ...

