"""適応型コーディネーター - 能力差を吸収する分業型Agent設計.

Hassabisの「参差不齐の知能を前提とした分業型Agent設計」に基づく実装。

設計原則:
- Agent能力プロファイルによる適切なタスク割り当て
- 動的な負荷分散
- 能力に応じた段階的タスク委譲
- パフォーマンスフィードバックによる学習

使用例:
    >>> coordinator = AdaptiveCoordinator()
    >>> coordinator.register_agent(
    ...     agent_id="expert",
    ...     agent=ExpertAgent(),
    ...     capabilities=["analysis", "reasoning"],
    ...     performance_level=0.9,
    ... )
    >>> result = await coordinator.delegate_task(
    ...     task={"type": "analysis", "data": data},
    ... )
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class PerformanceLevel(str, Enum):
    """パフォーマンスレベル."""

    EXPERT = "expert"  # 専門家レベル (0.9+)
    ADVANCED = "advanced"  # 上級レベル (0.7-0.9)
    STANDARD = "standard"  # 標準レベル (0.5-0.7)
    BASIC = "basic"  # 基本レベル (0.3-0.5)
    LEARNING = "learning"  # 学習中 (0.0-0.3)


class AgentCapability(BaseModel):
    """Agent能力.

    Attributes:
        name: 能力名
        proficiency: 熟練度 (0.0-1.0)
        last_used: 最後に使用した日時
        success_rate: 成功率
        avg_latency: 平均レイテンシ（秒）
    """

    name: str = Field(..., description="能力名")
    proficiency: float = Field(default=0.5, ge=0.0, le=1.0, description="熟練度")
    last_used: datetime | None = Field(default=None)
    success_rate: float = Field(default=1.0, ge=0.0, le=1.0)
    avg_latency: float = Field(default=1.0, ge=0.0, description="平均レイテンシ秒")
    task_count: int = Field(default=0, ge=0, description="実行タスク数")


class AgentProfile(BaseModel):
    """Agentプロファイル.

    Attributes:
        id: Agent ID
        name: Agent名
        capabilities: 能力リスト
        performance_level: パフォーマンスレベル
        current_load: 現在の負荷 (0.0-1.0)
        max_concurrent: 最大同時タスク数
        is_available: 利用可能フラグ
        metadata: メタデータ
    """

    id: str = Field(default_factory=lambda: f"agent-{uuid.uuid4().hex[:8]}")
    name: str = Field(default="", description="Agent名")
    capabilities: dict[str, AgentCapability] = Field(default_factory=dict)
    performance_level: float = Field(default=0.5, ge=0.0, le=1.0)
    current_load: float = Field(default=0.0, ge=0.0, le=1.0)
    max_concurrent: int = Field(default=5, ge=1)
    is_available: bool = Field(default=True)
    metadata: dict[str, Any] = Field(default_factory=dict)
    registered_at: datetime = Field(default_factory=datetime.now)

    def get_capability_score(self, capability_name: str) -> float:
        """特定能力のスコアを取得."""
        cap = self.capabilities.get(capability_name)
        if not cap:
            return 0.0
        # 熟練度と成功率を考慮
        return cap.proficiency * cap.success_rate

    def get_overall_score(self) -> float:
        """総合スコアを取得."""
        if not self.capabilities:
            return self.performance_level
        avg_cap = sum(c.proficiency for c in self.capabilities.values()) / len(self.capabilities)
        return (avg_cap + self.performance_level) / 2


class TaskRequirement(BaseModel):
    """タスク要件.

    Attributes:
        required_capabilities: 必須能力
        preferred_capabilities: 推奨能力
        min_proficiency: 最小熟練度
        priority: 優先度
        timeout: タイムアウト秒
    """

    required_capabilities: list[str] = Field(default_factory=list)
    preferred_capabilities: list[str] = Field(default_factory=list)
    min_proficiency: float = Field(default=0.3, ge=0.0, le=1.0)
    priority: int = Field(default=1, ge=1)
    timeout: float = Field(default=60.0, gt=0)


class DelegationResult(BaseModel):
    """委譲結果.

    Attributes:
        success: 成功フラグ
        agent_id: 割り当てられたAgent ID
        result: 実行結果
        latency: レイテンシ秒
        error: エラー
    """

    success: bool = Field(default=False)
    agent_id: str | None = Field(default=None)
    result: dict[str, Any] = Field(default_factory=dict)
    latency: float = Field(default=0.0)
    error: str | None = Field(default=None)


class MatchScore(BaseModel):
    """マッチスコア."""

    agent_id: str
    score: float
    capability_match: float
    load_factor: float
    details: dict[str, Any] = Field(default_factory=dict)


@dataclass
class AdaptiveCoordinator:
    """適応型コーディネーター.

    Agent能力を考慮した適切なタスク割り当てと負荷分散を行う。

    主な機能:
    - Agentプロファイル管理
    - タスク要件とAgent能力のマッチング
    - 動的負荷分散
    - パフォーマンスフィードバック
    """

    _agents: dict[str, Any] = field(default_factory=dict)
    _profiles: dict[str, AgentProfile] = field(default_factory=dict)
    load_balance_threshold: float = 0.8
    _logger: logging.Logger = field(default_factory=lambda: logging.getLogger("agentflow.patterns.coordinator"))

    def register_agent(
        self,
        agent_id: str,
        agent: Any,
        capabilities: list[str] | None = None,
        performance_level: float = 0.5,
        name: str = "",
        max_concurrent: int = 5,
    ) -> AgentProfile:
        """Agentを登録.

        Args:
            agent_id: Agent ID
            agent: Agentインスタンス
            capabilities: 能力リスト
            performance_level: パフォーマンスレベル
            name: Agent名
            max_concurrent: 最大同時タスク数

        Returns:
            AgentProfile
        """
        capabilities = capabilities or []

        profile = AgentProfile(
            id=agent_id,
            name=name or agent_id,
            performance_level=performance_level,
            max_concurrent=max_concurrent,
            capabilities={cap: AgentCapability(name=cap, proficiency=performance_level) for cap in capabilities},
        )

        self._agents[agent_id] = agent
        self._profiles[agent_id] = profile

        self._logger.info(f"Agent登録: {agent_id} (capabilities={capabilities})")

        return profile

    def unregister_agent(self, agent_id: str) -> bool:
        """Agentを登録解除."""
        if agent_id in self._agents:
            del self._agents[agent_id]
            del self._profiles[agent_id]
            return True
        return False

    def get_profile(self, agent_id: str) -> AgentProfile | None:
        """Agentプロファイルを取得."""
        return self._profiles.get(agent_id)

    def get_all_profiles(self) -> list[AgentProfile]:
        """全Agentプロファイルを取得."""
        return list(self._profiles.values())

    def find_best_agent(
        self,
        requirements: TaskRequirement,
    ) -> MatchScore | None:
        """最適なAgentを検索.

        Args:
            requirements: タスク要件

        Returns:
            MatchScore or None
        """
        candidates: list[MatchScore] = []

        for agent_id, profile in self._profiles.items():
            if not profile.is_available:
                continue

            # 負荷チェック
            if profile.current_load >= self.load_balance_threshold:
                continue

            # 必須能力チェック
            has_required = all(
                cap in profile.capabilities and profile.capabilities[cap].proficiency >= requirements.min_proficiency
                for cap in requirements.required_capabilities
            )
            if not has_required:
                continue

            # スコア計算
            capability_score = self._calculate_capability_score(profile, requirements)
            load_factor = 1.0 - profile.current_load
            total_score = capability_score * 0.7 + load_factor * 0.3

            candidates.append(
                MatchScore(
                    agent_id=agent_id,
                    score=total_score,
                    capability_match=capability_score,
                    load_factor=load_factor,
                    details={
                        "required_match": has_required,
                        "profile_level": profile.performance_level,
                    },
                )
            )

        if not candidates:
            return None

        # 最高スコアのAgentを返す
        return max(candidates, key=lambda x: x.score)

    def _calculate_capability_score(
        self,
        profile: AgentProfile,
        requirements: TaskRequirement,
    ) -> float:
        """能力スコアを計算."""
        scores = []

        # 必須能力
        for cap in requirements.required_capabilities:
            scores.append(profile.get_capability_score(cap))

        # 推奨能力（重み0.5）
        for cap in requirements.preferred_capabilities:
            scores.append(profile.get_capability_score(cap) * 0.5)

        if not scores:
            return profile.performance_level

        return sum(scores) / len(scores)

    async def delegate_task(
        self,
        task: dict[str, Any],
        requirements: TaskRequirement | None = None,
    ) -> DelegationResult:
        """タスクを委譲.

        Args:
            task: タスクデータ
            requirements: タスク要件

        Returns:
            DelegationResult
        """
        import time

        # 要件が指定されていない場合はタスクから推論
        if requirements is None:
            requirements = self._infer_requirements(task)

        # 最適なAgentを検索
        match = self.find_best_agent(requirements)

        if match is None:
            return DelegationResult(
                success=False,
                error="適切なAgentが見つかりません",
            )

        agent_id = match.agent_id
        agent = self._agents[agent_id]
        profile = self._profiles[agent_id]

        # 負荷更新
        profile.current_load += 1.0 / profile.max_concurrent

        start_time = time.time()

        try:
            # Agent実行
            if hasattr(agent, "run"):
                result = await agent.run(task)
            elif hasattr(agent, "invoke"):
                result = await agent.invoke(task)
            elif callable(agent):
                result = await agent(task)
            else:
                result = {"status": "completed"}

            latency = time.time() - start_time

            # 成功フィードバック
            self._update_performance(agent_id, task, success=True, latency=latency)

            return DelegationResult(
                success=True,
                agent_id=agent_id,
                result=result,
                latency=latency,
            )

        except Exception as e:
            latency = time.time() - start_time

            # 失敗フィードバック
            self._update_performance(agent_id, task, success=False, latency=latency)

            return DelegationResult(
                success=False,
                agent_id=agent_id,
                error=str(e),
                latency=latency,
            )

        finally:
            # 負荷解放
            profile.current_load = max(0.0, profile.current_load - 1.0 / profile.max_concurrent)

    def _infer_requirements(self, task: dict[str, Any]) -> TaskRequirement:
        """タスクから要件を推論."""
        task_type = task.get("type", "")
        capabilities = []

        if task_type:
            capabilities.append(task_type)

        return TaskRequirement(
            required_capabilities=capabilities,
            priority=task.get("priority", 1),
            timeout=task.get("timeout", 60.0),
        )

    def _update_performance(
        self,
        agent_id: str,
        task: dict[str, Any],
        success: bool,
        latency: float,
    ) -> None:
        """パフォーマンスを更新."""
        profile = self._profiles.get(agent_id)
        if not profile:
            return

        task_type = task.get("type", "")
        if task_type and task_type in profile.capabilities:
            cap = profile.capabilities[task_type]

            # 成功率を更新（指数移動平均）
            alpha = 0.1
            cap.success_rate = alpha * (1.0 if success else 0.0) + (1 - alpha) * cap.success_rate

            # レイテンシを更新
            cap.avg_latency = alpha * latency + (1 - alpha) * cap.avg_latency

            # タスク数を更新
            cap.task_count += 1
            cap.last_used = datetime.now()

    def get_load_summary(self) -> dict[str, Any]:
        """負荷状況の概要を取得."""
        return {
            "total_agents": len(self._agents),
            "available_agents": sum(1 for p in self._profiles.values() if p.is_available),
            "agents": {
                agent_id: {
                    "load": profile.current_load,
                    "performance": profile.performance_level,
                    "capabilities": list(profile.capabilities.keys()),
                }
                for agent_id, profile in self._profiles.items()
            },
        }


__all__ = [
    "AdaptiveCoordinator",
    "AgentCapability",
    "AgentProfile",
    "DelegationResult",
    "MatchScore",
    "PerformanceLevel",
    "TaskRequirement",
]
