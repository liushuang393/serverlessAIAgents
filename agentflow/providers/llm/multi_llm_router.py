"""マルチLLMルーター - Agent別モデル割り当てと動的ルーティング.

複数LLMプロバイダーの動的ルーティング、Agent別モデル割り当て、
コスト・速度・品質のトレードオフ最適化を提供。

使用例:
    >>> router = MultiLLMRouter(
    ...     agent_mapping=AgentModelMapping(
    ...         default_model="gpt-4o",
    ...         agent_models={
    ...             "dao": "claude-sonnet-4.5",
    ...             "fa": "gpt-4o",
    ...             "shu": "gemini-2.0-flash",
    ...         }
    ...     )
    ... )
    >>> provider = await router.get_provider_for_agent("dao")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Literal

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from agentflow.llm.llm_client import LLMClient

logger = logging.getLogger(__name__)


class AgentModelMapping(BaseModel):
    """Agent別モデル割り当て設定.

    Attributes:
        default_model: デフォルトモデル名
        agent_models: Agent ID → モデル名のマッピング
        task_type_models: タスクタイプ → モデル名のマッピング
    """

    default_model: str = Field(default="gpt-4o", description="デフォルトモデル")
    agent_models: dict[str, str] = Field(
        default_factory=dict,
        description="Agent別モデル割り当て（agent_id → model_name）",
    )
    task_type_models: dict[str, str] = Field(
        default_factory=dict,
        description="タスクタイプ別モデル割り当て（task_type → model_name）",
    )


class RouterConfig(BaseModel):
    """ルーター設定.

    Attributes:
        priority: 優先基準（cost/speed/quality）
        max_cost_per_1k_tokens: 1Kトークンあたりの最大コスト（USD）
        min_quality_score: 最小品質スコア（0.0-1.0）
        max_latency_ms: 最大レイテンシ（ミリ秒）
        enable_fallback: フォールバック有効化
        fallback_models: フォールバックモデルリスト
    """

    priority: Literal["cost", "speed", "quality"] = Field(default="quality", description="優先基準")
    max_cost_per_1k_tokens: float = Field(default=0.1, description="最大コスト/1Kトークン")
    min_quality_score: float = Field(default=0.8, ge=0.0, le=1.0, description="最小品質スコア")
    max_latency_ms: int = Field(default=5000, description="最大レイテンシ（ms）")
    enable_fallback: bool = Field(default=True, description="フォールバック有効化")
    fallback_models: list[str] = Field(
        default_factory=lambda: ["gpt-4o-mini", "claude-3-5-haiku-20241022"],
        description="フォールバックモデル",
    )


@dataclass
class ProviderPerformance:
    """プロバイダー性能キャッシュ.

    Attributes:
        avg_latency_ms: 平均レイテンシ（ms）
        success_rate: 成功率
        total_requests: 総リクエスト数
        total_cost: 総コスト（USD）
    """

    avg_latency_ms: float = 0.0
    success_rate: float = 1.0
    total_requests: int = 0
    total_cost: float = 0.0


class MultiLLMRouter:
    """マルチLLMルーター.

    複数LLMプロバイダーの動的ルーティングを提供。
    Agent別モデル割り当て、タスクタイプ別選択、コスト最適化をサポート。

    使用例:
        >>> router = MultiLLMRouter(
        ...     agent_mapping=AgentModelMapping(
        ...         default_model="gpt-4o",
        ...         agent_models={"dao": "claude-sonnet-4.5"},
        ...     ),
        ...     config=RouterConfig(priority="quality"),
        ... )
        >>> # Agent別プロバイダー取得
        >>> provider = await router.get_provider_for_agent("dao")
        >>> # タスクタイプ別プロバイダー取得
        >>> provider = await router.select_provider(task_type="code_generation")
    """

    def __init__(
        self,
        agent_mapping: AgentModelMapping | None = None,
        config: RouterConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            agent_mapping: Agent別モデル割り当て設定
            config: ルーター設定
        """
        self._agent_mapping = agent_mapping or AgentModelMapping()
        self._config = config or RouterConfig()
        self._providers: dict[str, LLMClient] = {}
        self._performance_cache: dict[str, ProviderPerformance] = {}
        self._initialized = False

        logger.info(
            f"MultiLLMRouter初期化: priority={self._config.priority}, "
            f"default_model={self._agent_mapping.default_model}"
        )

    async def initialize(self) -> None:
        """プロバイダーを初期化."""
        if self._initialized:
            return

        from agentflow.llm.llm_client import LLMClient, LLMConfig
        from agentflow.llm.models import MODELS

        # 必要なモデルのクライアントを初期化
        models_to_init = set()
        models_to_init.add(self._agent_mapping.default_model)
        models_to_init.update(self._agent_mapping.agent_models.values())
        models_to_init.update(self._agent_mapping.task_type_models.values())
        models_to_init.update(self._config.fallback_models)

        for model_name in models_to_init:
            if model_name in self._providers:
                continue

            model_info = MODELS.get(model_name)
            if not model_info:
                logger.warning(f"モデル情報が見つかりません: {model_name}")
                continue

            try:
                config = LLMConfig(
                    provider=model_info.provider,
                    model=model_info.name,
                )
                self._providers[model_name] = LLMClient(config)
                self._performance_cache[model_name] = ProviderPerformance()
                logger.info(f"プロバイダー初期化完了: {model_name}")
            except Exception as e:
                logger.exception(f"プロバイダー初期化失敗: {model_name} - {e}")

        self._initialized = True

    def get_model_for_agent(self, agent_id: str) -> str:
        """Agent IDに対応するモデル名を取得.

        Args:
            agent_id: Agent ID

        Returns:
            モデル名
        """
        return self._agent_mapping.agent_models.get(agent_id, self._agent_mapping.default_model)

    def get_model_for_task_type(self, task_type: str) -> str:
        """タスクタイプに対応するモデル名を取得.

        Args:
            task_type: タスクタイプ

        Returns:
            モデル名
        """
        return self._agent_mapping.task_type_models.get(
            task_type, self._agent_mapping.default_model
        )

    async def get_provider_for_agent(self, agent_id: str) -> LLMClient:
        """Agent IDに対応するプロバイダーを取得.

        Args:
            agent_id: Agent ID

        Returns:
            LLMClient インスタンス
        """
        await self.initialize()
        model_name = self.get_model_for_agent(agent_id)
        return await self._get_provider(model_name)

    async def select_provider(
        self,
        task_type: str | None = None,
        required_capabilities: list[str] | None = None,
    ) -> LLMClient:
        """タスクタイプまたは能力要件に基づいてプロバイダーを選択.

        Args:
            task_type: タスクタイプ
            required_capabilities: 必要な能力リスト

        Returns:
            LLMClient インスタンス
        """
        await self.initialize()

        # タスクタイプ指定がある場合
        if task_type:
            model_name = self.get_model_for_task_type(task_type)
            return await self._get_provider(model_name)

        # 能力要件に基づく選択
        if required_capabilities:
            model_name = await self._select_by_capabilities(required_capabilities)
            return await self._get_provider(model_name)

        # デフォルトモデル
        return await self._get_provider(self._agent_mapping.default_model)

    async def _get_provider(self, model_name: str) -> LLMClient:
        """プロバイダーを取得（フォールバック付き）.

        Args:
            model_name: モデル名

        Returns:
            LLMClient インスタンス

        Raises:
            ValueError: プロバイダーが見つからない場合
        """
        if model_name in self._providers:
            return self._providers[model_name]

        # フォールバック
        if self._config.enable_fallback:
            for fallback in self._config.fallback_models:
                if fallback in self._providers:
                    logger.warning(f"モデル {model_name} が利用不可、フォールバック: {fallback}")
                    return self._providers[fallback]

        msg = f"利用可能なプロバイダーがありません: {model_name}"
        raise ValueError(msg)

    async def _select_by_capabilities(self, required_capabilities: list[str]) -> str:
        """能力要件に基づいてモデルを選択.

        Args:
            required_capabilities: 必要な能力リスト

        Returns:
            モデル名
        """
        from agentflow.llm.models import MODELS

        # 能力を持つモデルをフィルタリング
        candidates = []
        for model_name in self._providers:
            model_info = MODELS.get(model_name)
            if not model_info:
                continue

            model_caps = {cap.value for cap in model_info.capabilities}
            if all(cap in model_caps for cap in required_capabilities):
                candidates.append(model_name)

        if not candidates:
            return self._agent_mapping.default_model

        # 優先基準に基づいてソート
        return await self._rank_candidates(candidates)

    async def _rank_candidates(self, candidates: list[str]) -> str:
        """候補モデルをランク付け.

        Args:
            candidates: 候補モデルリスト

        Returns:
            最適なモデル名
        """
        from agentflow.llm.models import MODELS, ModelTier

        if not candidates:
            return self._agent_mapping.default_model

        priority = self._config.priority

        if priority == "cost":
            # コスト優先：最も安いモデル
            return min(
                candidates,
                key=lambda m: (
                    MODELS[m].input_cost_per_1k + MODELS[m].output_cost_per_1k
                    if m in MODELS
                    else float("inf")
                ),
            )
        if priority == "speed":
            # 速度優先：レイテンシが最も低いモデル
            return min(
                candidates,
                key=lambda m: (
                    self._performance_cache.get(m, ProviderPerformance()).avg_latency_ms
                    or float("inf")
                ),
            )
        # 品質優先：最高階層のモデル
        tier_order = {ModelTier.PREMIUM: 0, ModelTier.STANDARD: 1, ModelTier.ECONOMY: 2}
        return min(
            candidates,
            key=lambda m: tier_order.get(MODELS[m].tier if m in MODELS else ModelTier.ECONOMY, 2),
        )

    def update_performance(
        self,
        model_name: str,
        latency_ms: float,
        success: bool,
        cost: float = 0.0,
    ) -> None:
        """プロバイダー性能を更新.

        Args:
            model_name: モデル名
            latency_ms: レイテンシ（ms）
            success: 成功したか
            cost: コスト（USD）
        """
        if model_name not in self._performance_cache:
            self._performance_cache[model_name] = ProviderPerformance()

        perf = self._performance_cache[model_name]
        perf.total_requests += 1
        perf.total_cost += cost

        # 移動平均でレイテンシを更新
        alpha = 0.1
        perf.avg_latency_ms = (1 - alpha) * perf.avg_latency_ms + alpha * latency_ms

        # 成功率を更新
        if success:
            perf.success_rate = (
                perf.success_rate * (perf.total_requests - 1) + 1
            ) / perf.total_requests
        else:
            perf.success_rate = (
                perf.success_rate * (perf.total_requests - 1)
            ) / perf.total_requests

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報辞書
        """
        return {
            model_name: {
                "avg_latency_ms": perf.avg_latency_ms,
                "success_rate": perf.success_rate,
                "total_requests": perf.total_requests,
                "total_cost": perf.total_cost,
            }
            for model_name, perf in self._performance_cache.items()
        }

    def set_agent_model(self, agent_id: str, model_name: str) -> None:
        """Agent別モデル割り当てを設定.

        Args:
            agent_id: Agent ID
            model_name: モデル名
        """
        self._agent_mapping.agent_models[agent_id] = model_name
        logger.info(f"Agent {agent_id} のモデルを {model_name} に設定")

    def set_task_type_model(self, task_type: str, model_name: str) -> None:
        """タスクタイプ別モデル割り当てを設定.

        Args:
            task_type: タスクタイプ
            model_name: モデル名
        """
        self._agent_mapping.task_type_models[task_type] = model_name
        logger.info(f"タスクタイプ {task_type} のモデルを {model_name} に設定")
