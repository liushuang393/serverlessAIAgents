"""LLM Model Router - インテリジェントモデルルーティングと切り替え.

マルチモデル管理、自動切り替え、コスト最適化、負荷分散などの機能を提供。
"""

import asyncio
import logging
import time
from collections import defaultdict
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Callable

from pydantic import BaseModel, Field

from agentflow.llm.llm_client import LLMClient, LLMConfig, LLMMessage, LLMResponse

logger = logging.getLogger(__name__)


# ============================================================================
# モデル定義
# ============================================================================


class ModelTier(Enum):
    """モデル階層."""

    ECONOMY = "economy"  # エコノミー型（高速、低コスト）
    STANDARD = "standard"  # スタンダード型（バランス）
    PREMIUM = "premium"  # プレミアム型（最強、最高価格）


class ModelCapability(Enum):
    """モデル能力."""

    CHAT = "chat"  # 対話
    COMPLETION = "completion"  # 補完
    CODE = "code"  # コード生成
    REASONING = "reasoning"  # 推論
    VISION = "vision"  # 画像理解
    EMBEDDING = "embedding"  # ベクトル埋め込み
    FUNCTION_CALLING = "function_calling"  # 関数呼び出し
    AUDIO = "audio"  # 音声理解/生成
    REALTIME = "realtime"  # リアルタイム音声対話


@dataclass
class ModelInfo:
    """モデル情報."""

    name: str
    provider: str
    tier: ModelTier
    capabilities: list[ModelCapability]
    context_window: int
    input_cost_per_1k: float  # 1000トークンあたりの入力コスト（USD）
    output_cost_per_1k: float  # 1000トークンあたりの出力コスト（USD）
    max_output_tokens: int = 4096
    supports_streaming: bool = True
    supports_json_mode: bool = False
    description: str = ""


# 事前定義モデルライブラリ（2025年12月30日更新）
MODELS: dict[str, ModelInfo] = {
    # ========================================
    # OpenAI（テキスト/マルチモーダル）
    # ========================================
    "gpt-4o": ModelInfo(
        name="gpt-4o",
        provider="openai",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.FUNCTION_CALLING,
            ModelCapability.AUDIO,
        ],
        context_window=128000,
        input_cost_per_1k=0.0025,
        output_cost_per_1k=0.01,
        max_output_tokens=16384,
        supports_json_mode=True,
        description="GPT-4o フラグシップマルチモーダルモデル（テキスト/画像/音声）",
    ),
    "gpt-4o-mini": ModelInfo(
        name="gpt-4o-mini",
        provider="openai",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.FUNCTION_CALLING,
            ModelCapability.VISION,
        ],
        context_window=128000,
        input_cost_per_1k=0.00015,
        output_cost_per_1k=0.0006,
        max_output_tokens=16384,
        supports_json_mode=True,
        description="GPT-4o-mini 高コストパフォーマンスモデル",
    ),
    "o1": ModelInfo(
        name="o1",
        provider="openai",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.015,
        output_cost_per_1k=0.06,
        max_output_tokens=100000,
        supports_json_mode=True,
        description="o1 深度推論モデル、複雑なタスクに適している",
    ),
    "o1-mini": ModelInfo(
        name="o1-mini",
        provider="openai",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=128000,
        input_cost_per_1k=0.003,
        output_cost_per_1k=0.012,
        max_output_tokens=65536,
        supports_json_mode=True,
        description="o1-mini 軽量推論モデル",
    ),
    "o3-mini": ModelInfo(
        name="o3-mini",
        provider="openai",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.0011,
        output_cost_per_1k=0.0044,
        max_output_tokens=100000,
        supports_json_mode=True,
        description="o3-mini 最新推論モデル（2025年1月リリース）",
    ),
    # OpenAI音声モデル
    "gpt-4o-realtime": ModelInfo(
        name="gpt-4o-realtime-preview",
        provider="openai",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.AUDIO,
            ModelCapability.REALTIME,
        ],
        context_window=128000,
        input_cost_per_1k=0.005,
        output_cost_per_1k=0.02,
        max_output_tokens=4096,
        description="GPT-4o Realtime リアルタイム音声対話",
    ),
    "gpt-4o-audio": ModelInfo(
        name="gpt-4o-audio-preview",
        provider="openai",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.AUDIO,
        ],
        context_window=128000,
        input_cost_per_1k=0.0025,
        output_cost_per_1k=0.01,
        max_output_tokens=16384,
        description="GPT-4o Audio 音声理解と生成",
    ),
    # ========================================
    # Anthropic Claude（テキスト/マルチモーダル）
    # ========================================
    "claude-sonnet-4-20250514": ModelInfo(
        name="claude-sonnet-4-20250514",
        provider="anthropic",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.003,
        output_cost_per_1k=0.015,
        max_output_tokens=64000,
        description="Claude Sonnet 4 最新フラグシップ（2025年リリース予定）",
    ),
    "claude-3-5-sonnet-20241022": ModelInfo(
        name="claude-3-5-sonnet-20241022",
        provider="anthropic",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.003,
        output_cost_per_1k=0.015,
        max_output_tokens=8192,
        description="Claude 3.5 Sonnet コード能力が最強",
    ),
    "claude-3-5-haiku-20241022": ModelInfo(
        name="claude-3-5-haiku-20241022",
        provider="anthropic",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=200000,
        input_cost_per_1k=0.0008,
        output_cost_per_1k=0.004,
        max_output_tokens=8192,
        description="Claude 3.5 Haiku 高速エコノミー",
    ),
    "claude-3-opus-20240229": ModelInfo(
        name="claude-3-opus-20240229",
        provider="anthropic",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
        ],
        context_window=200000,
        input_cost_per_1k=0.015,
        output_cost_per_1k=0.075,
        max_output_tokens=4096,
        description="Claude 3 Opus 最強推論能力",
    ),
    # ========================================
    # Google Gemini（テキスト/マルチモーダル）
    # ========================================
    "gemini-2.0-flash": ModelInfo(
        name="gemini-2.0-flash-exp",
        provider="google",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.AUDIO,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=1000000,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=8192,
        description="Gemini 2.0 Flash 最新マルチモーダル（無料プレビュー）",
    ),
    "gemini-2.0-flash-thinking": ModelInfo(
        name="gemini-2.0-flash-thinking-exp",
        provider="google",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=1000000,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=8192,
        description="Gemini 2.0 Flash Thinking 推論強化",
    ),
    "gemini-1.5-pro": ModelInfo(
        name="gemini-1.5-pro",
        provider="google",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.VISION,
            ModelCapability.AUDIO,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=2000000,
        input_cost_per_1k=0.00125,
        output_cost_per_1k=0.005,
        max_output_tokens=8192,
        description="Gemini 1.5 Pro 200万コンテキスト",
    ),
    "gemini-1.5-flash": ModelInfo(
        name="gemini-1.5-flash",
        provider="google",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.VISION,
            ModelCapability.AUDIO,
        ],
        context_window=1000000,
        input_cost_per_1k=0.000075,
        output_cost_per_1k=0.0003,
        max_output_tokens=8192,
        description="Gemini 1.5 Flash 超高速レスポンス",
    ),
    # ========================================
    # DeepSeek（テキスト/コード）
    # ========================================
    "deepseek-chat": ModelInfo(
        name="deepseek-chat",
        provider="deepseek",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=64000,
        input_cost_per_1k=0.00014,
        output_cost_per_1k=0.00028,
        max_output_tokens=8192,
        description="DeepSeek V3 コストパフォーマンスの王者",
    ),
    "deepseek-reasoner": ModelInfo(
        name="deepseek-reasoner",
        provider="deepseek",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=64000,
        input_cost_per_1k=0.00055,
        output_cost_per_1k=0.00219,
        max_output_tokens=8192,
        description="DeepSeek R1 推論専用",
    ),
    # ========================================
    # ローカルモデル（Ollama/LocalAI）
    # ========================================
    "llama3.3-70b": ModelInfo(
        name="llama3.3:70b",
        provider="ollama",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=128000,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=4096,
        description="Llama 3.3 70B ローカル実行",
    ),
    "qwen2.5-72b": ModelInfo(
        name="qwen2.5:72b",
        provider="ollama",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=131072,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=8192,
        description="Qwen 2.5 72B 中国語最強ローカルモデル",
    ),
    "qwen2.5-coder-32b": ModelInfo(
        name="qwen2.5-coder:32b",
        provider="ollama",
        tier=ModelTier.STANDARD,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
        ],
        context_window=131072,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=8192,
        description="Qwen 2.5 Coder 32B コード専用",
    ),
    "mistral-large": ModelInfo(
        name="mistral-large:latest",
        provider="ollama",
        tier=ModelTier.PREMIUM,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
            ModelCapability.FUNCTION_CALLING,
        ],
        context_window=128000,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=4096,
        description="Mistral Large 123B ローカル実行",
    ),
    "phi-4": ModelInfo(
        name="phi4:latest",
        provider="ollama",
        tier=ModelTier.ECONOMY,
        capabilities=[
            ModelCapability.CHAT,
            ModelCapability.CODE,
            ModelCapability.REASONING,
        ],
        context_window=16384,
        input_cost_per_1k=0.0,
        output_cost_per_1k=0.0,
        max_output_tokens=4096,
        description="Microsoft Phi-4 14B 軽量高効率",
    ),
}


# ============================================================================
# ルーティング戦略
# ============================================================================


class RoutingStrategy(Enum):
    """ルーティング戦略."""

    COST_OPTIMIZED = "cost_optimized"  # コスト優先
    QUALITY_OPTIMIZED = "quality_optimized"  # 品質優先
    BALANCED = "balanced"  # バランス
    ROUND_ROBIN = "round_robin"  # ラウンドロビン
    LATENCY_OPTIMIZED = "latency_optimized"  # レイテンシ優先
    CAPABILITY_MATCH = "capability_match"  # 能力マッチ


@dataclass
class RoutingConfig:
    """ルーティング設定."""

    strategy: RoutingStrategy = RoutingStrategy.BALANCED
    fallback_models: list[str] = field(default_factory=list)
    max_retries: int = 3
    retry_delay: float = 1.0
    timeout: int = 60
    cost_limit_per_request: float | None = None
    preferred_providers: list[str] = field(default_factory=list)


# ============================================================================
# モデル統計
# ============================================================================


@dataclass
class ModelStats:
    """モデル使用統計."""

    total_requests: int = 0
    successful_requests: int = 0
    failed_requests: int = 0
    total_input_tokens: int = 0
    total_output_tokens: int = 0
    total_cost: float = 0.0
    total_latency_ms: int = 0
    avg_latency_ms: float = 0.0
    last_used: float = 0.0
    error_rate: float = 0.0


# ============================================================================
# モデルルーター
# ============================================================================


class ModelRouter:
    """インテリジェントモデルルーター.

    マルチモデル管理、自動切り替え、コスト最適化、負荷分散などの機能を提供。

    使用例:
        ```python
        router = ModelRouter(
            models={
                "primary": LLMConfig(provider="anthropic", model="claude-3-5-sonnet-20241022"),
                "fallback": LLMConfig(provider="openai", model="gpt-4o"),
                "economy": LLMConfig(provider="openai", model="gpt-4o-mini"),
            },
            routing_config=RoutingConfig(strategy=RoutingStrategy.BALANCED),
        )

        # 最適なモデルを自動選択
        response = await router.chat(messages)

        # モデルを指定
        response = await router.chat(messages, model="economy")

        # 能力に基づいて選択
        response = await router.chat_with_capability(
            messages,
            required_capabilities=[ModelCapability.CODE, ModelCapability.REASONING],
        )
        ```
    """

    def __init__(
        self,
        models: dict[str, LLMConfig] | None = None,
        routing_config: RoutingConfig | None = None,
    ) -> None:
        """モデルルーターを初期化.

        Args:
            models: モデル設定辞書 {"name": LLMConfig}
            routing_config: ルーティング設定
        """
        self._models = models or {}
        self._routing_config = routing_config or RoutingConfig()
        self._clients: dict[str, LLMClient] = {}
        self._stats: dict[str, ModelStats] = defaultdict(ModelStats)
        self._round_robin_index = 0

        self._initialize_clients()

    def _initialize_clients(self) -> None:
        """すべてのモデルクライアントを初期化."""
        for name, config in self._models.items():
            try:
                self._clients[name] = LLMClient(config)
                logger.info(f"モデルクライアントを初期化しました: {name} ({config.provider}/{config.model})")
            except Exception as e:
                logger.warning(f"モデル {name} の初期化に失敗しました: {e}")

    def add_model(self, name: str, config: LLMConfig) -> None:
        """モデルを追加.

        Args:
            name: モデル名
            config: モデル設定
        """
        self._models[name] = config
        self._clients[name] = LLMClient(config)
        logger.info(f"モデルを追加しました: {name}")

    def remove_model(self, name: str) -> None:
        """モデルを削除.

        Args:
            name: モデル名
        """
        if name in self._models:
            del self._models[name]
        if name in self._clients:
            del self._clients[name]
        logger.info(f"モデルを削除しました: {name}")

    def get_model_info(self, model_name: str) -> ModelInfo | None:
        """モデル情報を取得.

        Args:
            model_name: モデル名

        Returns:
            モデル情報
        """
        config = self._models.get(model_name)
        if config:
            return MODELS.get(config.model)
        return None

    def list_models(self) -> list[str]:
        """すべてのモデルをリストアップ.

        Returns:
            モデル名リスト
        """
        return list(self._models.keys())

    # ========================================================================
    # ルーティングロジック
    # ========================================================================

    def _select_model(
        self,
        required_capabilities: list[ModelCapability] | None = None,
        max_cost: float | None = None,
    ) -> str:
        """戦略に基づいてモデルを選択.

        Args:
            required_capabilities: 必要な能力
            max_cost: 最大コスト制限

        Returns:
            選択されたモデル名
        """
        strategy = self._routing_config.strategy
        available = list(self._clients.keys())

        if not available:
            raise ValueError("利用可能なモデルがありません")

        # フィルタリング：能力マッチ
        if required_capabilities:
            filtered = []
            for name in available:
                info = self.get_model_info(name)
                if info:
                    if all(cap in info.capabilities for cap in required_capabilities):
                        filtered.append(name)
            if filtered:
                available = filtered

        # フィルタリング：コスト制限
        if max_cost:
            filtered = []
            for name in available:
                info = self.get_model_info(name)
                if info and info.input_cost_per_1k <= max_cost:
                    filtered.append(name)
            if filtered:
                available = filtered

        # 戦略選択
        if strategy == RoutingStrategy.COST_OPTIMIZED:
            return self._select_cheapest(available)
        elif strategy == RoutingStrategy.QUALITY_OPTIMIZED:
            return self._select_highest_quality(available)
        elif strategy == RoutingStrategy.LATENCY_OPTIMIZED:
            return self._select_lowest_latency(available)
        elif strategy == RoutingStrategy.ROUND_ROBIN:
            return self._select_round_robin(available)
        else:  # BALANCED
            return self._select_balanced(available)

    def _select_cheapest(self, available: list[str]) -> str:
        """最も安価なモデルを選択."""
        cheapest = available[0]
        cheapest_cost = float("inf")

        for name in available:
            info = self.get_model_info(name)
            if info:
                cost = info.input_cost_per_1k + info.output_cost_per_1k
                if cost < cheapest_cost:
                    cheapest = name
                    cheapest_cost = cost

        return cheapest

    def _select_highest_quality(self, available: list[str]) -> str:
        """最高品質のモデルを選択."""
        # 階層でソート：PREMIUM > STANDARD > ECONOMY
        tier_order = {ModelTier.PREMIUM: 0, ModelTier.STANDARD: 1, ModelTier.ECONOMY: 2}

        best = available[0]
        best_tier = 2

        for name in available:
            info = self.get_model_info(name)
            if info:
                tier = tier_order.get(info.tier, 2)
                if tier < best_tier:
                    best = name
                    best_tier = tier

        return best

    def _select_lowest_latency(self, available: list[str]) -> str:
        """レイテンシが最も低いモデルを選択（履歴統計に基づく）."""
        best = available[0]
        best_latency = float("inf")

        for name in available:
            stats = self._stats[name]
            if stats.avg_latency_ms > 0 and stats.avg_latency_ms < best_latency:
                best = name
                best_latency = stats.avg_latency_ms

        return best

    def _select_round_robin(self, available: list[str]) -> str:
        """ラウンドロビンでモデルを選択."""
        selected = available[self._round_robin_index % len(available)]
        self._round_robin_index += 1
        return selected

    def _select_balanced(self, available: list[str]) -> str:
        """バランス選択（コスト、品質、レイテンシを総合的に考慮）."""
        scores: dict[str, float] = {}

        for name in available:
            info = self.get_model_info(name)
            stats = self._stats[name]

            score = 0.0

            # コストスコア（低いほど良い）
            if info:
                cost = info.input_cost_per_1k + info.output_cost_per_1k
                score += (1 / (cost + 0.001)) * 0.3

            # 品質スコア
            if info:
                tier_score = {ModelTier.PREMIUM: 1.0, ModelTier.STANDARD: 0.7, ModelTier.ECONOMY: 0.4}
                score += tier_score.get(info.tier, 0.5) * 0.4

            # 成功率スコア
            if stats.total_requests > 0:
                success_rate = stats.successful_requests / stats.total_requests
                score += success_rate * 0.3

            scores[name] = score

        return max(scores.keys(), key=lambda k: scores[k])

    # ========================================================================
    # リクエスト処理
    # ========================================================================

    async def chat(
        self,
        messages: list[LLMMessage],
        model: str | None = None,
        **kwargs: Any,
    ) -> LLMResponse:
        """チャットリクエストを送信.

        Args:
            messages: メッセージリスト
            model: 指定モデル（オプション）
            **kwargs: その他のパラメータ

        Returns:
            LLMレスポンス
        """
        selected_model = model or self._select_model()
        return await self._execute_with_retry(selected_model, messages, **kwargs)

    async def chat_with_capability(
        self,
        messages: list[LLMMessage],
        required_capabilities: list[ModelCapability],
        **kwargs: Any,
    ) -> LLMResponse:
        """能力に基づいてモデルを選択し、リクエストを送信.

        Args:
            messages: メッセージリスト
            required_capabilities: 必要な能力
            **kwargs: その他のパラメータ

        Returns:
            LLMレスポンス
        """
        selected_model = self._select_model(required_capabilities=required_capabilities)
        return await self._execute_with_retry(selected_model, messages, **kwargs)

    async def chat_cost_limited(
        self,
        messages: list[LLMMessage],
        max_cost_per_request: float,
        **kwargs: Any,
    ) -> LLMResponse:
        """コスト制限下でリクエストを送信.

        Args:
            messages: メッセージリスト
            max_cost_per_request: 最大コスト
            **kwargs: その他のパラメータ

        Returns:
            LLMレスポンス
        """
        selected_model = self._select_model(max_cost=max_cost_per_request)
        return await self._execute_with_retry(selected_model, messages, **kwargs)

    async def _execute_with_retry(
        self,
        model_name: str,
        messages: list[LLMMessage],
        **kwargs: Any,
    ) -> LLMResponse:
        """リトライ付きリクエスト実行.

        Args:
            model_name: モデル名
            messages: メッセージリスト
            **kwargs: その他のパラメータ

        Returns:
            LLMレスポンス
        """
        models_to_try = [model_name] + self._routing_config.fallback_models
        last_error: Exception | None = None

        for attempt, current_model in enumerate(models_to_try):
            if current_model not in self._clients:
                continue

            client = self._clients[current_model]
            start_time = time.time()

            try:
                response = await client.chat(messages, **kwargs)

                # 統計を更新
                self._update_stats(
                    current_model,
                    success=True,
                    latency_ms=int((time.time() - start_time) * 1000),
                    input_tokens=response.usage.get("prompt_tokens", 0),
                    output_tokens=response.usage.get("completion_tokens", 0),
                )

                return response

            except Exception as e:
                last_error = e
                logger.warning(
                    f"モデル {current_model} のリクエストが失敗しました (試行 {attempt + 1}): {e}"
                )

                # 統計を更新
                self._update_stats(
                    current_model,
                    success=False,
                    latency_ms=int((time.time() - start_time) * 1000),
                )

                # 待機してリトライ
                if attempt < len(models_to_try) - 1:
                    await asyncio.sleep(self._routing_config.retry_delay)

        raise last_error or ValueError("すべてのモデルが失敗しました")

    def _update_stats(
        self,
        model_name: str,
        success: bool,
        latency_ms: int,
        input_tokens: int = 0,
        output_tokens: int = 0,
    ) -> None:
        """モデル統計を更新."""
        stats = self._stats[model_name]
        stats.total_requests += 1
        stats.total_latency_ms += latency_ms
        stats.last_used = time.time()

        if success:
            stats.successful_requests += 1
            stats.total_input_tokens += input_tokens
            stats.total_output_tokens += output_tokens

            # コストを計算
            info = self.get_model_info(model_name)
            if info:
                cost = (
                    (input_tokens / 1000) * info.input_cost_per_1k
                    + (output_tokens / 1000) * info.output_cost_per_1k
                )
                stats.total_cost += cost
        else:
            stats.failed_requests += 1

        # 平均値を更新
        if stats.total_requests > 0:
            stats.avg_latency_ms = stats.total_latency_ms / stats.total_requests
            stats.error_rate = stats.failed_requests / stats.total_requests

    # ========================================================================
    # 統計クエリ
    # ========================================================================

    def get_stats(self, model_name: str | None = None) -> dict[str, ModelStats]:
        """モデル統計を取得.

        Args:
            model_name: モデル名（オプション、指定しない場合はすべて返す）

        Returns:
            モデル統計辞書
        """
        if model_name:
            return {model_name: self._stats[model_name]}
        return dict(self._stats)

    def get_total_cost(self) -> float:
        """総コストを取得.

        Returns:
            総コスト（USD）
        """
        return sum(s.total_cost for s in self._stats.values())

    def get_cost_breakdown(self) -> dict[str, float]:
        """コスト内訳を取得.

        Returns:
            各モデルのコスト辞書
        """
        return {name: stats.total_cost for name, stats in self._stats.items()}

    def reset_stats(self, model_name: str | None = None) -> None:
        """統計をリセット.

        Args:
            model_name: モデル名（オプション、指定しない場合はすべてリセット）
        """
        if model_name:
            self._stats[model_name] = ModelStats()
        else:
            self._stats.clear()


# ============================================================================
# 便利関数
# ============================================================================


def create_router_from_env() -> ModelRouter:
    """環境変数からルーターを作成.

    環境変数（優先度の高い順）:
        - OPENAI_API_KEY: OpenAI APIキー
        - ANTHROPIC_API_KEY: Anthropic APIキー
        - GOOGLE_API_KEY: Google AI Studio APIキー
        - DEEPSEEK_API_KEY: DeepSeek APIキー
        - OLLAMA_BASE_URL: Ollamaサービスアドレス（デフォルト: http://localhost:11434）
        - LOCALAI_BASE_URL: LocalAIサービスアドレス（デフォルト: http://localhost:8080）

    Returns:
        設定済みモデルルーター（利用可能なプロバイダーを自動検出）
    """
    import os

    models: dict[str, LLMConfig] = {}

    # OpenAI（クラウド優先）
    if os.environ.get("OPENAI_API_KEY"):
        models["gpt-4o"] = LLMConfig(
            provider="openai",
            model="gpt-4o",
            api_key=os.environ["OPENAI_API_KEY"],
        )
        models["gpt-4o-mini"] = LLMConfig(
            provider="openai",
            model="gpt-4o-mini",
            api_key=os.environ["OPENAI_API_KEY"],
        )
        models["o1-mini"] = LLMConfig(
            provider="openai",
            model="o1-mini",
            api_key=os.environ["OPENAI_API_KEY"],
        )

    # Anthropic
    if os.environ.get("ANTHROPIC_API_KEY"):
        models["claude-sonnet"] = LLMConfig(
            provider="anthropic",
            model="claude-3-5-sonnet-20241022",
            api_key=os.environ["ANTHROPIC_API_KEY"],
        )
        models["claude-haiku"] = LLMConfig(
            provider="anthropic",
            model="claude-3-5-haiku-20241022",
            api_key=os.environ["ANTHROPIC_API_KEY"],
        )

    # Google Gemini
    if os.environ.get("GOOGLE_API_KEY"):
        models["gemini-2.0-flash"] = LLMConfig(
            provider="google",
            model="gemini-2.0-flash-exp",
            api_key=os.environ["GOOGLE_API_KEY"],
        )
        models["gemini-1.5-pro"] = LLMConfig(
            provider="google",
            model="gemini-1.5-pro",
            api_key=os.environ["GOOGLE_API_KEY"],
        )

    # DeepSeek
    if os.environ.get("DEEPSEEK_API_KEY"):
        models["deepseek-chat"] = LLMConfig(
            provider="deepseek",
            model="deepseek-chat",
            api_key=os.environ["DEEPSEEK_API_KEY"],
        )
        models["deepseek-reasoner"] = LLMConfig(
            provider="deepseek",
            model="deepseek-reasoner",
            api_key=os.environ["DEEPSEEK_API_KEY"],
        )

    # Ollama（ローカルモデル）
    ollama_url = os.environ.get("OLLAMA_BASE_URL", "http://localhost:11434")
    if _check_service_available(ollama_url):
        models["ollama-llama3.3"] = LLMConfig(
            provider="ollama",
            model="llama3.3:70b",
            base_url=ollama_url,
        )
        models["ollama-qwen2.5"] = LLMConfig(
            provider="ollama",
            model="qwen2.5:72b",
            base_url=ollama_url,
        )

    # LocalAI（ローカルモデル - デフォルト）
    localai_url = os.environ.get("LOCALAI_BASE_URL", "http://localhost:8080")
    if _check_service_available(localai_url):
        models["localai-default"] = LLMConfig(
            provider="localai",
            model="gpt-4",
            base_url=localai_url,
        )

    # 利用可能なモデルがない場合、警告を追加
    if not models:
        import logging

        logging.warning(
            "LLMプロバイダーが設定されていません。次のいずれかを設定してください: "
            "OPENAI_API_KEY, ANTHROPIC_API_KEY, GOOGLE_API_KEY, DEEPSEEK_API_KEY, "
            "またはOllama/LocalAIサービスを起動してください。"
        )

    return ModelRouter(
        models=models,
        routing_config=RoutingConfig(
            strategy=RoutingStrategy.BALANCED,
            fallback_models=list(models.keys())[1:] if len(models) > 1 else [],
        ),
    )


def _check_service_available(url: str, timeout: float = 1.0) -> bool:
    """ローカルサービスが利用可能かチェック.

    Args:
        url: サービスURL
        timeout: タイムアウト時間（秒）

    Returns:
        サービスが利用可能かどうか
    """
    try:
        import httpx

        with httpx.Client(timeout=timeout) as client:
            response = client.get(url)
            return response.status_code < 500
    except Exception:
        return False

