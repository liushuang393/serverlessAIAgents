"""LLM Model Router - 智能模型路由与切换.

提供多模型管理、自动切换、成本优化、负载均衡等功能。
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
# 模型定义
# ============================================================================


class ModelTier(Enum):
    """模型等级."""

    ECONOMY = "economy"  # 经济型（快速、便宜）
    STANDARD = "standard"  # 标准型（平衡）
    PREMIUM = "premium"  # 高级型（最强、最贵）


class ModelCapability(Enum):
    """模型能力."""

    CHAT = "chat"  # 对话
    COMPLETION = "completion"  # 补全
    CODE = "code"  # 代码生成
    REASONING = "reasoning"  # 推理
    VISION = "vision"  # 图像理解
    EMBEDDING = "embedding"  # 向量嵌入
    FUNCTION_CALLING = "function_calling"  # 函数调用
    AUDIO = "audio"  # 音频理解/生成
    REALTIME = "realtime"  # 实时语音对话


@dataclass
class ModelInfo:
    """模型信息."""

    name: str
    provider: str
    tier: ModelTier
    capabilities: list[ModelCapability]
    context_window: int
    input_cost_per_1k: float  # 每 1000 token 输入成本（USD）
    output_cost_per_1k: float  # 每 1000 token 输出成本（USD）
    max_output_tokens: int = 4096
    supports_streaming: bool = True
    supports_json_mode: bool = False
    description: str = ""


# 预定义模型库（2025年12月30日更新）
MODELS: dict[str, ModelInfo] = {
    # ========================================
    # OpenAI（文本/多模态）
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
        description="GPT-4o 旗舰多模态模型（文本/图像/音频）",
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
        description="GPT-4o-mini 高性价比模型",
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
        description="o1 深度推理模型，适合复杂任务",
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
        description="o1-mini 轻量推理模型",
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
        description="o3-mini 最新推理模型（2025年1月发布）",
    ),
    # OpenAI 语音模型
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
        description="GPT-4o Realtime 实时语音对话",
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
        description="GPT-4o Audio 音频理解与生成",
    ),
    # ========================================
    # Anthropic Claude（文本/多模态）
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
        description="Claude Sonnet 4 最新旗舰（预计2025年发布）",
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
        description="Claude 3.5 Sonnet 代码能力最强",
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
        description="Claude 3.5 Haiku 快速经济",
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
        description="Claude 3 Opus 最强推理能力",
    ),
    # ========================================
    # Google Gemini（文本/多模态）
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
        description="Gemini 2.0 Flash 最新多模态（免费预览）",
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
        description="Gemini 2.0 Flash Thinking 推理增强",
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
        description="Gemini 1.5 Pro 200万上下文",
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
        description="Gemini 1.5 Flash 极速响应",
    ),
    # ========================================
    # DeepSeek（文本/代码）
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
        description="DeepSeek V3 性价比之王",
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
        description="DeepSeek R1 推理专用",
    ),
    # ========================================
    # 本地模型（Ollama/LocalAI）
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
        description="Llama 3.3 70B 本地运行",
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
        description="Qwen 2.5 72B 中文最强本地模型",
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
        description="Qwen 2.5 Coder 32B 代码专用",
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
        description="Mistral Large 123B 本地运行",
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
        description="Microsoft Phi-4 14B 轻量高效",
    ),
}


# ============================================================================
# 路由策略
# ============================================================================


class RoutingStrategy(Enum):
    """路由策略."""

    COST_OPTIMIZED = "cost_optimized"  # 成本优先
    QUALITY_OPTIMIZED = "quality_optimized"  # 质量优先
    BALANCED = "balanced"  # 平衡
    ROUND_ROBIN = "round_robin"  # 轮询
    LATENCY_OPTIMIZED = "latency_optimized"  # 延迟优先
    CAPABILITY_MATCH = "capability_match"  # 能力匹配


@dataclass
class RoutingConfig:
    """路由配置."""

    strategy: RoutingStrategy = RoutingStrategy.BALANCED
    fallback_models: list[str] = field(default_factory=list)
    max_retries: int = 3
    retry_delay: float = 1.0
    timeout: int = 60
    cost_limit_per_request: float | None = None
    preferred_providers: list[str] = field(default_factory=list)


# ============================================================================
# 模型统计
# ============================================================================


@dataclass
class ModelStats:
    """模型使用统计."""

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
# 模型路由器
# ============================================================================


class ModelRouter:
    """智能模型路由器.

    提供多模型管理、自动切换、成本优化、负载均衡等功能。

    使用示例:
        ```python
        router = ModelRouter(
            models={
                "primary": LLMConfig(provider="anthropic", model="claude-3-5-sonnet-20241022"),
                "fallback": LLMConfig(provider="openai", model="gpt-4o"),
                "economy": LLMConfig(provider="openai", model="gpt-4o-mini"),
            },
            routing_config=RoutingConfig(strategy=RoutingStrategy.BALANCED),
        )

        # 自动选择最佳模型
        response = await router.chat(messages)

        # 指定模型
        response = await router.chat(messages, model="economy")

        # 根据能力选择
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
        """初始化模型路由器.

        Args:
            models: 模型配置字典 {"name": LLMConfig}
            routing_config: 路由配置
        """
        self._models = models or {}
        self._routing_config = routing_config or RoutingConfig()
        self._clients: dict[str, LLMClient] = {}
        self._stats: dict[str, ModelStats] = defaultdict(ModelStats)
        self._round_robin_index = 0

        self._initialize_clients()

    def _initialize_clients(self) -> None:
        """初始化所有模型客户端."""
        for name, config in self._models.items():
            try:
                self._clients[name] = LLMClient(config)
                logger.info(f"已初始化模型客户端: {name} ({config.provider}/{config.model})")
            except Exception as e:
                logger.warning(f"模型 {name} 初始化失败: {e}")

    def add_model(self, name: str, config: LLMConfig) -> None:
        """添加模型.

        Args:
            name: 模型名称
            config: 模型配置
        """
        self._models[name] = config
        self._clients[name] = LLMClient(config)
        logger.info(f"已添加模型: {name}")

    def remove_model(self, name: str) -> None:
        """移除模型.

        Args:
            name: 模型名称
        """
        if name in self._models:
            del self._models[name]
        if name in self._clients:
            del self._clients[name]
        logger.info(f"已移除模型: {name}")

    def get_model_info(self, model_name: str) -> ModelInfo | None:
        """获取模型信息.

        Args:
            model_name: 模型名称

        Returns:
            模型信息
        """
        config = self._models.get(model_name)
        if config:
            return MODELS.get(config.model)
        return None

    def list_models(self) -> list[str]:
        """列出所有模型.

        Returns:
            模型名称列表
        """
        return list(self._models.keys())

    # ========================================================================
    # 路由逻辑
    # ========================================================================

    def _select_model(
        self,
        required_capabilities: list[ModelCapability] | None = None,
        max_cost: float | None = None,
    ) -> str:
        """根据策略选择模型.

        Args:
            required_capabilities: 必需的能力
            max_cost: 最大成本限制

        Returns:
            选中的模型名称
        """
        strategy = self._routing_config.strategy
        available = list(self._clients.keys())

        if not available:
            raise ValueError("没有可用的模型")

        # 过滤：能力匹配
        if required_capabilities:
            filtered = []
            for name in available:
                info = self.get_model_info(name)
                if info:
                    if all(cap in info.capabilities for cap in required_capabilities):
                        filtered.append(name)
            if filtered:
                available = filtered

        # 过滤：成本限制
        if max_cost:
            filtered = []
            for name in available:
                info = self.get_model_info(name)
                if info and info.input_cost_per_1k <= max_cost:
                    filtered.append(name)
            if filtered:
                available = filtered

        # 策略选择
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
        """选择最便宜的模型."""
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
        """选择最高质量的模型."""
        # 按等级排序：PREMIUM > STANDARD > ECONOMY
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
        """选择延迟最低的模型（基于历史统计）."""
        best = available[0]
        best_latency = float("inf")

        for name in available:
            stats = self._stats[name]
            if stats.avg_latency_ms > 0 and stats.avg_latency_ms < best_latency:
                best = name
                best_latency = stats.avg_latency_ms

        return best

    def _select_round_robin(self, available: list[str]) -> str:
        """轮询选择模型."""
        selected = available[self._round_robin_index % len(available)]
        self._round_robin_index += 1
        return selected

    def _select_balanced(self, available: list[str]) -> str:
        """平衡选择（综合考虑成本、质量、延迟）."""
        scores: dict[str, float] = {}

        for name in available:
            info = self.get_model_info(name)
            stats = self._stats[name]

            score = 0.0

            # 成本得分（越低越好）
            if info:
                cost = info.input_cost_per_1k + info.output_cost_per_1k
                score += (1 / (cost + 0.001)) * 0.3

            # 质量得分
            if info:
                tier_score = {ModelTier.PREMIUM: 1.0, ModelTier.STANDARD: 0.7, ModelTier.ECONOMY: 0.4}
                score += tier_score.get(info.tier, 0.5) * 0.4

            # 成功率得分
            if stats.total_requests > 0:
                success_rate = stats.successful_requests / stats.total_requests
                score += success_rate * 0.3

            scores[name] = score

        return max(scores.keys(), key=lambda k: scores[k])

    # ========================================================================
    # 请求处理
    # ========================================================================

    async def chat(
        self,
        messages: list[LLMMessage],
        model: str | None = None,
        **kwargs: Any,
    ) -> LLMResponse:
        """发送聊天请求.

        Args:
            messages: 消息列表
            model: 指定模型（可选）
            **kwargs: 其他参数

        Returns:
            LLM 响应
        """
        selected_model = model or self._select_model()
        return await self._execute_with_retry(selected_model, messages, **kwargs)

    async def chat_with_capability(
        self,
        messages: list[LLMMessage],
        required_capabilities: list[ModelCapability],
        **kwargs: Any,
    ) -> LLMResponse:
        """根据能力选择模型并发送请求.

        Args:
            messages: 消息列表
            required_capabilities: 必需的能力
            **kwargs: 其他参数

        Returns:
            LLM 响应
        """
        selected_model = self._select_model(required_capabilities=required_capabilities)
        return await self._execute_with_retry(selected_model, messages, **kwargs)

    async def chat_cost_limited(
        self,
        messages: list[LLMMessage],
        max_cost_per_request: float,
        **kwargs: Any,
    ) -> LLMResponse:
        """在成本限制下发送请求.

        Args:
            messages: 消息列表
            max_cost_per_request: 最大成本
            **kwargs: 其他参数

        Returns:
            LLM 响应
        """
        selected_model = self._select_model(max_cost=max_cost_per_request)
        return await self._execute_with_retry(selected_model, messages, **kwargs)

    async def _execute_with_retry(
        self,
        model_name: str,
        messages: list[LLMMessage],
        **kwargs: Any,
    ) -> LLMResponse:
        """带重试的请求执行.

        Args:
            model_name: 模型名称
            messages: 消息列表
            **kwargs: 其他参数

        Returns:
            LLM 响应
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

                # 更新统计
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
                    f"模型 {current_model} 请求失败 (尝试 {attempt + 1}): {e}"
                )

                # 更新统计
                self._update_stats(
                    current_model,
                    success=False,
                    latency_ms=int((time.time() - start_time) * 1000),
                )

                # 等待后重试
                if attempt < len(models_to_try) - 1:
                    await asyncio.sleep(self._routing_config.retry_delay)

        raise last_error or ValueError("所有模型都失败了")

    def _update_stats(
        self,
        model_name: str,
        success: bool,
        latency_ms: int,
        input_tokens: int = 0,
        output_tokens: int = 0,
    ) -> None:
        """更新模型统计."""
        stats = self._stats[model_name]
        stats.total_requests += 1
        stats.total_latency_ms += latency_ms
        stats.last_used = time.time()

        if success:
            stats.successful_requests += 1
            stats.total_input_tokens += input_tokens
            stats.total_output_tokens += output_tokens

            # 计算成本
            info = self.get_model_info(model_name)
            if info:
                cost = (
                    (input_tokens / 1000) * info.input_cost_per_1k
                    + (output_tokens / 1000) * info.output_cost_per_1k
                )
                stats.total_cost += cost
        else:
            stats.failed_requests += 1

        # 更新平均值
        if stats.total_requests > 0:
            stats.avg_latency_ms = stats.total_latency_ms / stats.total_requests
            stats.error_rate = stats.failed_requests / stats.total_requests

    # ========================================================================
    # 统计查询
    # ========================================================================

    def get_stats(self, model_name: str | None = None) -> dict[str, ModelStats]:
        """获取模型统计.

        Args:
            model_name: 模型名称（可选，不指定返回全部）

        Returns:
            模型统计字典
        """
        if model_name:
            return {model_name: self._stats[model_name]}
        return dict(self._stats)

    def get_total_cost(self) -> float:
        """获取总成本.

        Returns:
            总成本（USD）
        """
        return sum(s.total_cost for s in self._stats.values())

    def get_cost_breakdown(self) -> dict[str, float]:
        """获取成本明细.

        Returns:
            各模型成本字典
        """
        return {name: stats.total_cost for name, stats in self._stats.items()}

    def reset_stats(self, model_name: str | None = None) -> None:
        """重置统计.

        Args:
            model_name: 模型名称（可选，不指定重置全部）
        """
        if model_name:
            self._stats[model_name] = ModelStats()
        else:
            self._stats.clear()


# ============================================================================
# 便捷函数
# ============================================================================


def create_router_from_env() -> ModelRouter:
    """从环境变量创建路由器.

    环境变量（优先级从高到低）:
        - OPENAI_API_KEY: OpenAI API 密钥
        - ANTHROPIC_API_KEY: Anthropic API 密钥
        - GOOGLE_API_KEY: Google AI Studio API 密钥
        - DEEPSEEK_API_KEY: DeepSeek API 密钥
        - OLLAMA_BASE_URL: Ollama 服务地址（默认 http://localhost:11434）
        - LOCALAI_BASE_URL: LocalAI 服务地址（默认 http://localhost:8080）

    Returns:
        配置好的模型路由器（自动检测可用提供商）
    """
    import os

    models: dict[str, LLMConfig] = {}

    # OpenAI（云端首选）
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

    # Ollama（本地模型）
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

    # LocalAI（本地模型 - 默认）
    localai_url = os.environ.get("LOCALAI_BASE_URL", "http://localhost:8080")
    if _check_service_available(localai_url):
        models["localai-default"] = LLMConfig(
            provider="localai",
            model="gpt-4",
            base_url=localai_url,
        )

    # 如果没有任何可用模型，添加警告
    if not models:
        import logging

        logging.warning(
            "No LLM providers configured. Set one of: "
            "OPENAI_API_KEY, ANTHROPIC_API_KEY, GOOGLE_API_KEY, DEEPSEEK_API_KEY, "
            "or start Ollama/LocalAI service."
        )

    return ModelRouter(
        models=models,
        routing_config=RoutingConfig(
            strategy=RoutingStrategy.BALANCED,
            fallback_models=list(models.keys())[1:] if len(models) > 1 else [],
        ),
    )


def _check_service_available(url: str, timeout: float = 1.0) -> bool:
    """检查本地服务是否可用.

    Args:
        url: 服务 URL
        timeout: 超时时间（秒）

    Returns:
        服务是否可用
    """
    try:
        import httpx

        with httpx.Client(timeout=timeout) as client:
            response = client.get(url)
            return response.status_code < 500
    except Exception:
        return False

