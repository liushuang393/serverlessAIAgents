# -*- coding: utf-8 -*-
"""LLM統合モジュール.

このモジュールは、各種LLMプロバイダー（OpenAI、Anthropic、Gemini等）との
統一インターフェースを提供します。

推奨API（松耦合設計）:
    >>> from agentflow import get_llm
    >>> llm = get_llm()  # プロバイダー/モデル不明でOK
    >>> response = await llm.chat([{"role": "user", "content": "hello"}])

高度な機能:
- ModelRouter: 智能模型路由与切换
- 多模型管理、自动切换、成本优化、负载均衡
"""

from agentflow.llm.llm_client import LLMClient, LLMConfig, LLMMessage, LLMResponse
from agentflow.llm.model_router import (
    ModelRouter,
    ModelInfo,
    ModelTier,
    ModelCapability,
    RoutingStrategy,
    RoutingConfig,
    ModelStats,
    MODELS,
    create_router_from_env,
)

__all__ = [
    # 基础客户端（内部使用・通常は get_llm() を使用）
    "LLMClient",
    "LLMConfig",
    "LLMMessage",
    "LLMResponse",
    # 模型路由（高度な機能）
    "ModelRouter",
    "ModelInfo",
    "ModelTier",
    "ModelCapability",
    "RoutingStrategy",
    "RoutingConfig",
    "ModelStats",
    "MODELS",
    "create_router_from_env",
]

