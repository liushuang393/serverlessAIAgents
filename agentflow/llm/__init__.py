"""LLM統合モジュール.

このモジュールは、各種LLMプロバイダー（OpenAI、Anthropic、Gemini等）との
統一インターフェースを提供します。

推奨API（松耦合設計）:
    >>> from agentflow import get_llm
    >>> llm = get_llm()  # プロバイダー/モデル不明でOK
    >>> response = await llm.chat([{"role": "user", "content": "hello"}])

高度な機能:
- ModelRouter: インテリジェントモデルルーティングと切り替え
- マルチモデル管理、自動切り替え、コスト最適化、負荷分散
"""

from agentflow.llm.llm_client import LLMClient, LLMConfig, LLMMessage, LLMResponse, ToolCall
from agentflow.llm.model_router import (
    MODELS,
    ModelCapability,
    ModelInfo,
    ModelRouter,
    ModelStats,
    ModelTier,
    RoutingConfig,
    RoutingStrategy,
    create_router_from_env,
)


__all__ = [
    "MODELS",
    # 基本クライアント（内部使用・通常はget_llm()を使用）
    "LLMClient",
    "LLMConfig",
    "LLMMessage",
    "LLMResponse",
    "ModelCapability",
    "ModelInfo",
    # モデルルーティング（高度な機能）
    "ModelRouter",
    "ModelStats",
    "ModelTier",
    "RoutingConfig",
    "RoutingStrategy",
    "ToolCall",
    "create_router_from_env",
]

