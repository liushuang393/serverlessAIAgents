"""LLM Model Router - インテリジェントモデルルーティングと切り替え.

マルチモデル管理、自動切り替え、コスト最適化、負荷分散などの機能を提供。

v0.5.0: モジュラー化 - 機能別ファイルに分割
- models.py: モデル定義（ModelTier, ModelCapability, ModelInfo, MODELS）
- stats.py: 統計関連（ModelStats）
- router.py: ルーティングロジック（RoutingStrategy, RoutingConfig, ModelRouter）

このファイルは後方互換性のための再エクスポートを提供します。
"""

from __future__ import annotations

# モデル定義を再エクスポート
from agentflow.llm.models import (
    MODELS,
    ModelCapability,
    ModelInfo,
    ModelTier,
)

# ルーターを再エクスポート
from agentflow.llm.router import (
    ModelRouter,
    RoutingConfig,
    RoutingStrategy,
    create_router_from_env,
)

# 統計を再エクスポート
from agentflow.llm.stats import ModelStats


__all__ = [
    "MODELS",
    "ModelCapability",
    "ModelInfo",
    "ModelRouter",
    # 統計
    "ModelStats",
    # モデル定義
    "ModelTier",
    "RoutingConfig",
    # ルーティング
    "RoutingStrategy",
    # 便利関数
    "create_router_from_env",
]
