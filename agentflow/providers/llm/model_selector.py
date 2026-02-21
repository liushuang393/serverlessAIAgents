"""モデル選択戦略 - タスクタイプ別モデル選択.

タスクタイプに基づいて最適なモデルを選択する戦略を提供。

使用例:
    >>> selector = ModelSelector()
    >>> model = selector.select(
    ...     task_type=TaskType.CODE_GENERATION,
    ...     criteria=ModelSelectionCriteria(priority="quality"),
    ... )
"""

from __future__ import annotations

import logging
from enum import Enum
from typing import Any, Literal

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class TaskType(str, Enum):
    """タスクタイプ."""

    # 分析・推論
    ANALYSIS = "analysis"  # 分析タスク
    REASONING = "reasoning"  # 推論タスク
    DECISION = "decision"  # 意思決定

    # コード関連
    CODE_GENERATION = "code_generation"  # コード生成
    CODE_REVIEW = "code_review"  # コードレビュー
    CODE_MIGRATION = "code_migration"  # コード移行

    # テキスト処理
    SUMMARIZATION = "summarization"  # 要約
    TRANSLATION = "translation"  # 翻訳
    EXTRACTION = "extraction"  # 情報抽出

    # 対話
    CHAT = "chat"  # 一般対話
    QA = "qa"  # 質問応答

    # マルチモーダル
    VISION = "vision"  # 画像理解
    AUDIO = "audio"  # 音声処理


class ModelSelectionCriteria(BaseModel):
    """モデル選択基準.

    Attributes:
        priority: 優先基準（cost/speed/quality）
        max_cost_per_1k_tokens: 1Kトークンあたりの最大コスト（USD）
        min_quality_score: 最小品質スコア（0.0-1.0）
        max_latency_ms: 最大レイテンシ（ミリ秒）
        required_capabilities: 必要な能力リスト
    """

    priority: Literal["cost", "speed", "quality"] = Field(default="quality", description="優先基準")
    max_cost_per_1k_tokens: float = Field(default=0.1, description="最大コスト/1Kトークン")
    min_quality_score: float = Field(default=0.8, ge=0.0, le=1.0, description="最小品質スコア")
    max_latency_ms: int = Field(default=5000, description="最大レイテンシ（ms）")
    required_capabilities: list[str] = Field(default_factory=list, description="必要な能力リスト")


# タスクタイプ別推奨モデル設定
TASK_TYPE_RECOMMENDATIONS: dict[TaskType, dict[str, Any]] = {
    # 分析・推論タスク：高品質モデル推奨
    TaskType.ANALYSIS: {
        "recommended": ["claude-3-5-sonnet-20241022", "gpt-4o", "o1"],
        "capabilities": ["reasoning"],
        "priority_override": "quality",
    },
    TaskType.REASONING: {
        "recommended": ["o1", "o3-mini", "claude-3-5-sonnet-20241022"],
        "capabilities": ["reasoning"],
        "priority_override": "quality",
    },
    TaskType.DECISION: {
        "recommended": ["claude-3-5-sonnet-20241022", "gpt-4o"],
        "capabilities": ["reasoning"],
        "priority_override": "quality",
    },
    # コード関連タスク：コード能力重視
    TaskType.CODE_GENERATION: {
        "recommended": ["claude-3-5-sonnet-20241022", "gpt-4o", "deepseek-chat"],
        "capabilities": ["code"],
        "priority_override": None,
    },
    TaskType.CODE_REVIEW: {
        "recommended": ["claude-3-5-sonnet-20241022", "gpt-4o"],
        "capabilities": ["code", "reasoning"],
        "priority_override": "quality",
    },
    TaskType.CODE_MIGRATION: {
        "recommended": ["claude-3-5-sonnet-20241022", "gpt-4o"],
        "capabilities": ["code"],
        "priority_override": "quality",
    },
    # テキスト処理：バランス重視
    TaskType.SUMMARIZATION: {
        "recommended": ["gpt-4o-mini", "claude-3-5-haiku-20241022", "gemini-2.0-flash"],
        "capabilities": ["chat"],
        "priority_override": None,
    },
    TaskType.TRANSLATION: {
        "recommended": ["gpt-4o", "claude-3-5-sonnet-20241022"],
        "capabilities": ["chat"],
        "priority_override": None,
    },
    TaskType.EXTRACTION: {
        "recommended": ["gpt-4o-mini", "claude-3-5-haiku-20241022"],
        "capabilities": ["chat"],
        "priority_override": None,
    },
    # 対話タスク：速度重視
    TaskType.CHAT: {
        "recommended": ["gpt-4o-mini", "claude-3-5-haiku-20241022", "gemini-2.0-flash"],
        "capabilities": ["chat"],
        "priority_override": "speed",
    },
    TaskType.QA: {
        "recommended": ["gpt-4o-mini", "claude-3-5-haiku-20241022"],
        "capabilities": ["chat"],
        "priority_override": None,
    },
    # マルチモーダル
    TaskType.VISION: {
        "recommended": ["gpt-4o", "claude-3-5-sonnet-20241022", "gemini-2.0-flash"],
        "capabilities": ["vision"],
        "priority_override": None,
    },
    TaskType.AUDIO: {
        "recommended": ["gpt-4o-audio", "gemini-2.0-flash"],
        "capabilities": ["audio"],
        "priority_override": None,
    },
}


class ModelSelector:
    """タスクタイプ別モデル選択器.

    タスクタイプと選択基準に基づいて最適なモデルを選択。

    使用例:
        >>> selector = ModelSelector()
        >>> model = selector.select(
        ...     task_type=TaskType.CODE_GENERATION,
        ...     criteria=ModelSelectionCriteria(priority="quality"),
        ... )
        >>> print(model)
        'claude-3-5-sonnet-20241022'
    """

    def __init__(self, available_models: list[str] | None = None) -> None:
        """初期化.

        Args:
            available_models: 利用可能なモデルリスト（None の場合は全モデル）
        """
        self._available_models = available_models
        logger.info(f"ModelSelector初期化: available_models={available_models}")

    def select(
        self,
        task_type: TaskType,
        criteria: ModelSelectionCriteria | None = None,
    ) -> str:
        """タスクタイプと基準に基づいてモデルを選択.

        Args:
            task_type: タスクタイプ
            criteria: 選択基準（None の場合はデフォルト）

        Returns:
            選択されたモデル名
        """
        criteria = criteria or ModelSelectionCriteria()

        # タスクタイプの推奨設定を取得
        recommendation = TASK_TYPE_RECOMMENDATIONS.get(task_type)
        if not recommendation:
            logger.warning(f"タスクタイプ {task_type} の推奨設定が見つかりません")
            return "gpt-4o"  # デフォルトフォールバック

        recommended = recommendation["recommended"]
        priority = recommendation.get("priority_override") or criteria.priority

        # 利用可能なモデルでフィルタリング
        candidates = self._filter_available(recommended)
        if not candidates:
            logger.warning("利用可能な推奨モデルがありません、デフォルトを使用")
            return "gpt-4o"

        # 優先基準に基づいて選択
        return self._select_by_priority(candidates, priority, criteria)

    def _filter_available(self, models: list[str]) -> list[str]:
        """利用可能なモデルでフィルタリング.

        Args:
            models: モデルリスト

        Returns:
            フィルタリング後のモデルリスト
        """
        if self._available_models is None:
            return models
        return [m for m in models if m in self._available_models]

    def _select_by_priority(
        self,
        candidates: list[str],
        priority: str,
        criteria: ModelSelectionCriteria,
    ) -> str:
        """優先基準に基づいてモデルを選択.

        Args:
            candidates: 候補モデルリスト
            priority: 優先基準（cost/speed/quality）
            criteria: 選択基準

        Returns:
            選択されたモデル名
        """
        from agentflow.llm.models import MODELS, ModelTier

        if priority == "cost":
            # コスト優先：最も安いモデル
            filtered = [
                m
                for m in candidates
                if m in MODELS
                and (MODELS[m].input_cost_per_1k + MODELS[m].output_cost_per_1k) <= criteria.max_cost_per_1k_tokens * 2
            ]
            if filtered:
                return min(
                    filtered,
                    key=lambda m: MODELS[m].input_cost_per_1k + MODELS[m].output_cost_per_1k,
                )
            return candidates[0]

        if priority == "speed":
            # 速度優先：エコノミー/スタンダード層を優先
            speed_order = {ModelTier.ECONOMY: 0, ModelTier.STANDARD: 1, ModelTier.PREMIUM: 2}
            return min(
                candidates,
                key=lambda m: speed_order.get(MODELS[m].tier if m in MODELS else ModelTier.STANDARD, 1),
            )

        # 品質優先：プレミアム層を優先
        quality_order = {ModelTier.PREMIUM: 0, ModelTier.STANDARD: 1, ModelTier.ECONOMY: 2}
        return min(
            candidates,
            key=lambda m: quality_order.get(MODELS[m].tier if m in MODELS else ModelTier.STANDARD, 1),
        )

    def get_recommendations(self, task_type: TaskType) -> dict[str, Any]:
        """タスクタイプの推奨設定を取得.

        Args:
            task_type: タスクタイプ

        Returns:
            推奨設定辞書
        """
        return TASK_TYPE_RECOMMENDATIONS.get(task_type, {})
