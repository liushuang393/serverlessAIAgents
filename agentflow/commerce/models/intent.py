# -*- coding: utf-8 -*-
"""購買意図データモデル.

Google Gemini の意図広告に対応した購買意図データ構造。
ユーザーの発話から購買意図を抽出・分析。
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class IntentType(str, Enum):
    """意図タイプ."""

    BROWSE = "browse"               # 閲覧
    COMPARE = "compare"             # 比較
    RESEARCH = "research"           # 調査
    PURCHASE = "purchase"           # 購入
    REPURCHASE = "repurchase"       # 再購入
    GIFT = "gift"                   # ギフト
    URGENT = "urgent"               # 緊急購入
    BARGAIN_HUNT = "bargain_hunt"   # お得探し


class IntentConfidence(str, Enum):
    """意図確度レベル."""

    LOW = "low"           # 低（0.0-0.3）
    MEDIUM = "medium"     # 中（0.3-0.7）
    HIGH = "high"         # 高（0.7-0.9）
    VERY_HIGH = "very_high"  # 非常に高（0.9-1.0）


class PurchaseIntent(BaseModel):
    """購買意図モデル.

    ユーザーの発話から抽出された購買意図。
    意図広告（Direct Offers）のトリガーとして使用。

    Attributes:
        intent_id: 意図一意識別子
        user_input: ユーザー入力テキスト
        intent_type: 意図タイプ
        confidence_score: 確度スコア
        product_categories: 対象商品カテゴリ
        price_range: 価格帯
        urgency: 緊急度
    """

    intent_id: str = Field(..., description="意図一意識別子")
    session_id: str | None = Field(default=None, description="セッションID")
    user_id: str | None = Field(default=None, description="ユーザーID")

    # 入力
    user_input: str = Field(..., description="ユーザー入力テキスト")
    conversation_context: list[dict[str, str]] = Field(
        default_factory=list, description="会話コンテキスト"
    )

    # 意図分析結果
    intent_type: IntentType = Field(
        default=IntentType.BROWSE, description="意図タイプ"
    )
    confidence_score: float = Field(
        default=0.0, ge=0.0, le=1.0, description="確度スコア"
    )
    confidence_level: IntentConfidence = Field(
        default=IntentConfidence.LOW, description="確度レベル"
    )

    # 商品関連
    product_categories: list[str] = Field(
        default_factory=list, description="対象商品カテゴリ"
    )
    product_keywords: list[str] = Field(
        default_factory=list, description="商品キーワード"
    )
    brand_preferences: list[str] = Field(
        default_factory=list, description="ブランド嗜好"
    )

    # 価格・予算
    price_range: dict[str, float | None] = Field(
        default_factory=lambda: {"min": None, "max": None},
        description="価格帯",
    )
    budget_sensitivity: str = Field(
        default="medium", description="予算感度（low/medium/high）"
    )

    # 緊急度・タイミング
    urgency: str = Field(default="normal", description="緊急度（low/normal/high/urgent）")
    purchase_timeline: str | None = Field(
        default=None, description="購入予定時期"
    )

    # 推薦オファータイプ
    recommended_offer_types: list[str] = Field(
        default_factory=list, description="推薦オファータイプ"
    )

    # AI分析結果
    ai_analysis: dict[str, Any] = Field(
        default_factory=dict, description="AI分析詳細"
    )
    extracted_entities: list[dict[str, Any]] = Field(
        default_factory=list, description="抽出エンティティ"
    )

    # タイムスタンプ
    created_at: datetime = Field(default_factory=datetime.now, description="作成日時")

    model_config = {"frozen": False, "extra": "allow"}

    def is_purchase_ready(self) -> bool:
        """購入準備完了かどうかを判定.

        Returns:
            購入意図が高い場合True
        """
        return (
            self.confidence_score >= 0.7
            and self.intent_type in [IntentType.PURCHASE, IntentType.URGENT]
        )

    def should_show_offer(self) -> bool:
        """オファー表示すべきかどうかを判定.

        Returns:
            オファー表示すべき場合True
        """
        return self.confidence_score >= 0.5 and self.intent_type != IntentType.BROWSE

    @classmethod
    def from_confidence_score(cls, score: float) -> IntentConfidence:
        """スコアから確度レベルを取得.

        Args:
            score: 確度スコア

        Returns:
            確度レベル
        """
        if score >= 0.9:
            return IntentConfidence.VERY_HIGH
        if score >= 0.7:
            return IntentConfidence.HIGH
        if score >= 0.3:
            return IntentConfidence.MEDIUM
        return IntentConfidence.LOW

