# -*- coding: utf-8 -*-
"""UCPプロトコルメッセージ定義.

Universal Commerce Protocolのメッセージ構造を定義。
すべてのメッセージはPydanticモデルで型安全に実装。
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class UCPMessageType(str, Enum):
    """UCPメッセージタイプ."""

    # 意図分析
    INTENT_REQUEST = "intent_request"
    INTENT_RESPONSE = "intent_response"
    # オファー
    OFFER_REQUEST = "offer_request"
    OFFER_RESPONSE = "offer_response"
    # トランザクション
    TRANSACTION_REQUEST = "transaction_request"
    TRANSACTION_RESPONSE = "transaction_response"
    # 通知
    NOTIFICATION = "notification"
    # エラー
    ERROR = "error"


class UCPMessage(BaseModel):
    """UCPメッセージ基底クラス."""

    message_id: str = Field(..., description="メッセージID")
    message_type: UCPMessageType = Field(..., description="メッセージタイプ")
    timestamp: datetime = Field(default_factory=datetime.now, description="タイムスタンプ")
    version: str = Field(default="1.0", description="UCPプロトコルバージョン")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")

    model_config = {"extra": "allow"}


class UCPIntentRequest(UCPMessage):
    """意図分析リクエスト."""

    message_type: UCPMessageType = Field(
        default=UCPMessageType.INTENT_REQUEST, frozen=True
    )
    user_input: str = Field(..., description="ユーザー入力テキスト")
    user_id: str | None = Field(default=None, description="ユーザーID")
    session_id: str | None = Field(default=None, description="セッションID")
    conversation_history: list[dict[str, str]] = Field(
        default_factory=list, description="会話履歴"
    )
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")


class UCPIntentResponse(UCPMessage):
    """意図分析レスポンス."""

    message_type: UCPMessageType = Field(
        default=UCPMessageType.INTENT_RESPONSE, frozen=True
    )
    intent_id: str = Field(..., description="意図ID")
    intent_type: str = Field(..., description="意図タイプ")
    confidence: float = Field(..., ge=0.0, le=1.0, description="信頼度スコア")
    product_categories: list[str] = Field(
        default_factory=list, description="商品カテゴリ"
    )
    keywords: list[str] = Field(default_factory=list, description="キーワード")
    price_range: dict[str, float | None] = Field(
        default_factory=lambda: {"min": None, "max": None}, description="価格範囲"
    )
    recommended_actions: list[str] = Field(
        default_factory=list, description="推奨アクション"
    )


class UCPOfferRequest(UCPMessage):
    """オファー取得リクエスト."""

    message_type: UCPMessageType = Field(
        default=UCPMessageType.OFFER_REQUEST, frozen=True
    )
    intent_id: str = Field(..., description="意図ID")
    user_id: str | None = Field(default=None, description="ユーザーID")
    product_ids: list[str] = Field(default_factory=list, description="商品IDリスト")
    keywords: list[str] = Field(default_factory=list, description="キーワード")
    limit: int = Field(default=10, ge=1, le=100, description="最大取得数")
    filters: dict[str, Any] = Field(default_factory=dict, description="フィルター条件")


class UCPOfferResponse(UCPMessage):
    """オファー取得レスポンス."""

    message_type: UCPMessageType = Field(
        default=UCPMessageType.OFFER_RESPONSE, frozen=True
    )
    offers: list[dict[str, Any]] = Field(default_factory=list, description="オファーリスト")
    total_count: int = Field(default=0, ge=0, description="総オファー数")
    has_more: bool = Field(default=False, description="追加オファーの有無")
    recommendations: list[dict[str, Any]] = Field(
        default_factory=list, description="AI推薦"
    )


class UCPTransactionRequest(UCPMessage):
    """トランザクションリクエスト."""

    message_type: UCPMessageType = Field(
        default=UCPMessageType.TRANSACTION_REQUEST, frozen=True
    )
    action: str = Field(..., description="アクション（create/update/complete/cancel）")
    cart_id: str | None = Field(default=None, description="カートID")
    transaction_id: str | None = Field(default=None, description="トランザクションID")
    items: list[dict[str, Any]] = Field(default_factory=list, description="商品アイテム")
    offers: list[str] = Field(default_factory=list, description="適用オファーID")
    payment_method: str | None = Field(default=None, description="決済方法")
    payment_details: dict[str, Any] = Field(
        default_factory=dict, description="決済詳細"
    )


class UCPTransactionResponse(UCPMessage):
    """トランザクションレスポンス."""

    message_type: UCPMessageType = Field(
        default=UCPMessageType.TRANSACTION_RESPONSE, frozen=True
    )
    transaction_id: str = Field(..., description="トランザクションID")
    status: str = Field(..., description="ステータス")
    cart_id: str | None = Field(default=None, description="カートID")
    subtotal: float = Field(default=0.0, ge=0.0, description="小計")
    discount_amount: float = Field(default=0.0, ge=0.0, description="割引額")
    total_amount: float = Field(default=0.0, ge=0.0, description="合計金額")
    payment_status: str | None = Field(default=None, description="決済ステータス")
    order_id: str | None = Field(default=None, description="注文ID")


class UCPError(UCPMessage):
    """UCPエラーメッセージ."""

    message_type: UCPMessageType = Field(default=UCPMessageType.ERROR, frozen=True)
    error_code: str = Field(..., description="エラーコード")
    error_message: str = Field(..., description="エラーメッセージ")
    details: dict[str, Any] = Field(default_factory=dict, description="エラー詳細")
    recoverable: bool = Field(default=True, description="リカバリ可能かどうか")

