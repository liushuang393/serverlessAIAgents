# -*- coding: utf-8 -*-
"""UCP (Universal Commerce Protocol) 実装.

GoogleのAgentic Commerce戦略に対応するための
Universal Commerce Protocolクライアント・サーバー・アダプターを提供。

設計原則:
- 非同期I/O: すべての操作はasyncで実装
- 型安全: 100%型アノテーション
- セキュリティ: ホワイトリスト、監査ログ、パラメータ検証
- 拡張性: 抽象インターフェースによる疎結合設計

Example:
    >>> from agentflow.protocols.ucp import UCPClient
    >>> async with UCPClient(config) as client:
    ...     intent = await client.analyze_intent("ノートPCが欲しい")
    ...     offers = await client.get_offers(intent)
"""

from agentflow.protocols.ucp.ucp_messages import (
    UCPMessageType,
    UCPMessage,
    UCPIntentRequest,
    UCPIntentResponse,
    UCPOfferRequest,
    UCPOfferResponse,
    UCPTransactionRequest,
    UCPTransactionResponse,
    UCPError,
)
from agentflow.protocols.ucp.ucp_config import (
    UCPConfig,
    UCPEndpoint,
    UCPSecurityConfig,
)
from agentflow.protocols.ucp.ucp_client import UCPClient
from agentflow.protocols.ucp.ucp_adapter import UCPAdapter

__all__ = [
    # メッセージ
    "UCPMessageType",
    "UCPMessage",
    "UCPIntentRequest",
    "UCPIntentResponse",
    "UCPOfferRequest",
    "UCPOfferResponse",
    "UCPTransactionRequest",
    "UCPTransactionResponse",
    "UCPError",
    # 設定
    "UCPConfig",
    "UCPEndpoint",
    "UCPSecurityConfig",
    # クライアント
    "UCPClient",
    # アダプター
    "UCPAdapter",
]

