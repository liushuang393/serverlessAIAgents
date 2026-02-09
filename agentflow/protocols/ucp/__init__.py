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

from agentflow.protocols.ucp.ucp_adapter import UCPAdapter
from agentflow.protocols.ucp.ucp_client import UCPClient
from agentflow.protocols.ucp.ucp_config import (
    UCPConfig,
    UCPEndpoint,
    UCPSecurityConfig,
)
from agentflow.protocols.ucp.ucp_messages import (
    UCPError,
    UCPIntentRequest,
    UCPIntentResponse,
    UCPMessage,
    UCPMessageType,
    UCPOfferRequest,
    UCPOfferResponse,
    UCPTransactionRequest,
    UCPTransactionResponse,
)


__all__ = [
    # アダプター
    "UCPAdapter",
    # クライアント
    "UCPClient",
    # 設定
    "UCPConfig",
    "UCPEndpoint",
    "UCPError",
    "UCPIntentRequest",
    "UCPIntentResponse",
    "UCPMessage",
    # メッセージ
    "UCPMessageType",
    "UCPOfferRequest",
    "UCPOfferResponse",
    "UCPSecurityConfig",
    "UCPTransactionRequest",
    "UCPTransactionResponse",
]

