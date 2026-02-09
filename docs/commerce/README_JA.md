# AI商取引モジュール (agentflow.commerce)

## 概要

`agentflow.commerce` モジュールは、GoogleのAgentic Commerce戦略（UCP、意図広告、AI主導商取引）に対応するための抽象インターフェースと実装を提供します。

### 設計原則

- **高凝集 (High Cohesion)**: 各インターフェースは単一責任（SRP）
- **低結合 (Low Coupling)**: ABC + 依存性注入（DIP）パターン
- **型安全**: 100% 型アノテーション + Pydantic v2
- **非同期**: すべてのメソッドは async
- **拡張性**: extra="allow" で将来の属性追加に対応

---

## インストール

```bash
pip install -e ".[dev]"
```

---

## モジュール構造

```
agentflow/commerce/
├── __init__.py              # メインエントリポイント
├── models/                   # データモデル（Pydantic v2）
│   ├── product.py           # Product, ProductCategory
│   ├── offer.py             # Offer, OfferType, DirectOffer
│   ├── cart.py              # Cart, CartItem
│   ├── transaction.py       # Transaction, TransactionStatus
│   └── intent.py            # PurchaseIntent, IntentType
├── interfaces/               # 抽象インターフェース
│   ├── core.py              # IProduct, IOffer, ICart, ITransaction, IPayment
│   ├── agents.py            # IIntentAnalyzer, IOfferProvider, IDealRecommender
│   ├── flow.py              # ICommerceFlow, ICommerceStep, FlowContext
│   └── ai.py                # ICommerceAI
├── providers/                # 具体実装
│   └── mock.py              # テスト用モック実装
└── protocols/ucp/            # UCPプロトコル
    ├── ucp_messages.py      # メッセージ定義
    ├── ucp_config.py        # 設定
    ├── ucp_client.py        # クライアント
    └── ucp_adapter.py       # プロトコルアダプター
```

---

## 基本的な使用方法

### 1. データモデルの使用

```python
from agentflow.commerce.models import (
    Product,
    ProductCategory,
    Offer,
    OfferType,
    Cart,
    CartItem,
    PurchaseIntent,
    IntentType,
)

# 商品を作成
product = Product(
    product_id="laptop-001",
    name="高性能ノートPC",
    price=150000.0,
    category=ProductCategory.ELECTRONICS,
    description="最新のAIチップ搭載",
    intent_keywords=["ノートPC", "AI", "高性能"],
)

# Schema.org形式でエクスポート
schema_org = product.to_schema_org()

# UCP形式でエクスポート
ucp_data = product.to_ucp()
```

### 2. 購買意図の分析

```python
from agentflow.commerce.providers import MockIntentAnalyzer

# モック意図分析器を使用
analyzer = MockIntentAnalyzer()
intent = await analyzer.analyze("ノートPCが欲しい")

print(f"意図タイプ: {intent.intent_type}")
print(f"信頼度: {intent.confidence_score:.0%}")
print(f"購入準備: {intent.is_purchase_ready()}")
```

### 3. オファーの取得

```python
from agentflow.commerce.providers import MockOfferProvider, MockOfferMatcher

# オファープロバイダーを設定
offer_provider = MockOfferProvider()
offer_provider.add_offer(Offer(
    offer_id="offer-001",
    product_id="laptop-001",
    offer_type=OfferType.DISCOUNT,
    discount_value=10.0,
    discount_type="percent",
    target_intents=["ノートPC", "PC"],
))

# 意図に基づくオファーを取得
matcher = MockOfferMatcher(offer_provider)
offers = await matcher.get_offers_for_intent(intent)
```

### 4. カートと取引

```python
from agentflow.commerce.providers import MockCartProvider, MockTransactionProvider

# カートを作成
cart_provider = MockCartProvider()
cart = await cart_provider.create_cart(user_id="user-001")

# アイテムを追加
item = CartItem(
    product_id="laptop-001",
    quantity=1,
    unit_price=150000.0,
)
cart = await cart_provider.add_item(cart.cart_id, item)

# トランザクションを作成
txn_provider = MockTransactionProvider()
transaction = await txn_provider.create_transaction(cart)
```

---

## UCPプロトコルの使用

### 環境変数設定

```bash
export UCP_ENDPOINT_URL="https://ucp.example.com/api/v1"
export UCP_API_KEY=
```

### UCPクライアントの使用

```python
from agentflow.protocols.ucp import UCPClient, UCPConfig

# 環境変数から設定を読み込み
config = UCPConfig.from_env()

# UCPクライアントを使用
async with UCPClient(config) as client:
    # 意図分析
    response = await client.analyze_intent("ノートPCが欲しい")
    
    # オファー取得
    offers = await client.get_offers(response.intent_id)
    
    # トランザクション作成
    txn = await client.create_transaction(
        cart_id="cart-001",
        items=[{"product_id": "laptop-001", "quantity": 1}],
    )
```

### UCPアダプターの使用（AgentFlow統合）

```python
from agentflow.protocols.ucp import UCPAdapter, UCPConfig

config = UCPConfig.from_env()

async with UCPAdapter(config) as adapter:
    # IIntentAnalyzer互換
    intent_analyzer = adapter.create_intent_analyzer()
    intent = await intent_analyzer.analyze("ノートPCが欲しい")
    
    # IOfferProvider互換
    offer_provider = adapter.create_offer_provider()
    offers = await offer_provider.get_offers_for_intent(intent)
    
    # IDealRecommender互換
    recommender = adapter.create_deal_recommender()
    deals = await recommender.recommend(intent, offers=offers)
```

---

## カスタム実装の作成

### IIntentAnalyzerの実装例

```python
from agentflow.commerce.interfaces import IIntentAnalyzer
from agentflow.commerce.models import PurchaseIntent, IntentType

class MyIntentAnalyzer(IIntentAnalyzer):
    """カスタム意図分析器."""

    def __init__(self, llm_provider):
        self._llm = llm_provider

    async def analyze(
        self,
        user_input: str,
        *,
        conversation_context: list[dict[str, str]] | None = None,
        user_id: str | None = None,
    ) -> PurchaseIntent:
        # LLMを使用して意図を分析
        result = await self._llm.complete(
            f"購買意図を分析してください: {user_input}"
        )
        # 結果をPurchaseIntentに変換
        return PurchaseIntent(
            intent_id=f"intent-{uuid.uuid4().hex[:8]}",
            user_input=user_input,
            intent_type=IntentType.PURCHASE,
            confidence_score=0.9,
        )
```

---

## テストの実行

```bash
# commerceモジュールのテストを実行
pytest tests/unit/commerce/ -v

# カバレッジ付き
pytest tests/unit/commerce/ --cov=agentflow.commerce
```

---

## アーキテクチャ図

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                        │
│              (Decision Governance Engine等)                 │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                      Flow Layer                             │
│         ICommerceFlow, ICommerceStep, FlowContext          │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                     Agent Layer                             │
│   IIntentAnalyzer, IOfferProvider, IDealRecommender        │
│                      ICommerceAI                            │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    Provider Layer                           │
│     IProduct, IOffer, ICart, ITransaction, IPayment        │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                   Protocol Layer                            │
│           UCPClient, UCPAdapter, UCPMessages               │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                 Infrastructure Layer                        │
│          外部UCPサービス、決済システム、データベース          │
└─────────────────────────────────────────────────────────────┘
```

---

## 関連ドキュメント

- [AgentFlow メインドキュメント](../../README.md)
- [プロトコル仕様](../protocols/)
- [APIリファレンス](../api/)

---

## ライセンス

MIT License
