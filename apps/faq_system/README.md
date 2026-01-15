# FAQ System Demo

AgentFlow フレームワーク級 Agent/サービスを使用した FAQ システムのデモアプリケーションです。

## 🆕 v2.0 強化版

新しい強化版（`main_enhanced.py`）では以下の機能が追加されました：

### 新機能

| 機能 | 説明 |
|------|------|
| **富文本レスポンス** | Markdown、コードブロック、表格、チャートを統合表示 |
| **リアルタイム進捗** | WebSocket/SSE による処理進捗のリアルタイム表示 |
| **引用表示** | 回答のソース/引用を明示表示 |
| **チャート自動生成** | データから自動的にEChartsグラフを生成 |
| **ギャップ分析** | 知識ベースの不足を自動検出 |

### 起動方法

```bash
# v2.0 強化版
uvicorn apps.faq_system.main_enhanced:app --reload --port 8002

# v1.0 オリジナル版
uvicorn apps.faq_system.main:app --reload --port 8001
```

### 対応コンポーネント

- **Markdown**: 見出し、リスト、リンク、引用
- **コードブロック**: シンタックスハイライト（Python, SQL, JSON等）
- **データテーブル**: ソート、フィルタ、ページネーション
- **チャート**: 棒グラフ、折れ線、円グラフ（ECharts）
- **引用/Citation**: ソース表示、関連度スコア

---

## ⚠️ 重要：アーキテクチャについて

このアプリは **薄い App 層** として設計されています。
**業務ロジックはすべてフレームワーク側で実装** されています。

### Agent/サービスの場所

| コンポーネント | 場所 | 説明 |
|---------------|------|------|
| **FAQAgent** | `agentflow/agents/faq_agent.py` | FAQ 専門 Agent（ResilientAgent 継承） |
| **FAQInput/Output** | `agentflow/agents/faq_agent.py` | 型安全な入出力スキーマ |
| **RAGService** | `agentflow/services/` | RAG 検索サービス |
| **Text2SQLService** | `agentflow/services/` | SQL 生成サービス |

```
apps/faq_system/          ← App層（薄い：APIルーティングのみ）
    └── main.py           ← FAQAgentを呼び出すのみ
        │
        ▼
agentflow/agents/         ← Agent層（新アーキテクチャ）
    └── faq_agent.py      ← FAQAgent（ResilientAgent継承）
        │
        ▼
agentflow/services/       ← サービス層
    ├── rag_service.py
    ├── text2sql_service.py
    ├── chart_service.py
    └── suggestion_service.py
```

### Agent 実装パターン（必読）

新しい Agent を作成する際は、以下のパターンに従ってください：

```python
from agentflow import ResilientAgent
from pydantic import BaseModel

# 1. 入出力スキーマを定義（Pydantic）
class MyInput(BaseModel):
    question: str

class MyOutput(BaseModel):
    answer: str

# 2. ResilientAgent を継承
class MyAgent(ResilientAgent[MyInput, MyOutput]):
    name = "MyAgent"
    temperature = 0.3

    def _parse_input(self, input_data: dict) -> MyInput:
        return MyInput(**input_data)

    async def process(self, input_data: MyInput) -> MyOutput:
        # 業務ロジック
        response = await self._call_llm(f"質問: {input_data.question}")
        return MyOutput(answer=response)
```

## 機能

| 機能 | サービス | 説明 |
|------|----------|------|
| RAG 検索 | `RAGService` | ナレッジベースを検索して回答を生成 |
| Text2SQL | `Text2SQLService` | 自然言語からSQLを生成して実行 |
| チャート生成 | `ChartService` | クエリ結果からチャートを自動生成 |
| 提案生成 | `SuggestionService` | フォローアップ質問を提案 |
| 認証 | `AuthService` | JWT/API Key 認証 |

## 起動方法

```bash
# 開発サーバー起動
uvicorn apps.faq_system.main:app --reload --port 8001

# または
python -m apps.faq_system.main
```

## 環境変数

| 変数名 | 説明 | デフォルト |
|--------|------|----------|
| `RAG_COLLECTION` | RAGコレクション名 | `faq_knowledge` |
| `DB_SCHEMA` | DBスキーマJSON | `{}` |

## API エンドポイント

### チャット

```bash
# 同期
POST /api/chat
{
  "message": "返品ポリシーを教えて"
}

# ストリーム（SSE）
POST /api/chat/stream
{
  "message": "今月の売上TOP10は？"
}
```

### RAG

```bash
# クエリ
POST /api/rag/query
{
  "question": "返品ポリシーは？",
  "collection": "faq_knowledge",
  "top_k": 5
}

# ドキュメント追加
POST /api/rag/add
{
  "content": "返品は30日以内に...",
  "metadata": {"category": "policy"}
}
```

### SQL

```bash
POST /api/sql/query
{
  "question": "今月の売上合計は？"
}
```

## Studio からの利用

このアプリの機能は Studio UI からノーコードで利用できます：

1. **RAGノード**: ナレッジベース検索
2. **Text2SQLノード**: データベースクエリ
3. **チャートノード**: 可視化
4. **提案ノード**: フォローアップ生成

```
GET /api/nodes/service
→ 利用可能なサービスノード一覧を取得
```

## アーキテクチャ

```
┌─────────────────────────────────────────────────┐
│                   App Layer                      │
│  apps/faq_system/main.py                        │
│  - API エンドポイント定義                        │
│  - FAQAgent 呼び出し                            │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│               Agent Layer (NEW)                  │
│  agentflow/agents/faq_agent.py                  │
│  - FAQAgent (ResilientAgent 継承)               │
│  - FAQInput/FAQOutput (Pydantic)                │
│  - 自動リトライ・タイムアウト制御               │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│               Service Layer                      │
│  agentflow/services/                            │
│  ├── rag_service.py      ← RAG 検索            │
│  ├── text2sql_service.py ← SQL 生成・実行      │
│  ├── chart_service.py    ← チャート生成        │
│  └── suggestion_service.py ← 提案生成          │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│                 Core Layer                       │
│  - LLM Provider（松耦合・環境変数から自動取得） │
│  - Vector DB Provider                           │
│  - Database Provider                            │
└─────────────────────────────────────────────────┘
```

## 注意事項（利用者向け）

### ❌ やってはいけないこと

1. **`apps/faq_system/backend/agents/` に独自 Agent を作成しない**
   - Agent はフレームワーク層（`agentflow/agents/`）に配置
   - App 層は API ルーティングのみ

2. **`AgentBlock` を直接継承しない**
   - 必ず `ResilientAgent[Input, Output]` を継承
   - 型パラメータで入出力を明示

3. **`self._llm.chat()` を直接呼び出さない**
   - `self._call_llm(prompt)` を使用（ResilientAgent が提供）
   - 自動リトライ・タイムアウトが適用される

### ✅ やるべきこと

1. **Pydantic で入出力スキーマを定義**
2. **`_parse_input()` と `process()` を実装**
3. **内部メソッドは `_` または `__` でプレフィックス**

## ライセンス

MIT License
