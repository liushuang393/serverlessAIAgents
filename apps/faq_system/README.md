# FAQ System Demo

AgentFlow フレームワーク級 Agent/サービスを使用した FAQ システムのデモアプリケーションです。

## 🆕 v3.0 企業級強化版

最新の v3.0 では、企業向けの本格的な機能が実装されました。

### v3.0 新機能一覧

| 機能カテゴリ | 機能 | 説明 |
|-------------|------|------|
| **社内FAQ** | 双KB隔離 | 社内/対客KB を物理的に隔離 |
| | 保守モード | 規則類は直接摘録優先、自由発揮を抑制 |
| | 必須引用 | 来源/バージョン/更新日を必ず提示 |
| | 工単自動生成 | 不確定回答時に自動でチケット生成 |
| **メンテナンス支援** | 仕様差分総結 | 新旧ドキュメントの差分を自動抽出 |
| | 影響範囲分析 | モジュール/API/DB/テストへの影響を特定 |
| | 成果物自動生成 | Release Note、FAQ更新草案等を自動生成 |
| **高層データ分析** | 語義層 | 指標・ディメンション辞書による口径統一 |
| | SQL護欄 | SELECT限定、ブラックリスト、LIMIT自動付与 |
| | 証拠チェーン | データソース、前提条件、制限事項を明示 |
| **セキュリティ** | RBAC/ABAC | ロール・属性ベースのアクセス制御 |
| | APPI準拠 | PII検出/マスク、MyNumber完全除外 |
| | 監査ログ | 全操作記録、異常検知 |

### 起動方法

```bash
# v3.0 企業級強化版
uvicorn apps.faq_system.main_v3:app --reload --port 8003

# v2.0 強化版
uvicorn apps.faq_system.main_enhanced:app --reload --port 8002

# v1.0 オリジナル版
uvicorn apps.faq_system.main:app --reload --port 8001
```

### 詳細設計書

詳細な設計と使用方法は [DESIGN.md](./DESIGN.md) を参照してください。

### テスト手順

```bash
# 1. NL2SQL 増強サービスの単体テスト（28テスト）
pytest tests/unit/test_nl2sql_services.py -v --no-cov

# 2. FAQ システム全体のテスト
pytest apps/faq_system/tests/ -v --no-cov

# 3. サーバー起動してAPIテスト
uvicorn apps.faq_system.main_v3:app --reload --port 8003

# 4. API テスト（別ターミナル）
curl -X POST http://localhost:8003/api/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "今月の売上TOP10を教えて"}'
```

---

## v2.0 強化版

強化版（`main_enhanced.py`）の機能：

### 新機能

| 機能 | 説明 |
|------|------|
| **富文本レスポンス** | Markdown、コードブロック、表格、チャートを統合表示 |
| **リアルタイム進捗** | WebSocket/SSE による処理進捗のリアルタイム表示 |
| **引用表示** | 回答のソース/引用を明示表示 |
| **チャート自動生成** | データから自動的にEChartsグラフを生成 |
| **ギャップ分析** | 知識ベースの不足を自動検出 |

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
agentflow/skills/builtin/design_skills/ ← 営業資料画像生成
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
| 営業資料画像生成 | `design_skills` | 営業向け画像セットを生成し、ダウンロード可能なアセットを返却 |
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
| `FAQ_SALES_MATERIAL_DIR` | 営業資料画像の出力先ディレクトリ | `/tmp/faq_sales_material` |

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

# MAQ統合入口（FAQ/SQL/営業資料を自動振り分け）
POST /api/maq/chat
{
  "message": "営業資料図を4枚作成して"
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

### 営業資料画像のダウンロード

`/api/chat` または `/api/maq/chat` のレスポンスで `artifacts[].download_url` が返る。

```bash
GET /api/assets/{artifact_id}/download
```

### A2A カード

```bash
GET /api/a2a/card
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

---

## v3.0 クイックスタート

### 社内FAQ検索

```python
from apps.faq_system.backend.agents import InternalKBAgent, InternalKBConfig

# Agent 初期化
config = InternalKBConfig(
    conservative_mode=True,  # 規則類は保守モード
    require_citation=True,   # 引用必須
)
agent = InternalKBAgent(config=config)

# 質問実行
result = await agent.run({
    "question": "年次有給休暇は何日もらえますか？",
    "user_context": {
        "user_id": "user123",
        "role": "employee",
        "department": "営業部",
    },
})

print(f"回答: {result['answer']}")
print(f"信頼度: {result['confidence']}")
print(f"引用: {result['citations']}")
```

### メンテナンス支援

```python
from apps.faq_system.backend.agents import MaintenanceAgent

agent = MaintenanceAgent()

# 仕様差分分析
result = await agent.run({
    "action": "full",
    "old_document": old_spec,
    "new_document": new_spec,
})

print(f"差分: {result['diffs']}")
print(f"影響: {result['impact']}")
print(f"Release Note: {result['deliverables']['release_note']}")
```

### データ分析（NL2SQL 増強版）

```python
from apps.faq_system.backend.agents import AnalyticsAgent, AnalyticsConfig, NL2SQLEnhancementConfig

# NL2SQL 増強設定
nl2sql_config = NL2SQLEnhancementConfig(
    enable_schema_linking=True,   # Schema Linking 有効
    schema_linking_use_llm=False, # LLM スコアリング（オプション）
    enable_fewshot=True,          # Few-shot 動的選択
    fewshot_k=3,                  # 類似例の数
    enable_postprocess=True,      # SQL 後処理（検証・修正）
)

config = AnalyticsConfig(
    nl2sql_enhancement=nl2sql_config,
)

agent = AnalyticsAgent(config=config)

result = await agent.run({
    "question": "今月の売上TOP10を教えてください",
    "user_context": {"role": "analyst"},
})

print(f"回答: {result['answer']}")
print(f"SQL: {result['sql']}")
print(f"証拠チェーン: {result['evidence_chain']}")
```

#### NL2SQL 増強機能

| 機能 | 説明 | 設定 |
|------|------|------|
| **Schema Linking** | 関連テーブル・カラムを自動選択（全スキーマをLLMに渡さない） | `enable_schema_linking` |
| **Few-shot 動的選択** | BM25 類似度で最適な例を選択（ベクトルDB不要） | `enable_fewshot`, `fewshot_k` |
| **SQL 後処理** | 構文検証、セキュリティ検証、自動修正 | `enable_postprocess` |

#### DataAnalyticsAgent（統一入口）

フレームワーク層の統一 Agent も利用可能：

```python
from agentflow.agents import DataAnalyticsAgent, DataAnalyticsConfig

agent = DataAnalyticsAgent(config=DataAnalyticsConfig(
    db_schema={"sales": ["id", "amount", "date", "region"]},
    auto_chart=True,
    auto_insights=True,
    enable_dsl_pipeline=True,  # NL → DSL → SQL パイプライン
))

result = await agent.run({"question": "今月の売上TOP10を教えて"})
print(f"SQL: {result['sql']}")
print(f"DSL: {result['dsl']}")      # 中間表現
print(f"Chart: {result['chart']}")  # 自動生成チャート
print(f"Insights: {result['insights']}")  # データインサイト
```

### 術語辞書

```python
from apps.faq_system.backend.services import GlossaryService

glossary = GlossaryService()

# クエリ拡張（同義語展開）
expanded = glossary.expand_query("有休申請")
# ["有休申請", "年次有給休暇申請", "休暇申請", ...]
```

### APPI準拠（PII検出）

```python
from apps.faq_system.backend.security import APPIComplianceChecker

checker = APPIComplianceChecker()

# PII検出＆マスク
text = "山田太郎のマイナンバーは123456789012です"
masked = checker.mask_pii(text)
# "山田太郎のマイナンバーは************です"
```

---

## ライセンス

MIT License
