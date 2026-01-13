# Agent と Service の境界設計

> ⚠️ **AI向け指示**: 新機能を実装する前に、必ずこのドキュメントを確認すること。

---

## 🎯 核心原則

### Service（サービス）
- **無状態**の機能モジュール
- **単一責任**（RAG検索だけ、SQL生成だけ）
- App/Agent/Studio から呼び出される**部品**

### Agent（エージェント）
- **有状態**の対話エンティティ
- **複数のService/Toolを協調**する
- ユーザーの**意図を理解**して適切なServiceを選択

---

## 📊 判断フローチャート

```
新機能を実装する際の判断:

Q1: ユーザーとの対話が必要？
    ├─ YES → Q2へ
    └─ NO → Service として実装

Q2: 複数のService/Toolを組み合わせる必要がある？
    ├─ YES → Agent として実装
    └─ NO → 既存Agentに機能追加、または Service

Q3: 通用Agent（AgentPool）で対応可能？
    ├─ YES → AgentPool の既存Agent を使用
    └─ NO → 専門化Agent を新規作成
```

---

## 🏗️ アーキテクチャ図

```
┌─────────────────────────────────────────────────────────────┐
│                        App Layer                            │
│   (薄い: パラメータを渡してAgent/Serviceを呼び出すのみ)      │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      Agent Layer                            │
│  ┌──────────────────┐  ┌──────────────────────────────────┐ │
│  │   AgentPool      │  │    専門化Agent                   │ │
│  │  (6個の通用Agent) │  │  - FAQAgent                     │ │
│  │  - RESEARCH      │  │  - SalesAgent                   │ │
│  │  - ANALYSIS      │  │  - CustomerAgent (将来)          │ │
│  │  - PLANNING      │  │  - ...                          │ │
│  │  - EXECUTION     │  │                                  │ │
│  │  - REVIEW        │  │  ※ 通用Agentで対応不可な場合のみ  │ │
│  │  - REPORT        │  │    新規作成                      │ │
│  └──────────────────┘  └──────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     Service Layer                           │
│  RAGService | Text2SQLService | ChartService | AuthService  │
│  ※ 単一責任、無状態、Agent から呼び出される部品             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      Core Layer                             │
│           LLM | VectorDB | Database | Tools                 │
└─────────────────────────────────────────────────────────────┘
```

---

## ✅ チェックリスト（新機能実装時）

実装前に必ず確認:

- [ ] **既存の通用Agent（AgentPool）で対応可能か？**
  - RESEARCH: 調査・検索 → RAGスキルあり
  - ANALYSIS: 分析・推論
  - PLANNING: 計画・設計
  - EXECUTION: 実行・操作
  - REVIEW: 審査・検証
  - REPORT: 報告・総括

- [ ] **既存の専門化Agentで対応可能か？**
  - FAQAgent: RAG + SQL + Chart + 提案
  - SalesAgent: 売上分析専門

- [ ] **既存のServiceで対応可能か？**
  - RAGService, Text2SQLService, ChartService, SuggestionService, AuthService

- [ ] **新規作成が必要な場合、Agent か Service か？**
  - 対話・協調が必要 → Agent
  - 単一機能 → Service

---

## 📝 例

### ❌ 悪い設計

```python
# App層で直接複数Serviceを呼び出し、ロジックが分散
@app.post("/api/chat")
async def chat(question):
    if is_sql_query(question):
        result = await sql_service.execute(...)
    else:
        result = await rag_service.execute(...)
    suggestions = await suggestion_service.execute(...)
    return result
```

### ✅ 良い設計

```python
# App層はAgentを呼び出すのみ
@app.post("/api/chat")
async def chat(question):
    agent = get_faq_agent()  # 内部でService協調
    return await agent.run({"question": question})
```

---

## 📚 関連ドキュメント

- `agentflow/patterns/__init__.py` - Agent パターン一覧
- `agentflow/services/__init__.py` - Service 一覧
- `agentflow/agents/__init__.py` - 専門化Agent 一覧
