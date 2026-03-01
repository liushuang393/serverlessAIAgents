# FAQ System 改善設計書

> **作成日**: 2026-02-28
> **対象アプリ**: `apps/faq_system/`
> **アプローチ**: B（設計再整合）— バグ・性能・設計不整合・UX・セキュリティを一括対応

---

## 1. 背景・目的

コードレビューにより以下のカテゴリの問題を発見した。本設計はこれらを一括修正し、設計の一貫性を確保する。

| 重大度 | カテゴリ | 件数 |
|--------|---------|------|
| 🔴 バグ | LLM未実装・変数未使用 | 2件 |
| 🔴 性能 | N+1クエリ | 1件 |
| 🟡 設計不整合 | 分類器分裂・Agent未登録・提案固定値 | 3件 |
| 🟠 UX/セキュリティ/規約 | CORS・confirm()・type:ignore | 3件 |

---

## 2. 変更ファイル一覧

| ファイル | 変更種別 | 対応セクション |
|---------|---------|--------------|
| `backend/agents/internal_kb_agent.py` | 修正 | §3.1, §3.2 |
| `backend/services/chat_history_service.py` | 修正 | §3.3, §3.4 |
| `backend/services/query_classifier.py` | **新規作成** | §3.5 |
| `backend/services/faq_service.py` | 修正 | §3.5 |
| `backend/agents/enhanced_faq_agent.py` | 修正 | §3.5, §3.6 |
| `app_config.json` | 修正 | §3.7 |
| `frontend/src/components/layout/Sidebar.tsx` | 修正 | §3.8 |
| `frontend/src/i18n/index.ts` | 修正 | §3.8 |
| `main.py` | 修正 | §3.9 |
| `.env.example` | 修正 | §3.9 |

---

## 3. 設計詳細

### 3.1 `InternalKBAgent._generate_answer` — LLM実装（🔴 バグ）

**問題箇所**: `backend/agents/internal_kb_agent.py:439`

```python
# 現状（ハードコード）
# TODO: 実際のLLM呼び出し
answer = f"参考情報 [1] に基づくと、{search_results[0]['content'][:100]}..."
```

**修正方針**: `ResilientAgent` が持つ `self._llm_client` 経由で LLM を呼び出す。既存の `SYSTEM_PROMPT` を使用。

```python
# 修正後
context = "\n\n".join(context_parts)
prompt_messages = [
    {"role": "system", "content": self.SYSTEM_PROMPT},
    {"role": "user", "content": f"以下のコンテキストを参照して質問に回答してください。\n\nコンテキスト:\n{context}\n\n質問: {question}"},
]
llm_response = await self._llm_client.chat(prompt_messages)
answer = llm_response.get("content", "回答を生成できませんでした。")
```

同様に `_generate_conservative_answer` も LLM を使用するよう修正（`CONSERVATIVE_SYSTEM_PROMPT` 使用）。

---

### 3.2 `run_stream` 未使用変数修正（🔴 バグ）

**問題箇所**: `backend/agents/internal_kb_agent.py:272`

```python
# 修正前 — 戻り値を変数に代入せず捨てている
input_data.get("question", "")

# 修正後
question = input_data.get("question", "")
```

`question` を `yield` の `message` フィールドに活用する（例: `f"「{question[:20]}」を検索中..."`）。

---

### 3.3 `type: ignore` 根本修正（🟠 規約違反）

**問題箇所**: `backend/services/chat_history_service.py:172`

```python
# 修正前
return result.rowcount > 0  # type: ignore[union-attr]

# 修正後 — SQLAlchemy の型を正しく扱う
from sqlalchemy.engine import CursorResult
if isinstance(result, CursorResult):
    return (result.rowcount or 0) > 0
return False
```

---

### 3.4 `list_sessions` N+1クエリ修正（🔴 性能）

**問題箇所**: `backend/services/chat_history_service.py:130-141`

現状は N セッションに対して N+1 回 DB クエリを実行する。

**修正方針**: 各セッションの「最初のユーザーメッセージ」を 1 回のサブクエリで一括取得し、Python 側で辞書にマージする。

```python
# セッション集計クエリ（既存）
agg_stmt = (
    select(
        ChatMessage.session_id,
        func.count(ChatMessage.id).label("message_count"),
        func.max(ChatMessage.created_at).label("last_message_at"),
    )
    .where(ChatMessage.user_id == user.user_id)
    .group_by(ChatMessage.session_id)
    .order_by(func.max(ChatMessage.created_at).desc())
    .limit(min(limit, 200))
    .offset(offset)
)

# 先頭メッセージ一括取得（新規 — N 回を 1 回に削減）
session_ids_subq = agg_stmt.with_only_columns(
    ChatMessage.session_id
).subquery()

first_msg_subq = (
    select(
        ChatMessage.session_id,
        func.min(ChatMessage.created_at).label("first_at"),
    )
    .where(
        ChatMessage.session_id.in_(select(session_ids_subq)),
        ChatMessage.user_id == user.user_id,
        ChatMessage.role == "user",
    )
    .group_by(ChatMessage.session_id)
    .subquery()
)

preview_stmt = (
    select(ChatMessage.session_id, ChatMessage.content)
    .join(
        first_msg_subq,
        (ChatMessage.session_id == first_msg_subq.c.session_id)
        & (ChatMessage.created_at == first_msg_subq.c.first_at),
    )
)

async with get_db_session() as session:
    rows = (await session.execute(agg_stmt)).all()
    previews = dict((await session.execute(preview_stmt)).all())
```

**効果**: クエリ数 `N+1` → `2` 回（セッション数に依存しない）。

---

### 3.5 クエリ分類器の統一（🟡 設計不整合）

**問題**: 同一ロジックが 2 箇所に分裂し、対応言語が異なる。

**新規ファイル**: `backend/services/query_classifier.py`

```python
"""クエリタイプ分類器 — 日本語・中国語・英語対応."""
from __future__ import annotations
from enum import Enum


class QueryType(str, Enum):
    FAQ = "faq"
    SQL = "sql"
    HYBRID = "hybrid"


class QueryClassifier:
    """質問文からクエリタイプを判定する統一分類器."""

    _SQL_KEYWORDS: frozenset[str] = frozenset([
        # 日本語
        "売上", "収入", "数量", "統計", "レポート", "top", "ランキング",
        "トレンド", "比較", "金額", "注文", "顧客数", "件数", "合計",
        "平均", "月別", "年別", "日別",
        # 中国語（既存互換）
        "销售", "收入", "数量", "统计", "报表", "排名", "趋势",
        "对比", "同比", "环比", "金额", "订单", "客户数",
        # 英語
        "revenue", "sales", "count", "report", "ranking", "trend",
        "comparison", "total", "average", "monthly", "yearly",
    ])

    def classify(self, question: str) -> QueryType:
        """質問文を分析してクエリタイプを返す."""
        lower = question.lower()
        score = sum(1 for k in self._SQL_KEYWORDS if k in lower)
        if score >= 2:
            return QueryType.SQL
        if score >= 1:
            return QueryType.HYBRID
        return QueryType.FAQ


# シングルトン
_classifier = QueryClassifier()


def classify_query(question: str) -> QueryType:
    """モジュールレベルのショートカット関数."""
    return _classifier.classify(question)
```

**修正**: `faq_service.py` と `enhanced_faq_agent.py` の `_classify_query` メソッドをこのモジュールに委譲。

---

### 3.6 提案生成をSuggestionServiceに委譲（🟡 設計不整合）

**問題箇所**: `backend/agents/enhanced_faq_agent.py:446`

現状は全質問で同一の固定文3件を返す。

**修正方針**: `dependencies.py` で既に初期化済みの `SuggestionService` を活用する。

```python
async def _generate_suggestions(
    self, question: str, query_type: str
) -> list[dict[str, Any]]:
    """SuggestionService 経由でフォローアップ提案を生成."""
    try:
        result = await self._suggestion_service.execute(
            action="suggest",
            question=question,
            query_type=query_type,
        )
        if result.success:
            return result.data.get("suggestions", [])
    except Exception:
        self._logger.warning("提案生成失敗、フォールバックを使用")

    # フォールバック（LLM 不可時）
    if query_type == "sql":
        return [
            {"text": "前月との比較を見せて", "type": "followup"},
            {"text": "カテゴリ別の内訳は？", "type": "followup"},
            {"text": "トップ10を表示", "type": "followup"},
        ]
    return [
        {"text": "もう少し詳しく教えて", "type": "followup"},
        {"text": "関連する情報は？", "type": "followup"},
        {"text": "例を見せて", "type": "followup"},
    ]
```

`_ensure_initialized` で `SuggestionService` の遅延初期化を追加。

---

### 3.7 `app_config.json` Agent登録漏れ修正（🟡 設計不整合）

`agents[]` に未登録の 5 Agent を追記（code-rules §15.2 フォーマット準拠）。

```json
{
  "name": "EnhancedFAQAgent",
  "module": "apps.faq_system.backend.agents.enhanced_faq_agent",
  "capabilities": ["faq", "rag", "sql", "rich_response", "citation"],
  "business_base": "knowledge",
  "pattern": "specialist"
},
{
  "name": "InternalKBAgent",
  "module": "apps.faq_system.backend.agents.internal_kb_agent",
  "capabilities": ["faq", "rag", "rbac", "conservative_mode", "ticket"],
  "business_base": "knowledge",
  "pattern": "specialist"
},
{
  "name": "ExternalKBAgent",
  "module": "apps.faq_system.backend.agents.external_kb_agent",
  "capabilities": ["faq", "rag", "external_kb"],
  "business_base": "knowledge",
  "pattern": "specialist"
},
{
  "name": "MaintenanceAgent",
  "module": "apps.faq_system.backend.agents.maintenance_agent",
  "capabilities": ["maintenance", "impact_analysis", "documentation"],
  "business_base": "operations",
  "pattern": "specialist"
},
{
  "name": "AnalyticsAgent",
  "module": "apps.faq_system.backend.agents.analytics_agent",
  "capabilities": ["sql", "chart", "analysis", "trend"],
  "business_base": "reasoning",
  "pattern": "analyzer"
}
```

---

### 3.8 Sidebar 削除確認 i18n 対応（🟠 UX）

**問題箇所**: `frontend/src/components/layout/Sidebar.tsx:146`

`confirm('Delete this session?')` を 2 段階インライン確認に置き換え。

```tsx
// useState 追加
const [pendingDeleteId, setPendingDeleteId] = useState<string | null>(null);

// 削除ボタン部分の置き換え
{pendingDeleteId === session.session_id ? (
    <div className="absolute right-1 flex gap-1">
        <button
            onClick={() => { void deleteSession(session.session_id); setPendingDeleteId(null); }}
            className="px-2 py-1 text-[10px] rounded-lg bg-red-500/20 text-red-400 border border-red-500/30"
        >
            {t('sidebar.confirm_delete')}
        </button>
        <button
            onClick={() => setPendingDeleteId(null)}
            className="px-2 py-1 text-[10px] rounded-lg bg-white/5 text-[var(--text-muted)]"
        >
            {t('common.cancel')}
        </button>
    </div>
) : (
    <button onClick={() => setPendingDeleteId(session.session_id)}>
        <Trash2 size={14} />
    </button>
)}
```

**i18n追加キー** (`frontend/src/i18n/index.ts`):
```typescript
'sidebar.confirm_delete': { ja: '削除確認', en: 'Confirm Delete', zh: '确认删除' },
'common.cancel':          { ja: 'キャンセル', en: 'Cancel', zh: '取消' },
```

---

### 3.9 CORS を環境変数で制御（🟠 セキュリティ）

**問題箇所**: `main.py:211`

```python
# 修正後
_raw_origins = os.getenv("FAQ_CORS_ORIGINS", "*")
_cors_origins: list[str] = (
    ["*"] if _raw_origins.strip() == "*"
    else [o.strip() for o in _raw_origins.split(",") if o.strip()]
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=_cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

**`.env.example` 追記**:
```
# CORS 許可オリジン（カンマ区切り。本番では必ず明示指定）
# 例: FAQ_CORS_ORIGINS=https://faq.example.com,https://admin.example.com
FAQ_CORS_ORIGINS=http://localhost:3004
```

---

## 4. テスト方針

| 対象 | テスト内容 |
|------|----------|
| `QueryClassifier` | 日本語・中国語・英語の各キーワードで正しい QueryType が返ること |
| `list_sessions` | セッション有無・複数件でクエリ回数が 2 回以内であること（mock DB） |
| `InternalKBAgent._generate_answer` | LLM が呼び出されること（mock LLM） |
| `Sidebar` | 削除ボタン 1 回目で確認状態、2 回目で `deleteSession` が呼ばれること |
| CORS | `FAQ_CORS_ORIGINS` 未設定時は `["*"]`、設定時はその値が使われること |

---

## 5. 実装順序（推奨）

1. `query_classifier.py` 新規作成（他が依存）
2. `internal_kb_agent.py` バグ修正（LLM実装・変数未使用）
3. `chat_history_service.py` N+1修正・type:ignore修正
4. `faq_service.py` / `enhanced_faq_agent.py` 分類器委譲・提案修正
5. `app_config.json` Agent登録追加
6. `Sidebar.tsx` + `i18n/index.ts` UX修正
7. `main.py` + `.env.example` CORS修正
8. テスト追加・`./check.sh all` 実行

---

## 6. 完了条件（DoD）

- [ ] `./check.sh all` が全通過（Ruff / mypy / pytest 80%+ / ESLint / tsc / build）
- [ ] 各修正に対応するユニットテストが存在する
- [ ] `app_config.json` 更新後に Platform rescan で全 Agent が表示される
- [ ] CORS 設定が `.env.example` にドキュメント化されている
