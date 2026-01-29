# Context Engineering ガイド

AgentFlow の上下文エンジニアリング機能の完全ガイド。

## 概要

Context Engineering は、LLM の「注意力」を予算として管理し、上下文を最適化する設計パターンです。

### 設計原則

| 原則 | 説明 | 予算目安 |
|------|------|----------|
| **システムプロンプト制限** | コア指示に集中、冗長な説明を排除 | ≤500 token |
| **動的ツール選択** | 全ツールではなく関連ツールのみ公開 | Top 5-7 |
| **検索判定** | 毎回検索ではなく必要時のみRAG | 条件付き |
| **コンテキスト注入** | 検索結果は重要な1-2片段のみ | Top 1-2 |
| **会話圧縮** | 長い会話は定期的に摘要 | 10ターンごと |
| **重要情報保持** | 圧縮されても失われない Notes | 永続化 |
| **結果フィルタリング** | 子Agent結果から中間状態を除去 | 最終出力のみ |

## クイックスタート

### 統合インターフェース（推奨）

```python
from agentflow import ContextEngineer, ContextConfig, BudgetConfig

# 設定
config = ContextConfig(
    budget_config=BudgetConfig(
        system_prompt_budget=500,
        tools_budget=300,
        rag_context_budget=2000,
        history_budget=4000,
    ),
)

# 初期化
engineer = ContextEngineer(config=config)
await engineer.start()

# メッセージ追加（自動圧縮対応）
engineer.add_message("user", "APIの仕様を教えてください")
engineer.add_message("assistant", "どのAPIでしょうか？")
engineer.add_message("user", "決済APIです")

# コンテキスト構築
context = await engineer.build_context(
    query="決済APIの仕様を教えて",
    base_prompt="あなたは親切な技術アシスタントです。",
    available_tools=tool_provider.list_tools(),
    rag_search_func=rag_pipeline.search,
)

# 使用
response = await llm.chat(
    messages=[
        {"role": "system", "content": context.system_prompt},
        *context.messages,
    ],
    tools=[t.to_openai_format() for t in context.tools],
)
```

## コンポーネント詳細

### 1. TokenBudgetManager（Token予算管理）

上下文の各部分にToken予算を配分し、自動的に切り詰めを行う。

```python
from agentflow import TokenBudgetManager, BudgetConfig

# 設定
config = BudgetConfig(
    system_prompt_budget=500,   # システムプロンプト予算
    tools_budget=300,           # ツール説明予算
    rag_context_budget=2000,    # RAGコンテキスト予算
    history_budget=4000,        # 会話履歴予算
    key_notes_budget=500,       # 重要Notes予算
)

manager = TokenBudgetManager(config=config)

# システムプロンプト配分
allocation = manager.allocate_system_prompt(
    base_prompt="あなたは...",  # 長いプロンプト
    skills=[skill1, skill2],    # スキル追加
)

print(f"Token数: {allocation.token_count}")  # 500以内
print(f"切り詰め: {allocation.truncated}")   # True/False

# RAGコンテキスト配分（Top 1-2のみ）
rag_allocation = manager.allocate_rag_context(
    chunks=search_results,
    max_chunks=2,  # 最大2片段
)

# 使用量サマリ
print(manager.get_usage_summary())
```

### 2. ToolRelevanceSelector（ツール関連性選択）

クエリに基づいて最も関連性の高いツールを動的に選択。

```python
from agentflow import ToolRelevanceSelector, SelectionConfig

# 設定
config = SelectionConfig(
    max_tools=7,              # 最大ツール数
    min_score=0.1,            # 最小スコア閾値
    keyword_weight=0.3,       # キーワードスコア重み
    semantic_weight=0.5,      # セマンティックスコア重み
    safety_weight=0.2,        # 安全性スコア重み
)

selector = ToolRelevanceSelector(
    config=config,
    embedding_provider=get_embedding(),  # オプション
)

# ツール選択
selected_tools = await selector.select_relevant_tools(
    query="データベースから顧客を検索",
    all_tools=tool_provider.list_tools(),
    max_tools=7,
)

# 詳細スコア取得
scores = await selector.score_tools(query, all_tools)
for score in scores[:5]:
    print(f"{score.tool.name}: {score.final_score:.3f}")
```

### 3. RetrievalGate（RAG検索判定）

全てのクエリで検索するのではなく、必要な場合のみRAGを実行。

```python
from agentflow import RetrievalGate, GateConfig

# 設定
config = GateConfig(
    confidence_threshold=0.5,
    enable_query_rewrite=True,
    domain_keywords=["API", "設計", "仕様"],  # ドメインキーワード
)

gate = RetrievalGate(config=config)

# 検索必要性判定
decision = await gate.should_retrieve(
    query="このAPIの仕様を教えて",
    context={"existing_info": "..."},
)

if decision.should_retrieve:
    print(f"検索実行: {decision.reason.value}")
    print(f"推奨クエリ: {decision.suggested_query}")
    results = await rag.search(decision.suggested_query)
else:
    print(f"検索スキップ: {decision.reason.value}")

# 判定例
# "こんにちは"           -> False (casual_chat)
# "文書の内容を教えて"   -> True  (explicit_request)
# "100+200を計算して"    -> False (simple_task)
# "APIの仕様を検索して"  -> True  (explicit_request)
```

### 4. KeyNotesStore（重要Notes永続化）

会話中の重要情報を抽出・保存し、圧縮されても失われないように管理。

```python
from agentflow import KeyNotesStore, NoteImportance, StoreConfig

# 設定
config = StoreConfig(
    max_notes=100,
    dedup_threshold=0.8,      # 重複判定閾値
    auto_extract=True,        # 自動抽出有効化
)

store = KeyNotesStore(config=config)

# 手動追加
note = store.add_note(
    content="予算は100万円以内",
    importance=NoteImportance.HIGH,
    category=NoteCategory.DECISION,
)

# 自動抽出
notes = await store.extract_and_store(
    content="私の名前は田中太郎です。東京在住です。",
    source="user_message",
)

# 取得
all_notes = store.get_all_notes()
high_notes = store.get_notes_by_importance(NoteImportance.HIGH)

# コンテキスト文字列として取得
context_str = store.to_context_string(
    max_tokens=500,
    min_importance=NoteImportance.MEDIUM,
)
```

### 5. TurnBasedCompressor（ターン圧縮）

会話のターン数に基づいて自動的に圧縮を実行。

```python
from agentflow import TurnBasedCompressor, TurnConfig

# 設定
config = TurnConfig(
    turn_threshold=10,        # 10ターンごとに圧縮
    token_threshold=4000,     # Token数でも圧縮トリガー
    keep_recent_turns=3,      # 最近3ターンは保持
    max_summary_tokens=300,   # 要約の最大Token数
    extract_notes=True,       # KeyNotes抽出有効化
)

compressor = TurnBasedCompressor(
    config=config,
    key_notes_store=KeyNotesStore(),
)

# メッセージ追加
for i in range(15):
    compressor.add_message("user", f"質問{i}")
    compressor.add_message("assistant", f"回答{i}")

# 圧縮チェック
if compressor.should_compress():
    result = await compressor.compress()
    print(f"圧縮: {result.original_count} -> {result.compressed_count}")
    print(f"削減率: {result.token_reduction * 100:.1f}%")
    print(f"抽出Notes: {result.extracted_notes_count}")

# メッセージ取得
messages = compressor.get_messages_as_dicts()

# 統計
stats = compressor.get_stats()
```

### 6. ResultSummarizer（子Agent結果フィルター）

子Agentの実行結果から中間状態を除去し、最終結果のみを返す。

```python
from agentflow.patterns.deep_agent import (
    ResultSummarizer,
    SummarizerConfig,
    FilterLevel,
    DeepAgentResultFilter,
)

# 設定
config = SummarizerConfig(
    filter_level=FilterLevel.STANDARD,
    max_result_length=1000,
    include_metadata=False,
    excluded_keys=[
        "debug", "debug_info", "trace",
        "intermediate", "intermediate_steps",
        "raw", "raw_response",
    ],
)

summarizer = ResultSummarizer(config=config)

# 結果サマライズ
results = {
    "task1": {
        "status": "completed",
        "output": "調査結果です",
        "debug_info": {"internal": "debug"},        # 除去される
        "intermediate_steps": ["step1", "step2"],   # 除去される
    },
    "task2": {
        "status": "completed",
        "output": "分析結果です",
        "raw_response": "raw data",                 # 除去される
    },
}

summarized = await summarizer.summarize_results(results)

print(f"ステータス: {summarized.status}")
print(f"成功: {summarized.success_count}/{summarized.task_count}")
print(f"フィルター後: {summarized.final_output}")

# DeepAgent専用フィルター
filter = DeepAgentResultFilter()
filtered = await filter.filter_coordinator_results(
    todos=todo_list,
    results=execution_results,
)
```

## 使用パターン

### パターン1: シンプルなAgent

```python
from agentflow import ContextEngineer

engineer = ContextEngineer()
await engineer.start()

@agent
class SimpleAgent:
    system_prompt = "アシスタント"

    async def run(self, query: str):
        engineer.add_message("user", query)

        context = await engineer.build_context(
            query=query,
            base_prompt=self.system_prompt,
        )

        response = await get_llm().chat([
            {"role": "system", "content": context.system_prompt},
            *context.messages,
        ])

        engineer.add_message("assistant", response)
        return response
```

### パターン2: RAG Agent

```python
from agentflow import ContextEngineer, RetrievalGate

engineer = ContextEngineer()
gate = RetrievalGate()

async def rag_query(query: str, rag_pipeline):
    # 検索必要性判定
    decision = await gate.should_retrieve(query)

    rag_func = rag_pipeline.search if decision.should_retrieve else None

    context = await engineer.build_context(
        query=query,
        base_prompt="知識ベースアシスタント",
        rag_search_func=rag_func,
    )

    return await llm.chat(...)
```

### パターン3: マルチAgent協調

```python
from agentflow import ContextEngineer
from agentflow.patterns.deep_agent import (
    DeepAgentCoordinator,
    ResultSummarizer,
)

# 子Agent用コンテキスト（独立）
child_engineers = {}

async def execute_sub_task(task_id: str, task: str):
    # 子Agent用に独立したコンテキスト
    if task_id not in child_engineers:
        child_engineers[task_id] = ContextEngineer()
        await child_engineers[task_id].start()

    engineer = child_engineers[task_id]
    context = await engineer.build_context(...)
    result = await execute(context)

    return result

# 結果フィルタリング（中間状態除去）
summarizer = ResultSummarizer()
results = await execute_all_tasks(tasks)
final_result = await summarizer.summarize_results(results)

# 親Agentには最終結果のみ返す
return final_result.final_output
```

## API リファレンス

### ContextEngineer

| メソッド | 説明 |
|---------|------|
| `start()` | エンジニア開始 |
| `stop()` | エンジニア停止 |
| `add_message(role, content)` | メッセージ追加 |
| `build_context(query, ...)` | コンテキスト構築 |
| `remember(content, source)` | 重要情報記憶 |
| `add_domain_keywords(keywords)` | ドメインキーワード追加 |
| `get_stats()` | 統計取得 |
| `reset()` | 状態リセット |

### BuiltContext

| フィールド | 説明 |
|-----------|------|
| `system_prompt` | 予算内のシステムプロンプト |
| `tools` | 選択されたツール（Top-K） |
| `rag_results` | RAG検索結果（必要時のみ） |
| `messages` | 圧縮済みメッセージ履歴 |
| `key_notes` | 重要Notes文字列 |
| `metadata` | メタデータ |

### BudgetConfig

| パラメータ | デフォルト | 説明 |
|-----------|----------|------|
| `system_prompt_budget` | 500 | システムプロンプト予算 |
| `tools_budget` | 300 | ツール説明予算 |
| `rag_context_budget` | 2000 | RAGコンテキスト予算 |
| `history_budget` | 4000 | 会話履歴予算 |
| `key_notes_budget` | 500 | 重要Notes予算 |
| `total_budget` | 8000 | 総予算 |

### RetrievalReason

| 値 | 説明 |
|----|------|
| `EXPLICIT_REQUEST` | 明示的な検索要求 |
| `FACTUAL_QUESTION` | 事実に関する質問 |
| `DOMAIN_RELATED` | ドメイン関連 |
| `CASUAL_CHAT` | 雑談（検索不要） |
| `SIMPLE_TASK` | 単純タスク（検索不要） |
| `META_QUESTION` | メタ質問（検索不要） |
| `CONTEXT_SUFFICIENT` | 既存コンテキストで十分 |

### NoteImportance

| 値 | 説明 |
|----|------|
| `CRITICAL` | 絶対保持（ユーザー名、設定等） |
| `HIGH` | 高優先度（明示的要求、決定事項） |
| `MEDIUM` | 中優先度（関連情報） |
| `LOW` | 低優先度（補足情報） |

## ベストプラクティス

### 1. 予算設定

```python
# 一般的な設定
BudgetConfig(
    system_prompt_budget=500,   # コア指示に集中
    tools_budget=300,           # 5-7ツール分
    rag_context_budget=2000,    # 1-2片段分
    history_budget=4000,        # 最近の会話
)

# RAG重視の設定
BudgetConfig(
    system_prompt_budget=300,
    rag_context_budget=3000,    # より多くの検索結果
)
```

### 2. ドメインキーワード設定

```python
# 技術ドキュメント用
gate = RetrievalGate()
gate.add_domain_keywords([
    "API", "SDK", "仕様", "設計", "アーキテクチャ",
    "エラー", "例外", "トラブルシューティング",
])
```

### 3. Notes の活用

```python
# 重要情報は明示的に記憶
await engineer.remember(
    content="ユーザーは Python 3.13 を使用",
    source="system_info",
)

# 自動抽出に任せる
# (会話中の重要情報は自動的に抽出される)
```

## トラブルシューティング

### Token数が正確でない

```python
# tiktoken を使用して正確に計算
pip install tiktoken

# または設定でモデルを指定
from agentflow.context.budget_manager import TiktokenCounter
counter = TiktokenCounter(model="gpt-4")
manager = TokenBudgetManager(counter=counter)
```

### 検索が必要なのにスキップされる

```python
# ドメインキーワードを追加
gate.add_domain_keywords(["your", "domain", "keywords"])

# または閾値を調整
gate.set_confidence_threshold(0.4)  # 下げる
```

### 圧縮が頻繁すぎる

```python
# ターン閾値を上げる
config = TurnConfig(
    turn_threshold=15,      # 10 -> 15
    token_threshold=6000,   # 4000 -> 6000
)
```
