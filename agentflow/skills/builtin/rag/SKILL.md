---
name: rag
description: |
  検索増強生成（RAG）Skill。知識ベースへのドキュメント追加、意味検索、LLM 回答生成をサポート。
  ドキュメント Q&A、社内知識検索、カスタム知識ベース構築に使用。
version: 1.0.0
author: AgentFlow Team
triggers:
  - rag
  - retrieval
  - 検索
  - knowledge
  - 知識
  - document
  - ドキュメント
  - qa
  - 質問応答
  - vector
  - embedding
  - semantic search
requirements:
  - openai>=1.0.0
  - chromadb>=0.4.0
tags:
  - retrieval
  - knowledge
  - search
  - core-skill
examples:
  - "ドキュメントから情報を検索"
  - "知識ベースを構築"
  - "RAG で質問応答"
  - "意味検索を実行"
---

# RAG Skill

## 概要

検索増強生成（Retrieval-Augmented Generation）を提供する Skill。
知識ベースからの関連情報取得と LLM による回答生成を統合。

## 機能

| 機能 | 説明 |
|------|------|
| **ドキュメント追加** | テキスト/ファイルを知識ベースに追加 |
| **意味検索** | ベクトル類似度による関連情報検索 |
| **回答生成** | 検索結果を基に LLM で回答生成 |
| **トピック分類** | ドキュメントをトピック別に管理 |
| **ソース追跡** | 回答に使用したソースを追跡 |

## クイックスタート

### 基本的な使い方

```python
from agentflow.skills.rag import RAGSkill

# 初期化
rag = RAGSkill()
await rag.start()

# ドキュメント追加
await rag.add_document(
    content="AgentFlow は軽量な AI エージェント開発フレームワークです。",
    topic="framework",
    metadata={"source": "README.md"},
)

# 質問応答
result = await rag.query("AgentFlow とは何ですか？")
print(result.answer)
# → "AgentFlow は軽量な AI エージェント開発フレームワークです。"
print(result.sources)
# → [{"content": "...", "source": "README.md", "similarity": 0.92}]
```

### カスタム設定

```python
from agentflow.skills.rag import RAGSkill, RAGConfig
from agentflow.llm.llm_client import LLMConfig

# LLM 設定
llm_config = LLMConfig(
    model="gpt-4o",
    temperature=0.3,  # 事実性重視で低め
)

# RAG 設定
rag_config = RAGConfig(
    top_k=5,                    # 上位 5 件を取得
    min_similarity=0.5,         # 最小類似度 50%
    system_prompt="あなたは技術文書の専門家です。",
)

rag = RAGSkill(llm_config=llm_config, config=rag_config)
```

## ドキュメント管理

### テキスト追加

```python
# 単一ドキュメント
doc_id = await rag.add_document(
    content="ドキュメントの内容...",
    topic="general",
    metadata={"author": "user", "date": "2024-01-01"},
)

# 複数ドキュメント一括追加
docs = [
    {"content": "内容1...", "topic": "topic1"},
    {"content": "内容2...", "topic": "topic2"},
]
doc_ids = await rag.add_documents(docs)
```

### ファイル追加

```python
# テキストファイル
await rag.add_file("document.txt", topic="docs")

# Markdown ファイル
await rag.add_file("README.md", topic="readme")

# ディレクトリ一括追加
await rag.add_directory("./docs", pattern="*.md", topic="documentation")
```

### チャンク分割

```python
# 長いドキュメントを自動分割
await rag.add_document(
    content=long_text,
    topic="manual",
    chunk_size=500,      # 500 トークンごと
    chunk_overlap=50,    # 50 トークン重複
)
```

### ドキュメント削除

```python
# ID で削除
await rag.delete_document(doc_id)

# トピックで一括削除
await rag.delete_by_topic("old_topic")
```

## 検索と質問応答

### 基本的な質問応答

```python
result = await rag.query("製品の使い方を教えて")

print(f"回答: {result.answer}")
print(f"使用ソース数: {len(result.sources)}")
print(f"コンテキスト: {result.context_used}")
```

### 検索のみ（LLM なし）

```python
# 関連ドキュメントのみ取得
results = await rag.search(
    query="エラーハンドリング",
    top_k=10,
    topic="docs",  # トピック指定
)

for r in results:
    print(f"類似度: {r['similarity']:.2f} - {r['content'][:100]}...")
```

### トピック指定検索

```python
# 特定トピック内で検索
result = await rag.query(
    question="認証の設定方法は？",
    topic="authentication",
)
```

### フィルタリング検索

```python
# メタデータでフィルタ
result = await rag.query(
    question="最新の更新内容は？",
    filters={"date": {"$gte": "2024-01-01"}},
)
```

## ストリーミング

```python
# ストリーミング回答
async for chunk in rag.query_stream("詳細な説明をして"):
    print(chunk, end="", flush=True)
print()
```

## Agent 統合

### @agent との統合

```python
from agentflow import agent

@agent
class DocumentQAAgent:
    """ドキュメント Q&A Agent"""
    
    system_prompt = "ドキュメントに基づいて質問に答えます。"
    
    def __init__(self):
        self.rag = RAGSkill()
    
    async def initialize(self):
        """初期化時にドキュメント読み込み"""
        await self.rag.start()
        await self.rag.add_directory("./docs", pattern="*.md")
    
    async def run(self, input_data: dict) -> dict:
        question = input_data.get("question", "")
        result = await self.rag.query(question)
        return {
            "answer": result.answer,
            "sources": result.sources,
        }
```

### Decision Governance Engine での使用例

```python
from agentflow.skills.rag import RAGSkill

class ShuAgent(AgentBlock):
    """術 Agent - 事例データベース検索"""
    
    def __init__(self):
        self.rag = RAGSkill()
    
    async def initialize_rag(self):
        """事例データベースを初期化"""
        await self.rag.start()
        # 事例ドキュメントを追加
        await self.rag.add_directory("./cases", topic="historical_cases")
    
    async def run(self, input_data: dict) -> dict:
        decision_context = input_data.get("decision_context", "")
        
        # 類似事例を検索
        result = await self.rag.query(
            question=f"類似した意思決定事例: {decision_context}",
            topic="historical_cases",
        )
        
        return {
            "historical_analysis": result.answer,
            "similar_cases": result.sources,
        }
```

## 設定オプション

| オプション | デフォルト | 説明 |
|-----------|----------|------|
| `top_k` | 5 | 検索結果の上位 K 件 |
| `min_similarity` | 0.3 | 最小類似度閾値 |
| `system_prompt` | "あなたは..." | システムプロンプト |
| `context_template` | "参考情報:..." | コンテキストテンプレート |
| `chunk_size` | 500 | デフォルトチャンクサイズ |
| `chunk_overlap` | 50 | チャンク重複サイズ |

## ベクトルストア

### ChromaDB（デフォルト）

```python
# デフォルトで ChromaDB を使用
rag = RAGSkill()
```

### カスタムベクトルストア

```python
from agentflow.memory.memory_manager import MemoryManager

# カスタム Memory Manager
memory = MemoryManager(
    provider="pgvector",  # PostgreSQL + pgvector
    config={
        "connection_string": "postgresql://...",
    },
)

rag = RAGSkill(memory_manager=memory)
```

## ベストプラクティス

### 1. ドキュメント準備

```python
# 構造化されたドキュメント追加
await rag.add_document(
    content="""
    # タイトル
    
    ## セクション 1
    内容...
    
    ## セクション 2
    内容...
    """,
    topic="structured",
    metadata={
        "title": "ドキュメントタイトル",
        "category": "reference",
    },
)
```

### 2. トピック設計

```python
# トピック別に管理
topics = ["faq", "manual", "api-reference", "troubleshooting"]

for topic in topics:
    await rag.add_directory(f"./docs/{topic}", topic=topic)

# トピック指定で精度向上
result = await rag.query("API の使い方", topic="api-reference")
```

### 3. 回答品質向上

```python
# カスタムプロンプト
rag_config = RAGConfig(
    system_prompt="""
    あなたは技術文書の専門家です。
    - 提供された参考情報のみに基づいて回答してください
    - 情報が不足している場合は、その旨を明示してください
    - コードサンプルを含める場合は、完全な例を示してください
    """,
    context_template="""
    以下の参考情報を使用して質問に答えてください：

    {context}

    ---
    質問: {query}
    """,
)
```

### 4. 更新管理

```python
# ドキュメント更新時
async def update_document(doc_id: str, new_content: str):
    await rag.delete_document(doc_id)
    return await rag.add_document(content=new_content)

# 定期的な再インデックス
async def reindex_all():
    await rag.clear_all()
    await rag.add_directory("./docs")
```

## トラブルシューティング

### 検索結果が不正確

```python
# 類似度閾値を調整
rag_config = RAGConfig(
    min_similarity=0.6,  # 閾値を上げる
    top_k=3,             # 件数を減らす
)
```

### メモリ使用量が大きい

```python
# バッチ処理でドキュメント追加
async def add_large_corpus(files: list[str]):
    batch_size = 100
    for i in range(0, len(files), batch_size):
        batch = files[i:i+batch_size]
        await rag.add_files(batch)
        # メモリ解放
        import gc
        gc.collect()
```

### 回答が遅い

```python
# キャッシュを有効化
rag = RAGSkill(
    config=RAGConfig(enable_cache=True, cache_ttl=3600),
)
```

