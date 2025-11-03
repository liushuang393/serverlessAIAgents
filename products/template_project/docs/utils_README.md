# ユーティリティライブラリ概要

このディレクトリには、AI・機械学習プロジェクトで使用する各種ユーティリティモジュールが含まれています。

## 📁 ファイル構成

### 🤖 AI・LLM関連
- **`LLMProvider.py`** - 複数LLMプロバイダー（OpenAI、Anthropic、Google、Ollama等）への統一インターフェース
- **`embedding_utils.py`** - 埋め込みAPI（OpenAI、Azure、Google、Cohere、HuggingFace、Jina等）の統合管理
- **`vector_utils.py`** - ベクトルデータベース（FAISS、Pinecone、Qdrant、Chroma等）の統一操作

### 🔍 検索・データ処理関連
- **`websearch_utils.py`** - Web検索API（Google、Bing、DuckDuckGo、Brave、SerpApi）の統合
- **`text_utils.py`** - テキストチャンキング（固定サイズ、文ベース、段落ベース、セマンティック）
- **`audio_utils.py`** - Text-to-Speech（Amazon Polly、Google TTS、Azure TTS、ElevenLabs等）

### 🛠️ システム・デバッグ関連
- **`viz_utils.py`** - Mermaid図生成、コールスタックデバッグ、パフォーマンス監視

## 🚀 主な機能

### 1. LLM統合管理 (`LLMProvider.py`)
複数のLLMプロバイダー（OpenAI、Anthropic、Google、HuggingFace）を統一インターフェースで管理。

**主要機能:**
- 統一されたAPI呼び出しインターフェース
- メモリ内キャッシュ機能（@lru_cache使用）
- チャット履歴処理機能
- 詳細ログ機能
- 再試行対応
- 自動フォールバック機能

```python
# OpenAI API使用（キャッシュ有効）
response = generate("こんにちは、世界について教えて", use_cache=True)

# Anthropic API使用
response = generate_anthropic("量子コンピューティングについて説明して")

# Google API使用
response = generate_google("AIの未来について教えて")

# HuggingFace API使用
response = generate_huggingface("機械学習とは何ですか？")

# チャット履歴処理
messages = [
    {"role": "user", "content": "こんにちは"},
    {"role": "assistant", "content": "こんにちは！何かお手伝いできることはありますか？"},
    {"role": "user", "content": "天気について教えて"}
]
response = call_llm_with_messages(messages, use_cache=True)

# 再試行機能付き呼び出し
response = generate("質問内容", cur_retry=1, use_cache=False)
```

### 2. 埋め込み管理 (`embedding_utils.py`)
```python
from embedding_utils import embed, setup_embedding_providers

# テキスト埋め込み
embedding = embed("これはテストテキストです")
embeddings = embed(["テキスト1", "テキスト2", "テキスト3"])

# プロバイダー設定
config = {
    "openai": {"type": "openai", "api_key": "your-key", "default": True},
    "cohere": {"type": "cohere", "api_key": "your-key"}
}
setup_embedding_providers(config)
```

### 3. ベクトルデータベース (`vector_utils.py`)
```python
from vector_utils import create_collection, upsert_documents, search_vectors, VectorDocument

# コレクション作成
create_collection("my_collection", dimension=1536, provider="faiss")

# 文書追加
documents = [
    VectorDocument("doc1", [0.1, 0.2, 0.3], {"title": "文書1"}, "これは文書1です"),
    VectorDocument("doc2", [0.4, 0.5, 0.6], {"title": "文書2"}, "これは文書2です")
]
upsert_documents("my_collection", documents)

# 検索
query_vector = [0.1, 0.2, 0.3]
results = search_vectors("my_collection", query_vector, top_k=5)
```

### 4. Web検索 (`websearch_utils.py`)
```python
from websearch_utils import search_web, setup_search_providers

# 検索実行
results = search_web("Python プログラミング", num_results=5, provider="duckduckgo")

for result in results:
    print(f"タイトル: {result.title}")
    print(f"URL: {result.url}")
    print(f"概要: {result.snippet}")
```

### 5. テキストチャンキング (`text_utils.py`)
```python
from text_utils import chunk_text, preprocess_text

# テキスト前処理
clean_text = preprocess_text("<p>これは   テストです</p>")

# 固定サイズチャンキング
chunks = chunk_text(text, chunker_type="fixed", chunk_size=1000, overlap=200)

# 文ベースチャンキング
chunks = chunk_text(text, chunker_type="sentence", max_sentences=5)

# セマンティックチャンキング（埋め込み関数が必要）
def embedding_func(text):
    return embed(text)  # embedding_utils使用

chunks = chunk_text(
    text, 
    chunker_type="semantic", 
    embedding_function=embedding_func,
    similarity_threshold=0.7
)
```

### 6. Text-to-Speech (`audio_utils.py`)
```python
from audio_utils import text_to_speech, setup_tts_providers, synthesize_long_text

# 音声生成
text_to_speech("こんにちは、世界", "output.mp3", provider="amazon_polly")

# 長いテキストの分割音声生成
long_text = "これは非常に長いテキストです..." * 100
audio_files = synthesize_long_text(long_text, "output_dir/", max_length=5000)
```

### 7. 可視化・デバッグ (`viz_utils.py`)
```python
from viz_utils import build_mermaid, profile_execution, debug_call_stack

# Mermaid図生成
mermaid_code = build_mermaid(flow_object, direction="LR")

# パフォーマンス監視
@profile_execution("my_function")
def my_function():
    # 処理内容
    pass

# コールスタックデバッグ
debug_call_stack("BaseNode")
```

## 🔧 設定とカスタマイズ

### 環境変数
各プロバイダーのAPIキーを環境変数で設定できます：

```bash
# LLM
export OPENAI_API_KEY="your-openai-key"
export ANTHROPIC_API_KEY="your-anthropic-key"
export GOOGLE_API_KEY="your-google-key"

# 埋め込み
export COHERE_API_KEY="your-cohere-key"
export HUGGINGFACE_API_KEY="your-hf-key"
export JINA_API_KEY="your-jina-key"

# Web検索
export GOOGLE_SEARCH_API_KEY="your-google-search-key"
export GOOGLE_SEARCH_CX_ID="your-cx-id"
export BING_SEARCH_API_KEY="your-bing-key"

# TTS
export AWS_ACCESS_KEY_ID="your-aws-key"
export AWS_SECRET_ACCESS_KEY="your-aws-secret"
export ELEVENLABS_API_KEY="your-elevenlabs-key"
```

### プロバイダー設定例
```python
# 統合設定例
from llm_utils import setup_providers as setup_llm
from embedding_utils import setup_embedding_providers
from websearch_utils import setup_search_providers
from audio_utils import setup_tts_providers

# LLM設定
llm_config = {
    "openai": {
        "type": "openai",
        "api_key": os.getenv("OPENAI_API_KEY"),
        "model": "gpt-4",
        "default": True
    },
    "anthropic": {
        "type": "anthropic", 
        "api_key": os.getenv("ANTHROPIC_API_KEY"),
        "model": "claude-3-sonnet-20240229"
    }
}
setup_llm(llm_config)

# 埋め込み設定
embedding_config = {
    "openai": {
        "type": "openai",
        "api_key": os.getenv("OPENAI_API_KEY"),
        "default": True
    }
}
setup_embedding_providers(embedding_config)
```

## 🧪 テスト

テストファイルは `__temp_tests__/` ディレクトリに配置されています：

```bash
# すべてのテストを実行
cd __temp_tests__
python run_tests.py

# 特定のテストを実行
python run_tests.py llm_utils
python run_tests.py embedding_utils
python run_tests.py text_utils
```

## 📋 依存関係

### 必須
- `numpy` - 数値計算
- `requests` - HTTP通信

### オプション（機能別）
- **LLM**: `openai`, `anthropic`, `google-generativeai`
- **埋め込み**: `cohere`, `boto3`, `google-cloud-aiplatform`
- **ベクトルDB**: `faiss-cpu`, `pinecone-client`, `chromadb`
- **テキスト処理**: `nltk`
- **音声**: `boto3`, `google-cloud-texttospeech`, `azure-cognitiveservices-speech`
- **その他**: `aiohttp`, `pydub`, `librosa`

## 🔄 互換性

既存の `LLMProvider.py` との互換性を維持しています：
- `generate()` 関数
- `call_llm_with_messages()` 関数  
- `generate_anthropic()` 関数
- キャッシュ機能

## 📝 ライセンス

このプロジェクトのライセンスに従います。

## 🤝 貢献

バグ報告や機能要望は、プロジェクトのIssueトラッカーまでお願いします。
