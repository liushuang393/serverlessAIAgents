"""
AI Blocksの設定管理

このモジュールは、環境変数、設定ファイル、デフォルト値を統合的に管理します。
Pydanticを使用して型安全な設定を提供します。
"""

from pathlib import Path
from typing import List, Optional

# Pydanticのインポート処理
try:
    # Pydantic v2の場合
    from pydantic import ConfigDict, Field
    from pydantic_settings import BaseSettings

    HAS_CONFIG_DICT = True
except ImportError:
    try:
        # Pydantic v1の場合
        from typing import Any

        from pydantic import BaseSettings, Field  # type: ignore

        # Pydantic v1用のダミー型
        ConfigDict = Any  # type: ignore
        HAS_CONFIG_DICT = False
    except ImportError as e:
        raise ImportError(
            "pydanticまたはpydantic-settingsがインストールされていません。"
            "pip install pydantic-settings または pip install pydantic を実行してください。"
        ) from e


class Settings(BaseSettings):
    """アプリケーション設定クラス"""

    # 基本設定
    app_name: str = Field(default="AI Blocks", description="アプリケーション名")
    app_version: str = Field(default="0.1.0", description="アプリケーションバージョン")
    debug: bool = Field(default=False, description="デバッグモード")

    # ログ設定
    log_level: str = Field(default="INFO", description="ログレベル")
    log_format: str = Field(
        default=(
            "{time:YYYY-MM-DD HH:mm:ss} | {level} | "
            "{name}:{function}:{line} - {message}"
        ),
        description="ログフォーマット",
    )
    log_file: Optional[str] = Field(default=None, description="ログファイルパス")

    # LLM設定
    openai_api_key: Optional[str] = Field(default=None, description="OpenAI APIキー")
    anthropic_api_key: Optional[str] = Field(
        default=None, description="Anthropic APIキー"
    )
    base_url: Optional[str] = Field(default=None, description="LLM APIのベースURL（Ollama等）")
    ai_blocks_base_url: Optional[str] = Field(
        default=None, description="AI Blocks用のベースURL"
    )
    default_llm_provider: str = Field(default="openai", description="デフォルトLLMプロバイダー")
    default_model: str = Field(default="gpt-3.5-turbo", description="デフォルトモデル")
    max_tokens: int = Field(default=1000, description="最大トークン数")
    temperature: float = Field(default=0.7, description="生成温度")

    # ベクトルデータベース設定
    vector_store_type: str = Field(default="chroma", description="ベクトルストアタイプ")
    chroma_persist_directory: str = Field(
        default="./chroma_db", description="ChromaDB永続化ディレクトリ"
    )
    pinecone_api_key: Optional[str] = Field(default=None, description="Pinecone APIキー")
    pinecone_environment: Optional[str] = Field(default=None, description="Pinecone環境")

    # メモリ設定
    memory_max_items: int = Field(default=1000, description="メモリの最大アイテム数")
    memory_similarity_threshold: float = Field(default=0.7, description="類似度閾値")

    # チャンク設定
    chunk_size: int = Field(default=1000, description="デフォルトチャンクサイズ")
    chunk_overlap: int = Field(default=200, description="チャンクオーバーラップ")

    # ツール設定
    tool_timeout: float = Field(default=30.0, description="ツール実行タイムアウト（秒）")
    max_tool_calls: int = Field(default=10, description="最大ツール呼び出し回数")

    # セキュリティ設定
    enable_input_sanitization: bool = Field(
        default=True, description="入力サニタイゼーションを有効にする"
    )
    enable_output_filtering: bool = Field(default=True, description="出力フィルタリングを有効にする")
    allowed_domains: List[str] = Field(default_factory=list, description="許可されたドメインリスト")

    # パフォーマンス設定
    enable_caching: bool = Field(default=True, description="キャッシュを有効にする")
    cache_ttl: int = Field(default=3600, description="キャッシュTTL（秒）")
    max_concurrent_requests: int = Field(default=10, description="最大同時リクエスト数")

    # メトリクス設定
    enable_metrics: bool = Field(default=True, description="メトリクス収集を有効にする")
    metrics_export_interval: int = Field(default=60, description="メトリクス出力間隔（秒）")

    # トレーシング設定
    enable_tracing: bool = Field(default=False, description="トレーシングを有効にする")
    trace_sample_rate: float = Field(default=0.1, description="トレースサンプリング率")

    # Pydantic v2の設定
    if HAS_CONFIG_DICT:
        model_config = {
            "env_file": ".env",
            "env_file_encoding": "utf-8",
            "case_sensitive": False,
            "env_prefix": "AI_BLOCKS_",
            "extra": "ignore",  # 未定義フィールドを無視
        }
    else:
        # Pydantic v1の場合の後方互換性
        class Config:
            env_file = ".env"
            env_file_encoding = "utf-8"
            case_sensitive = False
            env_prefix = "AI_BLOCKS_"
            extra = "ignore"


# グローバル設定インスタンス
_settings: Optional[Settings] = None


def get_settings() -> Settings:
    """設定インスタンスを取得する（シングルトンパターン）"""
    global _settings
    if _settings is None:
        _settings = Settings()
    return _settings


def reload_settings() -> Settings:
    """設定を再読み込みする"""
    global _settings
    _settings = Settings()
    return _settings


def get_config_path() -> Path:
    """設定ファイルのパスを取得する"""
    return Path(__file__).parent


def create_default_env_file() -> None:
    """デフォルトの.envファイルを作成する"""
    env_content = """# AI Blocks 設定ファイル

# 基本設定
AI_BLOCKS_DEBUG=false
AI_BLOCKS_LOG_LEVEL=INFO

# LLM設定
# AI_BLOCKS_OPENAI_API_KEY=your_openai_api_key_here
# AI_BLOCKS_ANTHROPIC_API_KEY=your_anthropic_api_key_here
AI_BLOCKS_DEFAULT_LLM_PROVIDER=openai
AI_BLOCKS_DEFAULT_MODEL=gpt-3.5-turbo
AI_BLOCKS_MAX_TOKENS=1000
AI_BLOCKS_TEMPERATURE=0.7

# ベクトルデータベース設定
AI_BLOCKS_VECTOR_STORE_TYPE=chroma
AI_BLOCKS_CHROMA_PERSIST_DIRECTORY=./chroma_db

# メモリ設定
AI_BLOCKS_MEMORY_MAX_ITEMS=1000
AI_BLOCKS_MEMORY_SIMILARITY_THRESHOLD=0.7

# チャンク設定
AI_BLOCKS_CHUNK_SIZE=1000
AI_BLOCKS_CHUNK_OVERLAP=200

# ツール設定
AI_BLOCKS_TOOL_TIMEOUT=30.0
AI_BLOCKS_MAX_TOOL_CALLS=10

# セキュリティ設定
AI_BLOCKS_ENABLE_INPUT_SANITIZATION=true
AI_BLOCKS_ENABLE_OUTPUT_FILTERING=true

# パフォーマンス設定
AI_BLOCKS_ENABLE_CACHING=true
AI_BLOCKS_CACHE_TTL=3600
AI_BLOCKS_MAX_CONCURRENT_REQUESTS=10

# メトリクス設定
AI_BLOCKS_ENABLE_METRICS=true
AI_BLOCKS_METRICS_EXPORT_INTERVAL=60

# トレーシング設定
AI_BLOCKS_ENABLE_TRACING=false
AI_BLOCKS_TRACE_SAMPLE_RATE=0.1
"""

    env_path = Path(".env")
    if not env_path.exists():
        with open(env_path, "w", encoding="utf-8") as f:
            f.write(env_content)
        print(f"デフォルトの.envファイルを作成しました: {env_path.absolute()}")
    else:
        print(f".envファイルは既に存在します: {env_path.absolute()}")


if __name__ == "__main__":
    # 設定ファイルの作成
    create_default_env_file()

    # 設定の表示
    settings = get_settings()
    print("現在の設定:")
    print(settings.json(indent=2))
