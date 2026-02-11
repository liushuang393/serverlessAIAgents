"""設定管理.

アプリケーション全体の設定を管理します。
"""

import json
import os
from dataclasses import dataclass, field
from pathlib import Path


def _parse_port(value: str | int | None, default: int) -> int:
    """ポート値を安全に整数へ変換する."""
    if isinstance(value, int):
        return value if value > 0 else default

    if isinstance(value, str):
        stripped = value.strip()
        if stripped:
            try:
                parsed = int(stripped)
            except ValueError:
                return default
            return parsed if parsed > 0 else default

    return default


def _load_app_config() -> dict[str, object]:
    """アプリ共通設定ファイルを読み込む."""
    config_path = Path(__file__).resolve().parents[1] / "app_config.json"

    try:
        raw = config_path.read_text(encoding="utf-8")
    except FileNotFoundError:
        return {}

    try:
        parsed = json.loads(raw)
    except json.JSONDecodeError:
        return {}

    return parsed if isinstance(parsed, dict) else {}


APP_CONFIG = _load_app_config()
DEFAULT_API_HOST = str(APP_CONFIG.get("api_host", "localhost"))
DEFAULT_API_PORT = _parse_port(APP_CONFIG.get("api_port"), 8002)
DEFAULT_FRONTEND_PORT = _parse_port(APP_CONFIG.get("frontend_port"), 3002)


def _default_cors_origins() -> list[str]:
    """CORSのデフォルト許可オリジンを構築する."""
    frontend_port = _parse_port(
        os.getenv("MARKET_TREND_MONITOR_FRONTEND_PORT"), DEFAULT_FRONTEND_PORT
    )
    return [f"http://localhost:{frontend_port}", "http://localhost:3000"]


@dataclass
class CollectorConfig:
    """データ収集設定."""

    # 監視キーワード
    keywords: list[str] = field(
        default_factory=lambda: [
            "COBOL Refactoring",
            "Java migration automation",
            "legacy modernization AI",
            "Mainframe Modernization",
            "DX 推進",
            "システム刷新 AI",
            "LLM code analysis legacy",
            "COBOL to Java",
        ]
    )

    # データソース
    sources: list[str] = field(default_factory=lambda: ["news", "github", "arxiv", "rss"])

    # 収集間隔（秒）
    interval_seconds: int = 3600  # 1時間

    # 最大記事数
    max_articles_per_source: int = 100

    # RSS フィード
    rss_feeds: list[str] = field(
        default_factory=lambda: [
            "https://hnrss.org/newest",
            "https://www.reddit.com/r/MachineLearning/.rss",
            "https://itmedia.jp/rss/2.0/itall/index.xml",
            "https://tech.nikkeibp.co.jp/rss/index.rdf",
            "https://qiita.com/tags/cobol/feed",
        ]
    )


@dataclass
class AnalyzerConfig:
    """分析設定.

    Note:
        LLM設定は環境変数から自動検出されます（松耦合原則）。
        get_llm() を使用することで、OpenAI/Anthropic/Google等を
        自動的に切り替えることができます。
    """

    # トレンドスコア閾値
    trend_score_threshold: float = 0.5

    # センチメント分析有効化
    enable_sentiment_analysis: bool = True

    # トレンド算定ウィンドウ（日数）
    trend_window_days: int = 7


@dataclass
class ReporterConfig:
    """レポート生成設定."""

    # レポート生成間隔
    daily_report_time: str = "09:00"  # 毎日9時
    weekly_report_day: str = "monday"  # 毎週月曜日

    # レポート形式
    output_formats: list[str] = field(default_factory=lambda: ["markdown", "html"])


@dataclass
class NotifierConfig:
    """通知設定."""

    # 通知チャネル
    channels: list[str] = field(default_factory=lambda: ["websocket"])

    # アラート閾値
    alert_growth_rate_threshold: float = 0.3  # 30%以上の成長率でアラート

    # 通知頻度制限（秒）
    rate_limit_seconds: int = 300  # 5分


@dataclass
class DatabaseConfig:
    """データベース設定."""

    # データベースURL
    url: str = field(default_factory=lambda: os.getenv("DATABASE_URL", "sqlite:///./market_trend.db"))

    # 接続プール設定
    pool_size: int = 5
    max_overflow: int = 10


@dataclass
class APIConfig:
    """API設定."""

    # サーバー設定
    host: str = field(
        default_factory=lambda: os.getenv("MARKET_TREND_MONITOR_API_HOST", DEFAULT_API_HOST)
    )
    port: int = field(
        default_factory=lambda: _parse_port(
            os.getenv("MARKET_TREND_MONITOR_API_PORT"),
            DEFAULT_API_PORT,
        )
    )

    # CORS設定
    cors_origins: list[str] = field(default_factory=_default_cors_origins)

    # WebSocket設定
    websocket_ping_interval: int = 30


@dataclass
class VectorDBConfig:
    """ベクトルDB設定."""

    collection_name: str = "market_trend_articles"
    embedding_dimension: int = 1536
    similarity_threshold: float = 0.92
    mmr_lambda: float = 0.7


@dataclass
class EmbeddingConfig:
    """Embedding設定."""

    model_name: str = field(
        default_factory=lambda: os.getenv("EMBEDDING_MODEL", "text-embedding-3-small")
    )
    batch_size: int = 32


@dataclass
class AppConfig:
    """アプリケーション全体設定."""

    # 各モジュール設定
    collector: CollectorConfig = field(default_factory=CollectorConfig)
    analyzer: AnalyzerConfig = field(default_factory=AnalyzerConfig)
    reporter: ReporterConfig = field(default_factory=ReporterConfig)
    notifier: NotifierConfig = field(default_factory=NotifierConfig)
    database: DatabaseConfig = field(default_factory=DatabaseConfig)
    api: APIConfig = field(default_factory=APIConfig)
    vectordb: VectorDBConfig = field(default_factory=VectorDBConfig)
    embedding: EmbeddingConfig = field(default_factory=EmbeddingConfig)

    # ログ設定
    log_level: str = field(default_factory=lambda: os.getenv("LOG_LEVEL", "INFO"))

    # データディレクトリ
    data_dir: Path = field(default_factory=lambda: Path("./data"))

    def ensure_data_dir(self) -> Path:
        """データディレクトリを確保して返す.

        Returns:
            データディレクトリのPath
        """
        self.data_dir.mkdir(parents=True, exist_ok=True)
        return self.data_dir


# グローバル設定インスタンス
config = AppConfig()
