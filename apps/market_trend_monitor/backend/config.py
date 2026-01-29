"""設定管理.

アプリケーション全体の設定を管理します。
"""

import os
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class CollectorConfig:
    """データ収集設定."""

    # 監視キーワード
    keywords: list[str] = field(
        default_factory=lambda: [
            "COBOL",
            "Java migration",
            "legacy modernization",
            "AI",
            "LLM",
            "machine learning",
        ]
    )

    # データソース
    sources: list[str] = field(default_factory=lambda: ["news", "github", "arxiv", "rss"])

    # 収集間隔（秒）
    interval_seconds: int = 3600  # 1時間

    # 最大記事数
    max_articles_per_source: int = 100


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
    host: str = "0.0.0.0"
    port: int = 8000

    # CORS設定
    cors_origins: list[str] = field(default_factory=lambda: ["http://localhost:3000"])

    # WebSocket設定
    websocket_ping_interval: int = 30


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

    # ログ設定
    log_level: str = field(default_factory=lambda: os.getenv("LOG_LEVEL", "INFO"))

    # データディレクトリ
    data_dir: Path = field(default_factory=lambda: Path("./data"))

    def __post_init__(self) -> None:
        """初期化後処理."""
        # データディレクトリを作成
        self.data_dir.mkdir(parents=True, exist_ok=True)


# グローバル設定インスタンス
config = AppConfig()

