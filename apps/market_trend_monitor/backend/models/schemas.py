"""データモデル定義.

Market Trend Monitor のデータスキーマを定義します。
"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


class SourceType(str, Enum):
    """データソースタイプ."""

    NEWS = "news"
    GITHUB = "github"
    ARXIV = "arxiv"
    RSS = "rss"


class SentimentType(str, Enum):
    """センチメントタイプ."""

    POSITIVE = "positive"
    NEUTRAL = "neutral"
    NEGATIVE = "negative"


class NotificationPriority(str, Enum):
    """通知優先度."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    CRITICAL = "critical"


@dataclass
class Article:
    """記事データモデル.

    外部ソースから収集した記事情報を表現します。
    """

    id: str
    title: str
    url: str
    source: SourceType
    published_at: datetime
    content: str
    keywords: list[str] = field(default_factory=list)
    collected_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "title": self.title,
            "url": self.url,
            "source": self.source.value,
            "published_at": self.published_at.isoformat(),
            "content": self.content,
            "keywords": self.keywords,
            "collected_at": self.collected_at.isoformat(),
            "metadata": self.metadata,
        }


@dataclass
class Trend:
    """トレンドデータモデル.

    分析結果から抽出されたトレンド情報を表現します。
    """

    id: str
    topic: str
    score: float
    articles_count: int
    keywords: list[str]
    sentiment: SentimentType
    growth_rate: float
    created_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "topic": self.topic,
            "score": self.score,
            "articles_count": self.articles_count,
            "keywords": self.keywords,
            "sentiment": self.sentiment.value,
            "growth_rate": self.growth_rate,
            "created_at": self.created_at.isoformat(),
            "metadata": self.metadata,
        }


@dataclass
class ReportSection:
    """レポートセクション."""

    title: str
    content: str
    charts: list[dict[str, Any]] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "title": self.title,
            "content": self.content,
            "charts": self.charts,
            "metadata": self.metadata,
        }


@dataclass
class Report:
    """レポートデータモデル.

    生成されたレポート情報を表現します。
    """

    id: str
    title: str
    sections: list[ReportSection]
    generated_at: datetime = field(default_factory=datetime.now)
    period: str = ""
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "title": self.title,
            "sections": [s.to_dict() for s in self.sections],
            "generated_at": self.generated_at.isoformat(),
            "period": self.period,
            "metadata": self.metadata,
        }


@dataclass
class Notification:
    """通知データモデル."""

    id: str
    type: str
    priority: NotificationPriority
    message: str
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "type": self.type,
            "priority": self.priority.value,
            "message": self.message,
            "timestamp": self.timestamp.isoformat(),
            "metadata": self.metadata,
        }

