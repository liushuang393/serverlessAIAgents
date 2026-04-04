"""flight パッケージ — 機票検索・監視・通知の単一責務分割.

元の flight_watch.py (800行) を以下に分割:
- models.py: ドメインモデル + NLP パーサー
- ranking.py: ランキングエンジン
- providers/: 検索プロバイダ (fake, web)
- search_service.py: 検索サービス
- subscription_service.py: サブスクリプション管理 + 監視ループ
- notification_service.py: 通知サービス
"""

from apps.messaging_hub.flight.models import (
    DateWindow,
    FlightOffer,
    FlightSearchRequest,
    FlightSearchResult,
    FlightWatchSubscription,
    NotificationChannel,
    NotificationTarget,
    RankingWeights,
    SubscriptionStatus,
)
from apps.messaging_hub.flight.notification_service import FlightNotificationService
from apps.messaging_hub.flight.search_service import FlightSearchService
from apps.messaging_hub.flight.subscription_service import FlightSubscriptionService


__all__ = [
    "DateWindow",
    "FlightNotificationService",
    "FlightOffer",
    "FlightSearchRequest",
    "FlightSearchResult",
    "FlightSearchService",
    "FlightSubscriptionService",
    "FlightWatchSubscription",
    "NotificationChannel",
    "NotificationTarget",
    "RankingWeights",
    "SubscriptionStatus",
]
