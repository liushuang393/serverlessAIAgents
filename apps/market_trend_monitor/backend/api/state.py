"""API 共有状態.

各ルーターで使用するサービスインスタンスを集約します。
"""

from apps.market_trend_monitor.backend.services import MarketStore


store = MarketStore()
