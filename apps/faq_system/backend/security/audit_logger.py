"""監査ログサービス（Audit Logger）.

FAQ システムの全操作を監査ログに記録。

機能:
- 全アクセス記録
- データ閲覧記録
- 異常検知
- コンプライアンス対応

使用例:
    >>> from apps.faq_system.backend.security import AuditLogger
    >>>
    >>> logger = AuditLogger()
    >>> await logger.log_access(
    ...     user_id="user123",
    ...     action="search",
    ...     resource="internal_kb",
    ...     query="年休の付与日数",
    ... )
"""

from __future__ import annotations

import hashlib
import json
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class AuditEventType(str, Enum):
    """監査イベントタイプ."""

    # 認証関連
    LOGIN = "login"
    LOGOUT = "logout"
    AUTH_FAILURE = "auth_failure"

    # KB アクセス
    KB_SEARCH = "kb_search"
    KB_VIEW = "kb_view"
    KB_ADD = "kb_add"
    KB_UPDATE = "kb_update"
    KB_DELETE = "kb_delete"

    # データ分析
    SQL_QUERY = "sql_query"
    DATA_EXPORT = "data_export"

    # 管理操作
    CONFIG_CHANGE = "config_change"
    PERMISSION_CHANGE = "permission_change"

    # セキュリティ
    ACCESS_DENIED = "access_denied"
    SENSITIVE_DATA_ACCESS = "sensitive_data_access"
    ANOMALY_DETECTED = "anomaly_detected"


class AuditSeverity(str, Enum):
    """監査深刻度."""

    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


@dataclass
class AuditEvent:
    """監査イベント.

    Attributes:
        event_id: イベントID
        event_type: イベントタイプ
        severity: 深刻度
        timestamp: タイムスタンプ
        user_id: ユーザーID
        user_role: ユーザーロール
        action: アクション
        resource: リソース
        details: 詳細
        ip_address: IPアドレス
        user_agent: User Agent
        session_id: セッションID
        success: 成功フラグ
        error_message: エラーメッセージ
    """

    event_id: str
    event_type: AuditEventType
    severity: AuditSeverity = AuditSeverity.INFO
    timestamp: datetime = field(default_factory=datetime.now)
    user_id: str = ""
    user_role: str = ""
    action: str = ""
    resource: str = ""
    details: dict[str, Any] = field(default_factory=dict)
    ip_address: str = ""
    user_agent: str = ""
    session_id: str = ""
    success: bool = True
    error_message: str = ""

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "event_id": self.event_id,
            "event_type": self.event_type.value,
            "severity": self.severity.value,
            "timestamp": self.timestamp.isoformat(),
            "user_id": self.user_id,
            "user_role": self.user_role,
            "action": self.action,
            "resource": self.resource,
            "details": self.details,
            "ip_address": self.ip_address,
            "session_id": self.session_id,
            "success": self.success,
            "error_message": self.error_message,
        }

    def to_json(self) -> str:
        """JSON に変換."""
        return json.dumps(self.to_dict(), ensure_ascii=False, default=str)


@dataclass
class AuditLoggerConfig:
    """監査ログ設定."""

    # ログ保存
    retention_days: int = 365 * 3  # 3年保存
    compress_after_days: int = 30

    # 記録設定
    log_query_text: bool = True
    log_response_preview: bool = False
    hash_sensitive_data: bool = True

    # アラート設定
    alert_on_access_denied: bool = True
    alert_on_sensitive_access: bool = True
    alert_on_bulk_access: bool = True
    bulk_access_threshold: int = 100

    # 異常検知
    enable_anomaly_detection: bool = True
    max_queries_per_minute: int = 60


class AuditLogger:
    """監査ログサービス.

    全操作を監査ログに記録。

    Example:
        >>> logger = AuditLogger()
        >>>
        >>> # アクセス記録
        >>> await logger.log_access(
        ...     user_id="user123",
        ...     action="search",
        ...     resource="internal_kb",
        ...     query="年休の付与日数",
        ... )
        >>>
        >>> # ログ検索
        >>> events = await logger.search(
        ...     user_id="user123",
        ...     since=datetime.now() - timedelta(days=7),
        ... )
    """

    def __init__(
        self,
        config: AuditLoggerConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or AuditLoggerConfig()
        self._events: list[AuditEvent] = []
        self._counter = 0
        self._query_counts: dict[str, list[datetime]] = {}  # user_id -> timestamps
        self._logger = logging.getLogger(__name__)

    async def log_access(
        self,
        user_id: str,
        action: str,
        resource: str,
        query: str = "",
        result_count: int = 0,
        user_role: str = "",
        ip_address: str = "",
        session_id: str = "",
        success: bool = True,
        error_message: str = "",
        details: dict[str, Any] | None = None,
    ) -> AuditEvent:
        """アクセスを記録.

        Args:
            user_id: ユーザーID
            action: アクション
            resource: リソース
            query: クエリ
            result_count: 結果件数
            user_role: ユーザーロール
            ip_address: IPアドレス
            session_id: セッションID
            success: 成功フラグ
            error_message: エラーメッセージ
            details: 詳細

        Returns:
            監査イベント
        """
        event_type = self._determine_event_type(action, resource)
        severity = self._determine_severity(event_type, success)

        event_details = details or {}

        # クエリを記録
        if query and self._config.log_query_text:
            if self._config.hash_sensitive_data:
                event_details["query_hash"] = self._hash_text(query)
                event_details["query_preview"] = query[:50] + "..." if len(query) > 50 else query
            else:
                event_details["query"] = query

        event_details["result_count"] = result_count

        event = await self._create_event(
            event_type=event_type,
            severity=severity,
            user_id=user_id,
            user_role=user_role,
            action=action,
            resource=resource,
            details=event_details,
            ip_address=ip_address,
            session_id=session_id,
            success=success,
            error_message=error_message,
        )

        # 異常検知
        if self._config.enable_anomaly_detection:
            await self._check_anomaly(user_id, event)

        return event

    async def log_sensitive_access(
        self,
        user_id: str,
        resource: str,
        field_name: str,
        reason: str = "",
        user_role: str = "",
        ip_address: str = "",
    ) -> AuditEvent:
        """機密データアクセスを記録.

        Args:
            user_id: ユーザーID
            resource: リソース
            field_name: フィールド名
            reason: 理由
            user_role: ユーザーロール
            ip_address: IPアドレス

        Returns:
            監査イベント
        """
        event = await self._create_event(
            event_type=AuditEventType.SENSITIVE_DATA_ACCESS,
            severity=AuditSeverity.WARNING,
            user_id=user_id,
            user_role=user_role,
            action="sensitive_access",
            resource=resource,
            details={
                "field_name": field_name,
                "reason": reason,
            },
            ip_address=ip_address,
        )

        if self._config.alert_on_sensitive_access:
            self._logger.warning(
                "SENSITIVE DATA ACCESS: user=%s, field=%s, reason=%s",
                user_id,
                field_name,
                reason,
            )

        return event

    async def log_access_denied(
        self,
        user_id: str,
        resource: str,
        action: str,
        reason: str,
        user_role: str = "",
        ip_address: str = "",
    ) -> AuditEvent:
        """アクセス拒否を記録.

        Args:
            user_id: ユーザーID
            resource: リソース
            action: アクション
            reason: 理由
            user_role: ユーザーロール
            ip_address: IPアドレス

        Returns:
            監査イベント
        """
        event = await self._create_event(
            event_type=AuditEventType.ACCESS_DENIED,
            severity=AuditSeverity.WARNING,
            user_id=user_id,
            user_role=user_role,
            action=action,
            resource=resource,
            details={"reason": reason},
            ip_address=ip_address,
            success=False,
            error_message=reason,
        )

        if self._config.alert_on_access_denied:
            self._logger.warning(
                "ACCESS DENIED: user=%s, resource=%s, action=%s, reason=%s",
                user_id,
                resource,
                action,
                reason,
            )

        return event

    async def log_sql_query(
        self,
        user_id: str,
        sql: str,
        tables_accessed: list[str],
        row_count: int = 0,
        execution_time_ms: float = 0,
        user_role: str = "",
        ip_address: str = "",
    ) -> AuditEvent:
        """SQLクエリを記録.

        Args:
            user_id: ユーザーID
            sql: SQL
            tables_accessed: アクセステーブル
            row_count: 行数
            execution_time_ms: 実行時間
            user_role: ユーザーロール
            ip_address: IPアドレス

        Returns:
            監査イベント
        """
        event = await self._create_event(
            event_type=AuditEventType.SQL_QUERY,
            severity=AuditSeverity.INFO,
            user_id=user_id,
            user_role=user_role,
            action="sql_query",
            resource="database",
            details={
                "sql_hash": self._hash_text(sql),
                "sql_preview": sql[:100] + "..." if len(sql) > 100 else sql,
                "tables_accessed": tables_accessed,
                "row_count": row_count,
                "execution_time_ms": execution_time_ms,
            },
            ip_address=ip_address,
        )

        # 大量アクセスアラート
        if row_count > self._config.bulk_access_threshold:
            if self._config.alert_on_bulk_access:
                self._logger.warning(
                    "BULK DATA ACCESS: user=%s, row_count=%d",
                    user_id,
                    row_count,
                )

        return event

    async def _create_event(
        self,
        event_type: AuditEventType,
        severity: AuditSeverity,
        user_id: str,
        action: str,
        resource: str,
        user_role: str = "",
        details: dict[str, Any] | None = None,
        ip_address: str = "",
        user_agent: str = "",
        session_id: str = "",
        success: bool = True,
        error_message: str = "",
    ) -> AuditEvent:
        """イベントを作成."""
        self._counter += 1
        event_id = f"audit-{datetime.now().strftime('%Y%m%d%H%M%S')}-{self._counter:06d}"

        event = AuditEvent(
            event_id=event_id,
            event_type=event_type,
            severity=severity,
            user_id=user_id,
            user_role=user_role,
            action=action,
            resource=resource,
            details=details or {},
            ip_address=ip_address,
            user_agent=user_agent,
            session_id=session_id,
            success=success,
            error_message=error_message,
        )

        self._events.append(event)

        # ログ出力
        self._logger.info(
            "AUDIT: %s | user=%s | action=%s | resource=%s | success=%s",
            event_type.value,
            user_id,
            action,
            resource,
            success,
        )

        return event

    async def _check_anomaly(self, user_id: str, event: AuditEvent) -> None:
        """異常検知."""
        now = datetime.now()
        one_minute_ago = now - timedelta(minutes=1)

        # クエリカウントを更新
        if user_id not in self._query_counts:
            self._query_counts[user_id] = []

        self._query_counts[user_id].append(now)

        # 古いエントリを削除
        self._query_counts[user_id] = [ts for ts in self._query_counts[user_id] if ts > one_minute_ago]

        # 閾値チェック
        if len(self._query_counts[user_id]) > self._config.max_queries_per_minute:
            await self._create_event(
                event_type=AuditEventType.ANOMALY_DETECTED,
                severity=AuditSeverity.WARNING,
                user_id=user_id,
                action="rate_limit_exceeded",
                resource="system",
                details={
                    "query_count": len(self._query_counts[user_id]),
                    "threshold": self._config.max_queries_per_minute,
                },
            )
            self._logger.warning(
                "ANOMALY: Rate limit exceeded for user=%s, count=%d",
                user_id,
                len(self._query_counts[user_id]),
            )

    def _determine_event_type(self, action: str, resource: str) -> AuditEventType:
        """イベントタイプを決定."""
        action_lower = action.lower()
        resource.lower()

        if action_lower == "search":
            return AuditEventType.KB_SEARCH
        if action_lower == "view":
            return AuditEventType.KB_VIEW
        if action_lower in ("add", "create"):
            return AuditEventType.KB_ADD
        if action_lower in ("update", "edit"):
            return AuditEventType.KB_UPDATE
        if action_lower == "delete":
            return AuditEventType.KB_DELETE
        if action_lower == "sql_query":
            return AuditEventType.SQL_QUERY
        if action_lower == "export":
            return AuditEventType.DATA_EXPORT

        return AuditEventType.KB_SEARCH

    def _determine_severity(self, event_type: AuditEventType, success: bool) -> AuditSeverity:
        """深刻度を決定."""
        if not success:
            return AuditSeverity.WARNING

        if event_type in (
            AuditEventType.ACCESS_DENIED,
            AuditEventType.SENSITIVE_DATA_ACCESS,
            AuditEventType.ANOMALY_DETECTED,
        ):
            return AuditSeverity.WARNING

        return AuditSeverity.INFO

    def _hash_text(self, text: str) -> str:
        """テキストをハッシュ化."""
        return hashlib.sha256(text.encode()).hexdigest()[:16]

    async def search(
        self,
        user_id: str | None = None,
        event_type: AuditEventType | None = None,
        since: datetime | None = None,
        until: datetime | None = None,
        resource: str | None = None,
        success: bool | None = None,
        limit: int = 100,
    ) -> list[AuditEvent]:
        """イベントを検索.

        Args:
            user_id: ユーザーID
            event_type: イベントタイプ
            since: 開始日時
            until: 終了日時
            resource: リソース
            success: 成功フラグ
            limit: 取得件数

        Returns:
            イベントリスト
        """
        results = self._events.copy()

        if user_id:
            results = [e for e in results if e.user_id == user_id]
        if event_type:
            results = [e for e in results if e.event_type == event_type]
        if since:
            results = [e for e in results if e.timestamp >= since]
        if until:
            results = [e for e in results if e.timestamp <= until]
        if resource:
            results = [e for e in results if e.resource == resource]
        if success is not None:
            results = [e for e in results if e.success == success]

        # 日時降順
        results.sort(key=lambda e: e.timestamp, reverse=True)

        return results[:limit]

    def get_stats(
        self,
        since: datetime | None = None,
        until: datetime | None = None,
    ) -> dict[str, Any]:
        """統計を取得."""
        events = self._events

        if since:
            events = [e for e in events if e.timestamp >= since]
        if until:
            events = [e for e in events if e.timestamp <= until]

        return {
            "total_events": len(events),
            "success_count": len([e for e in events if e.success]),
            "failure_count": len([e for e in events if not e.success]),
            "by_type": {t.value: len([e for e in events if e.event_type == t]) for t in AuditEventType},
            "unique_users": len({e.user_id for e in events}),
        }


__all__ = [
    "AuditEvent",
    "AuditEventType",
    "AuditLogger",
    "AuditLoggerConfig",
    "AuditSeverity",
]
