# -*- coding: utf-8 -*-
"""リアルタイム状態同期.

フロントエンドとバックエンド間の状態をリアルタイムで同期。
WebSocket/SSE両方に対応。

設計原則:
- 双方向同期: サーバー→クライアント、クライアント→サーバー
- 差分更新: 変更部分のみ送信
- 再接続: 接続断絶時の自動リカバリ

使用例:
    >>> from agentflow.integrations.realtime_sync import RealtimeStateSync
    >>>
    >>> sync = RealtimeStateSync(state_store=my_store)
    >>>
    >>> # クライアントを接続
    >>> await sync.connect_client("client-123", websocket)
    >>>
    >>> # 状態更新を送信
    >>> await sync.push_update("client-123", {"progress": 0.5})
"""

from __future__ import annotations

import asyncio
import json
import logging
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Protocol

from agentflow.state.store import GlobalStateStore


class SyncEventType(str, Enum):
    """同期イベント種別."""

    STATE_UPDATE = "state_update"      # 状態更新
    STATE_DIFF = "state_diff"          # 差分更新
    STATE_SNAPSHOT = "state_snapshot"  # スナップショット
    CLIENT_ACTION = "client_action"    # クライアントアクション
    HEARTBEAT = "heartbeat"            # ハートビート
    CONNECTED = "connected"            # 接続完了
    DISCONNECTED = "disconnected"      # 切断
    ERROR = "error"                    # エラー


@dataclass
class SyncEvent:
    """同期イベント.

    Attributes:
        event_type: イベント種別
        data: データ
        client_id: クライアントID
        timestamp: タイムスタンプ
    """

    event_type: SyncEventType
    data: dict[str, Any] = field(default_factory=dict)
    client_id: str | None = None
    timestamp: datetime = field(default_factory=datetime.now)

    def to_json(self) -> str:
        """JSONに変換."""
        return json.dumps({
            "event_type": self.event_type.value,
            "data": self.data,
            "client_id": self.client_id,
            "timestamp": self.timestamp.isoformat(),
        }, ensure_ascii=False)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "event_type": self.event_type.value,
            "data": self.data,
            "client_id": self.client_id,
            "timestamp": self.timestamp.isoformat(),
        }


class WebSocketProtocol(Protocol):
    """WebSocketプロトコル."""

    async def send(self, data: str) -> None:
        """データを送信."""
        ...

    async def receive(self) -> str:
        """データを受信."""
        ...


@dataclass
class ClientConnection:
    """クライアント接続.

    Attributes:
        client_id: クライアントID
        websocket: WebSocket接続
        connected_at: 接続時刻
        last_activity: 最終アクティビティ
        subscribed_paths: 購読中のパス
        metadata: メタデータ
    """

    client_id: str
    websocket: Any = None
    connected_at: datetime = field(default_factory=datetime.now)
    last_activity: datetime = field(default_factory=datetime.now)
    subscribed_paths: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


class RealtimeStateSync:
    """リアルタイム状態同期マネージャー.

    フロントエンドとバックエンド間の状態をリアルタイムで同期。

    主な機能:
    - WebSocket接続管理
    - 状態変更の自動プッシュ
    - クライアントアクションの受信
    - 再接続時のスナップショット送信

    Example:
        >>> sync = RealtimeStateSync(state_store=store)
        >>>
        >>> # WebSocket接続
        >>> await sync.connect_client("client-123", websocket)
        >>>
        >>> # 状態更新を送信
        >>> await sync.broadcast_update({"progress": 0.5})
    """

    def __init__(
        self,
        state_store: GlobalStateStore | None = None,
        heartbeat_interval: float = 30.0,
        max_clients: int = 100,
    ) -> None:
        """初期化.

        Args:
            state_store: グローバル状態ストア
            heartbeat_interval: ハートビート間隔（秒）
            max_clients: 最大クライアント数
        """
        self._store = state_store or GlobalStateStore()
        self._heartbeat_interval = heartbeat_interval
        self._max_clients = max_clients
        self._clients: dict[str, ClientConnection] = {}
        self._action_handlers: dict[str, Callable[..., Any]] = {}
        self._heartbeat_task: asyncio.Task[None] | None = None
        self._logger = logging.getLogger(__name__)

        # 状態変更を購読
        self._unsubscribe: Callable[[], None] | None = None

    async def start(self) -> None:
        """同期サービスを開始."""
        # 状態変更を購読
        self._unsubscribe = self._store.subscribe(self._on_state_change)

        # ハートビートを開始
        self._heartbeat_task = asyncio.create_task(self._heartbeat_loop())

        self._logger.info("リアルタイム同期サービスを開始しました")

    async def stop(self) -> None:
        """同期サービスを停止."""
        # ハートビートを停止
        if self._heartbeat_task:
            self._heartbeat_task.cancel()
            try:
                await self._heartbeat_task
            except asyncio.CancelledError:
                pass

        # 購読解除
        if self._unsubscribe:
            self._unsubscribe()

        # 全クライアントを切断
        for client_id in list(self._clients.keys()):
            await self.disconnect_client(client_id)

        self._logger.info("リアルタイム同期サービスを停止しました")

    async def connect_client(
        self,
        client_id: str,
        websocket: Any,
        metadata: dict[str, Any] | None = None,
    ) -> bool:
        """クライアントを接続.

        Args:
            client_id: クライアントID
            websocket: WebSocket接続
            metadata: メタデータ

        Returns:
            成功したかどうか
        """
        if len(self._clients) >= self._max_clients:
            self._logger.warning(f"最大クライアント数に達しました: {self._max_clients}")
            return False

        connection = ClientConnection(
            client_id=client_id,
            websocket=websocket,
            metadata=metadata or {},
        )
        self._clients[client_id] = connection

        self._logger.info(f"クライアント接続: {client_id}")

        # 接続完了イベントを送信
        await self._send_to_client(
            client_id,
            SyncEvent(
                event_type=SyncEventType.CONNECTED,
                data={"client_id": client_id},
                client_id=client_id,
            ),
        )

        # 現在の状態スナップショットを送信
        await self._send_snapshot(client_id)

        return True

    async def disconnect_client(self, client_id: str) -> None:
        """クライアントを切断.

        Args:
            client_id: クライアントID
        """
        if client_id not in self._clients:
            return

        del self._clients[client_id]
        self._logger.info(f"クライアント切断: {client_id}")

    async def push_update(
        self,
        client_id: str,
        data: dict[str, Any],
    ) -> bool:
        """特定クライアントに状態更新を送信.

        Args:
            client_id: クライアントID
            data: 更新データ

        Returns:
            成功したかどうか
        """
        return await self._send_to_client(
            client_id,
            SyncEvent(
                event_type=SyncEventType.STATE_UPDATE,
                data=data,
                client_id=client_id,
            ),
        )

    async def broadcast_update(
        self,
        data: dict[str, Any],
        exclude: list[str] | None = None,
    ) -> int:
        """全クライアントに状態更新をブロードキャスト.

        Args:
            data: 更新データ
            exclude: 除外するクライアントID

        Returns:
            送信成功したクライアント数
        """
        exclude = exclude or []
        success_count = 0

        event = SyncEvent(
            event_type=SyncEventType.STATE_UPDATE,
            data=data,
        )

        for client_id in self._clients:
            if client_id not in exclude:
                if await self._send_to_client(client_id, event):
                    success_count += 1

        return success_count

    async def broadcast_diff(
        self,
        diff: dict[str, Any],
        exclude: list[str] | None = None,
    ) -> int:
        """差分更新をブロードキャスト.

        Args:
            diff: 差分データ
            exclude: 除外するクライアントID

        Returns:
            送信成功したクライアント数
        """
        exclude = exclude or []
        success_count = 0

        event = SyncEvent(
            event_type=SyncEventType.STATE_DIFF,
            data=diff,
        )

        for client_id in self._clients:
            if client_id not in exclude:
                if await self._send_to_client(client_id, event):
                    success_count += 1

        return success_count

    async def _send_to_client(
        self,
        client_id: str,
        event: SyncEvent,
    ) -> bool:
        """クライアントにイベントを送信.

        Args:
            client_id: クライアントID
            event: イベント

        Returns:
            成功したかどうか
        """
        connection = self._clients.get(client_id)
        if not connection or not connection.websocket:
            return False

        try:
            await connection.websocket.send(event.to_json())
            connection.last_activity = datetime.now()
            return True
        except Exception as e:
            self._logger.warning(f"クライアント送信エラー: {client_id} - {e}")
            # 切断されたクライアントを削除
            await self.disconnect_client(client_id)
            return False

    async def _send_snapshot(self, client_id: str) -> bool:
        """クライアントに状態スナップショットを送信.

        Args:
            client_id: クライアントID

        Returns:
            成功したかどうか
        """
        state = self._store.get_state()
        return await self._send_to_client(
            client_id,
            SyncEvent(
                event_type=SyncEventType.STATE_SNAPSHOT,
                data={"state": state},
                client_id=client_id,
            ),
        )

    def _on_state_change(self, state: dict[str, Any]) -> None:
        """状態変更ハンドラー.

        Args:
            state: 更新された状態
        """
        # 非同期でブロードキャスト
        asyncio.create_task(self.broadcast_update(state))

    async def _heartbeat_loop(self) -> None:
        """ハートビートループ."""
        while True:
            try:
                await asyncio.sleep(self._heartbeat_interval)

                event = SyncEvent(
                    event_type=SyncEventType.HEARTBEAT,
                    data={"timestamp": datetime.now().isoformat()},
                )

                for client_id in list(self._clients.keys()):
                    await self._send_to_client(client_id, event)

            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.error(f"ハートビートエラー: {e}")

    def register_action_handler(
        self,
        action_type: str,
        handler: Callable[..., Any],
    ) -> None:
        """クライアントアクションハンドラーを登録.

        Args:
            action_type: アクション種別
            handler: ハンドラー関数
        """
        self._action_handlers[action_type] = handler

    async def handle_client_message(
        self,
        client_id: str,
        message: str,
    ) -> dict[str, Any]:
        """クライアントメッセージを処理.

        Args:
            client_id: クライアントID
            message: メッセージ（JSON）

        Returns:
            レスポンス
        """
        try:
            data = json.loads(message)
            action_type = data.get("action")
            payload = data.get("payload", {})

            if not action_type:
                return {"error": "アクションタイプが指定されていません"}

            # ハンドラーを実行
            handler = self._action_handlers.get(action_type)
            if handler:
                result = await handler(client_id, payload)
                return {"success": True, "result": result}
            else:
                return {"error": f"不明なアクション: {action_type}"}

        except json.JSONDecodeError:
            return {"error": "無効なJSONフォーマット"}
        except Exception as e:
            self._logger.error(f"クライアントメッセージ処理エラー: {e}")
            return {"error": str(e)}

    def subscribe_path(
        self,
        client_id: str,
        path: str,
    ) -> bool:
        """クライアントの購読パスを追加.

        Args:
            client_id: クライアントID
            path: 購読パス

        Returns:
            成功したかどうか
        """
        connection = self._clients.get(client_id)
        if not connection:
            return False

        if path not in connection.subscribed_paths:
            connection.subscribed_paths.append(path)

        return True

    def unsubscribe_path(
        self,
        client_id: str,
        path: str,
    ) -> bool:
        """クライアントの購読パスを削除.

        Args:
            client_id: クライアントID
            path: 購読パス

        Returns:
            成功したかどうか
        """
        connection = self._clients.get(client_id)
        if not connection:
            return False

        if path in connection.subscribed_paths:
            connection.subscribed_paths.remove(path)

        return True

    def get_connected_clients(self) -> list[str]:
        """接続中のクライアント一覧を取得.

        Returns:
            クライアントIDリスト
        """
        return list(self._clients.keys())

    def get_client_info(self, client_id: str) -> dict[str, Any] | None:
        """クライアント情報を取得.

        Args:
            client_id: クライアントID

        Returns:
            クライアント情報
        """
        connection = self._clients.get(client_id)
        if not connection:
            return None

        return {
            "client_id": connection.client_id,
            "connected_at": connection.connected_at.isoformat(),
            "last_activity": connection.last_activity.isoformat(),
            "subscribed_paths": connection.subscribed_paths,
            "metadata": connection.metadata,
        }

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        return {
            "connected_clients": len(self._clients),
            "max_clients": self._max_clients,
            "heartbeat_interval": self._heartbeat_interval,
            "action_handlers": list(self._action_handlers.keys()),
        }

    async def __aenter__(self) -> "RealtimeStateSync":
        """非同期コンテキストマネージャー."""
        await self.start()
        return self

    async def __aexit__(self, *args: Any) -> None:
        """非同期コンテキストマネージャー終了."""
        await self.stop()


# エクスポート
__all__ = [
    "SyncEventType",
    "SyncEvent",
    "ClientConnection",
    "RealtimeStateSync",
]
