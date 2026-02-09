"""Signal Adapter - Signal Messenger 統合.

signal-cli REST API を使用した Signal 統合アダプター。

Note:
    Signal は公式 API を提供していないため、signal-cli を使用します。
    https://github.com/AsamK/signal-cli
    https://github.com/bbernhard/signal-cli-rest-api

Example:
    >>> from agentflow.channels import SignalAdapter, MessageGateway
    >>> from agentflow import ChatBotSkill, WebSocketHub
    >>>
    >>> # アダプター作成（signal-cli REST API）
    >>> signal = SignalAdapter(
    ...     api_url="http://localhost:8080",
    ...     phone_number="+81901234567",
    ... )
    >>>
    >>> # ゲートウェイに登録
    >>> gateway = MessageGateway(WebSocketHub(), ChatBotSkill())
    >>> gateway.register_channel("signal", signal)
"""

from __future__ import annotations

import asyncio
import contextlib
import logging
from typing import TYPE_CHECKING, Any

import httpx

from agentflow.channels.base import MessageChannelAdapter, UserInfo


if TYPE_CHECKING:
    from agentflow.channels.gateway import MessageGateway


logger = logging.getLogger(__name__)


class SignalAdapter(MessageChannelAdapter):
    """Signal Messenger アダプター.

    signal-cli REST API を使用した統合。

    Features:
    - テキストメッセージ送受信
    - 画像・ファイル送信
    - グループメッセージ
    - 既読確認
    - リアクション
    """

    def __init__(
        self,
        api_url: str,
        phone_number: str,
        *,
        timeout: int = 30,
    ) -> None:
        """Signal アダプターを初期化.

        Args:
            api_url: signal-cli REST API の URL（例: http://localhost:8080）
            phone_number: 登録済み Signal 電話番号（国際形式）
            timeout: リクエストタイムアウト（秒）
        """
        self._api_url = api_url.rstrip("/")
        self._phone_number = phone_number
        self._timeout = timeout
        self._logger = logging.getLogger("signal_adapter")
        self._client = httpx.AsyncClient(timeout=timeout)
        self._polling_task: asyncio.Task[None] | None = None
        self._running = False

    @property
    def platform_name(self) -> str:
        """プラットフォーム名."""
        return "signal"

    async def send_message(
        self,
        channel_id: str,
        text: str,
        **kwargs: Any,
    ) -> str:
        """Signal にメッセージを送信.

        Args:
            channel_id: 受信者の電話番号またはグループ ID
            text: メッセージテキスト
            **kwargs: 追加パラメータ（quote_timestamp, attachments 等）

        Returns:
            タイムスタンプ（メッセージ ID として使用）
        """
        url = f"{self._api_url}/v2/send"

        payload: dict[str, Any] = {
            "message": text,
            "number": self._phone_number,
        }

        # グループか個人かを判定
        if channel_id.startswith("group."):
            payload["recipients"] = [channel_id]
        else:
            payload["recipients"] = [channel_id]

        # 引用返信
        quote_timestamp = kwargs.get("quote_timestamp")
        if quote_timestamp:
            payload["quote_timestamp"] = quote_timestamp
            payload["quote_author"] = kwargs.get("quote_author", channel_id)

        # 添付ファイル
        attachments = kwargs.get("attachments")
        if attachments:
            payload["base64_attachments"] = attachments

        try:
            response = await self._client.post(url, json=payload)
            response.raise_for_status()
            data = response.json()
            # signal-cli はタイムスタンプを返す
            timestamp = str(data.get("timestamp", ""))
            self._logger.info(f"Sent Signal message: {timestamp}")
            return timestamp

        except httpx.HTTPStatusError as e:
            self._logger.exception(f"Signal API error: {e.response.text}")
            raise
        except Exception as e:
            self._logger.error(f"Failed to send Signal message: {e}", exc_info=True)
            raise

    async def send_typing_indicator(self, channel_id: str) -> None:
        """入力インジケーターを送信.

        Args:
            channel_id: 受信者の電話番号
        """
        # signal-cli REST API は typing indicator をサポート
        url = f"{self._api_url}/v1/typing-indicator/{self._phone_number}"

        try:
            await self._client.put(url, json={"recipient": channel_id})
        except Exception as e:
            self._logger.warning(f"Failed to send typing indicator: {e}")

    async def get_user_info(self, user_id: str) -> UserInfo:
        """ユーザー情報を取得.

        Args:
            user_id: ユーザー ID（電話番号）

        Returns:
            ユーザー情報
        """
        # Signal プロファイル情報を取得
        url = f"{self._api_url}/v1/profiles/{self._phone_number}/{user_id}"

        try:
            response = await self._client.get(url)
            if response.status_code == 200:
                data = response.json()
                return UserInfo(
                    user_id=user_id,
                    display_name=data.get("name", f"+{user_id}"),
                    avatar_url=data.get("avatar"),
                    metadata={"platform": "signal", "phone": user_id},
                )
        except Exception as e:
            self._logger.warning(f"Failed to get Signal profile: {e}")

        return UserInfo(
            user_id=user_id,
            display_name=f"+{user_id}",
            metadata={"platform": "signal", "phone": user_id},
        )

    async def send_image(
        self,
        channel_id: str,
        image_url: str,
        caption: str | None = None,
        **kwargs: Any,
    ) -> str:
        """画像を送信.

        Args:
            channel_id: 受信者の電話番号
            image_url: 画像 URL または base64 データ
            caption: キャプション

        Returns:
            タイムスタンプ
        """
        import base64

        # URL から画像をダウンロードして base64 エンコード
        if image_url.startswith("http"):
            try:
                response = await self._client.get(image_url)
                response.raise_for_status()
                image_data = base64.b64encode(response.content).decode()
            except Exception as e:
                self._logger.exception(f"Failed to download image: {e}")
                raise
        else:
            image_data = image_url

        return await self.send_message(
            channel_id,
            caption or "",
            attachments=[image_data],
        )

    async def send_reaction(
        self,
        channel_id: str,
        target_timestamp: str,
        emoji: str,
        remove: bool = False,
    ) -> bool:
        """リアクションを送信.

        Args:
            channel_id: 受信者の電話番号
            target_timestamp: 対象メッセージのタイムスタンプ
            emoji: 絵文字
            remove: リアクションを削除するかどうか

        Returns:
            成功したかどうか
        """
        url = f"{self._api_url}/v1/reactions/{self._phone_number}"

        payload = {
            "recipient": channel_id,
            "reaction": emoji,
            "target_author": channel_id,
            "timestamp": int(target_timestamp),
        }

        if remove:
            payload["remove"] = True

        try:
            response = await self._client.post(url, json=payload)
            response.raise_for_status()
            return True
        except Exception as e:
            self._logger.warning(f"Failed to send reaction: {e}")
            return False

    async def get_groups(self) -> list[dict[str, Any]]:
        """参加グループ一覧を取得.

        Returns:
            グループ情報リスト
        """
        url = f"{self._api_url}/v1/groups/{self._phone_number}"

        try:
            response = await self._client.get(url)
            response.raise_for_status()
            return response.json()
        except Exception as e:
            self._logger.exception(f"Failed to get groups: {e}")
            return []

    async def start_polling(
        self,
        gateway: MessageGateway,
        poll_interval: float = 1.0,
    ) -> None:
        """メッセージポーリングを開始.

        Args:
            gateway: メッセージゲートウェイ
            poll_interval: ポーリング間隔（秒）
        """
        self._running = True
        self._logger.info("Starting Signal message polling...")

        while self._running:
            try:
                await self._poll_messages(gateway)
            except Exception as e:
                self._logger.error(f"Polling error: {e}", exc_info=True)

            await asyncio.sleep(poll_interval)

    async def _poll_messages(self, gateway: MessageGateway) -> None:
        """メッセージをポーリング.

        Args:
            gateway: メッセージゲートウェイ
        """
        url = f"{self._api_url}/v1/receive/{self._phone_number}"

        try:
            response = await self._client.get(url)
            if response.status_code != 200:
                return

            messages = response.json()

            for msg in messages:
                envelope = msg.get("envelope", {})
                data_message = envelope.get("dataMessage")

                if not data_message:
                    continue

                source = envelope.get("source")
                timestamp = str(envelope.get("timestamp", ""))
                text = data_message.get("message", "")
                group_info = data_message.get("groupInfo")

                if not source or not text:
                    continue

                # グループメッセージの場合
                channel_id = source
                if group_info:
                    channel_id = f"group.{group_info.get('groupId', '')}"

                await gateway.route_message_async(
                    platform=self.platform_name,
                    user_id=source,
                    text=text,
                    channel_id=channel_id,
                    metadata={
                        "timestamp": timestamp,
                        "group_id": group_info.get("groupId") if group_info else None,
                        "attachments": data_message.get("attachments", []),
                    },
                )

        except Exception as e:
            self._logger.warning(f"Failed to poll messages: {e}")

    async def stop_polling(self) -> None:
        """ポーリングを停止."""
        self._running = False
        if self._polling_task:
            self._polling_task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._polling_task
            self._polling_task = None
        self._logger.info("Signal polling stopped")

    async def handle_webhook(
        self,
        payload: dict[str, Any],
        gateway: MessageGateway,
    ) -> dict[str, Any]:
        """Webhook リクエストを処理（signal-cli-rest-api の callback モード）.

        Args:
            payload: Webhook ペイロード
            gateway: メッセージゲートウェイ

        Returns:
            処理結果
        """
        try:
            envelope = payload.get("envelope", {})
            data_message = envelope.get("dataMessage")

            if not data_message:
                return {"ok": True, "skipped": True}

            source = envelope.get("source")
            timestamp = str(envelope.get("timestamp", ""))
            text = data_message.get("message", "")
            group_info = data_message.get("groupInfo")

            if not source or not text:
                return {"ok": True, "skipped": True}

            # グループメッセージの場合
            channel_id = source
            if group_info:
                channel_id = f"group.{group_info.get('groupId', '')}"

            await gateway.route_message_async(
                platform=self.platform_name,
                user_id=source,
                text=text,
                channel_id=channel_id,
                metadata={
                    "timestamp": timestamp,
                    "group_id": group_info.get("groupId") if group_info else None,
                    "attachments": data_message.get("attachments", []),
                },
            )

            return {"ok": True}

        except Exception as e:
            self._logger.error(f"Error handling Signal webhook: {e}", exc_info=True)
            return {"error": str(e)}

    async def close(self) -> None:
        """リソースをクリーンアップ."""
        await self.stop_polling()
        await self._client.aclose()

