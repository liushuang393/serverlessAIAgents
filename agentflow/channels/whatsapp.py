"""WhatsApp Business API Adapter - WhatsApp Cloud API 統合.

Meta WhatsApp Business Cloud API を使用した WhatsApp 統合アダプター。

Example:
    >>> from agentflow.channels import WhatsAppAdapter, MessageGateway
    >>> from agentflow import ChatBotSkill, WebSocketHub
    >>>
    >>> # アダプター作成
    >>> whatsapp = WhatsAppAdapter(
    ...     phone_number_id=PHONE_NUMBER_ID,
    ...     access_token=ACCESS_TOKEN,
    ...     verify_token=VERIFY_TOKEN,
    ... )
    >>>
    >>> # ゲートウェイに登録
    >>> gateway = MessageGateway(WebSocketHub(), ChatBotSkill())
    >>> gateway.register_channel("whatsapp", whatsapp)
    >>>
    >>> # Webhook 処理（FastAPI）
    >>> @app.post("/webhook/whatsapp")
    >>> async def whatsapp_webhook(request: Request):
    >>>     body = await request.json()
    >>>     await whatsapp.handle_webhook(body, gateway)
    >>>     return {"ok": True}
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

import httpx

from agentflow.channels.base import MessageChannelAdapter, UserInfo


if TYPE_CHECKING:
    from agentflow.channels.gateway import MessageGateway


logger = logging.getLogger(__name__)


class WhatsAppAdapter(MessageChannelAdapter):
    """WhatsApp Business Cloud API アダプター.

    Meta WhatsApp Business Cloud API を使用した統合。

    Features:
    - テキストメッセージ送受信
    - 画像・ドキュメント送信
    - テンプレートメッセージ
    - インタラクティブメッセージ（ボタン、リスト）
    - Webhook 検証
    """

    # WhatsApp Cloud API ベース URL
    API_BASE = "https://graph.facebook.com/v18.0"

    def __init__(
        self,
        phone_number_id: str,
        access_token: str,
        verify_token: str | None = None,
        *,
        api_version: str = "v18.0",
        timeout: int = 30,
    ) -> None:
        """WhatsApp アダプターを初期化.

        Args:
            phone_number_id: WhatsApp Business Phone Number ID
            access_token: Meta Graph API Access Token
            verify_token: Webhook 検証トークン
            api_version: Graph API バージョン
            timeout: リクエストタイムアウト（秒）
        """
        self._phone_number_id = phone_number_id
        self._access_token = access_token
        self._verify_token = verify_token
        self._api_version = api_version
        self._timeout = timeout
        self._logger = logging.getLogger("whatsapp_adapter")

        self._api_base = f"https://graph.facebook.com/{api_version}"
        self._client = httpx.AsyncClient(timeout=timeout)

    @property
    def platform_name(self) -> str:
        """プラットフォーム名."""
        return "whatsapp"

    async def send_message(
        self,
        channel_id: str,
        text: str,
        **kwargs: Any,
    ) -> str:
        """WhatsApp にメッセージを送信.

        Args:
            channel_id: 受信者の電話番号（国際形式、例: 81901234567）
            text: メッセージテキスト
            **kwargs: 追加パラメータ（preview_url, reply_to 等）

        Returns:
            メッセージ ID
        """
        url = f"{self._api_base}/{self._phone_number_id}/messages"
        headers = {
            "Authorization": f"Bearer {self._access_token}",
            "Content-Type": "application/json",
        }

        payload: dict[str, Any] = {
            "messaging_product": "whatsapp",
            "recipient_type": "individual",
            "to": channel_id,
            "type": "text",
            "text": {
                "preview_url": kwargs.get("preview_url", False),
                "body": text,
            },
        }

        # 返信の場合
        reply_to = kwargs.get("reply_to")
        if reply_to:
            payload["context"] = {"message_id": reply_to}

        try:
            response = await self._client.post(url, headers=headers, json=payload)
            response.raise_for_status()
            data = response.json()
            message_id = data.get("messages", [{}])[0].get("id", "")
            self._logger.info(f"Sent WhatsApp message: {message_id}")
            return str(message_id)

        except httpx.HTTPStatusError as e:
            self._logger.exception(f"WhatsApp API error: {e.response.text}")
            raise
        except Exception as e:
            self._logger.error(f"Failed to send WhatsApp message: {e}", exc_info=True)
            raise

    async def send_typing_indicator(self, channel_id: str) -> None:
        """入力インジケーターを送信（WhatsApp では未サポート）.

        Args:
            channel_id: 受信者の電話番号
        """
        # WhatsApp Cloud API は typing indicator をサポートしていない

    async def get_user_info(self, user_id: str) -> UserInfo:
        """ユーザー情報を取得.

        Args:
            user_id: ユーザー ID（電話番号）

        Returns:
            ユーザー情報
        """
        # WhatsApp では電話番号がユーザー ID
        return UserInfo(
            user_id=user_id,
            display_name=f"+{user_id}",
            metadata={"platform": "whatsapp", "phone": user_id},
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
            image_url: 画像 URL
            caption: キャプション

        Returns:
            メッセージ ID
        """
        url = f"{self._api_base}/{self._phone_number_id}/messages"
        headers = {
            "Authorization": f"Bearer {self._access_token}",
            "Content-Type": "application/json",
        }

        payload: dict[str, Any] = {
            "messaging_product": "whatsapp",
            "recipient_type": "individual",
            "to": channel_id,
            "type": "image",
            "image": {
                "link": image_url,
            },
        }

        if caption:
            payload["image"]["caption"] = caption

        try:
            response = await self._client.post(url, headers=headers, json=payload)
            response.raise_for_status()
            data = response.json()
            return str(data.get("messages", [{}])[0].get("id", ""))
        except Exception as e:
            self._logger.error(f"Failed to send WhatsApp image: {e}", exc_info=True)
            raise

    async def send_template(
        self,
        channel_id: str,
        template_name: str,
        language_code: str = "ja",
        components: list[dict[str, Any]] | None = None,
    ) -> str:
        """テンプレートメッセージを送信.

        Args:
            channel_id: 受信者の電話番号
            template_name: テンプレート名
            language_code: 言語コード
            components: テンプレートコンポーネント

        Returns:
            メッセージ ID
        """
        url = f"{self._api_base}/{self._phone_number_id}/messages"
        headers = {
            "Authorization": f"Bearer {self._access_token}",
            "Content-Type": "application/json",
        }

        payload: dict[str, Any] = {
            "messaging_product": "whatsapp",
            "to": channel_id,
            "type": "template",
            "template": {
                "name": template_name,
                "language": {"code": language_code},
            },
        }

        if components:
            payload["template"]["components"] = components

        try:
            response = await self._client.post(url, headers=headers, json=payload)
            response.raise_for_status()
            data = response.json()
            return str(data.get("messages", [{}])[0].get("id", ""))
        except Exception as e:
            self._logger.error(f"Failed to send WhatsApp template: {e}", exc_info=True)
            raise

    async def send_interactive(
        self,
        channel_id: str,
        interactive_type: str,
        body_text: str,
        buttons: list[dict[str, str]] | None = None,
        sections: list[dict[str, Any]] | None = None,
        header: str | None = None,
        footer: str | None = None,
    ) -> str:
        """インタラクティブメッセージを送信（ボタン、リスト）.

        Args:
            channel_id: 受信者の電話番号
            interactive_type: "button" または "list"
            body_text: 本文テキスト
            buttons: ボタンリスト（type="button" の場合）
            sections: セクションリスト（type="list" の場合）
            header: ヘッダーテキスト
            footer: フッターテキスト

        Returns:
            メッセージ ID
        """
        url = f"{self._api_base}/{self._phone_number_id}/messages"
        headers = {
            "Authorization": f"Bearer {self._access_token}",
            "Content-Type": "application/json",
        }

        interactive: dict[str, Any] = {
            "type": interactive_type,
            "body": {"text": body_text},
        }

        if header:
            interactive["header"] = {"type": "text", "text": header}
        if footer:
            interactive["footer"] = {"text": footer}

        if interactive_type == "button" and buttons:
            interactive["action"] = {
                "buttons": [
                    {"type": "reply", "reply": {"id": b["id"], "title": b["title"]}}
                    for b in buttons[:3]  # 最大3ボタン
                ]
            }
        elif interactive_type == "list" and sections:
            interactive["action"] = {
                "button": "選択してください",
                "sections": sections,
            }

        payload: dict[str, Any] = {
            "messaging_product": "whatsapp",
            "recipient_type": "individual",
            "to": channel_id,
            "type": "interactive",
            "interactive": interactive,
        }

        try:
            response = await self._client.post(url, headers=headers, json=payload)
            response.raise_for_status()
            data = response.json()
            return str(data.get("messages", [{}])[0].get("id", ""))
        except Exception as e:
            self._logger.error(f"Failed to send WhatsApp interactive: {e}", exc_info=True)
            raise

    def verify_webhook(self, mode: str, token: str, challenge: str) -> str | None:
        """Webhook 検証リクエストを処理.

        Args:
            mode: hub.mode パラメータ
            token: hub.verify_token パラメータ
            challenge: hub.challenge パラメータ

        Returns:
            検証成功時は challenge、失敗時は None
        """
        if mode == "subscribe" and token == self._verify_token:
            self._logger.info("WhatsApp webhook verified")
            return challenge
        self._logger.warning("WhatsApp webhook verification failed")
        return None

    async def handle_webhook(
        self,
        payload: dict[str, Any],
        gateway: MessageGateway,
    ) -> dict[str, Any]:
        """Webhook リクエストを処理.

        Args:
            payload: Webhook ペイロード
            gateway: メッセージゲートウェイ

        Returns:
            処理結果
        """
        try:
            # WhatsApp Cloud API の Webhook 構造を解析
            entry = payload.get("entry", [])
            for e in entry:
                changes = e.get("changes", [])
                for change in changes:
                    value = change.get("value", {})
                    messages = value.get("messages", [])

                    for message in messages:
                        msg_type = message.get("type")
                        from_number = message.get("from")
                        msg_id = message.get("id")

                        # テキストメッセージ
                        if msg_type == "text":
                            text = message.get("text", {}).get("body", "")
                            if from_number and text:
                                await gateway.route_message_async(
                                    platform=self.platform_name,
                                    user_id=from_number,
                                    text=text,
                                    channel_id=from_number,
                                    metadata={
                                        "message_id": msg_id,
                                        "message_type": msg_type,
                                        "timestamp": message.get("timestamp"),
                                    },
                                )

                        # インタラクティブ応答（ボタン、リスト選択）
                        elif msg_type == "interactive":
                            interactive = message.get("interactive", {})
                            int_type = interactive.get("type")

                            if int_type == "button_reply":
                                reply = interactive.get("button_reply", {})
                                text = reply.get("title", "")
                            elif int_type == "list_reply":
                                reply = interactive.get("list_reply", {})
                                text = reply.get("title", "")
                            else:
                                continue

                            if from_number and text:
                                await gateway.route_message_async(
                                    platform=self.platform_name,
                                    user_id=from_number,
                                    text=text,
                                    channel_id=from_number,
                                    metadata={
                                        "message_id": msg_id,
                                        "message_type": "interactive",
                                        "interactive_type": int_type,
                                        "reply_id": reply.get("id"),
                                    },
                                )

            return {"ok": True}

        except Exception as e:
            self._logger.error(f"Error handling WhatsApp webhook: {e}", exc_info=True)
            return {"error": str(e)}

    async def mark_as_read(self, message_id: str) -> bool:
        """メッセージを既読にマーク.

        Args:
            message_id: メッセージ ID

        Returns:
            成功したかどうか
        """
        url = f"{self._api_base}/{self._phone_number_id}/messages"
        headers = {
            "Authorization": f"Bearer {self._access_token}",
            "Content-Type": "application/json",
        }

        payload = {
            "messaging_product": "whatsapp",
            "status": "read",
            "message_id": message_id,
        }

        try:
            response = await self._client.post(url, headers=headers, json=payload)
            response.raise_for_status()
            return True
        except Exception as e:
            self._logger.warning(f"Failed to mark message as read: {e}")
            return False

    async def close(self) -> None:
        """HTTP クライアントをクローズ."""
        await self._client.aclose()
