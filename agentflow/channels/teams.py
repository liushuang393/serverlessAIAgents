"""Microsoft Teams Adapter - Teams Bot Framework 統合.

Microsoft Teams Bot Framework SDK を使用した Teams 統合アダプター。

Example:
    >>> from agentflow.channels import TeamsAdapter, MessageGateway
    >>> from agentflow import ChatBotSkill, WebSocketHub
    >>>
    >>> # アダプター作成
    >>> teams = TeamsAdapter(app_id=APP_ID, app_password=APP_PASSWORD)
    >>>
    >>> # ゲートウェイに登録
    >>> gateway = MessageGateway(WebSocketHub(), ChatBotSkill())
    >>> gateway.register_channel("teams", teams)
    >>>
    >>> # Webhook 処理（FastAPI）
    >>> @app.post("/api/messages")
    >>> async def teams_webhook(request: Request):
    >>>     body = await request.json()
    >>>     auth_header = request.headers.get("Authorization", "")
    >>>     await teams.handle_webhook(body, auth_header, gateway)
    >>>     return {"ok": True}
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from agentflow.channels.base import MessageChannelAdapter, UserInfo


if TYPE_CHECKING:
    from agentflow.channels.gateway import MessageGateway


logger = logging.getLogger(__name__)


class TeamsAdapter(MessageChannelAdapter):
    """Microsoft Teams メッセージプラットフォームアダプター.

    Bot Framework SDK を使用した Teams 統合。

    Features:
    - テキストメッセージ送受信
    - Adaptive Cards 対応
    - 入力インジケーター
    - ユーザー情報取得
    - Webhook / Proactive メッセージ
    """

    def __init__(
        self,
        app_id: str,
        app_password: str,
        *,
        tenant_id: str | None = None,
        timeout: int = 30,
    ) -> None:
        """Microsoft Teams アダプターを初期化.

        Args:
            app_id: Azure Bot App ID
            app_password: Azure Bot App Password
            tenant_id: Azure Tenant ID（オプション、シングルテナント用）
            timeout: リクエストタイムアウト（秒）

        Raises:
            ImportError: botbuilder-core がインストールされていない場合
        """
        try:
            from botbuilder.core import TurnContext
            from botbuilder.schema import Activity, ActivityTypes
        except ImportError as e:
            msg = (
                "botbuilder-core is required for TeamsAdapter. "
                "Install it with: pip install botbuilder-core>=4.14"
            )
            raise ImportError(msg) from e

        self._app_id = app_id
        self._app_password = app_password
        self._tenant_id = tenant_id
        self._timeout = timeout
        self._logger = logging.getLogger("teams_adapter")

        # Bot Framework コンポーネント
        self._activity_types = ActivityTypes
        self._turn_context_class = TurnContext
        self._activity_class = Activity

        # 会話参照キャッシュ（Proactive メッセージ用）
        self._conversation_references: dict[str, Any] = {}

    @property
    def platform_name(self) -> str:
        """プラットフォーム名."""
        return "teams"

    async def send_message(
        self,
        channel_id: str,
        text: str,
        **kwargs: Any,
    ) -> str:
        """Teams にメッセージを送信.

        Args:
            channel_id: 会話 ID
            text: メッセージテキスト
            **kwargs: 追加パラメータ（adaptive_card, reply_to_id 等）

        Returns:
            メッセージ ID
        """
        try:
            from botbuilder.core import BotFrameworkAdapter, BotFrameworkAdapterSettings
            from botbuilder.schema import Activity, ActivityTypes

            settings = BotFrameworkAdapterSettings(
                app_id=self._app_id,
                app_password=self._app_password,
            )
            adapter = BotFrameworkAdapter(settings)

            # 会話参照を取得
            conv_ref = self._conversation_references.get(channel_id)
            if not conv_ref:
                self._logger.warning(f"No conversation reference for {channel_id}")
                return ""

            message_id = ""

            async def send_callback(turn_context: Any) -> None:
                nonlocal message_id
                # Adaptive Card がある場合
                adaptive_card = kwargs.get("adaptive_card")
                if adaptive_card:
                    activity = Activity(
                        type=ActivityTypes.message,
                        attachments=[{
                            "contentType": "application/vnd.microsoft.card.adaptive",
                            "content": adaptive_card,
                        }],
                    )
                    response = await turn_context.send_activity(activity)
                else:
                    response = await turn_context.send_activity(text)
                message_id = response.id if response else ""

            await adapter.continue_conversation(conv_ref, send_callback, self._app_id)
            return message_id

        except Exception as e:
            self._logger.error(f"Failed to send Teams message: {e}", exc_info=True)
            raise

    async def send_typing_indicator(self, channel_id: str) -> None:
        """入力インジケーターを送信.

        Args:
            channel_id: 会話 ID
        """
        try:
            from botbuilder.core import BotFrameworkAdapter, BotFrameworkAdapterSettings
            from botbuilder.schema import Activity, ActivityTypes

            settings = BotFrameworkAdapterSettings(
                app_id=self._app_id,
                app_password=self._app_password,
            )
            adapter = BotFrameworkAdapter(settings)

            conv_ref = self._conversation_references.get(channel_id)
            if not conv_ref:
                return

            async def typing_callback(turn_context: Any) -> None:
                typing_activity = Activity(type=ActivityTypes.typing)
                await turn_context.send_activity(typing_activity)

            await adapter.continue_conversation(conv_ref, typing_callback, self._app_id)

        except Exception as e:
            self._logger.warning(f"Failed to send typing indicator: {e}")

    async def get_user_info(self, user_id: str) -> UserInfo:
        """ユーザー情報を取得.

        Args:
            user_id: ユーザー ID

        Returns:
            ユーザー情報
        """
        # Teams ではユーザー情報は Activity から取得するため、
        # キャッシュから返すか基本情報を返す
        return UserInfo(
            user_id=user_id,
            display_name=f"Teams User {user_id[:8]}",
            metadata={"platform": "teams"},
        )

    async def get_bot_info(self) -> dict[str, Any]:
        """Bot 情報を取得.

        Returns:
            Bot 情報（app_id, tenant_id 等）
        """
        return {
            "app_id": self._app_id,
            "tenant_id": self._tenant_id,
            "platform": "teams",
        }

    async def handle_webhook(
        self,
        activity_data: dict[str, Any],
        auth_header: str,
        gateway: MessageGateway,
    ) -> dict[str, Any]:
        """Webhook リクエストを処理.

        Args:
            activity_data: Activity データ（dict）
            auth_header: Authorization ヘッダー
            gateway: メッセージゲートウェイ

        Returns:
            処理結果
        """
        try:
            from botbuilder.core import BotFrameworkAdapter, BotFrameworkAdapterSettings
            from botbuilder.schema import Activity

            settings = BotFrameworkAdapterSettings(
                app_id=self._app_id,
                app_password=self._app_password,
            )
            BotFrameworkAdapter(settings)

            # Activity をパース
            activity = Activity().deserialize(activity_data)

            # 認証検証
            # 本番環境では adapter.authenticate_request を使用

            # メッセージ Activity を処理
            if activity.type == "message" and activity.text:
                user_id = activity.from_property.id if activity.from_property else ""
                channel_id = activity.conversation.id if activity.conversation else ""
                text = activity.text

                # 会話参照を保存（Proactive メッセージ用）
                if channel_id:
                    self._conversation_references[channel_id] = (
                        activity.get_conversation_reference()
                    )

                # ゲートウェイにルーティング
                if user_id and channel_id and text:
                    await gateway.route_message_async(
                        platform=self.platform_name,
                        user_id=user_id,
                        text=text,
                        channel_id=channel_id,
                        metadata={
                            "activity_id": activity.id,
                            "service_url": activity.service_url,
                            "channel_type": activity.channel_id,
                            "tenant_id": (
                                activity.conversation.tenant_id
                                if activity.conversation
                                else None
                            ),
                        },
                    )

            return {"ok": True}

        except Exception as e:
            self._logger.error(f"Error handling Teams webhook: {e}", exc_info=True)
            return {"error": str(e)}

    async def send_adaptive_card(
        self,
        channel_id: str,
        card: dict[str, Any],
        text: str = "",
    ) -> str:
        """Adaptive Card を送信.

        Args:
            channel_id: 会話 ID
            card: Adaptive Card JSON
            text: フォールバックテキスト

        Returns:
            メッセージ ID
        """
        return await self.send_message(
            channel_id,
            text,
            adaptive_card=card,
        )

