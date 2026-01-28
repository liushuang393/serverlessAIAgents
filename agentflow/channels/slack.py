# -*- coding: utf-8 -*-
"""Slack Adapter - Slack 消息平台适配器.

支持通过 slack-sdk 库与 Slack API 集成。

依赖安装：
    pip install slack-sdk>=3.0

环境变量：
    SLACK_BOT_TOKEN: Slack Bot User OAuth Token (xoxb-...)
    SLACK_SIGNING_SECRET: Slack Signing Secret（验证 webhook）

Example:
    >>> from agentflow.channels import SlackAdapter, MessageGateway
    >>> from agentflow import ChatBotSkill, WebSocketHub
    >>>
    >>> # 创建适配器
    >>> slack = SlackAdapter(token=SLACK_BOT_TOKEN)
    >>>
    >>> # 注册到网关
    >>> gateway = MessageGateway(WebSocketHub(), ChatBotSkill())
    >>> gateway.register_channel("slack", slack)
    >>>
    >>> # 处理 webhook（FastAPI）
    >>> @app.post("/webhook/slack")
    >>> async def slack_webhook(request: Request):
    >>>     body = await request.body()
    >>>     headers = dict(request.headers)
    >>>     await slack.handle_webhook(body, headers, gateway)
    >>>     return {"ok": True}
"""

from __future__ import annotations

import json
import logging
from typing import TYPE_CHECKING, Any

from agentflow.channels.base import MessageChannelAdapter, UserInfo

if TYPE_CHECKING:
    from agentflow.channels.gateway import MessageGateway


logger = logging.getLogger(__name__)


class SlackAdapter(MessageChannelAdapter):
    """Slack 消息平台适配器.

    使用 slack-sdk 库实现与 Slack 的集成。

    Features:
    - 发送/接收文本消息
    - 线程回复支持
    - 用户信息获取
    - Block Kit 富文本支持
    - 文件上传
    - Webhook 验证
    """

    def __init__(
        self,
        token: str,
        signing_secret: str | None = None,
        *,
        timeout: int = 30,
    ) -> None:
        """初始化 Slack 适配器.

        Args:
            token: Slack Bot Token (xoxb-...)
            signing_secret: Slack Signing Secret（webhook 验证）
            timeout: 请求超时时间（秒）

        Raises:
            ImportError: 如果 slack-sdk 未安装
        """
        try:
            from slack_sdk.web.async_client import AsyncWebClient
        except ImportError as e:
            msg = (
                "slack-sdk is required for SlackAdapter. "
                "Install it with: pip install slack-sdk>=3.0"
            )
            raise ImportError(msg) from e

        self._token = token
        self._signing_secret = signing_secret
        self._timeout = timeout
        self._client = AsyncWebClient(token=token, timeout=timeout)
        self._logger = logging.getLogger("slack_adapter")

    @property
    def platform_name(self) -> str:
        """平台名称."""
        return "slack"

    # =========================================================================
    # 消息发送
    # =========================================================================

    async def send_message(
        self,
        channel_id: str,
        text: str,
        **kwargs: Any,
    ) -> str:
        """发送消息到 Slack.

        Args:
            channel_id: 频道 ID 或用户 ID
            text: 消息文本
            **kwargs: 可选参数
                - thread_ts: 线程 timestamp（回复线程）
                - blocks: Block Kit 块列表
                - attachments: 附件列表
                - unfurl_links: 展开链接（默认 True）

        Returns:
            消息 timestamp（消息 ID）

        Raises:
            Exception: 发送失败
        """
        try:
            response = await self._client.chat_postMessage(
                channel=channel_id,
                text=text,
                thread_ts=kwargs.get("thread_ts"),
                blocks=kwargs.get("blocks"),
                attachments=kwargs.get("attachments"),
                unfurl_links=kwargs.get("unfurl_links", True),
            )

            if not response["ok"]:
                msg = f"Slack API error: {response.get('error')}"
                raise Exception(msg)

            return response["ts"]

        except Exception as e:
            self._logger.error(f"Failed to send Slack message: {e}")
            raise

    async def send_typing_indicator(self, channel_id: str) -> None:
        """发送"正在输入"指示器.

        注意：Slack 没有官方的 typing indicator API。
        这里使用短暂消息模拟（会立即更新）。

        Args:
            channel_id: 频道 ID
        """
        # Slack 没有 typing indicator，可以发送临时消息
        # 或者不实现（静默失败）
        pass

    async def send_image(
        self,
        channel_id: str,
        image_url: str,
        caption: str | None = None,
        **kwargs: Any,
    ) -> str:
        """发送图片.

        Args:
            channel_id: 频道 ID
            image_url: 图片 URL
            caption: 图片说明
            **kwargs: 可选参数

        Returns:
            消息 timestamp
        """
        try:
            # 使用 attachments 或 blocks 发送图片
            blocks = [
                {
                    "type": "image",
                    "image_url": image_url,
                    "alt_text": caption or "Image",
                }
            ]

            if caption:
                blocks.insert(
                    0,
                    {
                        "type": "section",
                        "text": {"type": "mrkdwn", "text": caption},
                    },
                )

            response = await self._client.chat_postMessage(
                channel=channel_id,
                blocks=blocks,
                text=caption or "Image",
            )

            return response["ts"]

        except Exception as e:
            self._logger.error(f"Failed to send image: {e}")
            raise

    async def send_file(
        self,
        channel_id: str,
        file_url: str,
        filename: str | None = None,
        **kwargs: Any,
    ) -> str:
        """发送文件.

        Args:
            channel_id: 频道 ID
            file_url: 文件 URL 或路径
            filename: 文件名
            **kwargs: 可选参数

        Returns:
            文件 ID
        """
        try:
            response = await self._client.files_upload(
                channels=channel_id,
                file=file_url,
                filename=filename,
                title=kwargs.get("title", filename),
                initial_comment=kwargs.get("comment"),
            )

            return response["file"]["id"]

        except Exception as e:
            self._logger.error(f"Failed to send file: {e}")
            raise

    async def delete_message(
        self,
        channel_id: str,
        message_id: str,
    ) -> bool:
        """删除消息.

        Args:
            channel_id: 频道 ID
            message_id: 消息 timestamp

        Returns:
            是否成功删除
        """
        try:
            response = await self._client.chat_delete(
                channel=channel_id,
                ts=message_id,
            )
            return response["ok"]
        except Exception as e:
            self._logger.warning(f"Failed to delete message: {e}")
            return False

    async def edit_message(
        self,
        channel_id: str,
        message_id: str,
        new_text: str,
        **kwargs: Any,
    ) -> bool:
        """编辑消息.

        Args:
            channel_id: 频道 ID
            message_id: 消息 timestamp
            new_text: 新文本
            **kwargs: 可选参数

        Returns:
            是否成功编辑
        """
        try:
            response = await self._client.chat_update(
                channel=channel_id,
                ts=message_id,
                text=new_text,
                blocks=kwargs.get("blocks"),
            )
            return response["ok"]
        except Exception as e:
            self._logger.warning(f"Failed to edit message: {e}")
            return False

    # =========================================================================
    # 用户信息
    # =========================================================================

    async def get_user_info(self, user_id: str) -> UserInfo:
        """获取用户信息.

        Args:
            user_id: 用户 ID

        Returns:
            用户信息
        """
        try:
            response = await self._client.users_info(user=user_id)

            if not response["ok"]:
                raise Exception(f"Slack API error: {response.get('error')}")

            user = response["user"]
            profile = user.get("profile", {})

            return UserInfo(
                user_id=user_id,
                username=user.get("name"),
                display_name=profile.get("display_name") or profile.get("real_name") or "Unknown",
                avatar_url=profile.get("image_192"),
                is_bot=user.get("is_bot", False),
                metadata={
                    "real_name": profile.get("real_name"),
                    "email": profile.get("email"),
                    "status_text": profile.get("status_text"),
                    "team_id": user.get("team_id"),
                },
            )

        except Exception as e:
            self._logger.warning(f"Failed to get user info: {e}")
            return UserInfo(user_id=user_id, display_name="Unknown")

    # =========================================================================
    # Webhook
    # =========================================================================

    async def handle_webhook(
        self,
        body: bytes,
        headers: dict[str, str],
        gateway: MessageGateway,
    ) -> dict[str, Any]:
        """处理 webhook 请求.

        Args:
            body: 请求体（bytes）
            headers: 请求头
            gateway: 消息网关实例

        Returns:
            响应字典
        """
        try:
            # 验证签名（如果提供了 signing_secret）
            if self._signing_secret:
                if not self._verify_signature(body, headers):
                    self._logger.warning("Invalid Slack signature")
                    return {"error": "Invalid signature"}

            # 解析 body
            data = json.loads(body.decode("utf-8"))

            # URL 验证挑战
            if data.get("type") == "url_verification":
                return {"challenge": data["challenge"]}

            # 事件回调
            if data.get("type") == "event_callback":
                event = data.get("event", {})

                # 忽略 bot 消息
                if event.get("bot_id"):
                    return {"ok": True}

                # 处理消息事件
                if event.get("type") == "message" and event.get("text"):
                    user_id = event.get("user")
                    channel_id = event.get("channel")
                    text = event.get("text")
                    thread_ts = event.get("thread_ts")

                    if user_id and channel_id and text:
                        # 路由到网关（异步）
                        await gateway.route_message_async(
                            platform=self.platform_name,
                            user_id=user_id,
                            text=text,
                            channel_id=channel_id,
                            metadata={
                                "ts": event.get("ts"),
                                "thread_ts": thread_ts,
                                "channel_type": event.get("channel_type"),
                            },
                        )

            return {"ok": True}

        except Exception as e:
            self._logger.error(f"Error handling webhook: {e}", exc_info=True)
            return {"error": str(e)}

    def _verify_signature(self, body: bytes, headers: dict[str, str]) -> bool:
        """验证 Slack 签名.

        Args:
            body: 请求体
            headers: 请求头

        Returns:
            签名是否有效
        """
        try:
            import hashlib
            import hmac
            import time

            timestamp = headers.get("x-slack-request-timestamp", "")
            signature = headers.get("x-slack-signature", "")

            if not timestamp or not signature:
                return False

            # 防止重放攻击（5分钟内）
            if abs(time.time() - float(timestamp)) > 60 * 5:
                return False

            # 计算签名
            sig_basestring = f"v0:{timestamp}:{body.decode('utf-8')}"
            my_signature = (
                "v0="
                + hmac.new(
                    self._signing_secret.encode(),
                    sig_basestring.encode(),
                    hashlib.sha256,
                ).hexdigest()
            )

            return hmac.compare_digest(my_signature, signature)

        except Exception as e:
            self._logger.error(f"Error verifying signature: {e}")
            return False

    # =========================================================================
    # 辅助方法
    # =========================================================================

    async def get_bot_info(self) -> dict[str, Any]:
        """获取 Bot 信息.

        Returns:
            Bot 信息字典
        """
        try:
            response = await self._client.auth_test()
            return {
                "user_id": response["user_id"],
                "bot_id": response.get("bot_id"),
                "team_id": response["team_id"],
                "user": response["user"],
            }
        except Exception as e:
            self._logger.error(f"Failed to get bot info: {e}")
            return {}

    async def get_channel_info(self, channel_id: str) -> dict[str, Any]:
        """获取频道信息.

        Args:
            channel_id: 频道 ID

        Returns:
            频道信息字典
        """
        try:
            response = await self._client.conversations_info(channel=channel_id)
            if response["ok"]:
                return response["channel"]
            return {}
        except Exception as e:
            self._logger.error(f"Failed to get channel info: {e}")
            return {}
