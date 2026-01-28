# -*- coding: utf-8 -*-
"""Telegram Adapter - Telegram 消息平台适配器.

支持通过 python-telegram-bot 库与 Telegram Bot API 集成。

依赖安装：
    pip install python-telegram-bot>=20.0

环境变量：
    TELEGRAM_BOT_TOKEN: Telegram Bot Token

Example:
    >>> from agentflow.channels import TelegramAdapter, MessageGateway
    >>> from agentflow import ChatBotSkill, WebSocketHub
    >>>
    >>> # 创建适配器
    >>> telegram = TelegramAdapter(token=TELEGRAM_BOT_TOKEN)
    >>>
    >>> # 注册到网关
    >>> gateway = MessageGateway(WebSocketHub(), ChatBotSkill())
    >>> gateway.register_channel("telegram", telegram)
    >>>
    >>> # 启动 webhook（FastAPI）
    >>> @app.post("/webhook/telegram")
    >>> async def telegram_webhook(update: dict):
    >>>     await telegram.handle_webhook(update, gateway)
    >>>     return {"ok": True}
    >>>
    >>> # 或启动轮询
    >>> await telegram.start_polling(gateway)
"""

from __future__ import annotations

import asyncio
import logging
from typing import TYPE_CHECKING, Any

from agentflow.channels.base import MessageChannelAdapter, UserInfo

if TYPE_CHECKING:
    from agentflow.channels.gateway import MessageGateway


logger = logging.getLogger(__name__)


class TelegramAdapter(MessageChannelAdapter):
    """Telegram 消息平台适配器.

    使用 python-telegram-bot 库实现与 Telegram 的集成。

    Features:
    - 发送/接收文本消息
    - 输入指示器（typing）
    - 用户信息获取
    - 支持 webhook 和轮询模式
    - 图片、文件发送
    """

    def __init__(
        self,
        token: str,
        *,
        parse_mode: str = "Markdown",
        timeout: int = 30,
    ) -> None:
        """初始化 Telegram 适配器.

        Args:
            token: Telegram Bot Token
            parse_mode: 消息解析模式（Markdown, HTML, None）
            timeout: 请求超时时间（秒）

        Raises:
            ImportError: 如果 python-telegram-bot 未安装
        """
        try:
            from telegram import Bot
            from telegram.constants import ChatAction
        except ImportError as e:
            msg = (
                "python-telegram-bot is required for TelegramAdapter. "
                "Install it with: pip install python-telegram-bot>=20.0"
            )
            raise ImportError(msg) from e

        self._token = token
        self._parse_mode = parse_mode
        self._timeout = timeout
        self._bot = Bot(token=token)
        self._chat_action = ChatAction
        self._logger = logging.getLogger("telegram_adapter")

    @property
    def platform_name(self) -> str:
        """平台名称."""
        return "telegram"

    # =========================================================================
    # 消息发送
    # =========================================================================

    async def send_message(
        self,
        channel_id: str,
        text: str,
        **kwargs: Any,
    ) -> str:
        """发送消息到 Telegram.

        Args:
            channel_id: 聊天 ID（用户或群组）
            text: 消息文本
            **kwargs: 可选参数
                - reply_to_message_id: 回复的消息 ID
                - parse_mode: 解析模式（覆盖默认值）
                - disable_web_page_preview: 禁用链接预览

        Returns:
            消息 ID

        Raises:
            Exception: 发送失败
        """
        try:
            parse_mode = kwargs.get("parse_mode", self._parse_mode)
            reply_to = kwargs.get("reply_to_message_id")
            disable_preview = kwargs.get("disable_web_page_preview", False)

            message = await self._bot.send_message(
                chat_id=int(channel_id),
                text=text,
                parse_mode=parse_mode,
                reply_to_message_id=reply_to,
                disable_web_page_preview=disable_preview,
            )

            return str(message.message_id)

        except Exception as e:
            self._logger.error(f"Failed to send Telegram message: {e}")
            raise

    async def send_typing_indicator(self, channel_id: str) -> None:
        """发送"正在输入"指示器.

        Args:
            channel_id: 聊天 ID
        """
        try:
            await self._bot.send_chat_action(
                chat_id=int(channel_id),
                action=self._chat_action.TYPING,
            )
        except Exception as e:
            self._logger.warning(f"Failed to send typing indicator: {e}")

    async def send_image(
        self,
        channel_id: str,
        image_url: str,
        caption: str | None = None,
        **kwargs: Any,
    ) -> str:
        """发送图片.

        Args:
            channel_id: 聊天 ID
            image_url: 图片 URL 或文件路径
            caption: 图片说明
            **kwargs: 可选参数

        Returns:
            消息 ID
        """
        try:
            message = await self._bot.send_photo(
                chat_id=int(channel_id),
                photo=image_url,
                caption=caption,
                parse_mode=kwargs.get("parse_mode", self._parse_mode),
            )
            return str(message.message_id)
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
            channel_id: 聊天 ID
            file_url: 文件 URL 或路径
            filename: 文件名（可选）
            **kwargs: 可选参数

        Returns:
            消息 ID
        """
        try:
            message = await self._bot.send_document(
                chat_id=int(channel_id),
                document=file_url,
                filename=filename,
                caption=kwargs.get("caption"),
            )
            return str(message.message_id)
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
            channel_id: 聊天 ID
            message_id: 消息 ID

        Returns:
            是否成功删除
        """
        try:
            await self._bot.delete_message(
                chat_id=int(channel_id),
                message_id=int(message_id),
            )
            return True
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
            channel_id: 聊天 ID
            message_id: 消息 ID
            new_text: 新文本
            **kwargs: 可选参数

        Returns:
            是否成功编辑
        """
        try:
            await self._bot.edit_message_text(
                chat_id=int(channel_id),
                message_id=int(message_id),
                text=new_text,
                parse_mode=kwargs.get("parse_mode", self._parse_mode),
            )
            return True
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
            chat = await self._bot.get_chat(int(user_id))

            username = chat.username
            display_name = chat.full_name or chat.first_name or "Unknown"

            # 获取头像（如果有）
            avatar_url = None
            if chat.photo:
                photo_file = await self._bot.get_file(chat.photo.big_file_id)
                avatar_url = photo_file.file_path

            return UserInfo(
                user_id=user_id,
                username=username,
                display_name=display_name,
                avatar_url=avatar_url,
                is_bot=chat.type == "bot",
                metadata={
                    "type": chat.type,
                    "bio": chat.bio,
                },
            )

        except Exception as e:
            self._logger.warning(f"Failed to get user info: {e}")
            # 返回基础信息
            return UserInfo(user_id=user_id, display_name="Unknown")

    # =========================================================================
    # Webhook / Polling
    # =========================================================================

    async def handle_webhook(
        self,
        update_data: dict[str, Any],
        gateway: MessageGateway,
    ) -> None:
        """处理 webhook 更新.

        Args:
            update_data: Telegram Update 数据（dict）
            gateway: 消息网关实例
        """
        try:
            from telegram import Update

            # 解析 Update
            update = Update.de_json(update_data, self._bot)

            if not update or not update.message:
                return

            message = update.message

            # 只处理文本消息
            if not message.text:
                return

            user_id = str(message.from_user.id)
            chat_id = str(message.chat.id)
            text = message.text

            # 路由到网关（异步处理）
            await gateway.route_message_async(
                platform=self.platform_name,
                user_id=user_id,
                text=text,
                channel_id=chat_id,
                metadata={
                    "message_id": message.message_id,
                    "username": message.from_user.username,
                    "chat_type": message.chat.type,
                },
            )

        except Exception as e:
            self._logger.error(f"Error handling webhook: {e}", exc_info=True)

    async def start_polling(
        self,
        gateway: MessageGateway,
        *,
        interval: float = 1.0,
    ) -> None:
        """启动轮询模式.

        Args:
            gateway: 消息网关实例
            interval: 轮询间隔（秒）
        """
        from telegram import Update
        from telegram.ext import Application, MessageHandler, filters

        self._logger.info("Starting Telegram polling...")

        # 创建 Application
        app = Application.builder().token(self._token).build()

        # 定义消息处理器
        async def handle_message(update: Update, context: Any) -> None:
            """处理消息."""
            if not update.message or not update.message.text:
                return

            user_id = str(update.message.from_user.id)
            chat_id = str(update.message.chat.id)
            text = update.message.text

            # 路由到网关
            await gateway.route_message_async(
                platform=self.platform_name,
                user_id=user_id,
                text=text,
                channel_id=chat_id,
                metadata={
                    "message_id": update.message.message_id,
                    "username": update.message.from_user.username,
                },
            )

        # 注册处理器
        app.add_handler(MessageHandler(filters.TEXT & ~filters.COMMAND, handle_message))

        # 启动轮询
        await app.initialize()
        await app.start()
        await app.updater.start_polling(poll_interval=interval)

        self._logger.info("Telegram polling started")

        # 保持运行
        try:
            await asyncio.Event().wait()
        except KeyboardInterrupt:
            self._logger.info("Stopping Telegram polling...")
            await app.stop()
            await app.shutdown()

    # =========================================================================
    # 辅助方法
    # =========================================================================

    async def get_bot_info(self) -> dict[str, Any]:
        """获取 Bot 信息.

        Returns:
            Bot 信息字典
        """
        try:
            me = await self._bot.get_me()
            return {
                "id": me.id,
                "username": me.username,
                "first_name": me.first_name,
                "is_bot": me.is_bot,
            }
        except Exception as e:
            self._logger.error(f"Failed to get bot info: {e}")
            return {}

    async def set_webhook(self, webhook_url: str) -> bool:
        """设置 webhook.

        Args:
            webhook_url: Webhook URL

        Returns:
            是否成功设置
        """
        try:
            await self._bot.set_webhook(url=webhook_url)
            self._logger.info(f"Webhook set to: {webhook_url}")
            return True
        except Exception as e:
            self._logger.error(f"Failed to set webhook: {e}")
            return False

    async def delete_webhook(self) -> bool:
        """删除 webhook.

        Returns:
            是否成功删除
        """
        try:
            await self._bot.delete_webhook()
            self._logger.info("Webhook deleted")
            return True
        except Exception as e:
            self._logger.error(f"Failed to delete webhook: {e}")
            return False
