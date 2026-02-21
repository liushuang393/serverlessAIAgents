"""Discord Adapter - Discord 消息平台适配器.

支持通过 discord.py 库与 Discord Bot API 集成。

依赖安装：
    pip install discord.py>=2.0

环境变量：
    DISCORD_BOT_TOKEN: Discord Bot Token

Example:
    >>> from agentflow.channels import DiscordAdapter, MessageGateway
    >>> from agentflow import ChatBotSkill, WebSocketHub
    >>>
    >>> # 创建适配器
    >>> discord = DiscordAdapter(token=DISCORD_BOT_TOKEN)
    >>>
    >>> # 注册到网关
    >>> gateway = MessageGateway(WebSocketHub(), ChatBotSkill())
    >>> gateway.register_channel("discord", discord)
    >>>
    >>> # 启动 bot（长连接模式）
    >>> await discord.start_bot(gateway)
"""

from __future__ import annotations

import asyncio
import logging
from typing import TYPE_CHECKING, Any

from agentflow.channels.base import MessageChannelAdapter, UserInfo


if TYPE_CHECKING:
    from agentflow.channels.gateway import MessageGateway


logger = logging.getLogger(__name__)


class DiscordAdapter(MessageChannelAdapter):
    """Discord 消息平台适配器.

    使用 discord.py 库实现与 Discord 的集成。

    Features:
    - 发送/接收文本消息
    - Embed 富文本支持
    - 输入指示器（typing）
    - 用户信息获取
    - 图片、文件发送
    - 线程/频道支持
    """

    def __init__(
        self,
        token: str,
        *,
        intents_guilds: bool = True,
        intents_messages: bool = True,
        intents_message_content: bool = True,
    ) -> None:
        """初始化 Discord 适配器.

        Args:
            token: Discord Bot Token
            intents_guilds: 启用 Guilds intent
            intents_messages: 启用 Messages intent
            intents_message_content: 启用 Message Content intent

        Raises:
            ImportError: 如果 discord.py 未安装
        """
        try:
            import discord
        except ImportError as e:
            msg = "discord.py is required for DiscordAdapter. Install it with: pip install discord.py>=2.0"
            raise ImportError(msg) from e

        self._token = token
        self._logger = logging.getLogger("discord_adapter")

        # 创建 Intents
        intents = discord.Intents.default()
        intents.guilds = intents_guilds
        intents.messages = intents_messages
        intents.message_content = intents_message_content

        # 创建 Client（延迟初始化）
        self._client: discord.Client | None = None
        self._intents = intents
        self._discord = discord

    @property
    def platform_name(self) -> str:
        """平台名称."""
        return "discord"

    def _ensure_client(self) -> Any:
        """确保 client 已初始化.

        Returns:
            Discord Client 实例
        """
        if not self._client:
            self._client = self._discord.Client(intents=self._intents)
        return self._client

    # =========================================================================
    # 消息发送
    # =========================================================================

    async def send_message(
        self,
        channel_id: str,
        text: str,
        **kwargs: Any,
    ) -> str:
        """发送消息到 Discord.

        Args:
            channel_id: 频道 ID
            text: 消息文本
            **kwargs: 可选参数
                - embed: Discord Embed 对象
                - file: Discord File 对象
                - reference: 回复的消息 ID

        Returns:
            消息 ID

        Raises:
            Exception: 发送失败
        """
        try:
            client = self._ensure_client()

            # 获取频道
            channel = client.get_channel(int(channel_id))
            if not channel:
                channel = await client.fetch_channel(int(channel_id))

            # 发送消息
            message = await channel.send(
                content=text,
                embed=kwargs.get("embed"),
                file=kwargs.get("file"),
                reference=kwargs.get("reference"),
            )

            return str(message.id)

        except Exception as e:
            self._logger.exception(f"Failed to send Discord message: {e}")
            raise

    async def send_typing_indicator(self, channel_id: str) -> None:
        """发送"正在输入"指示器.

        Args:
            channel_id: 频道 ID
        """
        try:
            client = self._ensure_client()
            channel = client.get_channel(int(channel_id))
            if not channel:
                channel = await client.fetch_channel(int(channel_id))

            async with channel.typing():
                # 保持 typing 状态（Discord 自动持续10秒）
                await asyncio.sleep(0.1)

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
            channel_id: 频道 ID
            image_url: 图片 URL 或文件路径
            caption: 图片说明
            **kwargs: 可选参数

        Returns:
            消息 ID
        """
        try:
            client = self._ensure_client()
            channel = client.get_channel(int(channel_id))
            if not channel:
                channel = await client.fetch_channel(int(channel_id))

            # 创建 Embed
            embed = self._discord.Embed(description=caption or "")
            embed.set_image(url=image_url)

            message = await channel.send(embed=embed)
            return str(message.id)

        except Exception as e:
            self._logger.exception(f"Failed to send image: {e}")
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
            file_url: 文件路径
            filename: 文件名
            **kwargs: 可选参数

        Returns:
            消息 ID
        """
        try:
            client = self._ensure_client()
            channel = client.get_channel(int(channel_id))
            if not channel:
                channel = await client.fetch_channel(int(channel_id))

            # 创建 File
            file = self._discord.File(file_url, filename=filename)

            message = await channel.send(
                content=kwargs.get("content", ""),
                file=file,
            )
            return str(message.id)

        except Exception as e:
            self._logger.exception(f"Failed to send file: {e}")
            raise

    async def delete_message(
        self,
        channel_id: str,
        message_id: str,
    ) -> bool:
        """删除消息.

        Args:
            channel_id: 频道 ID
            message_id: 消息 ID

        Returns:
            是否成功删除
        """
        try:
            client = self._ensure_client()
            channel = client.get_channel(int(channel_id))
            if not channel:
                channel = await client.fetch_channel(int(channel_id))

            message = await channel.fetch_message(int(message_id))
            await message.delete()
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
            channel_id: 频道 ID
            message_id: 消息 ID
            new_text: 新文本
            **kwargs: 可选参数

        Returns:
            是否成功编辑
        """
        try:
            client = self._ensure_client()
            channel = client.get_channel(int(channel_id))
            if not channel:
                channel = await client.fetch_channel(int(channel_id))

            message = await channel.fetch_message(int(message_id))
            await message.edit(content=new_text, embed=kwargs.get("embed"))
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
            client = self._ensure_client()

            # 尝试从缓存获取
            user = client.get_user(int(user_id))
            if not user:
                user = await client.fetch_user(int(user_id))

            return UserInfo(
                user_id=user_id,
                username=user.name,
                display_name=user.display_name or user.name,
                avatar_url=user.avatar.url if user.avatar else None,
                is_bot=user.bot,
                metadata={
                    "discriminator": user.discriminator,
                    "created_at": user.created_at.isoformat() if user.created_at else None,
                },
            )

        except Exception as e:
            self._logger.warning(f"Failed to get user info: {e}")
            return UserInfo(user_id=user_id, display_name="Unknown")

    # =========================================================================
    # Bot 运行
    # =========================================================================

    async def start_bot(
        self,
        gateway: MessageGateway,
    ) -> None:
        """启动 Discord Bot（长连接模式）.

        Args:
            gateway: 消息网关实例
        """
        client = self._ensure_client()

        @client.event  # type: ignore[untyped-decorator]
        async def on_ready() -> None:
            """Bot 启动完成."""
            self._logger.info(f"Discord bot logged in as {client.user}")

        @client.event  # type: ignore[untyped-decorator]
        async def on_message(message: Any) -> None:
            """收到消息."""
            # 忽略自己的消息
            if message.author == client.user:
                return

            # 忽略其他 bot
            if message.author.bot:
                return

            # 只处理文本消息
            if not message.content:
                return

            user_id = str(message.author.id)
            channel_id = str(message.channel.id)
            text = message.content

            # 路由到网关（异步）
            await gateway.route_message_async(
                platform=self.platform_name,
                user_id=user_id,
                text=text,
                channel_id=channel_id,
                metadata={
                    "message_id": str(message.id),
                    "username": message.author.name,
                    "guild_id": str(message.guild.id) if message.guild else None,
                },
            )

        # 启动 bot
        self._logger.info("Starting Discord bot...")
        await client.start(self._token)

    async def stop_bot(self) -> None:
        """停止 Discord Bot."""
        if self._client:
            await self._client.close()
            self._logger.info("Discord bot stopped")

    # =========================================================================
    # 辅助方法
    # =========================================================================

    async def get_bot_info(self) -> dict[str, Any]:
        """获取 Bot 信息.

        Returns:
            Bot 信息字典
        """
        try:
            client = self._ensure_client()
            if client.user:
                return {
                    "id": str(client.user.id),
                    "username": client.user.name,
                    "discriminator": client.user.discriminator,
                    "is_bot": client.user.bot,
                }
            return {}
        except Exception as e:
            self._logger.exception(f"Failed to get bot info: {e}")
            return {}

    async def get_channel_info(self, channel_id: str) -> dict[str, Any]:
        """获取频道信息.

        Args:
            channel_id: 频道 ID

        Returns:
            频道信息字典
        """
        try:
            client = self._ensure_client()
            channel = client.get_channel(int(channel_id))
            if not channel:
                channel = await client.fetch_channel(int(channel_id))

            return {
                "id": str(channel.id),
                "name": channel.name,
                "type": str(channel.type),
                "guild_id": str(channel.guild.id) if hasattr(channel, "guild") else None,
            }
        except Exception as e:
            self._logger.exception(f"Failed to get channel info: {e}")
            return {}
