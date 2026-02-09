"""Message Gateway - 消息路由网关.

核心组件：统一管理多平台消息路由，连接消息平台与 AI Agent。

架构：
    Platform → MessageGateway → ChatBotSkill → Agent/Coordinator
                     ↓
              WebSocket Hub → Live Canvas

设计原则：
- 统一路由：所有平台消息统一处理
- 会话管理：自动管理用户会话
- 双向通信：平台 ↔ Agent 双向消息
- 实时同步：通过 WebSocket 同步到前端

Example:
    >>> from agentflow.channels import MessageGateway, TelegramAdapter
    >>> from agentflow import ChatBotSkill, WebSocketHub
    >>>
    >>> hub = WebSocketHub()
    >>> chatbot = ChatBotSkill()
    >>> gateway = MessageGateway(hub, chatbot)
    >>>
    >>> # 注册平台
    >>> gateway.register_channel("telegram", TelegramAdapter(token=TOKEN))
    >>>
    >>> # 路由消息
    >>> await gateway.route_message("telegram", "user_123", "Hello")
"""

from __future__ import annotations

import asyncio
import logging
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.api.websocket_hub import WebSocketHub
    from agentflow.channels.base import MessageChannelAdapter
    from agentflow.skills.chatbot import ChatBotSkill, ChatSession


logger = logging.getLogger(__name__)


class MessageGateway:
    """消息路由网关.

    核心功能：
    1. 管理多个消息平台适配器
    2. 路由消息到正确的 Agent
    3. 管理用户会话（跨平台）
    4. 同步消息到 WebSocket 客户端
    5. 处理错误和重试
    """

    def __init__(
        self,
        websocket_hub: WebSocketHub,
        chatbot_skill: ChatBotSkill,
        *,
        enable_typing_indicator: bool = True,
        session_prefix: str = "chat",
    ) -> None:
        """初始化消息网关.

        Args:
            websocket_hub: WebSocket Hub 实例
            chatbot_skill: ChatBot Skill 实例
            enable_typing_indicator: 是否启用输入指示器
            session_prefix: 会话 ID 前缀
        """
        self._hub = websocket_hub
        self._chatbot = chatbot_skill
        self._enable_typing = enable_typing_indicator
        self._session_prefix = session_prefix

        # 平台适配器注册表
        self._channels: dict[str, MessageChannelAdapter] = {}

        # 用户会话映射：platform:user_id -> session_id
        self._user_sessions: dict[str, str] = {}

        # 消息处理队列（避免并发问题）
        self._message_queues: dict[str, asyncio.Queue[dict[str, Any]]] = {}
        self._queue_tasks: dict[str, asyncio.Task[None]] = {}

        self._logger = logging.getLogger("message_gateway")

    # =========================================================================
    # 平台管理
    # =========================================================================

    def register_channel(
        self,
        platform: str,
        adapter: MessageChannelAdapter,
    ) -> None:
        """注册消息平台适配器.

        Args:
            platform: 平台名称（telegram, slack 等）
            adapter: 平台适配器实例
        """
        if platform in self._channels:
            self._logger.warning(f"Platform {platform} already registered, overwriting")

        self._channels[platform] = adapter
        self._logger.info(f"Registered channel: {platform}")

    def unregister_channel(self, platform: str) -> bool:
        """取消注册平台.

        Args:
            platform: 平台名称

        Returns:
            是否成功取消注册
        """
        if platform in self._channels:
            del self._channels[platform]
            self._logger.info(f"Unregistered channel: {platform}")
            return True
        return False

    def get_channel(self, platform: str) -> MessageChannelAdapter | None:
        """获取平台适配器.

        Args:
            platform: 平台名称

        Returns:
            平台适配器，不存在时返回 None
        """
        return self._channels.get(platform)

    def list_channels(self) -> list[str]:
        """列出所有已注册平台.

        Returns:
            平台名称列表
        """
        return list(self._channels.keys())

    # =========================================================================
    # 会话管理
    # =========================================================================

    def _get_session_key(self, platform: str, user_id: str) -> str:
        """生成会话键.

        Args:
            platform: 平台名称
            user_id: 用户 ID

        Returns:
            会话键（platform:user_id）
        """
        return f"{platform}:{user_id}"

    def _get_or_create_session(
        self,
        platform: str,
        user_id: str,
        metadata: dict[str, Any] | None = None,
    ) -> ChatSession:
        """获取或创建用户会话.

        Args:
            platform: 平台名称
            user_id: 用户 ID
            metadata: 会话元数据

        Returns:
            聊天会话
        """
        session_key = self._get_session_key(platform, user_id)

        # 检查是否已有会话
        if session_key in self._user_sessions:
            session_id = self._user_sessions[session_key]
            session = self._chatbot.get_session(session_id)
            if session:
                return session

        # 创建新会话
        session_metadata = metadata or {}
        session_metadata.update({
            "platform": platform,
            "user_id": user_id,
            "session_key": session_key,
        })

        session = self._chatbot.create_session(metadata=session_metadata)
        self._user_sessions[session_key] = session.id

        self._logger.info(f"Created session {session.id} for {session_key}")
        return session

    def get_session_by_user(
        self,
        platform: str,
        user_id: str,
    ) -> ChatSession | None:
        """根据用户获取会话.

        Args:
            platform: 平台名称
            user_id: 用户 ID

        Returns:
            聊天会话，不存在时返回 None
        """
        session_key = self._get_session_key(platform, user_id)
        session_id = self._user_sessions.get(session_key)
        if session_id:
            return self._chatbot.get_session(session_id)
        return None

    def clear_user_session(self, platform: str, user_id: str) -> bool:
        """清除用户会话.

        Args:
            platform: 平台名称
            user_id: 用户 ID

        Returns:
            是否成功清除
        """
        session_key = self._get_session_key(platform, user_id)
        session_id = self._user_sessions.get(session_key)

        if session_id:
            self._chatbot.clear_session(session_id)
            del self._user_sessions[session_key]
            self._logger.info(f"Cleared session for {session_key}")
            return True
        return False

    # =========================================================================
    # 消息路由（核心功能）
    # =========================================================================

    async def route_message(
        self,
        platform: str,
        user_id: str,
        text: str,
        channel_id: str | None = None,
        metadata: dict[str, Any] | None = None,
        *,
        send_typing: bool | None = None,
    ) -> str:
        """路由消息：Platform → Agent → Platform.

        这是网关的核心方法，处理完整的消息流：
        1. 获取/创建用户会话
        2. 发送输入指示器（可选）
        3. 调用 ChatBot Agent 处理
        4. 发送响应回平台
        5. 同步到 WebSocket 客户端

        Args:
            platform: 平台名称
            user_id: 用户 ID
            text: 消息文本
            channel_id: 频道 ID（默认使用 user_id）
            metadata: 额外元数据
            send_typing: 是否发送输入指示器（默认使用全局设置）

        Returns:
            Agent 响应文本

        Raises:
            ValueError: 平台未注册
            Exception: 处理失败
        """
        # 1. 验证平台
        adapter = self._channels.get(platform)
        if not adapter:
            msg = f"Platform not registered: {platform}"
            raise ValueError(msg)

        channel_id = channel_id or user_id

        try:
            # 2. 获取/创建会话
            session = self._get_or_create_session(
                platform=platform,
                user_id=user_id,
                metadata=metadata,
            )

            # 3. 发送输入指示器
            should_send_typing = (
                send_typing if send_typing is not None else self._enable_typing
            )
            if should_send_typing:
                try:
                    await adapter.send_typing_indicator(channel_id)
                except Exception as e:
                    self._logger.warning(f"Failed to send typing indicator: {e}")

            # 4. 调用 Agent 处理
            self._logger.info(f"Processing message from {platform}:{user_id}: {text[:50]}")
            response = await self._chatbot.chat(
                session_id=session.id,
                user_input=text,
                platform=platform,
                user_id=user_id,
            )

            # 5. 发送响应到平台
            try:
                message_id = await adapter.send_message(
                    channel_id=channel_id,
                    text=response,
                )
                self._logger.info(
                    f"Sent response to {platform}:{channel_id}, message_id={message_id}"
                )
            except Exception as e:
                self._logger.exception(f"Failed to send message to platform: {e}")
                raise

            # 6. 同步到 WebSocket 客户端
            await self._broadcast_to_websocket(
                session_id=session.id,
                event_type="assistant_message",
                data={
                    "platform": platform,
                    "user_id": user_id,
                    "channel_id": channel_id,
                    "text": response,
                    "message_id": message_id,
                },
            )

            return response

        except Exception as e:
            self._logger.error(f"Error routing message: {e}", exc_info=True)

            # 尝试发送错误消息到平台
            try:
                error_msg = "抱歉，处理您的消息时出现了错误。请稍后再试。"
                await adapter.send_message(channel_id, error_msg)
            except Exception as send_error:
                self._logger.exception(f"Failed to send error message: {send_error}")

            raise

    async def route_message_async(
        self,
        platform: str,
        user_id: str,
        text: str,
        **kwargs: Any,
    ) -> None:
        """异步路由消息（不等待完成）.

        适用于 webhook 场景，需要快速响应 200 OK。

        Args:
            platform: 平台名称
            user_id: 用户 ID
            text: 消息文本
            **kwargs: 传递给 route_message 的参数
        """
        session_key = self._get_session_key(platform, user_id)

        # 创建队列（如果不存在）
        if session_key not in self._message_queues:
            self._message_queues[session_key] = asyncio.Queue()
            self._queue_tasks[session_key] = asyncio.create_task(
                self._process_message_queue(session_key)
            )

        # 加入队列
        await self._message_queues[session_key].put({
            "platform": platform,
            "user_id": user_id,
            "text": text,
            "kwargs": kwargs,
        })

    async def _process_message_queue(self, session_key: str) -> None:
        """处理消息队列.

        Args:
            session_key: 会话键
        """
        queue = self._message_queues[session_key]

        while True:
            try:
                # 获取消息
                message = await queue.get()

                # 处理消息
                await self.route_message(
                    platform=message["platform"],
                    user_id=message["user_id"],
                    text=message["text"],
                    **message["kwargs"],
                )

                queue.task_done()

            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.error(f"Error processing queued message: {e}", exc_info=True)

    # =========================================================================
    # WebSocket 同步
    # =========================================================================

    async def _broadcast_to_websocket(
        self,
        session_id: str,
        event_type: str,
        data: dict[str, Any],
    ) -> None:
        """广播事件到 WebSocket 客户端.

        Args:
            session_id: 会话 ID
            event_type: 事件类型
            data: 事件数据
        """
        try:
            await self._hub.broadcast_room(
                room=session_id,
                message={
                    "type": event_type,
                    "session_id": session_id,
                    "data": data,
                    "timestamp": data.get("timestamp"),
                },
            )
        except Exception as e:
            self._logger.warning(f"Failed to broadcast to WebSocket: {e}")

    # =========================================================================
    # 辅助方法
    # =========================================================================

    async def send_message_to_platform(
        self,
        platform: str,
        channel_id: str,
        text: str,
        **kwargs: Any,
    ) -> str:
        """直接发送消息到平台（不经过 Agent）.

        Args:
            platform: 平台名称
            channel_id: 频道 ID
            text: 消息文本
            **kwargs: 平台特定参数

        Returns:
            消息 ID

        Raises:
            ValueError: 平台未注册
        """
        adapter = self._channels.get(platform)
        if not adapter:
            msg = f"Platform not registered: {platform}"
            raise ValueError(msg)

        return await adapter.send_message(channel_id, text, **kwargs)

    def get_statistics(self) -> dict[str, Any]:
        """获取网关统计信息.

        Returns:
            统计信息字典
        """
        return {
            "registered_channels": len(self._channels),
            "active_sessions": len(self._user_sessions),
            "message_queues": len(self._message_queues),
            "platforms": self.list_channels(),
        }

    async def shutdown(self) -> None:
        """关闭网关，清理资源."""
        self._logger.info("Shutting down message gateway...")

        # 取消所有队列任务
        for task in self._queue_tasks.values():
            task.cancel()

        # 等待任务完成
        await asyncio.gather(*self._queue_tasks.values(), return_exceptions=True)

        self._logger.info("Message gateway shut down")
