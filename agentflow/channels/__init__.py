# -*- coding: utf-8 -*-
"""Messaging Channels Module - 统一消息平台集成.

该模块提供统一的消息平台适配器接口，支持多种消息平台：
- Telegram
- Slack
- Discord
- WhatsApp
- Microsoft Teams
- Signal

主要组件：
- MessageChannelAdapter: 消息平台适配器基类
- MessageGateway: 消息路由网关（核心）
- ChannelMessage: 统一消息格式
- UserInfo: 用户信息

Example:
    >>> from agentflow.channels import MessageGateway, TelegramAdapter
    >>> from agentflow import ChatBotSkill, WebSocketHub
    >>>
    >>> # 创建网关
    >>> hub = WebSocketHub()
    >>> chatbot = ChatBotSkill()
    >>> gateway = MessageGateway(hub, chatbot)
    >>>
    >>> # 注册平台适配器
    >>> telegram = TelegramAdapter(token=TELEGRAM_TOKEN)
    >>> gateway.register_channel("telegram", telegram)
    >>>
    >>> # 路由消息
    >>> await gateway.route_message(
    ...     platform="telegram",
    ...     user_id="123456",
    ...     text="Hello"
    ... )
"""

from agentflow.channels.base import (
    ChannelMessage,
    MessageChannelAdapter,
    MessageMetadata,
    UserInfo,
)
from agentflow.channels.gateway import MessageGateway

# 可选导入（需要相应的依赖）
try:
    from agentflow.channels.telegram import TelegramAdapter
except ImportError:
    TelegramAdapter = None  # type: ignore[misc, assignment]

try:
    from agentflow.channels.slack import SlackAdapter
except ImportError:
    SlackAdapter = None  # type: ignore[misc, assignment]

try:
    from agentflow.channels.discord import DiscordAdapter
except ImportError:
    DiscordAdapter = None  # type: ignore[misc, assignment]

try:
    from agentflow.channels.teams import TeamsAdapter
except ImportError:
    TeamsAdapter = None  # type: ignore[misc, assignment]

try:
    from agentflow.channels.whatsapp import WhatsAppAdapter
except ImportError:
    WhatsAppAdapter = None  # type: ignore[misc, assignment]

try:
    from agentflow.channels.signal import SignalAdapter
except ImportError:
    SignalAdapter = None  # type: ignore[misc, assignment]


__all__ = [
    # Base
    "MessageChannelAdapter",
    "ChannelMessage",
    "UserInfo",
    "MessageMetadata",
    # Gateway
    "MessageGateway",
    # Adapters
    "TelegramAdapter",
    "SlackAdapter",
    "DiscordAdapter",
    "TeamsAdapter",
    "WhatsAppAdapter",
    "SignalAdapter",
]
