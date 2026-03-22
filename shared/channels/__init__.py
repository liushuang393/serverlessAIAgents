"""channels パッケージ."""

from shared.channels.base import ChannelMessage, MessageChannelAdapter, MessageMetadata, MessageType, UserInfo
from shared.channels.discord import DiscordAdapter
from shared.channels.gateway import MessageGateway
from shared.channels.signal import SignalAdapter
from shared.channels.slack import SlackAdapter
from shared.channels.teams import TeamsAdapter
from shared.channels.telegram import TelegramAdapter
from shared.channels.whatsapp import WhatsAppAdapter


__all__ = [
    "ChannelMessage",
    "DiscordAdapter",
    "MessageChannelAdapter",
    "MessageGateway",
    "MessageMetadata",
    "MessageType",
    "SignalAdapter",
    "SlackAdapter",
    "TeamsAdapter",
    "TelegramAdapter",
    "UserInfo",
    "WhatsAppAdapter",
]
