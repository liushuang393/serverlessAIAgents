"""Tests for shared.channels public exports."""

from __future__ import annotations


def test_shared_channels_exports_message_and_adapter_types() -> None:
    """`from shared.channels import ...` should keep working."""
    from shared.channels import (
        ChannelMessage,
        DiscordAdapter,
        MessageChannelAdapter,
        MessageGateway,
        MessageMetadata,
        MessageType,
        SignalAdapter,
        SlackAdapter,
        TeamsAdapter,
        TelegramAdapter,
        UserInfo,
        WhatsAppAdapter,
    )

    assert MessageChannelAdapter.__name__ == "MessageChannelAdapter"
    assert MessageGateway.__name__ == "MessageGateway"
    assert MessageType.__name__ == "MessageType"
    assert UserInfo.__name__ == "UserInfo"
    assert MessageMetadata.__name__ == "MessageMetadata"
    assert ChannelMessage.__name__ == "ChannelMessage"
    assert TelegramAdapter.__name__ == "TelegramAdapter"
    assert SlackAdapter.__name__ == "SlackAdapter"
    assert TeamsAdapter.__name__ == "TeamsAdapter"
    assert DiscordAdapter.__name__ == "DiscordAdapter"
    assert WhatsAppAdapter.__name__ == "WhatsAppAdapter"
    assert SignalAdapter.__name__ == "SignalAdapter"
