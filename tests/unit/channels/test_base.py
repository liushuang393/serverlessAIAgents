# -*- coding: utf-8 -*-
"""Tests for base channel adapter."""

import pytest

from agentflow.channels.base import (
    ChannelMessage,
    MessageChannelAdapter,
    MessageMetadata,
    MessageType,
    UserInfo,
)


def test_message_type_enum() -> None:
    """Test MessageType enum."""
    assert MessageType.TEXT == "text"
    assert MessageType.IMAGE == "image"
    assert MessageType.AUDIO == "audio"


def test_user_info_creation() -> None:
    """Test UserInfo creation."""
    user = UserInfo(
        user_id="123",
        username="testuser",
        display_name="Test User",
        is_bot=False,
    )

    assert user.user_id == "123"
    assert user.username == "testuser"
    assert user.display_name == "Test User"
    assert user.is_bot is False
    assert user.avatar_url is None


def test_message_metadata_creation() -> None:
    """Test MessageMetadata creation."""
    metadata = MessageMetadata(
        platform="telegram",
        platform_message_id="msg_123",
        thread_id="thread_456",
    )

    assert metadata.platform == "telegram"
    assert metadata.platform_message_id == "msg_123"
    assert metadata.thread_id == "thread_456"
    assert metadata.attachments == []


def test_channel_message_creation() -> None:
    """Test ChannelMessage creation."""
    message = ChannelMessage(
        message_id="msg_001",
        user_id="user_123",
        channel_id="ch_456",
        text="Hello, world!",
        message_type=MessageType.TEXT,
    )

    assert message.message_id == "msg_001"
    assert message.user_id == "user_123"
    assert message.channel_id == "ch_456"
    assert message.text == "Hello, world!"
    assert message.message_type == MessageType.TEXT


class MockAdapter(MessageChannelAdapter):
    """Mock adapter for testing."""

    @property
    def platform_name(self) -> str:
        """Platform name."""
        return "mock"

    async def send_message(self, channel_id: str, text: str, **kwargs) -> str:  # type: ignore[no-untyped-def]
        """Send message."""
        return "msg_123"

    async def send_typing_indicator(self, channel_id: str) -> None:
        """Send typing indicator."""
        pass

    async def get_user_info(self, user_id: str) -> UserInfo:
        """Get user info."""
        return UserInfo(user_id=user_id, display_name="Mock User")


@pytest.mark.asyncio
async def test_adapter_abstract_methods() -> None:
    """Test adapter abstract methods."""
    adapter = MockAdapter()

    assert adapter.platform_name == "mock"

    # Test send_message
    msg_id = await adapter.send_message("ch_123", "Hello")
    assert msg_id == "msg_123"

    # Test get_user_info
    user_info = await adapter.get_user_info("user_456")
    assert user_info.user_id == "user_456"
    assert user_info.display_name == "Mock User"


@pytest.mark.asyncio
async def test_adapter_optional_methods() -> None:
    """Test adapter optional methods raise NotImplementedError."""
    adapter = MockAdapter()

    # send_image not implemented
    with pytest.raises(NotImplementedError):
        await adapter.send_image("ch_123", "http://example.com/image.jpg")

    # send_file not implemented
    with pytest.raises(NotImplementedError):
        await adapter.send_file("ch_123", "http://example.com/file.pdf")

    # delete_message returns False by default
    result = await adapter.delete_message("ch_123", "msg_123")
    assert result is False

    # edit_message returns False by default
    result = await adapter.edit_message("ch_123", "msg_123", "New text")
    assert result is False
