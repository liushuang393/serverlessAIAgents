"""Tests for message gateway."""

import pytest

from agentflow.api.websocket_hub import WebSocketHub
from agentflow.channels.base import MessageChannelAdapter, UserInfo
from agentflow.channels.gateway import MessageGateway
from agentflow.skills.chatbot import ChatBotSkill


class MockAdapter(MessageChannelAdapter):
    """Mock adapter for testing."""

    def __init__(self) -> None:
        """Initialize mock adapter."""
        self.sent_messages: list[dict] = []
        self.typing_indicators: list[str] = []

    @property
    def platform_name(self) -> str:
        """Platform name."""
        return "mock"

    async def send_message(self, channel_id: str, text: str, **kwargs) -> str:  # type: ignore[no-untyped-def]
        """Send message."""
        self.sent_messages.append(
            {
                "channel_id": channel_id,
                "text": text,
                "kwargs": kwargs,
            }
        )
        return f"msg_{len(self.sent_messages)}"

    async def send_typing_indicator(self, channel_id: str) -> None:
        """Send typing indicator."""
        self.typing_indicators.append(channel_id)

    async def get_user_info(self, user_id: str) -> UserInfo:
        """Get user info."""
        return UserInfo(user_id=user_id, display_name=f"User {user_id}")


@pytest.fixture
def gateway() -> MessageGateway:
    """Create gateway fixture."""
    hub = WebSocketHub()
    chatbot = ChatBotSkill()
    return MessageGateway(hub, chatbot)


@pytest.fixture
def mock_adapter() -> MockAdapter:
    """Create mock adapter fixture."""
    return MockAdapter()


def test_gateway_initialization(gateway: MessageGateway) -> None:
    """Test gateway initialization."""
    assert gateway is not None
    assert len(gateway.list_channels()) == 0


def test_register_channel(gateway: MessageGateway, mock_adapter: MockAdapter) -> None:
    """Test channel registration."""
    gateway.register_channel("mock", mock_adapter)

    assert "mock" in gateway.list_channels()
    assert gateway.get_channel("mock") == mock_adapter


def test_unregister_channel(gateway: MessageGateway, mock_adapter: MockAdapter) -> None:
    """Test channel unregistration."""
    gateway.register_channel("mock", mock_adapter)
    assert "mock" in gateway.list_channels()

    result = gateway.unregister_channel("mock")
    assert result is True
    assert "mock" not in gateway.list_channels()

    # Unregister non-existent channel
    result = gateway.unregister_channel("nonexistent")
    assert result is False


@pytest.mark.asyncio
async def test_route_message(gateway: MessageGateway, mock_adapter: MockAdapter) -> None:
    """Test message routing."""
    gateway.register_channel("mock", mock_adapter)

    # Route a message
    response = await gateway.route_message(
        platform="mock",
        user_id="user_123",
        text="Hello, bot!",
    )

    # Check response
    assert isinstance(response, str)
    assert len(response) > 0

    # Check adapter was called
    assert len(mock_adapter.sent_messages) == 1
    assert mock_adapter.sent_messages[0]["channel_id"] == "user_123"
    assert len(mock_adapter.sent_messages[0]["text"]) > 0

    # Check typing indicator was sent
    assert "user_123" in mock_adapter.typing_indicators


@pytest.mark.asyncio
async def test_route_message_platform_not_registered(gateway: MessageGateway) -> None:
    """Test routing message to unregistered platform."""
    with pytest.raises(ValueError, match="Platform not registered"):
        await gateway.route_message(
            platform="nonexistent",
            user_id="user_123",
            text="Hello",
        )


@pytest.mark.asyncio
async def test_session_management(gateway: MessageGateway, mock_adapter: MockAdapter) -> None:
    """Test session management."""
    gateway.register_channel("mock", mock_adapter)

    # First message creates session
    await gateway.route_message("mock", "user_123", "Hello")

    # Check session exists
    session = gateway.get_session_by_user("mock", "user_123")
    assert session is not None
    assert len(session.messages) > 0  # Should have system + user + assistant messages

    # Second message uses same session
    await gateway.route_message("mock", "user_123", "How are you?")

    session = gateway.get_session_by_user("mock", "user_123")
    assert session is not None
    assert len(session.messages) > 2  # More messages


@pytest.mark.asyncio
async def test_clear_user_session(gateway: MessageGateway, mock_adapter: MockAdapter) -> None:
    """Test clearing user session."""
    gateway.register_channel("mock", mock_adapter)

    # Create session
    await gateway.route_message("mock", "user_123", "Hello")
    assert gateway.get_session_by_user("mock", "user_123") is not None

    # Clear session
    result = gateway.clear_user_session("mock", "user_123")
    assert result is True
    assert gateway.get_session_by_user("mock", "user_123") is None

    # Clear non-existent session
    result = gateway.clear_user_session("mock", "user_999")
    assert result is False


@pytest.mark.asyncio
async def test_send_message_to_platform(
    gateway: MessageGateway,
    mock_adapter: MockAdapter,
) -> None:
    """Test sending message directly to platform."""
    gateway.register_channel("mock", mock_adapter)

    message_id = await gateway.send_message_to_platform(
        platform="mock",
        channel_id="ch_123",
        text="Direct message",
    )

    assert message_id.startswith("msg_")
    assert len(mock_adapter.sent_messages) == 1
    assert mock_adapter.sent_messages[0]["text"] == "Direct message"


def test_get_statistics(gateway: MessageGateway, mock_adapter: MockAdapter) -> None:
    """Test getting statistics."""
    gateway.register_channel("mock", mock_adapter)

    stats = gateway.get_statistics()

    assert stats["registered_channels"] == 1
    assert stats["active_sessions"] == 0
    assert "mock" in stats["platforms"]


@pytest.mark.asyncio
async def test_typing_indicator_disabled(
    gateway: MessageGateway, mock_adapter: MockAdapter
) -> None:
    """Test routing with typing indicator disabled."""
    gateway.register_channel("mock", mock_adapter)

    await gateway.route_message(
        platform="mock",
        user_id="user_123",
        text="Hello",
        send_typing=False,
    )

    # No typing indicator should be sent
    assert len(mock_adapter.typing_indicators) == 0
