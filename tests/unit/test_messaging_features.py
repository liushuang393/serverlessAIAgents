# -*- coding: utf-8 -*-
"""新機能のユニットテスト - Messaging Hub 追加機能.

テスト対象：
- Teams アダプター
- WhatsApp アダプター
- Signal アダプター
- Vision スキル
- Voice スキル
- 会話エクスポートスキル
"""

from __future__ import annotations

from datetime import datetime
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.channels.signal import SignalAdapter
from agentflow.channels.teams import TeamsAdapter
from agentflow.channels.whatsapp import WhatsAppAdapter
from agentflow.skills.conversation_export import (
    ConversationExportSkill,
    ExportConfig,
    ExportFormat,
    ExportMessage,
)
from agentflow.skills.vision import VisionConfig, VisionProvider, VisionResult, VisionSkill
from agentflow.skills.voice import TTSVoice, VoiceConfig, VoiceProvider, VoiceSkill


# =============================================================================
# Teams Adapter Tests
# =============================================================================

# Teams アダプターはオプショナルな依存関係（botbuilder-core）が必要
try:
    # botbuilder がインストールされているか確認
    import botbuilder.core  # noqa: F401

    BOTBUILDER_AVAILABLE = True
except ImportError:
    BOTBUILDER_AVAILABLE = False


@pytest.mark.skipif(not BOTBUILDER_AVAILABLE, reason="botbuilder-core not installed")
class TestTeamsAdapter:
    """Microsoft Teams アダプターのテスト."""

    def test_platform_name(self) -> None:
        """プラットフォーム名の確認."""
        adapter = TeamsAdapter(app_id="test", app_password="test")
        assert adapter.platform_name == "teams"

    @pytest.mark.asyncio
    async def test_send_message(self) -> None:
        """メッセージ送信テスト."""
        adapter = TeamsAdapter(app_id="test", app_password="test")

        # conversation_references がない場合
        result = await adapter.send_message("channel_123", "Hello")
        assert result == ""  # 参照がない場合は空文字列

    @pytest.mark.asyncio
    async def test_send_typing_indicator(self) -> None:
        """入力インジケーターテスト."""
        adapter = TeamsAdapter(app_id="test", app_password="test")
        # 例外が発生しないことを確認
        await adapter.send_typing_indicator("channel_123")


# =============================================================================
# WhatsApp Adapter Tests
# =============================================================================


class TestWhatsAppAdapter:
    """WhatsApp アダプターのテスト."""

    def test_platform_name(self) -> None:
        """プラットフォーム名の確認."""
        adapter = WhatsAppAdapter(phone_number_id="123", access_token="token")
        assert adapter.platform_name == "whatsapp"

    @pytest.mark.asyncio
    async def test_send_typing_indicator(self) -> None:
        """入力インジケーターテスト（WhatsApp は未サポート）."""
        adapter = WhatsAppAdapter(phone_number_id="123", access_token="token")
        # 何も起こらないことを確認
        await adapter.send_typing_indicator("channel_123")


# =============================================================================
# Signal Adapter Tests
# =============================================================================


class TestSignalAdapter:
    """Signal アダプターのテスト."""

    def test_platform_name(self) -> None:
        """プラットフォーム名の確認."""
        adapter = SignalAdapter(api_url="http://localhost:8080", phone_number="+81901234567")
        assert adapter.platform_name == "signal"

    @pytest.mark.asyncio
    async def test_get_user_info_fallback(self) -> None:
        """ユーザー情報取得（フォールバック）."""
        adapter = SignalAdapter(api_url="http://localhost:8080", phone_number="+81901234567")

        with patch.object(adapter._client, "get", new_callable=AsyncMock) as mock_get:
            mock_get.return_value = MagicMock(status_code=404)
            user_info = await adapter.get_user_info("+81901234568")

            assert user_info.user_id == "+81901234568"
            assert user_info.display_name == "++81901234568"


# =============================================================================
# Vision Skill Tests
# =============================================================================


class TestVisionSkill:
    """Vision スキルのテスト."""

    def test_config_defaults(self) -> None:
        """デフォルト設定の確認."""
        config = VisionConfig()
        assert config.provider == VisionProvider.AUTO
        assert config.max_tokens == 1024
        assert config.detail == "auto"

    def test_result_dataclass(self) -> None:
        """VisionResult データクラスの確認."""
        result = VisionResult(
            description="A cat sitting on a table",
            objects=["cat", "table"],
            text="",
            confidence=0.95,
        )
        assert result.description == "A cat sitting on a table"
        assert "cat" in result.objects

    @pytest.mark.asyncio
    async def test_prepare_image_url(self) -> None:
        """URL 画像の準備テスト."""
        skill = VisionSkill()
        result = await skill._prepare_image("https://example.com/image.jpg", None, None)
        assert result["type"] == "url"
        assert result["url"] == "https://example.com/image.jpg"

    @pytest.mark.asyncio
    async def test_prepare_image_base64(self) -> None:
        """Base64 画像の準備テスト."""
        skill = VisionSkill()
        result = await skill._prepare_image(None, None, "base64data")
        assert result["type"] == "base64"
        assert result["data"] == "base64data"

    @pytest.mark.asyncio
    async def test_prepare_image_no_source(self) -> None:
        """画像ソースなしのエラーテスト."""
        skill = VisionSkill()
        with pytest.raises(ValueError, match="No image source provided"):
            await skill._prepare_image(None, None, None)


# =============================================================================
# Voice Skill Tests
# =============================================================================


class TestVoiceSkill:
    """Voice スキルのテスト."""

    def test_config_defaults(self) -> None:
        """デフォルト設定の確認."""
        config = VoiceConfig()
        assert config.provider == VoiceProvider.AUTO
        assert config.stt_model == "whisper-1"
        assert config.tts_model == "tts-1"
        assert config.tts_voice == TTSVoice.NOVA

    def test_tts_voices(self) -> None:
        """TTS 音声タイプの確認."""
        assert TTSVoice.ALLOY.value == "alloy"
        assert TTSVoice.NOVA.value == "nova"
        assert TTSVoice.SHIMMER.value == "shimmer"

    @pytest.mark.asyncio
    async def test_prepare_audio_bytes(self) -> None:
        """バイトデータの準備テスト."""
        skill = VoiceSkill()
        audio_data = b"test audio data"
        result = await skill._prepare_audio(None, audio_data, None)
        assert result == audio_data

    @pytest.mark.asyncio
    async def test_prepare_audio_base64(self) -> None:
        """Base64 データの準備テスト."""
        import base64

        skill = VoiceSkill()
        original = b"test audio data"
        encoded = base64.b64encode(original).decode()
        result = await skill._prepare_audio(None, None, encoded)
        assert result == original

    @pytest.mark.asyncio
    async def test_prepare_audio_no_source(self) -> None:
        """音声ソースなしのエラーテスト."""
        skill = VoiceSkill()
        with pytest.raises(ValueError, match="No audio source provided"):
            await skill._prepare_audio(None, None, None)

    @pytest.mark.asyncio
    async def test_get_supported_formats(self) -> None:
        """サポート形式の取得テスト."""
        skill = VoiceSkill()
        formats = await skill.get_supported_formats()
        assert "mp3" in formats["input"]
        assert "mp3" in formats["output"]
        assert "opus" in formats["output"]


# =============================================================================
# Conversation Export Skill Tests
# =============================================================================


class TestConversationExportSkill:
    """会話エクスポートスキルのテスト."""

    def test_export_format_enum(self) -> None:
        """エクスポート形式 enum の確認."""
        assert ExportFormat.JSON.value == "json"
        assert ExportFormat.CSV.value == "csv"
        assert ExportFormat.MARKDOWN.value == "markdown"

    def test_export_config_defaults(self) -> None:
        """デフォルト設定の確認."""
        config = ExportConfig()
        assert config.include_metadata is True
        assert config.include_timestamps is True
        assert config.timezone == "UTC"
        assert config.max_messages is None

    def test_export_message_dataclass(self) -> None:
        """ExportMessage データクラスの確認."""
        msg = ExportMessage(
            timestamp="2025-01-01T00:00:00",
            platform="telegram",
            user_id="user123",
            user_name="Test User",
            role="user",
            content="Hello!",
        )
        assert msg.platform == "telegram"
        assert msg.content == "Hello!"

    @pytest.mark.asyncio
    async def test_export_json(self) -> None:
        """JSON エクスポートテスト."""
        skill = ConversationExportSkill()
        messages = [
            ExportMessage(
                timestamp="2025-01-01T10:00:00",
                platform="telegram",
                user_id="user1",
                user_name="User 1",
                role="user",
                content="Hello",
            ),
            ExportMessage(
                timestamp="2025-01-01T10:00:01",
                platform="telegram",
                user_id="bot",
                user_name="Bot",
                role="assistant",
                content="Hi there!",
            ),
        ]

        result = await skill.export_json(messages)
        assert '"total_messages": 2' in result
        assert '"Hello"' in result
        assert '"Hi there!"' in result

    @pytest.mark.asyncio
    async def test_export_csv(self) -> None:
        """CSV エクスポートテスト."""
        skill = ConversationExportSkill()
        messages = [
            ExportMessage(
                timestamp="2025-01-01T10:00:00",
                platform="slack",
                user_id="user1",
                user_name="User 1",
                role="user",
                content="Test message",
            ),
        ]

        result = await skill.export_csv(messages)
        assert "timestamp,platform,user_id,user_name,role,content" in result
        assert "slack" in result
        assert "Test message" in result

    @pytest.mark.asyncio
    async def test_export_markdown(self) -> None:
        """Markdown エクスポートテスト."""
        skill = ConversationExportSkill()
        messages = [
            ExportMessage(
                timestamp="2025-01-01T10:00:00",
                platform="discord",
                user_id="user1",
                user_name="User 1",
                role="user",
                content="Markdown test",
            ),
        ]

        result = await skill.export_markdown(messages)
        assert "# 会話履歴" in result
        assert "👤 User 1" in result
        assert "Markdown test" in result

    @pytest.mark.asyncio
    async def test_normalize_dict_messages(self) -> None:
        """dict メッセージの正規化テスト."""
        skill = ConversationExportSkill()
        messages = [
            {
                "timestamp": "2025-01-01T10:00:00",
                "platform": "telegram",
                "user_id": "user1",
                "content": "Dict message",
            },
        ]

        normalized = skill._normalize_messages(messages)
        assert len(normalized) == 1
        assert normalized[0].content == "Dict message"
        assert normalized[0].role == "user"  # デフォルト値

    @pytest.mark.asyncio
    async def test_filter_messages_by_platform(self) -> None:
        """プラットフォームフィルタテスト."""
        skill = ConversationExportSkill()
        messages = [
            ExportMessage(
                timestamp="2025-01-01T10:00:00",
                platform="telegram",
                user_id="u1",
                user_name="U1",
                role="user",
                content="Telegram",
            ),
            ExportMessage(
                timestamp="2025-01-01T10:00:00",
                platform="slack",
                user_id="u2",
                user_name="U2",
                role="user",
                content="Slack",
            ),
        ]

        filtered = await skill.filter_messages(messages, platforms=["telegram"])
        assert len(filtered) == 1
        assert filtered[0].platform == "telegram"

    @pytest.mark.asyncio
    async def test_get_statistics(self) -> None:
        """統計情報取得テスト."""
        skill = ConversationExportSkill()
        messages = [
            ExportMessage(
                timestamp="2025-01-01T10:00:00",
                platform="telegram",
                user_id="u1",
                user_name="U1",
                role="user",
                content="Msg 1",
            ),
            ExportMessage(
                timestamp="2025-01-01T10:00:01",
                platform="telegram",
                user_id="u1",
                user_name="U1",
                role="user",
                content="Msg 2",
            ),
            ExportMessage(
                timestamp="2025-01-01T10:00:02",
                platform="slack",
                user_id="u2",
                user_name="U2",
                role="assistant",
                content="Msg 3",
            ),
        ]

        stats = await skill.get_statistics(messages)
        assert stats["total_messages"] == 3
        assert stats["platforms"]["telegram"] == 2
        assert stats["platforms"]["slack"] == 1
        assert stats["users"]["u1"] == 2
        assert stats["roles"]["user"] == 2
        assert stats["roles"]["assistant"] == 1

