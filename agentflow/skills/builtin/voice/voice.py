# -*- coding: utf-8 -*-
"""Voice Skill - 音声認識・合成機能.

OpenAI Whisper / TTS を使用した音声処理スキル。

Example:
    >>> from agentflow.skills import VoiceSkill
    >>>
    >>> # スキル作成
    >>> voice = VoiceSkill()
    >>>
    >>> # 音声認識（Speech-to-Text）
    >>> text = await voice.transcribe(audio_path="recording.mp3")
    >>> print(text)
    >>>
    >>> # 音声合成（Text-to-Speech）
    >>> audio_data = await voice.synthesize("こんにちは、世界！")
    >>> with open("output.mp3", "wb") as f:
    ...     f.write(audio_data)
"""

from __future__ import annotations

import base64
import logging
import os
import tempfile
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any

import httpx


logger = logging.getLogger(__name__)


class VoiceProvider(str, Enum):
    """音声プロバイダー."""

    OPENAI = "openai"  # Whisper + TTS
    GOOGLE = "google"  # Google Cloud Speech
    AZURE = "azure"  # Azure Cognitive Services
    AUTO = "auto"  # 自動検出


class TTSVoice(str, Enum):
    """TTS 音声タイプ（OpenAI）."""

    ALLOY = "alloy"
    ECHO = "echo"
    FABLE = "fable"
    ONYX = "onyx"
    NOVA = "nova"
    SHIMMER = "shimmer"


@dataclass
class VoiceConfig:
    """Voice スキル設定.

    Attributes:
        provider: 音声プロバイダー
        stt_model: 音声認識モデル
        tts_model: 音声合成モデル
        tts_voice: TTS 音声タイプ
        language: デフォルト言語
        timeout: タイムアウト（秒）
    """

    provider: VoiceProvider = VoiceProvider.AUTO
    stt_model: str = "whisper-1"
    tts_model: str = "tts-1"
    tts_voice: TTSVoice = TTSVoice.NOVA
    language: str | None = None  # None = 自動検出
    timeout: int = 120


class VoiceSkill:
    """音声認識・合成スキル.

    OpenAI Whisper / TTS を使用した音声処理機能を提供。

    Features:
    - 音声認識（Speech-to-Text）
    - 音声合成（Text-to-Speech）
    - 音声翻訳
    - リアルタイムストリーミング（将来対応）
    """

    def __init__(self, config: VoiceConfig | None = None) -> None:
        """Voice スキルを初期化.

        Args:
            config: Voice 設定
        """
        self._config = config or VoiceConfig()
        self._logger = logging.getLogger("voice_skill")
        self._http_client = httpx.AsyncClient(timeout=self._config.timeout)

        # API キーを取得
        self._api_key = os.getenv("OPENAI_API_KEY", "")
        if not self._api_key:
            self._logger.warning("OPENAI_API_KEY not set, voice features may not work")

    async def transcribe(
        self,
        audio_path: str | Path | None = None,
        audio_data: bytes | None = None,
        audio_base64: str | None = None,
        *,
        language: str | None = None,
        prompt: str | None = None,
        response_format: str = "text",
    ) -> str:
        """音声をテキストに変換（Speech-to-Text）.

        Args:
            audio_path: 音声ファイルパス
            audio_data: 音声バイナリデータ
            audio_base64: Base64 エンコード音声
            language: 言語コード（ISO-639-1、例: ja, en, zh）
            prompt: 認識ヒント（専門用語等）
            response_format: 出力形式（text, json, srt, vtt）

        Returns:
            認識されたテキスト

        Raises:
            ValueError: 音声ソースが指定されていない場合
        """
        # 音声データを準備
        audio_bytes = await self._prepare_audio(audio_path, audio_data, audio_base64)

        # OpenAI Whisper API を呼び出し
        url = "https://api.openai.com/v1/audio/transcriptions"
        headers = {"Authorization": f"Bearer {self._api_key}"}

        # multipart/form-data で送信
        files = {"file": ("audio.mp3", audio_bytes, "audio/mpeg")}
        data: dict[str, Any] = {
            "model": self._config.stt_model,
            "response_format": response_format,
        }

        if language or self._config.language:
            data["language"] = language or self._config.language
        if prompt:
            data["prompt"] = prompt

        try:
            response = await self._http_client.post(
                url, headers=headers, files=files, data=data
            )
            response.raise_for_status()

            if response_format == "text":
                return response.text
            else:
                return response.json().get("text", "")

        except httpx.HTTPStatusError as e:
            self._logger.error(f"Whisper API error: {e.response.text}")
            raise
        except Exception as e:
            self._logger.error(f"Transcription failed: {e}", exc_info=True)
            raise

    async def synthesize(
        self,
        text: str,
        *,
        voice: TTSVoice | str | None = None,
        speed: float = 1.0,
        output_format: str = "mp3",
    ) -> bytes:
        """テキストを音声に変換（Text-to-Speech）.

        Args:
            text: 変換するテキスト
            voice: 音声タイプ
            speed: 再生速度（0.25 - 4.0）
            output_format: 出力形式（mp3, opus, aac, flac）

        Returns:
            音声バイナリデータ
        """
        url = "https://api.openai.com/v1/audio/speech"
        headers = {
            "Authorization": f"Bearer {self._api_key}",
            "Content-Type": "application/json",
        }

        voice_name = voice if isinstance(voice, str) else (voice or self._config.tts_voice).value

        payload = {
            "model": self._config.tts_model,
            "input": text,
            "voice": voice_name,
            "speed": max(0.25, min(4.0, speed)),
            "response_format": output_format,
        }

        try:
            response = await self._http_client.post(url, headers=headers, json=payload)
            response.raise_for_status()
            return response.content

        except httpx.HTTPStatusError as e:
            self._logger.error(f"TTS API error: {e.response.text}")
            raise
        except Exception as e:
            self._logger.error(f"Speech synthesis failed: {e}", exc_info=True)
            raise

    async def translate(
        self,
        audio_path: str | Path | None = None,
        audio_data: bytes | None = None,
        audio_base64: str | None = None,
        *,
        prompt: str | None = None,
    ) -> str:
        """音声を英語に翻訳.

        Args:
            audio_path: 音声ファイルパス
            audio_data: 音声バイナリデータ
            audio_base64: Base64 エンコード音声
            prompt: 認識ヒント

        Returns:
            英語に翻訳されたテキスト
        """
        audio_bytes = await self._prepare_audio(audio_path, audio_data, audio_base64)

        url = "https://api.openai.com/v1/audio/translations"
        headers = {"Authorization": f"Bearer {self._api_key}"}

        files = {"file": ("audio.mp3", audio_bytes, "audio/mpeg")}
        data: dict[str, Any] = {"model": self._config.stt_model}

        if prompt:
            data["prompt"] = prompt

        try:
            response = await self._http_client.post(
                url, headers=headers, files=files, data=data
            )
            response.raise_for_status()
            return response.json().get("text", "")

        except Exception as e:
            self._logger.error(f"Translation failed: {e}", exc_info=True)
            raise

    async def synthesize_to_file(
        self,
        text: str,
        output_path: str | Path,
        *,
        voice: TTSVoice | str | None = None,
        speed: float = 1.0,
    ) -> Path:
        """テキストを音声ファイルに変換.

        Args:
            text: 変換するテキスト
            output_path: 出力ファイルパス
            voice: 音声タイプ
            speed: 再生速度

        Returns:
            出力ファイルパス
        """
        output_path = Path(output_path)
        output_format = output_path.suffix.lstrip(".") or "mp3"

        audio_data = await self.synthesize(
            text, voice=voice, speed=speed, output_format=output_format
        )

        with open(output_path, "wb") as f:
            f.write(audio_data)

        return output_path

    async def transcribe_and_respond(
        self,
        audio_path: str | Path | None = None,
        audio_data: bytes | None = None,
        audio_base64: str | None = None,
        *,
        response_generator: Any = None,
        voice: TTSVoice | str | None = None,
    ) -> tuple[str, bytes]:
        """音声を認識し、応答を音声で返す.

        Args:
            audio_path: 入力音声ファイルパス
            audio_data: 入力音声バイナリデータ
            audio_base64: 入力 Base64 エンコード音声
            response_generator: 応答生成関数（テキスト -> テキスト）
            voice: 出力音声タイプ

        Returns:
            (認識テキスト, 応答音声データ) のタプル
        """
        # 音声認識
        input_text = await self.transcribe(
            audio_path=audio_path,
            audio_data=audio_data,
            audio_base64=audio_base64,
        )

        # 応答生成
        if response_generator:
            if callable(response_generator):
                response_text = await response_generator(input_text)
            else:
                response_text = str(response_generator)
        else:
            response_text = input_text  # エコーバック

        # 音声合成
        output_audio = await self.synthesize(response_text, voice=voice)

        return input_text, output_audio

    async def _prepare_audio(
        self,
        audio_path: str | Path | None,
        audio_data: bytes | None,
        audio_base64: str | None,
    ) -> bytes:
        """音声データを準備.

        Args:
            audio_path: 音声ファイルパス
            audio_data: 音声バイナリデータ
            audio_base64: Base64 エンコード音声

        Returns:
            音声バイナリデータ

        Raises:
            ValueError: 音声ソースが指定されていない場合
        """
        if audio_data:
            return audio_data

        if audio_path:
            path = Path(audio_path)
            if not path.exists():
                msg = f"Audio file not found: {path}"
                raise ValueError(msg)
            with open(path, "rb") as f:
                return f.read()

        if audio_base64:
            return base64.b64decode(audio_base64)

        msg = "No audio source provided. Specify audio_path, audio_data, or audio_base64."
        raise ValueError(msg)

    async def get_supported_formats(self) -> dict[str, list[str]]:
        """サポートされる音声形式を取得.

        Returns:
            入力/出力形式のリスト
        """
        return {
            "input": ["mp3", "mp4", "mpeg", "mpga", "m4a", "wav", "webm"],
            "output": ["mp3", "opus", "aac", "flac"],
        }

    async def close(self) -> None:
        """HTTP クライアントをクローズ."""
        await self._http_client.aclose()

