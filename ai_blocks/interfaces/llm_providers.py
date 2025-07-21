"""
LLMプロバイダーインターフェース

このモジュールは、様々なLLMサービス（OpenAI、Anthropic等）との
統一されたインターフェースを提供します。
"""

import asyncio
import time
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any, AsyncGenerator, Dict, List, Optional

from ..config import get_settings
from ..core.models import Message, MessageRole
from ..utils.logging import get_logger

logger = get_logger(__name__)


@dataclass
class LLMResponse:
    """LLM応答のデータクラス"""

    content: str
    usage: Dict[str, int]
    model: str
    finish_reason: str
    response_time: float


class LLMProvider(ABC):
    """LLMプロバイダーの抽象基底クラス"""

    def __init__(self, model: str = None, **kwargs):
        """
        LLMプロバイダーを初期化する

        Args:
            model: 使用するモデル名
            **kwargs: プロバイダー固有の設定
        """
        self.model = model
        self.config = kwargs
        self.settings = get_settings()

    @abstractmethod
    async def generate(
        self,
        messages: List[Message],
        temperature: float = None,
        max_tokens: int = None,
        **kwargs,
    ) -> LLMResponse:
        """
        メッセージリストから応答を生成する

        Args:
            messages: 会話メッセージのリスト
            temperature: 生成温度（0.0-1.0）
            max_tokens: 最大トークン数
            **kwargs: その他のパラメータ

        Returns:
            LLMResponse: 生成された応答
        """
        pass

    @abstractmethod
    async def stream_generate(
        self,
        messages: List[Message],
        temperature: float = None,
        max_tokens: int = None,
        **kwargs,
    ) -> AsyncGenerator[str, None]:
        """
        ストリーミング形式で応答を生成する

        Args:
            messages: 会話メッセージのリスト
            temperature: 生成温度（0.0-1.0）
            max_tokens: 最大トークン数
            **kwargs: その他のパラメータ

        Yields:
            str: 生成されたテキストの断片
        """
        pass

    @abstractmethod
    def count_tokens(self, text: str) -> int:
        """
        テキストのトークン数をカウントする

        Args:
            text: カウント対象のテキスト

        Returns:
            int: トークン数
        """
        pass

    def _format_messages(self, messages: List[Message]) -> List[Dict[str, Any]]:
        """メッセージをプロバイダー固有の形式に変換する"""
        return [{"role": msg.role.value, "content": msg.content} for msg in messages]


class OpenAIProvider(LLMProvider):
    """OpenAI APIプロバイダー"""

    def __init__(self, api_key: str = None, model: str = None, **kwargs):
        """
        OpenAIプロバイダーを初期化する

        Args:
            api_key: OpenAI APIキー
            model: 使用するモデル名
            **kwargs: その他の設定
        """
        super().__init__(model or "gpt-3.5-turbo", **kwargs)

        self.api_key = api_key or self.settings.openai_api_key
        if not self.api_key:
            raise ValueError("OpenAI APIキーが設定されていません")

        # OpenAIクライアントの初期化は実際の使用時に行う
        self._client = None

        logger.info(f"OpenAIProvider を初期化しました (model: {self.model})")

    def _get_client(self):
        """OpenAIクライアントを取得する（遅延初期化）"""
        if self._client is None:
            try:
                import openai

                self._client = openai.AsyncOpenAI(api_key=self.api_key)
            except ImportError:
                raise ImportError("openaiパッケージがインストールされていません: pip install openai")
        return self._client

    async def generate(
        self,
        messages: List[Message],
        temperature: float = None,
        max_tokens: int = None,
        **kwargs,
    ) -> LLMResponse:
        """OpenAI APIを使用して応答を生成する"""
        client = self._get_client()

        start_time = time.time()

        try:
            response = await client.chat.completions.create(
                model=self.model,
                messages=self._format_messages(messages),
                temperature=temperature or self.settings.temperature,
                max_tokens=max_tokens or self.settings.max_tokens,
                **kwargs,
            )

            response_time = time.time() - start_time

            return LLMResponse(
                content=response.choices[0].message.content,
                usage={
                    "prompt_tokens": response.usage.prompt_tokens,
                    "completion_tokens": response.usage.completion_tokens,
                    "total_tokens": response.usage.total_tokens,
                },
                model=response.model,
                finish_reason=response.choices[0].finish_reason,
                response_time=response_time,
            )

        except Exception as e:
            logger.error(f"OpenAI API呼び出しエラー: {e}")
            raise

    async def stream_generate(
        self,
        messages: List[Message],
        temperature: float = None,
        max_tokens: int = None,
        **kwargs,
    ) -> AsyncGenerator[str, None]:
        """OpenAI APIを使用してストリーミング応答を生成する"""
        client = self._get_client()

        try:
            stream = await client.chat.completions.create(
                model=self.model,
                messages=self._format_messages(messages),
                temperature=temperature or self.settings.temperature,
                max_tokens=max_tokens or self.settings.max_tokens,
                stream=True,
                **kwargs,
            )

            async for chunk in stream:
                if chunk.choices[0].delta.content:
                    yield chunk.choices[0].delta.content

        except Exception as e:
            logger.error(f"OpenAI ストリーミングエラー: {e}")
            raise

    def count_tokens(self, text: str) -> int:
        """tiktoken を使用してトークン数をカウントする"""
        try:
            import tiktoken

            encoding = tiktoken.encoding_for_model(self.model)
            return len(encoding.encode(text))
        except ImportError:
            logger.warning("tiktokenがインストールされていません。概算値を返します")
            # 概算: 1トークン ≈ 4文字
            return len(text) // 4


class AnthropicProvider(LLMProvider):
    """Anthropic Claude APIプロバイダー"""

    def __init__(self, api_key: str = None, model: str = None, **kwargs):
        """
        Anthropicプロバイダーを初期化する

        Args:
            api_key: Anthropic APIキー
            model: 使用するモデル名
            **kwargs: その他の設定
        """
        super().__init__(model or "claude-3-sonnet-20240229", **kwargs)

        self.api_key = api_key or self.settings.anthropic_api_key
        if not self.api_key:
            raise ValueError("Anthropic APIキーが設定されていません")

        self._client = None

        logger.info(f"AnthropicProvider を初期化しました (model: {self.model})")

    def _get_client(self):
        """Anthropicクライアントを取得する（遅延初期化）"""
        if self._client is None:
            try:
                import anthropic

                self._client = anthropic.AsyncAnthropic(api_key=self.api_key)
            except ImportError:
                raise ImportError("anthropicパッケージがインストールされていません: pip install anthropic")
        return self._client

    async def generate(
        self,
        messages: List[Message],
        temperature: float = None,
        max_tokens: int = None,
        **kwargs,
    ) -> LLMResponse:
        """Anthropic APIを使用して応答を生成する"""
        client = self._get_client()

        start_time = time.time()

        try:
            # Anthropic APIの形式に変換
            system_message = None
            formatted_messages = []

            for msg in messages:
                if msg.role == MessageRole.SYSTEM:
                    system_message = msg.content
                else:
                    formatted_messages.append(
                        {
                            "role": "user"
                            if msg.role == MessageRole.USER
                            else "assistant",
                            "content": msg.content,
                        }
                    )

            response = await client.messages.create(
                model=self.model,
                messages=formatted_messages,
                system=system_message,
                temperature=temperature or self.settings.temperature,
                max_tokens=max_tokens or self.settings.max_tokens,
                **kwargs,
            )

            response_time = time.time() - start_time

            return LLMResponse(
                content=response.content[0].text,
                usage={
                    "prompt_tokens": response.usage.input_tokens,
                    "completion_tokens": response.usage.output_tokens,
                    "total_tokens": response.usage.input_tokens
                    + response.usage.output_tokens,
                },
                model=response.model,
                finish_reason=response.stop_reason,
                response_time=response_time,
            )

        except Exception as e:
            logger.error(f"Anthropic API呼び出しエラー: {e}")
            raise

    async def stream_generate(
        self,
        messages: List[Message],
        temperature: float = None,
        max_tokens: int = None,
        **kwargs,
    ) -> AsyncGenerator[str, None]:
        """Anthropic APIを使用してストリーミング応答を生成する"""
        client = self._get_client()

        try:
            # メッセージ形式の変換（generateメソッドと同様）
            system_message = None
            formatted_messages = []

            for msg in messages:
                if msg.role == MessageRole.SYSTEM:
                    system_message = msg.content
                else:
                    formatted_messages.append(
                        {
                            "role": "user"
                            if msg.role == MessageRole.USER
                            else "assistant",
                            "content": msg.content,
                        }
                    )

            async with client.messages.stream(
                model=self.model,
                messages=formatted_messages,
                system=system_message,
                temperature=temperature or self.settings.temperature,
                max_tokens=max_tokens or self.settings.max_tokens,
                **kwargs,
            ) as stream:
                async for text in stream.text_stream:
                    yield text

        except Exception as e:
            logger.error(f"Anthropic ストリーミングエラー: {e}")
            raise

    def count_tokens(self, text: str) -> int:
        """Anthropicのトークン数をカウントする（概算）"""
        # Anthropicの正確なトークナイザーが利用できない場合の概算
        logger.warning("Anthropicの正確なトークンカウントは未実装です。概算値を返します")
        # 概算: 1トークン ≈ 4文字
        return len(text) // 4
