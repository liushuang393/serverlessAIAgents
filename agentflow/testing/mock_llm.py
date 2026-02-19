"""Mock LLM Provider モジュール.

テスト用の LLM プロバイダーモックを提供します。

特徴:
- 固定レスポンス
- パターンマッチング
- ストリームレスポンス
- 呼び出し記録
"""

from __future__ import annotations

import asyncio
import logging
import re
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Callable


logger = logging.getLogger(__name__)


@dataclass
class MockResponse:
    """モックレスポンス.

    Attributes:
        content: レスポンス内容
        model: モデル名
        usage: トークン使用量
        metadata: メタデータ
    """

    content: str
    model: str = "mock-model"
    usage: dict[str, int] = field(
        default_factory=lambda: {"prompt_tokens": 10, "completion_tokens": 20}
    )
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "content": self.content,
            "model": self.model,
            "usage": self.usage,
            **self.metadata,
        }


@dataclass
class MockStreamResponse:
    """モックストリームレスポンス.

    Attributes:
        chunks: チャンクリスト
        delay: チャンク間の遅延（秒）
    """

    chunks: list[str]
    delay: float = 0.01


@dataclass
class CallRecord:
    """呼び出し記録.

    Attributes:
        messages: メッセージリスト
        kwargs: 追加引数
        response: レスポンス
    """

    messages: list[dict[str, Any]]
    kwargs: dict[str, Any]
    response: MockResponse | None = None


class MockLLMProvider:
    """Mock LLM Provider.

    テスト用の LLM プロバイダーモック。

    Example:
        >>> mock = MockLLMProvider()
        >>> mock.set_response("Hello, I'm an AI assistant!")
        >>> response = await mock.chat([{"role": "user", "content": "Hi"}])
        >>> assert response["content"] == "Hello, I'm an AI assistant!"
    """

    def __init__(self) -> None:
        """初期化."""
        self._default_response = MockResponse(content="Mock response")
        self._pattern_responses: list[tuple[str, MockResponse]] = []
        self._sequence_responses: list[MockResponse] = []
        self._sequence_index = 0
        self._call_records: list[CallRecord] = []
        self._response_callback: Callable[[list[dict[str, Any]]], MockResponse] | None = None
        self._logger = logging.getLogger(__name__)

    def set_response(self, content: str, **kwargs: Any) -> None:
        """デフォルトレスポンスを設定.

        Args:
            content: レスポンス内容
            **kwargs: MockResponse への追加引数
        """
        self._default_response = MockResponse(content=content, **kwargs)

    def add_pattern_response(self, pattern: str, content: str, **kwargs: Any) -> None:
        """パターンマッチングレスポンスを追加.

        Args:
            pattern: 正規表現パターン
            content: レスポンス内容
            **kwargs: MockResponse への追加引数
        """
        self._pattern_responses.append((pattern, MockResponse(content=content, **kwargs)))

    def add_sequence_response(self, content: str, **kwargs: Any) -> None:
        """シーケンスレスポンスを追加.

        Args:
            content: レスポンス内容
            **kwargs: MockResponse への追加引数
        """
        self._sequence_responses.append(MockResponse(content=content, **kwargs))

    def set_response_callback(
        self,
        callback: Callable[[list[dict[str, Any]]], MockResponse],
    ) -> None:
        """レスポンスコールバックを設定.

        Args:
            callback: メッセージを受け取りレスポンスを返す関数
        """
        self._response_callback = callback

    async def chat(
        self,
        messages: list[dict[str, Any]],
        **kwargs: Any,
    ) -> dict[str, Any]:
        """チャット API（モック）.

        Args:
            messages: メッセージリスト
            **kwargs: 追加引数

        Returns:
            レスポンス辞書
        """
        # レスポンスを決定
        response = self._get_response(messages)

        # 呼び出しを記録
        self._call_records.append(CallRecord(messages=messages, kwargs=kwargs, response=response))

        return response.to_dict()

    async def stream(
        self,
        messages: list[dict[str, Any]],
        **kwargs: Any,
    ) -> AsyncIterator[str]:
        """ストリーム API（モック）.

        Args:
            messages: メッセージリスト
            **kwargs: 追加引数

        Yields:
            レスポンスチャンク
        """
        response = self._get_response(messages)

        # 呼び出しを記録
        self._call_records.append(CallRecord(messages=messages, kwargs=kwargs, response=response))

        # チャンクに分割
        content = response.content
        chunk_size = max(1, len(content) // 5)
        chunks = [content[i : i + chunk_size] for i in range(0, len(content), chunk_size)]

        for chunk in chunks:
            await asyncio.sleep(0.001)  # 小さな遅延
            yield chunk

    def _get_response(self, messages: list[dict[str, Any]]) -> MockResponse:
        """レスポンスを決定.

        Args:
            messages: メッセージリスト

        Returns:
            MockResponse
        """
        # コールバックがあれば使用
        if self._response_callback:
            return self._response_callback(messages)

        # シーケンスレスポンスがあれば使用
        if self._sequence_responses:
            response = self._sequence_responses[
                self._sequence_index % len(self._sequence_responses)
            ]
            self._sequence_index += 1
            return response

        # パターンマッチング
        last_message = messages[-1].get("content", "") if messages else ""
        for pattern, response in self._pattern_responses:
            if re.search(pattern, last_message, re.IGNORECASE):
                return response

        # デフォルトレスポンス
        return self._default_response

    def get_call_count(self) -> int:
        """呼び出し回数を取得.

        Returns:
            呼び出し回数
        """
        return len(self._call_records)

    def get_calls(self) -> list[CallRecord]:
        """呼び出し記録を取得.

        Returns:
            CallRecord のリスト
        """
        return self._call_records.copy()

    def get_last_call(self) -> CallRecord | None:
        """最後の呼び出しを取得.

        Returns:
            CallRecord、または None
        """
        return self._call_records[-1] if self._call_records else None

    def assert_called(self) -> None:
        """呼び出されたことを検証.

        Raises:
            AssertionError: 呼び出されていない場合
        """
        if not self._call_records:
            msg = "MockLLM was not called"
            raise AssertionError(msg)

    def assert_called_with(self, content: str) -> None:
        """特定の内容で呼び出されたことを検証.

        Args:
            content: 期待するメッセージ内容

        Raises:
            AssertionError: 期待通りに呼び出されていない場合
        """
        for record in self._call_records:
            for message in record.messages:
                if content in message.get("content", ""):
                    return

        msg = f"MockLLM was not called with content: {content}"
        raise AssertionError(msg)

    def reset(self) -> None:
        """状態をリセット."""
        self._call_records.clear()
        self._sequence_index = 0
        self._logger.debug("MockLLM reset")


def create_mock_llm(
    default_response: str = "Mock response",
    **kwargs: Any,
) -> MockLLMProvider:
    """Mock LLM Provider を作成.

    Args:
        default_response: デフォルトレスポンス
        **kwargs: MockResponse への追加引数

    Returns:
        MockLLMProvider インスタンス
    """
    mock = MockLLMProvider()
    mock.set_response(default_response, **kwargs)
    return mock
