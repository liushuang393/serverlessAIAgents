# -*- coding: utf-8 -*-
"""LLM統合クライアント.

このモジュールは、各種LLMプロバイダーとの統一インターフェースを提供します。

サポートされるプロバイダー:
- OpenAI (GPT-4o, o1, o3-mini, etc.)
- Anthropic (Claude 3.5/4)
- Google (Gemini 2.0)
- Ollama (ローカルモデル)
- LocalAI (ローカルモデル - デフォルト)

環境変数:
    OPENAI_API_KEY: OpenAI API キー
    ANTHROPIC_API_KEY: Anthropic API キー
    GOOGLE_API_KEY: Google AI Studio API キー
    OLLAMA_BASE_URL: Ollama サーバー URL（デフォルト: http://localhost:11434）
    LOCALAI_BASE_URL: LocalAI サーバー URL（デフォルト: http://localhost:8080）
"""

import asyncio
import logging
import os
from typing import Any, AsyncIterator

import httpx
from pydantic import BaseModel, Field


class LLMMessage(BaseModel):
    """LLMメッセージ.

    Args:
        role: メッセージの役割（system/user/assistant/tool）
        content: メッセージ内容
        tool_call_id: ツール呼び出しID（tool roleの場合必須）
        name: ツール名（tool roleの場合）
    """

    role: str = Field(..., description="メッセージの役割")
    content: str | None = Field(default=None, description="メッセージ内容")
    tool_call_id: str | None = Field(default=None, description="ツール呼び出しID")
    name: str | None = Field(default=None, description="ツール名")


class ToolCall(BaseModel):
    """ツール呼び出し情報.

    Args:
        id: ツール呼び出しID
        name: ツール名
        arguments: ツール引数（JSON文字列またはdict）
    """

    id: str = Field(..., description="ツール呼び出しID")
    name: str = Field(..., description="ツール名")
    arguments: str | dict = Field(..., description="ツール引数")

    def get_arguments_dict(self) -> dict:
        """引数を辞書として取得."""
        import json
        if isinstance(self.arguments, dict):
            return self.arguments
        return json.loads(self.arguments)


class LLMResponse(BaseModel):
    """LLMレスポンス.

    Args:
        content: 生成されたテキスト
        model: 使用されたモデル名
        usage: トークン使用量
        finish_reason: 終了理由
        tool_calls: ツール呼び出しリスト
    """

    content: str | None = Field(default=None, description="生成されたテキスト")
    model: str = Field(..., description="使用されたモデル名")
    usage: dict[str, int] = Field(default_factory=dict, description="トークン使用量")
    finish_reason: str | None = Field(default=None, description="終了理由")
    tool_calls: list[ToolCall] = Field(default_factory=list, description="ツール呼び出しリスト")

    def has_tool_calls(self) -> bool:
        """ツール呼び出しがあるかどうか."""
        return len(self.tool_calls) > 0


class LLMConfig(BaseModel):
    """LLM設定.

    Args:
        provider: プロバイダー名（openai/anthropic/google/ollama/localai）
        model: モデル名
        api_key: APIキー
        base_url: カスタムベースURL（Ollama/LocalAI用）
        temperature: 温度パラメータ（0.0-2.0）
        max_tokens: 最大トークン数
        timeout: タイムアウト（秒）
    """

    provider: str = Field(default="openai", description="プロバイダー名")
    model: str = Field(default="gpt-4o", description="モデル名")
    api_key: str | None = Field(default=None, description="APIキー")
    base_url: str | None = Field(default=None, description="カスタムベースURL")
    temperature: float = Field(default=0.7, ge=0.0, le=2.0, description="温度パラメータ")
    max_tokens: int = Field(default=2000, gt=0, description="最大トークン数")
    timeout: int = Field(default=60, gt=0, description="タイムアウト（秒）")


class LLMClient:
    """LLM統合クライアント.

    各種LLMプロバイダーとの統一インターフェースを提供します。

    使用例:
        ```python
        config = LLMConfig(provider="openai", model="gpt-4", api_key="sk-...")
        client = LLMClient(config)

        # テキスト生成
        response = await client.complete("Hello, world!")
        print(response.content)

        # チャット形式
        messages = [
            LLMMessage(role="system", content="You are a helpful assistant."),
            LLMMessage(role="user", content="What is AI?"),
        ]
        response = await client.chat(messages)
        print(response.content)

        # ストリーミング
        async for chunk in client.stream(messages):
            print(chunk, end="", flush=True)
        ```
    """

    def __init__(self, config: LLMConfig) -> None:
        """初期化.

        Args:
            config: LLM設定
        """
        self._config = config
        self._logger = logging.getLogger(__name__)
        self._client: Any = None

        # プロバイダー別のクライアント初期化
        self._initialize_client()

    def _initialize_client(self) -> None:
        """プロバイダー別のクライアント初期化."""
        if self._config.provider == "openai":
            self._initialize_openai()
        elif self._config.provider == "anthropic":
            self._initialize_anthropic()
        elif self._config.provider == "google":
            self._initialize_google()
        elif self._config.provider == "ollama":
            self._initialize_ollama()
        elif self._config.provider in ("localai", "local"):
            self._initialize_localai()
        else:
            raise ValueError(f"Unsupported provider: {self._config.provider}")

    def _initialize_openai(self) -> None:
        """OpenAIクライアント初期化."""
        try:
            import openai

            api_key = self._config.api_key or os.environ.get("OPENAI_API_KEY")
            self._client = openai.AsyncOpenAI(api_key=api_key)
            self._logger.info("OpenAI client initialized")
        except ImportError:
            self._logger.warning("OpenAI library not installed. Using mock client.")
            self._client = None

    def _initialize_anthropic(self) -> None:
        """Anthropicクライアント初期化."""
        try:
            import anthropic

            api_key = self._config.api_key or os.environ.get("ANTHROPIC_API_KEY")
            self._client = anthropic.AsyncAnthropic(api_key=api_key)
            self._logger.info("Anthropic client initialized")
        except ImportError:
            self._logger.warning("Anthropic library not installed. Using mock client.")
            self._client = None

    def _initialize_google(self) -> None:
        """Google Geminiクライアント初期化（新 google.genai ライブラリ使用）."""
        try:
            from google import genai

            api_key = self._config.api_key or os.environ.get("GEMINI_API_KEY")
            if api_key:
                self._client = genai.Client(api_key=api_key)
                self._logger.info(f"Google Gemini client initialized: {self._config.model}")
            else:
                self._logger.warning("GEMINI_API_KEY not set. Using mock client.")
                self._client = None
        except ImportError:
            self._logger.warning("google-genai library not installed. Using mock client.")
            self._client = None

    def _initialize_ollama(self) -> None:
        """Ollamaクライアント初期化（OpenAI互換API使用）."""
        base_url = self._config.base_url or os.environ.get(
            "OLLAMA_BASE_URL", "http://localhost:11434"
        )
        self._base_url = f"{base_url}/v1"
        self._client = "ollama"
        self._logger.info(f"Ollama client initialized: {base_url}")

    def _initialize_localai(self) -> None:
        """LocalAIクライアント初期化（OpenAI互換API使用）."""
        base_url = self._config.base_url or os.environ.get(
            "LOCALAI_BASE_URL", "http://localhost:8080"
        )
        self._base_url = f"{base_url}/v1"
        self._client = "localai"
        self._logger.info(f"LocalAI client initialized: {base_url}")

    async def complete(self, prompt: str, **kwargs: Any) -> LLMResponse:
        """テキスト生成.

        Args:
            prompt: プロンプト
            **kwargs: 追加パラメータ

        Returns:
            LLMレスポンス
        """
        messages = [LLMMessage(role="user", content=prompt)]
        return await self.chat(messages, **kwargs)

    async def chat(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> LLMResponse:
        """チャット形式の対話.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Returns:
            LLMレスポンス
        """
        # モッククライアントの場合
        if self._client is None:
            return self._mock_response(messages)

        # プロバイダー別の実装
        if self._config.provider == "openai":
            return await self._chat_openai(messages, **kwargs)
        elif self._config.provider == "anthropic":
            return await self._chat_anthropic(messages, **kwargs)
        elif self._config.provider == "google":
            return await self._chat_google(messages, **kwargs)
        elif self._config.provider in ("ollama", "localai", "local"):
            return await self._chat_openai_compatible(messages, **kwargs)
        else:
            return self._mock_response(messages)

    async def _chat_openai(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> LLMResponse:
        """OpenAIチャット（tool calling対応）.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ
                - tools: ツール定義リスト（OpenAI形式）
                - tool_choice: ツール選択モード（auto/none/required/{type:function,function:{name:...}}）

        Returns:
            LLMレスポンス

        Raises:
            asyncio.TimeoutError: タイムアウト時
            Exception: API呼び出しエラー時
        """
        try:
            # モデル名を小文字で取得
            model_name = self._config.model.lower()

            # モデル分類
            is_reasoning_model = bool(model_name.startswith(('o1', 'o3', 'o4')))
            is_gpt5_model = bool(model_name.startswith('gpt-5') or 'gpt5' in model_name)

            # メッセージをOpenAI形式に変換
            openai_messages = []
            for msg in messages:
                msg_dict: dict[str, Any] = {"role": msg.role}
                if msg.content is not None:
                    msg_dict["content"] = msg.content
                if msg.tool_call_id:
                    msg_dict["tool_call_id"] = msg.tool_call_id
                if msg.name:
                    msg_dict["name"] = msg.name
                openai_messages.append(msg_dict)

            # 基本パラメータ
            params: dict[str, Any] = {
                "model": self._config.model,
                "messages": openai_messages,
            }

            # ツール設定
            if "tools" in kwargs and kwargs["tools"]:
                params["tools"] = kwargs["tools"]
                if "tool_choice" in kwargs:
                    params["tool_choice"] = kwargs["tool_choice"]

            # モデルに応じたパラメータ設定
            if is_reasoning_model:
                params["max_completion_tokens"] = kwargs.get("max_tokens", self._config.max_tokens)
            elif is_gpt5_model:
                params["max_completion_tokens"] = kwargs.get("max_tokens", self._config.max_tokens)
                params["temperature"] = kwargs.get("temperature", self._config.temperature)
            else:
                params["temperature"] = kwargs.get("temperature", self._config.temperature)
                params["max_tokens"] = kwargs.get("max_tokens", self._config.max_tokens)

            response = await asyncio.wait_for(
                self._client.chat.completions.create(**params),
                timeout=self._config.timeout,
            )

            # ツール呼び出しの解析
            tool_calls_list: list[ToolCall] = []
            message = response.choices[0].message
            if hasattr(message, 'tool_calls') and message.tool_calls:
                for tc in message.tool_calls:
                    tool_calls_list.append(ToolCall(
                        id=tc.id,
                        name=tc.function.name,
                        arguments=tc.function.arguments,
                    ))

            return LLMResponse(
                content=message.content,
                model=response.model,
                usage={
                    "prompt_tokens": response.usage.prompt_tokens,
                    "completion_tokens": response.usage.completion_tokens,
                    "total_tokens": response.usage.total_tokens,
                },
                finish_reason=response.choices[0].finish_reason,
                tool_calls=tool_calls_list,
            )
        except asyncio.TimeoutError:
            self._logger.error(f"OpenAI API timeout after {self._config.timeout}s")
            raise
        except Exception as e:
            self._logger.error(f"OpenAI API error: {e}")
            raise

    async def _chat_anthropic(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> LLMResponse:
        """Anthropicチャット（tool calling対応）.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ
                - tools: ツール定義リスト（Anthropic形式に自動変換）

        Returns:
            LLMレスポンス

        Raises:
            asyncio.TimeoutError: タイムアウト時
            Exception: API呼び出しエラー時
        """
        try:
            # システムメッセージを分離
            system_message = None
            user_messages = []
            for msg in messages:
                if msg.role == "system":
                    system_message = msg.content
                elif msg.role == "tool":
                    # ツール結果をAnthropic形式に変換
                    user_messages.append({
                        "role": "user",
                        "content": [{
                            "type": "tool_result",
                            "tool_use_id": msg.tool_call_id,
                            "content": msg.content,
                        }],
                    })
                else:
                    user_messages.append({"role": msg.role, "content": msg.content})

            # パラメータ構築
            params: dict[str, Any] = {
                "model": self._config.model,
                "messages": user_messages,
                "temperature": kwargs.get("temperature", self._config.temperature),
                "max_tokens": kwargs.get("max_tokens", self._config.max_tokens),
                "timeout": self._config.timeout,
            }
            if system_message:
                params["system"] = system_message

            # ツール設定（OpenAI形式からAnthropic形式に変換）
            if "tools" in kwargs and kwargs["tools"]:
                anthropic_tools = []
                for tool in kwargs["tools"]:
                    if tool.get("type") == "function":
                        func = tool["function"]
                        anthropic_tools.append({
                            "name": func["name"],
                            "description": func.get("description", ""),
                            "input_schema": func.get("parameters", {"type": "object", "properties": {}}),
                        })
                params["tools"] = anthropic_tools

            response = await asyncio.wait_for(
                self._client.messages.create(**params),
                timeout=self._config.timeout,
            )

            # レスポンス解析
            content_text = None
            tool_calls_list: list[ToolCall] = []

            for block in response.content:
                if hasattr(block, 'text'):
                    content_text = block.text
                elif hasattr(block, 'type') and block.type == "tool_use":
                    tool_calls_list.append(ToolCall(
                        id=block.id,
                        name=block.name,
                        arguments=block.input if isinstance(block.input, str) else block.input,
                    ))

            return LLMResponse(
                content=content_text,
                model=response.model,
                usage={
                    "input_tokens": response.usage.input_tokens,
                    "output_tokens": response.usage.output_tokens,
                },
                finish_reason=response.stop_reason,
                tool_calls=tool_calls_list,
            )
        except asyncio.TimeoutError:
            self._logger.error(f"Anthropic API timeout after {self._config.timeout}s")
            raise
        except Exception as e:
            self._logger.error(f"Anthropic API error: {e}")
            raise

    async def _chat_google(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> LLMResponse:
        """Google Geminiチャット（tool calling対応、新 google.genai ライブラリ使用）.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ
                - tools: ツール定義リスト（Gemini形式に自動変換）

        Returns:
            LLMレスポンス
        """
        from google import genai
        from google.genai import types

        try:
            # メッセージをGemini形式に変換
            contents: list[types.Content] = []
            system_instruction = None

            for msg in messages:
                if msg.role == "system":
                    system_instruction = msg.content
                elif msg.role == "user":
                    contents.append(types.Content(role="user", parts=[types.Part.from_text(text=msg.content)]))
                elif msg.role == "assistant":
                    contents.append(types.Content(role="model", parts=[types.Part.from_text(text=msg.content)]))
                elif msg.role == "tool":
                    # ツール結果
                    contents.append(types.Content(
                        role="user",
                        parts=[types.Part.from_function_response(
                            name=msg.name or "unknown",
                            response={"result": msg.content},
                        )],
                    ))

            # ツール定義の変換（OpenAI形式 → Gemini形式）
            gemini_tools = None
            if "tools" in kwargs and kwargs["tools"]:
                function_declarations = []
                for tool in kwargs["tools"]:
                    if tool.get("type") == "function":
                        func = tool["function"]
                        function_declarations.append(types.FunctionDeclaration(
                            name=func["name"],
                            description=func.get("description", ""),
                            parameters=func.get("parameters", {}),
                        ))
                if function_declarations:
                    gemini_tools = [types.Tool(function_declarations=function_declarations)]

            # 生成設定
            gen_config = types.GenerateContentConfig(
                temperature=kwargs.get("temperature", self._config.temperature),
                max_output_tokens=kwargs.get("max_tokens", self._config.max_tokens),
                system_instruction=system_instruction,
                tools=gemini_tools,
            )

            # 非同期API呼び出し
            response = await asyncio.wait_for(
                asyncio.to_thread(
                    self._client.models.generate_content,
                    model=self._config.model,
                    contents=contents,
                    config=gen_config,
                ),
                timeout=self._config.timeout,
            )

            # レスポンス解析
            content_text = None
            tool_calls_list: list[ToolCall] = []

            if response.candidates and response.candidates[0].content:
                for part in response.candidates[0].content.parts:
                    if part.text:
                        content_text = part.text
                    elif part.function_call:
                        fc = part.function_call
                        tool_calls_list.append(ToolCall(
                            id=f"gemini_{fc.name}_{id(fc)}",
                            name=fc.name,
                            arguments=dict(fc.args) if fc.args else {},
                        ))

            # 使用量取得
            usage = {}
            if response.usage_metadata:
                usage = {
                    "prompt_tokens": response.usage_metadata.prompt_token_count or 0,
                    "completion_tokens": response.usage_metadata.candidates_token_count or 0,
                    "total_tokens": response.usage_metadata.total_token_count or 0,
                }

            return LLMResponse(
                content=content_text,
                model=self._config.model,
                usage=usage,
                finish_reason=str(response.candidates[0].finish_reason) if response.candidates else "unknown",
                tool_calls=tool_calls_list,
            )
        except asyncio.TimeoutError:
            self._logger.error(f"Google API timeout after {self._config.timeout}s")
            raise
        except Exception as e:
            self._logger.error(f"Google API error: {e}")
            raise

    async def _chat_openai_compatible(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> LLMResponse:
        """OpenAI互換APIチャット（Ollama/LocalAI用）.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Returns:
            LLMレスポンス
        """
        try:
            async with httpx.AsyncClient(timeout=self._config.timeout) as client:
                response = await client.post(
                    f"{self._base_url}/chat/completions",
                    json={
                        "model": self._config.model,
                        "messages": [
                            {"role": msg.role, "content": msg.content} for msg in messages
                        ],
                        "temperature": kwargs.get("temperature", self._config.temperature),
                        "max_tokens": kwargs.get("max_tokens", self._config.max_tokens),
                    },
                )
                response.raise_for_status()
                data = response.json()

                return LLMResponse(
                    content=data["choices"][0]["message"]["content"],
                    model=data.get("model", self._config.model),
                    usage=data.get("usage", {}),
                    finish_reason=data["choices"][0].get("finish_reason"),
                )
        except httpx.TimeoutException:
            self._logger.error(f"API timeout after {self._config.timeout}s")
            raise asyncio.TimeoutError(f"Timeout after {self._config.timeout}s")
        except Exception as e:
            self._logger.error(f"API error: {e}")
            raise

    async def stream(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> AsyncIterator[str]:
        """ストリーミング生成.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Yields:
            生成されたテキストチャンク
        """
        # モッククライアントの場合
        if self._client is None:
            response = self._mock_response(messages)
            yield response.content
            return

        # プロバイダー別の実装
        if self._config.provider == "openai":
            async for chunk in self._stream_openai(messages, **kwargs):
                yield chunk
        elif self._config.provider == "anthropic":
            async for chunk in self._stream_anthropic(messages, **kwargs):
                yield chunk
        elif self._config.provider == "google":
            async for chunk in self._stream_google(messages, **kwargs):
                yield chunk
        elif self._config.provider in ("ollama", "localai", "local"):
            async for chunk in self._stream_openai_compatible(messages, **kwargs):
                yield chunk

    async def _stream_openai(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> AsyncIterator[str]:
        """OpenAIストリーミング.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Yields:
            生成されたテキストチャンク
        """
        # モデル名を小文字で取得
        model_name = self._config.model.lower()

        # モデル分類
        is_reasoning_model = bool(model_name.startswith(('o1', 'o3', 'o4')))
        is_gpt5_model = bool(model_name.startswith('gpt-5') or 'gpt5' in model_name)

        params: dict[str, Any] = {
            "model": self._config.model,
            "messages": [{"role": msg.role, "content": msg.content} for msg in messages],
            "stream": True,
        }

        if is_reasoning_model:
            # Reasoning モデル: temperature 非対応、max_completion_tokens を使用
            params["max_completion_tokens"] = kwargs.get("max_tokens", self._config.max_tokens)
        elif is_gpt5_model:
            # GPT-5 系: max_completion_tokens を使用、temperature は対応
            params["max_completion_tokens"] = kwargs.get("max_tokens", self._config.max_tokens)
            params["temperature"] = kwargs.get("temperature", self._config.temperature)
        else:
            # 従来モデル: max_tokens と temperature を使用
            params["temperature"] = kwargs.get("temperature", self._config.temperature)
            params["max_tokens"] = kwargs.get("max_tokens", self._config.max_tokens)

        stream = await self._client.chat.completions.create(**params)

        async for chunk in stream:
            if chunk.choices[0].delta.content:
                yield chunk.choices[0].delta.content

    async def _stream_anthropic(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> AsyncIterator[str]:
        """Anthropicストリーミング.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Yields:
            生成されたテキストチャンク
        """
        # システムメッセージを分離
        system_message = None
        user_messages = []
        for msg in messages:
            if msg.role == "system":
                system_message = msg.content
            else:
                user_messages.append({"role": msg.role, "content": msg.content})

        async with self._client.messages.stream(
            model=self._config.model,
            system=system_message,
            messages=user_messages,
            temperature=kwargs.get("temperature", self._config.temperature),
            max_tokens=kwargs.get("max_tokens", self._config.max_tokens),
        ) as stream:
            async for text in stream.text_stream:
                yield text

    async def _stream_google(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> AsyncIterator[str]:
        """Google Geminiストリーミング.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Yields:
            生成されたテキストチャンク
        """
        # メッセージをGemini形式に変換
        contents = []
        for msg in messages:
            if msg.role == "user":
                contents.append({"role": "user", "parts": [msg.content]})
            elif msg.role == "assistant":
                contents.append({"role": "model", "parts": [msg.content]})
            # system メッセージは最初のユーザーメッセージに含める

        # 同期ストリームを非同期で処理
        loop = asyncio.get_event_loop()
        response = await loop.run_in_executor(
            None,
            lambda: self._client.generate_content(
                contents,
                generation_config={
                    "temperature": kwargs.get("temperature", self._config.temperature),
                    "max_output_tokens": kwargs.get("max_tokens", self._config.max_tokens),
                },
                stream=True,
            ),
        )

        for chunk in response:
            if chunk.text:
                yield chunk.text

    async def _stream_openai_compatible(
        self, messages: list[LLMMessage], **kwargs: Any
    ) -> AsyncIterator[str]:
        """OpenAI互換APIストリーミング（Ollama/LocalAI用）.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Yields:
            生成されたテキストチャンク
        """
        async with httpx.AsyncClient(timeout=self._config.timeout) as client:
            async with client.stream(
                "POST",
                f"{self._base_url}/chat/completions",
                json={
                    "model": self._config.model,
                    "messages": [
                        {"role": msg.role, "content": msg.content} for msg in messages
                    ],
                    "temperature": kwargs.get("temperature", self._config.temperature),
                    "max_tokens": kwargs.get("max_tokens", self._config.max_tokens),
                    "stream": True,
                },
            ) as response:
                async for line in response.aiter_lines():
                    if line.startswith("data: "):
                        data = line[6:]
                        if data == "[DONE]":
                            break
                        try:
                            import json

                            chunk = json.loads(data)
                            content = chunk["choices"][0]["delta"].get("content", "")
                            if content:
                                yield content
                        except (json.JSONDecodeError, KeyError):
                            continue

    def _mock_response(self, messages: list[LLMMessage]) -> LLMResponse:
        """モックレスポンス生成.

        Args:
            messages: メッセージリスト

        Returns:
            モックLLMレスポンス
        """
        last_message = messages[-1].content if messages else ""
        mock_content = f"[MOCK] Response to: {last_message[:50]}..."

        return LLMResponse(
            content=mock_content,
            model=self._config.model,
            usage={"prompt_tokens": 10, "completion_tokens": 20, "total_tokens": 30},
            finish_reason="stop",
        )

