# -*- coding: utf-8 -*-
"""LLM Provider - 統一LLMアクセスインターフェース（松耦合設計）.

このモジュールは、各種LLMプロバイダーへの統一アクセスを提供します。
Agent/サービスは具体的なプロバイダーやモデルを意識する必要がありません。

設計原則:
- 高内聚: LLM関連のロジックをすべてここに集約
- 松耦合: 呼び出し側はget_llm()だけを知ればよい
- 環境変数優先: APIキーは環境変数から自動取得
- フォールバック: 複数プロバイダーの自動切り替え

使用例（推奨）:
    >>> from agentflow.providers import get_llm
    >>> llm = get_llm()  # 環境変数から最適なプロバイダーを自動選択
    >>> response = await llm.chat([{"role": "user", "content": "hello"}])
    >>> # ストリーミング
    >>> async for chunk in llm.stream([...]):
    ...     print(chunk, end="")

使用例（Agent内）:
    >>> class MyAgent(AgentBlock):
    ...     async def run(self, input_data):
    ...         llm = get_llm()  # プロバイダー不明、モデル不明でOK
    ...         return await llm.chat([...])
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, AsyncIterator

from pydantic import BaseModel, Field

if TYPE_CHECKING:
    from agentflow.config import AgentFlowSettings
    from agentflow.llm.llm_client import LLMClient
    from agentflow.runtime import RuntimeContext

logger = logging.getLogger(__name__)

# グローバルシングルトン
_default_llm: "LLMProvider | None" = None


class LLMProviderConfig(BaseModel):
    """LLMプロバイダー設定（内部用・通常は指定不要）.

    Note:
        通常は get_llm() を使用し、この設定は意識しません。
        環境変数から自動的に最適な設定が選択されます。
    """

    temperature: float = Field(default=0.7, ge=0.0, le=2.0, description="温度パラメータ")
    max_tokens: int = Field(default=2000, gt=0, description="最大トークン数")
    # LLM API超时设置为180秒（复杂分析任务需要较长时间）
    timeout: int = Field(default=180, gt=0, description="タイムアウト秒数")


def _detect_provider_from_env(
    settings: "AgentFlowSettings | None" = None,
) -> tuple[str, str, str | None, str | None, int]:
    """環境変数から最適なプロバイダーを自動検出.

    優先順位: 環境変数 > .env > config.py デフォルト値
    （get_settings() が自動的にこの優先順位を適用）

    Returns:
        (provider, model, api_key, base_url, timeout) のタプル
    """
    from agentflow.config import AgentFlowSettings, get_settings

    settings = settings or get_settings()
    llm_config = settings.get_active_llm_config()

    provider = llm_config["provider"]
    model = llm_config["model"]
    api_key = llm_config.get("api_key")
    base_url = llm_config.get("base_url")
    timeout = llm_config.get("timeout", 180)

    if provider == "mock":
        logger.warning("No LLM API key found in environment. Using mock client.")

    return provider, model, api_key, base_url, timeout


class LLMProvider:
    """LLM統一プロバイダー（松耦合設計）.

    Agent/サービスはこのクラスのみを使用し、
    具体的なプロバイダー（OpenAI/Anthropic等）を意識しません。

    Note:
        直接インスタンス化せず、get_llm() を使用してください。
    """

    def __init__(
        self,
        config: LLMProviderConfig | None = None,
        *,
        temperature: float | None = None,
        max_tokens: int | None = None,
        settings: "AgentFlowSettings | None" = None,
    ) -> None:
        """初期化（通常は get_llm() を使用）.

        Args:
            config: 設定（省略時は環境変数から自動検出）
            temperature: 温度パラメータ（オーバーライド用）
            max_tokens: 最大トークン数（オーバーライド用）
        """
        self._config = config or LLMProviderConfig()
        self._temperature_override = temperature
        self._max_tokens_override = max_tokens
        self._client: "LLMClient | None" = None
        self._provider_info: tuple[str, str, str | None, str | None] | None = None
        self._settings = settings
        self._initialize_client()

    def _initialize_client(self) -> None:
        """内部クライアントを初期化（環境変数から自動検出）."""
        from agentflow.llm.llm_client import LLMClient, LLMConfig

        # 環境変数から最適なプロバイダーを検出
        provider, model, api_key, base_url, timeout = _detect_provider_from_env(self._settings)
        self._provider_info = (provider, model, api_key, base_url)

        llm_config = LLMConfig(
            provider=provider,
            model=model,
            api_key=api_key,
            base_url=base_url,
            temperature=self._temperature_override or self._config.temperature,
            max_tokens=self._max_tokens_override or self._config.max_tokens,
            timeout=timeout,  # 環境変数から取得したtimeoutを使用
        )
        self._client = LLMClient(llm_config)
        logger.info(f"LLMProvider initialized: provider={provider}, model={model}, timeout={timeout}s")

    async def chat(
        self,
        messages: list[dict[str, str]],
        **kwargs: Any,
    ) -> dict[str, Any]:
        """チャット形式の対話.

        Args:
            messages: メッセージリスト [{"role": "user", "content": "..."}]
            **kwargs: 追加パラメータ

        Returns:
            レスポンス {"content": "...", "model": "...", "usage": {...}}
        """
        from agentflow.llm.llm_client import LLMMessage

        llm_messages = [LLMMessage(**msg) for msg in messages]
        response = await self._client.chat(llm_messages, **kwargs)
        return response.model_dump()

    async def complete(self, prompt: str, **kwargs: Any) -> dict[str, Any]:
        """テキスト補完.

        Args:
            prompt: プロンプト
            **kwargs: 追加パラメータ

        Returns:
            レスポンス
        """
        response = await self._client.complete(prompt, **kwargs)
        return response.model_dump()

    async def stream(
        self,
        messages: list[dict[str, str]],
        **kwargs: Any,
    ) -> AsyncIterator[str]:
        """ストリーミング生成.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Yields:
            生成されたテキストチャンク
        """
        from agentflow.llm.llm_client import LLMMessage

        llm_messages = [LLMMessage(**msg) for msg in messages]
        async for chunk in self._client.stream(llm_messages, **kwargs):
            yield chunk

    @property
    def config(self) -> LLMProviderConfig:
        """設定を取得."""
        return self._config


def get_llm(
    *,
    temperature: float | None = None,
    max_tokens: int | None = None,
    context: "RuntimeContext | None" = None,
    _new_instance: bool = False,
) -> LLMProvider:
    """LLMプロバイダーを取得（推奨API）.

    Agent/サービスはこの関数のみを使用してLLMにアクセスします。
    具体的なプロバイダー（OpenAI/Anthropic等）やモデルを知る必要はありません。

    Args:
        temperature: 温度パラメータ（省略時はデフォルト 0.7）
        max_tokens: 最大トークン数（省略時はデフォルト 2000）
        context: RuntimeContext（テナント/設定の分離用）
        _new_instance: 新しいインスタンスを強制作成（テスト用）

    Returns:
        LLMProvider: 環境変数から自動検出されたプロバイダー

    Examples:
        >>> # Agent内での使用（推奨）
        >>> llm = get_llm()
        >>> response = await llm.chat([{"role": "user", "content": "hello"}])
        >>>
        >>> # 温度を指定（創造的なタスク向け）
        >>> llm = get_llm(temperature=0.9)
        >>>
        >>> # 低温度（分析タスク向け）
        >>> llm = get_llm(temperature=0.3)
    """
    global _default_llm

    # コンテキスト指定やカスタムパラメータがある場合は新しいインスタンス
    if context is not None or temperature is not None or max_tokens is not None or _new_instance:
        from agentflow.runtime import resolve_settings

        return LLMProvider(
            temperature=temperature,
            max_tokens=max_tokens,
            settings=resolve_settings(context),
        )

    # シングルトン
    if _default_llm is None:
        _default_llm = LLMProvider()

    return _default_llm


def reset_llm() -> None:
    """LLMシングルトンをリセット（テスト用）."""
    global _default_llm
    _default_llm = None
