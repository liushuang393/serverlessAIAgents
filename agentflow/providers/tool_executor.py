"""Tool Executor - 并行工具执行器（OpenAI Function Calling 兼容接口）.

このモジュールは、OpenAI parallel function calling 互換の
並行ツール実行機能を提供します。

接口設計原則:
- OpenAI Function Calling API 完全互換
- 将来 LangChain/LiteLLM への差し替え可能
- 内部実装は自研、接口は業界標準

使用例:
    >>> executor = ToolExecutor(tool_provider=my_tools)
    >>> # 並行実行（OpenAI互換）
    >>> results = await executor.execute_parallel(tool_calls)
    >>> # フォールバック付き実行
    >>> result = await executor.execute_with_fallback(tool_call)

参考:
- OpenAI Function Calling API
- LangChain ToolExecutor
- Anthropic Tool Use
"""

from __future__ import annotations

import asyncio
import logging
import time
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from agentflow.governance import (
    GovernanceDecision,
    GovernanceEngine,
    GovernanceResult,
    ToolExecutionContext,
)
from agentflow.hitl import interrupt
from agentflow.hitl.types import ApprovalRequest
from agentflow.providers.tool_provider import RegisteredTool, ToolProvider


if TYPE_CHECKING:
    from collections.abc import Callable


# =============================================================================
# 標準化データモデル（OpenAI Function Calling 互換）
# =============================================================================


class ToolCallStatus(str, Enum):
    """ツール呼び出しステータス."""

    PENDING = "pending"
    RUNNING = "running"
    SUCCESS = "success"
    FAILED = "failed"
    TIMEOUT = "timeout"
    FALLBACK = "fallback"  # フォールバックツールで成功


class ToolCall(BaseModel):
    """ツール呼び出し定義（OpenAI互換）.

    OpenAI Function Calling API と完全互換。

    Attributes:
        id: 呼び出しID（OpenAI形式: call_xxx）
        type: 常に "function"
        function: 関数呼び出し情報
    """

    id: str = Field(default_factory=lambda: f"call_{uuid.uuid4().hex[:12]}")
    type: str = Field(default="function")
    function: FunctionCall = Field(...)

    @classmethod
    def create(cls, name: str, arguments: dict[str, Any]) -> ToolCall:
        """簡易作成メソッド."""
        return cls(function=FunctionCall(name=name, arguments=arguments))


class FunctionCall(BaseModel):
    """関数呼び出し情報（OpenAI互換）."""

    name: str = Field(..., description="関数名")
    arguments: dict[str, Any] = Field(default_factory=dict, description="引数")


class ToolResult(BaseModel):
    """ツール実行結果（OpenAI互換）.

    OpenAI tool message 形式と互換。

    Attributes:
        tool_call_id: 対応する ToolCall.id
        role: 常に "tool"
        content: 実行結果（文字列またはJSON）
        name: ツール名
    """

    tool_call_id: str = Field(..., description="対応するToolCall ID")
    role: str = Field(default="tool")
    content: str = Field(..., description="実行結果")
    name: str = Field(..., description="ツール名")

    # 拡張フィールド（内部使用）
    status: ToolCallStatus = Field(default=ToolCallStatus.SUCCESS)
    execution_time_ms: float = Field(default=0.0, description="実行時間（ミリ秒）")
    error: str | None = Field(default=None, description="エラー情報")
    metadata: dict[str, Any] = Field(default_factory=dict)


class BatchResult(BaseModel):
    """バッチ実行結果."""

    results: list[ToolResult] = Field(default_factory=list)
    total_time_ms: float = Field(default=0.0)
    success_count: int = Field(default=0)
    failed_count: int = Field(default=0)
    fallback_count: int = Field(default=0)


# =============================================================================
# フォールバック戦略（拡張可能）
# =============================================================================


class FallbackStrategy(ABC):
    """フォールバック戦略インターフェース.

    将来の拡張や差し替えを容易にするための抽象インターフェース。
    """

    @abstractmethod
    def find_fallback(
        self,
        failed_tool: str,
        error: Exception,
        available_tools: list[str],
    ) -> str | None:
        """フォールバックツールを探す.

        Args:
            failed_tool: 失敗したツール名
            error: 発生したエラー
            available_tools: 利用可能なツール一覧

        Returns:
            フォールバックツール名、または None
        """


class SimpleFallbackStrategy(FallbackStrategy):
    """シンプルなフォールバック戦略.

    事前定義されたマッピングに基づいてフォールバック。
    """

    def __init__(
        self,
        fallback_mapping: dict[str, list[str]] | None = None,
    ) -> None:
        """初期化.

        Args:
            fallback_mapping: {ツール名: [フォールバック候補]} のマッピング
        """
        self._mapping = fallback_mapping or {}

    def find_fallback(
        self,
        failed_tool: str,
        error: Exception,
        available_tools: list[str],
    ) -> str | None:
        """フォールバックツールを探す."""
        candidates = self._mapping.get(failed_tool, [])
        for candidate in candidates:
            if candidate in available_tools:
                return candidate
        return None

    def register_fallback(self, tool: str, fallbacks: list[str]) -> None:
        """フォールバックを登録."""
        self._mapping[tool] = fallbacks


class SemanticFallbackStrategy(FallbackStrategy):
    """意味的フォールバック戦略.

    ツールの説明を基に意味的に類似したツールを探す。
    """

    def __init__(
        self,
        tool_provider: ToolProvider,
        similarity_threshold: float = 0.7,
    ) -> None:
        """初期化."""
        self._provider = tool_provider
        self._threshold = similarity_threshold

    def find_fallback(
        self,
        failed_tool: str,
        error: Exception,
        available_tools: list[str],
    ) -> str | None:
        """意味的に類似したツールを探す."""
        failed_info = self._provider.get_tool(failed_tool)
        if not failed_info:
            return None

        # 簡易的な類似度計算（将来はEmbedding使用）
        failed_desc = failed_info.description.lower()
        failed_words = set(failed_desc.split())

        best_match: str | None = None
        best_score = 0.0

        for tool_name in available_tools:
            if tool_name == failed_tool:
                continue
            tool_info = self._provider.get_tool(tool_name)
            if not tool_info:
                continue

            # Jaccard類似度
            tool_words = set(tool_info.description.lower().split())
            if not tool_words:
                continue

            intersection = len(failed_words & tool_words)
            union = len(failed_words | tool_words)
            score = intersection / union if union > 0 else 0.0

            if score > best_score and score >= self._threshold:
                best_score = score
                best_match = tool_name

        return best_match


# =============================================================================
# リトライ戦略
# =============================================================================


@dataclass
class RetryConfig:
    """リトライ設定."""

    max_retries: int = 3
    base_delay: float = 1.0  # 秒
    max_delay: float = 30.0  # 秒
    exponential_base: float = 2.0
    jitter: bool = True


class RetryStrategy:
    """リトライ戦略（指数バックオフ）."""

    def __init__(self, config: RetryConfig | None = None) -> None:
        """初期化."""
        self._config = config or RetryConfig()

    @property
    def max_retries(self) -> int:
        """最大リトライ回数を取得."""

        return self._config.max_retries

    def should_retry(self, attempt: int, error: Exception) -> bool:
        """リトライすべきか判定."""
        if attempt >= self._config.max_retries:
            return False

        # リトライ可能なエラータイプ
        retryable_errors = (
            asyncio.TimeoutError,
            ConnectionError,
            TimeoutError,
        )
        return isinstance(error, retryable_errors)

    def get_delay(self, attempt: int) -> float:
        """次のリトライまでの遅延を計算."""
        import random

        delay = min(
            self._config.base_delay * (self._config.exponential_base**attempt),
            self._config.max_delay,
        )
        if self._config.jitter:
            delay *= 0.5 + random.random()
        return delay


# =============================================================================
# メイン: ToolExecutor
# =============================================================================


class ToolExecutor:
    """並行ツール実行器（OpenAI Function Calling 互換）.

    設計原則:
    - OpenAI API 完全互換の入出力
    - 並行実行、リトライ、フォールバックを統合
    - 将来の差し替えを考慮した抽象化

    使用例:
        >>> executor = ToolExecutor(tool_provider=my_tools)
        >>>
        >>> # 単一実行
        >>> result = await executor.execute(ToolCall.create("search", {"q": "AI"}))
        >>>
        >>> # 並行実行（OpenAI parallel function calling 互換）
        >>> results = await executor.execute_parallel([
        ...     ToolCall.create("search", {"q": "AI"}),
        ...     ToolCall.create("fetch", {"url": "..."}),
        ... ])
        >>>
        >>> # フォールバック付き
        >>> result = await executor.execute_with_fallback(tool_call)
    """

    def __init__(
        self,
        tool_provider: ToolProvider | None = None,
        fallback_strategy: FallbackStrategy | None = None,
        retry_config: RetryConfig | None = None,
        max_concurrent: int = 10,
        default_timeout: float = 30.0,
        on_tool_start: Callable[[ToolCall], None] | None = None,
        on_tool_complete: Callable[[ToolResult], None] | None = None,
        governance_engine: GovernanceEngine | None = None,
    ) -> None:
        """初期化.

        Args:
            tool_provider: ツールプロバイダー
            fallback_strategy: フォールバック戦略
            retry_config: リトライ設定
            max_concurrent: 最大同時実行数
            default_timeout: デフォルトタイムアウト（秒）
            on_tool_start: ツール開始コールバック
            on_tool_complete: ツール完了コールバック
        """
        self._provider = tool_provider or ToolProvider.discover()
        self._fallback = fallback_strategy or SimpleFallbackStrategy()
        self._retry = RetryStrategy(retry_config)
        self._max_concurrent = max_concurrent
        self._default_timeout = default_timeout
        self._on_start = on_tool_start
        self._on_complete = on_tool_complete
        self._governance = governance_engine or GovernanceEngine()
        self._logger = logging.getLogger(__name__)

        # 実行統計
        self._stats = {
            "total_calls": 0,
            "success_calls": 0,
            "failed_calls": 0,
            "fallback_calls": 0,
            "total_time_ms": 0.0,
        }

    async def execute(
        self,
        tool_call: ToolCall,
        timeout: float | None = None,
        execution_context: ToolExecutionContext | None = None,
    ) -> ToolResult:
        """単一ツールを実行.

        Args:
            tool_call: ツール呼び出し
            timeout: タイムアウト（秒）

        Returns:
            実行結果
        """
        return await self._execute_single(
            tool_call,
            timeout or self._default_timeout,
            enable_fallback=False,
            execution_context=execution_context,
        )

    async def execute_with_fallback(
        self,
        tool_call: ToolCall,
        timeout: float | None = None,
        execution_context: ToolExecutionContext | None = None,
    ) -> ToolResult:
        """フォールバック付きで単一ツールを実行.

        Args:
            tool_call: ツール呼び出し
            timeout: タイムアウト（秒）

        Returns:
            実行結果
        """
        return await self._execute_single(
            tool_call,
            timeout or self._default_timeout,
            enable_fallback=True,
            execution_context=execution_context,
        )

    async def execute_parallel(
        self,
        tool_calls: list[ToolCall],
        timeout: float | None = None,
        enable_fallback: bool = True,
        execution_context: ToolExecutionContext | None = None,
    ) -> BatchResult:
        """複数ツールを並行実行（OpenAI parallel function calling 互換）.

        OpenAI の parallel function calling と同様に、
        複数のツール呼び出しを同時に実行します。

        Args:
            tool_calls: ツール呼び出しリスト
            timeout: 個々のタイムアウト（秒）
            enable_fallback: フォールバックを有効化

        Returns:
            バッチ実行結果
        """
        if not tool_calls:
            return BatchResult()

        start_time = time.time()
        semaphore = asyncio.Semaphore(self._max_concurrent)
        effective_timeout = timeout or self._default_timeout

        async def _execute_one(tc: ToolCall) -> ToolResult:
            async with semaphore:
                return await self._execute_single(
                    tc,
                    effective_timeout,
                    enable_fallback,
                    execution_context=execution_context,
                )

        # 並行実行
        tasks = [_execute_one(tc) for tc in tool_calls]
        results = await asyncio.gather(*tasks, return_exceptions=True)

        # 結果を整理
        processed_results: list[ToolResult] = []
        success_count = 0
        failed_count = 0
        fallback_count = 0

        for i, result in enumerate(results):
            if isinstance(result, ToolResult):
                tool_result = result
                processed_results.append(tool_result)
                if tool_result.status == ToolCallStatus.SUCCESS:
                    success_count += 1
                elif tool_result.status == ToolCallStatus.FALLBACK:
                    fallback_count += 1
                else:
                    failed_count += 1
                continue

            # gather で例外が発生した場合
            error = result
            processed_results.append(
                ToolResult(
                    tool_call_id=tool_calls[i].id,
                    name=tool_calls[i].function.name,
                    content=f"Error: {error}",
                    status=ToolCallStatus.FAILED,
                    error=str(error),
                )
            )
            failed_count += 1

        total_time = (time.time() - start_time) * 1000

        return BatchResult(
            results=processed_results,
            total_time_ms=total_time,
            success_count=success_count,
            failed_count=failed_count,
            fallback_count=fallback_count,
        )

    async def _execute_single(
        self,
        tool_call: ToolCall,
        timeout: float,
        enable_fallback: bool,
        execution_context: ToolExecutionContext | None = None,
    ) -> ToolResult:
        """単一ツールを実行（内部）."""
        start_time = time.time()
        tool_name = tool_call.function.name
        arguments = tool_call.function.arguments
        tool_info = self._provider.get_tool(tool_name)

        # コールバック
        if self._on_start:
            self._on_start(tool_call)

        self._stats["total_calls"] += 1

        if tool_info is None:
            execution_time = (time.time() - start_time) * 1000
            self._stats["failed_calls"] += 1
            self._stats["total_time_ms"] += execution_time
            result = ToolResult(
                tool_call_id=tool_call.id,
                name=tool_name,
                content=f"Error: Tool not found: {tool_name}",
                status=ToolCallStatus.FAILED,
                execution_time_ms=execution_time,
                error=f"Tool not found: {tool_name}",
            )
            if self._on_complete:
                self._on_complete(result)
            return result

        governance_result = await self._governance.evaluate_tool(
            tool_info,
            tool_call.id,
            dict(arguments),
            execution_context,
        )

        if governance_result.decision == GovernanceDecision.DENY:
            execution_time = (time.time() - start_time) * 1000
            self._stats["failed_calls"] += 1
            self._stats["total_time_ms"] += execution_time
            result = ToolResult(
                tool_call_id=tool_call.id,
                name=tool_name,
                content=f"Error: {governance_result.reason}",
                status=ToolCallStatus.FAILED,
                execution_time_ms=execution_time,
                error=governance_result.reason,
                metadata={
                    "governance_warnings": governance_result.warnings,
                    "plugin_id": governance_result.plugin_id,
                    "plugin_version": governance_result.plugin_version,
                    "plugin_risk_tier": governance_result.plugin_risk_tier,
                },
            )
            if self._on_complete:
                self._on_complete(result)
            return result

        if governance_result.decision == GovernanceDecision.APPROVAL_REQUIRED:
            await self._interrupt_for_approval(
                tool_info,
                tool_call,
                execution_context,
                governance_result=governance_result,
            )

        # リトライループ
        last_error: Exception | None = None
        for attempt in range(self._retry.max_retries + 1):
            try:
                # タイムアウト付きで実行
                result_value = await asyncio.wait_for(
                    self._provider.call(tool_name, **arguments),
                    timeout=timeout,
                )

                # 成功
                execution_time = (time.time() - start_time) * 1000
                self._stats["success_calls"] += 1
                self._stats["total_time_ms"] += execution_time

                result = ToolResult(
                    tool_call_id=tool_call.id,
                    name=tool_name,
                    content=self._serialize_result(result_value),
                    status=ToolCallStatus.SUCCESS,
                    execution_time_ms=execution_time,
                    metadata={
                        "governance_warnings": governance_result.warnings,
                        "plugin_id": governance_result.plugin_id,
                        "plugin_version": governance_result.plugin_version,
                        "plugin_risk_tier": governance_result.plugin_risk_tier,
                    },
                )

                if self._on_complete:
                    self._on_complete(result)

                return result

            except TimeoutError as e:
                last_error = e
                self._logger.warning(f"Tool timeout: {tool_name} (attempt {attempt + 1})")

            except Exception as e:
                last_error = e
                self._logger.warning(f"Tool error: {tool_name} - {e} (attempt {attempt + 1})")

            # リトライ判定
            if self._retry.should_retry(attempt, last_error):
                delay = self._retry.get_delay(attempt)
                await asyncio.sleep(delay)
            else:
                break

        # 失敗: フォールバック試行
        if enable_fallback and last_error:
            fallback_result = await self._try_fallback(tool_call, last_error, timeout, start_time)
            if fallback_result:
                return fallback_result

        # 完全失敗
        execution_time = (time.time() - start_time) * 1000
        self._stats["failed_calls"] += 1
        self._stats["total_time_ms"] += execution_time

        result = ToolResult(
            tool_call_id=tool_call.id,
            name=tool_name,
            content=f"Error: {last_error}",
            status=ToolCallStatus.FAILED,
            execution_time_ms=execution_time,
            error=str(last_error),
            metadata={
                "governance_warnings": governance_result.warnings,
                "plugin_id": governance_result.plugin_id,
                "plugin_version": governance_result.plugin_version,
                "plugin_risk_tier": governance_result.plugin_risk_tier,
            },
        )

        if self._on_complete:
            self._on_complete(result)

        return result

    async def _interrupt_for_approval(
        self,
        tool_info: RegisteredTool,
        tool_call: ToolCall,
        execution_context: ToolExecutionContext | None,
        governance_result: GovernanceResult | None = None,
    ) -> None:
        """承認が必要な場合に割り込みを発火."""

        priority = self._approval_priority(tool_info.risk_level.value)
        request = ApprovalRequest(
            action=tool_info.name,
            resource_id=None,
            resource_type=None,
            reason="承認が必要なツール実行です",
            context={
                "tool_name": tool_info.name,
                "operation_type": tool_info.operation_type.value,
                "risk_level": tool_info.risk_level.value,
                "arguments": tool_call.function.arguments,
            },
            requester="ToolExecutor",
            priority=priority,
            timeout_seconds=None,
            expires_at=None,
            metadata={
                "tool_call_id": tool_call.id,
                "audit_required": tool_info.needs_audit(),
                "plugin_id": (governance_result.plugin_id if governance_result else tool_info.plugin_id),
                "plugin_version": (governance_result.plugin_version if governance_result else tool_info.plugin_version),
                "plugin_risk_tier": (governance_result.plugin_risk_tier if governance_result else None),
            },
        )

        flow_id = execution_context.flow_id if execution_context else None
        _ = await interrupt(
            request,
            flow_id=flow_id,
            state={
                "tool_call_id": tool_call.id,
                "tool_name": tool_info.name,
            },
        )

    def _approval_priority(self, risk_level: str) -> str:
        """リスクに応じた承認優先度を決定."""

        if risk_level in ("critical",):
            return "critical"
        if risk_level in ("high",):
            return "high"
        if risk_level in ("medium",):
            return "normal"
        return "low"

    async def _try_fallback(
        self,
        original_call: ToolCall,
        error: Exception,
        timeout: float,
        start_time: float,
    ) -> ToolResult | None:
        """フォールバックを試行."""
        available_tools = [t.name for t in self._provider.list_tools()]
        fallback_tool = self._fallback.find_fallback(
            original_call.function.name,
            error,
            available_tools,
        )

        if not fallback_tool:
            return None

        self._logger.info(f"Trying fallback: {original_call.function.name} -> {fallback_tool}")

        try:
            result_value = await asyncio.wait_for(
                self._provider.call(fallback_tool, **original_call.function.arguments),
                timeout=timeout,
            )

            execution_time = (time.time() - start_time) * 1000
            self._stats["fallback_calls"] += 1
            self._stats["success_calls"] += 1
            self._stats["total_time_ms"] += execution_time

            return ToolResult(
                tool_call_id=original_call.id,
                name=fallback_tool,
                content=self._serialize_result(result_value),
                status=ToolCallStatus.FALLBACK,
                execution_time_ms=execution_time,
                metadata={
                    "original_tool": original_call.function.name,
                    "fallback_reason": str(error),
                },
            )

        except Exception as e:
            self._logger.warning(f"Fallback also failed: {fallback_tool} - {e}")
            return None

    def _serialize_result(self, value: Any) -> str:
        """結果をシリアライズ."""
        import json

        if isinstance(value, str):
            return value
        try:
            return json.dumps(value, ensure_ascii=False, default=str)
        except Exception:
            return str(value)

    def get_stats(self) -> dict[str, Any]:
        """実行統計を取得."""
        return dict(self._stats)

    def reset_stats(self) -> None:
        """統計をリセット."""
        self._stats = {
            "total_calls": 0,
            "success_calls": 0,
            "failed_calls": 0,
            "fallback_calls": 0,
            "total_time_ms": 0.0,
        }

    def register_fallback(self, tool: str, fallbacks: list[str]) -> None:
        """フォールバックを登録（SimpleFallbackStrategy使用時）."""
        if isinstance(self._fallback, SimpleFallbackStrategy):
            self._fallback.register_fallback(tool, fallbacks)


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    "BatchResult",
    # 戦略
    "FallbackStrategy",
    "FunctionCall",
    "RetryConfig",
    "RetryStrategy",
    "SemanticFallbackStrategy",
    "SimpleFallbackStrategy",
    # データモデル（OpenAI互換）
    "ToolCall",
    "ToolCallStatus",
    # メイン
    "ToolExecutor",
    "ToolResult",
]
