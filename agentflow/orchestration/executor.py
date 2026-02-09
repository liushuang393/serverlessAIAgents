"""実行Agent - 計画ステップの実行.

計画に基づいてステップを実行し、結果を管理する。

設計原則:
- ステップ単位の実行
- エラーハンドリングとリトライ
- 並列実行サポート

使用例:
    >>> from agentflow.orchestration.executor import ExecutorAgent
    >>>
    >>> executor = ExecutorAgent(tool_provider=my_provider)
    >>> result = await executor.execute_step(step)
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from agentflow.orchestration.planner import (
    ExecutionPlan,
    PlanStep,
    StepStatus,
    StepType,
)


@dataclass
class StepResult:
    """ステップ実行結果.

    Attributes:
        step_id: ステップID
        success: 成功したかどうか
        output: 出力データ
        error: エラーメッセージ
        duration_ms: 実行時間（ミリ秒）
        retries: リトライ回数
        metadata: メタデータ
    """

    step_id: str
    success: bool
    output: Any = None
    error: str | None = None
    duration_ms: float = 0.0
    retries: int = 0
    metadata: dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "step_id": self.step_id,
            "success": self.success,
            "output": self.output,
            "error": self.error,
            "duration_ms": self.duration_ms,
            "retries": self.retries,
            "metadata": self.metadata,
            "timestamp": self.timestamp.isoformat(),
        }


@dataclass
class ExecutorConfig:
    """実行Agent設定.

    Attributes:
        max_retries: 最大リトライ回数
        retry_delay: リトライ遅延（秒）
        max_parallel: 最大並列実行数
        default_timeout: デフォルトタイムアウト
    """

    max_retries: int = 3
    retry_delay: float = 1.0
    max_parallel: int = 5
    default_timeout: float = 60.0


class ExecutorAgent:
    """実行Agent.

    計画のステップを実行し、結果を管理する。

    主な機能:
    - ステップの実行
    - エラーハンドリングとリトライ
    - 並列実行
    - 結果の追跡

    Example:
        >>> executor = ExecutorAgent(tool_provider=my_provider)
        >>>
        >>> # 単一ステップ実行
        >>> result = await executor.execute_step(step, context)
        >>>
        >>> # 計画全体を実行
        >>> async for result in executor.execute_plan(plan):
        ...     print(f"{result.step_id}: {result.success}")
    """

    def __init__(
        self,
        tool_provider: Any = None,
        llm_client: Any = None,
        config: ExecutorConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            tool_provider: ツールプロバイダー
            llm_client: LLMクライアント
            config: 設定
        """
        self._tool_provider = tool_provider
        self._llm = llm_client
        self._config = config or ExecutorConfig()
        self._results: dict[str, StepResult] = {}
        self._logger = logging.getLogger(__name__)

    async def execute_step(
        self,
        step: PlanStep,
        context: dict[str, Any] | None = None,
    ) -> StepResult:
        """単一ステップを実行.

        Args:
            step: 実行するステップ
            context: 実行コンテキスト

        Returns:
            StepResult
        """
        context = context or {}
        start_time = datetime.now()
        retries = 0
        last_error: str | None = None

        self._logger.info(f"ステップ実行開始: {step.name} ({step.step_type.value})")
        step.status = StepStatus.RUNNING

        while retries <= self._config.max_retries:
            try:
                # タイムアウト付きで実行
                timeout = step.timeout_seconds or self._config.default_timeout
                output = await asyncio.wait_for(
                    self._execute_step_internal(step, context),
                    timeout=timeout,
                )

                duration_ms = (datetime.now() - start_time).total_seconds() * 1000

                result = StepResult(
                    step_id=step.id,
                    success=True,
                    output=output,
                    duration_ms=duration_ms,
                    retries=retries,
                )

                step.status = StepStatus.COMPLETED
                step.result = output
                self._results[step.id] = result

                self._logger.info(
                    f"ステップ完了: {step.name} ({duration_ms:.0f}ms)"
                )
                return result

            except TimeoutError:
                last_error = f"タイムアウト ({timeout}秒)"
                self._logger.warning(
                    f"ステップタイムアウト: {step.name} (リトライ {retries + 1}/{self._config.max_retries})"
                )
            except Exception as e:
                last_error = str(e)
                self._logger.warning(
                    f"ステップ失敗: {step.name} - {e} (リトライ {retries + 1}/{self._config.max_retries})"
                )

            retries += 1
            if retries <= self._config.max_retries:
                await asyncio.sleep(self._config.retry_delay * retries)

        # 全リトライ失敗
        duration_ms = (datetime.now() - start_time).total_seconds() * 1000
        result = StepResult(
            step_id=step.id,
            success=False,
            error=last_error,
            duration_ms=duration_ms,
            retries=retries - 1,
        )

        step.status = StepStatus.FAILED
        step.error = last_error
        self._results[step.id] = result

        self._logger.error(f"ステップ最終失敗: {step.name} - {last_error}")
        return result

    async def _execute_step_internal(
        self,
        step: PlanStep,
        context: dict[str, Any],
    ) -> Any:
        """ステップの内部実行.

        Args:
            step: ステップ
            context: コンテキスト

        Returns:
            出力データ
        """
        if step.step_type == StepType.TOOL_CALL:
            return await self._execute_tool_call(step, context)
        if step.step_type == StepType.LLM_GENERATION:
            return await self._execute_llm_generation(step, context)
        if step.step_type == StepType.HUMAN_INPUT:
            return await self._execute_human_input(step, context)
        if step.step_type == StepType.PARALLEL:
            return await self._execute_parallel(step, context)
        if step.step_type == StepType.SEQUENTIAL:
            return await self._execute_sequential(step, context)
        msg = f"不明なステップ種別: {step.step_type}"
        raise ValueError(msg)

    async def _execute_tool_call(
        self,
        step: PlanStep,
        context: dict[str, Any],
    ) -> Any:
        """ツール呼び出しを実行."""
        if not self._tool_provider:
            msg = "ツールプロバイダーが設定されていません"
            raise RuntimeError(msg)

        if not step.tool_uri:
            msg = "ツールURIが指定されていません"
            raise ValueError(msg)

        # パラメータをコンテキストで展開
        params = self._expand_params(step.params, context)

        result = await self._tool_provider.call(step.tool_uri, params)

        if not result.success:
            msg = f"ツール呼び出し失敗: {result.error}"
            raise RuntimeError(msg)

        return result.output

    async def _execute_llm_generation(
        self,
        step: PlanStep,
        context: dict[str, Any],
    ) -> Any:
        """LLM生成を実行."""
        if not self._llm:
            msg = "LLMクライアントが設定されていません"
            raise RuntimeError(msg)

        # プロンプトを構築
        prompt = step.params.get("prompt", step.description)
        prompt = self._expand_template(prompt, context)

        return await self._llm.generate(prompt)

    async def _execute_human_input(
        self,
        step: PlanStep,
        context: dict[str, Any],
    ) -> Any:
        """人間入力待ちを実行."""
        # HITLシステムを使用
        step.status = StepStatus.WAITING
        self._logger.info(f"人間入力待ち: {step.name}")

        # ここでは仮実装
        # 実際にはApprovalManagerと連携
        return {"status": "waiting_for_human", "step": step.name}

    async def _execute_parallel(
        self,
        step: PlanStep,
        context: dict[str, Any],
    ) -> list[Any]:
        """並列実行."""
        if not step.sub_steps:
            return []

        # 同時実行数を制限
        semaphore = asyncio.Semaphore(self._config.max_parallel)

        async def run_with_semaphore(sub_step: PlanStep) -> StepResult:
            async with semaphore:
                return await self.execute_step(sub_step, context)

        results = await asyncio.gather(
            *[run_with_semaphore(s) for s in step.sub_steps],
            return_exceptions=True,
        )

        outputs = []
        for r in results:
            if isinstance(r, Exception):
                outputs.append({"error": str(r)})
            else:
                outputs.append(r.output if r.success else {"error": r.error})

        return outputs

    async def _execute_sequential(
        self,
        step: PlanStep,
        context: dict[str, Any],
    ) -> list[Any]:
        """順次実行."""
        outputs = []
        current_context = dict(context)

        for sub_step in step.sub_steps:
            result = await self.execute_step(sub_step, current_context)

            if result.success:
                outputs.append(result.output)
                # 結果をコンテキストに追加
                current_context[f"step_{sub_step.id}_output"] = result.output
            else:
                outputs.append({"error": result.error})
                break

        return outputs

    def _expand_params(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """パラメータをコンテキストで展開.

        {{key}}形式のプレースホルダーをコンテキスト値で置換。
        """
        expanded = {}
        for key, value in params.items():
            if isinstance(value, str):
                expanded[key] = self._expand_template(value, context)
            elif isinstance(value, dict):
                expanded[key] = self._expand_params(value, context)
            else:
                expanded[key] = value
        return expanded

    def _expand_template(
        self,
        template: str,
        context: dict[str, Any],
    ) -> str:
        """テンプレートを展開."""
        result = template
        for key, value in context.items():
            result = result.replace(f"{{{{{key}}}}}", str(value))
        return result

    async def execute_plan(
        self,
        plan: ExecutionPlan,
        context: dict[str, Any] | None = None,
    ):
        """計画全体を実行.

        依存関係を考慮して順次/並列で実行。

        Args:
            plan: 実行計画
            context: 実行コンテキスト

        Yields:
            StepResult
        """
        context = context or dict(plan.context)

        while True:
            ready_steps = plan.get_ready_steps()
            if not ready_steps:
                break

            if len(ready_steps) == 1:
                # 単一ステップは順次実行
                result = await self.execute_step(ready_steps[0], context)
                if result.success and result.output:
                    context[f"step_{ready_steps[0].id}_output"] = result.output
                yield result
            else:
                # 複数ステップは並列実行
                tasks = [self.execute_step(s, context) for s in ready_steps]
                results = await asyncio.gather(*tasks)

                for step, result in zip(ready_steps, results, strict=False):
                    if result.success and result.output:
                        context[f"step_{step.id}_output"] = result.output
                    yield result

    def get_results(self) -> dict[str, StepResult]:
        """全ての実行結果を取得."""
        return dict(self._results)

    def get_result(self, step_id: str) -> StepResult | None:
        """特定ステップの結果を取得."""
        return self._results.get(step_id)

    def clear_results(self) -> None:
        """実行結果をクリア."""
        self._results.clear()

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        success_count = sum(1 for r in self._results.values() if r.success)
        total_duration = sum(r.duration_ms for r in self._results.values())

        return {
            "total_steps": len(self._results),
            "success_count": success_count,
            "failure_count": len(self._results) - success_count,
            "total_duration_ms": total_duration,
            "has_tool_provider": self._tool_provider is not None,
            "has_llm": self._llm is not None,
        }


# エクスポート
__all__ = [
    "ExecutorAgent",
    "ExecutorConfig",
    "StepResult",
]
