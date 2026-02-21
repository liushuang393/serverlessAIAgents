"""Result Summarizer - 子Agent結果の過濾・要約.

子Agentの実行結果から中間状態を除去し、
最終結果のみを親Agentに返す。

設計原則:
- 中間状態除外: デバッグ情報、中間計算、ログを除去
- 最終結果抽出: 完了タスクの最終出力のみ
- 要約生成: 複数結果を簡潔に統合
- 構造保持: 重要なメタデータは保持

使用例:
    >>> summarizer = ResultSummarizer()
    >>> clean_result = await summarizer.summarize_results(
    ...     results={"task1": {...}, "task2": {...}},
    ...     include_metadata=False,
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Protocol, cast


_logger = logging.getLogger(__name__)


class FilterLevel(str, Enum):
    """フィルタリングレベル."""

    MINIMAL = "minimal"  # 最小限のフィルタリング
    STANDARD = "standard"  # 標準フィルタリング
    AGGRESSIVE = "aggressive"  # 積極的フィルタリング


@dataclass
class SummarizerConfig:
    """サマライザー設定.

    Attributes:
        filter_level: フィルタリングレベル
        max_result_length: 結果の最大長
        include_status: ステータス情報を含める
        include_metadata: メタデータを含める
        excluded_keys: 除外するキー
    """

    filter_level: FilterLevel = FilterLevel.STANDARD
    max_result_length: int = 1000
    include_status: bool = True
    include_metadata: bool = False
    excluded_keys: list[str] = field(
        default_factory=lambda: [
            # デバッグ・中間状態
            "debug",
            "debug_info",
            "trace",
            "stack_trace",
            "logs",
            "intermediate",
            "intermediate_steps",
            "intermediate_results",
            "internal",
            "internal_state",
            "_internal",
            # 一時データ
            "temp",
            "temporary",
            "cache",
            "cached",
            # 低レベル詳細
            "raw",
            "raw_response",
            "raw_output",
            "tokens",
            "token_count",
            "usage",
            # レンダリング関連
            "render",
            "rendered",
            "html",
            "markdown_source",
            "style",
            "styles",
            "css",
        ]
    )


@dataclass
class SummarizedResult:
    """要約された結果.

    Attributes:
        final_output: 最終出力
        status: ステータス
        task_count: タスク数
        success_count: 成功数
        failed_count: 失敗数
        metadata: メタデータ（オプション）
    """

    final_output: dict[str, Any]
    status: str
    task_count: int
    success_count: int
    failed_count: int
    metadata: dict[str, Any] = field(default_factory=dict)


class LLMSummarizer(Protocol):
    """LLM要約インターフェース（DI用）."""

    async def summarize_text(self, text: str, max_length: int) -> str:
        """テキストを要約."""
        ...


class ResultSummarizer:
    """子Agent結果サマライザー.

    複数の子Agent結果から中間状態を除去し、
    最終結果のみを抽出・統合する。

    Example:
        >>> summarizer = ResultSummarizer()
        >>>
        >>> # 複数タスクの結果を要約
        >>> results = {
        ...     "research": {
        ...         "status": "completed",
        ...         "output": "調査結果...",
        ...         "debug_info": {...},  # 除去される
        ...         "intermediate_steps": [...],  # 除去される
        ...     },
        ...     "analysis": {
        ...         "status": "completed",
        ...         "output": "分析結果...",
        ...     },
        ... }
        >>> clean = await summarizer.summarize_results(results)
    """

    def __init__(
        self,
        config: SummarizerConfig | None = None,
        llm_summarizer: LLMSummarizer | None = None,
    ) -> None:
        """初期化.

        Args:
            config: サマライザー設定
            llm_summarizer: LLM要約器（オプション）
        """
        self._config = config or SummarizerConfig()
        self._llm = llm_summarizer
        self._logger = logging.getLogger(__name__)

    async def summarize_results(
        self,
        results: dict[str, Any],
        include_metadata: bool | None = None,
    ) -> SummarizedResult:
        """複数タスクの結果を要約.

        Args:
            results: タスクID -> 結果の辞書
            include_metadata: メタデータを含める（設定をオーバーライド）

        Returns:
            要約された結果
        """
        include_meta = (
            include_metadata if include_metadata is not None else self._config.include_metadata
        )

        filtered_results: dict[str, Any] = {}
        success_count = 0
        failed_count = 0

        for task_id, result in results.items():
            # ステータス判定
            status = self._get_task_status(result)
            if status == "completed":
                success_count += 1
            elif status == "failed":
                failed_count += 1

            # 結果をフィルタリング
            filtered = self._filter_result(result)

            # 長さ制限
            filtered = self._truncate_result(filtered)

            filtered_results[task_id] = filtered

        # 全体ステータス
        overall_status = self._determine_overall_status(success_count, failed_count, len(results))

        # メタデータ
        metadata = {}
        if include_meta:
            metadata = self._extract_metadata(results)

        return SummarizedResult(
            final_output=filtered_results,
            status=overall_status,
            task_count=len(results),
            success_count=success_count,
            failed_count=failed_count,
            metadata=metadata,
        )

    async def summarize_single_result(
        self,
        result: dict[str, Any],
    ) -> dict[str, Any]:
        """単一結果を要約.

        Args:
            result: タスク結果

        Returns:
            フィルタリングされた結果
        """
        filtered = self._filter_result(result)
        return cast("dict[str, Any]", self._truncate_result(filtered))

    async def generate_text_summary(
        self,
        results: dict[str, Any],
        max_length: int = 500,
    ) -> str:
        """テキスト形式の要約を生成.

        Args:
            results: タスク結果
            max_length: 最大長

        Returns:
            テキスト要約
        """
        summarized = await self.summarize_results(results)

        # LLM要約
        if self._llm:
            try:
                text = str(summarized.final_output)
                return await self._llm.summarize_text(text, max_length)
            except Exception as e:
                self._logger.warning("LLM要約に失敗: %s", e)

        # 簡易要約
        return self._generate_simple_summary(summarized)

    def _filter_result(self, result: Any) -> Any:
        """結果をフィルタリング.

        Args:
            result: 元の結果

        Returns:
            フィルタリングされた結果
        """
        if isinstance(result, dict):
            return self._filter_dict(result)
        if isinstance(result, list):
            return [self._filter_result(item) for item in result]
        return result

    def _filter_dict(self, data: dict[str, Any]) -> dict[str, Any]:
        """辞書をフィルタリング.

        Args:
            data: 元の辞書

        Returns:
            フィルタリングされた辞書
        """
        filtered: dict[str, Any] = {}

        for key, value in data.items():
            # 除外キーチェック
            if self._should_exclude_key(key):
                continue

            # 再帰的フィルタリング
            filtered_value = self._filter_result(value)

            # 空値のスキップ（AGGRESSIVEモード）
            if self._config.filter_level == FilterLevel.AGGRESSIVE:
                if filtered_value is None or filtered_value in ("", []):
                    continue

            filtered[key] = filtered_value

        return filtered

    def _should_exclude_key(self, key: str) -> bool:
        """キーを除外すべきかチェック.

        Args:
            key: キー名

        Returns:
            除外すべきか
        """
        key_lower = key.lower()

        # 明示的除外リスト
        if key_lower in [k.lower() for k in self._config.excluded_keys]:
            return True

        # パターンマッチング
        exclude_patterns = [
            "_debug",
            "_trace",
            "_internal",
            "_temp",
            "_cache",
            "_raw",
            "_intermediate",
        ]
        for pattern in exclude_patterns:
            if pattern in key_lower:
                return True

        # プライベートキー（AGGRESSIVEモード）
        return bool(self._config.filter_level == FilterLevel.AGGRESSIVE and key.startswith("_"))

    def _truncate_result(self, result: Any) -> Any:
        """結果を切り詰め.

        Args:
            result: 元の結果

        Returns:
            切り詰められた結果
        """
        if isinstance(result, str):
            if len(result) > self._config.max_result_length:
                return result[: self._config.max_result_length] + "..."
        elif isinstance(result, dict):
            return {k: self._truncate_result(v) for k, v in result.items()}
        elif isinstance(result, list):
            # リストは最初の10要素のみ
            truncated = [self._truncate_result(item) for item in result[:10]]
            if len(result) > 10:
                truncated.append(f"... (+{len(result) - 10} more)")
            return truncated

        return result

    def _get_task_status(self, result: Any) -> str:
        """タスクステータスを取得.

        Args:
            result: タスク結果

        Returns:
            ステータス文字列
        """
        if isinstance(result, dict):
            # 明示的ステータス
            status = result.get("status", "")
            if status:
                return str(status).lower()

            # エラーチェック
            if result.get("error") or result.get("exception"):
                return "failed"

            # 出力存在チェック
            if result.get("output") or result.get("result"):
                return "completed"

        return "unknown"

    def _determine_overall_status(
        self,
        success: int,
        failed: int,
        total: int,
    ) -> str:
        """全体ステータスを判定.

        Args:
            success: 成功数
            failed: 失敗数
            total: 総数

        Returns:
            全体ステータス
        """
        if total == 0:
            return "empty"
        if failed == 0:
            return "completed"
        if success == 0:
            return "failed"
        return "partial"

    def _extract_metadata(self, results: dict[str, Any]) -> dict[str, Any]:
        """メタデータを抽出.

        Args:
            results: タスク結果

        Returns:
            抽出されたメタデータ
        """
        metadata: dict[str, Any] = {
            "task_ids": list(results.keys()),
        }

        # 実行時間の集計
        total_time = 0.0
        for result in results.values():
            if isinstance(result, dict):
                time_taken = result.get("time_taken", result.get("duration", 0))
                if isinstance(time_taken, (int, float)):
                    total_time += time_taken

        if total_time > 0:
            metadata["total_time"] = total_time

        return metadata

    def _generate_simple_summary(self, summarized: SummarizedResult) -> str:
        """簡易テキスト要約を生成.

        Args:
            summarized: 要約結果

        Returns:
            テキスト要約
        """
        parts = [
            f"ステータス: {summarized.status}",
            f"タスク: {summarized.success_count}/{summarized.task_count} 成功",
        ]

        # 主要な出力を抽出
        for task_id, output in list(summarized.final_output.items())[:3]:
            if isinstance(output, dict):
                # outputキーを優先
                content = output.get("output", output.get("result", ""))
                if content:
                    preview = str(content)[:100]
                    parts.append(f"- {task_id}: {preview}")

        return "\n".join(parts)


class DeepAgentResultFilter:
    """DeepAgent専用の結果フィルター.

    DeepAgentCoordinatorの結果を親Agentに返す際に使用。
    TodoItem結果から最終出力のみを抽出。
    """

    def __init__(self, summarizer: ResultSummarizer | None = None) -> None:
        """初期化.

        Args:
            summarizer: 基底サマライザー
        """
        self._summarizer = summarizer or ResultSummarizer()

    async def filter_coordinator_results(
        self,
        todos: list[Any],  # TodoItem
        results: dict[str, Any],
    ) -> dict[str, Any]:
        """コーディネーター結果をフィルター.

        Args:
            todos: TodoItemリスト
            results: タスクID -> 結果

        Returns:
            フィルターされた結果
        """
        # 完了タスクのみ
        completed_results = {}
        for todo in todos:
            todo_id = getattr(todo, "id", str(todo))
            status = getattr(todo, "status", None)

            # 完了タスクのみ含める
            if status and str(status).lower() in ("completed", "done", "success"):
                if todo_id in results:
                    completed_results[todo_id] = results[todo_id]

        # サマライズ
        summarized = await self._summarizer.summarize_results(completed_results)

        return {
            "status": summarized.status,
            "results": summarized.final_output,
            "summary": {
                "total": summarized.task_count,
                "success": summarized.success_count,
                "failed": summarized.failed_count,
            },
        }
