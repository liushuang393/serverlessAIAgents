"""DeepAgent メイン協調器.

DeepAgentCoordinatorは以下の責務を持つ:
1. 認知分析（Cognitive Analysis）: タスクの意図・複雑度を分析
2. タスク分解（Task Decomposition）: TodoListへの分解
3. 並行実行（Parallel Execution）: 依存関係に基づく並行処理
4. 品質評審（Quality Review）: 多次元品質評価
5. 自己進化（Self-Evolution）: 成功パターン学習

実行フロー:
    User Request
        ↓
    [認知分析] → 意図理解、複雑度判定
        ↓
    [タスク分解] → TodoList生成
        ↓
    [並行実行] → Agent池から適切なAgentを選択・実行
        ↓
    [品質評審] → 結果検証、必要に応じて再実行
        ↓
    [自己進化] → 成功パターン学習
        ↓
    Final Response
"""

from __future__ import annotations

import asyncio
import logging
from typing import Any

from agentflow.patterns.deep_agent.da_compressor import ContextCompressor
from agentflow.patterns.deep_agent.da_evolver import SelfEvolver
from agentflow.patterns.deep_agent.da_models import (
    AgentType,
    CognitiveAnalysis,
    CompactionStrategy,
    QualityDimension,
    QualityReview,
    TaskStatus,
    TodoItem,
)
from agentflow.patterns.deep_agent.da_pool import AgentPool
from agentflow.patterns.deep_agent.da_progress import ProgressManager
from agentflow.patterns.deep_agent.da_stores import (
    EvolutionStore,
    MemoryEvolutionStore,
    MemoryRuntimeStore,
    RuntimeStore,
)


_logger = logging.getLogger(__name__)


class DeepAgentCoordinator:
    """DeepAgent協調器.

    DeepAgentsフレームワークの核心実装。
    複雑なタスクを自律的に分解・実行・検証する。

    Example:
        >>> coordinator = DeepAgentCoordinator(llm_client=my_llm)
        >>> result = await coordinator.execute("市場調査レポートを作成して")
    """

    # 品質閾値
    QUALITY_THRESHOLD = 70.0
    MAX_RETRY = 3

    def __init__(
        self,
        llm_client: Any = None,
        runtime_store: RuntimeStore | None = None,
        evolution_store: EvolutionStore | None = None,
        tools: list[Any] | None = None,
        skills: list[Any] | None = None,
        max_parallel: int = 5,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            runtime_store: 実行時ストア
            evolution_store: 進化ストア
            tools: 利用可能ツール
            skills: 利用可能スキル
            max_parallel: 最大並行数
        """
        self._llm = llm_client
        self._runtime_store = runtime_store or MemoryRuntimeStore()
        self._evolution_store = evolution_store or MemoryEvolutionStore()
        self._agent_pool = AgentPool(llm_client, tools, skills)
        self._progress = ProgressManager()
        self._compressor = ContextCompressor(llm_client)
        self._evolver = SelfEvolver(self._evolution_store)
        self._max_parallel = max_parallel
        self._context: dict[str, Any] = {}

    # =========================================================================
    # メイン実行フロー
    # =========================================================================

    async def execute(self, request: str, context: dict[str, Any] | None = None) -> dict[str, Any]:
        """タスクを実行.

        Args:
            request: ユーザーリクエスト
            context: 追加コンテキスト

        Returns:
            実行結果
        """
        self._context = context or {}
        _logger.info("DeepAgent実行開始: %s", request[:100])

        try:
            # Phase 1: 認知分析
            analysis = await self._cognitive_analysis(request)
            if not analysis.is_clear and analysis.clarification_needed:
                return {
                    "status": "clarification_needed",
                    "questions": analysis.clarification_needed,
                    "analysis": analysis.model_dump(),
                }

            # Phase 2: パターン検索（自己進化）
            similar_pattern = await self._evolver.find_similar_pattern(analysis)

            # Phase 3: タスク分解
            if similar_pattern:
                todos = await self._evolver.apply_pattern(similar_pattern, self._context)
                _logger.info("既存パターン適用: %d個のTodo", len(todos))
            else:
                todos = await self._decompose_task(request, analysis)

            # Phase 4: 進捗登録
            self._progress.register_todos(todos)

            # Phase 5: 並行実行
            results = await self._execute_todos(todos)

            # Phase 6: 品質評審
            review = await self._quality_review(todos, results, analysis)

            # Phase 7: 再実行（必要に応じて）
            retry_count = 0
            while not review.is_acceptable and retry_count < self.MAX_RETRY:
                retry_count += 1
                _logger.info("品質不合格、再実行 (%d/%d)", retry_count, self.MAX_RETRY)
                results = await self._retry_failed_tasks(todos, review)
                review = await self._quality_review(todos, results, analysis)

            # Phase 8: 自己進化（成功時）
            if review.is_acceptable:
                await self._evolver.learn_success_pattern(todos, results, analysis, review)

            # Phase 9: 最終レポート生成
            final_report = await self._generate_report(todos, results, review)

            return {
                "status": "completed" if review.is_acceptable else "partial",
                "report": final_report,
                "results": results,
                "review": review.model_dump(),
                "stats": self._progress.get_stats(),
            }

        except Exception as e:
            _logger.exception("DeepAgent実行エラー")
            return {"status": "error", "error": str(e)}

    # =========================================================================
    # Phase 1: 認知分析
    # =========================================================================

    async def _cognitive_analysis(self, request: str) -> CognitiveAnalysis:
        """タスクの認知分析を実行.

        LLMを使用してユーザー意図、複雑度、必要なAgentを分析。
        """
        if not self._llm:
            # LLMなしの場合はデフォルト分析
            return CognitiveAnalysis(
                intent=request,
                is_clear=True,
                complexity="medium",
                domains=["general"],
                suggested_agents=[AgentType.EXECUTION.value],
            )

        prompt = f"""以下のユーザーリクエストを分析してください。

リクエスト: {request}

以下の形式でJSON出力してください:
{{
    "intent": "ユーザーの意図（1文で）",
    "is_clear": true/false,
    "clarification_needed": ["不明点があれば質問"],
    "complexity": "low/medium/high",
    "domains": ["関連領域"],
    "suggested_agents": ["research", "analysis", "planning", "execution", "review", "report"]
}}"""

        try:
            response = await self._llm.generate(prompt)
            import json
            data = json.loads(response)
            return CognitiveAnalysis(**data)
        except Exception as e:
            _logger.warning("認知分析失敗: %s", e)
            return CognitiveAnalysis(
                intent=request,
                is_clear=True,
                complexity="medium",
                domains=["general"],
                suggested_agents=[AgentType.EXECUTION.value],
            )

    # =========================================================================
    # Phase 3: タスク分解
    # =========================================================================

    async def _decompose_task(
        self,
        request: str,
        analysis: CognitiveAnalysis,
    ) -> list[TodoItem]:
        """タスクをTodoListに分解."""
        if not self._llm:
            # LLMなしの場合はシンプルな分解
            return self._simple_decompose(request, analysis)

        prompt = f"""以下のタスクをステップに分解してください。

タスク: {request}
意図: {analysis.intent}
複雑度: {analysis.complexity}
推薦Agent: {analysis.suggested_agents}

以下の形式でJSON配列を出力:
[
    {{
        "task": "具体的なタスク説明",
        "agent_type": "research/analysis/planning/execution/review/report",
        "priority": 0-10,
        "dependencies": [],
        "tools": [],
        "skills": []
    }}
]"""

        try:
            response = await self._llm.generate(prompt)
            import json
            data = json.loads(response)
            return [TodoItem(**item) for item in data]
        except Exception as e:
            _logger.warning("タスク分解失敗: %s", e)
            return self._simple_decompose(request, analysis)

    def _simple_decompose(
        self,
        request: str,
        analysis: CognitiveAnalysis,
    ) -> list[TodoItem]:
        """シンプルなタスク分解（LLMなし）."""
        todos = []

        # 複雑度に応じた分解
        if analysis.complexity == "low":
            todos.append(TodoItem(
                task=request,
                agent_type=AgentType.EXECUTION.value,
                priority=5,
            ))
        else:
            # 標準的な分解パターン
            if "research" in analysis.suggested_agents:
                todos.append(TodoItem(
                    task=f"調査: {request}",
                    agent_type=AgentType.RESEARCH.value,
                    priority=10,
                ))

            if "analysis" in analysis.suggested_agents:
                todos.append(TodoItem(
                    task=f"分析: {request}",
                    agent_type=AgentType.ANALYSIS.value,
                    priority=8,
                    dependencies=[todos[-1].id] if todos else [],
                ))

            todos.append(TodoItem(
                task=f"実行: {request}",
                agent_type=AgentType.EXECUTION.value,
                priority=5,
                dependencies=[todos[-1].id] if todos else [],
            ))

            if "review" in analysis.suggested_agents:
                todos.append(TodoItem(
                    task=f"検証: {request}",
                    agent_type=AgentType.REVIEW.value,
                    priority=3,
                    dependencies=[todos[-1].id] if todos else [],
                ))

        return todos

    # =========================================================================
    # Phase 5: 並行実行
    # =========================================================================

    async def _execute_todos(self, todos: list[TodoItem]) -> dict[str, Any]:
        """TodoListを並行実行."""
        results: dict[str, Any] = {}
        completed_ids: set[str] = set()

        while True:
            # 実行可能なタスクを取得
            ready_todos = [t for t in todos if t.is_ready(completed_ids) and t.status == TaskStatus.PENDING]
            if not ready_todos:
                break

            # 並行実行（最大数制限）
            batch = ready_todos[:self._max_parallel]
            _logger.info("並行実行: %d個のタスク", len(batch))

            tasks = []
            for todo in batch:
                self._progress.mark_started(todo.id)
                agent = self._agent_pool.get_agent(todo.agent_type)
                tasks.append(self._execute_single_todo(todo, agent))

            batch_results = await asyncio.gather(*tasks, return_exceptions=True)

            for todo, result in zip(batch, batch_results, strict=False):
                if isinstance(result, Exception):
                    self._progress.mark_failed(todo.id, str(result))
                    results[todo.id] = {"status": "failed", "error": str(result)}
                else:
                    self._progress.mark_completed(todo.id, result)
                    results[todo.id] = result
                    completed_ids.add(todo.id)

        return results

    async def _execute_single_todo(
        self,
        todo: TodoItem,
        agent: Any,
    ) -> dict[str, Any]:
        """単一タスクを実行."""
        try:
            return await agent.execute(todo, self._context)
        except Exception as e:
            _logger.warning("タスク実行失敗 [%s]: %s", todo.id, e)
            raise

    # =========================================================================
    # Phase 6: 品質評審
    # =========================================================================

    async def _quality_review(
        self,
        todos: list[TodoItem],
        results: dict[str, Any],
        analysis: CognitiveAnalysis,
    ) -> QualityReview:
        """品質評審を実行."""
        if not self._llm:
            return self._simple_quality_review(todos, results)

        # LLMによる品質評価
        prompt = f"""以下のタスク実行結果を評価してください。

元のリクエスト意図: {analysis.intent}

実行結果:
{results}

以下の形式でJSON出力:
{{
    "is_acceptable": true/false,
    "score": 0-100,
    "issues": ["問題点"],
    "suggestions": ["改善提案"],
    "retry_tasks": ["再実行すべきタスクID"],
    "dimension_scores": {{
        "completeness": 0-100,
        "accuracy": 0-100,
        "consistency": 0-100,
        "efficiency": 0-100,
        "clarity": 0-100
    }},
    "verdict": "pass/revise/reject"
}}"""

        try:
            response = await self._llm.generate(prompt)
            import json
            data = json.loads(response)
            return QualityReview(**data)
        except Exception as e:
            _logger.warning("品質評審失敗: %s", e)
            return self._simple_quality_review(todos, results)

    def _simple_quality_review(
        self,
        todos: list[TodoItem],
        results: dict[str, Any],
    ) -> QualityReview:
        """シンプルな品質評審（LLMなし）."""
        completed = sum(1 for t in todos if t.status == TaskStatus.COMPLETED)
        failed = sum(1 for t in todos if t.status == TaskStatus.FAILED)
        total = len(todos)

        completion_rate = completed / total if total > 0 else 0
        score = completion_rate * 100

        return QualityReview(
            is_acceptable=score >= self.QUALITY_THRESHOLD,
            score=score,
            issues=[f"{failed}個のタスクが失敗"] if failed > 0 else [],
            suggestions=[],
            retry_tasks=[t.id for t in todos if t.status == TaskStatus.FAILED],
            dimension_scores={
                QualityDimension.COMPLETENESS.value: completion_rate * 100,
                QualityDimension.ACCURACY.value: 80.0,
                QualityDimension.CONSISTENCY.value: 80.0,
                QualityDimension.EFFICIENCY.value: 80.0,
                QualityDimension.CLARITY.value: 80.0,
            },
            verdict="pass" if score >= self.QUALITY_THRESHOLD else "revise",
        )

    # =========================================================================
    # Phase 7: 再実行
    # =========================================================================

    async def _retry_failed_tasks(
        self,
        todos: list[TodoItem],
        review: QualityReview,
    ) -> dict[str, Any]:
        """失敗タスクを再実行."""
        retry_todos = [t for t in todos if t.id in review.retry_tasks]
        for todo in retry_todos:
            todo.status = TaskStatus.PENDING
            todo.retry_count += 1

        return await self._execute_todos(retry_todos)

    # =========================================================================
    # Phase 9: レポート生成
    # =========================================================================

    async def _generate_report(
        self,
        todos: list[TodoItem],
        results: dict[str, Any],  # noqa: ARG002 - 将来の拡張用
        review: QualityReview,
    ) -> str:
        """最終レポートを生成.

        Note:
            asyncは将来のLLM統合のために保持。
        """
        stats = self._progress.get_stats()

        report_lines = [
            "# DeepAgent 実行レポート",
            "",
            "## 概要",
            f"- 総タスク数: {stats['total']}",
            f"- 完了: {stats['completed']}",
            f"- 失敗: {stats['failed']}",
            f"- 進捗率: {stats['progress_percent']:.1f}%",
            f"- 品質スコア: {review.score:.1f}",
            "",
            "## タスク詳細",
        ]

        for todo in todos:
            status_icon = "✅" if todo.status == TaskStatus.COMPLETED else "❌"
            report_lines.append(f"- {status_icon} [{todo.agent_type}] {todo.task}")

        if review.issues:
            report_lines.extend(["", "## 問題点"])
            for issue in review.issues:
                report_lines.append(f"- {issue}")

        if review.suggestions:
            report_lines.extend(["", "## 改善提案"])
            for suggestion in review.suggestions:
                report_lines.append(f"- {suggestion}")

        return "\n".join(report_lines)

    # =========================================================================
    # コンテキスト圧縮
    # =========================================================================

    async def compress_context(
        self,
        max_tokens: int = 4000,
        strategy: CompactionStrategy = CompactionStrategy.HYBRID,
    ) -> None:
        """コンテキストを圧縮."""
        messages = self._agent_pool.get_message_history()
        _, result = await self._compressor.compact_messages(
            messages, max_tokens, strategy
        )
        _logger.info(
            "コンテキスト圧縮: %d → %d トークン (%.1f%%)",
            result.original_tokens,
            result.compressed_tokens,
            result.compression_ratio * 100,
        )


# =============================================================================
# エクスポート
# =============================================================================

__all__ = ["DeepAgentCoordinator"]

