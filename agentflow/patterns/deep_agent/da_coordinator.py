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
from contextlib import suppress
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
        max_iterations: int = 10,
        max_retries: int = 3,
        quality_threshold: float = 70.0,
        enable_evolution: bool = True,
        enable_memory: bool = True,
        on_progress: Any = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            runtime_store: 実行時ストア
            evolution_store: 進化ストア
            tools: 利用可能ツール
            skills: 利用可能スキル
            max_parallel: 最大並行数
            max_iterations: 最大イテレーション数
            max_retries: 最大リトライ数
            quality_threshold: 品質閾値
            enable_evolution: 進化を有効にするか
            enable_memory: メモリを有効にするか
            on_progress: 進捗コールバック
        """
        self._llm = llm_client
        self._runtime_store = runtime_store or MemoryRuntimeStore()
        self._evolution_store = evolution_store or MemoryEvolutionStore()
        self._agent_pool = AgentPool(llm_client, tools, skills)
        self._progress = ProgressManager()
        self._compressor = ContextCompressor(llm_client)
        self._evolver = SelfEvolver(self._evolution_store)
        self._max_parallel = max_parallel
        self._max_iterations = max_iterations
        self._max_retries = max_retries
        self._quality_threshold = quality_threshold
        self._enable_evolution = enable_evolution
        self._enable_memory = enable_memory
        self._on_progress = on_progress
        self._context: dict[str, Any] = {}

    @property
    def pattern(self) -> Any:
        """協調パターンを返す."""
        from agentflow.patterns.coordinator import CoordinationPattern

        return CoordinationPattern.HIERARCHICAL

    def register_agent(self, name: str, agent: Any) -> None:
        """カスタムAgentをプールに登録.

        Args:
            name: Agent名
            agent: Agentインスタンス
        """
        self._agent_pool.register_agent(name, agent)

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            progress/available_agents/evolution を含む辞書
        """
        return {
            "progress": self._progress.get_progress(),
            "available_agents": self._agent_pool.list_agents(),
            "evolution": {
                "enabled": self._enable_evolution,
                "max_iterations": self._max_iterations,
            },
        }

    async def process_feedback(
        self,
        feedback_type: str,
        content: str,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """フィードバックを処理.

        Args:
            feedback_type: フィードバック種別
            content: フィードバック内容

        Returns:
            処理結果
        """
        if not self._enable_evolution:
            return {"status": "evolution_disabled"}

        try:
            from agentflow.patterns.deep_agent.da_models import EvolutionRecord

            record = EvolutionRecord(
                event_type=feedback_type,
                pattern=content,
            )
            await self._evolution_store.save_feedback(record)
            return {"status": "processed", "feedback_type": feedback_type}
        except Exception as e:
            _logger.warning("フィードバック処理エラー: %s", e)
            return {"status": "processed", "feedback_type": feedback_type}

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
        import time as _time_mod

        self._context = context or {}
        _logger.info("DeepAgent実行開始: %s", request[:100])
        _start_time = _time_mod.monotonic()

        def _emit_progress(event: dict[str, Any]) -> None:
            if self._on_progress:
                with suppress(Exception):
                    self._on_progress(event)

        _emit_progress({"event": "start", "request": request[:100]})

        try:
            # Phase 1: 認知分析
            analysis = await self._cognitive_analysis(request)
            _emit_progress({"event": "cognitive_analysis", "analysis": analysis.model_dump()})
            if not analysis.is_clear and analysis.clarification_needed:
                return {
                    "status": "clarification_needed",
                    "questions": analysis.clarification_needed,
                    "analysis": analysis.model_dump(),
                }

            # Phase 2: パターン検索（自己進化、必要時のみ）
            similar_pattern = None
            if self._should_lookup_pattern(analysis):
                similar_pattern = await self._evolver.find_similar_pattern(analysis)
            else:
                _logger.debug("パターン検索をスキップ: complexity=%s", analysis.complexity)

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

            progress = self._progress.get_progress()
            _emit_progress({"event": "complete", "progress": progress})
            return {
                "status": "success" if review.is_acceptable else "partial",
                "report": final_report,
                "results": results,
                "review": review.model_dump(),
                "stats": self._progress.get_stats(),
                "progress": progress,
                "execution_time": _time_mod.monotonic() - _start_time,
            }

        except Exception as e:
            _logger.exception("DeepAgent実行エラー")
            _emit_progress({"event": "error", "error": str(e)})
            return {
                "status": "error",
                "error": str(e),
                "progress": self._progress.get_progress(),
                "execution_time": _time_mod.monotonic() - _start_time,
            }

    def _should_lookup_pattern(self, analysis: CognitiveAnalysis) -> bool:
        """既存パターン検索の要否を判定."""
        if bool(self._context.get("force_pattern_lookup", False)):
            return True

        recent_failures = int(self._context.get("recent_failures", 0))
        if recent_failures >= 2:
            return True

        confidence = float(self._context.get("self_confidence", 0.6))
        novelty = float(self._context.get("task_novelty", 0.5))
        staleness_risk = str(self._context.get("experience_staleness_risk", "medium")).lower()

        if confidence >= 0.82 and analysis.complexity == "low" and novelty <= 0.40 and staleness_risk == "low":
            return False

        if confidence < 0.55 or analysis.complexity == "high" or novelty >= 0.65:
            return True

        return analysis.complexity != "low"

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
            todos.append(
                TodoItem(
                    task=request,
                    agent_type=AgentType.EXECUTION.value,
                    priority=5,
                )
            )
        else:
            # 標準的な分解パターン
            if "research" in analysis.suggested_agents:
                todos.append(
                    TodoItem(
                        task=f"調査: {request}",
                        agent_type=AgentType.RESEARCH.value,
                        priority=10,
                    )
                )

            if "analysis" in analysis.suggested_agents:
                todos.append(
                    TodoItem(
                        task=f"分析: {request}",
                        agent_type=AgentType.ANALYSIS.value,
                        priority=8,
                        dependencies=[todos[-1].id] if todos else [],
                    )
                )

            todos.append(
                TodoItem(
                    task=f"実行: {request}",
                    agent_type=AgentType.EXECUTION.value,
                    priority=5,
                    dependencies=[todos[-1].id] if todos else [],
                )
            )

            if "review" in analysis.suggested_agents:
                todos.append(
                    TodoItem(
                        task=f"検証: {request}",
                        agent_type=AgentType.REVIEW.value,
                        priority=3,
                        dependencies=[todos[-1].id] if todos else [],
                    )
                )

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
            batch = ready_todos[: self._max_parallel]
            _logger.info("並行実行: %d個のタスク", len(batch))

            tasks = []
            for todo in batch:
                self._progress.mark_started(todo.id)
                agent = self._agent_pool.get_agent(todo.agent_type)
                tasks.append(self._execute_single_todo(todo, agent))

            batch_results = await asyncio.gather(*tasks, return_exceptions=True)

            for todo, result in zip(batch, batch_results, strict=False):
                if isinstance(result, BaseException):
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
            result = await agent.execute(todo, self._context)
            if isinstance(result, dict):
                return result
            return {"result": result}
        except Exception as e:
            _logger.warning("タスク実行失敗 [%s]: %s", todo.id, e)
            raise

    # =========================================================================
    # Phase 6: 品質評審
    # =========================================================================

    async def _quality_review(
        self,
        todos: list[TodoItem] | str,
        results: dict[str, Any],
        analysis: CognitiveAnalysis | None = None,
    ) -> QualityReview:
        """品質評審を実行."""
        if not self._llm:
            return self._simple_quality_review(todos, results)

        # LLMによる品質評価
        intent_text = analysis.intent if analysis is not None else ""
        prompt = f"""以下のタスク実行結果を評価してください。

元のリクエスト意図: {intent_text}

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
        todos: list[TodoItem] | str,
        results: dict[str, Any],
    ) -> QualityReview:
        """シンプルな品質評審（LLMなし）."""
        # todosが文字列の場合はProgressManagerから取得
        todo_list = self._progress.get_all_todos() if isinstance(todos, str) else todos

        completed = sum(1 for t in todo_list if t.status == TaskStatus.COMPLETED)
        failed = sum(1 for t in todo_list if t.status == TaskStatus.FAILED)
        total = len(todo_list)

        completion_rate = completed / total if total > 0 else 0
        completeness_score = completion_rate * 100

        dimension_scores = {
            QualityDimension.COMPLETENESS.value: completeness_score,
            QualityDimension.ACCURACY.value: 70.0,
            QualityDimension.CONSISTENCY.value: 75.0,
            QualityDimension.EFFICIENCY.value: 70.0,
            QualityDimension.CLARITY.value: 70.0,
        }
        score = sum(dimension_scores.values()) / len(dimension_scores)

        return QualityReview(
            is_acceptable=score >= self.QUALITY_THRESHOLD,
            score=score,
            issues=[f"{failed}個のタスクが失敗"] if failed > 0 else [],
            suggestions=[],
            retry_tasks=[t.id for t in todo_list if t.status == TaskStatus.FAILED],
            dimension_scores=dimension_scores,
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
        results: dict[str, Any],
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
    # チェックポイント / リカバリ
    # =========================================================================

    async def save_checkpoint(
        self,
        phase: str = "execute",
        extra_state: dict[str, Any] | None = None,
    ) -> str:
        """チェックポイントを保存."""
        import uuid

        checkpoint_id = f"cp-{uuid.uuid4().hex[:8]}"
        state = {
            "phase": phase,
            "todos": [t.model_dump() for t in self._progress.get_all_todos()],
            "context": self._context,
            **(extra_state or {}),
        }
        await self._runtime_store.save_checkpoint(checkpoint_id, state)
        self.__checkpoint_id = checkpoint_id
        return checkpoint_id

    async def load_checkpoint(self, checkpoint_id: str) -> dict[str, Any] | None:
        """チェックポイントを読み込み."""
        return await self._runtime_store.load_checkpoint(checkpoint_id)

    async def list_checkpoints(self) -> list[str]:
        """チェックポイント一覧を返す."""
        checkpoints = await self._runtime_store.list_checkpoints()
        return list(checkpoints)

    async def resume_from_checkpoint(
        self,
        checkpoint_id: str,
        task: str,
    ) -> dict[str, Any]:
        """チェックポイントから再開."""
        state = await self._runtime_store.load_checkpoint(checkpoint_id)
        if state is None:
            msg = f"チェックポイントが見つかりません: {checkpoint_id}"
            raise ValueError(msg)

        todos_data = state.get("todos", [])
        for todo_data in todos_data:
            todo = TodoItem(**todo_data)
            self._progress.add_todo(todo)

        self._context = state.get("context", {})
        result = await self.execute(task)
        result["resumed_from"] = checkpoint_id
        return result

    @property
    def _current_checkpoint_id(self) -> str | None:
        """現在のチェックポイントID."""
        try:
            return self.__checkpoint_id
        except AttributeError:
            return None

    @_current_checkpoint_id.setter
    def _current_checkpoint_id(self, value: str) -> None:
        self.__checkpoint_id = value

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
        _, result = await self._compressor.compact_messages(messages, max_tokens, strategy)
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
