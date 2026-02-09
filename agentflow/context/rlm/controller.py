"""RLM Controller - メインオーケストレーションループ.

RLM (Recursive Language Model) のメインコントローラー。
長いコンテキストを外部変数として扱い、選択的読み取りで処理する。

処理フロー:
1. コンテキストを保存 → ハンドル取得
2. ループ開始
   a. 次のアクションを計画（LLM or ヒューリスティック）
   b. アクション実行（peek/search/summarize/extract）
   c. ワークスペースを更新
   d. 信頼度を評価
3. 停止条件チェック
4. 最終回答を合成

停止条件:
- ANSWER_FOUND: 信頼度 >= 閾値
- BUDGET_EXHAUSTED: Token予算枯渇
- MAX_ITERATIONS: 最大ループ数到達
- CONVERGENCE: 新情報なし

使用例:
    >>> controller = RLMController(config, llm_client)
    >>> result = await controller.run(
    ...     query="認証要件は？",
    ...     long_inputs=[api_spec_document],
    ... )
    >>> print(result.answer)
"""

from __future__ import annotations

import logging
import time
import uuid
from dataclasses import dataclass, field
from typing import Any

from agentflow.context.rlm.config import (
    ActionType,
    RLMConfig,
    StopReason,
    SubCallBudget,
)
from agentflow.context.rlm.context_ops import ContextOps
from agentflow.context.rlm.context_store import ContextHandle, ContextStore
from agentflow.context.rlm.events import RLMEventEmitter
from agentflow.context.rlm.subcall import LLMClientProtocol, SubCallManager
from agentflow.context.rlm.workspace import VariableType, Workspace


@dataclass
class RLMResult:
    """RLM処理結果.

    Attributes:
        success: 成功フラグ
        answer: 最終回答
        confidence: 回答の信頼度
        stop_reason: 停止理由
        iterations: 実行イテレーション数
        subcalls: 実行サブコール数
        tokens_used: 消費Token数
        evidence: 根拠リスト
        metadata: 追加メタデータ
    """

    success: bool
    answer: str
    confidence: float
    stop_reason: StopReason
    iterations: int
    subcalls: int
    tokens_used: int
    evidence: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


class RLMController:
    """RLMメインコントローラー.

    長いコンテキストを外部変数として扱い、
    選択的読み取りとセマンティック操作で処理する。

    使用例:
        >>> config = RLMConfig(activation_threshold=15000)
        >>> controller = RLMController(config, llm_client)
        >>>
        >>> # イベントコールバック設定（オプション）
        >>> controller.set_event_callback(lambda e: print(e))
        >>>
        >>> # 実行
        >>> result = await controller.run(
        ...     query="APIの認証要件を教えて",
        ...     long_inputs=[api_specification],
        ... )
        >>>
        >>> if result.success:
        ...     print(result.answer)
        ...     print(f"Confidence: {result.confidence}")
    """

    def __init__(
        self,
        config: RLMConfig | None = None,
        llm_client: LLMClientProtocol | None = None,
    ) -> None:
        """初期化.

        Args:
            config: RLM設定
            llm_client: LLMクライアント
        """
        self._config = config or RLMConfig()
        self._llm_client = llm_client
        self._logger = logging.getLogger(__name__)

        # コンポーネント初期化
        self._store = ContextStore()
        self._workspace = Workspace(
            capacity=self._config.workspace_capacity,
            token_budget=self._config.workspace_token_budget,
        )
        self._ops = ContextOps(self._store)
        self._budget = SubCallBudget.from_config(self._config)
        self._subcall = SubCallManager(llm_client, self._budget)

        # 状態
        self._flow_id = ""
        self._event_emitter: RLMEventEmitter | None = None
        self._current_confidence = 0.0
        self._confidence_history: list[float] = []

    def set_llm_client(self, llm_client: LLMClientProtocol) -> None:
        """LLMクライアントを設定.

        Args:
            llm_client: LLMクライアント
        """
        self._llm_client = llm_client
        self._subcall.set_llm_client(llm_client)

    def set_event_callback(self, callback: Any) -> None:
        """イベントコールバックを設定.

        Args:
            callback: イベントコールバック関数
        """
        if self._event_emitter:
            self._event_emitter.set_callback(callback)

    async def run(
        self,
        query: str,
        long_inputs: list[str] | None = None,
        metadata: dict[str, Any] | None = None,
        event_callback: Any = None,
    ) -> RLMResult:
        """RLM処理を実行.

        Args:
            query: ユーザークエリ
            long_inputs: 長いコンテキストリスト
            metadata: 追加メタデータ
            event_callback: イベントコールバック

        Returns:
            RLMResult
        """
        long_inputs = long_inputs or []
        self._flow_id = f"rlm_{uuid.uuid4().hex[:8]}"

        # イベントエミッター初期化
        self._event_emitter = RLMEventEmitter(
            flow_id=self._flow_id,
            callback=event_callback,
            enabled=self._config.emit_events,
        )

        # 状態リセット
        self._reset_state()

        try:
            # 1. コンテキストを保存
            handles = await self._store_contexts(long_inputs, metadata)
            total_tokens = sum(h.total_tokens for h in handles)

            # 開始イベント
            self._event_emitter.emit_start(
                total_contexts=len(handles),
                total_tokens=total_tokens,
                activation_reason=f"Total tokens: {total_tokens}",
                config_summary={
                    "max_iterations": self._config.max_iterations,
                    "max_subcalls": self._config.max_subcalls,
                    "convergence_threshold": self._config.convergence_threshold,
                },
            )

            # 2. メインループ
            result = await self._main_loop(query, handles)

            # 完了イベント
            answer_preview = result.answer[:100] if result.answer else ""
            self._event_emitter.emit_complete(
                stop_reason=result.stop_reason.value,
                total_iterations=result.iterations,
                total_subcalls=result.subcalls,
                total_tokens_used=result.tokens_used,
                final_confidence=result.confidence,
                has_answer=bool(result.answer),
                answer_preview=answer_preview,
            )

            return result

        except Exception as e:
            self._logger.exception("RLM execution failed")
            self._event_emitter.emit_error(
                error_message=str(e),
                error_type=type(e).__name__,
                recoverable=False,
            )

            return RLMResult(
                success=False,
                answer="",
                confidence=0.0,
                stop_reason=StopReason.ERROR,
                iterations=0,
                subcalls=self._budget.calls_made,
                tokens_used=self._budget.tokens_used,
                metadata={"error": str(e)},
            )

    def _reset_state(self) -> None:
        """状態をリセット."""
        self._store.clear()
        self._workspace.clear()
        self._budget = SubCallBudget.from_config(self._config)
        self._subcall.set_budget(self._budget)
        self._current_confidence = 0.0
        self._confidence_history = []

    async def _store_contexts(
        self,
        long_inputs: list[str],
        metadata: dict[str, Any] | None,
    ) -> list[ContextHandle]:
        """コンテキストを保存.

        Args:
            long_inputs: 長いコンテキストリスト
            metadata: メタデータ

        Returns:
            ハンドルリスト
        """
        handles = []
        for i, content in enumerate(long_inputs):
            handle = await self._store.store(
                content,
                metadata={
                    "index": i,
                    **(metadata or {}),
                },
            )
            handles.append(handle)
        return handles

    async def _main_loop(
        self,
        query: str,
        handles: list[ContextHandle],
    ) -> RLMResult:
        """メインループを実行.

        Args:
            query: クエリ
            handles: コンテキストハンドル

        Returns:
            RLMResult
        """
        iteration = 0

        # 初期アクション: 構造取得
        for handle in handles:
            await self._execute_action(
                ActionType.GET_STRUCTURE,
                {"handle_id": handle.handle_id},
            )

        while iteration < self._config.max_iterations:
            iteration += 1

            # イテレーションイベント
            if self._event_emitter:
                self._event_emitter.emit_iteration(
                    iteration=iteration,
                    max_iterations=self._config.max_iterations,
                    confidence=self._current_confidence,
                    action_planned="planning...",
                    budget_remaining={
                        "tokens": self._budget.remaining_tokens,
                        "calls": self._budget.remaining_calls,
                    },
                )

            # 停止条件チェック
            stop_reason = self._check_stop_conditions(iteration)
            if stop_reason:
                break

            # 次のアクションを計画
            action, params = await self._plan_next_action(query, handles)

            if action == ActionType.SYNTHESIZE:
                # 最終回答を合成
                answer = await self._synthesize_answer(query)
                return RLMResult(
                    success=True,
                    answer=answer,
                    confidence=self._current_confidence,
                    stop_reason=StopReason.ANSWER_FOUND,
                    iterations=iteration,
                    subcalls=self._budget.calls_made,
                    tokens_used=self._budget.tokens_used,
                    evidence=self._workspace.get_evidence(),
                )

            if action == ActionType.GIVE_UP:
                # 回答不可
                return RLMResult(
                    success=False,
                    answer="Unable to find sufficient information to answer the query.",
                    confidence=self._current_confidence,
                    stop_reason=StopReason.CONVERGENCE,
                    iterations=iteration,
                    subcalls=self._budget.calls_made,
                    tokens_used=self._budget.tokens_used,
                )

            # アクション実行
            await self._execute_action(action, params)

            # 信頼度評価
            await self._assess_confidence(query)

        # 最大イテレーション到達
        # 現状のワークスペースから回答を合成
        answer = await self._synthesize_answer(query)

        return RLMResult(
            success=bool(answer),
            answer=answer,
            confidence=self._current_confidence,
            stop_reason=stop_reason or StopReason.MAX_ITERATIONS,
            iterations=iteration,
            subcalls=self._budget.calls_made,
            tokens_used=self._budget.tokens_used,
            evidence=self._workspace.get_evidence(),
        )

    def _check_stop_conditions(self, iteration: int) -> StopReason | None:
        """停止条件をチェック.

        Args:
            iteration: 現在のイテレーション

        Returns:
            停止理由、または None（続行）
        """
        # 信頼度閾値
        if self._current_confidence >= self._config.convergence_threshold:
            return StopReason.ANSWER_FOUND

        # 予算枯渇
        if self._budget.is_exhausted:
            return StopReason.BUDGET_EXHAUSTED

        # 収束チェック（信頼度の変化が小さい）
        if len(self._confidence_history) >= self._config.stagnation_rounds:
            recent = self._confidence_history[-self._config.stagnation_rounds :]
            max_delta = max(abs(recent[i] - recent[i - 1]) for i in range(1, len(recent)))
            if max_delta < self._config.min_confidence_delta:
                return StopReason.CONVERGENCE

        return None

    async def _plan_next_action(
        self,
        query: str,
        handles: list[ContextHandle],
    ) -> tuple[ActionType, dict[str, Any]]:
        """次のアクションを計画.

        Args:
            query: クエリ
            handles: ハンドル

        Returns:
            (アクションタイプ, パラメータ)
        """
        # 信頼度が高ければ合成
        if self._current_confidence >= self._config.convergence_threshold:
            return ActionType.SYNTHESIZE, {}

        # 決定的操作を優先
        if self._config.prefer_deterministic:
            # まだキーワード検索していないハンドルがあれば
            searched_handles = {
                v.metadata.get("handle_id")
                for v in self._workspace.list_variables(VariableType.SEARCH_RESULT)
            }
            unsearched = [h for h in handles if h.handle_id not in searched_handles]

            if unsearched:
                # クエリからキーワードを抽出
                keywords = self._extract_keywords(query)
                return ActionType.KEYWORD_FIND, {
                    "handle_id": unsearched[0].handle_id,
                    "keywords": keywords,
                    "operator": "OR",
                }

        # LLMに計画を依頼
        if self._llm_client and self._budget.can_afford(500):
            handle_dicts = [h.to_dict() for h in handles]
            workspace_summary = self._workspace.to_context_string(max_tokens=1000)

            result = await self._subcall.plan_next_action(
                query=query,
                context_handles=handle_dicts,
                workspace_summary=workspace_summary,
                available_actions=[a.value for a in ActionType],
            )

            if result.success and isinstance(result.content, dict):
                action_str = result.content.get("action", "peek")
                try:
                    action = ActionType(action_str)
                    params = result.content.get("parameters", {})
                    return action, params
                except ValueError:
                    pass

        # フォールバック: 先頭からpeek
        if handles:
            return ActionType.PEEK, {
                "handle_id": handles[0].handle_id,
                "start_line": 0,
                "num_lines": self._config.chunk_size,
            }

        return ActionType.GIVE_UP, {}

    def _extract_keywords(self, query: str) -> list[str]:
        """クエリからキーワードを抽出.

        Args:
            query: クエリ

        Returns:
            キーワードリスト
        """
        import re

        # ストップワードを除去
        stop_words = {
            "の",
            "は",
            "が",
            "を",
            "に",
            "で",
            "と",
            "から",
            "まで",
            "a",
            "an",
            "the",
            "is",
            "are",
            "was",
            "were",
            "be",
            "what",
            "how",
            "when",
            "where",
            "which",
            "who",
            "について",
            "とは",
            "教えて",
            "ください",
        }

        # 単語を抽出
        words = re.findall(r"[\w]+", query.lower())
        keywords = [w for w in words if w not in stop_words and len(w) > 1]

        return keywords[:5]  # 最大5キーワード

    async def _execute_action(
        self,
        action: ActionType,
        params: dict[str, Any],
    ) -> None:
        """アクションを実行.

        Args:
            action: アクションタイプ
            params: パラメータ
        """
        start_time = time.time()
        result: dict[str, Any] = {}

        if action == ActionType.PEEK:
            result = self._ops.ctx_peek(
                handle_id=params.get("handle_id", ""),
                start_line=params.get("start_line", 0),
                num_lines=params.get("num_lines", 100),
            )
            if result.get("success"):
                self._workspace.set(
                    f"peek_{params.get('handle_id', '')}_{params.get('start_line', 0)}",
                    result.get("content", ""),
                    var_type=VariableType.TEMP,
                    metadata={"action": "peek", **params},
                )

        elif action == ActionType.REGEX_FIND:
            result = self._ops.ctx_regex_find(
                handle_id=params.get("handle_id", ""),
                pattern=params.get("pattern", ""),
                max_matches=params.get("max_matches", 20),
            )
            if result.get("success") and result.get("matches"):
                self._workspace.set(
                    f"regex_{params.get('pattern', '')[:20]}",
                    result.get("matches", []),
                    var_type=VariableType.SEARCH_RESULT,
                    metadata={
                        "action": "regex_find",
                        "handle_id": params.get("handle_id"),
                        **params,
                    },
                )

        elif action == ActionType.KEYWORD_FIND:
            result = self._ops.ctx_keyword_find(
                handle_id=params.get("handle_id", ""),
                keywords=params.get("keywords", []),
                operator=params.get("operator", "OR"),
                max_matches=params.get("max_matches", 20),
            )
            if result.get("success") and result.get("matches"):
                self._workspace.set(
                    f"keyword_{params.get('handle_id', '')}",
                    result.get("matches", []),
                    var_type=VariableType.SEARCH_RESULT,
                    metadata={
                        "action": "keyword_find",
                        "handle_id": params.get("handle_id"),
                        **params,
                    },
                )

        elif action == ActionType.GET_STRUCTURE:
            result = self._ops.ctx_get_structure(
                handle_id=params.get("handle_id", ""),
            )
            if result.get("success"):
                self._workspace.set(
                    f"structure_{params.get('handle_id', '')}",
                    result.get("outline", ""),
                    var_type=VariableType.SUMMARY,
                    metadata={"action": "get_structure", "handle_id": params.get("handle_id")},
                )

        elif action == ActionType.SUMMARIZE:
            content = params.get("content", "")
            focus = params.get("focus", "")
            subcall_result = await self._subcall.summarize_section(content, focus)
            if subcall_result.success:
                self._workspace.set(
                    f"summary_{len(self._workspace.list_variables(VariableType.SUMMARY))}",
                    subcall_result.content,
                    var_type=VariableType.SUMMARY,
                    metadata={"action": "summarize", "focus": focus},
                )

        elif action == ActionType.EXTRACT:
            content = params.get("content", "")
            spec = params.get("extraction_spec", "")
            subcall_result = await self._subcall.extract_information(content, spec)
            if subcall_result.success:
                self._workspace.set(
                    f"extract_{len(self._workspace.list_variables(VariableType.EXTRACT))}",
                    subcall_result.content,
                    var_type=VariableType.EXTRACT,
                    metadata={"action": "extract", "spec": spec},
                )

        # アクションイベント
        execution_time = (time.time() - start_time) * 1000
        if self._event_emitter:
            self._event_emitter.emit_action(
                action_type=action.value,
                target_handle=params.get("handle_id", ""),
                parameters=params,
                result_count=len(result.get("matches", [])) if "matches" in result else 1,
                execution_time_ms=execution_time,
            )

    async def _assess_confidence(self, query: str) -> None:
        """信頼度を評価.

        Args:
            query: クエリ
        """
        # 簡易評価: ワークスペースの充実度
        summaries = self._workspace.list_variables(VariableType.SUMMARY)
        extracts = self._workspace.list_variables(VariableType.EXTRACT)
        search_results = self._workspace.list_variables(VariableType.SEARCH_RESULT)

        # 基本スコア
        base_score = 0.0

        if summaries:
            base_score += 0.3
        if extracts:
            base_score += 0.3
        if search_results:
            base_score += 0.2

        # 検索結果の質
        total_matches = sum(
            len(v.value) if isinstance(v.value, list) else 1 for v in search_results
        )
        if total_matches > 5:
            base_score += 0.1
        if total_matches > 10:
            base_score += 0.1

        # LLMによる評価（予算があれば）
        if self._llm_client and self._budget.can_afford(300) and base_score > 0.3:
            current_answer = self._workspace.get_final_answer() or ""
            if not current_answer:
                # 暫定回答を生成
                workspace_content = self._workspace.to_context_string(max_tokens=2000)
                current_answer = f"Based on gathered information: {workspace_content[:500]}"

            evidence = [str(v.value)[:500] for v in search_results[:3]]
            result = await self._subcall.assess_confidence(
                query=query,
                current_answer=current_answer,
                evidence=evidence,
            )

            if result.success and isinstance(result.content, dict):
                llm_confidence = result.content.get("confidence", 0.5)
                # 基本スコアとLLM評価を平均
                self._current_confidence = (base_score + llm_confidence) / 2
            else:
                self._current_confidence = base_score
        else:
            self._current_confidence = base_score

        self._confidence_history.append(self._current_confidence)

    async def _synthesize_answer(self, query: str) -> str:
        """最終回答を合成.

        Args:
            query: クエリ

        Returns:
            回答文字列
        """
        # ワークスペースから情報を収集
        workspace_content = self._workspace.to_context_string(max_tokens=5000)
        evidence = self._workspace.get_evidence()

        # 検索結果も追加
        search_results = self._workspace.list_variables(VariableType.SEARCH_RESULT)
        for sr in search_results[:3]:
            if isinstance(sr.value, list):
                for match in sr.value[:3]:
                    if isinstance(match, dict):
                        evidence.append(match.get("context", match.get("line", "")))

        # LLMで合成
        if self._llm_client and self._budget.can_afford(2000):
            result = await self._subcall.synthesize_answer(
                query=query,
                workspace_content=workspace_content,
                evidence=evidence,
            )

            if result.success:
                # 最終回答をワークスペースに保存
                self._workspace.set(
                    "final_answer",
                    result.content,
                    var_type=VariableType.FINAL,
                )
                return str(result.content)

        # フォールバック: ワークスペースの内容をそのまま返す
        return f"Based on the analysis:\n\n{workspace_content[:3000]}"

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計辞書
        """
        return {
            "store": self._store.get_stats(),
            "workspace": self._workspace.get_stats(),
            "budget": self._budget.to_dict(),
            "confidence": self._current_confidence,
            "confidence_history": self._confidence_history,
        }
