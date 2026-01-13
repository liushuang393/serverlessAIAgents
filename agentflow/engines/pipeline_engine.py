# -*- coding: utf-8 -*-
"""PipelineEngine - 複数Agent順次実行 + Reviewパターン.

最も複雑なEngine Pattern、以下に適用：
- マルチステップ承認フロー
- 意思決定エンジン
- 複数Agent協調 + レビューループ

フロー: Gate → [Agents...] → Review → (PASS) → Report
                            ↓ (REVISE)
                         ロールバックしてリトライ

内部実装：
    flow モジュールを活用してフローを構築・実行します。

使用例:
    >>> from agentflow.engines import PipelineEngine
    >>>
    >>> engine = PipelineEngine(
    ...     stages=[
    ...         {"name": "gate", "agent": GateAgent, "gate": True},
    ...         {"name": "analysis", "agents": [DaoAgent, FaAgent]},
    ...         {"name": "review", "agent": ReviewAgent, "review": True},
    ...     ],
    ...     max_revisions=2,
    ... )
    >>> result = await engine.run(inputs)

=============================================================================
継承ガイド
=============================================================================

PipelineEngine を継承してカスタムエンジンを作成する場合、2つの推奨パターンがあります：

パターン1: _setup_stages() をオーバーライド（推奨）
--------------------------------------------------
ステージを動的に設定する場合に最適。親クラスの _initialize() が自動的に
_initialize_agents() と _finalize_initialization() を呼び出すため、
Flow 構築が確実に行われます。

    >>> class MyEngine(PipelineEngine):
    ...     def __init__(self, registry):
    ...         self._registry = registry
    ...         super().__init__(stages=[])  # 空で初期化
    ...
    ...     async def _setup_stages(self) -> None:
    ...         await self._registry.initialize()
    ...         self._stage_configs = self._parse_stages([
    ...             {"name": "gate", "agent": self._registry.get("gate"), "gate": True},
    ...             {"name": "process", "agent": self._registry.get("process")},
    ...         ])
    ...         # stage_instances も設定
    ...         for stage in self._stage_configs:
    ...             self._stage_instances[stage.name] = [stage.agent] if stage.agent else []

パターン2: _initialize() をオーバーライド（上級者向け）
------------------------------------------------------
完全なカスタマイズが必要な場合。必ず最後に _finalize_initialization() を呼び出すこと！

    >>> class MyEngine(PipelineEngine):
    ...     async def _initialize(self) -> None:
    ...         # カスタム初期化ロジック
    ...         self._stage_configs = self._parse_stages([...])
    ...         for stage in self._stage_configs:
    ...             self._stage_instances[stage.name] = [...]
    ...
    ...         # 重要: Flow 構築を忘れずに！
    ...         await self._finalize_initialization()

WARNING:
    _finalize_initialization() を呼び忘れると、self._flow が None のままになり、
    run_stream() が期待通りに動作しません（fallback ロジックが使用されます）。
"""

from __future__ import annotations

import logging
from collections.abc import AsyncIterator, Callable
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from agentflow.engines.base import BaseEngine, EngineConfig
from agentflow.engines.report_builder import ReportBuilder

if TYPE_CHECKING:
    from agentflow.flow import Flow
    from agentflow.flow.context import FlowContext


@dataclass
class StageConfig:
    """Pipelineステージ設定.

    Attributes:
        name: ステージ名（ノードID）
        agent: 単一Agent（agentsとどちらか一方）
        agents: 複数Agent（並列または順次実行）
        gate: Gateステージかどうか
        review: Reviewステージかどうか
        gate_check: Gate通過条件（lambda result: bool）
        retry_from: Review REVISE時のロールバック先ステージ名
        parallel: agentsを並列実行するか（デフォルト: False）
        interrupt_before: このステージ実行前に人間の承認を要求
        interrupt_after: このステージ実行後に人間の承認を要求
    """

    name: str
    agent: type | Any | None = None
    agents: list[type | Any] | None = None
    gate: bool = False
    review: bool = False
    gate_check: Callable[[dict[str, Any]], bool] | None = None
    retry_from: str | None = None
    parallel: bool = False
    interrupt_before: bool = False
    interrupt_after: bool = False


class PipelineEngine(BaseEngine):
    """複数Agent順次実行 + Reviewエンジン.

    内部でflow モジュールを活用してフロー構築・実行を行う。

    特徴：
    - マルチステージ順次実行
    - Gateインターセプトをサポート
    - Review + REVISEループをサポート
    - 並列実行ステージをサポート
    """

    def __init__(
        self,
        stages: list[dict[str, Any] | StageConfig],
        *,
        max_revisions: int = 2,
        report_generator: Callable[[dict[str, Any]], dict[str, Any]] | None = None,
        report_builder: ReportBuilder | None = None,
        config: EngineConfig | None = None,
    ) -> None:
        """PipelineEngineを初期化.

        Args:
            stages: ステージ設定リスト
            max_revisions: 最大リビジョン回数
            report_generator: レポートジェネレーター（コールバック、後方互換用）
            report_builder: ReportBuilder インスタンス（推奨）
            config: Engine設定

        Note:
            report_builder と report_generator の両方が指定された場合、
            report_builder が優先される。
        """
        super().__init__(config)
        self._stage_configs = self._parse_stages(stages)
        self._max_revisions = max_revisions
        self._report_generator = report_generator
        self._report_builder = report_builder
        self._flow: Flow | None = None
        self._stage_instances: dict[str, list[Any]] = {}
        self._results: dict[str, Any] = {}
        self._inputs: dict[str, Any] = {}
        self._logger = logging.getLogger("agentflow.engines.pipeline")

    def _parse_stages(self, stages: list[dict[str, Any] | StageConfig]) -> list[StageConfig]:
        """ステージ設定を解析."""
        parsed = []
        for stage in stages:
            if isinstance(stage, StageConfig):
                parsed.append(stage)
            else:
                parsed.append(StageConfig(**stage))
        return parsed

    def _resolve_agent(self, agent: type | Any) -> Any:
        """Agentを解決：クラスの場合はインスタンス化."""
        if isinstance(agent, type):
            return agent()
        return agent

    async def _initialize(self) -> None:
        """すべてのAgentを初期化し、Flowを構築.

        Note:
            サブクラスでこのメソッドをオーバーライドする場合は、
            必ず最後に super()._finalize_initialization() を呼び出すか、
            _setup_stages() をオーバーライドして stages を動的に設定してください。
        """
        # サブクラスでステージを動的に設定する機会を与える
        await self._setup_stages()

        # Agentインスタンスを初期化
        await self._initialize_agents()

        # Flow を構築（重要：これを忘れるとストリーム実行が動作しない）
        await self._finalize_initialization()

    async def _setup_stages(self) -> None:
        """ステージを動的に設定するためのフック.

        サブクラスでオーバーライドして、stages を動的に設定できます。
        このメソッドは Agent 初期化前に呼ばれます。

        Example:
            >>> async def _setup_stages(self) -> None:
            ...     await self._registry.initialize()
            ...     self._stage_configs = self._parse_stages([
            ...         {"name": "gate", "agent": self._registry.get_agent("gate")},
            ...         ...
            ...     ])
        """
        pass  # デフォルトは何もしない

    async def _initialize_agents(self) -> None:
        """すべてのAgentインスタンスを初期化."""
        for stage in self._stage_configs:
            instances = []

            if stage.agent:
                inst = self._resolve_agent(stage.agent)
                if hasattr(inst, "initialize"):
                    await inst.initialize()
                instances.append(inst)

            if stage.agents:
                for agent in stage.agents:
                    inst = self._resolve_agent(agent)
                    if hasattr(inst, "initialize"):
                        await inst.initialize()
                    instances.append(inst)

            self._stage_instances[stage.name] = instances

    async def _finalize_initialization(self) -> None:
        """初期化を完了（Flow 構築）.

        Warning:
            サブクラスで _initialize() をオーバーライドする場合は、
            必ずこのメソッドを最後に呼び出してください。
        """
        self._flow = self._build_flow()
        self._logger.info(f"PipelineEngine initialized with {len(self._stage_configs)} stages")

    def _build_flow(self) -> "Flow":
        """ステージ設定からFlowを構築."""
        from agentflow.flow import create_flow

        builder = create_flow(self._config.name)

        # ステージ名のリストを保持（依存関係追跡用）
        completed_stages: list[str] = []

        for stage in self._stage_configs:
            instances = self._stage_instances[stage.name]

            if stage.gate and instances:
                # Gateステージ - 前ステージの結果を渡す input_mapper を設定
                input_mapper = self._create_stage_input_mapper(completed_stages.copy())
                builder.gate(
                    instances[0],
                    id=stage.name,
                    input_mapper=input_mapper,
                    check=stage.gate_check,
                )
                completed_stages.append(stage.name)
            elif stage.review and instances:
                # Reviewステージ - 全ての結果を入力として渡す
                builder.review(
                    instances[0],
                    id=stage.name,
                    retry_from=stage.retry_from,
                    max_revisions=self._max_revisions,
                    input_mapper=self._create_review_input_mapper(),
                )
                completed_stages.append(stage.name)
            elif stage.parallel and len(instances) > 1:
                # 並列ステージ
                agent_tuples = [
                    (f"{stage.name}_{i}", inst)
                    for i, inst in enumerate(instances)
                ]
                builder.parallel(*agent_tuples, id=stage.name)
                completed_stages.append(stage.name)
            else:
                # 通常のAgentステージ - 前ステージの結果を渡す input_mapper を設定
                input_mapper = self._create_stage_input_mapper(completed_stages.copy())
                if len(instances) == 1:
                    # 単一Agent: ステージ名をnode_idとして使用
                    input_mappers = {stage.name: input_mapper}
                    builder.then(instances[0], ids=[stage.name], input_mappers=input_mappers)
                else:
                    # 複数Agent: 各Agentに固有のIDを付与して input_mapper を渡す
                    node_ids = [f"{stage.name}_{i}" for i in range(len(instances))]
                    input_mappers = {node_id: input_mapper for node_id in node_ids}
                    for i, inst in enumerate(instances):
                        builder.then(inst, ids=[node_ids[i]], input_mappers=input_mappers)
                completed_stages.append(stage.name)

        return builder.with_config(max_revisions=self._max_revisions).build()

    def _create_stage_input_mapper(
        self, completed_stages: list[str]
    ) -> Callable[["FlowContext"], dict[str, Any]]:
        """ステージ用の入力マッパーを生成.

        前ステージの結果を {stage_name}_result として渡し、
        結果を直接マージして後方互換性を維持。

        Args:
            completed_stages: 完了したステージ名のリスト

        Returns:
            入力マッピング関数
        """
        def mapper(ctx: FlowContext) -> dict[str, Any]:
            # 1. 元の入力を取得
            inputs = ctx.get_inputs()

            # 2. 前ステージの結果を追加
            for stage_name in completed_stages:
                result = ctx.get_result(stage_name)
                if result:
                    # {stage_name}_result として設定（Agent間依存用）
                    inputs[f"{stage_name}_result"] = result
                    # 結果を直接マージ（後方互換）
                    inputs.update(result)

            return inputs

        return mapper

    def _create_review_input_mapper(self) -> Callable[["FlowContext"], dict[str, Any]]:
        """Review用の入力マッパーを生成.

        すべての結果を {stage_name}_result 形式で渡す。

        Returns:
            入力マッピング関数
        """
        def mapper(ctx: FlowContext) -> dict[str, Any]:
            # 元の結果を取得
            all_results = ctx.get_all_results()
            self._logger.debug(f"[review_input_mapper] all_results keys: {list(all_results.keys())}")

            # {stage_name}_result 形式に変換
            formatted_results: dict[str, Any] = {}
            for stage_name, result in all_results.items():
                # {stage_name}_result として設定
                formatted_results[f"{stage_name}_result"] = result
                # 直接マージも行う（後方互換）
                if isinstance(result, dict):
                    formatted_results.update(result)

            # 元の入力も含める
            inputs = ctx.get_inputs()
            formatted_results.update(inputs)

            self._logger.debug(f"[review_input_mapper] formatted keys: {list(formatted_results.keys())}")
            return formatted_results

        return mapper

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Pipelineフローを実行（Flowを活用）."""
        if self._flow:
            # Flowを使用して実行
            result = await self._flow.run(inputs)

            # レポートジェネレーターがあれば適用
            if self._report_generator:
                return self._report_generator(result)

            return {
                "status": "success",
                "results": result,
            }

        # Fallback: 従来の実行ロジック
        return await self._execute_fallback(inputs)

    async def _execute_fallback(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """従来の実行ロジック（Flowが使用できない場合のフォールバック）."""
        self._results = {"inputs": inputs}
        current_inputs = inputs.copy()

        # Reviewステージとロールバック先を検索
        retry_from_idx = 0
        for stage in self._stage_configs:
            if stage.review and stage.retry_from:
                for j, s in enumerate(self._stage_configs):
                    if s.name == stage.retry_from:
                        retry_from_idx = j
                        break

        # メインループ（REVISEをサポート）
        for revision in range(self._max_revisions + 1):
            start_idx = retry_from_idx if revision > 0 else 0

            for i in range(start_idx, len(self._stage_configs)):
                stage = self._stage_configs[i]
                self._logger.info(f"Running stage: {stage.name} (revision {revision})")

                # ステージを実行
                stage_result = await self._run_stage(stage, current_inputs)
                self._results[stage.name] = stage_result

                # Gateチェック
                if stage.gate:
                    check = stage.gate_check or (lambda r: r.get("passed", True))
                    if not check(stage_result):
                        self._logger.info(f"Gate {stage.name} rejected")
                        return {
                            "status": "rejected",
                            "stage": stage.name,
                            "reason": stage_result.get("reason", "Gate rejected"),
                            "results": self._results,
                        }

                # Reviewチェック
                if stage.review:
                    verdict = stage_result.get("verdict", "PASS")
                    if verdict == "PASS":
                        break
                    elif verdict == "REJECT":
                        return {
                            "status": "rejected",
                            "stage": stage.name,
                            "reason": stage_result.get("reason", "Review rejected"),
                            "results": self._results,
                        }
                    elif verdict == "REVISE" and revision < self._max_revisions:
                        self._logger.info(f"REVISE requested, retry from {retry_from_idx}")
                        break

                current_inputs.update(stage_result)
            else:
                break

        if self._report_generator:
            return self._report_generator(self._results)

        return {
            "status": "success",
            "results": self._results,
        }

    async def _run_agent(self, agent: Any, inputs: dict[str, Any]) -> dict[str, Any]:
        """単一Agentを実行."""
        if hasattr(agent, "run"):
            result = await agent.run(inputs)
        elif hasattr(agent, "invoke"):
            result = await agent.invoke(inputs)
        elif hasattr(agent, "process"):
            result = await agent.process(inputs)
        else:
            raise AttributeError(f"Agent {agent} has no run/invoke/process method")

        if isinstance(result, dict):
            return result
        elif hasattr(result, "model_dump"):
            return result.model_dump()
        return {"result": result}

    async def _run_stage(
        self, stage: StageConfig, inputs: dict[str, Any]
    ) -> dict[str, Any]:
        """単一ステージを実行.

        HITL が有効でステージに interrupt_before/after が設定されている場合、
        人間の承認を要求します。
        """
        import asyncio

        # HITL: 実行前の承認チェック
        if stage.interrupt_before and self._should_interrupt(stage.name, "before"):
            await self._request_stage_approval(stage, inputs, "before")

        instances = self._stage_instances[stage.name]

        if len(instances) == 1:
            result = await self._run_agent(instances[0], inputs)
        elif stage.parallel:
            tasks = [self._run_agent(inst, inputs) for inst in instances]
            results = await asyncio.gather(*tasks)
            result = {f"agent_{i}": r for i, r in enumerate(results)}
        else:
            combined = {}
            current_inputs = inputs.copy()
            for inst in instances:
                agent_result = await self._run_agent(inst, current_inputs)
                agent_name = getattr(inst, "name", inst.__class__.__name__)
                combined[agent_name] = agent_result
                current_inputs.update(agent_result)
            result = combined

        # HITL: 実行後の承認チェック
        if stage.interrupt_after and self._should_interrupt(stage.name, "after"):
            await self._request_stage_approval(stage, inputs, "after", result)

        return result

    def _should_interrupt(self, stage_name: str, timing: str) -> bool:
        """ステージで割り込みが必要かどうかを判定."""
        if not self._config.hitl.enabled:
            return False

        # 設定の interrupt_before/after もチェック
        if timing == "before" and stage_name in self._config.hitl.interrupt_before:
            return True
        if timing == "after" and stage_name in self._config.hitl.interrupt_after:
            return True

        return True  # ステージ設定で既にチェック済み

    async def _request_stage_approval(
        self,
        stage: StageConfig,
        inputs: dict[str, Any],
        timing: str,
        result: dict[str, Any] | None = None,
    ) -> None:
        """ステージの承認を要求."""
        from agentflow.hitl import ApprovalRequest, interrupt

        request = ApprovalRequest(
            action=f"stage_{timing}_{stage.name}",
            reason=f"ステージ '{stage.name}' の{timing}で承認が必要です",
            context={
                "stage_name": stage.name,
                "timing": timing,
                "inputs": inputs,
                "result": result,
            },
            requester=self._config.name,
        )

        # interrupt() は InterruptSignal を発生させる
        await interrupt(
            request,
            node_id=stage.name,
            flow_id=self._flow_id,
            state={"stage": stage.name, "timing": timing, "result": result},
        )

    async def _execute_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """Pipelineをストリーム実行（Flowを活用）."""
        if self._flow:
            # Flowのrun_streamを使用
            # レポートジェネレーターがある場合、flow_complete イベントを変換
            async for event in self._flow.run_stream(inputs):
                if event.get("type") == "flow_complete" and self._report_generator:
                    # flow_complete の result を変換して置き換え
                    result = event.get("result", {})
                    transformed = self._report_generator(result)
                    yield {
                        **event,
                        "result": transformed,
                        "data": {**event.get("data", {}), "transformed": True},
                    }
                else:
                    yield event
            return

        # Fallback: 従来のストリーム実行ロジック
        async for event in self._execute_stream_fallback(inputs):
            yield event

    async def _execute_stream_fallback(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """従来のストリーム実行ロジック（フォールバック）."""
        self._inputs = inputs.copy()
        self._results = {"inputs": inputs}
        current_inputs = inputs.copy()

        retry_from_idx = 0
        for stage in self._stage_configs:
            if stage.review and stage.retry_from:
                for j, s in enumerate(self._stage_configs):
                    if s.name == stage.retry_from:
                        retry_from_idx = j
                        break

        review_passed = False
        for revision in range(self._max_revisions + 1):
            start_idx = retry_from_idx if revision > 0 else 0

            for i in range(start_idx, len(self._stage_configs)):
                stage = self._stage_configs[i]

                if event := self._emit_node_start(stage.name):
                    yield event

                progress = (i + 1) / len(self._stage_configs) * 100
                yield {
                    "type": "progress",
                    "data": {"stage": stage.name, "progress": progress, "revision": revision},
                }

                stage_result = await self._run_stage(stage, current_inputs)
                self._results[stage.name] = stage_result

                if event := self._emit_node_complete(stage.name, stage_result):
                    yield event

                # 思考過程LOGイベントを発行（道・法・術・器の詳細分析を含む）
                for log_event in self._emit_thinking_logs(stage.name, stage_result):
                    yield log_event

                if stage.gate:
                    check = stage.gate_check or (lambda r: r.get("passed", True))
                    if not check(stage_result):
                        yield {"type": "gate_rejected", "data": {"stage": stage.name}}
                        yield {
                            "type": "result",
                            "data": {
                                "status": "rejected",
                                "stage": stage.name,
                                "results": self._results,
                            },
                        }
                        return

                if stage.review:
                    verdict = stage_result.get("verdict", stage_result.get("overall_verdict", "PASS"))
                    yield {"type": "review_verdict", "data": {"verdict": verdict}}

                    if verdict == "PASS":
                        review_passed = True
                        break
                    elif verdict == "REJECT":
                        yield {
                            "type": "result",
                            "data": {"status": "rejected", "results": self._results},
                        }
                        return
                    elif verdict == "REVISE" and revision < self._max_revisions:
                        yield {"type": "revise", "data": {"retry_from": retry_from_idx}}
                        break

                # 前ステージの結果を次のステージに渡す
                # 1. {stage_name}_result として設定（Agent間依存用）
                current_inputs[f"{stage.name}_result"] = stage_result
                # 2. 結果を直接マージ（後方互換）
                current_inputs.update(stage_result)
            else:
                # 内部ループが break なしで完了した場合（全ステージ正常完了）
                break

            # Review で PASS した場合は外部ループも終了
            if review_passed:
                break

        # 最終レポート生成
        final = self._build_final_report()
        yield {"type": "result", "data": final}

    def _build_final_report(self) -> dict[str, Any]:
        """最終レポートを生成.

        ReportBuilder > report_generator > デフォルト の優先順で使用。

        Returns:
            JSON シリアライズ可能なレポート辞書
        """
        if self._report_builder:
            return self._report_builder.build(
                results=self._results,
                inputs=self._inputs,
            )
        elif self._report_generator:
            return self._report_generator(self._results)
        else:
            return {
                "status": "success",
                "results": self._results,
            }

    def _emit_thinking_logs(
        self, stage_name: str, stage_result: dict[str, Any]
    ) -> list[dict[str, Any]]:
        """Agent思考過程のLOGイベントを生成.

        道・法・術・器 各Agentの詳細な思考内容を抽出し、
        フロントエンドに表示するためのLOGイベントを生成。

        Args:
            stage_name: ステージ名（agent ID）
            stage_result: Agent結果辞書

        Returns:
            LOGイベントのリスト
        """
        import time

        if not self._config.enable_events:
            return []

        events: list[dict[str, Any]] = []

        # Agent別の思考過程抽出
        thinking_content = self._extract_thinking_content(stage_name, stage_result)
        self._logger.debug(f"[_emit_thinking_logs] stage={stage_name}, contents={len(thinking_content)}")
        if not thinking_content:
            # 結果キーをログ出力（デバッグ用）
            self._logger.debug(f"[_emit_thinking_logs] result_keys={list(stage_result.keys()) if stage_result else 'empty'}")

        for content in thinking_content:
            events.append({
                "event_type": "log",
                "timestamp": time.time(),
                "flow_id": self._flow_id or "",
                "node_id": stage_name,
                "node_name": stage_name,
                "level": "INFO",
                "message": content,
                "source": stage_name,
            })

        return events

    def _extract_thinking_content(
        self, stage_name: str, result: dict[str, Any]
    ) -> list[str]:
        """Agentの思考過程コンテンツを抽出.

        各Agent固有のフィールドから思考過程を人間可読な形式で抽出。

        Args:
            stage_name: ステージ名
            result: Agent結果辞書

        Returns:
            思考過程の文字列リスト
        """
        contents: list[str] = []

        # 認知判定（Cognitive Gate）
        if stage_name == "cognitive_gate":
            if eo := result.get("evaluation_object"):
                contents.append(f"【評価対象】{eo}")
            if cb := result.get("cognitive_biases"):
                if isinstance(cb, list) and cb:
                    contents.append(f"【認知バイアス検出】{', '.join(str(b) for b in cb)}")
            if cf := result.get("clarifying_questions"):
                if isinstance(cf, list) and cf:
                    contents.append(f"【追加確認事項】{', '.join(str(q) for q in cf[:3])}")

        # 門番（Gatekeeper）
        elif stage_name == "gatekeeper":
            if cat := result.get("category"):
                contents.append(f"【分類】{cat}")
            if conf := result.get("confidence"):
                contents.append(f"【信頼度】{conf}")

        # 診断（Clarification）
        elif stage_name == "clarification":
            if rq := result.get("restated_question"):
                contents.append(f"【再定義した質問】{rq}")
            if ambig := result.get("ambiguities"):
                if isinstance(ambig, list) and ambig:
                    contents.append(f"【曖昧性】{', '.join(str(a) for a in ambig[:3])}")
            if bias := result.get("cognitive_biases"):
                if isinstance(bias, list) and bias:
                    contents.append(f"【認知バイアス】{', '.join(str(b) for b in bias[:3])}")

        # 道（Dao）- 本質分析
        elif stage_name == "dao":
            if pt := result.get("problem_type"):
                contents.append(f"【問題タイプ】{pt}")
            if essence := result.get("essence"):
                contents.append(f"【本質】{essence}")
            # 本質導出プロセス（思考の可視化）
            if ed := result.get("essence_derivation"):
                if isinstance(ed, dict):
                    if sp := ed.get("surface_problem"):
                        contents.append(f"【表層問題】{sp}")
                    if uw := ed.get("underlying_why"):
                        contents.append(f"【深層理由】{uw}")
                    if rc := ed.get("root_constraint"):
                        contents.append(f"【根本制約】{rc}")
            if ic := result.get("immutable_constraints"):
                if isinstance(ic, list) and ic:
                    contents.append(f"【不可変制約】{', '.join(str(c) for c in ic[:3])}")
            if dt := result.get("death_traps"):
                if isinstance(dt, list) and dt:
                    trap_names = [t.get("name", str(t)) if isinstance(t, dict) else str(t) for t in dt[:2]]
                    contents.append(f"【死穴（禁忌）】{', '.join(trap_names)}")

        # 法（Fa）- 戦略選定
        elif stage_name == "fa":
            if rp := result.get("recommended_paths"):
                if isinstance(rp, list) and rp:
                    path_names = [p.get("name", str(p)) if isinstance(p, dict) else str(p) for p in rp[:2]]
                    contents.append(f"【推奨戦略】{', '.join(path_names)}")
            if sp := result.get("strategic_prohibitions"):
                if isinstance(sp, list) and sp:
                    contents.append(f"【戦略的禁止事項】{', '.join(str(p) for p in sp[:3])}")
            if da := result.get("differentiation_axis"):
                if isinstance(da, dict):
                    axis_name = da.get("axis_name", "")
                    if axis_name:
                        contents.append(f"【差別化軸】{axis_name}")
            if wef := result.get("why_existing_fails"):
                contents.append(f"【既存解が使えない理由】{wef}")

        # 術（Shu）- 実行計画
        elif stage_name == "shu":
            if fa := result.get("first_action"):
                contents.append(f"【最初の一歩】{fa}")
            if phases := result.get("phases"):
                if isinstance(phases, list) and phases:
                    phase_names = [p.get("name", str(p)) if isinstance(p, dict) else str(p) for p in phases[:3]]
                    contents.append(f"【実行フェーズ】{', '.join(phase_names)}")
            if cl := result.get("cut_list"):
                if isinstance(cl, list) and cl:
                    contents.append(f"【切り捨てリスト】{', '.join(str(c) for c in cl[:3])}")
            if svp := result.get("single_validation_point"):
                if isinstance(svp, dict):
                    point = svp.get("validation_point", "")
                    if point:
                        contents.append(f"【単一検証ポイント】{point}")
            if ec := result.get("exit_criteria"):
                if isinstance(ec, dict):
                    trigger = ec.get("exit_trigger", "")
                    if trigger:
                        contents.append(f"【撤退基準】{trigger}")

        # 器（Qi）- 技術実装
        elif stage_name == "qi":
            if impls := result.get("implementations"):
                if isinstance(impls, list) and impls:
                    # Implementation モデル: component フィールドを使用
                    impl_names = [i.get("component", i.get("name", str(i))) if isinstance(i, dict) else str(i) for i in impls[:3]]
                    contents.append(f"【実装要素】{', '.join(impl_names)}")
            if tools := result.get("tool_recommendations"):
                if isinstance(tools, list) and tools:
                    contents.append(f"【推奨ツール】{', '.join(str(t) for t in tools[:3])}")
            if dtech := result.get("domain_technologies"):
                if isinstance(dtech, list) and dtech:
                    # DomainSpecificTechnology モデル: technology_name フィールドを使用
                    tech_names = [d.get("technology_name", d.get("name", str(d))) if isinstance(d, dict) else str(d) for d in dtech[:2]]
                    contents.append(f"【ドメイン固有技術】{', '.join(tech_names)}")

        # 検証（Review）
        elif stage_name == "review":
            if verdict := result.get("overall_verdict"):
                contents.append(f"【総合判定】{verdict}")
            if score := result.get("confidence_score"):
                contents.append(f"【信頼度スコア】{score}")
            if findings := result.get("findings"):
                if isinstance(findings, list) and findings:
                    finding_types = [f.get("finding_type", str(f)) if isinstance(f, dict) else str(f) for f in findings[:3]]
                    contents.append(f"【検証所見】{', '.join(finding_types)}")

        return contents
