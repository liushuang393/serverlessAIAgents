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
"""

from __future__ import annotations

import logging
from collections.abc import AsyncIterator, Callable
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from agentflow.engines.base import BaseEngine, EngineConfig

if TYPE_CHECKING:
    from agentflow.flow import Flow


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
    """
    name: str
    agent: type | Any | None = None
    agents: list[type | Any] | None = None
    gate: bool = False
    review: bool = False
    gate_check: Callable[[dict[str, Any]], bool] | None = None
    retry_from: str | None = None
    parallel: bool = False


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
        config: EngineConfig | None = None,
    ) -> None:
        """PipelineEngineを初期化.

        Args:
            stages: ステージ設定リスト
            max_revisions: 最大リビジョン回数
            report_generator: レポートジェネレーター（オプション）
            config: Engine設定
        """
        super().__init__(config)
        self._stage_configs = self._parse_stages(stages)
        self._max_revisions = max_revisions
        self._report_generator = report_generator
        self._flow: Flow | None = None
        self._stage_instances: dict[str, list[Any]] = {}
        self._results: dict[str, Any] = {}
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
        """すべてのAgentを初期化し、Flowを構築."""
        # flow モジュールをインポート
        from agentflow.flow import create_flow

        # Agentインスタンスを初期化
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

        # Flow を構築
        self._flow = self._build_flow()
        self._logger.info(f"PipelineEngine initialized with {len(self._stage_configs)} stages")

    def _build_flow(self) -> "Flow":
        """ステージ設定からFlowを構築."""
        from agentflow.flow import create_flow

        builder = create_flow(self._config.name)

        for stage in self._stage_configs:
            instances = self._stage_instances[stage.name]

            if stage.gate and instances:
                # Gateステージ
                builder.gate(
                    instances[0],
                    id=stage.name,
                    check=stage.gate_check,
                )
            elif stage.review and instances:
                # Reviewステージ
                builder.review(
                    instances[0],
                    id=stage.name,
                    retry_from=stage.retry_from,
                    max_revisions=self._max_revisions,
                )
            elif stage.parallel and len(instances) > 1:
                # 並列ステージ
                agent_tuples = [
                    (f"{stage.name}_{i}", inst)
                    for i, inst in enumerate(instances)
                ]
                builder.parallel(*agent_tuples, id=stage.name)
            else:
                # 通常のAgentステージ
                for inst in instances:
                    node_id = stage.name if len(instances) == 1 else None
                    builder.then(inst, ids=[node_id] if node_id else None)

        return builder.with_config(max_revisions=self._max_revisions).build()

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
        """単一ステージを実行."""
        import asyncio

        instances = self._stage_instances[stage.name]

        if len(instances) == 1:
            return await self._run_agent(instances[0], inputs)

        if stage.parallel:
            tasks = [self._run_agent(inst, inputs) for inst in instances]
            results = await asyncio.gather(*tasks)
            return {f"agent_{i}": r for i, r in enumerate(results)}
        else:
            combined = {}
            current_inputs = inputs.copy()
            for inst in instances:
                result = await self._run_agent(inst, current_inputs)
                agent_name = getattr(inst, "name", inst.__class__.__name__)
                combined[agent_name] = result
                current_inputs.update(result)
            return combined

    async def _execute_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """Pipelineをストリーム実行（Flowを活用）."""
        if self._flow:
            # Flowのrun_streamを使用
            async for event in self._flow.run_stream(inputs):
                yield event

            # レポートジェネレーターがあれば最終結果に適用
            if self._report_generator and event.get("type") == "flow_complete":
                result = event.get("result", {})
                yield {"type": "result", "data": self._report_generator(result)}
            return

        # Fallback: 従来のストリーム実行ロジック
        async for event in self._execute_stream_fallback(inputs):
            yield event

    async def _execute_stream_fallback(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """従来のストリーム実行ロジック（フォールバック）."""
        self._results = {"inputs": inputs}
        current_inputs = inputs.copy()

        retry_from_idx = 0
        for stage in self._stage_configs:
            if stage.review and stage.retry_from:
                for j, s in enumerate(self._stage_configs):
                    if s.name == stage.retry_from:
                        retry_from_idx = j
                        break

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
                    verdict = stage_result.get("verdict", "PASS")
                    yield {"type": "review_verdict", "data": {"verdict": verdict}}

                    if verdict == "PASS":
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

                current_inputs.update(stage_result)
            else:
                break

        final = self._report_generator(self._results) if self._report_generator else {
            "status": "success",
            "results": self._results,
        }
        yield {"type": "result", "data": final}

