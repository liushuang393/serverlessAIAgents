# -*- coding: utf-8 -*-
"""AgentPipeline - 自動進捗追跡付きのAgentパイプライン.

このモジュールは、複数のAgentを順次実行し、進捗イベントを自動発射する
パイプラインを提供します。

設計原則:
- 自動進捗追跡: Agentの開始/完了を自動的にイベント化
- 結果チェーン: 前のAgentの出力を次のAgentの入力に渡す
- 条件分岐: 特定条件でパイプラインを早期終了可能
- ロールバック機構: REVISE判定時に指定Agentまで戻って再実行

使用例:
    >>> from agentflow.patterns.agent_pipeline import AgentPipeline, PipelineConfig
    >>> from agentflow.patterns.progress_emitter import AgentMeta
    >>>
    >>> # 基本的な使用法
    >>> pipeline = AgentPipeline(
    ...     agents=[gatekeeper, dao, fa, shu, qi, review],
    ...     flow_id="decision-flow",
    ... )
    >>> async for result, event in pipeline.run_with_events(input_data):
    ...     if event:
    ...         yield event  # SSE 配信
    ...     if result:
    ...         final_result = result
    >>>
    >>> # 同期実行（イベントなし）
    >>> result = await pipeline.run_sync(input_data)
"""

import asyncio
import logging
import uuid
from collections.abc import AsyncIterator, Callable
from dataclasses import dataclass, field
from typing import Any, Protocol

from agentflow.patterns.progress_emitter import AgentMeta, ProgressEmitter
from agentflow.protocols.agui_events import (
    AGUIEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
)


class AgentProtocol(Protocol):
    """Agent プロトコル（ダックタイピング用）."""

    name: str

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent を実行."""
        ...


@dataclass
class AgentConfig:
    """Agent 設定（パイプライン用）.

    Attributes:
        agent: Agentインスタンス
        id: Agent ID（イベント用）
        name: Agent名（表示用）
        label: Agentラベル（表示用）
        icon: アイコン（オプション）
        is_gate: ゲートAgentか（拒否時に早期終了）
        skip_condition: スキップ条件（関数）
        progress_messages: 進捗メッセージリスト
    """

    agent: AgentProtocol
    id: str = ""
    name: str = ""
    label: str = ""
    icon: str = ""
    is_gate: bool = False
    skip_condition: Callable[[dict[str, Any]], bool] | None = None
    progress_messages: list[tuple[int, str]] = field(default_factory=list)

    def __post_init__(self) -> None:
        """初期化後処理."""
        if not self.id:
            # Agentクラス名から推定: DaoAgent → dao
            self.id = self.agent.name.replace("Agent", "").lower()
        if not self.name:
            self.name = self.agent.name


@dataclass
class PipelineConfig:
    """パイプライン設定.

    Attributes:
        flow_id: フローID（Noneの場合は自動生成）
        max_revisions: 最大リビジョン回数
        emit_progress_per_node: ノード単位の進捗イベントを発射するか
        result_key: 最終結果を格納するキー
    """

    flow_id: str | None = None
    max_revisions: int = 2
    emit_progress_per_node: bool = True
    result_key: str = "pipeline_result"


class RevisionRequest:
    """ロールバックリクエスト.

    Review AgentなどがREVISE判定を返した場合に使用。

    Attributes:
        target_agent_id: ロールバック先のAgent ID
        feedback: フィードバック情報
    """

    def __init__(self, target_agent_id: str, feedback: dict[str, Any] | None = None):
        """初期化.

        Args:
            target_agent_id: ロールバック先のAgent ID
            feedback: フィードバック情報
        """
        self.target_agent_id = target_agent_id
        self.feedback = feedback or {}


class AgentPipeline:
    """自動進捗追跡付きAgentパイプライン.

    複数のAgentを順次実行し、AG-UI準拠の進捗イベントを自動発射。
    ゲートAgentによる早期終了、条件分岐、ロールバック機構をサポート。

    Attributes:
        agents: AgentConfigリスト
        config: PipelineConfig
        emitter: ProgressEmitter

    使用例:
        >>> pipeline = AgentPipeline(
        ...     agents=[gatekeeper, dao, fa, shu, qi, review],
        ...     flow_id="decision-flow",
        ... )
        >>> # SSEストリーム付き実行
        >>> async for result, event in pipeline.run_with_events(input_data):
        ...     if event:
        ...         yield event
        ...     if result:
        ...         final_result = result
        >>>
        >>> # 同期実行
        >>> result = await pipeline.run_sync(input_data)
    """

    def __init__(
        self,
        agents: list[AgentProtocol | AgentConfig],
        flow_id: str | None = None,
        config: PipelineConfig | None = None,
        agent_metas: list[AgentMeta] | None = None,
    ) -> None:
        """初期化.

        Args:
            agents: Agentリスト（AgentProtocolまたはAgentConfig）
            flow_id: フローID（オプション）
            config: パイプライン設定（オプション）
            agent_metas: Agentメタデータリスト（オプション、後方互換）
        """
        self._logger = logging.getLogger("agentflow.pipeline")
        self.config = config or PipelineConfig(flow_id=flow_id)
        self.flow_id = self.config.flow_id or f"pipeline-{uuid.uuid4().hex[:8]}"

        # AgentConfigに正規化
        self.agents: list[AgentConfig] = []
        for i, agent in enumerate(agents):
            if isinstance(agent, AgentConfig):
                self.agents.append(agent)
            else:
                # AgentMetaがあれば使用
                meta = agent_metas[i] if agent_metas and i < len(agent_metas) else None
                config_item = AgentConfig(
                    agent=agent,
                    id=meta.id if meta else "",
                    name=meta.name if meta else "",
                    label=meta.label if meta else "",
                    icon=meta.icon if meta else "",
                )
                self.agents.append(config_item)

        # ProgressEmitterを初期化
        self.emitter = ProgressEmitter(
            flow_id=self.flow_id,
            total_agents=len(self.agents),
        )
        # Agentメタデータを登録
        self.emitter.register_agents([
            AgentMeta(id=a.id, name=a.name, label=a.label, icon=a.icon)
            for a in self.agents
        ])

        # 結果キャッシュ（ロールバック時に使用）
        self._results: dict[str, dict[str, Any]] = {}

    def get_agent_index(self, agent_id: str) -> int:
        """Agent ID からインデックスを取得.

        Args:
            agent_id: Agent ID

        Returns:
            インデックス（見つからない場合は -1）
        """
        for i, config in enumerate(self.agents):
            if config.id == agent_id:
                return i
        return -1

    async def run_sync(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """同期的にパイプラインを実行（イベントなし）.

        Args:
            input_data: 入力データ

        Returns:
            最終結果
        """
        result = input_data
        for config in self.agents:
            # スキップ条件チェック
            if config.skip_condition and config.skip_condition(result):
                self._logger.info(f"Skipping {config.id} due to skip_condition")
                continue

            # Agent 実行
            result = await config.agent.run(result)
            self._results[config.id] = result

            # ゲート Agent の場合、拒否されたら早期終了
            if config.is_gate and self._is_rejected(result):
                self._logger.warning(f"Pipeline stopped by gate: {config.id}")
                return result

        return result

    async def run_with_events(
        self, input_data: dict[str, Any]
    ) -> AsyncIterator[tuple[dict[str, Any] | None, AGUIEvent | None]]:
        """イベント発射付きでパイプラインを実行.

        Args:
            input_data: 入力データ

        Yields:
            (結果 | None, イベント | None) のタプル
            - 結果が None でない場合: パイプライン完了
            - イベントが None でない場合: AG-UI イベント
        """
        import time

        # Flow 開始イベント
        yield (None, FlowStartEvent(
            timestamp=time.time(),
            flow_id=self.flow_id,
            data={"total_agents": len(self.agents)},
        ))

        result = input_data
        self.emitter.reset()

        try:
            for i, config in enumerate(self.agents):
                # スキップ条件チェック
                if config.skip_condition and config.skip_condition(result):
                    self._logger.info(f"Skipping {config.id} due to skip_condition")
                    continue

                # ノード開始イベント
                async for event in self.emitter.emit_node_start(
                    config.id, config.name, config.label
                ):
                    yield (None, event)

                # 進捗メッセージがあれば発射（遅延付き）
                if config.progress_messages and self.config.emit_progress_per_node:
                    for percentage, message in config.progress_messages:
                        async for event in self.emitter.emit_node_progress(
                            config.id, percentage, message
                        ):
                            yield (None, event)
                        await asyncio.sleep(0.1)  # 短いディレイでUX改善

                # Agent 実行
                result = await config.agent.run(result)
                self._results[config.id] = result

                # ノード完了イベント
                result_summary = self._extract_result_summary(config.id, result)
                async for event in self.emitter.emit_node_complete(
                    config.id, config.name, result_summary
                ):
                    yield (None, event)

                # ゲート Agent の場合、拒否されたら早期終了
                if config.is_gate and self._is_rejected(result):
                    self._logger.warning(f"Pipeline stopped by gate: {config.id}")
                    yield (None, FlowErrorEvent(
                        timestamp=time.time(),
                        flow_id=self.flow_id,
                        data={"rejected_by": config.id},
                        error_message=result.get("message", "拒否されました"),
                        error_type="RejectedByGate",
                    ))
                    yield (result, None)
                    return

            # Flow 完了イベント
            yield (None, FlowCompleteEvent(
                timestamp=time.time(),
                flow_id=self.flow_id,
                data={},
                result=result,
                include_result=True,
            ))
            yield (result, None)

        except Exception as e:
            self._logger.error(f"Pipeline error: {e}")
            yield (None, FlowErrorEvent(
                timestamp=time.time(),
                flow_id=self.flow_id,
                data={},
                error_message=str(e),
                error_type=type(e).__name__,
            ))
            raise

    async def run_with_revision(
        self,
        input_data: dict[str, Any],
        revision_checker: Callable[[dict[str, Any]], RevisionRequest | None],
    ) -> AsyncIterator[tuple[dict[str, Any] | None, AGUIEvent | None]]:
        """ロールバック機構付きでパイプラインを実行.

        Args:
            input_data: 入力データ
            revision_checker: ロールバックチェック関数（結果からRevisionRequestを返す）

        Yields:
            (結果 | None, イベント | None) のタプル
        """
        import time

        for revision_count in range(self.config.max_revisions + 1):
            self._logger.info(
                f"Pipeline execution round {revision_count + 1}/{self.config.max_revisions + 1}"
            )

            # パイプライン実行
            final_result: dict[str, Any] | None = None
            async for result, event in self.run_with_events(input_data):
                if event:
                    yield (None, event)
                if result is not None:
                    final_result = result

            if final_result is None:
                return

            # ロールバックチェック
            revision_request = revision_checker(final_result)
            if revision_request is None:
                # ロールバックなし - 完了
                yield (final_result, None)
                return

            # ロールバック処理
            if revision_count >= self.config.max_revisions:
                self._logger.warning("Max revisions reached")
                yield (None, FlowErrorEvent(
                    timestamp=time.time(),
                    flow_id=self.flow_id,
                    data={"revision_count": revision_count},
                    error_message=f"最大リビジョン回数（{self.config.max_revisions}回）に到達",
                    error_type="MaxRevisionsReached",
                ))
                yield (final_result, None)
                return

            # ロールバック先から再開するために入力を調整
            target_index = self.get_agent_index(revision_request.target_agent_id)
            if target_index >= 0:
                # ロールバック先の前のAgentの結果を入力として使用
                if target_index > 0:
                    prev_agent_id = self.agents[target_index - 1].id
                    if prev_agent_id in self._results:
                        input_data = self._results[prev_agent_id]
                # フィードバックを追加
                input_data["revision_feedback"] = revision_request.feedback
                input_data["revision_round"] = revision_count + 1

            self._logger.info(
                f"Revising from {revision_request.target_agent_id}"
            )

    def _is_rejected(self, result: dict[str, Any]) -> bool:
        """結果が拒否かどうかを判定.

        Args:
            result: Agent の結果

        Returns:
            拒否の場合 True
        """
        # is_acceptable が False の場合
        if result.get("is_acceptable") is False:
            return True
        # proceed が False の場合
        if result.get("proceed") is False:
            return True
        # status が rejected の場合
        if result.get("status") == "rejected":
            return True
        return False

    def _extract_result_summary(
        self, agent_id: str, result: dict[str, Any]
    ) -> str:
        """結果からサマリーを抽出.

        Args:
            agent_id: Agent ID
            result: Agent の結果

        Returns:
            サマリー文字列
        """
        # 各種フィールドからサマリーを抽出
        if "problem_type" in result:
            return f"問題タイプ: {result['problem_type']}"
        if "recommended_paths" in result:
            paths = result.get("recommended_paths", [])
            return f"推奨パス: {len(paths)}件"
        if "phases" in result:
            phases = result.get("phases", [])
            return f"フェーズ: {len(phases)}件"
        if "implementations" in result:
            impls = result.get("implementations", [])
            return f"実装要素: {len(impls)}件"
        if "overall_verdict" in result:
            return f"判定: {result['overall_verdict']}"
        if "is_acceptable" in result:
            return "受理" if result["is_acceptable"] else "拒否"
        if "proceed" in result:
            return "通過" if result["proceed"] else "補足情報要求中"
        if "diagnosis_confidence" in result:
            conf = result.get("diagnosis_confidence", 0)
            return f"診断完了 (信頼度: {conf:.0%})"
        return "完了"

    def get_results(self) -> dict[str, dict[str, Any]]:
        """全 Agent の結果を取得.

        Returns:
            Agent ID → 結果 の辞書
        """
        return self._results.copy()


__all__ = [
    "AgentConfig",
    "AgentPipeline",
    "AgentProtocol",
    "PipelineConfig",
    "RevisionRequest",
]

