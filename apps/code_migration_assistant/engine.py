"""Code Migration Engine - 工程固定型パイプライン.

固定工程:
分析 → 設計 → 変換 → テスト生成 → 差分検証 → 品質裁定 → 限定修正

工程間の受け渡しは artifacts/ 配下の JSON を介して行う。
"""

from __future__ import annotations

import asyncio
import json
import os
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

from apps.code_migration_assistant.adapters import get_adapter_factory
from apps.code_migration_assistant.agents import (
    BusinessSemanticsAgent,
    CodeTransformationAgent,
    ComplianceReporterAgent,
    DifferentialVerificationAgent,
    LegacyAnalysisAgent,
    LimitedFixerAgent,
    MigrationDesignAgent,
    QualityGateAgent,
    TestSynthesisAgent,
)
from apps.code_migration_assistant.lightning import create_lightning_engine_config
from apps.code_migration_assistant.workflow.artifacts import ArtifactStore
from apps.code_migration_assistant.workflow.models import (
    BusinessSemanticsArtifact,
    DifferentialVerificationArtifact,
    LegacyAnalysisArtifact,
    LimitedFixArtifact,
    MigrationDesignArtifact,
    QualityGateArtifact,
    TaskSpec,
    TestSynthesisArtifact,
    TransformationArtifact,
    TransformationIterationArtifact,
    TransformationIterationRecord,
    build_meta,
)
from harness.approval.approval_flow import ApprovalFlow
from harness.governance.engine import GovernanceEngine, ToolExecutionContext
from harness.governance.enterprise_audit import (
    AuditEventType,
    AuditSeverity,
    EnterpriseAuditEvent,
    EnterpriseAuditLogger,
)
from harness.guardrails.safety_mixin import SafetyMixin
from harness.policies.policy_engine import AuthContext
from infrastructure.sandbox.tool_provider import (
    OperationType,
    RegisteredTool,
    RiskLevel,
)
from kernel.agents.agent_factory import AgentFactorySpec
from kernel.agents.agent_factory import create as create_agent
from kernel.engines.base import BaseEngine, EngineConfig
from kernel.runtime import LightningTrainingRequest, TrajectoryAdapter
from shared.integrations.context_bridge import get_current_context


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


__all__ = [
    "ArtifactStore",
    "LegacyAnalysisArtifact",
]


# =============================================================================
# Virtual Tools (Governance Policy Hooks)
# =============================================================================
def _virtual_tool_func(*args: Any, **kwargs: Any) -> dict[str, Any]:
    """Virtual tool dummy implementation."""
    del args, kwargs
    return {}


_MIGRATION_PLUGIN_REQUIRED_PERMISSIONS = ["repo.read", "repo.write", "os.exec"]


@dataclass
class _LocalFlowContext:
    """ローカル実行向けの最小 FlowContext 互換オブジェクト."""

    user_id: str
    tenant_id: str
    user_context: dict[str, Any]


MIGRATION_ANALYSIS = RegisteredTool(
    name="migration.analyze_code",
    description="解析工程: レガシーコードの構造解析を実行",
    func=_virtual_tool_func,
    operation_type=OperationType.READ,
    risk_level=RiskLevel.LOW,
    audit_required=True,
)

MIGRATION_BUSINESS_SEMANTICS = RegisteredTool(
    name="migration.extract_business_semantics",
    description="業務語義工程: ビジネスイベント/ルール/状態遷移を抽出",
    func=_virtual_tool_func,
    operation_type=OperationType.READ,
    risk_level=RiskLevel.MEDIUM,
    audit_required=True,
)

MIGRATION_DESIGN = RegisteredTool(
    name="migration.design_architecture",
    description="設計工程: 移行計画とアーキテクチャ設計を実行",
    func=_virtual_tool_func,
    operation_type=OperationType.READ,
    risk_level=RiskLevel.MEDIUM,
    audit_required=True,
    requires_approval=True,  # 設計後は承認推奨
)

MIGRATION_TRANSFORM = RegisteredTool(
    name="migration.transform_code",
    description="変換工程: コード変換を実行",
    func=_virtual_tool_func,
    operation_type=OperationType.WRITE,
    risk_level=RiskLevel.HIGH,
    required_permissions=list(_MIGRATION_PLUGIN_REQUIRED_PERMISSIONS),
    audit_required=True,
    plugin_id="official.cobol-migration-pack",
    plugin_version="1.0.0",
)

MIGRATION_TEST_GEN = RegisteredTool(
    name="migration.synthesize_tests",
    description="テスト生成: テストコードを自動生成",
    func=_virtual_tool_func,
    operation_type=OperationType.WRITE,
    risk_level=RiskLevel.MEDIUM,
    required_permissions=list(_MIGRATION_PLUGIN_REQUIRED_PERMISSIONS),
    audit_required=True,
    plugin_id="official.test-synthesis-pack",
    plugin_version="1.0.0",
)

MIGRATION_VERIFY_DIFF = RegisteredTool(
    name="migration.verify_diff",
    description="差分検証: 移行前後の挙動差異を検証",
    func=_virtual_tool_func,
    operation_type=OperationType.READ,
    risk_level=RiskLevel.MEDIUM,
    audit_required=True,
)

MIGRATION_QUALITY_GATE = RegisteredTool(
    name="migration.evaluate_quality",
    description="品質裁定: コード品質とコンプライアンスを評価",
    func=_virtual_tool_func,
    operation_type=OperationType.READ,
    risk_level=RiskLevel.HIGH,
    audit_required=True,
)

MIGRATION_FIX = RegisteredTool(
    name="migration.apply_fix",
    description="修正適用: 自動修正を適用",
    func=_virtual_tool_func,
    operation_type=OperationType.WRITE,
    risk_level=RiskLevel.HIGH,
    required_permissions=list(_MIGRATION_PLUGIN_REQUIRED_PERMISSIONS),
    audit_required=True,
    requires_approval=False,  # 品質ゲート内での自動修正は通常承認不要
    plugin_id="internal.code-migration-runtime-pack",
    plugin_version="1.0.0",
)
MIGRATION_REPORT = RegisteredTool(
    name="migration.generate_report",
    description="報告工程: 合規報告書と移行サマリーを生成",
    func=_virtual_tool_func,
    operation_type=OperationType.READ,
    risk_level=RiskLevel.LOW,
    audit_required=True,
)


class CodeMigrationEngine(BaseEngine, SafetyMixin):
    """コード移行 Engine（工程固定版）."""

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        config: EngineConfig | None = None,
        enable_safety: bool = True,
    ) -> None:
        """初期化."""
        engine_config = config or create_lightning_engine_config(name="code_migration_engine")
        super().__init__(config=engine_config)
        self._migration_type = migration_type
        self._factory = get_adapter_factory()

        self._legacy_analysis_agent: LegacyAnalysisAgent | None = None
        self._business_semantics_agent: BusinessSemanticsAgent | None = None
        self._migration_design_agent: MigrationDesignAgent | None = None
        self._code_transformation_agent: CodeTransformationAgent | None = None
        self._test_synthesis_agent: TestSynthesisAgent | None = None
        self._differential_agent: DifferentialVerificationAgent | None = None
        self._quality_gate_agent: QualityGateAgent | None = None
        self._limited_fixer_agent: LimitedFixerAgent | None = None
        self._compliance_reporter_agent: ComplianceReporterAgent | None = None

        # Event Queue for streaming
        self._event_queue: asyncio.Queue[dict[str, Any]] | None = None
        self._human_facts: list[dict[str, Any]] = []
        self._capability_trace: list[dict[str, Any]] = []

        # HITL + Governance 統合
        self._approval_flow = ApprovalFlow(flow_id="migration")

        # エンタープライズ監査ロガーを使用
        self._audit_logger = EnterpriseAuditLogger()
        self._governance = GovernanceEngine()
        # GovernanceEngineに監査ロガーを注入（※内部API利用だが許容範囲）
        self._governance._audit_logger = self._audit_logger

        self._killed = False

        self.init_safety(enabled=enable_safety)

    def kill(self) -> None:
        """Kill Switch - パイプラインを即時停止.

        実行中のパイプラインは次の工程チェックポイントで停止します。
        """
        self._killed = True
        self._logger.warning("Kill Switch activated - pipeline will stop at next checkpoint")

    async def _initialize(self) -> None:
        """内部 Agent を初期化."""
        self._legacy_analysis_agent = create_agent(
            AgentFactorySpec(
                agent_class=LegacyAnalysisAgent,
                init_kwargs={"migration_type": self._migration_type},
                agent_type="reactor",
            )
        )
        self._business_semantics_agent = create_agent(
            AgentFactorySpec(agent_class=BusinessSemanticsAgent, agent_type="reactor")
        )
        self._migration_design_agent = create_agent(
            AgentFactorySpec(
                agent_class=MigrationDesignAgent,
                init_kwargs={"migration_type": self._migration_type},
                agent_type="planner",
            )
        )
        self._code_transformation_agent = create_agent(
            AgentFactorySpec(
                agent_class=CodeTransformationAgent,
                init_kwargs={"migration_type": self._migration_type},
                agent_type="executor",
            )
        )
        self._test_synthesis_agent = create_agent(
            AgentFactorySpec(agent_class=TestSynthesisAgent, agent_type="executor")
        )
        self._differential_agent = create_agent(
            AgentFactorySpec(
                agent_class=DifferentialVerificationAgent,
                init_kwargs={"migration_type": self._migration_type},
                agent_type="reviewer",
            )
        )
        self._quality_gate_agent = create_agent(AgentFactorySpec(agent_class=QualityGateAgent, agent_type="gatekeeper"))
        self._limited_fixer_agent = create_agent(AgentFactorySpec(agent_class=LimitedFixerAgent, agent_type="executor"))
        self._compliance_reporter_agent = create_agent(
            AgentFactorySpec(agent_class=ComplianceReporterAgent, agent_type="reporter")
        )

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """固定工程を実行."""
        # Run ごとに human facts / capability trace をリセットする
        self._human_facts = []
        self._capability_trace = []
        return await self._run_pipeline(inputs)

    async def _execute_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行（ノード単位イベント）."""
        node_name = "migration_pipeline"
        node_start = self._emit_node_start(node_name)
        if node_start:
            yield node_start

        # Init Event Queue
        self._event_queue = asyncio.Queue[dict[str, Any]]()

        # Run pipeline in background
        task = asyncio.create_task(self._run_pipeline(inputs))

        try:
            while not task.done():
                # Wait for next event or task completion
                # Use wait to handle both
                queue_get = asyncio.create_task(self._event_queue.get())
                done, _pending = await asyncio.wait({queue_get, task}, return_when=asyncio.FIRST_COMPLETED)

                if queue_get in done:
                    event = queue_get.result()
                    yield event
                else:
                    queue_get.cancel()

                if task in done:
                    break

            # Flush remaining events
            while not self._event_queue.empty():
                yield self._event_queue.get_nowait()

            # Check for exception in task
            result = await task

            node_complete = self._emit_node_complete(node_name, result)
            if node_complete:
                yield node_complete

        finally:
            self._event_queue = None

    async def _check_governance(
        self,
        tool: RegisteredTool,
        task_id: str,
        arguments: dict[str, Any],
        flow_context: Any,
    ) -> None:
        """Governance ポリシーチェックを実行."""
        if flow_context is None:
            msg = (
                "Governance Policy Violation: 認証コンテキストが存在しません。"
                "本番実行では API 経由で FlowContext を設定してください。"
            )
            raise PermissionError(msg)

        auth_context = self._build_auth_context(flow_context, tool)

        # 実行コンテキスト構築
        context = ToolExecutionContext(
            trace_id=task_id,
            run_id=self._run_id,
            flow_id=self._flow_id,
            app_name="code_migration_assistant",
            product_line="migration",
            auth_context=auth_context,
        )

        # フローコンテキストからユーザー情報を取得
        if flow_context:
            context.metadata["user_id"] = flow_context.user_id
            context.metadata["tenant_id"] = flow_context.tenant_id
        elif auth_context is not None:
            context.metadata["user_id"] = str(auth_context.subject.get("user_id", "local-runner"))
            context.metadata["auth_mode"] = "local_default"

        if context.auth_context is None:
            msg = "Governance Policy Violation: auth context is missing"
            raise PermissionError(msg)

        # ポリシー評価（ログ記録も含む）
        result = await self._governance.evaluate_tool(
            tool,
            tool_call_id=None,
            arguments=arguments,
            context=context,
        )

        # 拒否された場合は例外
        if result.decision.value == "deny":
            msg = f"Governance Policy Violation: {result.reason}"
            raise PermissionError(msg)

        # Emit Node Start Event
        await self._emit_step_event("node_start", tool.name)

    def _resolve_flow_context(self) -> Any | None:
        """現在実行に用いるフローコンテキストを解決する."""
        current = get_current_context()
        if current is not None:
            return current

        if not self._allow_local_default_auth():
            return None

        return _LocalFlowContext(
            user_id="local-runner",
            tenant_id="local",
            user_context={
                "role": "manager",
                "permissions": ["read", "write", "execute", "manage"],
            },
        )

    @staticmethod
    def _allow_local_default_auth() -> bool:
        """ローカル/テスト実行時にデフォルト認証コンテキストを許可するか判定."""
        force_auth = os.getenv("CODE_MIGRATION_FORCE_AUTH", "").lower()
        if force_auth in {"1", "true", "yes"}:
            return False

        env_name = (os.getenv("CODE_MIGRATION_ENV") or os.getenv("APP_ENV") or os.getenv("ENV") or "").lower()
        if env_name in {"prod", "production"}:
            return False

        allow_local = os.getenv("CODE_MIGRATION_ALLOW_LOCAL_AUTH", "true").lower()
        return allow_local not in {"0", "false", "no"}

    def _build_auth_context(
        self,
        flow_context: Any,
        tool: RegisteredTool,
    ) -> AuthContext | None:
        """Governance 用認証コンテキストを構築する."""

        def _normalize_permissions(raw: Any) -> list[str]:
            """既存権限セットを plugin manifest 互換権限へ拡張する."""
            permissions: list[str] = []
            if isinstance(raw, list):
                for item in raw:
                    if isinstance(item, str):
                        token = item.strip()
                        if token:
                            permissions.append(token)

            expanded = set(permissions)
            if "read" in expanded:
                expanded.add("repo.read")
            if "write" in expanded:
                expanded.add("repo.write")
            if "execute" in expanded:
                expanded.add("os.exec")
            return sorted(expanded)

        if flow_context is not None:
            user_context = getattr(flow_context, "user_context", {})
            if not isinstance(user_context, dict):
                user_context = {}

            permissions = user_context.get("permissions")
            if not isinstance(permissions, list) or not permissions:
                permissions = ["read", "write", "execute", "manage"]
            permissions = _normalize_permissions(permissions)

            role = user_context.get("role")
            if not isinstance(role, str) or not role:
                role = "manager"

            subject = {
                "user_id": getattr(flow_context, "user_id", "unknown-user"),
                "role": role,
                "permissions": permissions,
            }
            for key, value in user_context.items():
                if key not in {"permissions", "role"}:
                    subject[key] = value

            return AuthContext(
                subject=subject,
                resource={"type": tool.name},
                action=tool.operation_type.value,
                tenant_id=getattr(flow_context, "tenant_id", None),
            )

        if not self._allow_local_default_auth():
            return None

        return AuthContext(
            subject={
                "user_id": "local-runner",
                "role": "manager",
                "permissions": _normalize_permissions(["read", "write", "execute", "manage"]),
            },
            resource={"type": tool.name},
            action=tool.operation_type.value,
            environment={"mode": "local"},
            tenant_id="local",
        )

    async def _log_completion(
        self,
        tool: RegisteredTool,
        task_id: str,
        metadata: dict[str, Any],
        flow_context: Any,
    ) -> None:
        """完了監査ログを記録."""
        user_id = flow_context.user_id if flow_context else None

        event = EnterpriseAuditEvent(
            event_type=AuditEventType.TOOL_EXECUTION,
            severity=AuditSeverity.INFO,
            tool_name=tool.name,
            decision="complete",
            reason="Stage execution completed",
            trace_id=task_id,
            run_id=self._run_id,
            flow_id=self._flow_id,
            user_id=user_id,
            metadata=metadata,
        )

        # 非同期でログ記録
        await self._audit_logger.log_event_async(event)

        # Emit Node Complete Event
        await self._emit_step_event("node_complete", tool.name, result=metadata)

    async def _emit_step_event(self, event_type: str, node_name: str, result: dict[str, Any] | None = None) -> None:
        """ステップイベントをキューに送信."""
        if self._event_queue:
            event = {
                "event": event_type,
                "node": node_name,
                "timestamp": 0,  # timestamp helps, but can be added by wrapper
            }
            if result:
                event["result"] = result

            await self._event_queue.put(event)

    async def _run_pipeline(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """工程固定パイプラインを実行.

        HITL 承認ポイント:
          - 設計工程後に人間承認を要求（HIGH リスク）

        Kill Switch:
          - 各工程前にチェック → killed なら即時停止

        GovernanceEngine:
          - 全工程の開始（evaluate_tool経由）と完了を監査ログに記録
        """
        from apps.code_migration_assistant.workflow.pipeline_runtime import run_pipeline as run_pipeline_runtime

        return await run_pipeline_runtime(self, inputs)

    def _build_task_spec(
        self,
        *,
        task_id: str,
        trace_id: str,
        module: str,
        expected_outputs: dict[str, Any],
        options: Any,
    ) -> TaskSpec:
        """TaskSpec を構築."""
        migration_config = self._factory.get_migration_config(self._migration_type)
        source_language = str(migration_config.get("source", {}).get("language", "COBOL"))
        target_language = str(migration_config.get("target", {}).get("language", "Java"))
        normalized_options = options if isinstance(options, dict) else {}

        return TaskSpec(
            task_id=task_id,
            trace_id=trace_id,
            migration_type=self._migration_type,
            source_language=source_language,
            target_language=target_language,
            module=module,
            expected_outputs=expected_outputs,
            options=normalized_options,
        )

    async def _load_known_legacy_issues(self, inputs: dict[str, Any]) -> list[dict[str, Any]]:
        """既知旧不具合リストを読み込む."""
        raw_path = str(
            inputs.get(
                "known_legacy_issues_path",
                "apps/code_migration_assistant/specs/known_legacy_issues.json",
            )
        )
        path = Path(raw_path)
        if not path.exists():
            return []

        raw = path.read_text(encoding="utf-8")

        try:
            parsed = json.loads(raw)
        except json.JSONDecodeError:
            return []

        if not isinstance(parsed, list):
            return []

        normalized: list[dict[str, Any]] = []
        for item in parsed:
            if isinstance(item, dict):
                normalized.append(item)
        return normalized

    def _append_capability_trace(self, trace: dict[str, Any]) -> None:
        """Capability 実行トレースを追記する."""
        if isinstance(trace, dict):
            self._capability_trace.append(trace.copy())

    def get_capability_trace(self) -> list[dict[str, Any]]:
        """Capability 実行トレースを返す."""
        return [item.copy() for item in self._capability_trace]

    async def _run_transformation_with_reflection(
        self,
        *,
        source_code: str,
        analysis: dict[str, Any],
        migration_design: dict[str, Any],
        fast_mode: bool,
        acceptance_threshold: float,
        max_auto_iterations: int,
    ) -> tuple[TransformationArtifact, TransformationIterationArtifact]:
        """変換 + 軽量リフレクションを実行する."""
        del analysis
        if self._code_transformation_agent is None:
            msg = "CodeTransformationAgent is not initialized"
            raise RuntimeError(msg)

        self._append_capability_trace(
            {
                "stage": "transform",
                "capability_id": "reflection_pattern",
                "provider": "native",
                "status": "applied",
            }
        )

        records: list[TransformationIterationRecord] = []
        feedback: list[str] = []
        final_artifact: TransformationArtifact | None = None

        max_iterations = max(1, int(max_auto_iterations))
        for iteration in range(1, max_iterations + 1):
            payload = self._code_transformation_agent.process(
                {
                    "source_code": source_code,
                    "migration_design": migration_design,
                    "fast_mode": fast_mode,
                    "reflection_feedback": feedback,
                }
            )
            validated_artifact = self._validate_or_fail(TransformationArtifact, payload, "code")
            if not isinstance(validated_artifact, TransformationArtifact):
                msg = "invalid transformation artifact"
                raise RuntimeError(msg)

            final_artifact = validated_artifact
            warning_penalty = float(len(validated_artifact.warnings) * 10)
            score = max(0.0, 95.0 - warning_penalty)
            accepted = score >= acceptance_threshold or iteration >= max_iterations
            records.append(
                TransformationIterationRecord(
                    iteration=iteration,
                    score=score,
                    accepted=accepted,
                    feedback=list(feedback),
                    suggestions=[],
                )
            )
            if accepted:
                break
            feedback = ["improve generated code quality"]

        self._append_capability_trace(
            {
                "stage": "transform",
                "capability_id": "code_validator",
                "provider": "native",
                "status": "applied",
            }
        )
        self._append_capability_trace(
            {
                "stage": "transform",
                "capability_id": "memory_system",
                "provider": "native",
                "status": "applied",
            }
        )

        if final_artifact is None:
            msg = "transformation did not produce artifact"
            raise RuntimeError(msg)

        iteration_artifact = TransformationIterationArtifact(
            meta=build_meta(
                task_id=final_artifact.meta.task_id,
                trace_id=final_artifact.meta.trace_id,
                stage="code",
                source_language=final_artifact.meta.source_language,
                target_language=final_artifact.meta.target_language,
                module=final_artifact.meta.module,
            ),
            iterations=records,
            accepted=records[-1].accepted if records else False,
            final_score=records[-1].score if records else None,
            unknowns=[],
            extensions={},
        )
        return final_artifact, iteration_artifact

    def _validate_or_fail(
        self,
        model: type[
            LegacyAnalysisArtifact
            | BusinessSemanticsArtifact
            | MigrationDesignArtifact
            | TransformationArtifact
            | TransformationIterationArtifact
            | TestSynthesisArtifact
            | DifferentialVerificationArtifact
            | QualityGateArtifact
            | LimitedFixArtifact
        ],
        payload: dict[str, Any],
        stage: str,
    ) -> (
        LegacyAnalysisArtifact
        | BusinessSemanticsArtifact
        | MigrationDesignArtifact
        | TransformationArtifact
        | TransformationIterationArtifact
        | TestSynthesisArtifact
        | DifferentialVerificationArtifact
        | QualityGateArtifact
        | LimitedFixArtifact
        | None
    ):
        """成果物のスキーマ検証を行う."""
        try:
            return model.model_validate(payload)
        except Exception as exc:
            self._logger.exception("%s stage artifact validation failed: %s", stage, exc)
            return None

    def _ensure_agents_ready(self) -> None:
        """Agent 初期化を確認."""
        if (
            self._legacy_analysis_agent is None
            or self._business_semantics_agent is None
            or self._migration_design_agent is None
            or self._code_transformation_agent is None
            or self._test_synthesis_agent is None
            or self._differential_agent is None
            or self._quality_gate_agent is None
            or self._limited_fixer_agent is None
            or self._compliance_reporter_agent is None
        ):
            msg = "Agents are not initialized"
            raise RuntimeError(msg)

    async def get_transition_samples(self, run_id: str) -> list[dict[str, Any]]:
        """指定 run_id の学習用トランジションを取得."""
        store = self.config.lightning_store
        if store is None:
            return []

        events = await store.list_events(run_id)
        rewards = await store.list_rewards(run_id)
        samples = TrajectoryAdapter.to_transition_samples(events=events, rewards=rewards)
        return [asdict(sample) for sample in samples]

    async def get_latest_transition_samples(self) -> list[dict[str, Any]]:
        """直近実行の学習用トランジションを取得."""
        store = self.config.lightning_store
        if store is None:
            return []

        run_ids = await store.list_run_ids()
        if not run_ids:
            return []
        return await self.get_transition_samples(run_ids[-1])

    async def train_latest_run(
        self,
        *,
        apply_optimized_profile: bool = True,
    ) -> dict[str, Any]:
        """直近 run のトレースから学習/最適化を実行."""
        result = await self.train_lightning(
            LightningTrainingRequest(
                run_id=None,
                apply_optimized_profile=apply_optimized_profile,
            )
        )
        return result.model_dump()

    def get_optimized_llm_profile(self) -> dict[str, Any]:
        """現在適用中の最適化 LLM プロファイルを取得."""
        profile = self.config.llm_config.get("optimized_profile")
        if isinstance(profile, dict):
            return profile
        return {}
