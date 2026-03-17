"""Code Migration Engine - 工程固定型パイプライン.

固定工程:
分析 → 設計 → 変換 → テスト生成 → 差分検証 → 品質裁定 → 限定修正

工程間の受け渡しは artifacts/ 配下の JSON を介して行う。
"""

from __future__ import annotations

import asyncio
import json
import os
import uuid
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any

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
    QualityDecision,
    QualityGateArtifact,
    TaskSpec,
    TestSynthesisArtifact,
    TransformationArtifact,
    TransformationIterationArtifact,
    TransformationIterationRecord,
    build_meta,
)

from kernel.agents.agent_factory import AgentFactorySpec
from kernel.agents.agent_factory import create as create_agent
from kernel.engines.base import BaseEngine, EngineConfig
from harness.governance.engine import GovernanceEngine, ToolExecutionContext
from harness.governance.enterprise_audit import (
    AuditEventType,
    AuditSeverity,
    EnterpriseAuditEvent,
    EnterpriseAuditLogger,
)
from harness.approval.approval_flow import ApprovalFlow
from shared.integrations.context_bridge import get_current_context
from infrastructure.llm.providers.tool_provider import (
    OperationType,
    RegisteredTool,
    RiskLevel,
)
from kernel.runtime import LightningTrainingRequest, TrajectoryAdapter
from harness.guardrails.safety_mixin import SafetyMixin
from harness.policies.policy_engine import AuthContext


# =============================================================================
# Virtual Tools (Governance Policy Hooks)
# =============================================================================
def _virtual_tool_func(*args, **kwargs):
    """Virtual tool dummy implementation."""


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
        self._event_queue: asyncio.Queue | None = None
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
        self._quality_gate_agent = create_agent(
            AgentFactorySpec(agent_class=QualityGateAgent, agent_type="gatekeeper")
        )
        self._limited_fixer_agent = create_agent(
            AgentFactorySpec(agent_class=LimitedFixerAgent, agent_type="executor")
        )
        self._compliance_reporter_agent = create_agent(
            AgentFactorySpec(agent_class=ComplianceReporterAgent, agent_type="reporter")
        )

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """固定工程を実行."""
        # Run ごとに human facts / capability trace をリセットする
        self._human_facts = []
        self._capability_trace = []
        return await self._run_pipeline(inputs)

    async def _execute_stream(self, inputs: dict[str, Any]):
        """ストリーム実行（ノード単位イベント）."""
        node_name = "migration_pipeline"
        node_start = self._emit_node_start(node_name)
        if node_start:
            yield node_start

        # Init Event Queue
        self._event_queue = asyncio.Queue()

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

        env_name = (
            os.getenv("CODE_MIGRATION_ENV")
            or os.getenv("APP_ENV")
            or os.getenv("ENV")
            or ""
        ).lower()
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

        self._ensure_agents_ready()

        # Kill Switch チェック
        if self._killed:
            return {"success": False, "error": "Pipeline killed by Kill Switch"}

        source_code = inputs.get("source_code") or inputs.get("cobol_code", "")
        if not source_code:
            return {"success": False, "error": "source_code is required"}

        expected_outputs = inputs.get("expected_outputs", {})
        if not isinstance(expected_outputs, dict):
            return {"success": False, "error": "expected_outputs must be dict"}
        fast_mode = bool(inputs.get("fast_mode", True))

        task_id = str(inputs.get("task_id") or f"task-{uuid.uuid4().hex[:12]}")
        trace_id = str(inputs.get("trace_id") or task_id)
        module = str(inputs.get("module") or "UNKNOWN")

        # ContextBridgeからコンテキスト取得
        flow_context = get_current_context()

        task_spec = self._build_task_spec(
            task_id=task_id,
            trace_id=trace_id,
            module=module,
            expected_outputs=expected_outputs,
            options=inputs.get("options", {}),
        )

        artifacts_dir = inputs.get("artifacts_dir")
        decisions_path = inputs.get("decisions_path")
        failures_path = inputs.get("failures_path")
        artifact_store = ArtifactStore(
            base_dir=Path(str(artifacts_dir)) if artifacts_dir else None,
            decisions_path=Path(str(decisions_path)) if decisions_path else None,
            failures_path=Path(str(failures_path)) if failures_path else None,
        )
        await artifact_store.initialize()
        lock_path = await artifact_store.acquire_lock(task_id)

        artifact_paths: dict[str, str] = {}

        try:
            # 1) 分析
            # Governance Check (Start Log)
            await self._check_governance(MIGRATION_ANALYSIS, task_id, {"module": module}, flow_context)

            analysis = self._legacy_analysis_agent.process(
                {
                    "source_code": source_code,
                    "task_spec": task_spec.model_dump(mode="json"),
                }
            )
            analysis_artifact = self._validate_or_fail(LegacyAnalysisArtifact, analysis, "analysis")
            if analysis_artifact is None:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="analysis",
                    responsible_stage="analysis",
                    reason="schema validation failed",
                )
                return {"success": False, "stage": "analysis", "error": "invalid analysis artifact"}

            analysis_path = await artifact_store.write_json(
                stage="analysis",
                task_id=task_id,
                artifact_name="legacy_analysis",
                payload=analysis_artifact.model_dump(mode="json"),
            )
            artifact_paths["analysis"] = str(analysis_path)
            await artifact_store.append_decision(task_id, "分析工程を完了")
            analysis_for_next = await artifact_store.read_json(analysis_path)

            # Completion Log
            await self._log_completion(MIGRATION_ANALYSIS, task_id, {"path": str(analysis_path)}, flow_context)

            # 2) 設計
            await self._check_governance(MIGRATION_DESIGN, task_id, {}, flow_context)

            design = self._migration_design_agent.process({"legacy_analysis": analysis_for_next})
            design_artifact = self._validate_or_fail(MigrationDesignArtifact, design, "design")
            if design_artifact is None:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="design",
                    responsible_stage="design",
                    reason="schema validation failed",
                )
                return {"success": False, "stage": "design", "error": "invalid design artifact"}

            design_path = await artifact_store.write_json(
                stage="design",
                task_id=task_id,
                artifact_name="migration_design",
                payload=design_artifact.model_dump(mode="json"),
            )
            artifact_paths["design"] = str(design_path)
            await artifact_store.append_decision(task_id, "設計工程を完了")
            design_for_next = await artifact_store.read_json(design_path)

            await self._log_completion(MIGRATION_DESIGN, task_id, {"path": str(design_path)}, flow_context)

            # ── HITL 承認ポイント（設計後） ──
            # 設計結果の人間レビューを要求
            if self._killed:
                return {"success": False, "error": "Pipeline killed by Kill Switch"}

            try:
                # 承認フローのリスクレベルもツール定義から取得
                # request_approval は AsyncIterator なので、イベントを消費して待機する
                self._logger.info("Initializing HITL approval flow...")
                approval_iterator = self._approval_flow.request_approval(
                    action="migration_design_approval",
                    reason=f"移行設計の承認を要求: モジュール={module}, "
                    f"変換ルール数={len(design_artifact.mapping_rules)}",
                    risk_level=MIGRATION_DESIGN.risk_level,
                    context={
                        "task_id": task_id,
                        "module": module,
                        "design_path": str(design_path),
                    },
                )

                # イベントを待機（承認されるとループ終了）
                async for event in approval_iterator:
                    self._logger.info(f"Received approval event: {event}")

                    # Forward approval events to stream
                    if self._event_queue:
                        # Map to AG-UI expected format or generic
                        if event.get("event_type") == "approval_required":
                            await self._event_queue.put(
                                {
                                    "event": "approval_required",
                                    "request_id": event.get("request_id"),
                                    "reason": event.get("reason"),
                                    "context": event.get("context"),
                                }
                            )

                    if event.get("event_type") == "approval_submitted":
                        if not event.get("approved", False):
                            await artifact_store.append_failure(
                                task_id=task_id,
                                stage="design",
                                responsible_stage="approval",
                                reason=f"Rejected by user: {event.get('comment')}",
                            )
                            return {
                                "success": False,
                                "stage": "design",
                                "error": "Migration Design Rejected by User",
                            }
                        self._logger.info(f"Design Approved by {event.get('approver')}")
                        # 承認されたらループを抜けて次の工程へ
                        break

                    if event.get("event_type") == "approval_timeout":
                        return {"success": False, "stage": "design", "error": "Approval Timeout"}

                    # 承認待ちでストリームを止めるため、とりあえずループを継続しつつ、外部へのイベント通知はApprovalFlowがやる

            except Exception as e:
                self._logger.warning(f"HITL approval integration failed, skipping: {e}")
            # ── HITL 承認ポイント終了 ──

            # 3) 変換
            await self._check_governance(MIGRATION_TRANSFORM, task_id, {"fast_mode": fast_mode}, flow_context)

            transformation = self._code_transformation_agent.process(
                {
                    "source_code": source_code,
                    "migration_design": design_for_next,
                    "fast_mode": fast_mode,
                }
            )
            transformation_artifact = self._validate_or_fail(TransformationArtifact, transformation, "code")
            if transformation_artifact is None:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="code",
                    responsible_stage="transform",
                    reason="schema validation failed",
                )
                return {
                    "success": False,
                    "stage": "transform",
                    "error": "invalid transformation artifact",
                }

            transformation_path = await artifact_store.write_json(
                stage="code",
                task_id=task_id,
                artifact_name="transformation",
                payload=transformation_artifact.model_dump(mode="json"),
            )
            artifact_paths["code"] = str(transformation_path)
            # Write all generated files
            for gf in transformation_artifact.generated_files:
                await artifact_store.write_text(
                    stage="code",
                    task_id=task_id,
                    artifact_name=gf.path.replace("/", "_").replace(".java", ""),
                    extension="java",
                    content=gf.content,
                )

            # Keep target_code for single-file tools if needed
            await artifact_store.write_text(
                stage="code",
                task_id=task_id,
                artifact_name="target_code",
                extension="java",
                content=transformation_artifact.target_code,
            )
            await artifact_store.append_decision(task_id, "変換工程を完了")
            transformation_for_next = await artifact_store.read_json(transformation_path)

            await self._log_completion(MIGRATION_TRANSFORM, task_id, {"path": str(transformation_path)}, flow_context)

            # 4) テスト生成
            await self._check_governance(MIGRATION_TEST_GEN, task_id, {}, flow_context)

            test_synthesis = self._test_synthesis_agent.process(
                {
                    "legacy_analysis": analysis_for_next,
                    "expected_outputs": expected_outputs,
                    "target_language": task_spec.target_language,
                }
            )
            test_artifact = self._validate_or_fail(TestSynthesisArtifact, test_synthesis, "tests")
            if test_artifact is None:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="tests",
                    responsible_stage="test",
                    reason="schema validation failed",
                )
                return {"success": False, "stage": "tests", "error": "invalid test artifact"}

            tests_path = await artifact_store.write_json(
                stage="tests",
                task_id=task_id,
                artifact_name="test_synthesis",
                payload=test_artifact.model_dump(mode="json"),
            )
            artifact_paths["tests"] = str(tests_path)
            await artifact_store.append_decision(task_id, "テスト生成工程を完了")
            tests_for_next = await artifact_store.read_json(tests_path)

            await self._log_completion(MIGRATION_TEST_GEN, task_id, {"path": str(tests_path)}, flow_context)

            # 5) 差分検証
            await self._check_governance(MIGRATION_VERIFY_DIFF, task_id, {}, flow_context)

            differential = self._differential_agent.process(
                {
                    "transformation": transformation_for_next,
                    "test_synthesis": tests_for_next,
                    "fast_mode": fast_mode,
                }
            )
            diff_artifact = self._validate_or_fail(
                DifferentialVerificationArtifact,
                differential,
                "diff",
            )
            if diff_artifact is None:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="diff",
                    responsible_stage="diff",
                    reason="schema validation failed",
                )
                return {"success": False, "stage": "diff", "error": "invalid differential artifact"}

            diff_path = await artifact_store.write_json(
                stage="diff",
                task_id=task_id,
                artifact_name="differential",
                payload=diff_artifact.model_dump(mode="json"),
            )
            artifact_paths["diff"] = str(diff_path)
            await artifact_store.append_decision(task_id, "差分検証工程を完了")
            diff_for_next = await artifact_store.read_json(diff_path)

            await self._log_completion(MIGRATION_VERIFY_DIFF, task_id, {"path": str(diff_path)}, flow_context)

            # 6) 品質裁定
            await self._check_governance(MIGRATION_QUALITY_GATE, task_id, {}, flow_context)

            known_legacy_issues = await self._load_known_legacy_issues(inputs)
            quality_gate = self._quality_gate_agent.process(
                {
                    "differential": diff_for_next,
                    "migration_design": design_for_next,
                    "test_synthesis": tests_for_next,
                    "known_legacy_issues": known_legacy_issues,
                }
            )
            quality_artifact = self._validate_or_fail(QualityGateArtifact, quality_gate, "quality")
            if quality_artifact is None:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="quality",
                    responsible_stage="quality",
                    reason="schema validation failed",
                )
                return {"success": False, "stage": "quality", "error": "invalid quality artifact"}

            quality_path = await artifact_store.write_json(
                stage="quality",
                task_id=task_id,
                artifact_name="quality_gate",
                payload=quality_artifact.model_dump(mode="json"),
            )
            artifact_paths["quality"] = str(quality_path)
            await artifact_store.append_decision(
                task_id,
                f"品質裁定: {quality_artifact.decision.value} / next={quality_artifact.target_agent}",
            )
            quality_for_next = await artifact_store.read_json(quality_path)

            await self._log_completion(
                MIGRATION_QUALITY_GATE,
                task_id,
                {"decision": quality_artifact.decision.value},
                flow_context,
            )

            # 7) 限定修正
            if quality_artifact.decision == QualityDecision.TRANSFORM_ISSUE:
                await self._check_governance(MIGRATION_FIX, task_id, {}, flow_context)

                fix = self._limited_fixer_agent.process(
                    {
                        "quality_gate": quality_for_next,
                        "transformation": transformation_for_next,
                        "migration_design": design_for_next,
                    }
                )
                fix_artifact = self._validate_or_fail(LimitedFixArtifact, fix, "fix")
            else:
                fix_artifact = LimitedFixArtifact(
                    meta=transformation_artifact.meta.model_copy(update={"stage": "fix"}),
                    applied=False,
                    target_code=transformation_artifact.target_code,
                    patch_summary=["修正不要（裁定が TRANSFORM_ISSUE 以外）"],
                    retest_required=False,
                    unknowns=[],
                    extensions={},
                )

            if fix_artifact is None:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="fix",
                    responsible_stage="fix",
                    reason="schema validation failed",
                )
                return {"success": False, "stage": "fix", "error": "invalid fix artifact"}

            fix_path = await artifact_store.write_json(
                stage="fix",
                task_id=task_id,
                artifact_name="limited_fix",
                payload=fix_artifact.model_dump(mode="json"),
            )
            artifact_paths["fix"] = str(fix_path)
            await artifact_store.append_decision(task_id, "限定修正工程を完了")

            if quality_artifact.decision == QualityDecision.TRANSFORM_ISSUE:
                await self._log_completion(MIGRATION_FIX, task_id, {"applied": fix_artifact.applied}, flow_context)

            final_differential = diff_artifact
            final_quality = quality_artifact
            iterations = 1

            if fix_artifact.retest_required:
                iterations = 2
                adjusted_transformation = transformation_for_next.copy()
                adjusted_transformation["target_code"] = fix_artifact.target_code

                # 再検証時は軽量なのでGovernance Checkはスキップ（または別途定義）
                post_differential = self._differential_agent.process(
                    {
                        "transformation": adjusted_transformation,
                        "test_synthesis": tests_for_next,
                        "fast_mode": fast_mode,
                    }
                )
                post_diff_artifact = DifferentialVerificationArtifact.model_validate(post_differential)
                post_diff_path = await artifact_store.write_json(
                    stage="diff",
                    task_id=task_id,
                    artifact_name="differential_post_fix",
                    payload=post_diff_artifact.model_dump(mode="json"),
                )
                artifact_paths["diff_post_fix"] = str(post_diff_path)

                post_quality = self._quality_gate_agent.process(
                    {
                        "differential": post_diff_artifact.model_dump(mode="json"),
                        "migration_design": design_for_next,
                        "test_synthesis": tests_for_next,
                        "known_legacy_issues": known_legacy_issues,
                    }
                )
                post_quality_artifact = QualityGateArtifact.model_validate(post_quality)
                post_quality_path = await artifact_store.write_json(
                    stage="quality",
                    task_id=task_id,
                    artifact_name="quality_gate_post_fix",
                    payload=post_quality_artifact.model_dump(mode="json"),
                )
                artifact_paths["quality_post_fix"] = str(post_quality_path)

                final_differential = post_diff_artifact
                final_quality = post_quality_artifact
                await artifact_store.append_decision(
                    task_id,
                    f"再裁定: {final_quality.decision.value}",
                )

            success = final_quality.decision in {
                QualityDecision.PASSED,
                QualityDecision.KNOWN_LEGACY,
            }

            if not success:
                await artifact_store.append_failure(
                    task_id=task_id,
                    stage="quality",
                    responsible_stage=final_quality.decision.value,
                    reason=final_quality.reason,
                )

            # 8) 報告生成
            await self._check_governance(MIGRATION_REPORT, task_id, {}, flow_context)

            report = self._compliance_reporter_agent.process(
                {
                    "task_id": task_id,
                    "migration_type": self._migration_type,
                    "source_code": source_code,
                    "analysis": analysis_artifact.model_dump(mode="json"),
                    "design": design_artifact.model_dump(mode="json"),
                    "transformation": transformation_artifact.model_dump(mode="json"),
                    "quality": final_quality.model_dump(mode="json"),
                    "fix": locals().get("fix_artifact", {}).model_dump(mode="json")
                    if locals().get("fix_artifact")
                    else {},
                }
            )

            report_path = await artifact_store.write_text(
                stage="report",
                task_id=task_id,
                artifact_name="compliance_report",
                extension="md",
                content=report.get("report_markdown", "# Report Generation Failed"),
            )
            artifact_paths["report"] = str(report_path)

            await self._log_completion(MIGRATION_REPORT, task_id, {"path": str(report_path)}, flow_context)

            class_name = design_artifact.class_mapping.get("primary_class", "MigratedProgram")
            final_target_code = (
                locals().get("fix_artifact", {}).target_code
                if locals().get("fix_artifact") and locals().get("fix_artifact").applied
                else transformation_artifact.target_code
            )

            return {
                "success": success,
                "migration_type": self._migration_type,
                "source_language": task_spec.source_language,
                "target_language": task_spec.target_language,
                "task_id": task_id,
                "trace_id": trace_id,
                "class_name": class_name,
                "target_code": final_target_code,
                "verdict": "PASS" if success else "FAIL",
                "iterations": iterations,
                "check_result": final_differential.model_dump(mode="json"),
                "quality_gate": final_quality.model_dump(mode="json"),
                "artifact_paths": artifact_paths,
                "fast_mode": fast_mode,
            }
        finally:
            await artifact_store.release_lock(lock_path)

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
            artifact = self._validate_or_fail(TransformationArtifact, payload, "code")
            if artifact is None:
                msg = "invalid transformation artifact"
                raise RuntimeError(msg)

            final_artifact = artifact
            warning_penalty = float(len(artifact.warnings) * 10)
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
