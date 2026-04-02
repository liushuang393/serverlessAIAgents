"""Messaging Hub 向けの focused subagents と semantic tool contracts."""

from __future__ import annotations

from enum import StrEnum
from typing import Any

from pydantic import BaseModel, Field


class SemanticOperationType(StrEnum):
    """意味単位の操作種別."""

    READ = "read"
    ANALYZE = "analyze"
    DRAFT = "draft"
    WRITE = "write"
    ADVISE = "advise"


class FocusedWorkerRole(StrEnum):
    """coordinator 内で分離する focused subagent 役割."""

    TRIAGE = "triage"
    ACTION_ROUTER = "action_router"
    DOMAIN_WORKER = "domain_worker"
    SUMMARY = "summary"


class SemanticToolContract(BaseModel):
    """狭く意味づけされた connector/tool 契約."""

    contract_id: str = Field(..., min_length=1)
    template_name: str = Field(..., min_length=1)
    action_name: str = Field(..., min_length=1)
    description: str = Field(..., min_length=1)
    auth_scope: list[str] = Field(default_factory=list)
    idempotency_keys: list[str] = Field(default_factory=list)
    error_surface: list[str] = Field(default_factory=list)
    operation_type: SemanticOperationType = Field(default=SemanticOperationType.READ)
    backing_skills: list[str] = Field(default_factory=list)
    requires_human_gate: bool = Field(default=False)
    notes: list[str] = Field(default_factory=list)


class FocusedTaskPlan(BaseModel):
    """focused subagent が決定した task plan."""

    template_name: str = Field(..., min_length=1)
    handler_name: str = Field(..., min_length=1)
    triage_reason: str = Field(..., min_length=1)
    worker_roles: list[FocusedWorkerRole] = Field(default_factory=list)
    semantic_contracts: list[SemanticToolContract] = Field(default_factory=list)
    policy_hooks: list[str] = Field(default_factory=list)
    route_tags: list[str] = Field(default_factory=list)
    summary_mode: str = Field(default="executive")


class SemanticToolContractRegistry:
    """テンプレート別 semantic tool contract registry."""

    def contracts_for_template(
        self,
        *,
        template_name: str,
        params: dict[str, Any] | None = None,
    ) -> list[SemanticToolContract]:
        """テンプレートに応じた契約一覧を返す."""
        normalized = template_name or "general"
        payload = params or {}

        if normalized == "email_organize":
            return [
                self._contract(
                    contract_id="mailbox.inspect_snapshot",
                    template_name=normalized,
                    action_name="inspect_mailbox_snapshot",
                    description="受信箱候補を読み取り、優先度分類に必要な一覧を取得する",
                    auth_scope=["mail.read", "fs.read"],
                    idempotency_keys=["folder", "days"],
                    error_surface=["folder_not_found", "mailbox_unavailable"],
                    operation_type=SemanticOperationType.READ,
                    backing_skills=["list_dir"],
                )
            ]

        if normalized == "file_organize":
            return [
                self._contract(
                    contract_id="workspace.inspect_files",
                    template_name=normalized,
                    action_name="inspect_workspace_files",
                    description="対象フォルダを走査し、分類候補を集める",
                    auth_scope=["fs.read"],
                    idempotency_keys=["path", "days_old"],
                    error_surface=["path_not_found", "permission_denied"],
                    operation_type=SemanticOperationType.READ,
                    backing_skills=["list_dir"],
                )
            ]

        if normalized == "system_optimize":
            return [
                self._contract(
                    contract_id="system.inspect_usage",
                    template_name=normalized,
                    action_name="inspect_system_usage",
                    description="最適化前にリソース状況を取得する",
                    auth_scope=["system.read"],
                    idempotency_keys=["level"],
                    error_surface=["resource_probe_failed"],
                    operation_type=SemanticOperationType.READ,
                    backing_skills=["get_resource_usage"],
                )
            ]

        if normalized == "research":
            return [
                self._contract(
                    contract_id="research.collect_sources",
                    template_name=normalized,
                    action_name="collect_research_sources",
                    description="指定トピックの外部ソースを取得して要点化する",
                    auth_scope=["net.read"],
                    idempotency_keys=["topic", "depth"],
                    error_surface=["network_source_unavailable", "research_parse_failed"],
                    operation_type=SemanticOperationType.ANALYZE,
                    backing_skills=["http_request"],
                )
            ]

        if normalized == "competitor_analysis":
            return [
                self._contract(
                    contract_id="market.collect_competitor_signals",
                    template_name=normalized,
                    action_name="collect_competitor_signals",
                    description="競合シグナルを収集し、観点別に比較可能な形へ整える",
                    auth_scope=["net.read", "browser.read"],
                    idempotency_keys=["competitor", "aspects"],
                    error_surface=["competitor_source_unavailable", "browser_disabled"],
                    operation_type=SemanticOperationType.ANALYZE,
                    backing_skills=["http_request"],
                )
            ]

        if normalized == "report":
            contracts = [
                self._contract(
                    contract_id="report.create_draft",
                    template_name=normalized,
                    action_name="create_report_draft",
                    description="指定条件からレポート本文の草稿を生成する",
                    auth_scope=["llm.generate"],
                    idempotency_keys=["title", "format", "audience", "objective"],
                    error_surface=["draft_generation_failed"],
                    operation_type=SemanticOperationType.DRAFT,
                    backing_skills=["llm_generate"],
                )
            ]
            if str(payload.get("format", "markdown")).lower() == "markdown":
                contracts.append(
                    self._contract(
                        contract_id="report.persist_draft",
                        template_name=normalized,
                        action_name="persist_report_draft",
                        description="生成済みレポート草稿をローカルへ保存する",
                        auth_scope=["fs.write"],
                        idempotency_keys=["title", "format"],
                        error_surface=["report_file_write_failed", "permission_denied"],
                        operation_type=SemanticOperationType.WRITE,
                        backing_skills=["write_file"],
                        requires_human_gate=True,
                    )
                )
            return contracts

        if normalized == "business_advice":
            return [
                self._contract(
                    contract_id="business.compose_advice",
                    template_name=normalized,
                    action_name="compose_business_advice",
                    description="事業相談を advisory agent に委譲し、実行可能な提案を返す",
                    auth_scope=["advisor.invoke"],
                    idempotency_keys=["question"],
                    error_surface=["advisor_unavailable", "business_advice_error"],
                    operation_type=SemanticOperationType.ADVISE,
                    backing_skills=["biz_minimalist_review"],
                )
            ]

        return [
            self._contract(
                contract_id="general.reasoning",
                template_name="general",
                action_name="general_reasoning_response",
                description="汎用質問に対して reasoning ベースで応答する",
                auth_scope=["llm.generate"],
                idempotency_keys=["request"],
                error_surface=["general_task_error"],
                operation_type=SemanticOperationType.ADVISE,
                backing_skills=["llm_generate"],
            )
        ]

    def list_contracts(self) -> list[dict[str, Any]]:
        """公開向けに全契約を返す."""
        items: list[dict[str, Any]] = []
        for template_name in (
            "email_organize",
            "file_organize",
            "system_optimize",
            "research",
            "competitor_analysis",
            "report",
            "business_advice",
            "general",
        ):
            contracts = self.contracts_for_template(template_name=template_name, params={"format": "markdown"})
            items.extend(contract.model_dump(mode="json") for contract in contracts)
        return items

    @staticmethod
    def _contract(
        *,
        contract_id: str,
        template_name: str,
        action_name: str,
        description: str,
        auth_scope: list[str],
        idempotency_keys: list[str],
        error_surface: list[str],
        operation_type: SemanticOperationType,
        backing_skills: list[str],
        requires_human_gate: bool = False,
    ) -> SemanticToolContract:
        """契約を組み立てる."""
        notes = ["narrow_semantic_api", "deterministic_contract_surface"]
        if requires_human_gate:
            notes.append("human_gate_required")
        return SemanticToolContract(
            contract_id=contract_id,
            template_name=template_name,
            action_name=action_name,
            description=description,
            auth_scope=auth_scope,
            idempotency_keys=idempotency_keys,
            error_surface=error_surface,
            operation_type=operation_type,
            backing_skills=backing_skills,
            requires_human_gate=requires_human_gate,
            notes=notes,
        )


class TaskTriageSubagent:
    """意図を focused task plan へ変換する triage subagent."""

    _HANDLER_MAP: dict[str, str] = {
        "email_organize": "_execute_email_organize",
        "file_organize": "_execute_file_organize",
        "system_optimize": "_execute_system_optimize",
        "research": "_execute_research",
        "competitor_analysis": "_execute_competitor_analysis",
        "report": "_execute_report",
        "business_advice": "_execute_business_advice",
        "general": "_execute_general_task",
    }

    def __init__(self, contract_registry: SemanticToolContractRegistry) -> None:
        self._contract_registry = contract_registry

    def plan(
        self,
        *,
        template_name: str | None,
        parameters: dict[str, Any] | None,
        original_text: str,
        security_mode: str,
    ) -> FocusedTaskPlan:
        """focused task plan を返す."""
        resolved_template = template_name or "general"
        params = parameters or {}
        semantic_contracts = self._contract_registry.contracts_for_template(
            template_name=resolved_template,
            params=params,
        )
        handler_name = self._HANDLER_MAP.get(resolved_template, "_execute_general_task")
        policy_hooks = self._build_policy_hooks(semantic_contracts=semantic_contracts, security_mode=security_mode)
        route_tags = [resolved_template, security_mode]
        if any(contract.operation_type == SemanticOperationType.WRITE for contract in semantic_contracts):
            route_tags.append("mutation_path")
        triage_reason = (
            f"template={resolved_template} を選択し、{len(semantic_contracts)} 件の semantic contract を割り当てました"
        )
        return FocusedTaskPlan(
            template_name=resolved_template,
            handler_name=handler_name,
            triage_reason=triage_reason if original_text else triage_reason,
            worker_roles=[
                FocusedWorkerRole.TRIAGE,
                FocusedWorkerRole.ACTION_ROUTER,
                FocusedWorkerRole.DOMAIN_WORKER,
                FocusedWorkerRole.SUMMARY,
            ],
            semantic_contracts=semantic_contracts,
            policy_hooks=policy_hooks,
            route_tags=route_tags,
            summary_mode="executive",
        )

    @staticmethod
    def _build_policy_hooks(
        *,
        semantic_contracts: list[SemanticToolContract],
        security_mode: str,
    ) -> list[str]:
        """契約から policy hooks を導く."""
        hooks = ["semantic_contract_review", f"security_mode:{security_mode}"]
        if any(contract.operation_type == SemanticOperationType.WRITE for contract in semantic_contracts):
            hooks.append("pre_write_checkpoint")
        if any(contract.requires_human_gate for contract in semantic_contracts):
            hooks.append("human_gate")
        if security_mode != "autonomous":
            hooks.append("strict_control_overlay")
        return hooks


class ActionRouterSubagent:
    """focused task plan を具体 handler に接続する action router."""

    def resolve_handler(
        self,
        *,
        plan: FocusedTaskPlan,
        available_handlers: dict[str, Any],
    ) -> Any:
        """plan に対応する handler callable を返す."""
        handler = available_handlers.get(plan.handler_name)
        if handler is None:
            handler = available_handlers.get("_execute_general_task")
        return handler


class SummarySubagent:
    """実行結果へ focused route metadata を注入する summary subagent."""

    def enrich_result(
        self,
        *,
        plan: FocusedTaskPlan,
        result: dict[str, Any],
    ) -> dict[str, Any]:
        """summary builder 前に route metadata を追加する."""
        enriched = dict(result)
        enriched["focused_workers"] = [role.value for role in plan.worker_roles]
        enriched["semantic_actions"] = [contract.action_name for contract in plan.semantic_contracts]
        enriched["semantic_contracts"] = [contract.model_dump(mode="json") for contract in plan.semantic_contracts]
        enriched["policy_hooks"] = list(plan.policy_hooks)
        enriched["route_tags"] = list(plan.route_tags)
        summary_points = enriched.get("summary_points")
        if isinstance(summary_points, list):
            summary_points.append(f"semantic actions: {', '.join(enriched['semantic_actions'])}")
        else:
            enriched["summary_points"] = [f"semantic actions: {', '.join(enriched['semantic_actions'])}"]
        return enriched


__all__ = [
    "ActionRouterSubagent",
    "FocusedTaskPlan",
    "FocusedWorkerRole",
    "SemanticOperationType",
    "SemanticToolContract",
    "SemanticToolContractRegistry",
    "SummarySubagent",
    "TaskTriageSubagent",
]
