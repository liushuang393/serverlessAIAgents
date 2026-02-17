# -*- coding: utf-8 -*-
"""Studio Service.

3 つの製品主線（Migration / FAQ / Assistant）向けの
テンプレート提供・実行・成果物参照を統一する。
"""

from __future__ import annotations

from datetime import UTC, datetime
from typing import Any
from uuid import uuid4

from apps.platform.services.app_discovery import AppDiscoveryService
from apps.platform.services.app_lifecycle import AppLifecycleManager


_STUDIO_DEFINITIONS: dict[str, dict[str, Any]] = {
    "migration": {
        "id": "migration",
        "name": "Migration Studio",
        "description": "コード移行の事前評価・提案・成果物生成",
        "default_surface_profile": "business",
    },
    "faq": {
        "id": "faq",
        "name": "Enterprise FAQ Studio",
        "description": "引用付きFAQサービスと索引運用",
        "default_surface_profile": "business",
    },
    "assistant": {
        "id": "assistant",
        "name": "Computer Assistant Studio",
        "description": "制御付きOS/ブラウザ操作と実行ログ管理",
        "default_surface_profile": "business",
    },
}


class StudioService:
    """Studio 製品線サービス."""

    def __init__(
        self,
        discovery: AppDiscoveryService,
        lifecycle: AppLifecycleManager,
    ) -> None:
        self._discovery = discovery
        self._lifecycle = lifecycle
        self._runs: dict[tuple[str, str], dict[str, Any]] = {}

    def list_studios(self) -> list[dict[str, Any]]:
        """Studio 一覧を返す."""
        rows: list[dict[str, Any]] = []
        for studio_id, definition in _STUDIO_DEFINITIONS.items():
            template_count = len(self.list_templates(studio_id))
            rows.append(
                {
                    **definition,
                    "template_count": template_count,
                },
            )
        return rows

    def list_templates(self, studio: str) -> list[dict[str, Any]]:
        """Studio ごとのテンプレート一覧を返す."""
        self._validate_studio(studio)
        templates: list[dict[str, Any]] = []

        for app in self._discovery.list_apps():
            if app.product_line != studio:
                continue
            templates.append(
                {
                    "template_id": f"{studio}:{app.name}",
                    "app_name": app.name,
                    "display_name": app.display_name,
                    "description": app.description,
                    "risk_level": "medium",
                    "security_mode": app.security_mode,
                },
            )

        return templates

    def create_run(self, studio: str, payload: dict[str, Any]) -> dict[str, Any]:
        """Studio 実行を登録する."""
        self._validate_studio(studio)
        now = datetime.now(UTC).isoformat()
        run_id = uuid4().hex[:12]
        template_id = str(payload.get("template_id") or "").strip()
        if not template_id:
            template_id = f"{studio}:default"

        artifacts = self._build_artifacts(studio, template_id)
        run = {
            "id": run_id,
            "studio": studio,
            "template_id": template_id,
            "app_name": payload.get("app_name"),
            "data_sources": payload.get("data_sources", []),
            "permission_scopes": payload.get("permission_scopes", []),
            "risk_level": payload.get("risk_level", "medium"),
            "security_mode": payload.get("security_mode"),
            "status": "completed",
            "created_at": now,
            "updated_at": now,
            "artifacts": artifacts,
        }
        self._runs[(studio, run_id)] = run
        return {
            "run_id": run_id,
            "studio": studio,
            "status": "completed",
            "created_at": now,
            "artifact_count": len(artifacts),
        }

    def get_run_artifacts(self, studio: str, run_id: str) -> dict[str, Any]:
        """実行成果物を返す."""
        self._validate_studio(studio)
        run = self._runs.get((studio, run_id))
        if run is None:
            msg = f"Run not found: studio={studio}, run_id={run_id}"
            raise KeyError(msg)
        return {
            "run_id": run_id,
            "studio": studio,
            "status": run["status"],
            "artifacts": run["artifacts"],
        }

    @staticmethod
    def _validate_studio(studio: str) -> None:
        if studio not in _STUDIO_DEFINITIONS:
            msg = f"Unknown studio: {studio}"
            raise ValueError(msg)

    @staticmethod
    def _build_artifacts(studio: str, template_id: str) -> list[dict[str, Any]]:
        """Studio ごとの成果物メタデータを生成."""
        if studio == "migration":
            return [
                {
                    "artifact_id": "risk-report",
                    "name": "migration_risk_report.json",
                    "type": "risk_report",
                    "download_path": f"/artifacts/{template_id}/migration_risk_report.json",
                },
                {
                    "artifact_id": "evidence-report",
                    "name": "safety_evidence.md",
                    "type": "evidence",
                    "download_path": f"/artifacts/{template_id}/safety_evidence.md",
                },
                {
                    "artifact_id": "pr-plan",
                    "name": "migration_pr_plan.md",
                    "type": "proposal",
                    "download_path": f"/artifacts/{template_id}/migration_pr_plan.md",
                },
            ]
        if studio == "faq":
            return [
                {
                    "artifact_id": "faq-citations",
                    "name": "faq_with_citations.json",
                    "type": "faq_bundle",
                    "download_path": f"/artifacts/{template_id}/faq_with_citations.json",
                },
                {
                    "artifact_id": "index-status",
                    "name": "index_status.json",
                    "type": "index_status",
                    "download_path": f"/artifacts/{template_id}/index_status.json",
                },
                {
                    "artifact_id": "ops-report",
                    "name": "faq_ops_report.md",
                    "type": "ops_report",
                    "download_path": f"/artifacts/{template_id}/faq_ops_report.md",
                },
            ]
        return [
            {
                "artifact_id": "task-log",
                "name": "assistant_task_log.json",
                "type": "execution_log",
                "download_path": f"/artifacts/{template_id}/assistant_task_log.json",
            },
            {
                "artifact_id": "memory-state",
                "name": "managed_memory_state.json",
                "type": "memory_snapshot",
                "download_path": f"/artifacts/{template_id}/managed_memory_state.json",
            },
            {
                "artifact_id": "security-audit",
                "name": "security_mode_audit.md",
                "type": "security_audit",
                "download_path": f"/artifacts/{template_id}/security_mode_audit.md",
            },
        ]
