"""Generated artifact lifecycle manager for Messaging Hub."""

from __future__ import annotations

import hashlib
import re
import uuid
from datetime import UTC, datetime
from enum import StrEnum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from apps.messaging_hub.skills_manager import SkillsManager, WorkflowStatus


if TYPE_CHECKING:
    from apps.messaging_hub.storage.sqlite_store import SQLiteMessagingHubStore


class ArtifactType(StrEnum):
    """生成物タイプ."""

    SKILL = "skill"
    AGENT = "agent"


class ArtifactStatus(StrEnum):
    """生成物状態."""

    DRAFT = "draft"
    RUNTIME_ACTIVE = "runtime_active"
    VALIDATED = "validated"
    APPROVED = "approved"
    PUBLISHED = "published"
    REJECTED = "rejected"


class GeneratedArtifactRecord(BaseModel):
    """生成物レコード."""

    artifact_id: str = Field(..., min_length=1)
    artifact_type: ArtifactType = Field(default=ArtifactType.SKILL)
    name: str = Field(..., min_length=1)
    description: str = Field(..., min_length=1)
    status: ArtifactStatus = Field(default=ArtifactStatus.DRAFT)
    capabilities: list[str] = Field(default_factory=list)
    scopes: list[str] = Field(default_factory=list)
    allowed_tools: list[str] = Field(default_factory=list)
    allowed_mcp_servers: list[str] = Field(default_factory=list)
    dependencies: list[str] = Field(default_factory=list)
    guardrails: list[str] = Field(default_factory=list)
    tests: list[dict[str, Any]] = Field(default_factory=list)
    workflow_definition: dict[str, Any] | None = Field(default=None)
    runtime_binding: dict[str, Any] | None = Field(default=None)
    approval_request_id: str | None = Field(default=None)
    metadata: dict[str, Any] = Field(default_factory=dict)
    created_at: str = Field(default_factory=lambda: datetime.now(UTC).isoformat())
    updated_at: str = Field(default_factory=lambda: datetime.now(UTC).isoformat())

    def to_store_dict(self) -> dict[str, Any]:
        """ストア保存用辞書に変換する."""
        payload = self.model_dump()
        payload["artifact_type"] = self.artifact_type.value
        payload["status"] = self.status.value
        return payload


class GeneratedArtifactManager:
    """生成物の lifecycle を管理する."""

    _SCOPE_HINTS: tuple[tuple[str, str], ...] = (
        ("flight", "net.http"),
        ("ticket", "net.http"),
        ("travel", "net.http"),
        ("price", "net.http"),
        ("watch", "net.http"),
        ("monitor", "net.http"),
        ("browser", "browser.read"),
        ("web", "browser.read"),
        ("email", "secrets.use"),
        ("file", "fs.read"),
        ("organize", "fs.write"),
        ("write", "fs.write"),
    )

    _CAPABILITY_HINTS: tuple[tuple[str, str], ...] = (
        ("flight", "flight_watch"),
        ("ticket", "flight_watch"),
        ("travel", "travel_search"),
        ("price", "price_monitoring"),
        ("watch", "monitoring"),
        ("monitor", "monitoring"),
        ("notify", "notification"),
        ("email", "notification"),
        ("file", "file_organization"),
        ("meeting", "meeting"),
        ("business", "business_advice"),
        ("research", "research"),
    )

    _WORKFLOW_KEYWORDS: tuple[tuple[str, tuple[str, ...]], ...] = (
        ("flight_watch", ("web_search", "http_request", "browser_navigate", "browser_get_text")),
        ("travel_search", ("web_search", "http_request", "browser_navigate", "browser_get_text")),
        ("research", ("web_search", "http_request", "read_file")),
        ("notification", ("read_file",)),
        ("file_organization", ("list_dir",)),
    )

    def __init__(
        self,
        *,
        store: SQLiteMessagingHubStore,
        skills_manager: SkillsManager,
    ) -> None:
        """初期化."""
        self._store = store
        self._skills_manager = skills_manager

    async def create_runtime_artifact(
        self,
        *,
        description: str,
        examples: list[str] | None = None,
        requested_by: str,
        artifact_type: ArtifactType = ArtifactType.SKILL,
        metadata: dict[str, Any] | None = None,
    ) -> GeneratedArtifactRecord:
        """runtime_active な生成物を作成する."""
        normalized_description = description.strip()
        artifact_name = self._make_artifact_name(normalized_description, artifact_type)
        capabilities = self._infer_capabilities(normalized_description)
        required_capability = str((metadata or {}).get("required_capability", "")).strip()
        if required_capability and required_capability not in capabilities:
            capabilities.insert(0, required_capability)
        scopes = self._infer_scopes(normalized_description)
        available_tools = await self._list_available_tool_names()
        allowed_tools = self._infer_allowed_tools(capabilities, available_tools)
        allowed_mcp_servers = self._infer_allowed_mcp_servers(allowed_tools)
        workflow_definition = self._build_workflow_definition(
            name=artifact_name,
            description=normalized_description,
            capabilities=capabilities,
            allowed_tools=allowed_tools,
        )
        runtime_binding: dict[str, Any] | None = None
        if workflow_definition is not None:
            workflow = await self._skills_manager.create_workflow(workflow_definition)
            workflow.status = WorkflowStatus.ACTIVE
            runtime_binding = {
                "kind": "workflow",
                "workflow_id": workflow.id,
                "workflow_name": workflow.name,
            }

        now_iso = datetime.now(UTC).isoformat()
        record = GeneratedArtifactRecord(
            artifact_id=f"gart_{uuid.uuid4().hex}",
            artifact_type=artifact_type,
            name=artifact_name,
            description=normalized_description,
            status=ArtifactStatus.RUNTIME_ACTIVE,
            capabilities=capabilities,
            scopes=scopes,
            allowed_tools=allowed_tools,
            allowed_mcp_servers=allowed_mcp_servers,
            dependencies=[],
            guardrails=self._build_guardrails(scopes),
            tests=self._build_tests(capabilities, examples or []),
            workflow_definition=workflow_definition,
            runtime_binding=runtime_binding,
            metadata={
                "requested_by": requested_by,
                "examples": examples or [],
                **(metadata or {}),
            },
            created_at=now_iso,
            updated_at=now_iso,
        )
        await self._store.upsert_generated_artifact(record.to_store_dict())
        return record

    async def get_artifact(self, artifact_id: str) -> GeneratedArtifactRecord | None:
        """生成物を取得する."""
        raw = await self._store.get_generated_artifact(artifact_id)
        if raw is None:
            return None
        return GeneratedArtifactRecord.model_validate(raw)

    async def list_artifacts(self, status: str | None = None) -> list[GeneratedArtifactRecord]:
        """生成物一覧を返す."""
        rows = await self._store.list_generated_artifacts(status=status)
        return [GeneratedArtifactRecord.model_validate(row) for row in rows]

    async def validate_artifact(self, artifact_id: str) -> GeneratedArtifactRecord | None:
        """生成物を検証する."""
        record = await self.get_artifact(artifact_id)
        if record is None:
            return None

        validation_errors: list[str] = []
        if not record.capabilities:
            validation_errors.append("capability_missing")
        if not record.scopes:
            validation_errors.append("scope_missing")
        if record.workflow_definition is not None and record.runtime_binding is not None:
            workflow_id = str(record.runtime_binding.get("workflow_id", "")).strip()
            workflow = self._skills_manager.get_workflow(workflow_id)
            if workflow is None:
                validation_errors.append("runtime_workflow_missing")
            else:
                validation_errors.extend(await self._skills_manager.validate_workflow(workflow))

        record.metadata["validation_errors"] = validation_errors
        record.metadata["validated_at"] = datetime.now(UTC).isoformat()
        if not validation_errors:
            record.status = ArtifactStatus.VALIDATED
        record.updated_at = datetime.now(UTC).isoformat()
        await self._store.upsert_generated_artifact(record.to_store_dict())
        return record

    async def approve_artifact(
        self,
        artifact_id: str,
        *,
        approver_id: str,
    ) -> GeneratedArtifactRecord | None:
        """生成物を承認する."""
        record = await self.validate_artifact(artifact_id)
        if record is None:
            return None
        validation_errors = record.metadata.get("validation_errors", [])
        if isinstance(validation_errors, list) and validation_errors:
            return record
        record.status = ArtifactStatus.APPROVED
        record.metadata["approved_by"] = approver_id
        record.metadata["approved_at"] = datetime.now(UTC).isoformat()
        record.updated_at = datetime.now(UTC).isoformat()
        await self._store.upsert_generated_artifact(record.to_store_dict())
        return record

    async def reject_artifact(
        self,
        artifact_id: str,
        *,
        rejected_by: str,
        reason: str,
    ) -> GeneratedArtifactRecord | None:
        """生成物を却下する."""
        record = await self.get_artifact(artifact_id)
        if record is None:
            return None
        record.status = ArtifactStatus.REJECTED
        record.metadata["rejected_by"] = rejected_by
        record.metadata["rejected_at"] = datetime.now(UTC).isoformat()
        record.metadata["rejection_reason"] = reason
        record.updated_at = datetime.now(UTC).isoformat()
        await self._store.upsert_generated_artifact(record.to_store_dict())
        return record

    async def promote_artifact(self, artifact_id: str) -> GeneratedArtifactRecord | None:
        """承認済み生成物を published に昇格する."""
        record = await self.get_artifact(artifact_id)
        if record is None:
            return None
        if record.status != ArtifactStatus.APPROVED:
            return record
        record.status = ArtifactStatus.PUBLISHED
        record.metadata["published_at"] = datetime.now(UTC).isoformat()
        record.updated_at = datetime.now(UTC).isoformat()
        await self._store.upsert_generated_artifact(record.to_store_dict())
        return record

    async def bind_runtime_agent(
        self,
        artifact_id: str,
        *,
        agent_name: str,
    ) -> GeneratedArtifactRecord | None:
        """artifact と runtime agent 名を紐付ける."""
        record = await self.get_artifact(artifact_id)
        if record is None:
            return None
        binding = dict(record.runtime_binding or {})
        binding["agent_name"] = agent_name
        if "kind" not in binding:
            binding["kind"] = "workflow"
        record.runtime_binding = binding
        record.updated_at = datetime.now(UTC).isoformat()
        await self._store.upsert_generated_artifact(record.to_store_dict())
        return record

    async def restore_runtime_bindings(self) -> list[GeneratedArtifactRecord]:
        """永続化済み workflow binding を復元する."""
        restorable_statuses = {
            ArtifactStatus.RUNTIME_ACTIVE,
            ArtifactStatus.VALIDATED,
            ArtifactStatus.APPROVED,
            ArtifactStatus.PUBLISHED,
        }
        restored: list[GeneratedArtifactRecord] = []
        for artifact in await self.list_artifacts():
            if artifact.status not in restorable_statuses:
                continue
            if artifact.workflow_definition is None:
                restored.append(artifact)
                continue
            binding = dict(artifact.runtime_binding or {})
            workflow_id = str(binding.get("workflow_id", "")).strip()
            if workflow_id and self._skills_manager.get_workflow(workflow_id) is not None:
                restored.append(artifact)
                continue
            definition = dict(artifact.workflow_definition)
            if workflow_id:
                definition["id"] = workflow_id
            definition["status"] = WorkflowStatus.ACTIVE.value
            workflow = await self._skills_manager.create_workflow(definition)
            binding.update(
                {
                    "kind": "workflow",
                    "workflow_id": workflow.id,
                    "workflow_name": workflow.name,
                }
            )
            artifact.runtime_binding = binding
            artifact.updated_at = datetime.now(UTC).isoformat()
            await self._store.upsert_generated_artifact(artifact.to_store_dict())
            restored.append(artifact)
        return restored

    async def _list_available_tool_names(self) -> list[str]:
        """利用可能な tool 名一覧を返す."""
        skills = await self._skills_manager.list_available_skills()
        return [skill.name for skill in skills]

    @staticmethod
    def _make_artifact_name(description: str, artifact_type: ArtifactType) -> str:
        """説明から生成物名を作成する."""
        normalized = re.sub(r"[^a-z0-9]+", "_", description.lower()).strip("_")
        if not normalized:
            normalized = hashlib.sha1(description.encode("utf-8")).hexdigest()[:8]
        prefix = "generated_agent" if artifact_type == ArtifactType.AGENT else "generated_skill"
        return f"{prefix}_{normalized[:48]}"

    def _infer_capabilities(self, description: str) -> list[str]:
        """説明から capability を推定する."""
        lowered = description.lower()
        capabilities: list[str] = []
        for keyword, capability in self._CAPABILITY_HINTS:
            if keyword in lowered and capability not in capabilities:
                capabilities.append(capability)
        if not capabilities:
            capabilities.append("runtime_gap_fill")
        return capabilities

    def _infer_scopes(self, description: str) -> list[str]:
        """説明から scope を推定する."""
        lowered = description.lower()
        scopes: list[str] = []
        for keyword, scope in self._SCOPE_HINTS:
            if keyword in lowered and scope not in scopes:
                scopes.append(scope)
        if "net.http" not in scopes:
            scopes.append("net.http")
        return scopes

    def _infer_allowed_tools(
        self,
        capabilities: list[str],
        available_tools: list[str],
    ) -> list[str]:
        """capability から許可 tool を推定する."""
        allowed: list[str] = []
        for capability, preferred_tools in self._WORKFLOW_KEYWORDS:
            if capability not in capabilities:
                continue
            for tool_name in preferred_tools:
                if tool_name in available_tools and tool_name not in allowed:
                    allowed.append(tool_name)
        if not allowed and "web_search" in available_tools:
            allowed.append("web_search")
        if (
            any(
                capability in {"flight_watch", "travel_search", "research", "monitoring"} for capability in capabilities
            )
            and "http_request" in available_tools
            and "http_request" not in allowed
        ):
            allowed.append("http_request")
        return allowed

    @staticmethod
    def _infer_allowed_mcp_servers(allowed_tools: list[str]) -> list[str]:
        """許可 tool から MCP server ヒントを推定する."""
        servers: list[str] = []
        if any(tool_name.startswith("browser_") for tool_name in allowed_tools):
            servers.append("browser")
        if "web_search" in allowed_tools or "http_request" in allowed_tools:
            servers.append("serpapi")
        return servers

    def _build_workflow_definition(
        self,
        *,
        name: str,
        description: str,
        capabilities: list[str],
        allowed_tools: list[str],
    ) -> dict[str, Any] | None:
        """workflow 定義を作成する."""
        if not allowed_tools:
            return None
        steps: list[dict[str, Any]] = []
        for index, tool_name in enumerate(allowed_tools, start=1):
            step_id = f"step_{index}"
            params: dict[str, Any]
            if tool_name == "web_search":
                params = {"query": description}
            elif tool_name == "http_request":
                params = {"method": "GET", "url": "${params.seed_url}"}
            elif tool_name == "browser_navigate":
                params = {"url": "${params.seed_url}"}
            elif tool_name == "browser_get_text":
                params = {"selector": "body"}
            elif tool_name == "read_file":
                params = {"path": "${params.path}"}
            else:
                params = {}
            steps.append(
                {
                    "id": step_id,
                    "skill_name": tool_name,
                    "params": params,
                    "on_success": f"step_{index + 1}" if index < len(allowed_tools) else None,
                    "on_failure": None,
                    "timeout_seconds": 60,
                }
            )
        return {
            "name": name,
            "description": description,
            "entry_step_id": "step_1",
            "status": WorkflowStatus.ACTIVE.value,
            "steps": steps,
            "metadata": {"capabilities": capabilities},
        }

    @staticmethod
    def _build_guardrails(scopes: list[str]) -> list[str]:
        """guardrail 一覧を構築する."""
        guardrails = [
            "実行は runtime artifact 単位で監査する",
            "高リスク操作は承認なしで publish しない",
        ]
        if "fs.write" in scopes:
            guardrails.append("ファイル書き込みは workspace 配下に限定する")
        if "net.http" in scopes:
            guardrails.append("外部アクセスは allowlist と provider abstraction を経由する")
        return guardrails

    @staticmethod
    def _build_tests(capabilities: list[str], examples: list[str]) -> list[dict[str, Any]]:
        """生成物の最低限のテスト仕様を作成する."""
        tests: list[dict[str, Any]] = [
            {
                "name": "metadata_contract",
                "description": "capability / scope / guardrail が設定されていること",
            },
            {
                "name": "approval_gate",
                "description": "approved 以前は published へ昇格しないこと",
            },
        ]
        for index, example in enumerate(examples[:2], start=1):
            tests.append(
                {
                    "name": f"example_{index}",
                    "description": f"入力例 {index}: {example}",
                }
            )
        if "flight_watch" in capabilities:
            tests.append(
                {
                    "name": "flight_search_roundtrip",
                    "description": "往復便の入力不足時に clarification を返すこと",
                }
            )
        return tests
