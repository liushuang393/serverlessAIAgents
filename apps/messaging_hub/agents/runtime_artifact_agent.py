"""Runtime artifact backed agent."""

from __future__ import annotations

import logging
from typing import Any

from pydantic import BaseModel, Field

from apps.messaging_hub.generated_artifact_manager import (
    ArtifactStatus,
    ArtifactType,
    GeneratedArtifactManager,
    GeneratedArtifactRecord,
)
from apps.messaging_hub.harness_memory import HarnessMemoryService
from apps.messaging_hub.skills_manager import SkillsManager
from kernel.agents.contracts import descriptor_from_agent_instance
from kernel.agents.resilient_agent import ResilientAgent
from kernel.protocols.a2a_hub import get_hub


_LOGGER = logging.getLogger(__name__)


class RuntimeArtifactAgentInput(BaseModel):
    """Runtime artifact agent input."""

    task_description: str = Field(default="")
    input_data: dict[str, Any] = Field(default_factory=dict)
    user_id: str = Field(default="system")
    conversation_id: str | None = Field(default=None)
    task_id: str | None = Field(default=None)


class RuntimeArtifactAgentOutput(BaseModel):
    """Runtime artifact agent output."""

    status: str = Field(default="completed")
    runtime_artifact_id: str = Field(default="")
    workflow_result: dict[str, Any] | None = Field(default=None)
    allowed_tools: list[str] = Field(default_factory=list)
    error: str | None = Field(default=None)


class RuntimeArtifactAgent(ResilientAgent[RuntimeArtifactAgentInput, RuntimeArtifactAgentOutput]):
    """Generated artifact を runtime agent として実行する."""

    def __init__(
        self,
        *,
        artifact: GeneratedArtifactRecord,
        skills_manager: SkillsManager,
        memory_service: HarnessMemoryService | None = None,
    ) -> None:
        """初期化."""
        super().__init__()
        self._artifact = artifact
        self._skills_manager = skills_manager
        self._memory_service = memory_service
        self.name = self._agent_name_for_artifact(artifact)
        self._agent_type = artifact.capabilities[0] if artifact.capabilities else "runtime_gap_fill"

    @staticmethod
    def _agent_name_for_artifact(artifact: GeneratedArtifactRecord) -> str:
        """artifact から agent 名を解決する."""
        runtime_name = ""
        if isinstance(artifact.runtime_binding, dict):
            runtime_name = str(artifact.runtime_binding.get("agent_name", "")).strip()
        if runtime_name:
            return runtime_name
        return f"RuntimeArtifactAgent__{artifact.name}"

    def _parse_input(self, input_data: dict[str, Any]) -> RuntimeArtifactAgentInput:
        """入力をモデル化する."""
        return RuntimeArtifactAgentInput.model_validate(input_data)

    async def process(self, input_data: RuntimeArtifactAgentInput) -> RuntimeArtifactAgentOutput:
        """workflow ベースの runtime artifact を実行する."""
        if self._artifact.status == ArtifactStatus.REJECTED:
            return RuntimeArtifactAgentOutput(
                status="failed",
                runtime_artifact_id=self._artifact.artifact_id,
                error="artifact_rejected",
            )
        workflow_id = self._workflow_id()
        if workflow_id is None:
            return RuntimeArtifactAgentOutput(
                status="failed",
                runtime_artifact_id=self._artifact.artifact_id,
                error="runtime_workflow_missing",
                allowed_tools=list(self._artifact.allowed_tools),
            )

        workflow_result = await self._skills_manager.run_workflow(
            workflow_id,
            params={
                "task_description": input_data.task_description,
                **input_data.input_data,
            },
        )
        payload = workflow_result.to_dict()
        if self._memory_service is not None and input_data.task_id:
            await self._memory_service.remember_private(
                task_id=input_data.task_id,
                user_id=input_data.user_id,
                conversation_id=input_data.conversation_id,
                agent_name=self.name,
                key="specialist_scratchpad",
                content=payload,
                description="runtime artifact agent workflow output",
            )
        return RuntimeArtifactAgentOutput(
            status="completed" if workflow_result.error is None else "failed",
            runtime_artifact_id=self._artifact.artifact_id,
            workflow_result=payload,
            allowed_tools=list(self._artifact.allowed_tools),
            error=workflow_result.error,
        )

    def _workflow_id(self) -> str | None:
        """workflow id を返す."""
        if not isinstance(self._artifact.runtime_binding, dict):
            return None
        workflow_id = str(self._artifact.runtime_binding.get("workflow_id", "")).strip()
        return workflow_id or None


class RuntimeArtifactAgentRegistry:
    """Runtime artifact を A2A 呼び出し可能な agent に昇格する."""

    def __init__(
        self,
        *,
        skills_manager: SkillsManager,
        memory_service: HarnessMemoryService | None = None,
    ) -> None:
        """初期化."""
        self._skills_manager = skills_manager
        self._memory_service = memory_service
        self._hub = get_hub()

    async def materialize(self, artifact: GeneratedArtifactRecord) -> str | None:
        """artifact を runtime agent として登録する."""
        if artifact.artifact_type != ArtifactType.AGENT:
            return None
        agent = RuntimeArtifactAgent(
            artifact=artifact,
            skills_manager=self._skills_manager,
            memory_service=self._memory_service,
        )
        descriptor = descriptor_from_agent_instance(
            agent,
            agent_id=agent.name,
            description=artifact.description,
            capabilities=list(artifact.capabilities),
            protocol_exposure=["a2a"],
            metadata={
                "runtime_artifact_id": artifact.artifact_id,
                "allowed_tools": list(artifact.allowed_tools),
                "allowed_mcp_servers": list(artifact.allowed_mcp_servers),
            },
        )
        self._hub.bus.register(
            agent,
            descriptor=descriptor,
            replace=True,
        )
        _LOGGER.info("Runtime artifact agent registered: %s", agent.name)
        return agent.name

    async def restore(self, manager: GeneratedArtifactManager) -> list[str]:
        """永続化済み runtime agent を復元する."""
        restored_names: list[str] = []
        artifacts = await manager.list_artifacts()
        for artifact in artifacts:
            if artifact.artifact_type != ArtifactType.AGENT:
                continue
            if artifact.status == ArtifactStatus.REJECTED:
                continue
            agent_name = await self.materialize(artifact)
            if agent_name:
                restored_names.append(agent_name)
        return restored_names
