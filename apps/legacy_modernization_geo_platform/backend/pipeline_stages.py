"""パイプラインステージの Agent 非同期呼び出しヘルパー.

Agent process() を唯一の実行パスとし、フォールバックビルダーは廃止。
全呼び出しは async で行い、orchestrator の asyncio.gather 並列実行に対応する。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, TypeVar

from pydantic import BaseModel

from apps.legacy_modernization_geo_platform.agents._models import (
    ReportAssemblerOutput,
)

if TYPE_CHECKING:
    from kernel.agents.app_agent_runtime import AppAgentRuntime

logger = logging.getLogger(__name__)
ModelT = TypeVar("ModelT", bound=BaseModel)


# ---------------------------------------------------------------------------
# Agent 非同期呼び出し
# ---------------------------------------------------------------------------


async def invoke_agent(
    agent_runtime: AppAgentRuntime | None,
    agent_name: str,
    input_data: dict[str, Any],
) -> dict[str, Any]:
    """A2AHub / AppAgentRuntime 経由で Agent を非同期呼び出しする.

    Agent が見つからない場合は RuntimeError を送出する。
    """
    from kernel.protocols.a2a_hub import get_hub

    hub = get_hub()
    if hub.discover(agent_name) is not None:
        result = await hub.call(agent_name, input_data)
        if isinstance(result, dict):
            return result
    if agent_runtime is not None and agent_runtime.get_agent(agent_name) is not None:
        result = await agent_runtime.invoke(agent_name, input_data)
        if isinstance(result, dict):
            return result
    msg = f"Agent '{agent_name}' が Hub にも Runtime にも見つかりません"
    raise RuntimeError(msg)


async def execute_artifact_agent(
    agent_runtime: AppAgentRuntime | None,
    agent_name: str,
    input_data: dict[str, Any],
    artifact_model: type[ModelT],
) -> ModelT:
    """Agent を非同期呼び出しし、Artifact モデルにバリデートして返す."""
    result = await invoke_agent(agent_runtime, agent_name, input_data)
    artifact_payload = result.get("artifact")
    if not isinstance(artifact_payload, dict):
        msg = f"Agent '{agent_name}' が有効な artifact dict を返しませんでした"
        raise ValueError(msg)
    return artifact_model.model_validate(artifact_payload)


async def execute_report_agent(
    agent_runtime: AppAgentRuntime | None,
    input_data: dict[str, Any],
) -> ReportAssemblerOutput:
    """ReportAssembler Agent を非同期呼び出しする."""
    result = await invoke_agent(agent_runtime, "ReportAssembler", input_data)
    return ReportAssemblerOutput.model_validate(result)


# ---------------------------------------------------------------------------
# 成果物サマリ
# ---------------------------------------------------------------------------


def summarize_artifact(artifact_name: str, artifact: BaseModel) -> str:
    """成果物名に応じた簡潔なサマリ文字列を構築する."""
    if artifact_name == "account_signal_artifact":
        return f"Demand signals normalized: {len(getattr(artifact, 'signals', []))}"
    if artifact_name == "question_graph_artifact":
        return f"Question graph created for {len(getattr(artifact, 'personas', []))} persona(s)"
    if artifact_name == "content_draft_artifact":
        return f"Drafted {len(getattr(artifact, 'pages', []))} publishable page(s)"
    if artifact_name == "publish_manifest":
        return f"Published {len(getattr(artifact, 'pages', []))} page(s) and discovery files"
    return artifact_name.replace("_", " ")
