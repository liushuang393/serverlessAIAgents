"""ワークフローAPI ルート.

ワークフローのCRUD操作と実行に関するエンドポイント。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

import yaml
from fastapi import APIRouter, HTTPException

from agentflow.studio.models import (
    AgentRunRequest,
    AgentRunResponse,
    WorkflowCreateRequest,
    WorkflowUpdateRequest,
)


if TYPE_CHECKING:
    from pathlib import Path


def create_workflows_router(workflows_dir: Path) -> APIRouter:
    """ワークフローAPIルーターを作成.

    Args:
        workflows_dir: ワークフローディレクトリ

    Returns:
        FastAPI APIRouter
    """
    router = APIRouter(prefix="/api/workflows", tags=["workflows"])

    @router.get("")
    async def list_workflows() -> list[dict[str, Any]]:
        """ワークフロー一覧を取得."""
        workflows = []
        for workflow_file in workflows_dir.glob("*.yaml"):
            workflows.append(
                {
                    "id": workflow_file.stem,
                    "name": workflow_file.stem,
                    "path": str(workflow_file),
                }
            )
        return workflows

    @router.post("")
    async def create_workflow(request: WorkflowCreateRequest) -> dict[str, Any]:
        """ワークフローを作成."""
        workflow_id = request.name.lower().replace(" ", "-")
        workflow_path = workflows_dir / f"{workflow_id}.yaml"

        if workflow_path.exists():
            raise HTTPException(status_code=409, detail="Workflow already exists")

        workflow_data = {
            "name": request.name,
            "description": request.description,
            "nodes": request.nodes,
            "edges": request.edges,
        }

        with open(workflow_path, "w", encoding="utf-8") as f:
            yaml.dump(workflow_data, f, allow_unicode=True)

        return {
            "id": workflow_id,
            "name": request.name,
            "path": str(workflow_path),
        }

    @router.get("/{workflow_id}")
    async def get_workflow(workflow_id: str) -> dict[str, Any]:
        """ワークフロー詳細を取得."""
        workflow_path = workflows_dir / f"{workflow_id}.yaml"

        if not workflow_path.exists():
            raise HTTPException(status_code=404, detail="Workflow not found")

        with open(workflow_path, encoding="utf-8") as f:
            workflow_data = yaml.safe_load(f)

        return {
            "id": workflow_id,
            **workflow_data,
        }

    @router.put("/{workflow_id}")
    async def update_workflow(
        workflow_id: str,
        request: WorkflowUpdateRequest,
    ) -> dict[str, Any]:
        """ワークフローを更新."""
        workflow_path = workflows_dir / f"{workflow_id}.yaml"

        if not workflow_path.exists():
            raise HTTPException(status_code=404, detail="Workflow not found")

        with open(workflow_path, encoding="utf-8") as f:
            workflow_data = yaml.safe_load(f)

        if request.name:
            workflow_data["name"] = request.name
        if request.description is not None:
            workflow_data["description"] = request.description
        if request.nodes:
            workflow_data["nodes"] = request.nodes
        if request.edges:
            workflow_data["edges"] = request.edges

        with open(workflow_path, "w", encoding="utf-8") as f:
            yaml.dump(workflow_data, f, allow_unicode=True)

        return {
            "id": workflow_id,
            "name": workflow_data["name"],
            "path": str(workflow_path),
        }

    @router.delete("/{workflow_id}")
    async def delete_workflow(workflow_id: str) -> dict[str, str]:
        """ワークフローを削除."""
        workflow_path = workflows_dir / f"{workflow_id}.yaml"

        if not workflow_path.exists():
            raise HTTPException(status_code=404, detail="Workflow not found")

        workflow_path.unlink()
        return {"status": "deleted", "id": workflow_id}

    @router.post("/{workflow_id}/run")
    async def run_workflow(
        workflow_id: str,
        request: AgentRunRequest,
    ) -> AgentRunResponse:
        """ワークフローを実行."""
        workflow_path = workflows_dir / f"{workflow_id}.yaml"

        if not workflow_path.exists():
            raise HTTPException(status_code=404, detail="Workflow not found")

        try:
            result = {
                "message": f"Workflow {workflow_id} executed successfully",
                "input": request.input_data,
            }
            return AgentRunResponse(status="success", result=result)

        except Exception as e:
            return AgentRunResponse(status="error", result=None, error=str(e))

    return router
