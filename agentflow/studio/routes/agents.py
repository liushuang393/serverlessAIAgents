"""エージェントAPI ルート.

エージェントの一覧取得、詳細取得、実行に関するエンドポイント。
"""

from __future__ import annotations

import asyncio
import json
from typing import TYPE_CHECKING, Any

from fastapi import APIRouter, HTTPException, WebSocket, WebSocketDisconnect
from fastapi.responses import StreamingResponse

from agentflow.core.engine import AgentFlowEngine
from agentflow.studio.models import AgentRunRequest, AgentRunResponse


if TYPE_CHECKING:
    from pathlib import Path

    from agentflow.marketplace.registry import LocalRegistry


def create_agents_router(
    agents_dir: Path,
    registry: LocalRegistry,
    active_connections: dict[str, WebSocket],
) -> APIRouter:
    """エージェントAPIルーターを作成.

    Args:
        agents_dir: エージェントディレクトリ
        registry: ローカルレジストリ
        active_connections: WebSocket接続管理

    Returns:
        FastAPI APIRouter
    """
    router = APIRouter(prefix="/api/agents", tags=["agents"])

    @router.get("")
    async def list_agents() -> list[dict[str, Any]]:
        """インストール済みエージェント一覧を取得."""
        result = []

        # レジストリから取得
        installed = registry.list_agents()
        for agent in installed:
            result.append(
                {
                    "id": agent.id,
                    "name": agent.name,
                    "version": agent.version,
                    "description": agent.description,
                    "category": agent.category,
                    "installed_at": agent.installed_at,
                    "type": "yaml",
                }
            )

        # @agent デコレータで定義されたAgent
        try:
            from agentflow.agent_decorator import AgentClient

            decorated_agents = AgentClient.list_agents()
            for agent_name in decorated_agents:
                if not any(a["id"] == agent_name for a in result):
                    result.append(
                        {
                            "id": agent_name,
                            "name": agent_name,
                            "version": "0.2.0",
                            "description": f"@agent decorator agent: {agent_name}",
                            "category": "decorator",
                            "installed_at": None,
                            "type": "decorator",
                        }
                    )
        except Exception:
            pass

        return result

    @router.get("/{agent_id}")
    async def get_agent(agent_id: str) -> dict[str, Any]:
        """エージェント詳細を取得."""
        agent_info = registry.get_agent(agent_id)
        if not agent_info:
            raise HTTPException(status_code=404, detail="Agent not found")

        return {
            "id": agent_info.id,
            "name": agent_info.name,
            "version": agent_info.version,
            "description": agent_info.description,
            "category": agent_info.category,
            "protocols": agent_info.protocols,
            "inputs": agent_info.inputs,
            "outputs": agent_info.outputs,
        }

    @router.post("/{agent_id}/run")
    async def run_agent(agent_id: str, request: AgentRunRequest) -> AgentRunResponse:
        """エージェントを実行."""
        agent_info = registry.get_agent(agent_id)
        if not agent_info:
            raise HTTPException(status_code=404, detail="Agent not found")

        try:
            engine = AgentFlowEngine()
            result = await engine.run(agent_id, request.input_data)
            return AgentRunResponse(status="success", result=result)

        except Exception as e:
            return AgentRunResponse(status="error", result=None, error=str(e))

    @router.get("/{agent_id}/run/stream")
    async def run_agent_stream(agent_id: str) -> StreamingResponse:
        """エージェントをストリーム実行（SSE）."""

        async def event_generator():
            try:
                engine = AgentFlowEngine()

                async for event in engine.stream_events(agent_id, {}):
                    event_data = {"type": event.type, "data": event.data}
                    yield f"data: {json.dumps(event_data, ensure_ascii=False)}\n\n"

                    if event.type in ("complete", "error"):
                        return

                # フォールバック
                for i in range(5):
                    yield f"data: {{'type': 'log', 'message': 'Step {i + 1}'}}\n\n"
                    await asyncio.sleep(1)
                yield "data: {'type': 'complete', 'message': 'Done'}\n\n"

            except Exception as e:
                error_data = {"type": "error", "message": str(e)}
                yield f"data: {json.dumps(error_data, ensure_ascii=False)}\n\n"

        return StreamingResponse(event_generator(), media_type="text/event-stream")

    @router.websocket("/{agent_id}/ws")
    async def agent_websocket(websocket: WebSocket, agent_id: str):
        """エージェントWebSocket接続."""
        await websocket.accept()
        connection_id = f"{agent_id}_{id(websocket)}"
        active_connections[connection_id] = websocket

        try:
            while True:
                data = await websocket.receive_json()
                response = {"status": "received", "data": data}
                await websocket.send_json(response)
        except WebSocketDisconnect:
            del active_connections[connection_id]

    return router
