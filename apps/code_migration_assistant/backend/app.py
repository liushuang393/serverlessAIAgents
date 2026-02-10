
import asyncio
import json
import logging
import time
import uuid
from typing import Any, Dict

from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException, BackgroundTasks
from fastapi.responses import FileResponse
from fastapi.staticfiles import StaticFiles
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

from agentflow.protocols.agui_events import (
    AGUIEvent,
    FlowStartEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    NodeStartEvent,
    NodeCompleteEvent,
    LogEvent,
    ApprovalRequiredEvent
)
from apps.code_migration_assistant.engine import CodeMigrationEngine

# Logging setup
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("migration_server")

app = FastAPI(title="Code Migration Assistant API")

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Global State
active_tasks: Dict[str, CodeMigrationEngine] = {}
task_websockets: Dict[str, WebSocket] = {}

class MigrationRequest(BaseModel):
    source_code: str
    migration_type: str = "cobol-to-java"

class ApprovalRequest(BaseModel):
    approved: bool
    comment: str | None = None

async def run_migration_task(task_id: str, engine: CodeMigrationEngine, inputs: dict):
    """Background task to run the engine and stream events via WebSocket."""
    logger.info(f"Starting migration task {task_id}")
    try:
        # Flow Start Event
        if task_id in task_websockets:
            flow_start = FlowStartEvent(
                timestamp=time.time(),
                flow_id=task_id,
                data={"inputs": inputs}
            )
            await task_websockets[task_id].send_text(flow_start.to_sse())

        async for event in engine._execute_stream(inputs):
            # Send event to websocket if connected
            if task_id in task_websockets:
                ws = task_websockets[task_id]
                try:
                    # Check if event is already a dict compatible with AGUIEvent
                    # or if it's an engine internal event that needs conversion
                    
                    agui_event = None
                    event_type = event.get("event") or event.get("event_type")
                    
                    if event_type == "node_start":
                        agui_event = NodeStartEvent(
                            timestamp=time.time(),
                            flow_id=task_id,
                            node_id=event.get("node", "unknown"),
                            node_name=event.get("node", "unknown")
                        )
                    elif event_type == "node_complete":
                        agui_event = NodeCompleteEvent(
                            timestamp=time.time(),
                            flow_id=task_id,
                            node_id=event.get("node", "unknown"),
                            node_name=event.get("node", "unknown"),
                            data=event.get("result", {})
                        )
                    elif event_type == "log":
                        agui_event = LogEvent(
                            timestamp=time.time(),
                            flow_id=task_id,
                            level=event.get("level", "INFO"),
                            message=event.get("message", "")
                        )
                    elif event_type == "approval_required":
                         pass
                    
                    if agui_event:
                        await ws.send_text(agui_event.to_sse())
                    else:
                        pass

                except Exception as e:
                    logger.error(f"WebSocket send error: {e}")
        
        # Flow Complete Event
        if task_id in task_websockets:
            flow_complete = FlowCompleteEvent(
                timestamp=time.time(),
                flow_id=task_id,
                result={
                    "report_available": True,
                    "report_url": f"/api/artifacts/{task_id}/report/compliance_report.md"
                }
            )
            await task_websockets[task_id].send_text(flow_complete.to_sse())

    except Exception as e:
        logger.error(f"Migration task failed: {e}", exc_info=True)
        if task_id in task_websockets:
            flow_error = FlowErrorEvent(
                timestamp=time.time(),
                flow_id=task_id,
                error_message=str(e),
                error_type=type(e).__name__
            )
            await task_websockets[task_id].send_text(flow_error.to_sse())

    finally:
        # Cleanup
        if task_id in active_tasks:
            # Don't remove immediately so user can see result? 
            # For this demo, keep it.
            pass

@app.post("/api/migration/start")
async def start_migration(request: MigrationRequest, background_tasks: BackgroundTasks):
    task_id = str(uuid.uuid4())
    
    # Initialize Engine
    engine = CodeMigrationEngine(migration_type=request.migration_type)
    
    # Inject an event emitter that writes to WebSocket for ApprovalFlow?
    # ApprovalFlow supports event_emitter callback.
    async def ws_emitter(event_dict: dict):
        if task_id in task_websockets:
            # Wrap in SSE format manually since it's already a dict
            payload = f"data: {json.dumps(event_dict)}\n\n"
            await task_websockets[task_id].send_text(payload)
            
    engine._approval_flow._event_emitter = ws_emitter

    await engine._initialize()
    
    active_tasks[task_id] = engine
    
    inputs = {
        "source_code": request.source_code,
        "task_id": task_id,
        "artifacts_dir": f"/tmp/migration_artifacts/{task_id}" # Temp dir for demo
    }

    # Start loop in background
    background_tasks.add_task(run_migration_task, task_id, engine, inputs)
    
    return {"task_id": task_id, "status": "started"}

@app.websocket("/api/ws/{task_id}")
async def websocket_endpoint(websocket: WebSocket, task_id: str):
    await websocket.accept()
    task_websockets[task_id] = websocket
    try:
        while True:
            # Keep alive / listen for client messages (optional)
            data = await websocket.receive_text()
            # Echo or handle commands
    except WebSocketDisconnect:
        logger.info(f"Client disconnected for task {task_id}")
        if task_id in task_websockets:
            del task_websockets[task_id]

@app.get("/api/approvals/{task_id}")
async def get_approvals(task_id: str):
    if task_id not in active_tasks:
        raise HTTPException(status_code=404, detail="Task not found")
    
    engine = active_tasks[task_id]
    # Check pending approvals
    pending = engine._approval_flow.get_pending_requests()
    return [
        {
            "id": r.id,
            "action": r.action,
            "reason": r.reason,
            "context": r.context
        }
        for r in pending
    ]

@app.post("/api/approvals/{task_id}/{request_id}")
async def submit_approval(task_id: str, request_id: str, approval: ApprovalRequest):
    if task_id not in active_tasks:
        raise HTTPException(status_code=404, detail="Task not found")
        
    engine = active_tasks[task_id]
    success = await engine._approval_flow.submit_response(
        request_id=request_id,
        approved=approval.approved,
        comment=approval.comment,
        approver="admin"
    )
    
    if not success:
        raise HTTPException(status_code=400, detail="Failed to submit approval (invalid ID or timeout)")
        
    return {"status": "submitted"}

@app.get("/api/artifacts/{task_id}/{stage}/{filename}")
async def get_artifact(task_id: str, stage: str, filename: str):
    """成果物を取得."""
    # Note: In production, validate task_id and use a secure storage
    path = Path(f"/tmp/migration_artifacts/{task_id}/{stage}/{filename}")
    if not path.exists():
        raise HTTPException(status_code=404, detail="Artifact not found")
    return FileResponse(path)

# Serve UI
app.mount("/", StaticFiles(directory="apps/code_migration_assistant/frontend", html=True), name="ui")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8003)
