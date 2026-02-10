
import asyncio
import json
import logging
import uuid
from typing import Any, Dict

from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException, BackgroundTasks
from fastapi.staticfiles import StaticFiles
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

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
        async for event in engine._execute_stream(inputs):
            # Send event to websocket if connected
            if task_id in task_websockets:
                ws = task_websockets[task_id]
                try:
                    # Convert event to JSON-serializable dict
                    # Handling Pydantic models if present
                    payload = json.dumps(event, default=str)
                    await ws.send_text(payload)
                except Exception as e:
                    logger.error(f"WebSocket send error: {e}")
                    
            # 承認要求イベントの場合は特別なログを出すなど
            if event.get("event") == "node_complete":
                pass

    except Exception as e:
        logger.error(f"Migration task failed: {e}", exc_info=True)
        if task_id in task_websockets:
            await task_websockets[task_id].send_json({
                "error": str(e),
                "status": "failed"
            })
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
    await engine._initialize()  # Must init
    
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
    if task_id not in active_tasks:
        await websocket.close(code=4004, reason="Task not found")
        return
        
    task_websockets[task_id] = websocket
    try:
        while True:
            # Keep alive / listen for client messages (optional)
            data = await websocket.receive_text()
            # Echo or handle commands
    except WebSocketDisconnect:
        logger.info(f"Client disconnected for task {task_id}")
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

# Serve UI
app.mount("/", StaticFiles(directory="apps/code_migration_assistant/ui", html=True), name="ui")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
