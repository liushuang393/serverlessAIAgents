# -*- coding: utf-8 -*-
"""決策処理APIルーター.

PipelineEngine パターンを使用した決策処理 API。

エンドポイント:
    - POST /api/decision: 同期処理
    - GET /api/decision/stream: SSEストリーム付き処理
    - WebSocket /ws/decision: リアルタイム通知
"""

import logging
from typing import Any

from fastapi import APIRouter, Query, WebSocket, WebSocketDisconnect
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field

from apps.decision_governance_engine.engine import DecisionEngine
from apps.decision_governance_engine.schemas.input_schemas import (
    BudgetConstraint,
    ConstraintSet,
    TimelineConstraint,
)
from apps.decision_governance_engine.schemas.output_schemas import DecisionReport

logger = logging.getLogger("decision_api.decision")

router = APIRouter(tags=["決策処理"])


# ========================================
# スキーマ定義
# ========================================

class DecisionAPIRequest(BaseModel):
    """API リクエストスキーマ."""
    question: str = Field(..., min_length=10, max_length=2000, description="意思決定の質問")
    budget: float | None = Field(None, ge=0, description="予算制約（万円）")
    timeline_months: int | None = Field(None, ge=1, le=120, description="期間制約（月）")
    technical_constraints: list[str] = Field(default_factory=list, description="技術制約")
    regulatory_constraints: list[str] = Field(default_factory=list, description="法規制約")
    human_resources: list[str] = Field(default_factory=list, description="人的リソース")


class DecisionAPIResponse(BaseModel):
    """API レスポンススキーマ."""
    status: str = Field(..., description="処理状態")
    report_id: str | None = Field(None, description="レポートID")
    data: dict[str, Any] = Field(default_factory=dict, description="レポートデータ")


class RejectionResponse(BaseModel):
    """拒否レスポンススキーマ."""
    status: str = "rejected"
    reason: str | None = None
    message: str | None = None
    suggested_rephrase: str | None = None


# ========================================
# エンジン管理
# ========================================

_engine: DecisionEngine | None = None


def get_engine() -> DecisionEngine:
    """DecisionEngineシングルトンを取得."""
    global _engine
    if _engine is None:
        _engine = DecisionEngine()
    return _engine


def build_constraints(req: DecisionAPIRequest) -> ConstraintSet:
    """APIリクエストからConstraintSetを構築."""
    constraints = ConstraintSet(
        technical=req.technical_constraints,
        regulatory=req.regulatory_constraints,
        human_resources=req.human_resources,
    )
    if req.budget is not None:
        constraints.budget = BudgetConstraint(amount=req.budget, currency="JPY")
    if req.timeline_months is not None:
        constraints.timeline = TimelineConstraint(months=req.timeline_months)
    return constraints


# ========================================
# エンドポイント
# ========================================

def build_input_dict(question: str, constraints: ConstraintSet) -> dict:
    """Agent用入力辞書を構築.

    Args:
        question: ユーザーの質問
        constraints: 制約セット

    Returns:
        CognitiveGateAgent が期待する形式の入力辞書
    """
    constraint_list: list[str] = []
    if constraints.budget:
        constraint_list.append(f"予算: {constraints.budget.amount}万円")
    if constraints.timeline:
        constraint_list.append(f"期間: {constraints.timeline.months}ヶ月")
    constraint_list.extend(constraints.technical)
    constraint_list.extend(constraints.regulatory)
    constraint_list.extend(constraints.human_resources)

    return {
        "raw_question": question,
        "question": question,  # 後方互換のために残す
        "constraints": constraint_list,
    }


@router.post("/api/decision", response_model=DecisionAPIResponse | RejectionResponse)
async def process_decision(req: DecisionAPIRequest) -> DecisionAPIResponse | RejectionResponse:
    """同期的に意思決定を処理.

    PipelineEngine.run() を使用。
    """
    engine = get_engine()
    constraints = build_constraints(req)
    inputs = build_input_dict(req.question, constraints)
    result = await engine.run(inputs)

    # 拒否の場合
    if isinstance(result, dict) and result.get("status") == "rejected":
        return RejectionResponse(**result)

    # 成功の場合
    if isinstance(result, DecisionReport):
        return DecisionAPIResponse(
            status="success",
            report_id=result.report_id,
            data=result.model_dump(),
        )

    report_id = result.get("report_id", "") if isinstance(result, dict) else ""
    return DecisionAPIResponse(status="success", report_id=report_id, data=result)


@router.get("/api/decision/stream")
async def process_decision_stream(
    question: str = Query(..., min_length=10, max_length=2000, description="意思決定の質問"),
    budget: float | None = Query(None, ge=0, description="予算制約（万円）"),
    timeline_months: int | None = Query(None, ge=1, le=120, description="期間制約（月）"),
) -> StreamingResponse:
    """SSEストリーム付きで意思決定を処理.

    PipelineEngine.run_stream() を使用。
    """
    import json

    logger.info(f"[SSE] /api/decision/stream リクエスト受信")
    logger.info(f"[SSE] question={question[:50]}...")

    engine = get_engine()
    constraints = ConstraintSet()
    if budget is not None:
        constraints.budget = BudgetConstraint(amount=budget, currency="JPY")
    if timeline_months is not None:
        constraints.timeline = TimelineConstraint(months=timeline_months)

    inputs = build_input_dict(question, constraints)

    def serialize_event(event: dict) -> str:
        """イベントをJSON文字列に変換（enum対応）."""
        # event_type が Enum の場合は .value を使用
        if "event_type" in event:
            et = event["event_type"]
            if hasattr(et, "value"):
                event = {**event, "event_type": et.value}
        return json.dumps(event, ensure_ascii=False, default=str)

    async def generate_events():
        """SSEイベントを生成."""
        import asyncio
        import time as time_module
        logger.info("[SSE] ストリーム開始")
        # 即座に接続確認イベントを送信
        yield f"data: {json.dumps({'event_type': 'connection.established', 'timestamp': time_module.time()}, ensure_ascii=False)}\n\n"
        try:
            async for event in engine.run_stream(inputs):
                # イベントタイプをログ出力（type または event_type）
                event_type = event.get('event_type') or event.get('type', 'unknown')
                logger.info(f"[SSE] イベント発行: {event_type}")
                if isinstance(event, dict):
                    yield f"data: {serialize_event(event)}\n\n"
                else:
                    yield f"data: {event}\n\n"
        except asyncio.CancelledError:
            # クライアント切断時
            logger.info("[SSE] クライアント切断を検出 - 処理を中止")
            raise
        except GeneratorExit:
            # ジェネレーター終了時
            logger.info("[SSE] ジェネレーター終了 - クライアント切断")
            raise
        except Exception as e:
            logger.exception(f"[SSE] エラー発生: {e}")
            error_event = {"event_type": "flow.error", "error_message": str(e)}
            yield f"data: {json.dumps(error_event, ensure_ascii=False)}\n\n"
        finally:
            logger.info("[SSE] ストリーム終了")

    return StreamingResponse(
        generate_events(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )


# ========================================
# WebSocket
# ========================================

class ConnectionManager:
    """WebSocket接続管理."""

    def __init__(self) -> None:
        self.active_connections: list[WebSocket] = []

    async def connect(self, websocket: WebSocket) -> None:
        await websocket.accept()
        self.active_connections.append(websocket)

    def disconnect(self, websocket: WebSocket) -> None:
        if websocket in self.active_connections:
            self.active_connections.remove(websocket)

    async def broadcast(self, message: dict) -> None:
        for connection in self.active_connections:
            try:
                await connection.send_json(message)
            except Exception:
                pass


ws_manager = ConnectionManager()


@router.websocket("/ws/decision")
async def websocket_decision(websocket: WebSocket) -> None:
    """WebSocketで意思決定プロセスを実行."""
    await ws_manager.connect(websocket)
    try:
        while True:
            data = await websocket.receive_json()
            question = data.get("question", "")
            budget = data.get("budget")
            timeline_months = data.get("timeline_months")

            if not question or len(question) < 10:
                await websocket.send_json({
                    "type": "error",
                    "message": "質問は10文字以上必要です",
                })
                continue

            constraints = ConstraintSet()
            if budget is not None:
                constraints.budget = BudgetConstraint(amount=budget, currency="JPY")
            if timeline_months is not None:
                constraints.timeline = TimelineConstraint(months=timeline_months)

            inputs = build_input_dict(question, constraints)
            engine = get_engine()

            async for event in engine.run_stream(inputs):
                event_data = {
                    "type": "agent_event",
                    "event_type": getattr(event, "event_type", {}).value if hasattr(getattr(event, "event_type", None), "value") else str(event.get("event_type", "")),
                    "timestamp": getattr(event, "timestamp", 0),
                    "flow_id": getattr(event, "flow_id", ""),
                    "data": getattr(event, "data", {}),
                }
                if hasattr(event, "node_id"):
                    event_data["agent_id"] = event.node_id
                if hasattr(event, "node_name"):
                    event_data["agent_name"] = event.node_name
                if hasattr(event, "percentage"):
                    event_data["progress"] = event.percentage
                if hasattr(event, "error_message"):
                    event_data["error"] = event.error_message

                await websocket.send_json(event_data)

            await websocket.send_json({
                "type": "complete",
                "message": "意思決定プロセスが完了しました",
            })

    except WebSocketDisconnect:
        ws_manager.disconnect(websocket)
    except Exception as e:
        await websocket.send_json({"type": "error", "message": str(e)})
        ws_manager.disconnect(websocket)
