# -*- coding: utf-8 -*-
"""Decision Governance Engine - FastAPI REST API.

エンドポイント:
- POST /api/decision: 同期処理
- GET /api/decision/stream: SSEストリーム付き処理
- GET /api/agents: Agent定義取得
- GET /api/health: ヘルスチェック
"""

import asyncio
import io
import json
import logging
from collections.abc import AsyncIterator
from typing import Any

from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field

from apps.decision_governance_engine.schemas.input_schemas import (
    BudgetConstraint,
    ConstraintSet,
    DecisionRequest,
    TimelineConstraint,
)
from apps.decision_governance_engine.schemas.output_schemas import DecisionReport
from apps.decision_governance_engine.workflow import DecisionEngine

# ロガー設定
logger = logging.getLogger("decision_api")

# FastAPIアプリ
app = FastAPI(
    title="Decision Governance Engine API",
    description="企業級決策Agent平台 - 意思決定支援システム",
    version="1.0.0",
    docs_url="/docs",
    redoc_url="/redoc",
)

# CORS設定
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # 本番環境では適切に設定
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# リクエスト/レスポンススキーマ
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


class AgentDefinition(BaseModel):
    """Agent定義スキーマ."""

    id: str
    name: str
    label: str


class HealthResponse(BaseModel):
    """ヘルスチェックレスポンス."""

    status: str = "ok"
    version: str = "1.0.0"


# エンジンインスタンス（グローバル）
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


@app.get("/api/health", response_model=HealthResponse)
async def health_check() -> HealthResponse:
    """ヘルスチェック."""
    return HealthResponse()


@app.get("/api/agents", response_model=list[AgentDefinition])
async def get_agents() -> list[AgentDefinition]:
    """Agent定義を取得."""
    engine = get_engine()
    return [AgentDefinition(**d) for d in engine.get_agent_definitions()]


@app.post("/api/decision", response_model=DecisionAPIResponse | RejectionResponse)
async def process_decision(req: DecisionAPIRequest) -> DecisionAPIResponse | RejectionResponse:
    """同期的に意思決定を処理."""
    engine = get_engine()
    constraints = build_constraints(req)
    request = DecisionRequest(question=req.question, constraints=constraints)

    result = await engine.process(request)

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

    return DecisionAPIResponse(status="success", data=result)


@app.get("/api/decision/stream")
async def process_decision_stream(
    question: str = Query(..., min_length=10, max_length=2000, description="意思決定の質問"),
    budget: float | None = Query(None, ge=0, description="予算制約（万円）"),
    timeline_months: int | None = Query(None, ge=1, le=120, description="期間制約（月）"),
) -> StreamingResponse:
    """SSEストリーム付きで意思決定を処理.

    AG-UIプロトコル準拠のイベントをServer-Sent Eventsで配信。
    """
    engine = get_engine()

    # 制約を構築
    constraints = ConstraintSet()
    if budget is not None:
        constraints.budget = BudgetConstraint(amount=budget, currency="JPY")
    if timeline_months is not None:
        constraints.timeline = TimelineConstraint(months=timeline_months)

    request = DecisionRequest(question=question, constraints=constraints)

    async def event_generator() -> AsyncIterator[str]:
        """SSEイベントジェネレーター."""
        try:
            async for event in engine.process_with_events(request):
                # AG-UIイベントをSSE形式に変換
                event_data = {
                    "event_type": event.event_type.value,
                    "timestamp": event.timestamp,
                    "flow_id": event.flow_id,
                    "data": event.data,
                }
                # ノードイベントの追加フィールド
                if hasattr(event, "node_id"):
                    event_data["node_id"] = event.node_id
                if hasattr(event, "node_name"):
                    event_data["node_name"] = event.node_name
                if hasattr(event, "percentage"):
                    event_data["percentage"] = event.percentage
                if hasattr(event, "current"):
                    event_data["current"] = event.current
                if hasattr(event, "total"):
                    event_data["total"] = event.total
                if hasattr(event, "error_message"):
                    event_data["error_message"] = event.error_message
                if hasattr(event, "level"):
                    event_data["level"] = event.level
                if hasattr(event, "message"):
                    event_data["message"] = event.message

                yield f"data: {json.dumps(event_data, ensure_ascii=False)}\n\n"

        except Exception as e:
            error_data = {
                "event_type": "flow.error",
                "error_message": str(e),
                "error_type": type(e).__name__,
            }
            yield f"data: {json.dumps(error_data, ensure_ascii=False)}\n\n"

    return StreamingResponse(
        event_generator(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )


@app.get("/api/report/{report_id}/pdf")
async def export_report_pdf(report_id: str) -> StreamingResponse:
    """レポートをPDF形式でエクスポート.

    注意: 現在はレポートIDの検証なし（デモ用）。
    実運用ではストレージからレポートを取得する必要がある。
    """
    from apps.decision_governance_engine.services.pdf_generator import PDFGeneratorService

    # デモ用：空のレポートを生成（実際はDBから取得）
    from apps.decision_governance_engine.schemas.output_schemas import (
        DecisionReport,
        ExecutiveSummary,
    )

    # サンプルレポート（実際はストレージから取得）
    sample_report = DecisionReport(
        report_id=report_id,
        dao={"problem_type": "TRADE_OFF", "essence": "サンプル本質"},
        fa={"recommended_paths": [{"name": "推奨案A", "description": "説明"}]},
        shu={"phases": [{"phase_number": 1, "name": "準備", "duration": "2週間"}], "first_action": "MTG設定"},
        qi={"implementations": [], "tool_recommendations": []},
        review={"overall_verdict": "PASS", "confidence_score": 0.85},
        executive_summary=ExecutiveSummary(
            one_line_decision="推奨案Aを選択",
            recommended_action="段階的に実行",
            key_risks=["リスク1"],
            first_step="キックオフMTG",
            estimated_impact="目標達成見込み",
        ),
    )

    generator = PDFGeneratorService()
    pdf_bytes = generator.generate_pdf(sample_report)

    # Content-Typeを判定（ReportLabがない場合はHTML）
    content_type = "application/pdf" if generator._has_reportlab else "text/html"
    filename = f"decision_report_{report_id}.{'pdf' if generator._has_reportlab else 'html'}"

    return StreamingResponse(
        io.BytesIO(pdf_bytes),
        media_type=content_type,
        headers={"Content-Disposition": f"attachment; filename={filename}"},
    )


# A2UIコンポーネント取得エンドポイント
@app.get("/api/report/{report_id}/components")
async def get_report_components(report_id: str) -> dict[str, Any]:
    """レポートのA2UIコンポーネントを取得.

    フロントエンドはこれを受け取ってレンダリングする。
    """
    from apps.decision_governance_engine.services.ui_components import (
        DecisionUIComponentBuilder,
    )
    from apps.decision_governance_engine.schemas.output_schemas import (
        DecisionReport,
        ExecutiveSummary,
    )

    # デモ用サンプルレポート
    sample_report = DecisionReport(
        report_id=report_id,
        dao={"problem_type": "TRADE_OFF", "essence": "サンプル本質"},
        fa={"recommended_paths": [{"name": "推奨案A", "description": "説明"}]},
        shu={"phases": [{"phase_number": 1, "name": "準備", "duration": "2週間"}], "first_action": "MTG設定"},
        qi={"implementations": [], "tool_recommendations": []},
        review={"overall_verdict": "PASS", "confidence_score": 0.85},
        executive_summary=ExecutiveSummary(
            one_line_decision="推奨案Aを選択",
            recommended_action="段階的に実行",
            key_risks=["リスク1"],
            first_step="キックオフMTG",
            estimated_impact="目標達成見込み",
        ),
    )

    builder = DecisionUIComponentBuilder()
    components = builder.build_report_view(sample_report)

    return {
        "report_id": report_id,
        "components": [c.to_dict() for c in components],
    }


# ========================================
# WebSocket進捗通知
# ========================================

from fastapi import WebSocket, WebSocketDisconnect


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


@app.websocket("/ws/decision")
async def websocket_decision(websocket: WebSocket) -> None:
    """WebSocketで意思決定プロセスを実行.

    リアルタイム進捗通知とdao/fa/shu/qi各Agentの出力を配信。
    """
    await ws_manager.connect(websocket)
    try:
        while True:
            # クライアントからのリクエストを受信
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

            # 制約を構築
            constraints = ConstraintSet()
            if budget is not None:
                constraints.budget = BudgetConstraint(amount=budget, currency="JPY")
            if timeline_months is not None:
                constraints.timeline = TimelineConstraint(months=timeline_months)

            request = DecisionRequest(question=question, constraints=constraints)
            engine = get_engine()

            # AG-UIイベントをWebSocketで配信
            async for event in engine.process_with_events(request):
                event_data = {
                    "type": "agent_event",
                    "event_type": event.event_type.value,
                    "timestamp": event.timestamp,
                    "flow_id": event.flow_id,
                    "data": event.data,
                }
                # Agent固有フィールド
                if hasattr(event, "node_id"):
                    event_data["agent_id"] = event.node_id
                if hasattr(event, "node_name"):
                    event_data["agent_name"] = event.node_name
                if hasattr(event, "percentage"):
                    event_data["progress"] = event.percentage
                if hasattr(event, "error_message"):
                    event_data["error"] = event.error_message

                await websocket.send_json(event_data)

            # 完了通知
            await websocket.send_json({
                "type": "complete",
                "message": "意思決定プロセスが完了しました",
            })

    except WebSocketDisconnect:
        ws_manager.disconnect(websocket)
    except Exception as e:
        await websocket.send_json({
            "type": "error",
            "message": str(e),
        })
        ws_manager.disconnect(websocket)


# ========================================
# A2UI 個別Agent出力エンドポイント
# ========================================

@app.get("/api/report/{report_id}/agent/{agent_id}")
async def get_agent_output_component(report_id: str, agent_id: str) -> dict[str, Any]:
    """特定AgentのA2UI出力コンポーネントを取得.

    各Agent（dao/fa/shu/qi/review）の出力を個別に取得。
    """
    from apps.decision_governance_engine.services.ui_components import (
        DecisionUIComponentBuilder,
    )
    from apps.decision_governance_engine.schemas.output_schemas import (
        DecisionReport,
        ExecutiveSummary,
    )

    # デモ用サンプルレポート（実際はDBから取得）
    sample_report = DecisionReport(
        report_id=report_id,
        dao={"problem_type": "TRADE_OFF", "essence": "リソース配分の最適化"},
        fa={"recommended_paths": [
            {"name": "推奨案A", "description": "新規事業に集中", "success_probability": 0.7}
        ]},
        shu={"phases": [
            {"phase_number": 1, "name": "準備", "duration": "2週間"},
            {"phase_number": 2, "name": "実行", "duration": "2ヶ月"},
        ], "first_action": "キックオフMTG設定"},
        qi={"implementations": [
            {"component": "API", "technology": "FastAPI", "estimated_effort": "1人月"}
        ], "tool_recommendations": ["Jira", "Slack"]},
        review={"overall_verdict": "PASS", "confidence_score": 0.85},
        executive_summary=ExecutiveSummary(
            one_line_decision="推奨案Aを選択",
            recommended_action="段階的に実行",
            key_risks=["リスク1"],
            first_step="キックオフMTG",
            estimated_impact="目標達成見込み",
        ),
    )

    builder = DecisionUIComponentBuilder()

    # Agent IDに応じたコンポーネントを取得
    if agent_id == "dao":
        component = builder._build_dao_card(sample_report.dao)
    elif agent_id == "fa":
        component = builder._build_fa_card(sample_report.fa)
    elif agent_id == "shu":
        component = builder._build_shu_card(sample_report.shu)
    elif agent_id == "qi":
        component = builder._build_qi_card(sample_report.qi)
    elif agent_id == "review":
        component = builder._build_review_card(sample_report.review)
    elif agent_id == "summary":
        component = builder._build_summary_card(sample_report)
    else:
        raise HTTPException(status_code=404, detail=f"Unknown agent: {agent_id}")

    return {
        "report_id": report_id,
        "agent_id": agent_id,
        "component": component.to_dict(),
    }


@app.get("/api/workflow/config")
async def get_workflow_config() -> dict[str, Any]:
    """Studio UI用のワークフロー設定を取得.

    React Flow向けのノード・エッジ定義を含む。
    """
    import yaml
    from pathlib import Path

    config_path = Path(__file__).parent / "agent.yaml"
    with open(config_path) as f:
        config = yaml.safe_load(f)

    return {
        "name": config.get("name"),
        "version": config.get("version"),
        "description": config.get("description"),
        "agents": config.get("agents", {}),
        "workflow": config.get("workflow", {}),
        "studio": config.get("studio", {}),
    }


# アプリ起動（直接実行用）
if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8000)

