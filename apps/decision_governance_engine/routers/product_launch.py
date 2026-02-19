"""产品立项专用 API（端到端）.

目的:
    产品立项场景に特化した端到端 API。
    入力 → 情報採集 → Fa対比 → Shu計画 → Qi実装 → v1契約出力

エンドポイント:
    - POST /api/product-launch: 产品立项決策（完全フロー）
    - POST /api/product-launch/fast: 高速モード（情報採集簡略化）

設計:
    - 3档モード: FAST（5秒以内）/ STANDARD（15秒）/ AUDIT（60秒）
    - 外部情報を自動収集し Evidence として返却
    - 决策结论: GO / NO_GO / DELAY / PILOT
"""

import logging
import time
from uuid import uuid4

from apps.decision_governance_engine.engine import DecisionEngine
from apps.decision_governance_engine.schemas.contract_schemas import (
    DecisionGovResponseV1,
    DecisionMode,
)
from apps.decision_governance_engine.schemas.input_schemas import (
    BudgetConstraint,
    ConstraintSet,
    TimelineConstraint,
)
from apps.decision_governance_engine.services.decision_contract_builder import (
    DecisionGovContractBuilder,
)
from apps.decision_governance_engine.services.intelligence_service import (
    IntelligenceConfig,
    IntelligenceService,
)
from fastapi import APIRouter
from pydantic import BaseModel, Field


logger = logging.getLogger("decision_api.product_launch")

router = APIRouter(tags=["产品立项"])


# ========================================
# リクエスト/レスポンス スキーマ
# ========================================


class ProductLaunchRequest(BaseModel):
    """产品立项リクエスト."""

    question: str = Field(
        ...,
        min_length=15,
        max_length=2000,
        description="立項質問（例: 新SaaS製品を立ち上げるべきか）",
    )
    product_name: str = Field(default="", description="製品名（オプション）")
    target_market: str = Field(default="", description="ターゲット市場")
    budget_万円: float | None = Field(None, ge=0, description="予算制約（万円）")
    timeline_months: int | None = Field(None, ge=1, le=120, description="期間制約（月）")
    competitors: list[str] = Field(default_factory=list, description="競合リスト")
    mode: str = Field(default="STANDARD", description="モード: FAST/STANDARD/AUDIT")


class ProductLaunchResponse(BaseModel):
    """产品立项レスポンス（v1 契約ベース）."""

    status: str = Field(..., description="success/error")
    request_id: str = Field(..., description="リクエストID")
    decision_role: str = Field(..., description="GO/NO_GO/DELAY/PILOT")
    confidence: float = Field(default=0.5, description="確信度")
    processing_time_ms: int = Field(default=0, description="処理時間（ms）")
    contract: DecisionGovResponseV1 | None = Field(None, description="完全な v1 契約")
    summary: list[str] = Field(default_factory=list, description="要点サマリ")
    warnings: list[str] = Field(default_factory=list, description="警告")


# ========================================
# エンジン管理
# ========================================

_engine: DecisionEngine | None = None


def get_engine() -> DecisionEngine:
    """DecisionEngine シングルトン."""
    global _engine
    if _engine is None:
        _engine = DecisionEngine()
    return _engine


# ========================================
# エンドポイント
# ========================================


@router.post("/api/product-launch", response_model=ProductLaunchResponse)
async def process_product_launch(req: ProductLaunchRequest) -> ProductLaunchResponse:
    """产品立项 端到端決策.

    フロー:
        1. 外部情報採集（市場・競合・規制）
        2. 認知分析 + Gatekeeper
        3. Dao → Fa → Shu → Qi 分析
        4. Review + 决策结论生成
        5. v1 契約としてレスポンス
    """
    start_time = time.time()
    request_id = str(uuid4())
    warnings: list[str] = []

    # モード設定
    mode_map = {
        "FAST": DecisionMode.FAST,
        "STANDARD": DecisionMode.STANDARD,
        "AUDIT": DecisionMode.AUDIT,
    }
    decision_mode = mode_map.get(req.mode.upper(), DecisionMode.STANDARD)

    logger.info(
        f"[产品立项] request_id={request_id}, mode={decision_mode}, question={req.question[:50]}..."
    )

    # Step 1: 外部情報採集
    intel_config = IntelligenceConfig(mode=req.mode.upper())
    intel_service = IntelligenceService(intel_config)

    search_query = (
        f"{req.product_name} {req.target_market} market analysis"
        if req.product_name
        else req.question
    )
    topics = req.competitors[:3] if req.competitors else None

    intel_result = await intel_service.gather(search_query, topics=topics)
    warnings.extend(intel_result.warnings)

    # Step 2: エンジン実行
    engine = get_engine()
    constraints = ConstraintSet()
    if req.budget_万円:
        constraints.budget = BudgetConstraint(amount=req.budget_万円, currency="JPY")
    if req.timeline_months:
        constraints.timeline = TimelineConstraint(months=req.timeline_months)

    constraint_list: list[str] = []
    if constraints.budget:
        constraint_list.append(f"予算: {constraints.budget.amount}万円")
    if constraints.timeline:
        constraint_list.append(f"期間: {constraints.timeline.months}ヶ月")

    inputs = {
        "raw_question": req.question,
        "question": req.question,
        "constraints": constraint_list,
        "product_name": req.product_name,
        "target_market": req.target_market,
        "competitors": req.competitors,
    }

    result = await engine.run(inputs)
    processing_time_ms = int((time.time() - start_time) * 1000)

    # Step 3: v1 契約生成
    contract = DecisionGovContractBuilder.build_from_report(
        result, mode=decision_mode, request_id=request_id
    )

    # 外部情報を contract に追加
    contract.evidence.extend(intel_result.evidence)
    contract.warnings.extend(warnings)

    # 情報カバレッジに基づく confidence 調整
    base_confidence = 0.6
    confidence = min(0.95, base_confidence + intel_result.coverage_score * 0.3)

    return ProductLaunchResponse(
        status="success",
        request_id=request_id,
        decision_role=contract.decision_role.value,
        confidence=round(confidence, 3),
        processing_time_ms=processing_time_ms,
        contract=contract,
        summary=contract.summary_bullets[:5],
        warnings=contract.warnings,
    )
