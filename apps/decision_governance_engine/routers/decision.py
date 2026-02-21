"""決策処理APIルーター.

PipelineEngine パターンを使用した決策処理 API。

エンドポイント:
    - POST /api/decision: 同期処理（履歴自動保存）
    - GET /api/decision/stream: SSEストリーム付き処理
    - GET /api/decision/history: 履歴照会
    - GET /api/decision/history/{request_id}: 個別履歴取得
    - WebSocket /ws/decision: リアルタイム通知
"""

import logging
import time
from typing import Any
from uuid import UUID, uuid4

from apps.decision_governance_engine.engine import DecisionEngine
from apps.decision_governance_engine.routers.report import cache_report
from apps.decision_governance_engine.schemas.input_schemas import (
    BudgetConstraint,
    ConstraintSet,
    StakeholderInfo,
    TimelineConstraint,
)
from apps.decision_governance_engine.schemas.output_schemas import DecisionReport
from apps.decision_governance_engine.services.decision_contract_builder import (
    DecisionGovContractBuilder,
)
from apps.decision_governance_engine.services.human_review_policy import (
    enrich_review_with_policy,
)
from fastapi import APIRouter, HTTPException, Query, WebSocket, WebSocketDisconnect
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field


logger = logging.getLogger("decision_api.decision")

# 履歴保存フラグ（環境変数で制御可能）
import contextlib
import os


ENABLE_HISTORY = os.getenv("ENABLE_DECISION_HISTORY", "true").lower() == "true"

router = APIRouter(tags=["決策処理"])


# ========================================
# スキーマ定義
# ========================================


class DecisionAPIRequest(BaseModel):
    """API リクエストスキーマ."""

    question: str = Field(..., min_length=15, max_length=2000, description="意思決定の質問")
    budget: float | None = Field(None, ge=0, description="予算制約（万円）")
    timeline_months: int | None = Field(None, ge=1, le=120, description="期間制約（月）")
    technical_constraints: list[str] = Field(default_factory=list, description="技術制約")
    regulatory_constraints: list[str] = Field(default_factory=list, description="法規制約")
    human_resources: list[str] = Field(default_factory=list, description="人的リソース")
    stakeholders: StakeholderInfo | None = Field(
        None,
        description="ステークホルダー（責任者）情報（任意）",
    )


class DecisionAPIResponse(BaseModel):
    """API レスポンススキーマ."""

    status: str = Field(..., description="処理状態")
    request_id: str | None = Field(None, description="履歴照会・PDF出力用のリクエストID（UUID）")
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
_fallback_history_records: list[dict[str, Any]] = []
_FALLBACK_HISTORY_MAX = 200


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


def _extract_results_for_history(result_data: dict[str, Any]) -> dict[str, Any]:
    """履歴保存用に各セクション結果を抽出.

    注意:
        - Engine の出力は v3 系で `dao/fa/shu/qi/review` を基本とするが、
          旧互換で `*_analysis` を含む場合があるため両方を許容する。
        - DecisionRepository.save() が参照するキーに合わせて返す。
    """

    def _pick(*keys: str) -> Any:
        for key in keys:
            if key in result_data and result_data.get(key) is not None:
                return result_data.get(key)
        return None

    return {
        "cognitive_gate": _pick("cognitive_gate"),
        "gatekeeper": _pick("gatekeeper"),
        "dao": _pick("dao", "dao_analysis"),
        "fa": _pick("fa", "fa_analysis"),
        "shu": _pick("shu", "shu_analysis"),
        "qi": _pick("qi", "qi_analysis"),
        "review": enrich_review_with_policy(_pick("review")),
    }


def _extract_confidence_for_history(result_data: dict[str, Any]) -> float | None:
    """履歴表示用の信頼度を抽出."""
    raw_confidence = result_data.get("confidence")
    if isinstance(raw_confidence, (int, float)):
        return float(raw_confidence)

    review = result_data.get("review")
    if hasattr(review, "model_dump"):
        review = review.model_dump()
    if isinstance(review, dict):
        review_confidence = review.get("confidence_score")
        if isinstance(review_confidence, (int, float)):
            return float(review_confidence)
    return None


def _save_fallback_history(
    request_id: UUID,
    question: str,
    decision_role: str,
    confidence: float | None,
    mode: str,
    results: dict[str, Any],
    processing_time_ms: int | None,
) -> None:
    """DB 保存失敗時のメモリ退避."""
    record = {
        "id": f"fallback-{request_id}",
        "request_id": str(request_id),
        "question": question,
        "decision_role": decision_role,
        "confidence": confidence,
        "mode": mode,
        "created_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "fa_result": results.get("fa"),
        "shu_result": results.get("shu"),
        "qi_result": results.get("qi"),
        "review_result": results.get("review"),
        "processing_time_ms": processing_time_ms,
    }
    _fallback_history_records.insert(0, record)
    if len(_fallback_history_records) > _FALLBACK_HISTORY_MAX:
        del _fallback_history_records[_FALLBACK_HISTORY_MAX:]


def build_input_dict(
    question: str,
    constraints: ConstraintSet,
    stakeholders: StakeholderInfo | None = None,
) -> dict:
    """Agent用入力辞書を構築.

    Args:
        question: ユーザーの質問
        constraints: 制約セット
        stakeholders: ステークホルダー（責任者）情報（任意）

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

    # FaAgent用: 利用可能リソース辞書を構築
    available_resources: dict[str, str] = {}
    if constraints.budget:
        available_resources["budget"] = f"{constraints.budget.amount}万円"
    if constraints.human_resources:
        available_resources["team"] = ", ".join(constraints.human_resources)

    # FaAgent用: 時間軸を文字列化
    time_horizon = ""
    if constraints.timeline:
        time_horizon = f"{constraints.timeline.months}ヶ月"

    result: dict = {
        "raw_question": question,
        "question": question,  # 後方互換のために残す
        "constraints": constraint_list,
        # QiAgent用: 技術制約を分離（QiInput.tech_constraints へマッピング）
        "tech_constraints": list(constraints.technical),
        # FaAgent用: リソースと時間軸（FaInput.available_resources, time_horizon へマッピング）
        "available_resources": available_resources,
        "time_horizon": time_horizon,
    }

    # ステークホルダー情報を list[str] 形式に変換（DaoInput.stakeholders の型に合わせる）
    if stakeholders:
        label_map = {
            "product_owner": "プロダクトオーナー",
            "tech_lead": "技術責任者",
            "business_owner": "事業責任者",
            "legal_reviewer": "法務・コンプライアンス担当",
        }
        stakeholder_list = [f"{label_map.get(k, k)}: {v}" for k, v in stakeholders.model_dump().items() if v]
        if stakeholder_list:
            result["stakeholders"] = stakeholder_list

    return result


_STAGE_EXECUTION_ORDER = [
    "cognitive_gate",
    "gatekeeper",
    "clarification",
    "dao",
    "fa",
    "shu",
    "qi",
    "review",
]


def _parse_request_uuid(request_id: str | None) -> UUID | None:
    """request_id 文字列を UUID に変換."""
    if not request_id:
        return None
    try:
        return UUID(request_id)
    except (ValueError, TypeError):
        return None


def _detect_resume_from_stage(completed_stages: set[str]) -> str:
    """完了済みステージから再開ステージを推定."""
    for stage_name in _STAGE_EXECUTION_ORDER:
        if stage_name not in completed_stages:
            return stage_name
    return "review"


async def _load_resume_context(
    request_id: UUID,
    question: str,
) -> tuple[set[str], dict[str, dict[str, Any]], str]:
    """DBからresume実行用コンテキストを取得."""
    if not ENABLE_HISTORY:
        return set(), {}, question

    try:
        from apps.decision_governance_engine.repositories import DecisionRepository

        repo = DecisionRepository()
        completed_stages, stage_results, recorded_question = await repo.get_resume_context(request_id)
        effective_question = recorded_question or question
        return completed_stages, stage_results, effective_question
    except Exception as e:
        logger.warning(f"[SSE] resumeコンテキスト取得失敗（通常実行へフォールバック）: {e}")
        return set(), {}, question


@router.post("/api/decision", response_model=DecisionAPIResponse | RejectionResponse)
async def process_decision(
    req: DecisionAPIRequest,
    format: str = Query(
        default="legacy",
        description="レスポンス形式（legacy: DecisionReport 互換 / v1: 字段级契约）",
    ),
) -> DecisionAPIResponse | RejectionResponse:
    """同期的に意思決定を処理.

    PipelineEngine.run() を使用。履歴は自動保存。
    """
    start_time = time.time()
    request_id = uuid4()

    engine = get_engine()
    constraints = build_constraints(req)
    inputs = build_input_dict(req.question, constraints, req.stakeholders)
    # ステージ単位DB保存用に request_id を入力に渡す
    inputs["_request_id"] = request_id
    result = await engine.run(inputs)

    processing_time_ms = int((time.time() - start_time) * 1000)

    # 拒否の場合
    if isinstance(result, dict) and result.get("status") == "rejected":
        return RejectionResponse(**result)

    # 成功の場合 - 最終メタ情報を確定（ステージ結果は Engine 側で保存済み）
    decision_role = "PILOT"
    confidence = None
    report_case_id: str | None = None
    results_dict: dict[str, Any] = {}

    if isinstance(result, DecisionReport):
        report_data = result.model_dump()
        decision_role = _infer_decision_role(report_data)
        confidence = _extract_confidence_for_history(report_data)
        report_case_id = result.report_id
        results_dict = {
            "cognitive_gate": None,
            "gatekeeper": None,
            "dao": report_data.get("dao"),
            "fa": report_data.get("fa"),
            "shu": report_data.get("shu"),
            "qi": report_data.get("qi"),
            "review": report_data.get("review"),
        }
    elif isinstance(result, dict):
        decision_role = _infer_decision_role(result)
        confidence = _extract_confidence_for_history(result)
        report_case_id = str(result.get("report_id", "")) or None
        results_dict = _extract_results_for_history(result)

    # パイプライン完了後にメタ情報を確定（失敗しても処理は継続）
    if ENABLE_HISTORY:
        try:
            from apps.decision_governance_engine.repositories import DecisionRepository

            repo = DecisionRepository()
            # まず finalize を試みる（ステージ単位保存で既にレコードが存在する場合）
            finalized = await repo.finalize(
                request_id=request_id,
                decision_role=decision_role,
                confidence=confidence,
                report_case_id=report_case_id,
                processing_time_ms=processing_time_ms,
            )
            if not finalized:
                # レコード未存在の場合はフル保存（フォールバック）
                await repo.save(
                    request_id=request_id,
                    question=req.question,
                    decision_role=decision_role,
                    mode="STANDARD",
                    confidence=confidence,
                    report_case_id=report_case_id,
                    results=results_dict,
                    processing_time_ms=processing_time_ms,
                )
            logger.info(f"決策履歴保存完了: {request_id}")
        except Exception as e:
            logger.warning(f"決策履歴保存失敗（処理は継続）: {e}")
            _save_fallback_history(
                request_id=request_id,
                question=req.question,
                decision_role=decision_role,
                confidence=confidence,
                mode="STANDARD",
                results=results_dict,
                processing_time_ms=processing_time_ms,
            )

    # レスポンス生成 + キャッシュ保存
    if isinstance(result, DecisionReport):
        cache_report(result.report_id, result)
        cache_report(str(request_id), result)
        if format == "v1":
            contract = DecisionGovContractBuilder.build_from_report(result)
            return DecisionAPIResponse(
                status="success",
                request_id=str(request_id),
                report_id=result.report_id,
                data=contract.model_dump(),
            )
        return DecisionAPIResponse(
            status="success",
            request_id=str(request_id),
            report_id=result.report_id,
            data=result.model_dump(),
        )

    report_id = result.get("report_id", "") if isinstance(result, dict) else ""
    if report_id:
        _cache_report_from_result(result if isinstance(result, dict) else {}, request_id=str(request_id))
    if format == "v1":
        contract = DecisionGovContractBuilder.build_from_report(result)
        return DecisionAPIResponse(
            status="success",
            request_id=str(request_id),
            report_id=report_id,
            data=contract.model_dump(),
        )
    return DecisionAPIResponse(status="success", request_id=str(request_id), report_id=report_id, data=result)


def _cache_report_from_result(result_data: dict[str, Any], request_id: str | None = None) -> None:
    """SSE完了結果からレポートをキャッシュに保存.

    report_id（PROP-* 形式）をキーとしてキャッシュする。
    PDF出力時に _get_report_from_db() から参照される。

    Args:
        result_data: SSE完了時の結果データ
    """
    report_id = result_data.get("report_id", "")
    if not report_id:
        return

    payload = dict(result_data)
    payload["review"] = enrich_review_with_policy(payload.get("review"))

    try:
        # DecisionReport として構築を試みる
        report = DecisionReport(**payload)
        cache_report(report_id, report)
        if request_id:
            cache_report(request_id, report)
    except Exception:
        # Pydantic 構築失敗時は dict のままキャッシュ
        cache_report(report_id, payload)
        if request_id:
            cache_report(request_id, payload)
        logger.info(f"[SSE] レポートを dict 形式でキャッシュ: {report_id}")


def _infer_decision_role(data: dict[str, Any]) -> str:
    """レポートデータから決策結論を推論.

    Args:
        data: レポートデータ

    Returns:
        GO/NO_GO/DELAY/PILOT
    """
    # 明示的な decision_role があればそれを使用
    if "decision_role" in data:
        return data["decision_role"]

    # scoring.verdict があれば最優先
    scoring = data.get("scoring")
    if hasattr(scoring, "model_dump"):
        scoring = scoring.model_dump()
    if isinstance(scoring, dict):
        verdict = str(scoring.get("verdict", "")).upper()
        if verdict in {"GO", "NO_GO", "DELAY", "PILOT"}:
            return verdict

    # recommendation から推論
    recommendation = data.get("recommendation", "").lower()
    if "立項" in recommendation or "go" in recommendation or "実施" in recommendation:
        return "GO"
    if "不立項" in recommendation or "no_go" in recommendation or "中止" in recommendation:
        return "NO_GO"
    if "延後" in recommendation or "delay" in recommendation or "延期" in recommendation:
        return "DELAY"

    # デフォルトは PILOT（最も安全）
    return "PILOT"


# ========================================
# 履歴照会エンドポイント
# ========================================


class HistoryListResponse(BaseModel):
    """履歴一覧レスポンス."""

    status: str = "success"
    total: int = 0
    items: list[dict[str, Any]] = Field(default_factory=list)


class HistoryDetailResponse(BaseModel):
    """履歴詳細レスポンス."""

    status: str = "success"
    data: dict[str, Any] = Field(default_factory=dict)


@router.get("/api/decision/history", response_model=HistoryListResponse)
async def get_decision_history(
    limit: int = Query(default=20, ge=1, le=100, description="取得件数"),
    decision_role: str | None = Query(default=None, description="決策結果フィルタ（GO/NO_GO/DELAY/PILOT）"),
    mode: str | None = Query(default=None, description="モードフィルタ（FAST/STANDARD/AUDIT）"),
) -> HistoryListResponse:
    """決策履歴一覧を取得.

    Args:
        limit: 取得件数上限
        decision_role: 決策結果でフィルタ
        mode: モードでフィルタ

    Returns:
        履歴一覧
    """
    if not ENABLE_HISTORY:
        filtered_fallback = [
            item
            for item in _fallback_history_records
            if (not decision_role or item.get("decision_role") == decision_role)
            and (not mode or item.get("mode") == mode)
        ]
        fallback_items = [
            {
                "id": item.get("id", ""),
                "request_id": item.get("request_id", ""),
                "question": item.get("question", ""),
                "decision_role": item.get("decision_role", "PILOT"),
                "confidence": item.get("confidence"),
                "mode": item.get("mode", "STANDARD"),
                "created_at": item.get("created_at"),
            }
            for item in filtered_fallback[:limit]
        ]
        return HistoryListResponse(status="fallback", total=len(fallback_items), items=fallback_items)

    try:
        from apps.decision_governance_engine.repositories import DecisionRepository

        repo = DecisionRepository()
        records = await repo.find_recent(limit=limit, decision_role=decision_role, mode=mode)

        items = [
            {
                "id": str(r.id),
                "request_id": str(r.request_id),
                "question": r.question[:100] + "..." if len(r.question) > 100 else r.question,
                "decision_role": r.decision_role,
                "confidence": float(r.confidence) if r.confidence else None,
                "mode": r.mode,
                "created_at": r.created_at.isoformat() if r.created_at else None,
            }
            for r in records
        ]
        # DB 取得結果にフォールバック記録を補完
        known_request_ids = {item["request_id"] for item in items}
        fallback_items = [
            {
                "id": item.get("id", ""),
                "request_id": item.get("request_id", ""),
                "question": item.get("question", ""),
                "decision_role": item.get("decision_role", "PILOT"),
                "confidence": item.get("confidence"),
                "mode": item.get("mode", "STANDARD"),
                "created_at": item.get("created_at"),
            }
            for item in _fallback_history_records
            if item.get("request_id") not in known_request_ids
        ]
        all_items = (items + fallback_items)[:limit]
        return HistoryListResponse(status="success", total=len(all_items), items=all_items)
    except Exception as e:
        logger.exception(f"履歴取得失敗。フォールバックを返却: {e}")
        fallback_items = [
            {
                "id": item.get("id", ""),
                "request_id": item.get("request_id", ""),
                "question": item.get("question", ""),
                "decision_role": item.get("decision_role", "PILOT"),
                "confidence": item.get("confidence"),
                "mode": item.get("mode", "STANDARD"),
                "created_at": item.get("created_at"),
            }
            for item in _fallback_history_records
            if (not decision_role or item.get("decision_role") == decision_role)
            and (not mode or item.get("mode") == mode)
        ][:limit]

        return HistoryListResponse(status="fallback", total=len(fallback_items), items=fallback_items)


@router.get("/api/decision/history/{request_id}", response_model=HistoryDetailResponse)
async def get_decision_detail(request_id: str) -> HistoryDetailResponse:
    """決策履歴詳細を取得.

    Args:
        request_id: リクエストID（UUID）

    Returns:
        履歴詳細
    """
    if not ENABLE_HISTORY:
        fallback_data = next(
            (item for item in _fallback_history_records if item.get("request_id") == request_id),
            None,
        )
        if fallback_data:
            return HistoryDetailResponse(status="fallback", data=fallback_data)
        raise HTTPException(status_code=503, detail="履歴機能は無効です")

    try:
        from apps.decision_governance_engine.repositories import DecisionRepository

        repo = DecisionRepository()
        record = await repo.find_by_request_id(UUID(request_id))

        if not record:
            raise HTTPException(status_code=404, detail="履歴が見つかりません")

        data = {
            "id": str(record.id),
            "request_id": str(record.request_id),
            "question": record.question,
            "decision_role": record.decision_role,
            "confidence": float(record.confidence) if record.confidence else None,
            "mode": record.mode,
            "fa_result": record.fa_result,
            "shu_result": record.shu_result,
            "qi_result": record.qi_result,
            "review_result": record.review_result,
            "stage_io_logs": record.stage_io_logs,
            "summary_bullets": record.summary_bullets,
            "warnings": record.warnings,
            "processing_time_ms": record.processing_time_ms,
            "created_at": record.created_at.isoformat() if record.created_at else None,
        }
        return HistoryDetailResponse(status="success", data=data)
    except HTTPException:
        raise
    except Exception as e:
        logger.warning(f"履歴詳細取得失敗。フォールバック検索: {e}")
        fallback_data = next(
            (item for item in _fallback_history_records if item.get("request_id") == request_id),
            None,
        )
        if fallback_data:
            return HistoryDetailResponse(status="fallback", data=fallback_data)
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/api/decision/stream")
async def process_decision_stream(
    question: str = Query(..., min_length=15, max_length=2000, description="意思決定の質問"),
    budget: float | None = Query(None, ge=0, description="予算制約（万円）"),
    timeline_months: int | None = Query(None, ge=1, le=120, description="期間制約（月）"),
    stakeholder_product_owner: str = Query("", max_length=100, description="プロダクトオーナー"),
    stakeholder_tech_lead: str = Query("", max_length=100, description="技術責任者"),
    stakeholder_business_owner: str = Query("", max_length=100, description="事業責任者"),
    stakeholder_legal_reviewer: str = Query("", max_length=100, description="法務担当"),
    technical_constraints: list[str] = Query(default=[], description="技術制約"),
    regulatory_constraints: list[str] = Query(default=[], description="規制・コンプライアンス制約"),
    human_resources: str = Query("", max_length=100, description="人的リソース（チーム人数等）"),
    request_id: str | None = Query(
        default=None,
        description="既存request_id（resume時に指定）",
    ),
    resume: bool = Query(
        default=False,
        description="true の場合、完了済みステージをスキップして再開",
    ),
) -> StreamingResponse:
    """SSEストリーム付きで意思決定を処理.

    PipelineEngine.run_stream() を使用。履歴は完了時に自動保存。
    """
    import json

    logger.info("[SSE] /api/decision/stream リクエスト受信")
    logger.info(f"[SSE] question={question[:50]}...")

    start_time = time.time()
    request_uuid = _parse_request_uuid(request_id)
    if resume and request_uuid is None:
        logger.warning("[SSE] resume=true だが request_id が不正なため新規実行へフォールバック")
    request_uuid = request_uuid or uuid4()

    engine = get_engine()
    constraints = ConstraintSet()
    if budget is not None:
        constraints.budget = BudgetConstraint(amount=budget, currency="JPY")
    if timeline_months is not None:
        constraints.timeline = TimelineConstraint(months=timeline_months)
    # 技術制約・規制制約・人的リソースを設定
    constraints.technical = technical_constraints
    constraints.regulatory = regulatory_constraints
    if human_resources:
        constraints.human_resources = [human_resources]

    # ステークホルダー情報を構築（いずれかの値が設定されている場合のみ）
    stakeholders: StakeholderInfo | None = None
    if any(
        [
            stakeholder_product_owner,
            stakeholder_tech_lead,
            stakeholder_business_owner,
            stakeholder_legal_reviewer,
        ]
    ):
        stakeholders = StakeholderInfo(
            product_owner=stakeholder_product_owner,
            tech_lead=stakeholder_tech_lead,
            business_owner=stakeholder_business_owner,
            legal_reviewer=stakeholder_legal_reviewer,
        )

    inputs = build_input_dict(question, constraints, stakeholders)

    completed_stages: set[str] = set()
    resume_stage_results: dict[str, dict[str, Any]] = {}
    effective_question = question
    if resume:
        (
            completed_stages,
            resume_stage_results,
            effective_question,
        ) = await _load_resume_context(request_uuid, question)
        if effective_question:
            inputs["raw_question"] = effective_question
            inputs["question"] = effective_question
        if completed_stages:
            inputs["_resume_completed_stages"] = sorted(completed_stages)
        if resume_stage_results:
            inputs["_resume_stage_results"] = resume_stage_results

    # ステージ単位DB保存用に request_id を入力に渡す
    inputs["_request_id"] = request_uuid

    def serialize_event(event: dict) -> str:
        """イベントをJSON文字列に変換（enum対応）."""
        # event_type が Enum の場合は .value を使用
        if "event_type" in event:
            et = event["event_type"]
            if hasattr(et, "value"):
                event = {**event, "event_type": et.value}
        return json.dumps(event, ensure_ascii=False, default=str)

    async def save_history(result_data: dict[str, Any]) -> None:
        """完了時にメタ情報を確定（ステージ結果は Engine 側で保存済み）."""
        if not ENABLE_HISTORY:
            return

        processing_time_ms = int((time.time() - start_time) * 1000)
        decision_role = _infer_decision_role(result_data)
        confidence = _extract_confidence_for_history(result_data)
        report_case_id = str(result_data.get("report_id", "")) or None
        results_dict = _extract_results_for_history(result_data)

        try:
            from apps.decision_governance_engine.repositories import DecisionRepository

            repo = DecisionRepository()
            # まず finalize を試みる（ステージ単位保存で既にレコードが存在する場合）
            finalized = await repo.finalize(
                request_id=request_uuid,
                decision_role=decision_role,
                confidence=confidence,
                report_case_id=report_case_id,
                processing_time_ms=processing_time_ms,
            )
            if not finalized:
                # レコード未存在の場合はフル保存（フォールバック）
                await repo.save(
                    request_id=request_uuid,
                    question=effective_question,
                    decision_role=decision_role,
                    mode="STANDARD",
                    confidence=confidence,
                    report_case_id=report_case_id,
                    results=results_dict,
                    processing_time_ms=processing_time_ms,
                )
            logger.info(f"[SSE] 決策履歴保存完了: {request_uuid}")
        except Exception as e:
            logger.warning(f"[SSE] 決策履歴保存失敗（処理は継続）: {e}")
            _save_fallback_history(
                request_id=request_uuid,
                question=effective_question,
                decision_role=decision_role,
                confidence=confidence,
                mode="STANDARD",
                results=results_dict,
                processing_time_ms=processing_time_ms,
            )

    async def generate_events():
        """SSEイベントを生成."""
        import asyncio
        import time as time_module

        logger.info("[SSE] ストリーム開始")
        # 即座に接続確認イベントを送信
        yield (
            "data: "
            + json.dumps(
                {
                    "event_type": "connection.established",
                    "timestamp": time_module.time(),
                    "data": {"request_id": str(request_uuid)},
                },
                ensure_ascii=False,
            )
            + "\n\n"
        )

        if resume:
            resume_from_stage = _detect_resume_from_stage(completed_stages)
            yield (
                "data: "
                + json.dumps(
                    {
                        "event_type": "resume.context",
                        "timestamp": time_module.time(),
                        "data": {
                            "request_id": str(request_uuid),
                            "completed_stages": sorted(completed_stages),
                            "resume_from_stage": resume_from_stage,
                        },
                    },
                    ensure_ascii=False,
                )
                + "\n\n"
            )

        final_result: dict[str, Any] = {}
        is_rejected = False

        try:
            async for event in engine.run_stream(inputs):
                # イベントタイプをログ出力（type または event_type）
                event_type = event.get("event_type") or event.get("type", "unknown")
                logger.info(f"[SSE] イベント発行: {event_type}")

                # flow.complete または result イベントから結果を取得
                if event_type == "flow.complete" and event.get("result"):
                    final_result = event.get("result", {})
                    if final_result and not is_rejected:
                        # PDF出力の即時性を優先し、DB保存前にキャッシュへ格納
                        _cache_report_from_result(final_result, request_id=str(request_uuid))
                elif event_type == "result" and event.get("data"):
                    data = event.get("data", {})
                    if data.get("status") == "rejected":
                        is_rejected = True
                    elif data.get("results"):
                        final_result = data.get("results", {})
                elif event_type == "early_return":
                    is_rejected = True

                if isinstance(event, dict):
                    yield f"data: {serialize_event(event)}\n\n"
                else:
                    yield f"data: {event}\n\n"

            # 成功完了時のみ履歴を保存（拒否時は保存しない）
            if final_result and not is_rejected:
                await save_history(final_result)

                # レポートキャッシュに保存（PDF出力用）
                _cache_report_from_result(final_result, request_id=str(request_uuid))

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
            with contextlib.suppress(Exception):
                await connection.send_json(message)


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

            if not question or len(question) < 15:
                await websocket.send_json(
                    {
                        "type": "error",
                        "message": "質問は15文字以上必要です",
                    }
                )
                continue

            constraints = ConstraintSet()
            if budget is not None:
                constraints.budget = BudgetConstraint(amount=budget, currency="JPY")
            if timeline_months is not None:
                constraints.timeline = TimelineConstraint(months=timeline_months)

            # WebSocket経由のステークホルダー情報
            ws_stakeholders: StakeholderInfo | None = None
            stakeholder_data = data.get("stakeholders")
            if isinstance(stakeholder_data, dict):
                ws_stakeholders = StakeholderInfo(**stakeholder_data)

            inputs = build_input_dict(question, constraints, ws_stakeholders)
            engine = get_engine()

            async for event in engine.run_stream(inputs):
                event_data = {
                    "type": "agent_event",
                    "event_type": getattr(event, "event_type", {}).value
                    if hasattr(getattr(event, "event_type", None), "value")
                    else str(event.get("event_type", "")),
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

            await websocket.send_json(
                {
                    "type": "complete",
                    "message": "意思決定プロセスが完了しました",
                }
            )

    except WebSocketDisconnect:
        ws_manager.disconnect(websocket)
    except Exception as e:
        await websocket.send_json({"type": "error", "message": str(e)})
        ws_manager.disconnect(websocket)
