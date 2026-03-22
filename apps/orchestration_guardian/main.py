"""Orchestration Guardian API.

A lightweight service for orchestration/protocol readiness checks.

harness 統合:
    - ContractAuthGuard: HTTP ミドルウェア認証（自動トリガー）
    - ExecutionScorer: 検証結果の多次元スコアリング（手動）
    - RiskAssessor: 失敗チェックに基づくリスク評価（手動）
    - ReplayRecorder: 検証実行の記録（手動）
"""

from __future__ import annotations

import argparse
import logging
import os
from pathlib import Path
from typing import Any

import uvicorn
from fastapi import FastAPI, Request, Response
from pydantic import BaseModel, Field

from harness.gating.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig
from harness.replay.service import ReplayRecorder
from harness.risk.service import RiskAssessor, RiskFactor, RiskLevel
from harness.scoring.service import DimensionScore, ExecutionScorer, ScoreDimension
from kernel import __version__ as kernel_version
from kernel.agents.resilient_agent import ResilientAgent
from shared.config.manifest import load_app_manifest_dict


_logger = logging.getLogger(__name__)

_APP_ROOT = Path(__file__).resolve().parent
_APP_CONFIG_PATH = _APP_ROOT / "app_config.json"
_READY_SCORE_THRESHOLD = 75.0

# ---------------------------------------------------------------------------
# ContractAuthGuard（自動トリガー: HTTPミドルウェアとして全リクエストに適用）
# ---------------------------------------------------------------------------
_PUBLIC_HTTP_PATHS = {"/", "/api/health", "/favicon.ico", "/docs", "/redoc", "/openapi.json"}
_auth_guard = ContractAuthGuard(
    ContractAuthGuardConfig(
        app_config_path=_APP_CONFIG_PATH,
        public_http_paths=_PUBLIC_HTTP_PATHS,
        auth_header_name="x-api-key",
        ws_query_key="api_key",
        api_key_env_selector_var="ORCHESTRATION_GUARDIAN_API_KEY_ENV",
        default_api_key_env_var="ORCHESTRATION_GUARDIAN_API_KEY",
    ),
)

# ---------------------------------------------------------------------------
# harness サービス（手動トリガー: /api/verify エンドポイントで呼び出す）
# ---------------------------------------------------------------------------
_scorer = ExecutionScorer(
    weights={
        ScoreDimension.ACCURACY: 1.0,
        ScoreDimension.SAFETY: 2.0,   # セキュリティ関連を重視
        ScoreDimension.COMPLETENESS: 1.0,
    }
)
_risk_assessor = RiskAssessor(threshold=RiskLevel.HIGH)
_replay_recorder = ReplayRecorder()


def _load_app_config() -> dict[str, Any]:
    """Load app_config.json."""
    if not _APP_CONFIG_PATH.is_file():
        return {}
    try:
        return load_app_manifest_dict(_APP_CONFIG_PATH)
    except (OSError, ValueError):
        return {}


class VerifyRequest(BaseModel):
    """Payload for orchestration readiness check."""

    app_name: str = Field(..., min_length=1, description="Target app name")
    has_streaming: bool = Field(default=False, description="Whether SSE/WebSocket is implemented")
    has_a2a: bool = Field(default=False, description="Whether A2A contract is implemented")
    has_rag_contract: bool = Field(default=False, description="Whether RAG contract is aligned")
    has_auth_baseline: bool = Field(default=False, description="Whether auth baseline is enforced")


class VerifyResponse(BaseModel):
    """Response model for orchestration readiness check."""

    app_name: str
    score: float
    passed: int
    total: int
    checks: dict[str, bool]
    status: str
    risk: dict[str, Any]
    harness_score: dict[str, Any]


app = FastAPI(
    title="Orchestration Guardian",
    description="Orchestration and protocol readiness checker",
    version="1.0.0",
)


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """ContractAuthGuard による HTTP 認証（自動トリガー）."""
    return await _auth_guard.http_middleware(request, call_next)


@app.get("/")
async def root() -> dict[str, str]:
    """Root endpoint to avoid 404."""
    return {
        "message": "Orchestration Guardian API",
        "docs_url": "/docs",
        "health_url": "/api/health",
    }


@app.get("/favicon.ico", include_in_schema=False)
async def favicon() -> Response:
    """Favicon endpoint to avoid 404."""
    return Response(content=b"", media_type="image/x-icon")


@app.get("/api/health")
async def health() -> dict[str, Any]:
    """Health endpoint."""
    return {
        "status": "healthy",
        "service": "orchestration_guardian",
        "framework_version": kernel_version,
        "harness": {
            "auth_guard": "enabled",
            "execution_scorer": "enabled",
            "risk_assessor": "enabled",
            "replay_recorder": "enabled",
        },
    }


@app.get("/api/checklist")
async def checklist() -> dict[str, Any]:
    """Return baseline checklist for app orchestration."""
    return {
        "items": [
            "engine pattern and implementation match",
            "frontend-backend protocol contract is explicit",
            "streaming progress surface exists",
            "agent-to-agent contract (A2A card/routing) exists for multi-agent apps",
            "MCP/RAG/Skills declarations match runtime implementation",
            "security baseline is enabled for external dependencies",
        ]
    }


def _build_verify_result(app_name: str, checks: dict[str, bool]) -> dict[str, Any]:
    """ExecutionScorer / RiskAssessor / ReplayRecorder を使って検証結果を構築.

    手動トリガー: /api/verify エンドポイントおよび OrchestrationGuardianAgent.process() から呼ばれる。

    Args:
        app_name: 検証対象アプリ名
        checks: チェック項目と結果のマッピング

    Returns:
        スコア・リスク・再生記録を含む検証結果辞書
    """
    # --- ExecutionScorer: 多次元スコアリング ---
    # streaming/a2a は完全性、auth_baseline は安全性、rag_contract は精度に対応
    dimension_scores = [
        DimensionScore(
            dimension=ScoreDimension.COMPLETENESS,
            score=1.0 if checks.get("streaming", False) else 0.0,
            weight=1.0,
            details="SSE/WebSocket streaming",
        ),
        DimensionScore(
            dimension=ScoreDimension.ACCURACY,
            score=1.0 if checks.get("a2a", False) else 0.0,
            weight=1.0,
            details="A2A contract",
        ),
        DimensionScore(
            dimension=ScoreDimension.ACCURACY,
            score=1.0 if checks.get("rag_contract", False) else 0.0,
            weight=1.0,
            details="RAG contract alignment",
        ),
        DimensionScore(
            dimension=ScoreDimension.SAFETY,
            score=1.0 if checks.get("auth_baseline", False) else 0.0,
            weight=2.0,  # セキュリティ関連は重みを倍にする
            details="auth baseline",
        ),
    ]
    scoring_result = _scorer.score(dimension_scores)
    score_pct = round(scoring_result.overall_score * 100, 2)

    # --- RiskAssessor: 失敗チェックをリスク要因として評価 ---
    risk_factors: list[RiskFactor] = []
    if not checks.get("auth_baseline", False):
        risk_factors.append(
            RiskFactor(
                name="auth_baseline_missing",
                level=RiskLevel.HIGH,
                description="認証ベースラインが未適用。外部アクセスリスクあり。",
                score=0.8,
            )
        )
    if not checks.get("a2a", False):
        risk_factors.append(
            RiskFactor(
                name="a2a_contract_missing",
                level=RiskLevel.MEDIUM,
                description="A2A コントラクトが未定義。エージェント間通信が不安定になる可能性。",
                score=0.5,
            )
        )
    if not checks.get("streaming", False):
        risk_factors.append(
            RiskFactor(
                name="streaming_missing",
                level=RiskLevel.LOW,
                description="ストリーミングが未実装。長時間処理でタイムアウトの可能性。",
                score=0.3,
            )
        )
    if not checks.get("rag_contract", False):
        risk_factors.append(
            RiskFactor(
                name="rag_contract_misaligned",
                level=RiskLevel.LOW,
                description="RAG コントラクトが未整合。知識検索の精度に影響。",
                score=0.2,
            )
        )
    risk_assessment = _risk_assessor.assess(risk_factors)

    # --- ReplayRecorder: 検証実行を記録（監査証跡） ---
    _replay_recorder.record(
        event_type="orchestration_verify",
        payload={
            "app_name": app_name,
            "checks": checks,
            "score_pct": score_pct,
            "risk_level": risk_assessment.overall_level.value,
        },
        step_index=len(_replay_recorder.get_events()),
    )

    passed = sum(1 for ok in checks.values() if ok)
    total = len(checks)
    return {
        "app_name": app_name,
        "score": score_pct,
        "passed": passed,
        "total": total,
        "checks": checks,
        "status": "ready" if score_pct >= _READY_SCORE_THRESHOLD else "needs_improvement",
        "risk": {
            "level": risk_assessment.overall_level.value,
            "is_acceptable": risk_assessment.is_acceptable,
            "mitigations": risk_assessment.mitigations,
            "factors": [
                {"name": f.name, "level": f.level.value, "description": f.description}
                for f in risk_assessment.factors
            ],
        },
        "harness_score": {
            "overall": scoring_result.overall_score,
            "dimensions": [
                {
                    "dimension": ds.dimension.value,
                    "score": ds.score,
                    "weight": ds.weight,
                    "details": ds.details,
                }
                for ds in scoring_result.dimension_scores
            ],
        },
    }


@app.post("/api/verify")
async def verify(payload: VerifyRequest) -> dict[str, Any]:
    """harness Scoring/Risk/Replay を使ってオーケストレーション準備状況を評価（手動トリガー）."""
    checks = {
        "streaming": payload.has_streaming,
        "a2a": payload.has_a2a,
        "rag_contract": payload.has_rag_contract,
        "auth_baseline": payload.has_auth_baseline,
    }
    return _build_verify_result(payload.app_name, checks)


def main() -> None:
    """CLI entrypoint."""
    parser = argparse.ArgumentParser(description="Orchestration Guardian API")
    parser.add_argument("--host", default=None, help="Host (default from env or app_config)")
    parser.add_argument(
        "--port",
        type=int,
        default=None,
        help="Port (default from env or app_config)",
    )
    parser.add_argument("--reload", action="store_true", help="Enable auto reload")
    args = parser.parse_args()

    app_config = _load_app_config()
    default_port = app_config.get("ports", {}).get("api", 8007)
    host = args.host or os.getenv("ORCHESTRATION_GUARDIAN_HOST", "0.0.0.0")
    port = args.port or int(os.getenv("ORCHESTRATION_GUARDIAN_PORT", str(default_port)))

    if args.reload:
        uvicorn.run(
            "apps.orchestration_guardian.main:app",
            host=host,
            port=port,
            reload=True,
            reload_dirs=["apps/orchestration_guardian"],
        )
    else:
        uvicorn.run(app, host=host, port=port)


class OrchestrationGuardianAgent(ResilientAgent[VerifyRequest, VerifyResponse]):
    """オーケストレーション検証 Agent.

    アプリのオーケストレーション準備状況を検証する。
    """

    name = "OrchestrationGuardianAgent"

    async def process(self, input_data: VerifyRequest) -> VerifyResponse:
        """harness Scoring/Risk/Replay を使って検証を実行（手動トリガー）."""
        checks = {
            "streaming": input_data.has_streaming,
            "a2a": input_data.has_a2a,
            "rag_contract": input_data.has_rag_contract,
            "auth_baseline": input_data.has_auth_baseline,
        }
        result = _build_verify_result(input_data.app_name, checks)
        return VerifyResponse(**result)

    def _parse_input(self, input_data: dict[str, Any]) -> VerifyRequest:
        """入力辞書を VerifyRequest に変換."""
        return VerifyRequest(**input_data)


if __name__ == "__main__":
    main()
