"""ワークフロー定義.

AgentFlow 統一入口で構築。

v1.1 新機能:
    - AI安全防護統合（幻覚検出、データ脱敏）

使用例:
    >>> from apps.market_trend_monitor.backend.workflow import flow
    >>> result = await flow.run({"keywords": ["AI", "LLM"]})
    >>> # ストリーム実行
    >>> async for event in flow.run_stream(inputs):
    ...     print(event)
"""

import logging
from collections.abc import AsyncIterator
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.agents import (
    AnalyzerAgent,
    CollectorAgent,
    EvidenceLedgerAgent,
    NotifierAgent,
    RedTeamAgent,
    ReporterAgent,
    SignalScorerAgent,
)
from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.services.metrics_service import metrics_service
from apps.market_trend_monitor.backend.services.registry import (
    evidence_service,
    signal_service,
    source_reliability_tracker,
)

from harness.budget import BudgetConfig, TokenBudgetManager
from harness.guardrails.safety_mixin import SafetyMixin
from harness.risk import RiskAssessment, RiskAssessor, RiskFactor, RiskLevel
from kernel import Flow, create_flow
from kernel.agents.agent_factory import AgentFactorySpec
from kernel.agents.agent_factory import create as create_agent


logger = logging.getLogger(__name__)

# ============================================================
# Flow 作成（FlowBuilder パターン）
# ============================================================


def _safe_get_result(ctx: Any, node_id: str) -> dict[str, Any]:
    """Phase 13: エラー回復付きの結果取得."""
    try:
        result = ctx.get_result(node_id)
        return result if isinstance(result, dict) else {}
    except Exception as e:
        logger.warning("ノード '%s' の結果取得失敗: %s", node_id, e)
        return {}


def _map_evidence_input(ctx: Any) -> dict[str, Any]:
    """証拠台帳の入力を構築."""
    collector = _safe_get_result(ctx, "collector")
    return {"articles": collector.get("articles", [])}


def _map_analyzer_input(ctx: Any) -> dict[str, Any]:
    """分析の入力を構築."""
    collector = _safe_get_result(ctx, "collector")
    return {
        "articles": collector.get("articles", []),
        "enable_sentiment": config.analyzer.enable_sentiment_analysis,
    }


def _map_signal_input(ctx: Any) -> dict[str, Any]:
    """信号評価の入力を構築."""
    analyzer = _safe_get_result(ctx, "analyzer")
    collector = _safe_get_result(ctx, "collector")
    trends = analyzer.get("trends", [])
    articles = collector.get("articles", [])

    evidence_counts: dict[str, int] = {}
    source_types: dict[str, list[str]] = {}

    for trend in trends:
        trend_id = trend.get("id", "")
        topic = trend.get("topic", "")
        matched = [article for article in articles if topic in article.get("keywords", [])]
        evidence_counts[trend_id] = len(matched) if matched else int(trend.get("articles_count", 0))
        sources = {article.get("source") for article in matched if article.get("source")}
        if not sources:
            sources = {article.get("source") for article in articles if article.get("source")}
        source_types[trend_id] = sorted(sources)

    return {
        "trends": trends,
        "evidence_counts": evidence_counts,
        "source_types": source_types,
    }


def _map_reporter_input(ctx: Any) -> dict[str, Any]:
    """レポート生成の入力を構築."""
    analyzer = _safe_get_result(ctx, "analyzer")
    return {
        "trends": analyzer.get("trends", []),
        "summary": analyzer.get("summary", ""),
        "period": datetime.now().strftime("%Y-W%U"),
    }


def _map_redteam_input(ctx: Any) -> dict[str, Any]:
    """Red Team の入力を構築."""
    analyzer = _safe_get_result(ctx, "analyzer")
    trends = analyzer.get("trends", [])
    now = datetime.now().isoformat()

    claims: list[dict[str, Any]] = []
    for trend in trends:
        trend_id = trend.get("id", "")
        claims.append(
            {
                "id": f"claim-{trend_id}",
                "statement": f"トレンド「{trend.get('topic', '')}」が重要である",
                "level": "hypothesis",
                "confidence": float(trend.get("score", 0.0)),
                "evidence_ids": [],
                "counter_evidence_ids": [],
                "created_at": now,
                "updated_at": now,
                "metadata": {"trend_id": trend_id},
            }
        )

    return {"claims": claims}


def _map_notifier_input(ctx: Any) -> dict[str, Any]:
    """通知の入力を構築."""
    analyzer = _safe_get_result(ctx, "analyzer")
    return {
        "trends": analyzer.get("trends", []),
        "alert_threshold": config.notifier.alert_growth_rate_threshold,
    }


flow: Flow = (
    create_flow("market-trend-monitor", name="Market Trend Monitor")
    .then(
        create_agent(
            AgentFactorySpec(
                agent_class=CollectorAgent,
                init_kwargs={"source_reliability_tracker": source_reliability_tracker},
                agent_type="executor",
            )
        ),
        create_agent(
            AgentFactorySpec(
                agent_class=EvidenceLedgerAgent,
                init_kwargs={"evidence_service": evidence_service},
                agent_type="executor",
            )
        ),
        create_agent(
            AgentFactorySpec(
                agent_class=AnalyzerAgent,
                init_kwargs={"evidence_service": evidence_service},
                agent_type="reactor",
            )
        ),
        create_agent(
            AgentFactorySpec(
                agent_class=SignalScorerAgent,
                init_kwargs={"signal_service": signal_service},
                agent_type="reviewer",
            )
        ),
        create_agent(AgentFactorySpec(agent_class=ReporterAgent, agent_type="reporter")),
        create_agent(AgentFactorySpec(agent_class=RedTeamAgent, agent_type="reviewer")),
        create_agent(AgentFactorySpec(agent_class=NotifierAgent, agent_type="executor")),
        ids=[
            "collector",
            "evidence_ledger",
            "analyzer",
            "signal_scorer",
            "reporter",
            "red_team",
            "notifier",
        ],
        input_mappers={
            "evidence_ledger": _map_evidence_input,
            "analyzer": _map_analyzer_input,
            "signal_scorer": _map_signal_input,
            "reporter": _map_reporter_input,
            "red_team": _map_redteam_input,
            "notifier": _map_notifier_input,
        },
    )
    .build()
)


# ============================================================
# 便利関数
# ============================================================


async def run(input_data: dict[str, Any] | None = None) -> dict[str, Any]:
    """ワークフローを実行.

    Args:
        input_data: 入力データ（オプション）

    Returns:
        実行結果（NotifierAgentの出力）
    """
    if input_data is None:
        input_data = {
            "keywords": config.collector.keywords,
            "sources": config.collector.sources,
        }

    logger.info("市場動向ワークフローを開始")
    metrics_service.start_run()
    try:
        result = await flow.run(input_data)
        metrics_service.end_run(success=True)
        logger.info("市場動向ワークフローが完了")
        return result
    except Exception as exc:
        metrics_service.end_run(success=False, error=str(exc))
        raise


async def run_stream(
    input_data: dict[str, Any] | None = None,
) -> AsyncIterator[dict[str, Any]]:
    """ストリームモードで実行（SSE用）.

    Yields:
        イベント: node_start, node_complete, result
    """
    if input_data is None:
        input_data = {
            "keywords": config.collector.keywords,
            "sources": config.collector.sources,
        }

    async for event in flow.run_stream(input_data):
        yield event


async def cleanup() -> None:
    """クリーンアップ."""
    await flow.cleanup()


class MarketTrendWorkflow(SafetyMixin):
    """Market Trend Monitor ワークフロー（安全防護・Budget管理・Risk評価付き）."""

    def __init__(
        self,
        f: Flow | None = None,
        enable_safety: bool = True,
        budget_config: BudgetConfig | None = None,
    ) -> None:
        self._flow = f or flow
        self.init_safety(enabled=enable_safety)
        # TokenBudgetManager: 入力キーワード・コンテキストのToken予算管理（手動トリガー）
        self._budget_manager = TokenBudgetManager(
            config=budget_config
            or BudgetConfig(
                system_prompt_budget=500,
                rag_context_budget=2000,
                history_budget=2000,
                total_budget=6000,
            )
        )
        # RiskAssessor: 最終レポート配信前のリスク評価（手動トリガー）
        self._risk_assessor = RiskAssessor(threshold=RiskLevel.HIGH)

    def _assess_workflow_risk(self, result: dict[str, Any]) -> RiskAssessment:
        """ワークフロー結果のリスク評価.

        Args:
            result: ワークフロー実行結果

        Returns:
            リスク評価結果
        """
        factors: list[RiskFactor] = []

        # 安全警告がある場合はリスク要因として登録
        safety_warnings = result.get("_safety_warnings", [])
        if safety_warnings:
            factors.append(
                RiskFactor(
                    name="hallucination_risk",
                    level=RiskLevel.HIGH,
                    description=f"幻覚検出警告: {len(safety_warnings)}件",
                    score=0.8,
                )
            )

        # トレンド数が0の場合はデータ品質リスク
        trends = result.get("trends", [])
        if not trends:
            factors.append(
                RiskFactor(
                    name="no_trends_detected",
                    level=RiskLevel.MEDIUM,
                    description="トレンドが検出されませんでした。データ収集に問題がある可能性。",
                    score=0.5,
                )
            )

        # アラートが高スコアで検出された場合
        alerts = result.get("alerts", [])
        high_alerts = [a for a in alerts if isinstance(a, dict) and a.get("growth_rate", 0) > 2.0]
        if high_alerts:
            factors.append(
                RiskFactor(
                    name="high_growth_rate_alert",
                    level=RiskLevel.MEDIUM,
                    description=f"急成長トレンド検出: {len(high_alerts)}件（要注意）",
                    score=0.4,
                )
            )

        return self._risk_assessor.assess(factors)

    async def run(self, input_data: dict[str, Any] | None = None) -> dict[str, Any]:
        """ワークフローを実行（Budget制約なし、基本モード）."""
        return await run(input_data)

    async def run_with_safety(
        self,
        input_data: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """安全検査・Budget管理・Risk評価付きで実行.

        フロー:
        1. 入力キーワードのToken予算チェック（Budget自動管理）
        2. 入力の安全検査（SafetyMixin）
        3. ワークフロー実行
        4. 出力の幻覚検出（SafetyMixin）
        5. リスク評価（RiskAssessor）
        """
        if input_data is None:
            input_data = {}

        # --- TokenBudgetManager: 入力コンテキストのToken予算確認 ---
        keywords: list[str] = input_data.get("keywords", [])
        if keywords:
            keywords_text = ", ".join(keywords)
            allocation = self._budget_manager.allocate_rag_context([keywords_text])
            if allocation.truncated:
                logger.warning(
                    "入力キーワードがRAG予算を超過（%d/%d token）。切り詰めを適用。",
                    allocation.original_token_count,
                    allocation.budget_used,
                )

        # --- SafetyMixin: 入力安全検査 ---
        if keywords and self.safety_enabled:
            for keyword in keywords:
                check = await self.check_input_safety(keyword)
                if not check.is_safe:
                    logger.warning("入力キーワードに安全上の問題: %s", keyword)

        result = await run(input_data)

        # --- SafetyMixin: 出力幻覚検出 ---
        if self.safety_enabled and "report" in result:
            report_text = str(result.get("report", ""))
            output_check = await self.check_output_safety(report_text)
            if output_check.needs_review:
                result["_safety_warnings"] = output_check.issues
                logger.warning("出力に幻覚の可能性: %s", output_check.issues)

        # --- RiskAssessor: 最終レポートのリスク評価 ---
        risk_assessment = self._assess_workflow_risk(result)
        result["_risk_assessment"] = {
            "level": risk_assessment.overall_level.value,
            "is_acceptable": risk_assessment.is_acceptable,
            "factors": [
                {"name": f.name, "level": f.level.value, "description": f.description} for f in risk_assessment.factors
            ],
            "mitigations": risk_assessment.mitigations,
        }
        if not risk_assessment.is_acceptable:
            logger.warning(
                "ワークフロー結果のリスクレベルが閾値超過: %s（要レビュー）",
                risk_assessment.overall_level.value,
            )

        # --- TokenBudgetManager: 使用量サマリをログ出力 ---
        budget_summary = self._budget_manager.get_usage_summary()
        logger.debug("Token予算使用サマリ: %s", budget_summary)
        self._budget_manager.reset_usage()

        return result

    async def initialize(self) -> None:
        pass  # auto_initialize=True なので不要

    async def cleanup(self) -> None:
        await cleanup()

    @property
    def flow(self) -> Flow:
        return self._flow

    @property
    def budget_manager(self) -> TokenBudgetManager:
        """TokenBudgetManagerへのアクセス（テスト・外部利用用）."""
        return self._budget_manager

    @property
    def risk_assessor(self) -> RiskAssessor:
        """RiskAssessorへのアクセス（テスト・外部利用用）."""
        return self._risk_assessor


workflow = MarketTrendWorkflow(flow)
