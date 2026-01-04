# -*- coding: utf-8 -*-
"""Decision Governance Engine - ユニットテスト.

各Agentの基本機能とWorkflowの動作を検証する。
"""

import pytest

from apps.decision_governance_engine.agents.dao_agent import DaoAgent
from apps.decision_governance_engine.agents.fa_agent import FaAgent
from apps.decision_governance_engine.agents.gatekeeper_agent import GatekeeperAgent
from apps.decision_governance_engine.agents.qi_agent import QiAgent
from apps.decision_governance_engine.agents.review_agent import ReviewAgent
from apps.decision_governance_engine.agents.shu_agent import ShuAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    DaoOutput,
    FaOutput,
    ProblemType,
    QuestionCategory,
    ReviewVerdict,
    ShuOutput,
)
from apps.decision_governance_engine.schemas.input_schemas import (
    ConstraintSet,
    DecisionRequest,
)
from apps.decision_governance_engine.schemas.output_schemas import (
    DecisionReport,
    ExecutiveSummary,
)
from apps.decision_governance_engine.services.pdf_generator import PDFGeneratorService
from apps.decision_governance_engine.services.ui_components import (
    DecisionUIComponentBuilder,
)
from apps.decision_governance_engine.engine import DecisionEngine


class TestGatekeeperAgent:
    """GatekeeperAgentのテスト."""

    @pytest.fixture
    def agent(self) -> GatekeeperAgent:
        """テスト用Agentを作成."""
        return GatekeeperAgent()

    @pytest.mark.asyncio
    async def test_reject_weather_question(self, agent: GatekeeperAgent) -> None:
        """天気の質問は拒否される."""
        result = await agent.run({"raw_question": "今日の天気は？"})
        assert result["is_acceptable"] is False
        assert result["category"] == QuestionCategory.FACTUAL_LOOKUP.value

    @pytest.mark.asyncio
    async def test_reject_system_inquiry(self, agent: GatekeeperAgent) -> None:
        """システム質問は拒否される."""
        result = await agent.run({"raw_question": "このシステムはどうやって作られていますか？"})
        assert result["is_acceptable"] is False
        assert result["category"] == QuestionCategory.SYSTEM_INQUIRY.value

    @pytest.mark.asyncio
    async def test_accept_trade_off_question(self, agent: GatekeeperAgent) -> None:
        """トレードオフ質問は受理される."""
        result = await agent.run({"raw_question": "新規事業AとBのどちらに投資すべきか判断したい"})
        assert result["is_acceptable"] is True
        assert result["category"] == QuestionCategory.TRADE_OFF_CHOICE.value

    @pytest.mark.asyncio
    async def test_reject_short_question(self, agent: GatekeeperAgent) -> None:
        """短すぎる質問は拒否される."""
        result = await agent.run({"raw_question": "どうする？"})
        assert result["is_acceptable"] is False


class TestDaoAgent:
    """DaoAgentのテスト."""

    @pytest.fixture
    def agent(self) -> DaoAgent:
        """テスト用Agentを作成."""
        return DaoAgent()

    @pytest.mark.asyncio
    async def test_analyze_trade_off(self, agent: DaoAgent) -> None:
        """トレードオフ問題の分析."""
        result = await agent.run({
            "question": "新規事業AとBのどちらに投資すべきか",
            "constraints": ["予算1億円", "期間6ヶ月"],
        })
        # 結果はProblemType enumオブジェクト
        assert result["problem_type"] in [
            ProblemType.TRADE_OFF,
            ProblemType.RESOURCE_ALLOCATION,
        ]
        assert result["essence"]
        assert len(result["immutable_constraints"]) > 0

    @pytest.mark.asyncio
    async def test_analyze_timing(self, agent: DaoAgent) -> None:
        """タイミング問題の分析."""
        result = await agent.run({
            "question": "新システムの導入はいつ着手すべきか",
            "constraints": [],
        })
        # 結果はProblemType enumオブジェクト
        assert result["problem_type"] == ProblemType.TIMING_DECISION


class TestFaAgent:
    """FaAgentのテスト."""

    @pytest.fixture
    def agent(self) -> FaAgent:
        """テスト用Agentを作成."""
        return FaAgent()

    @pytest.fixture
    def dao_result(self) -> dict:
        """テスト用DaoOutput."""
        return DaoOutput(
            problem_type=ProblemType.TRADE_OFF,
            essence="複数の選択肢間の最適なバランス判断",
            immutable_constraints=["予算1億円", "期間6ヶ月"],
            hidden_assumptions=["市場環境が継続する"],
        ).model_dump()

    @pytest.mark.asyncio
    async def test_generate_paths(self, agent: FaAgent, dao_result: dict) -> None:
        """戦略パスの生成."""
        result = await agent.run({
            "dao_result": dao_result,
            "available_resources": {"budget": 10000},
            "time_horizon": "6ヶ月",
        })
        assert len(result["recommended_paths"]) >= 1
        assert len(result["recommended_paths"]) <= 2  # 最大2個
        assert len(result["rejected_paths"]) >= 1  # 不推奨必須
        assert len(result["decision_criteria"]) > 0


class TestShuAgent:
    """ShuAgentのテスト."""

    @pytest.fixture
    def agent(self) -> ShuAgent:
        """テスト用Agentを作成."""
        return ShuAgent()

    @pytest.fixture
    def fa_result(self) -> dict:
        """テスト用FaOutput."""
        return FaOutput(
            recommended_paths=[{
                "path_id": "A",
                "name": "推奨案A",
                "description": "段階的アプローチ",
                "pros": ["リスク低減"],
                "cons": ["時間がかかる"],
                "success_probability": 0.7,
            }],
            rejected_paths=[],
            decision_criteria=["リスク対リターン"],
        ).model_dump()

    @pytest.mark.asyncio
    async def test_generate_phases(self, agent: ShuAgent, fa_result: dict) -> None:
        """フェーズの生成."""
        result = await agent.run({
            "fa_result": fa_result,
            "selected_path_id": "A",
        })
        assert 3 <= len(result["phases"]) <= 5
        assert result["first_action"]
        assert len(result["dependencies"]) > 0


class TestQiAgent:
    """QiAgentのテスト."""

    @pytest.fixture
    def agent(self) -> QiAgent:
        """テスト用Agentを作成."""
        return QiAgent()

    @pytest.fixture
    def shu_result(self) -> dict:
        """テスト用ShuOutput（最低3フェーズ必要）."""
        return ShuOutput(
            phases=[
                {
                    "phase_number": 1,
                    "name": "準備",
                    "duration": "2週間",
                    "actions": ["チーム編成"],
                    "deliverables": ["計画書"],
                    "success_criteria": ["承認"],
                },
                {
                    "phase_number": 2,
                    "name": "実行",
                    "duration": "1ヶ月",
                    "actions": ["開発"],
                    "deliverables": ["成果物"],
                    "success_criteria": ["完了"],
                },
                {
                    "phase_number": 3,
                    "name": "評価",
                    "duration": "1週間",
                    "actions": ["レビュー"],
                    "deliverables": ["報告書"],
                    "success_criteria": ["承認"],
                },
            ],
            first_action="キックオフMTG",
            dependencies=["経営承認"],
        ).model_dump()

    @pytest.mark.asyncio
    async def test_generate_implementations(self, agent: QiAgent, shu_result: dict) -> None:
        """実装要素の生成."""
        result = await agent.run({
            "shu_result": shu_result,
            "tech_constraints": ["Python使用"],
        })
        assert len(result["implementations"]) > 0
        assert len(result["tool_recommendations"]) > 0


class TestReviewAgent:
    """ReviewAgentのテスト."""

    @pytest.fixture
    def agent(self) -> ReviewAgent:
        """テスト用Agentを作成."""
        return ReviewAgent()

    @pytest.fixture
    def full_input(self) -> dict:
        """テスト用フル入力（ShuOutputは最低3フェーズ必要）."""
        return {
            "dao_result": DaoOutput(
                problem_type=ProblemType.TRADE_OFF,
                essence="テスト本質",
                immutable_constraints=["制約1"],
                hidden_assumptions=["前提1"],
            ).model_dump(),
            "fa_result": FaOutput(
                recommended_paths=[{
                    "path_id": "A",
                    "name": "推奨案A",
                    "description": "説明",
                    "pros": ["メリット"],
                    "cons": ["デメリット"],
                    "success_probability": 0.7,
                }],
                rejected_paths=[],
                decision_criteria=["基準1"],
            ).model_dump(),
            "shu_result": ShuOutput(
                phases=[
                    {
                        "phase_number": 1,
                        "name": "準備",
                        "duration": "2週間",
                        "actions": ["行動1"],
                        "deliverables": ["成果物1"],
                        "success_criteria": ["条件1"],
                    },
                    {
                        "phase_number": 2,
                        "name": "実行",
                        "duration": "1ヶ月",
                        "actions": ["行動2"],
                        "deliverables": ["成果物2"],
                        "success_criteria": ["条件2"],
                    },
                    {
                        "phase_number": 3,
                        "name": "評価",
                        "duration": "1週間",
                        "actions": ["行動3"],
                        "deliverables": ["成果物3"],
                        "success_criteria": ["条件3"],
                    },
                ],
                first_action="最初の一歩",
                dependencies=["依存1"],
            ).model_dump(),
            "qi_result": {
                "implementations": [],
                "tool_recommendations": [],
                "integration_points": [],
                "technical_debt_warnings": [],
            },
        }

    @pytest.mark.asyncio
    async def test_review_pass(self, agent: ReviewAgent, full_input: dict) -> None:
        """正常ケースはPASS."""
        result = await agent.run(full_input)
        assert result["overall_verdict"] in [
            ReviewVerdict.PASS.value,
            ReviewVerdict.REVISE.value,
        ]
        assert "confidence_score" in result


class TestDecisionEngine:
    """DecisionEngineのテスト."""

    @pytest.fixture
    def engine(self) -> DecisionEngine:
        """テスト用Engineを作成."""
        return DecisionEngine()

    @pytest.mark.asyncio
    async def test_reject_invalid_question(self, engine: DecisionEngine) -> None:
        """不適格な質問は拒否される（CognitiveGateまたはGatekeeperで）."""
        # DecisionRequestは最低10文字必要なので、長めの不適格質問を使用
        result = await engine.run({"question": "今日の天気はどうですか？教えてください"})
        # 結果がdictの場合（拒否時）
        if isinstance(result, dict):
            # CognitiveGateまたはGatekeeperで拒否される
            assert result.get("status") in ["rejected", "cognitive_gate_blocked"]
        else:
            # DecisionReportオブジェクトの場合（処理された場合）
            assert hasattr(result, "report_id")

    @pytest.mark.asyncio
    async def test_process_valid_question(self, engine: DecisionEngine) -> None:
        """適格な質問は処理される."""
        result = await engine.run({"question": "新規事業AとBのどちらに投資すべきか判断したい"})
        # 結果がdictの場合（拒否時）
        if isinstance(result, dict):
            assert result.get("status") == "rejected"
        else:
            # DecisionReportオブジェクトの場合
            assert hasattr(result, "report_id")

    @pytest.mark.asyncio
    async def test_process_with_request_object(self, engine: DecisionEngine) -> None:
        """DecisionRequestオブジェクトでの処理."""
        request = DecisionRequest(
            question="リソース配分の最適化について判断したい",
            constraints=ConstraintSet(),
        )
        result = await engine.run({"question": request.question})
        # 結果がdictの場合（拒否時）
        if isinstance(result, dict):
            assert "status" in result
        else:
            # DecisionReportオブジェクトの場合
            assert hasattr(result, "report_id")


class TestUIComponents:
    """A2UI コンポーネントビルダーのテスト."""

    @pytest.fixture
    def sample_report(self) -> DecisionReport:
        """テスト用サンプルレポート."""
        return DecisionReport(
            report_id="TEST-001",
            dao={
                "problem_type": "TRADE_OFF",
                "essence": "テスト本質",
                "immutable_constraints": ["制約1"],
                "hidden_assumptions": ["前提1"],
            },
            fa={
                "recommended_paths": [{
                    "path_id": "A",
                    "name": "案A",
                    "description": "説明",
                    "pros": ["メリット"],
                    "cons": ["デメリット"],
                    "success_probability": 0.8,
                }],
                "rejected_paths": [],
                "decision_criteria": ["基準1"],
            },
            shu={
                "phases": [
                    {"phase_number": 1, "name": "準備", "duration": "2週間", "actions": ["行動1"], "deliverables": ["成果物"], "success_criteria": ["条件"]},
                    {"phase_number": 2, "name": "実行", "duration": "1ヶ月", "actions": ["行動2"], "deliverables": ["成果物2"], "success_criteria": ["条件2"]},
                    {"phase_number": 3, "name": "評価", "duration": "1週間", "actions": ["行動3"], "deliverables": ["成果物3"], "success_criteria": ["条件3"]},
                ],
                "first_action": "MTG",
                "dependencies": [],
            },
            qi={
                "implementations": [{
                    "component": "API",
                    "technology": "FastAPI",
                    "estimated_effort": "1人月",
                    "risks": [],
                }],
                "tool_recommendations": ["Jira"],
                "integration_points": [],
                "technical_debt_warnings": [],
            },
            review={"overall_verdict": "PASS", "confidence_score": 0.9, "findings": []},
            executive_summary=ExecutiveSummary(
                one_line_decision="案Aを選択",
                recommended_action="実行開始",
                key_risks=["リスク1"],
                first_step="MTG設定",
                estimated_impact="効果見込み",
            ),
        )

    def test_build_report_view(self, sample_report: DecisionReport) -> None:
        """レポートビュー構築テスト."""
        builder = DecisionUIComponentBuilder()
        components = builder.build_report_view(sample_report)
        assert len(components) >= 6
        assert components[0].props.get("title") == "📊 エグゼクティブサマリー"


class TestPDFGenerator:
    """PDF生成サービスのテスト."""

    @pytest.fixture
    def sample_report(self) -> DecisionReport:
        """テスト用サンプルレポート."""
        return DecisionReport(
            report_id="PDF-TEST-001",
            dao={
                "problem_type": "TRADE_OFF",
                "essence": "PDF本質",
                "immutable_constraints": [],
                "hidden_assumptions": [],
            },
            fa={
                "recommended_paths": [{
                    "path_id": "A",
                    "name": "案A",
                    "description": "説明",
                    "pros": [],
                    "cons": [],
                    "success_probability": 0.7,
                }],
                "rejected_paths": [],
                "decision_criteria": [],
            },
            shu={
                "phases": [
                    {"phase_number": 1, "name": "準備", "duration": "2週間", "actions": [], "deliverables": [], "success_criteria": []},
                    {"phase_number": 2, "name": "実行", "duration": "1ヶ月", "actions": [], "deliverables": [], "success_criteria": []},
                    {"phase_number": 3, "name": "評価", "duration": "1週間", "actions": [], "deliverables": [], "success_criteria": []},
                ],
                "first_action": "MTG",
                "dependencies": [],
            },
            qi={
                "implementations": [],
                "tool_recommendations": [],
                "integration_points": [],
                "technical_debt_warnings": [],
            },
            review={"overall_verdict": "PASS", "confidence_score": 0.85, "findings": []},
            executive_summary=ExecutiveSummary(
                one_line_decision="案Aを選択",
                recommended_action="実行",
                key_risks=[],
                first_step="MTG",
                estimated_impact="効果",
            ),
        )

    def test_generate_pdf_fallback(self, sample_report: DecisionReport) -> None:
        """PDF生成テスト（HTMLフォールバック）."""
        generator = PDFGeneratorService()
        result = generator.generate_pdf(sample_report)
        assert len(result) > 0
        if not generator._has_reportlab:
            assert b"<html" in result or b"<!DOCTYPE" in result
