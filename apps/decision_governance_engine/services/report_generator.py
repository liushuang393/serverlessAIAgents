"""ReportGenerator - 提案書生成サービス.

目的:
- Agent実行結果から最終提案書を生成
- ExecutiveSummary、タイトル、署名欄を自動生成
- PDF/Word出力対応（将来拡張）

使用例:
    >>> from apps.decision_governance_engine.services.report_generator import ReportGenerator
    >>>
    >>> generator = ReportGenerator()
    >>> report = generator.generate(results, original_question="新規事業への投資判断")
"""

import logging
from typing import Any

from apps.decision_governance_engine.schemas.output_schemas import (
    DecisionReport,
    ExecutiveSummary,
    generate_proposal_title,
    generate_signature_block,
)
from apps.decision_governance_engine.services.human_review_policy import (
    enrich_review_with_policy,
)


class ReportGenerator:
    """提案書生成サービス.

    Agent実行結果から最終的なDecisionReportを生成。
    ExecutiveSummary、タイトル、署名欄を自動生成。

    使用例:
        >>> generator = ReportGenerator()
        >>> results = {
        ...     "dao": dao_result,
        ...     "fa": fa_result,
        ...     "shu": shu_result,
        ...     "qi": qi_result,
        ...     "review": review_result,
        ... }
        >>> report = generator.generate(
        ...     results,
        ...     original_question="新規事業への投資判断",
        ...     clarification_result=clarification_output,
        ... )
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger("decision_engine.report_generator")

    def generate(
        self,
        results: dict[str, dict[str, Any]],
        original_question: str = "",
        clarification_result: dict[str, Any] | None = None,
        user_info: dict[str, Any] | None = None,
    ) -> DecisionReport:
        """最終提案書を生成.

        Args:
            results: 各Agentの実行結果（dao, fa, shu, qi, review）
            original_question: 元の質問文（タイトル生成用）
            clarification_result: 診断結果（オプション）
            user_info: ユーザー情報（署名欄用）

        Returns:
            DecisionReport: 提案書
        """
        self._logger.info("Generating decision report...")

        dao_result = results.get("dao", {})
        fa_result = results.get("fa", {})
        shu_result = results.get("shu", {})
        qi_result = results.get("qi", {})
        review_result = enrich_review_with_policy(results.get("review", {}))

        # ExecutiveSummary生成
        summary = self._generate_executive_summary(
            dao_result, fa_result, shu_result
        )

        # 提案書タイトル自動生成
        problem_type = dao_result.get("problem_type", "")
        if hasattr(problem_type, "value"):
            problem_type = problem_type.value
        proposal_title = generate_proposal_title(original_question, problem_type)

        # 署名欄自動生成
        signature_block = generate_signature_block(user_info)

        # 案件IDをレポートIDとして使用
        report_id = proposal_title.case_id

        return DecisionReport(
            report_id=report_id,
            proposal_title=proposal_title,
            original_question=original_question,
            signature_block=signature_block,
            clarification=clarification_result or {},
            dao=dao_result,
            fa=fa_result,
            shu=shu_result,
            qi=qi_result,
            review=review_result,
            executive_summary=summary,
        )

    def _generate_executive_summary(
        self,
        dao_result: dict[str, Any],
        fa_result: dict[str, Any],
        shu_result: dict[str, Any],
    ) -> ExecutiveSummary:
        """ExecutiveSummaryを生成.

        Args:
            dao_result: 道（本質分析）結果
            fa_result: 法（戦略選定）結果
            shu_result: 術（実行計画）結果

        Returns:
            ExecutiveSummary
        """
        recommended = fa_result.get("recommended_paths", [{}])[0]

        # 死穴から主要リスクを抽出
        death_traps = dao_result.get("death_traps", [])
        key_risks = list(recommended.get("cons", [])[:2])
        # 死穴があれば追加
        if death_traps:
            key_risks.append(f"⚠️ {death_traps[0].get('action', '禁忌行動あり')}")

        # 本質の一文を取得（essence_derivationから優先）
        essence_statement = ""
        essence_derivation = dao_result.get("essence_derivation", {})
        if essence_derivation:
            essence_statement = essence_derivation.get("essence_statement", "")
        if not essence_statement:
            essence_statement = dao_result.get("essence", "")

        # 戦略的禁止事項サマリーを取得
        strategic_prohibition_summary = ""
        strategic_prohibitions = fa_result.get("strategic_prohibitions", [])
        if strategic_prohibitions:
            strategic_prohibition_summary = strategic_prohibitions[0].get(
                "prohibition", ""
            )

        # 撤退基準サマリーを取得
        exit_criteria_summary = ""
        exit_criteria = shu_result.get("exit_criteria", {})
        if exit_criteria:
            exit_criteria_summary = (
                f"{exit_criteria.get('checkpoint', '')}: "
                f"{exit_criteria.get('exit_trigger', '')}"
            )

        return ExecutiveSummary(
            one_line_decision=f"{recommended.get('name', '推奨案')}を選択すべき"[:30],
            recommended_action=recommended.get(
                "description", "詳細は法セクション参照"
            ),
            key_risks=key_risks[:3],
            first_step=shu_result.get("first_action", "キックオフMTG設定"),
            estimated_impact="計画実行により目標達成を見込む",
            essence_statement=essence_statement[:50],
            strategic_prohibition_summary=strategic_prohibition_summary[:50],
            exit_criteria_summary=exit_criteria_summary[:50],
        )

    def generate_from_context(
        self,
        context: Any,
        original_question: str = "",
        user_info: dict[str, Any] | None = None,
    ) -> DecisionReport:
        """SharedContextから提案書を生成.

        Args:
            context: SharedContext インスタンス
            original_question: 元の質問文
            user_info: ユーザー情報

        Returns:
            DecisionReport
        """
        results = {
            "dao": context.get("dao_result", {}),
            "fa": context.get("fa_result", {}),
            "shu": context.get("shu_result", {}),
            "qi": context.get("qi_result", {}),
            "review": context.get("review_result", {}),
        }
        clarification_result = context.get("clarification_result")

        return self.generate(
            results=results,
            original_question=original_question,
            clarification_result=clarification_result,
            user_info=user_info,
        )


__all__ = ["ReportGenerator"]
