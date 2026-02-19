"""PDF出力サービス v3.0.

DecisionReportをPDF形式でエクスポートする。
署名欄付きの企業向けレポート出力。
v3.0: 本質導出・戦略的禁止事項・撤退基準など全フィールド対応。
"""

import io
import logging
from typing import Any

from apps.decision_governance_engine.schemas.output_schemas import DecisionReport


class PDFGeneratorService:
    """Decision レポート PDF 生成サービス v3.0.

    DecisionReportを署名可能なPDF形式に変換。
    v3.0: 全ての道・法・術・器フィールドを出力。
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger("pdf_generator")
        self._has_reportlab = self._check_reportlab()

    def _check_reportlab(self) -> bool:
        """ReportLabが利用可能か確認."""
        try:
            from reportlab.lib.pagesizes import A4

            return True
        except ImportError:
            self._logger.warning("ReportLab not installed. PDF export is unavailable.")
            return False

    def generate_pdf(self, report: DecisionReport) -> bytes:
        """PDFを生成.

        Args:
            report: 決策レポート

        Returns:
            PDFバイナリデータ

        Raises:
            ValueError: レポートが None または不正な場合
            RuntimeError: PDF生成に失敗した場合

        注意:
            - システム理念「変数・返回値強化」に基づき、入力検証を実施
        """
        if report is None:
            raise ValueError("report cannot be None")

        if not self._has_reportlab:
            raise RuntimeError("ReportLab が未インストールのため PDF 出力できません")

        try:
            return self._generate_with_reportlab(report)
        except Exception as e:
            self._logger.error(
                f"PDF generation failed: {type(e).__name__}: {e}",
                exc_info=True,
            )
            raise RuntimeError(f"PDF生成に失敗しました: {e}") from e

    def generate_html(self, report: DecisionReport) -> bytes:
        """HTMLを生成."""
        if report is None:
            raise ValueError("report cannot be None")
        return self._generate_html_fallback(report)

    def _to_dict(self, obj: Any) -> dict:
        """Pydanticオブジェクトまたはdictをdictに変換.

        Args:
            obj: 変換対象オブジェクト（Pydanticモデル、dict、またはNone）

        Returns:
            dict: 変換後の辞書（変換不可の場合は空辞書）

        注意:
            - システム理念「変数・返回値強化」に基づき、None や予期しない型を安全に処理
        """
        if obj is None:
            return {}
        if hasattr(obj, "model_dump"):
            try:
                return obj.model_dump()
            except Exception as e:
                self._logger.warning(f"Failed to dump Pydantic model: {type(obj).__name__} - {e}")
                return {}
        if isinstance(obj, dict):
            return obj
        # 予期しない型の場合
        self._logger.warning(f"Unexpected type in _to_dict: {type(obj).__name__}")
        return {}

    def _generate_with_reportlab(self, report: DecisionReport) -> bytes:
        """ReportLabでPDF生成 v3.0（CJK対応・全フィールド出力）."""
        from reportlab.lib import colors
        from reportlab.lib.pagesizes import A4
        from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
        from reportlab.lib.units import cm
        from reportlab.pdfbase import pdfmetrics
        from reportlab.pdfbase.cidfonts import UnicodeCIDFont
        from reportlab.platypus import (
            PageBreak,
            Paragraph,
            SimpleDocTemplate,
            Spacer,
            Table,
            TableStyle,
        )

        # CJKフォント登録（中日韓文字対応）
        pdfmetrics.registerFont(UnicodeCIDFont("HeiseiMin-W3"))
        cjk_font = "HeiseiMin-W3"

        buffer = io.BytesIO()
        doc = SimpleDocTemplate(buffer, pagesize=A4, topMargin=2 * cm, bottomMargin=2 * cm)
        styles = getSampleStyleSheet()
        elements: list[Any] = []

        # dictに変換
        dao = self._to_dict(report.dao)
        fa = self._to_dict(report.fa)
        shu = self._to_dict(report.shu)
        qi = self._to_dict(report.qi)
        review = self._to_dict(report.review)

        # 表紙情報（proposal_title / signature_block）
        proposal_title = self._to_dict(report.proposal_title) if report.proposal_title else {}
        title_ja = proposal_title.get("title_ja", "提案書")
        title_en = proposal_title.get("title_en", "")
        case_id = proposal_title.get("case_id", report.report_id)
        subtitle = proposal_title.get("subtitle", "")

        sig_block = self._to_dict(report.signature_block) if report.signature_block else {}
        author_name = sig_block.get("author_name", "")
        author_dept = sig_block.get("author_department", "")
        author_pos = sig_block.get("author_position", "")
        created_date = sig_block.get("created_date", report.created_at.strftime("%Y年%m月%d日"))

        # CJK対応スタイル
        title_style = ParagraphStyle(
            "CJKTitle", parent=styles["Title"], fontSize=18, fontName=cjk_font
        )
        heading_style = ParagraphStyle(
            "CJKHeading", parent=styles["Heading2"], fontName=cjk_font, spaceAfter=10
        )
        subheading_style = ParagraphStyle(
            "CJKSubHeading", parent=styles["Heading3"], fontName=cjk_font, fontSize=11
        )
        normal_style = ParagraphStyle(
            "CJKNormal", parent=styles["Normal"], fontName=cjk_font, spaceBefore=3, spaceAfter=3
        )
        highlight_style = ParagraphStyle(
            "CJKHighlight", parent=normal_style, backColor=colors.Color(0.9, 0.95, 1)
        )
        warning_style = ParagraphStyle(
            "CJKWarning", parent=normal_style, backColor=colors.Color(1, 0.95, 0.9)
        )

        # ========== タイトル ==========
        elements.append(Paragraph(title_ja, title_style))
        if title_en:
            elements.append(Paragraph(title_en, normal_style))
        if subtitle:
            elements.append(Paragraph(subtitle, normal_style))
        elements.append(Paragraph(f"案件ID: {case_id}", normal_style))
        if report.original_question:
            elements.append(Paragraph(f"対象質問: {report.original_question}", normal_style))
        elements.append(
            Paragraph(
                f"作成日: {created_date} | 生成日時: {report.created_at.strftime('%Y-%m-%d %H:%M')} | Version: {report.version}",
                normal_style,
            )
        )
        if any([author_dept, author_pos, author_name]):
            elements.append(
                Paragraph(f"作成: {author_dept} {author_pos} {author_name}".strip(), normal_style)
            )
        elements.append(Spacer(1, 0.5 * cm))

        # ========== エグゼクティブサマリー ==========
        elements.append(Paragraph("エグゼクティブサマリー", heading_style))
        summary = report.executive_summary
        elements.append(Paragraph(f"<b>結論:</b> {summary.one_line_decision}", highlight_style))

        if hasattr(summary, "essence_statement") and summary.essence_statement:
            elements.append(Paragraph(f"<b>本質:</b> {summary.essence_statement}", normal_style))
        elements.append(
            Paragraph(f"<b>推奨アクション:</b> {summary.recommended_action}", normal_style)
        )
        elements.append(Paragraph(f"<b>最初の一歩:</b> {summary.first_step}", normal_style))

        if (
            hasattr(summary, "strategic_prohibition_summary")
            and summary.strategic_prohibition_summary
        ):
            elements.append(
                Paragraph(
                    f"<b>戦略的禁止:</b> {summary.strategic_prohibition_summary}", warning_style
                )
            )
        if hasattr(summary, "exit_criteria_summary") and summary.exit_criteria_summary:
            elements.append(
                Paragraph(f"<b>撤退基準:</b> {summary.exit_criteria_summary}", warning_style)
            )

        if summary.key_risks:
            elements.append(Paragraph("<b>主要リスク:</b>", normal_style))
            for risk in summary.key_risks:
                elements.append(Paragraph(f"  - {risk}", normal_style))
        elements.append(Spacer(1, 0.5 * cm))

        # ========== 道セクション ==========
        elements.append(Paragraph("道 - 本質分析", heading_style))
        problem_type = dao.get("problem_type", "N/A")
        if hasattr(problem_type, "value"):
            problem_type = problem_type.value
        problem_nature = dao.get("problem_nature", "")
        if hasattr(problem_nature, "value"):
            problem_nature = problem_nature.value

        elements.append(Paragraph(f"<b>問題タイプ:</b> {problem_type}", normal_style))
        elements.append(Paragraph(f"<b>問題の本質的性質:</b> {problem_nature}", normal_style))
        elements.append(Paragraph(f"<b>本質:</b> {dao.get('essence', 'N/A')}", highlight_style))

        # 本質導出プロセス
        ed = dao.get("essence_derivation", {})
        if ed:
            elements.append(Paragraph("本質導出プロセス", subheading_style))
            elements.append(Paragraph(f"表面的問題: {ed.get('surface_problem', '')}", normal_style))
            elements.append(
                Paragraph(f"一段深い理由: {ed.get('underlying_why', '')}", normal_style)
            )
            elements.append(Paragraph(f"根本制約: {ed.get('root_constraint', '')}", normal_style))
            elements.append(
                Paragraph(f"<b>本質の一文:</b> {ed.get('essence_statement', '')}", highlight_style)
            )

        # 既存代替手段
        alternatives = dao.get("existing_alternatives", [])
        if alternatives:
            elements.append(Paragraph("既存代替手段", subheading_style))
            for alt in alternatives:
                elements.append(
                    Paragraph(
                        f"• <b>{alt.get('name', '')}</b>: {alt.get('why_not_viable', '')} (制約: {alt.get('specific_constraint', '')})",
                        normal_style,
                    )
                )

        immutable_constraints = dao.get("immutable_constraints", [])
        if immutable_constraints:
            elements.append(Paragraph("不可変制約", subheading_style))
            for c in immutable_constraints:
                elements.append(Paragraph(f"  - {c}", warning_style))

        hidden_assumptions = dao.get("hidden_assumptions", [])
        if hidden_assumptions:
            elements.append(Paragraph("隠れた前提", subheading_style))
            for a in hidden_assumptions:
                elements.append(Paragraph(f"  - {a}", normal_style))

        gears = dao.get("causal_gears", [])
        if gears:
            elements.append(Paragraph("因果齿轮", subheading_style))
            for g in gears:
                elements.append(
                    Paragraph(
                        f"  - {g.get('name', '')} (Leverage: {g.get('leverage', '')}): {g.get('description', '')}",
                        normal_style,
                    )
                )
            bottleneck = dao.get("bottleneck_gear", "")
            if bottleneck:
                elements.append(Paragraph(f"ボトルネック: {bottleneck}", highlight_style))

        # 死穴
        traps = dao.get("death_traps", [])
        if traps:
            elements.append(Paragraph("死穴（禁忌）", subheading_style))
            for trap in traps:
                elements.append(
                    Paragraph(
                        f"- <b>{trap.get('action', '')}</b> ({trap.get('severity', '')}): {trap.get('reason', '')}",
                        warning_style,
                    )
                )

        # v3.1: 制約境界条件
        boundaries = dao.get("constraint_boundaries", [])
        if boundaries:
            elements.append(Paragraph("制約境界条件", subheading_style))
            for cb in boundaries:
                elements.append(
                    Paragraph(
                        f"- <b>{cb.get('constraint_name', '')}</b>: {cb.get('definition', '')} "
                        f"(違反例: {cb.get('violation_example', '')}、例外: {cb.get('exceptions', '')})",
                        normal_style,
                    )
                )

        # v3.1: 成立ルート比較
        routes = dao.get("solution_routes", [])
        if routes:
            elements.append(Paragraph("成立ルート比較（解空間探索）", subheading_style))
            for sr in routes:
                tradeoffs_str = "、".join(sr.get("tradeoffs", []))
                elements.append(
                    Paragraph(
                        f"- <b>[{sr.get('route_type', '')}]</b> {sr.get('description', '')} "
                        f"(実現可能性: {sr.get('viability', '')}) トレードオフ: {tradeoffs_str}",
                        normal_style,
                    )
                )

        # v3.1: 定量指標
        metrics = dao.get("quantified_metrics", [])
        if metrics:
            elements.append(Paragraph("定量指標", subheading_style))
            for qm in metrics:
                elements.append(
                    Paragraph(
                        f"- P{qm.get('priority', '')} <b>{qm.get('metric_name', '')}</b>: "
                        f"目標 {qm.get('target_value', '')} ({qm.get('tradeoff_note', '')})",
                        normal_style,
                    )
                )

        # v3.1: 監査証拠チェックリスト
        audit_items = dao.get("audit_evidence_checklist", [])
        if audit_items:
            elements.append(Paragraph("監査証拠チェックリスト", subheading_style))
            for ae in audit_items:
                elements.append(
                    Paragraph(
                        f"- [{ae.get('category', '')}] {ae.get('required_evidence', '')} "
                        f"→ 確認: {ae.get('verification_method', '')}",
                        normal_style,
                    )
                )

        # v3.1: セルフチェック結果
        self_check = dao.get("self_check", {})
        if self_check:
            status = self_check.get("overall_status", "N/A")
            if hasattr(status, "value"):
                status = status.value
            elements.append(Paragraph(f"セルフチェック: {status}", subheading_style))
            for label, key in [
                ("境界未定義", "boundary_undefined"),
                ("選択肢漏れ", "missing_alternatives"),
                ("曖昧な指標", "ambiguous_metrics"),
                ("制約衝突", "constraint_conflicts"),
                ("証拠不足", "evidence_gaps"),
            ]:
                items = self_check.get(key, [])
                if items:
                    elements.append(Paragraph(f"  {label}: {', '.join(items)}", warning_style))

        elements.append(Spacer(1, 0.3 * cm))

        # ========== 法セクション v3.1 ==========
        elements.append(PageBreak())
        elements.append(Paragraph("法 - 戦略選定 v3.1", heading_style))

        # 戦略的禁止事項（仕組み化）
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            elements.append(Paragraph("戦略的禁止事項（仕組み化）", subheading_style))
            for p in prohibitions:
                elements.append(
                    Paragraph(
                        f"- <b>⛔ {p.get('prohibition', '')}</b>: {p.get('rationale', '')} → {p.get('violation_consequence', '')}",
                        warning_style,
                    )
                )
                if p.get("prevention_measure"):
                    elements.append(
                        Paragraph(f"  🛡️ 防止策: {p['prevention_measure']}", normal_style)
                    )
                if p.get("detection_metric"):
                    elements.append(
                        Paragraph(f"  📊 検知指標: {p['detection_metric']}", normal_style)
                    )
                if p.get("responsible_role"):
                    elements.append(
                        Paragraph(f"  👤 責任者: {p['responsible_role']}", normal_style)
                    )

        # v3.1: 競争優位仮説
        comp_hyp = fa.get("competitive_hypothesis", {})
        if comp_hyp:
            elements.append(Paragraph("競争優位仮説", subheading_style))
            elements.append(
                Paragraph(f"<b>差別化軸:</b> {comp_hyp.get('axis_name', '')}", highlight_style)
            )
            elements.append(
                Paragraph(f"<b>対象顧客:</b> {comp_hyp.get('target_customer', '')}", normal_style)
            )
            elements.append(
                Paragraph(
                    f"<b>代替障壁:</b> {comp_hyp.get('substitution_barrier', '')}", normal_style
                )
            )
            elements.append(
                Paragraph(f"<b>勝ち筋指標:</b> {comp_hyp.get('winning_metric', '')}", normal_style)
            )
            elements.append(
                Paragraph(
                    f"<b>最小検証:</b> {comp_hyp.get('minimum_verification', '')}", normal_style
                )
            )
        else:
            # v3.0フォールバック: 差別化軸
            diff_axis = fa.get("differentiation_axis", {})
            if diff_axis:
                elements.append(Paragraph("差別化軸", subheading_style))
                elements.append(
                    Paragraph(
                        f"<b>勝負する軸:</b> {diff_axis.get('axis_name', '')}", highlight_style
                    )
                )
                elements.append(
                    Paragraph(f"理由: {diff_axis.get('why_this_axis', '')}", normal_style)
                )
                elements.append(
                    Paragraph(
                        f"<b>勝負しない軸:</b> {diff_axis.get('not_this_axis', '')}", normal_style
                    )
                )

        # 既存解が使えない理由
        why_existing = fa.get("why_existing_fails", "")
        if why_existing:
            elements.append(
                Paragraph(f"<b>既存解が使えない理由:</b> {why_existing}", warning_style)
            )

        # 推奨パス（v3.1: 条件付き評価）
        for path in fa.get("recommended_paths", []):
            strategy_type = path.get("strategy_type", "")
            if hasattr(strategy_type, "value"):
                strategy_type = strategy_type.value
            elements.append(
                Paragraph(f"{path.get('name', '')} ({strategy_type})", subheading_style)
            )
            elements.append(Paragraph(path.get("description", ""), normal_style))
            rev = path.get("reversibility", "")
            elements.append(
                Paragraph(
                    f"価値実現: {path.get('time_to_value', '')} | 可逆性: {rev}", normal_style
                )
            )
            # v3.1: 条件付き評価
            cond_eval = path.get("conditional_evaluation", {})
            if cond_eval:
                sc = cond_eval.get("success_conditions", [])
                if sc:
                    elements.append(Paragraph(f"<b>成立条件:</b> {', '.join(sc)}", normal_style))
                rf = cond_eval.get("risk_factors", [])
                if rf:
                    elements.append(Paragraph(f"<b>リスク要因:</b> {', '.join(rf)}", warning_style))
                fm = cond_eval.get("failure_modes", [])
                if fm:
                    elements.append(Paragraph(f"<b>失敗モード:</b> {', '.join(fm)}", warning_style))
            risk_conc = path.get("risk_concentration", "")
            if risk_conc:
                elements.append(Paragraph(f"<b>リスク集中点:</b> {risk_conc}", warning_style))
            pros = path.get("pros", [])
            if pros:
                elements.append(Paragraph("<b>メリット:</b>", normal_style))
                for pro in pros:
                    elements.append(Paragraph(f"  - {pro}", normal_style))
            cons = path.get("cons", [])
            if cons:
                elements.append(Paragraph("<b>デメリット:</b>", normal_style))
                for con in cons:
                    elements.append(Paragraph(f"  - {con}", warning_style))

        # v3.1: 判断フレームワーク
        jf = fa.get("judgment_framework", {})
        if jf:
            elements.append(Paragraph("判断フレームワーク（Must/Should分離）", subheading_style))
            must_gates = jf.get("must_gates", [])
            if must_gates:
                elements.append(Paragraph("<b>Must（不可変ゲート）:</b>", normal_style))
                for g in must_gates:
                    elements.append(
                        Paragraph(
                            f"  🚪 {g.get('criterion', '')} — 閾値: {g.get('threshold', '')}",
                            normal_style,
                        )
                    )
            should = jf.get("should_criteria", [])
            if should:
                elements.append(Paragraph("<b>Should（比較評価）:</b>", normal_style))
                for s in should:
                    elements.append(
                        Paragraph(
                            f"  📏 {s.get('criterion', '')} [{s.get('weight', '')}] — {s.get('scoring_method', '')}",
                            normal_style,
                        )
                    )

        # v3.1: セルフチェック
        fa_sc = fa.get("fa_self_check", {})
        if fa_sc:
            status = fa_sc.get("overall_status", "")
            if hasattr(status, "value"):
                status = status.value
            sc_style = warning_style if status != "PASS" else normal_style
            elements.append(Paragraph(f"セルフチェック結果: {status}", sc_style))
            for label, key in [
                ("根拠なき数値", "baseless_numbers"),
                ("中間案漏れ", "missing_intermediate"),
                ("ゲート不在", "missing_gates"),
                ("見せかけ精度", "appearance_precision"),
            ]:
                items = fa_sc.get(key, [])
                if items:
                    elements.append(Paragraph(f"  {label}: {', '.join(items)}", warning_style))

        elements.append(Spacer(1, 0.3 * cm))

        # ========== 術セクション ==========
        elements.append(Paragraph("術 - 実行計画", heading_style))

        # 最初の一歩
        first_action = shu.get("first_action", "")
        if first_action:
            elements.append(Paragraph(f"<b>最初の一歩:</b> {first_action}", highlight_style))

        # 切り捨てリスト
        cut_list = shu.get("cut_list", [])
        if cut_list:
            elements.append(
                Paragraph("切り捨てリスト（最初の30日間でやらないこと）", subheading_style)
            )
            for c in cut_list:
                elements.append(Paragraph(f"  - {c}", warning_style))

        # 文脈特化行動
        context_actions = shu.get("context_specific_actions", [])
        if context_actions:
            elements.append(Paragraph("文脈特化行動", subheading_style))
            for a in context_actions:
                elements.append(
                    Paragraph(
                        f"• <b>{a.get('action', '')}</b> → {a.get('expected_output', '')}",
                        normal_style,
                    )
                )

        # 単一検証ポイント
        validation = shu.get("single_validation_point", {})
        if validation:
            elements.append(Paragraph("単一検証ポイント", subheading_style))
            elements.append(
                Paragraph(
                    f"検証: {validation.get('validation_target', '')} | "
                    f"基準: {validation.get('success_criteria', '')} | "
                    f"失敗時: {validation.get('failure_action', '')}",
                    warning_style,
                )
            )

        # 撤退基準
        exit_criteria = shu.get("exit_criteria", {})
        if exit_criteria:
            elements.append(Paragraph("撤退基準", subheading_style))
            elements.append(
                Paragraph(
                    f"チェック: {exit_criteria.get('checkpoint', '')} | "
                    f"トリガー: {exit_criteria.get('exit_trigger', '')} | "
                    f"行動: {exit_criteria.get('exit_action', '')}",
                    warning_style,
                )
            )

        dependencies = shu.get("dependencies", [])
        if dependencies:
            elements.append(Paragraph("依存関係", subheading_style))
            for dep in dependencies:
                elements.append(Paragraph(f"  - {dep}", normal_style))

        # フェーズ
        elements.append(Paragraph("フェーズ", subheading_style))
        for phase in shu.get("phases", []):
            elements.append(
                Paragraph(
                    f"Phase {phase.get('phase_number', '?')}: {phase.get('name', '')} ({phase.get('duration', '')})",
                    normal_style,
                )
            )
            actions = phase.get("actions", [])
            if actions:
                for action in actions:
                    elements.append(Paragraph(f"    • {action}", normal_style))
        elements.append(Spacer(1, 0.3 * cm))

        # ========== 器セクション ==========
        elements.append(PageBreak())
        elements.append(Paragraph("器 - 技術実装", heading_style))

        # ドメイン固有技術
        domain_techs = qi.get("domain_technologies", [])
        if domain_techs:
            elements.append(Paragraph("ドメイン固有技術", subheading_style))
            for t in domain_techs:
                elements.append(
                    Paragraph(
                        f"• <b>{t.get('technology_name', '')}</b> ({t.get('category', '')}): {t.get('why_required', '')}",
                        highlight_style,
                    )
                )

        # 規制対応
        regulations = qi.get("regulatory_considerations", [])
        if regulations:
            elements.append(Paragraph("規制対応事項", subheading_style))
            reg_data = [["地域", "規制", "要件", "影響"]]
            for r in regulations:
                reg_data.append(
                    [
                        r.get("region", ""),
                        r.get("regulation", ""),
                        r.get("requirement", ""),
                        r.get("implementation_impact", ""),
                    ]
                )
            reg_table = Table(reg_data, colWidths=[2 * cm, 3 * cm, 5 * cm, 5 * cm])
            reg_table.setStyle(
                TableStyle(
                    [
                        ("GRID", (0, 0), (-1, -1), 0.5, colors.black),
                        ("FONTNAME", (0, 0), (-1, -1), cjk_font),
                        ("FONTSIZE", (0, 0), (-1, -1), 8),
                        ("BACKGROUND", (0, 0), (-1, 0), colors.Color(0.9, 0.9, 0.9)),
                    ]
                )
            )
            elements.append(reg_table)

        # 地理的考慮
        geographics = qi.get("geographic_considerations", [])
        if geographics:
            elements.append(Paragraph("地理的考慮事項", subheading_style))
            for g in geographics:
                elements.append(
                    Paragraph(
                        f"• {g.get('region', '')}: {g.get('latency_requirement', '')} | {g.get('infrastructure_need', '')}",
                        normal_style,
                    )
                )

        # 実装要素
        for impl in qi.get("implementations", []):
            elements.append(
                Paragraph(
                    f"{impl.get('component', '')}: {impl.get('technology', '')} ({impl.get('estimated_effort', '')})",
                    normal_style,
                )
            )
            risks = impl.get("risks", [])
            if risks:
                for r in risks:
                    elements.append(Paragraph(f"  - リスク: {r}", warning_style))

        tools = qi.get("tool_recommendations", [])
        if tools:
            elements.append(Paragraph("推奨ツール", subheading_style))
            for t in tools:
                elements.append(Paragraph(f"  - {t}", normal_style))

        integration = qi.get("integration_points", [])
        if integration:
            elements.append(Paragraph("統合ポイント", subheading_style))
            for i in integration:
                elements.append(Paragraph(f"  - {i}", normal_style))

        debt_warnings = qi.get("technical_debt_warnings", [])
        if debt_warnings:
            elements.append(Paragraph("技術負債警告", subheading_style))
            for w in debt_warnings:
                elements.append(Paragraph(f"  - {w}", warning_style))
        elements.append(Spacer(1, 0.3 * cm))

        # ========== 検証セクション ==========
        elements.append(Paragraph("検証 - 最終判定", heading_style))
        verdict = review.get("overall_verdict", "N/A")
        if hasattr(verdict, "value"):
            verdict = verdict.value
        confidence = review.get("confidence_score", 0)
        verdict_style = highlight_style if verdict == "PASS" else warning_style
        elements.append(
            Paragraph(f"<b>判定: {verdict}</b> | 信頼度: {confidence * 100:.0f}%", verdict_style)
        )

        findings = review.get("findings", [])
        if findings:
            for f in findings:
                elements.append(
                    Paragraph(
                        f"• {f.get('severity', '')}: {f.get('description', '')}", normal_style
                    )
                )
        final_warnings = review.get("final_warnings", [])
        if final_warnings:
            elements.append(Paragraph("最終警告", subheading_style))
            for w in final_warnings:
                elements.append(Paragraph(f"  - {w}", warning_style))
        elements.append(Spacer(1, 0.5 * cm))

        # ========== 署名欄 ==========
        elements.append(Paragraph("署名欄", heading_style))
        sig_data = [
            ["作成", "部署", author_dept, "役職", author_pos],
            ["", "氏名", author_name, "日付", created_date],
            ["承認", "部署", "", "役職", ""],
            ["", "氏名", "", "日付", ""],
        ]
        sig_table = Table(sig_data, colWidths=[1.6 * cm, 2.2 * cm, 6.0 * cm, 1.6 * cm, 4.6 * cm])
        sig_table.setStyle(
            TableStyle(
                [
                    ("GRID", (0, 0), (-1, -1), 0.5, colors.black),
                    ("FONTNAME", (0, 0), (-1, -1), cjk_font),
                    ("BACKGROUND", (0, 0), (-1, 0), colors.Color(0.95, 0.95, 0.95)),
                ]
            )
        )
        elements.append(sig_table)

        doc.build(elements)
        return buffer.getvalue()

    def _generate_html_fallback(self, report: DecisionReport) -> bytes:
        """HTML形式での提案書出力 v3.1."""
        # Pydanticオブジェクトをdictに変換
        dao = self._to_dict(report.dao)
        fa = self._to_dict(report.fa)
        shu = self._to_dict(report.shu)
        qi = self._to_dict(report.qi)
        review = self._to_dict(report.review)
        summary = report.executive_summary

        # 提案書タイトル取得
        proposal_title = self._to_dict(report.proposal_title) if report.proposal_title else {}
        title_ja = proposal_title.get("title_ja", "課題解決提案書")
        title_en = proposal_title.get("title_en", "Solution_Proposal")
        case_id = proposal_title.get("case_id", report.report_id)
        subtitle = proposal_title.get("subtitle", "")

        # 署名欄情報
        sig_block = self._to_dict(report.signature_block) if report.signature_block else {}
        author_name = sig_block.get("author_name", "Decision Agent")
        author_dept = sig_block.get("author_department", "AI Decision Support")
        author_pos = sig_block.get("author_position", "AI Assistant")
        created_date = sig_block.get("created_date", report.created_at.strftime("%Y年%m月%d日"))

        # 各セクションの構築
        dao_html = self._build_dao_html(dao)
        fa_html = self._build_fa_html(fa)
        shu_html = self._build_shu_html(shu)
        qi_html = self._build_qi_html(qi)
        review_html = self._build_review_html(review)
        summary_html = self._build_summary_html(summary)

        html = f"""<!DOCTYPE html>
<html lang="ja">
<head><meta charset="UTF-8"><title>提案書 - {title_ja}</title>
<style>
@page {{ size: A4; margin: 2cm; }}
body{{font-family:'Yu Gothic','Hiragino Sans','Meiryo',sans-serif;max-width:920px;margin:0 auto;padding:30px;color:#e2e8f0;line-height:1.8;background:#0a0a0f}}
.cover{{text-align:center;padding:60px 0;border:2px solid #4338ca;margin-bottom:40px;background:linear-gradient(135deg,#12121a,#1a1a24);border-radius:16px}}
.cover-title{{font-size:32px;font-weight:bold;color:#f8fafc;margin:20px 0 10px;letter-spacing:2px}}
.cover-title-en{{font-size:14px;color:#94a3b8;font-family:'Helvetica Neue',Arial,sans-serif;letter-spacing:1px}}
.cover-subtitle{{font-size:16px;color:#cbd5e1;margin:20px 0}}
.cover-case-id{{font-size:12px;color:#94a3b8;font-family:monospace}}
.cover-date{{font-size:14px;color:#cbd5e1;margin-top:40px}}
.cover-author{{font-size:14px;color:#cbd5e1;margin-top:10px}}
h1{{border-bottom:2px solid #334155;padding-bottom:10px;color:#f8fafc;font-size:24px}}
h2{{color:#cbd5e1;margin-top:30px;border-left:4px solid #6366f1;padding-left:12px;font-size:18px}}
h3{{color:#94a3b8;margin-top:20px;font-size:14px}}
.meta{{color:#94a3b8;font-size:0.9em;margin-bottom:20px}}
.toc{{background:#12121a;padding:20px;border-radius:12px;margin:20px 0;border:1px solid #334155}}
.toc h2{{border:none;margin-top:0}}
.toc ol{{margin:0;padding-left:25px}}
.toc li{{margin:8px 0}}
.summary{{background:linear-gradient(135deg,#12121a,#1a1a24);padding:25px;border-radius:12px;margin:20px 0;border:1px solid #334155}}
.card{{background:#12121a;border:1px solid #334155;border-radius:10px;padding:15px;margin:15px 0;box-shadow:0 2px 6px rgba(15,23,42,0.3)}}
.prohibition{{background:#3f1114;border-left:4px solid #ef4444;padding:10px 15px;margin:10px 0;border-radius:8px}}
.highlight{{background:#172554;border-left:4px solid #6366f1;padding:10px 15px;margin:10px 0;border-radius:8px}}
.success{{background:#052e16;border-left:4px solid #22c55e;padding:10px 15px;margin:10px 0;border-radius:8px}}
.warning{{background:#422006;border-left:4px solid #f59e0b;padding:10px 15px;margin:10px 0;border-radius:8px}}
ul{{padding-left:20px}}
li{{margin:5px 0}}
table{{border-collapse:collapse;width:100%;background:#12121a}}
td,th{{border:1px solid #334155;padding:12px;text-align:left}}
th{{background:#1f2937;font-weight:bold}}
.signature-section{{margin-top:60px;page-break-inside:avoid}}
.signature-table{{border:2px solid #334155}}
.signature-table th{{background:#1f2937;width:100px}}
.signature-table td{{height:60px;vertical-align:top}}
.label{{font-weight:bold;color:#cbd5e1}}
.essence{{font-size:1.1em;color:#f8fafc;font-weight:bold}}
.footer{{text-align:center;color:#94a3b8;font-size:11px;margin-top:40px;padding-top:20px;border-top:1px solid #334155}}
.section-number{{color:#818cf8;font-weight:bold;margin-right:8px}}
</style></head>
<body>

<!-- 表紙 -->
<div class="cover">
<p style="font-size:18px;color:#555">御中</p>
<h1 class="cover-title">{title_ja}</h1>
<p class="cover-title-en">{title_en}</p>
<p class="cover-subtitle">{subtitle}</p>
<p class="cover-case-id">案件ID: {case_id}</p>
<p class="cover-date">{created_date}</p>
<p class="cover-author">{author_dept}<br/>{author_pos} {author_name}</p>
</div>

<!-- 目次 -->
<div class="toc">
<h2>📋 目次</h2>
<ol>
<li>エグゼクティブサマリー</li>
<li>現状の課題・問題点（道 - 本質分析）</li>
<li>提案内容・解決策（法 - 戦略選定）</li>
<li>実行計画・スケジュール（術 - 実行計画）</li>
<li>技術的な実装（器 - 技術実装）</li>
<li>根拠・検証結果</li>
<li>署名欄</li>
</ol>
</div>

<!-- 1. エグゼクティブサマリー -->
<h1><span class="section-number">1.</span> エグゼクティブサマリー</h1>
{summary_html}

<!-- 2. 現状の課題・問題点 -->
<h1><span class="section-number">2.</span> 現状の課題・問題点</h1>
{dao_html}

<!-- 3. 提案内容・解決策 -->
<h1><span class="section-number">3.</span> 提案内容・解決策</h1>
{fa_html}

<!-- 4. 実行計画・スケジュール -->
<h1><span class="section-number">4.</span> 実行計画・スケジュール</h1>
{shu_html}

<!-- 5. 技術的な実装 -->
<h1><span class="section-number">5.</span> 技術的な実装</h1>
{qi_html}

<!-- 6. 根拠・検証結果 -->
<h1><span class="section-number">6.</span> 根拠・検証結果</h1>
{review_html}

<!-- 7. 署名欄 -->
<div class="signature-section">
<h1><span class="section-number">7.</span> 署名欄</h1>
<table class="signature-table">
<tr>
<th rowspan="2">作成</th>
<th>部署</th><td>{author_dept}</td>
<th>役職</th><td>{author_pos}</td>
</tr>
<tr>
<th>氏名</th><td>{author_name}</td>
<th>日付</th><td>{created_date}</td>
</tr>
<tr>
<th rowspan="2">承認</th>
<th>部署</th><td></td>
<th>役職</th><td></td>
</tr>
<tr>
<th>氏名</th><td></td>
<th>日付</th><td></td>
</tr>
<tr>
<th colspan="5" style="text-align:center;background:#fafafa">承認印</th>
</tr>
<tr>
<td colspan="5" style="height:80px;text-align:center;vertical-align:middle">
<div style="display:inline-block;width:80px;height:80px;border:2px solid #ccc;border-radius:50%;margin:10px"></div>
</td>
</tr>
</table>
</div>

<div class="footer">
<p>本提案書は AI Decision Support により自動生成されました</p>
<p>案件ID: {case_id} | Version: {report.version} | 生成日時: {report.created_at.strftime("%Y-%m-%d %H:%M")}</p>
</div>

</body></html>"""
        return html.encode("utf-8")

    def _build_summary_html(self, summary: Any) -> str:
        """エグゼクティブサマリーHTMLを構築."""
        risks_html = ""
        if summary.key_risks:
            risks_html = "<ul>" + "".join(f"<li>⚠️ {r}</li>" for r in summary.key_risks) + "</ul>"

        # v3.0 新フィールド
        essence_html = ""
        if hasattr(summary, "essence_statement") and summary.essence_statement:
            essence_html = f'<p class="essence">📍 本質: {summary.essence_statement}</p>'

        prohibition_html = ""
        if (
            hasattr(summary, "strategic_prohibition_summary")
            and summary.strategic_prohibition_summary
        ):
            prohibition_html = f'<div class="prohibition">⛔ 戦略的禁止: {summary.strategic_prohibition_summary}</div>'

        exit_html = ""
        if hasattr(summary, "exit_criteria_summary") and summary.exit_criteria_summary:
            exit_html = f'<div class="warning">🚪 撤退基準: {summary.exit_criteria_summary}</div>'

        return f"""<div class="summary">
<h2>📊 エグゼクティブサマリー</h2>
<p><span class="label">結論:</span> <strong>{summary.one_line_decision}</strong></p>
{essence_html}
<p><span class="label">推奨アクション:</span> {summary.recommended_action}</p>
<p><span class="label">最初の一歩:</span> 🎯 {summary.first_step}</p>
{prohibition_html}
{exit_html}
<h3>主要リスク</h3>
{risks_html}
</div>"""

    def _build_dao_html(self, dao: dict) -> str:
        """道セクションHTMLを構築 v3.0."""
        problem_type = dao.get("problem_type", "N/A")
        if hasattr(problem_type, "value"):
            problem_type = problem_type.value

        problem_nature = dao.get("problem_nature", "")
        if hasattr(problem_nature, "value"):
            problem_nature = problem_nature.value

        # v3.0: 本質導出プロセス
        essence_derivation_html = ""
        ed = dao.get("essence_derivation", {})
        if ed:
            essence_derivation_html = f"""<div class="card">
<h3>🔍 本質導出プロセス</h3>
<p><span class="label">表面的問題:</span> {ed.get("surface_problem", "")}</p>
<p><span class="label">一段深い理由:</span> {ed.get("underlying_why", "")}</p>
<p><span class="label">根本制約:</span> {ed.get("root_constraint", "")}</p>
<p class="essence"><span class="label">本質の一文:</span> {ed.get("essence_statement", "")}</p>
</div>"""

        # v3.0: 既存代替手段
        alternatives_html = ""
        alternatives = dao.get("existing_alternatives", [])
        if alternatives:
            items = "".join(
                f"<li><strong>{a.get('name', '')}</strong>: {a.get('why_not_viable', '')} (制約: {a.get('specific_constraint', '')})</li>"
                for a in alternatives
            )
            alternatives_html = f"""<div class="card">
<h3>🔄 既存代替手段（なぜ使えないか）</h3>
<ul>{items}</ul>
</div>"""

        # 不可変制約
        constraints_html = ""
        constraints = dao.get("immutable_constraints", [])
        if constraints:
            items = "".join(f"<li>{c}</li>" for c in constraints)
            constraints_html = f"<h3>🔒 不可変制約</h3><ul>{items}</ul>"

        # 隠れた前提
        assumptions_html = ""
        assumptions = dao.get("hidden_assumptions", [])
        if assumptions:
            items = "".join(f"<li>{a}</li>" for a in assumptions)
            assumptions_html = f"<h3>💭 隠れた前提</h3><ul>{items}</ul>"

        # 因果齿轮
        gears_html = ""
        gears = dao.get("causal_gears", [])
        if gears:
            items = "".join(
                f"<li><strong>{g.get('name', '')}</strong> (Leverage: {g.get('leverage', '')}): {g.get('description', '')}</li>"
                for g in gears
            )
            bottleneck = dao.get("bottleneck_gear", "")
            gears_html = (
                f"<h3>⚙️ 因果齿轮</h3><ul>{items}</ul><p>🎯 ボトルネック: Gear {bottleneck}</p>"
            )

        # 死穴
        death_traps_html = ""
        traps = dao.get("death_traps", [])
        if traps:
            items = "".join(
                f'<div class="prohibition"><strong>⚠️ {t.get("action", "")}</strong> ({t.get("severity", "")})<br/>理由: {t.get("reason", "")}</div>'
                for t in traps
            )
            death_traps_html = f"<h3>💀 死穴（禁忌）</h3>{items}"

        # v3.1: 制約境界条件
        boundaries_html = ""
        boundaries = dao.get("constraint_boundaries", [])
        if boundaries:
            rows = "".join(
                f"<tr><td><strong>{cb.get('constraint_name', '')}</strong></td>"
                f"<td>{cb.get('definition', '')}</td>"
                f"<td>{cb.get('violation_example', '')}</td>"
                f"<td>{cb.get('exceptions', '')}</td></tr>"
                for cb in boundaries
            )
            boundaries_html = (
                "<h3>🚧 制約境界条件</h3>"
                "<table><thead><tr><th>制約名</th><th>判定条件</th><th>違反例</th><th>例外</th></tr></thead>"
                f"<tbody>{rows}</tbody></table>"
            )

        # v3.1: 成立ルート比較
        routes_html = ""
        routes = dao.get("solution_routes", [])
        if routes:
            route_items = ""
            for sr in routes:
                tradeoffs_str = "、".join(sr.get("tradeoffs", []))
                route_items += (
                    f'<div class="card"><strong>[{sr.get("route_type", "")}]</strong> '
                    f"{sr.get('description', '')}<br/>"
                    f'<span class="label">実現可能性:</span> {sr.get("viability", "")} '
                    f'<span class="label">トレードオフ:</span> {tradeoffs_str}</div>'
                )
            routes_html = f"<h3>🛤️ 成立ルート比較（解空間探索）</h3>{route_items}"

        # v3.1: 定量指標
        metrics_html = ""
        metrics = dao.get("quantified_metrics", [])
        if metrics:
            m_rows = "".join(
                f"<tr><td>P{qm.get('priority', '')}</td>"
                f"<td><strong>{qm.get('metric_name', '')}</strong></td>"
                f"<td>{qm.get('target_value', '')}</td>"
                f"<td>{qm.get('tradeoff_note', '')}</td></tr>"
                for qm in metrics
            )
            metrics_html = (
                "<h3>📊 定量指標</h3>"
                "<table><thead><tr><th>優先</th><th>指標名</th><th>目標値</th><th>トレードオフ</th></tr></thead>"
                f"<tbody>{m_rows}</tbody></table>"
            )

        # v3.1: 監査証拠チェックリスト
        audit_html = ""
        audit_items = dao.get("audit_evidence_checklist", [])
        if audit_items:
            a_items = "".join(
                f"<li><strong>[{ae.get('category', '')}]</strong> {ae.get('required_evidence', '')} "
                f"→ 確認: {ae.get('verification_method', '')}</li>"
                for ae in audit_items
            )
            audit_html = f"<h3>📋 監査証拠チェックリスト</h3><ul>{a_items}</ul>"

        # v3.1: セルフチェック結果
        selfcheck_html = ""
        self_check = dao.get("self_check", {})
        if self_check:
            sc_status = self_check.get("overall_status", "N/A")
            if hasattr(sc_status, "value"):
                sc_status = sc_status.value
            sc_details = ""
            for label, key in [
                ("境界未定義", "boundary_undefined"),
                ("選択肢漏れ", "missing_alternatives"),
                ("曖昧な指標", "ambiguous_metrics"),
                ("制約衝突", "constraint_conflicts"),
                ("証拠不足", "evidence_gaps"),
            ]:
                sc_items = self_check.get(key, [])
                if sc_items:
                    sc_details += f"<li>{label}: {', '.join(sc_items)}</li>"
            if sc_details:
                sc_details = f"<ul>{sc_details}</ul>"
            selfcheck_html = f'<h3>🔬 セルフチェック: <span class="highlight">{sc_status}</span></h3>{sc_details}'

        return f"""<h2>🎯 道 - 本質分析</h2>
<div class="card">
<p><span class="label">問題タイプ:</span> {problem_type}</p>
<p><span class="label">問題の本質的性質:</span> {problem_nature}</p>
<p class="essence"><span class="label">本質:</span> {dao.get("essence", "N/A")}</p>
</div>
{essence_derivation_html}
{alternatives_html}
{constraints_html}
{assumptions_html}
{gears_html}
{death_traps_html}
{boundaries_html}
{routes_html}
{metrics_html}
{audit_html}
{selfcheck_html}"""

    def _build_fa_html(self, fa: dict) -> str:
        """法セクションHTMLを構築 v3.1."""
        # v3.1: 戦略的禁止事項（仕組み化）
        prohibitions_html = ""
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            items = ""
            for p in prohibitions:
                enforcement = ""
                if p.get("prevention_measure"):
                    enforcement += f"<br/>🛡️ 防止策: {p['prevention_measure']}"
                if p.get("detection_metric"):
                    enforcement += f"<br/>📊 検知指標: {p['detection_metric']}"
                if p.get("responsible_role"):
                    enforcement += f"<br/>👤 責任者: {p['responsible_role']}"
                items += (
                    f'<div class="prohibition"><strong>⛔ {p.get("prohibition", "")}</strong><br/>'
                    f"理由: {p.get('rationale', '')}<br/>"
                    f"違反結果: {p.get('violation_consequence', '')}"
                    f"{enforcement}</div>"
                )
            prohibitions_html = f"<h3>🚫 戦略的禁止事項（仕組み化）</h3>{items}"

        # v3.1: 競争優位仮説
        hypothesis_html = ""
        comp_hyp = fa.get("competitive_hypothesis", {})
        if comp_hyp:
            hypothesis_html = f"""<div class="highlight">
<h3>🎯 競争優位仮説</h3>
<p><span class="label">差別化軸:</span> <strong>{comp_hyp.get("axis_name", "")}</strong></p>
<p><span class="label">対象顧客:</span> {comp_hyp.get("target_customer", "")}</p>
<p><span class="label">代替障壁:</span> {comp_hyp.get("substitution_barrier", "")}</p>
<p><span class="label">勝ち筋指標:</span> {comp_hyp.get("winning_metric", "")}</p>
<p><span class="label">最小検証:</span> {comp_hyp.get("minimum_verification", "")}</p>
</div>"""
        else:
            # v3.0フォールバック
            diff_axis = fa.get("differentiation_axis", {})
            if diff_axis:
                hypothesis_html = f"""<div class="highlight">
<h3>🎯 差別化軸</h3>
<p><span class="label">勝負する軸:</span> <strong>{diff_axis.get("axis_name", "")}</strong></p>
<p><span class="label">理由:</span> {diff_axis.get("why_this_axis", "")}</p>
<p><span class="label">勝負しない軸:</span> {diff_axis.get("not_this_axis", "")}</p>
</div>"""

        # 既存解が使えない理由
        why_existing_fails = fa.get("why_existing_fails", "")
        why_existing_html = ""
        if why_existing_fails:
            why_existing_html = f'<div class="warning"><span class="label">既存解が使えない理由:</span> {why_existing_fails}</div>'

        # 推奨パス（v3.1: 条件付き評価）
        paths_html = ""
        for path in fa.get("recommended_paths", []):
            strategy_type = path.get("strategy_type", "")
            if hasattr(strategy_type, "value"):
                strategy_type = strategy_type.value
            pros = "".join(f"<li>✅ {p}</li>" for p in path.get("pros", []))
            cons = "".join(f"<li>❌ {c}</li>" for c in path.get("cons", []))
            # v3.1: 条件付き評価HTML
            cond_html = ""
            cond_eval = path.get("conditional_evaluation", {})
            if cond_eval:
                sc = cond_eval.get("success_conditions", [])
                rf = cond_eval.get("risk_factors", [])
                fm = cond_eval.get("failure_modes", [])
                cond_items = ""
                if sc:
                    cond_items += f"<p><strong>成立条件:</strong> {', '.join(sc)}</p>"
                if rf:
                    cond_items += f"<p><strong>リスク要因:</strong> {', '.join(rf)}</p>"
                if fm:
                    cond_items += f"<p><strong>失敗モード:</strong> {', '.join(fm)}</p>"
                if cond_items:
                    cond_html = f'<div class="highlight"><h4>📋 条件付き評価</h4>{cond_items}</div>'
            risk_conc = path.get("risk_concentration", "")
            risk_html = f"<p><strong>⚡ リスク集中点:</strong> {risk_conc}</p>" if risk_conc else ""
            paths_html += f"""<div class="card">
<h3>📌 {path.get("name", "")} ({strategy_type})</h3>
<p>{path.get("description", "")}</p>
<p><span class="label">価値実現:</span> {path.get("time_to_value", "")} | <span class="label">可逆性:</span> {path.get("reversibility", "")}</p>
{cond_html}{risk_html}
<h4>メリット</h4><ul>{pros}</ul>
<h4>デメリット</h4><ul>{cons}</ul>
</div>"""

        # v3.1: 判断フレームワーク
        jf_html = ""
        jf = fa.get("judgment_framework", {})
        if jf:
            must_html = ""
            must_gates = jf.get("must_gates", [])
            if must_gates:
                must_rows = "".join(
                    f"<tr><td>🚪 {g.get('criterion', '')}</td><td>{g.get('threshold', '')}</td></tr>"
                    for g in must_gates
                )
                must_html = f"<h4>Must（不可変ゲート）</h4><table><tr><th>基準</th><th>閾値</th></tr>{must_rows}</table>"
            should_html = ""
            should = jf.get("should_criteria", [])
            if should:
                should_rows = "".join(
                    f"<tr><td>{s.get('criterion', '')}</td><td>{s.get('weight', '')}</td><td>{s.get('scoring_method', '')}</td></tr>"
                    for s in should
                )
                should_html = f"<h4>Should（比較評価）</h4><table><tr><th>基準</th><th>重み</th><th>採点方法</th></tr>{should_rows}</table>"
            jf_html = f"<h3>⚖️ 判断フレームワーク</h3>{must_html}{should_html}"

        # v3.1: セルフチェック
        selfcheck_html = ""
        fa_sc = fa.get("fa_self_check", {})
        if fa_sc:
            status = fa_sc.get("overall_status", "")
            if hasattr(status, "value"):
                status = status.value
            sc_items = ""
            for label, key in [
                ("根拠なき数値", "baseless_numbers"),
                ("中間案漏れ", "missing_intermediate"),
                ("ゲート不在", "missing_gates"),
                ("見せかけ精度", "appearance_precision"),
            ]:
                vals = fa_sc.get(key, [])
                if vals:
                    sc_items += f"<p><strong>{label}:</strong> {', '.join(vals)}</p>"
            sc_class = "pass" if status == "PASS" else "warning"
            selfcheck_html = (
                f'<div class="{sc_class}"><h3>🔍 セルフチェック: {status}</h3>{sc_items}</div>'
            )

        # 比較マトリックス
        comparison_html = ""
        comparison = fa.get("path_comparison", {})
        if comparison:
            dims = comparison.get("dimensions", [])
            scores = comparison.get("scores", {})
            if dims and scores:
                header = "".join(f"<th>{d}</th>" for d in dims)
                rows = ""
                for path_id, path_scores in scores.items():
                    cells = "".join(f"<td>{s}</td>" for s in path_scores)
                    rows += f"<tr><th>{path_id}</th>{cells}</tr>"
                comparison_html = f"""<h3>📊 比較マトリックス</h3>
<table><tr><th>パス</th>{header}</tr>{rows}</table>
<p>{comparison.get("recommendation_summary", "")}</p>"""

        return f"""<h2>⚖️ 法 - 戦略選定 v3.1</h2>
{prohibitions_html}
{hypothesis_html}
{why_existing_html}
{paths_html}
{jf_html}
{comparison_html}
{selfcheck_html}"""

    def _build_shu_html(self, shu: dict) -> str:
        """術セクションHTMLを構築 v3.0."""
        # フェーズ
        phases_html = ""
        for phase in shu.get("phases", []):
            actions = "".join(f"<li>{a}</li>" for a in phase.get("actions", []))
            deliverables = "".join(f"<li>{d}</li>" for d in phase.get("deliverables", []))
            criteria = "".join(f"<li>{c}</li>" for c in phase.get("success_criteria", []))
            phases_html += f"""<div class="card">
<h3>Phase {phase.get("phase_number", "?")}: {phase.get("name", "")} ({phase.get("duration", "")})</h3>
<h4>行動</h4><ul>{actions}</ul>
<h4>成果物</h4><ul>{deliverables}</ul>
<h4>完了条件</h4><ul>{criteria}</ul>
</div>"""

        # 最初の一歩
        first_action = shu.get("first_action", "")
        first_action_html = (
            f'<div class="success"><strong>🎯 最初の一歩:</strong> {first_action}</div>'
            if first_action
            else ""
        )

        # v3.0: 切り捨てリスト
        cut_list_html = ""
        cut_list = shu.get("cut_list", [])
        if cut_list:
            items = "".join(f"<li>❌ {c}</li>" for c in cut_list)
            cut_list_html = f"""<div class="prohibition">
<h3>✂️ 切り捨てリスト（最初の30日間でやらないこと）</h3>
<ul>{items}</ul>
</div>"""

        # v3.0: 文脈特化行動
        context_actions_html = ""
        context_actions = shu.get("context_specific_actions", [])
        if context_actions:
            items = "".join(
                f"<li><strong>{a.get('action', '')}</strong><br/>"
                f"理由: {a.get('why_this_context', '')}<br/>"
                f"期待出力: {a.get('expected_output', '')}</li>"
                for a in context_actions
            )
            context_actions_html = f"""<div class="highlight">
<h3>🎯 文脈特化行動（この問題固有）</h3>
<ul>{items}</ul>
</div>"""

        # v3.0: 単一検証ポイント
        validation_html = ""
        validation = shu.get("single_validation_point", {})
        if validation:
            validation_html = f"""<div class="warning">
<h3>🔬 単一検証ポイント（PoCで絶対に検証すべき1点）</h3>
<p><span class="label">検証対象:</span> {validation.get("validation_target", "")}</p>
<p><span class="label">成功基準:</span> {validation.get("success_criteria", "")}</p>
<p><span class="label">失敗時行動:</span> {validation.get("failure_action", "")}</p>
</div>"""

        # v3.0: 撤退基準
        exit_html = ""
        exit_criteria = shu.get("exit_criteria", {})
        if exit_criteria:
            exit_html = f"""<div class="prohibition">
<h3>🚪 撤退基準（どこで止めるか）</h3>
<p><span class="label">チェックポイント:</span> {exit_criteria.get("checkpoint", "")}</p>
<p><span class="label">撤退トリガー:</span> {exit_criteria.get("exit_trigger", "")}</p>
<p><span class="label">撤退時行動:</span> {exit_criteria.get("exit_action", "")}</p>
</div>"""

        # 30天节奏控制
        rhythm_html = ""
        rhythm = shu.get("rhythm_control", {})
        if rhythm:
            focus = rhythm.get("focus", {})
            avoid_list = focus.get("avoid_list", [])
            avoid_items = "".join(f"<li>❌ {a}</li>" for a in avoid_list)
            rhythm_html = f"""<div class="card">
<h3>⏱️ 30天行動節奏</h3>
<p><span class="label">聚焦:</span> <strong>{focus.get("name", "")}</strong></p>
<p>{focus.get("description", "")}</p>
<p><span class="label">成功指標:</span> {focus.get("success_metric", "")}</p>
<h4>この期間やらないこと</h4><ul>{avoid_items}</ul>
<p><span class="label">チェックポイント:</span> {rhythm.get("checkpoint_date", "")}</p>
<p><span class="label">次の判断:</span> {rhythm.get("next_decision_point", "")}</p>
</div>"""

        # v3.1: PoC完成定義
        poc_dod_html = ""
        poc_dod = shu.get("poc_definition_of_done", {})
        if poc_dod:
            exp_items = "".join(f"<li>✓ {c}</li>" for c in poc_dod.get("experience_conditions", []))
            metric_rows = "".join(
                f"<tr><td>{m.get('metric_name', '')}</td><td>{m.get('target_value', '')}</td>"
                f"<td>{m.get('measurement_method', '')}</td></tr>"
                for m in poc_dod.get("success_metrics", [])
            )
            fallback = poc_dod.get("fallback_strategy", "")
            poc_dod_html = f"""<div class="success">
<h3>🎯 PoC完成定義（Definition of Done）</h3>
<h4>体験条件</h4><ul>{exp_items}</ul>
<h4>成功指標</h4>
<table><tr><th>指標</th><th>目標値</th><th>計測方法</th></tr>{metric_rows}</table>
<p><span class="label">フォールバック:</span> {fallback}</p>
</div>"""

        # v3.1: 2段ロケット
        rocket_html = ""
        rocket = shu.get("two_stage_rocket", {})
        if rocket:
            for stage_key, label in [
                ("stage1_minimal_pipeline", "Stage1: 最小パイプライン"),
                ("stage2_governance", "Stage2: 統制強化"),
            ]:
                stage = rocket.get(stage_key, {})
                if not stage:
                    continue
                phase_items = ""
                for p in stage.get("phases", []):
                    tasks = ", ".join(p.get("tasks", []))
                    branches = "".join(
                        f"<li>{b.get('branch_name', '')}: {b.get('trigger_condition', '')} → {b.get('description', '')}</li>"
                        for b in p.get("branches", [])
                    )
                    branch_section = f"<h5>分岐</h5><ul>{branches}</ul>" if branches else ""
                    phase_items += f"""<div class="card">
<h4>Phase {p.get("phase_number", "")}: {p.get("name", "")} ({p.get("duration", "")})</h4>
<p><span class="label">目的:</span> {p.get("purpose", "")}</p>
<p><span class="label">作業:</span> {tasks}</p>
{branch_section}
</div>"""
                gate = ", ".join(stage.get("gate_criteria", []))
                gate_html = f"<p><span class='label'>ゲート基準:</span> {gate}</p>" if gate else ""
                rocket_html += f"""<div class="highlight">
<h3>🚀 {label}: {stage.get("stage_name", "")}</h3>
<p>{stage.get("objective", "")}</p>
{gate_html}
{phase_items}
</div>"""

        return f"""<h2>📋 術 - 実行計画 v3.1</h2>
{poc_dod_html}
{rocket_html}
{first_action_html}
{cut_list_html}
{context_actions_html}
{validation_html}
{exit_html}
{phases_html}
{rhythm_html}"""

    def _build_qi_html(self, qi: dict) -> str:
        """器セクションHTMLを構築 v3.0."""
        # 実装要素
        impl_html = ""
        for impl in qi.get("implementations", []):
            risks = "".join(f"<li>{r}</li>" for r in impl.get("risks", []))
            impl_html += f"""<div class="card">
<h3>🔧 {impl.get("component", "")}</h3>
<p><span class="label">技術:</span> {impl.get("technology", "")}</p>
<p><span class="label">工数:</span> {impl.get("estimated_effort", "")}</p>
<h4>リスク</h4><ul>{risks}</ul>
</div>"""

        # v3.0: ドメイン固有技術
        domain_tech_html = ""
        domain_techs = qi.get("domain_technologies", [])
        if domain_techs:
            items = "".join(
                f"<li><strong>{t.get('technology_name', '')}</strong> ({t.get('category', '')}): {t.get('why_required', '')}"
                f"<br/>代替: {', '.join(t.get('alternatives', []))}</li>"
                for t in domain_techs
            )
            domain_tech_html = f"""<div class="highlight">
<h3>🛠️ ドメイン固有技術（具体名詞）</h3>
<ul>{items}</ul>
</div>"""

        # v3.0: 規制対応
        regulatory_html = ""
        regulations = qi.get("regulatory_considerations", [])
        if regulations:
            rows = "".join(
                f"<tr><td>{r.get('region', '')}</td><td>{r.get('regulation', '')}</td>"
                f"<td>{r.get('requirement', '')}</td><td>{r.get('implementation_impact', '')}</td></tr>"
                for r in regulations
            )
            regulatory_html = f"""<div class="warning">
<h3>📜 規制対応事項</h3>
<table>
<tr><th>地域</th><th>規制</th><th>要件</th><th>実装影響</th></tr>
{rows}
</table>
</div>"""

        # v3.0: 地理的考慮
        geographic_html = ""
        geographics = qi.get("geographic_considerations", [])
        if geographics:
            rows = "".join(
                f"<tr><td>{g.get('region', '')}</td><td>{g.get('latency_requirement', '')}</td>"
                f"<td>{g.get('infrastructure_need', '')}</td></tr>"
                for g in geographics
            )
            geographic_html = f"""<div class="card">
<h3>🌍 地理的考慮事項</h3>
<table>
<tr><th>地域</th><th>レイテンシ要件</th><th>インフラ要件</th></tr>
{rows}
</table>
</div>"""

        # ツール推奨
        tools = qi.get("tool_recommendations", [])
        tools_html = ""
        if tools:
            items = "".join(f"<li>{t}</li>" for t in tools)
            tools_html = f"<h3>🧰 推奨ツール</h3><ul>{items}</ul>"

        # 統合ポイント
        integration = qi.get("integration_points", [])
        integration_html = ""
        if integration:
            items = "".join(f"<li>{i}</li>" for i in integration)
            integration_html = f"<h3>🔗 統合ポイント</h3><ul>{items}</ul>"

        # 技術負債警告
        warnings = qi.get("technical_debt_warnings", [])
        warnings_html = ""
        if warnings:
            items = "".join(f"<li>⚠️ {w}</li>" for w in warnings)
            warnings_html = f"<h3>⚠️ 技術負債警告</h3><ul>{items}</ul>"

        # v3.1: PoC最小アーキテクチャ
        poc_arch_html = ""
        poc_arch = qi.get("poc_minimal_architecture", {})
        if poc_arch:
            comp_rows = "".join(
                f"<tr><td>{c.get('name', '')}</td><td>{c.get('purpose', '')}</td>"
                f"<td>{c.get('technology_choice', '')}</td><td>{c.get('notes', '')}</td></tr>"
                for c in poc_arch.get("components", [])
            )
            flow = poc_arch.get("data_flow_description", "")
            logging_info = poc_arch.get("minimal_logging", {})
            log_html = ""
            if logging_info:
                log_html = f"<p><span class='label'>ID戦略:</span> {logging_info.get('correlation_id_strategy', '')}</p>"
            deferred = poc_arch.get("deferred_components", [])
            deferred_html = (
                f"<p><span class='label'>後回し:</span> {', '.join(deferred)}</p>"
                if deferred
                else ""
            )
            poc_arch_html = f"""<div class="highlight">
<h3>🏗️ PoC最小アーキテクチャ</h3>
<table><tr><th>コンポーネント</th><th>目的</th><th>技術選定</th><th>備考</th></tr>{comp_rows}</table>
<p><span class="label">データフロー:</span> {flow}</p>
{log_html}{deferred_html}
</div>"""

        # v3.1: 拡張アーキテクチャ段階
        expansion_html = ""
        expansion = qi.get("expansion_stages", [])
        if expansion:
            rows = "".join(
                f"<tr><td>{s.get('stage_name', '')}</td><td>{s.get('introduction_condition', '')}</td>"
                f"<td>{', '.join(s.get('added_components', []))}</td><td>{s.get('rationale', '')}</td></tr>"
                for s in expansion
            )
            expansion_html = f"""<div class="card">
<h3>📈 拡張アーキテクチャ（導入条件付き）</h3>
<table><tr><th>段階</th><th>導入条件</th><th>追加コンポーネント</th><th>理由</th></tr>{rows}</table>
</div>"""

        # v3.1: 実装手順
        steps_html = ""
        steps = qi.get("implementation_steps", [])
        if steps:
            step_items = ""
            for s in steps:
                tasks = ", ".join(s.get("tasks", []))
                pitfalls = "".join(f"<li>⚠️ {p}</li>" for p in s.get("common_pitfalls", []))
                pitfall_section = f"<ul>{pitfalls}</ul>" if pitfalls else ""
                step_items += f"<li><strong>Step {s.get('step_number', '')}: {s.get('objective', '')}</strong><br/>作業: {tasks}{pitfall_section}</li>"
            steps_html = f"""<div class="card">
<h3>📝 実装手順</h3>
<ol>{step_items}</ol>
</div>"""

        # v3.1: 将来スケール要件
        future_html = ""
        future = qi.get("future_scale_requirements", [])
        if future:
            items = "".join(f"<li>{r}</li>" for r in future)
            future_html = f"""<div class="card"><h3>🔮 将来スケール要件（PoC範囲外）</h3><ul>{items}</ul></div>"""

        return f"""<h2>🔧 器 - 技術実装 v3.1</h2>
{poc_arch_html}
{expansion_html}
{steps_html}
{future_html}
{domain_tech_html}
{regulatory_html}
{geographic_html}
{impl_html}
{tools_html}
{integration_html}
{warnings_html}"""

    def _build_review_html(self, review: dict) -> str:
        """検証セクションHTMLを構築 v3.1（差分パッチ型）."""
        verdict = review.get("overall_verdict", "N/A")
        if hasattr(verdict, "value"):
            verdict = verdict.value

        confidence = review.get("confidence_score", 0)
        verdict_class = (
            "success" if verdict == "PASS" else "prohibition" if verdict == "REJECT" else "warning"
        )

        # v3.1 信頼度分解
        breakdown_html = ""
        breakdown = review.get("confidence_breakdown")
        if isinstance(breakdown, dict):
            rows: list[str] = []
            for key in (
                "input_sufficiency",
                "logic_consistency",
                "implementation_feasibility",
                "risk_coverage",
            ):
                comp = breakdown.get(key, {})
                if isinstance(comp, dict):
                    rows.append(
                        f"<tr><td>{comp.get('name', key)}</td>"
                        f"<td>{comp.get('score', 0):.0f}%</td>"
                        f"<td>+{comp.get('checkbox_boost', 0):.0f}点</td>"
                        f"<td>{comp.get('description', '')}</td></tr>"
                    )
            if rows:
                breakdown_html = (
                    "<h3>📊 信頼度分解</h3>"
                    "<table><tr><th>項目</th><th>スコア</th><th>チェック加点</th><th>説明</th></tr>"
                    + "".join(rows)
                    + "</table>"
                )

        # v3.1 差分パッチ型 所見（最大3件）
        findings_html = ""
        findings = review.get("findings", [])
        if findings:
            item_rows: list[str] = []
            for finding in findings[:3]:
                if not isinstance(finding, dict):
                    continue
                action = finding.get("action_type", "RECALC")
                fp = finding.get("failure_point", "")
                isc = finding.get("impact_scope", "")
                mp = finding.get("minimal_patch", {})
                cb_label = mp.get("checkbox_label", "") if isinstance(mp, dict) else ""
                si_list = finding.get("score_improvements", [])
                si_text = ", ".join(
                    f"{s.get('target_score', '')}: {s.get('current_estimate', 0):.0f}%→{s.get('improved_estimate', 0):.0f}%(+{s.get('delta', 0):.0f})"
                    for s in si_list
                    if isinstance(s, dict)
                )
                item_rows.append(
                    f"<li><strong>[{action}]</strong> {finding.get('description', '')}"
                    + (f"<br/>破綻点: {fp}" if fp else "")
                    + (f"<br/>影響範囲: {isc}" if isc else "")
                    + (f"<br/>最小パッチ: ☐ {cb_label}" if cb_label else "")
                    + (f"<br/>改善見込み: {si_text}" if si_text else "")
                    + "</li>"
                )
            findings_html = (
                f"<h3>🎯 高レバレッジ欠陥（{len(findings)}件）</h3><ul>{''.join(item_rows)}</ul>"
            )

        # v3.1 チェックポイント項目
        checkpoint_html = ""
        checkpoints = review.get("checkpoint_items", [])
        if checkpoints:
            cp_items = "".join(
                f"<li>☐ {c.get('label', '')} (+{c.get('score_boost', 0):.0f}点)"
                f" — {c.get('default_suggestion', '')}</li>"
                for c in checkpoints
                if isinstance(c, dict)
            )
            checkpoint_html = f"<h3>☑️ 確認チェックポイント</h3><ul>{cp_items}</ul>"

        return f"""<h2>✅ 検証 - 差分パッチ型判定 v3.1</h2>
<div class="{verdict_class}">
<p><span class="label">判定:</span> <strong>{verdict}</strong></p>
<p><span class="label">信頼度:</span> {confidence * 100:.0f}%</p>
</div>
{breakdown_html}
{findings_html}
{checkpoint_html}"""
