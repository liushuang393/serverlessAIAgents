"""PDF Export Skill - PDF出力機能.

画面表示と同じフォーマットでPDFを出力するスキル。
ReportLabを使用してCJK（日本語・中国語・韓国語）対応のPDFを生成。
ReportPage.tsxと同じ構成・スタイルで各Agentの結果を出力。

Example:
    >>> from agentflow.skills import PDFExportSkill
    >>>
    >>> # スキル作成
    >>> exporter = PDFExportSkill()
    >>>
    >>> # Agent結果をPDFにエクスポート
    >>> pdf_bytes = await exporter.export_agent_results(
    ...     agent_results={"dao": {...}, "fa": {...}, ...},
    ...     title="決策レポート",
    ...     report_id="PROP-123456"
    ... )
"""

from __future__ import annotations

import io
import logging
from dataclasses import dataclass
from datetime import datetime
from typing import Any, cast


logger = logging.getLogger(__name__)


@dataclass
class PDFExportConfig:
    """PDF出力設定.

    Attributes:
        include_metadata: メタデータを含めるか
        include_timestamps: タイムスタンプを含めるか
        page_size: ページサイズ（A4, Letter等）
        margin_cm: マージン（cm）
    """

    include_metadata: bool = True
    include_timestamps: bool = True
    page_size: str = "A4"
    margin_cm: float = 2.0


class PDFExportSkill:
    """PDF出力スキル.

    画面表示（ReportPage.tsx）と同じフォーマットでPDF出力。

    Features:
    - 画面と同じセクション構成（サマリー、道、法、術、器、検証）
    - CJK対応（日本語・中国語・韓国語）
    - ReportLab / HTML フォールバック
    """

    def __init__(self, config: PDFExportConfig | None = None) -> None:
        """初期化.

        Args:
            config: PDF出力設定（None の場合はデフォルト設定）
        """
        self._config = config or PDFExportConfig()
        self._logger = logging.getLogger(f"{__name__}.PDFExportSkill")
        self._has_reportlab = self._check_reportlab()

    def _check_reportlab(self) -> bool:
        """ReportLabが利用可能か確認."""
        try:
            from reportlab.lib.pagesizes import A4

            return True
        except ImportError:
            self._logger.warning("ReportLab not installed. PDF export will use HTML fallback.")
            return False

    def _to_dict(self, obj: Any) -> dict[str, Any]:
        """Pydanticオブジェクトまたはdictをdictに変換."""
        if hasattr(obj, "model_dump"):
            return cast("dict[str, Any]", obj.model_dump())
        if isinstance(obj, dict):
            return obj
        return {}

    def _get_value(self, obj: Any) -> str:
        """Enumまたは通常の値を文字列に変換."""
        if hasattr(obj, "value"):
            return str(obj.value)
        return str(obj) if obj else "N/A"

    async def export_agent_results(
        self,
        agent_results: dict[str, Any],
        title: str = "決策レポート",
        report_id: str = "",
        original_question: str = "",
        executive_summary: dict[str, Any] | None = None,
        review_result: dict[str, Any] | None = None,
    ) -> bytes:
        """Agent結果をPDFにエクスポート（画面と同じフォーマット）.

        Args:
            agent_results: 各Agentの出力結果（{"dao": {...}, "fa": {...}, ...}）
            title: レポートタイトル
            report_id: レポートID
            original_question: 元の質問文
            executive_summary: エグゼクティブサマリー
            review_result: 検証結果

        Returns:
            PDFバイナリデータ
        """
        self._logger.info(f"Exporting agent results to PDF: {report_id}")

        if self._has_reportlab:
            return self._generate_with_reportlab(
                agent_results, title, report_id, original_question, executive_summary, review_result
            )
        return self._generate_html_fallback(
            agent_results, title, report_id, original_question, executive_summary, review_result
        )

    def _generate_with_reportlab(
        self,
        agent_results: dict[str, Any],
        title: str,
        report_id: str,
        original_question: str,
        executive_summary: dict[str, Any] | None,
        review_result: dict[str, Any] | None,
    ) -> bytes:
        """ReportLabでPDF生成（画面と同じフォーマット）."""
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

        # CJKフォント登録
        pdfmetrics.registerFont(UnicodeCIDFont("HeiseiMin-W3"))
        cjk_font = "HeiseiMin-W3"

        buffer = io.BytesIO()
        margin = self._config.margin_cm * cm
        doc = SimpleDocTemplate(buffer, pagesize=A4, topMargin=margin, bottomMargin=margin)
        styles = getSampleStyleSheet()
        elements: list[Any] = []

        # スタイル定義
        title_style = ParagraphStyle("CJKTitle", parent=styles["Title"], fontSize=18, fontName=cjk_font)
        heading_style = ParagraphStyle("CJKHeading", parent=styles["Heading2"], fontName=cjk_font, spaceAfter=10)
        subheading_style = ParagraphStyle("CJKSubHeading", parent=styles["Heading3"], fontName=cjk_font, fontSize=11)
        normal_style = ParagraphStyle(
            "CJKNormal", parent=styles["Normal"], fontName=cjk_font, spaceBefore=3, spaceAfter=3
        )
        highlight_style = ParagraphStyle("CJKHighlight", parent=normal_style, backColor=colors.Color(0.9, 0.95, 1))
        warning_style = ParagraphStyle("CJKWarning", parent=normal_style, backColor=colors.Color(1, 0.95, 0.9))

        # dictに変換
        dao = self._to_dict(agent_results.get("dao", {}))
        fa = self._to_dict(agent_results.get("fa", {}))
        shu = self._to_dict(agent_results.get("shu", {}))
        qi = self._to_dict(agent_results.get("qi", {}))
        review = self._to_dict(review_result or agent_results.get("review", {}))
        summary = self._to_dict(executive_summary or {})

        # ========== タイトル ==========
        elements.append(Paragraph(f"{title} v3.0", title_style))
        elements.append(Paragraph(f"Report ID: {report_id}", normal_style))
        elements.append(Paragraph(f"生成日時: {datetime.now().strftime('%Y-%m-%d %H:%M')}", normal_style))
        elements.append(Spacer(1, 0.5 * cm))

        # ========== 元の質問 ==========
        if original_question:
            elements.append(Paragraph("📝 分析対象の質問", heading_style))
            elements.append(Paragraph(original_question, highlight_style))
            elements.append(Spacer(1, 0.3 * cm))

        # ========== エグゼクティブサマリー ==========
        elements.append(Paragraph("📊 エグゼクティブサマリー", heading_style))
        if summary.get("one_line_decision"):
            elements.append(Paragraph(f"<b>💡 結論:</b> {summary.get('one_line_decision', '')}", highlight_style))
        if summary.get("essence_statement"):
            elements.append(Paragraph(f"<b>📍 本質:</b> {summary.get('essence_statement', '')}", normal_style))
        if summary.get("first_step"):
            elements.append(Paragraph(f"<b>🎯 最初の一歩:</b> {summary.get('first_step', '')}", normal_style))
        if summary.get("strategic_prohibition_summary"):
            elements.append(
                Paragraph(
                    f"<b>⛔ 戦略的禁止:</b> {summary.get('strategic_prohibition_summary', '')}",
                    warning_style,
                )
            )
        if summary.get("exit_criteria_summary"):
            elements.append(Paragraph(f"<b>🚪 撤退基準:</b> {summary.get('exit_criteria_summary', '')}", warning_style))
        key_risks = summary.get("key_risks", [])
        if key_risks:
            elements.append(Paragraph("<b>⚠️ 主要リスク:</b>", normal_style))
            for risk in key_risks:
                elements.append(Paragraph(f"  • {risk}", normal_style))
        elements.append(Spacer(1, 0.5 * cm))

        # ========== 道セクション ==========
        elements.append(PageBreak())
        elements.append(Paragraph("🎯 道 / 本質分析 v3.0", heading_style))

        problem_type = self._get_value(dao.get("problem_type", ""))
        problem_nature = self._get_value(dao.get("problem_nature", ""))
        elements.append(Paragraph(f"<b>問題タイプ:</b> {problem_type}", normal_style))
        elements.append(Paragraph(f"<b>問題の本質的性質:</b> {problem_nature}", normal_style))
        elements.append(Paragraph(f"<b>📍 本質（一文）:</b> {dao.get('essence', 'N/A')}", highlight_style))

        # 本質導出プロセス
        ed = dao.get("essence_derivation", {})
        if ed:
            elements.append(Paragraph("🔍 本質導出プロセス", subheading_style))
            elements.append(Paragraph(f"表面的問題: {ed.get('surface_problem', '')}", normal_style))
            elements.append(Paragraph(f"一段深い理由: {ed.get('underlying_why', '')}", normal_style))
            elements.append(Paragraph(f"根本制約: {ed.get('root_constraint', '')}", normal_style))
            elements.append(Paragraph(f"<b>本質の一文:</b> {ed.get('essence_statement', '')}", highlight_style))

        # 既存代替手段
        alternatives = dao.get("existing_alternatives", [])
        if alternatives:
            elements.append(Paragraph("🔄 既存代替手段（なぜ使えないか）", subheading_style))
            for alt in alternatives:
                elements.append(
                    Paragraph(
                        f"• <b>{alt.get('name', '')}</b>: {alt.get('why_not_viable', '')} "
                        f"(制約: {alt.get('specific_constraint', '')})",
                        normal_style,
                    )
                )

        # 不可変制約
        constraints = dao.get("immutable_constraints", [])
        if constraints:
            elements.append(Paragraph("🔒 不可変制約", subheading_style))
            for c in constraints:
                elements.append(Paragraph(f"  🔒 {c}", normal_style))

        # 隠れた前提
        assumptions = dao.get("hidden_assumptions", [])
        if assumptions:
            elements.append(Paragraph("💭 隠れた前提", subheading_style))
            for a in assumptions:
                elements.append(Paragraph(f"  💭 {a}", normal_style))

        # 因果齿轮
        gears = dao.get("causal_gears", [])
        if gears:
            elements.append(Paragraph("⚙️ 因果齿轮", subheading_style))
            bottleneck = dao.get("bottleneck_gear", "")
            for gear in gears:
                bn_mark = " [ボトルネック]" if gear.get("name") == bottleneck else ""
                elements.append(
                    Paragraph(
                        f"  ⚙️ {gear.get('name', '')} (Leverage: {gear.get('leverage', '')}){bn_mark}",
                        normal_style,
                    )
                )
                elements.append(Paragraph(f"     {gear.get('description', '')}", normal_style))

        # 死穴
        traps = dao.get("death_traps", [])
        if traps:
            elements.append(Paragraph("💀 死穴（禁忌）", subheading_style))
            for trap in traps:
                elements.append(
                    Paragraph(
                        f"⚠️ <b>{trap.get('action', '')}</b> ({trap.get('severity', '')}): {trap.get('reason', '')}",
                        warning_style,
                    )
                )
        elements.append(Spacer(1, 0.3 * cm))

        # ========== 法セクション ==========
        elements.append(PageBreak())
        elements.append(Paragraph("🛤️ 法 / 戦略選定 v3.0", heading_style))

        # 戦略的禁止事項
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            elements.append(Paragraph("🚫 戦略的禁止事項（絶対にやってはいけない）", subheading_style))
            for p in prohibitions:
                elements.append(
                    Paragraph(
                        f"⛔ <b>{p.get('prohibition', '')}</b>: {p.get('rationale', '')} "
                        f"→ 違反結果: {p.get('violation_consequence', '')}",
                        warning_style,
                    )
                )

        # 差別化軸
        diff_axis = fa.get("differentiation_axis", {})
        if diff_axis:
            elements.append(Paragraph("🎯 差別化軸", subheading_style))
            elements.append(Paragraph(f"<b>勝負する軸:</b> {diff_axis.get('axis_name', '')}", highlight_style))
            elements.append(Paragraph(f"理由: {diff_axis.get('why_this_axis', '')}", normal_style))
            elements.append(Paragraph(f"<b>勝負しない軸:</b> {diff_axis.get('not_this_axis', '')}", normal_style))

        # 既存解が使えない理由
        why_existing = fa.get("why_existing_fails", "")
        if why_existing:
            elements.append(Paragraph(f"<b>⚠️ 既存解が使えない理由:</b> {why_existing}", warning_style))

        # 推奨パス
        for path in fa.get("recommended_paths", []):
            strategy_type = self._get_value(path.get("strategy_type", ""))
            elements.append(Paragraph(f"📌 {path.get('name', '')} ({strategy_type})", subheading_style))
            elements.append(Paragraph(path.get("description", ""), normal_style))
            elements.append(
                Paragraph(
                    f"成功確率: {(path.get('success_probability', 0) * 100):.0f}% | "
                    f"価値実現: {path.get('time_to_value', '')} | "
                    f"可逆性: {path.get('reversibility', '')}",
                    normal_style,
                )
            )
            # メリット/デメリット
            pros = path.get("pros", [])
            cons = path.get("cons", [])
            if pros:
                elements.append(Paragraph("  メリット:", normal_style))
                for p in pros:
                    elements.append(Paragraph(f"    + {p}", normal_style))
            if cons:
                elements.append(Paragraph("  デメリット:", normal_style))
                for c in cons:
                    elements.append(Paragraph(f"    - {c}", normal_style))
        elements.append(Spacer(1, 0.3 * cm))

        # ========== 術セクション ==========
        elements.append(PageBreak())
        elements.append(Paragraph("📋 術 / 実行計画 v3.0", heading_style))

        # 最初の一歩
        first_action = shu.get("first_action", "")
        if first_action:
            elements.append(Paragraph(f"🎯 <b>最初の一歩:</b> {first_action}", highlight_style))

        # 切り捨てリスト
        cut_list = shu.get("cut_list", [])
        if cut_list:
            elements.append(Paragraph("✂️ 切り捨てリスト（最初の30日間でやらないこと）", subheading_style))
            for c in cut_list:
                elements.append(Paragraph(f"  ❌ {c}", warning_style))

        # 文脈特化行動
        context_actions = shu.get("context_specific_actions", [])
        if context_actions:
            elements.append(Paragraph("💎 文脈特化行動（この問題固有）", subheading_style))
            for a in context_actions:
                elements.append(Paragraph(f"• <b>{a.get('action', '')}</b>", normal_style))
                elements.append(Paragraph(f"  理由: {a.get('why_this_context', '')}", normal_style))
                elements.append(Paragraph(f"  期待出力: {a.get('expected_output', '')}", highlight_style))

        # 単一検証ポイント
        validation = shu.get("single_validation_point", {})
        if validation:
            elements.append(Paragraph("🔬 単一検証ポイント（PoCで絶対に検証すべき1点）", subheading_style))
            elements.append(Paragraph(f"検証対象: {validation.get('validation_target', '')}", normal_style))
            elements.append(Paragraph(f"成功基準: {validation.get('success_criteria', '')}", normal_style))
            elements.append(Paragraph(f"失敗時行動: {validation.get('failure_action', '')}", warning_style))

        # 撤退基準
        exit_criteria = shu.get("exit_criteria", {})
        if exit_criteria:
            elements.append(Paragraph("🚪 撤退基準（どこで止めるか）", subheading_style))
            elements.append(Paragraph(f"チェックポイント: {exit_criteria.get('checkpoint', '')}", normal_style))
            elements.append(Paragraph(f"撤退トリガー: {exit_criteria.get('exit_trigger', '')}", warning_style))
            elements.append(Paragraph(f"撤退時行動: {exit_criteria.get('exit_action', '')}", normal_style))

        # フェーズ
        phases = shu.get("phases", [])
        if phases:
            elements.append(Paragraph("📅 フェーズ", subheading_style))
            for phase in phases:
                elements.append(
                    Paragraph(
                        f"Phase {phase.get('phase_number', '?')}: {phase.get('name', '')} "
                        f"({phase.get('duration', '')})",
                        normal_style,
                    )
                )
                actions = phase.get("actions", [])
                if actions:
                    for action in actions[:3]:
                        elements.append(Paragraph(f"    • {action}", normal_style))
        elements.append(Spacer(1, 0.3 * cm))

        # ========== 器セクション ==========
        elements.append(PageBreak())
        elements.append(Paragraph("🔧 器 / 技術実装 v3.0", heading_style))

        # ドメイン固有技術
        domain_techs = qi.get("domain_technologies", [])
        if domain_techs:
            elements.append(Paragraph("🛠️ ドメイン固有技術（具体名詞）", subheading_style))
            for t in domain_techs:
                elements.append(
                    Paragraph(
                        f"• <b>{t.get('technology_name', '')}</b> ({t.get('category', '')}): "
                        f"{t.get('why_required', '')}",
                        highlight_style,
                    )
                )
                alts = t.get("alternatives", [])
                if alts:
                    elements.append(Paragraph(f"  代替: {', '.join(alts)}", normal_style))

        # 規制対応
        regulations = qi.get("regulatory_considerations", [])
        if regulations:
            elements.append(Paragraph("📜 規制対応事項", subheading_style))
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
            elements.append(Paragraph("🌍 地理的考慮事項", subheading_style))
            for g in geographics:
                elements.append(
                    Paragraph(
                        f"• {g.get('region', '')}: レイテンシ {g.get('latency_requirement', '')} | "
                        f"インフラ {g.get('infrastructure_need', '')}",
                        normal_style,
                    )
                )

        # 実装要素
        implementations = qi.get("implementations", [])
        if implementations:
            elements.append(Paragraph("🔧 実装要素", subheading_style))
            for impl in implementations:
                elements.append(
                    Paragraph(
                        f"• {impl.get('component', '')}: {impl.get('technology', '')} "
                        f"({impl.get('estimated_effort', '')})",
                        normal_style,
                    )
                )
                risks = impl.get("risks", [])
                if risks:
                    elements.append(Paragraph(f"  ⚠️ リスク: {', '.join(risks)}", warning_style))

        # ツール推奨
        tool_recommendations = qi.get("tool_recommendations", [])
        if tool_recommendations:
            elements.append(Paragraph("🧰 ツール推奨", subheading_style))
            for rec in tool_recommendations:
                if isinstance(rec, dict):
                    category = rec.get("category", "")
                    tools_list = rec.get("tools", [])
                    rationale = rec.get("rationale", "")
                    # tools_listが文字列のリストの場合
                    if tools_list and isinstance(tools_list[0], str):
                        tools_str = ", ".join(tools_list)
                    else:
                        tools_str = str(tools_list)
                    elements.append(Paragraph(f"• {category}: {tools_str}", normal_style))
                    if rationale:
                        elements.append(Paragraph(f"  理由: {rationale}", normal_style))
                elif isinstance(rec, str):
                    # 単純な文字列の場合
                    elements.append(Paragraph(f"• {rec}", normal_style))
        elements.append(Spacer(1, 0.3 * cm))

        # ========== 検証セクション ==========
        elements.append(PageBreak())
        elements.append(Paragraph("🔍 検証 / ReviewAgent", heading_style))

        verdict = self._get_value(review.get("overall_verdict", "N/A"))
        confidence = review.get("confidence_score", 0)
        verdict_style = highlight_style if verdict == "PASS" else warning_style
        elements.append(Paragraph(f"<b>総合判定: {verdict}</b> | 信頼度: {confidence * 100:.0f}%", verdict_style))

        # 指摘事項
        findings = review.get("findings", [])
        if findings:
            elements.append(Paragraph("📋 指摘事項", subheading_style))
            for f in findings:
                severity = f.get("severity", "")
                sty = warning_style if severity in ["CRITICAL", "WARNING"] else normal_style
                elements.append(Paragraph(f"• [{severity}] {f.get('description', '')}", sty))
                if f.get("suggested_revision"):
                    elements.append(Paragraph(f"  💡 修正提案: {f.get('suggested_revision', '')}", normal_style))

        # 最終警告
        final_warnings = review.get("final_warnings", [])
        if final_warnings:
            elements.append(Paragraph("⚠️ 最終警告（意思決定者への注意事項）", subheading_style))
            for w in final_warnings:
                elements.append(Paragraph(f"  • {w}", warning_style))
        elements.append(Spacer(1, 0.5 * cm))

        # ========== 署名欄 ==========
        elements.append(Paragraph("✍️ 署名欄", heading_style))
        sig_data = [["承認者", "", "日付", ""], ["署名", "", "", ""]]
        sig_table = Table(sig_data, colWidths=[3 * cm, 6 * cm, 2 * cm, 4 * cm])
        sig_table.setStyle(
            TableStyle(
                [
                    ("GRID", (0, 0), (-1, -1), 0.5, colors.black),
                    ("FONTNAME", (0, 0), (-1, -1), cjk_font),
                ]
            )
        )
        elements.append(sig_table)

        doc.build(elements)
        return buffer.getvalue()

    def _generate_html_fallback(
        self,
        agent_results: dict[str, Any],
        title: str,
        report_id: str,
        original_question: str,
        executive_summary: dict[str, Any] | None,
        review_result: dict[str, Any] | None,
    ) -> bytes:
        """HTML形式でのフォールバック出力（画面と同じフォーマット）."""
        now = datetime.now().strftime("%Y-%m-%d %H:%M")

        # dictに変換
        dao = self._to_dict(agent_results.get("dao", {}))
        fa = self._to_dict(agent_results.get("fa", {}))
        shu = self._to_dict(agent_results.get("shu", {}))
        qi = self._to_dict(agent_results.get("qi", {}))
        review = self._to_dict(review_result or agent_results.get("review", {}))
        summary = self._to_dict(executive_summary or {})

        # HTMLエスケープ関数
        def esc(text: Any) -> str:
            s = str(text) if text else ""
            return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

        # サマリーセクション
        summary_html = self._build_summary_html(summary, esc)
        # 道セクション
        dao_html = self._build_dao_html(dao, esc)
        # 法セクション
        fa_html = self._build_fa_html(fa, esc)
        # 術セクション
        shu_html = self._build_shu_html(shu, esc)
        # 器セクション
        qi_html = self._build_qi_html(qi, esc)
        # 検証セクション
        review_html = self._build_review_html(review, esc)

        html = f"""<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="UTF-8">
<title>{title}</title>
<style>
@page {{ size: A4; margin: 2cm; }}
body {{ font-family: 'Yu Gothic', 'Hiragino Sans', 'Meiryo', sans-serif; max-width: 900px; margin: 0 auto; padding: 30px; color: #333; line-height: 1.6; background: #fff; }}
.header {{ text-align: center; border-bottom: 3px solid #2c3e50; padding-bottom: 20px; margin-bottom: 30px; }}
.header h1 {{ color: #2c3e50; font-size: 28px; margin-bottom: 10px; }}
.header .meta {{ color: #7f8c8d; font-size: 14px; }}
.section {{ margin: 30px 0; page-break-inside: avoid; }}
.section h2 {{ color: #2c3e50; font-size: 20px; border-bottom: 2px solid #3498db; padding-bottom: 8px; margin-bottom: 15px; }}
.section h3 {{ color: #34495e; font-size: 16px; margin: 15px 0 10px; }}
.highlight {{ background: #e8f4f8; padding: 10px 15px; border-radius: 5px; margin: 10px 0; }}
.warning {{ background: #fef3e2; padding: 10px 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #e67e22; }}
.danger {{ background: #fde8e8; padding: 10px 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #e74c3c; }}
.success {{ background: #e8f8e8; padding: 10px 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #27ae60; }}
ul {{ margin: 10px 0; padding-left: 25px; }}
li {{ margin: 5px 0; }}
table {{ width: 100%; border-collapse: collapse; margin: 15px 0; }}
th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; }}
th {{ background: #f4f4f4; }}
.footer {{ text-align: center; color: #999; font-size: 11px; margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; }}
@media print {{ .section {{ page-break-inside: avoid; }} }}
</style>
</head>
<body>
<div class="header">
<h1>{esc(title)} v3.0</h1>
<p class="meta">Report ID: {esc(report_id)}</p>
<p class="meta">生成日時: {now}</p>
</div>

<div class="section">
<h2>📝 分析対象の質問</h2>
<div class="highlight">{esc(original_question)}</div>
</div>

{summary_html}
{dao_html}
{fa_html}
{shu_html}
{qi_html}
{review_html}

<div class="section">
<h2>✍️ 署名欄</h2>
<table>
<tr><th>承認者</th><td style="width:40%"></td><th>日付</th><td style="width:25%"></td></tr>
<tr><th>署名</th><td colspan="3"></td></tr>
</table>
</div>

<div class="footer">
<p>本レポートは AgentFlow PDF Export Skill により自動生成されました</p>
</div>
</body>
</html>"""
        return html.encode("utf-8")

    def _build_summary_html(self, summary: dict[str, Any], esc: Any) -> str:
        """サマリーセクションのHTML生成."""
        risks_html = ""
        key_risks = summary.get("key_risks", [])
        if key_risks:
            risks_html = "<ul>" + "".join(f"<li>{esc(r)}</li>" for r in key_risks) + "</ul>"

        return f"""<div class="section">
<h2>📊 エグゼクティブサマリー</h2>
<div class="highlight"><strong>💡 結論:</strong> {esc(summary.get("one_line_decision", "N/A"))}</div>
<p><strong>📍 本質:</strong> {esc(summary.get("essence_statement", ""))}</p>
<p><strong>🎯 最初の一歩:</strong> {esc(summary.get("first_step", ""))}</p>
<div class="warning"><strong>⛔ 戦略的禁止:</strong> {esc(summary.get("strategic_prohibition_summary", ""))}</div>
<div class="warning"><strong>🚪 撤退基準:</strong> {esc(summary.get("exit_criteria_summary", ""))}</div>
<h3>⚠️ 主要リスク</h3>
{risks_html}
</div>"""

    def _build_dao_html(self, dao: dict[str, Any], esc: Any) -> str:
        """道セクションのHTML生成."""
        # 本質導出プロセス
        ed = dao.get("essence_derivation", {})
        ed_html = ""
        if ed:
            ed_html = f"""<h3>🔍 本質導出プロセス</h3>
<p>表面的問題: {esc(ed.get("surface_problem", ""))}</p>
<p>一段深い理由: {esc(ed.get("underlying_why", ""))}</p>
<p>根本制約: {esc(ed.get("root_constraint", ""))}</p>
<div class="highlight"><strong>本質の一文:</strong> {esc(ed.get("essence_statement", ""))}</div>"""

        # 既存代替手段
        alts = dao.get("existing_alternatives", [])
        alts_html = ""
        if alts:
            alts_items = "".join(
                f"<li><strong>{esc(a.get('name', ''))}</strong>: {esc(a.get('why_not_viable', ''))} "
                f"(制約: {esc(a.get('specific_constraint', ''))})</li>"
                for a in alts
            )
            alts_html = f"<h3>🔄 既存代替手段（なぜ使えないか）</h3><ul>{alts_items}</ul>"

        # 不可変制約
        constraints = dao.get("immutable_constraints", [])
        constraints_html = ""
        if constraints:
            constraints_html = (
                "<h3>🔒 不可変制約</h3><ul>" + "".join(f"<li>🔒 {esc(c)}</li>" for c in constraints) + "</ul>"
            )

        # 隠れた前提
        assumptions = dao.get("hidden_assumptions", [])
        assumptions_html = ""
        if assumptions:
            assumptions_html = (
                "<h3>💭 隠れた前提</h3><ul>" + "".join(f"<li>💭 {esc(a)}</li>" for a in assumptions) + "</ul>"
            )

        # 因果歯車
        gears = dao.get("causal_gears", [])
        gears_html = ""
        if gears:
            bottleneck = dao.get("bottleneck_gear", "")
            gears_items = ""
            for g in gears:
                bn_mark = " <strong>[ボトルネック]</strong>" if g.get("name") == bottleneck else ""
                gears_items += f"<li>⚙️ {esc(g.get('name', ''))} (Leverage: {esc(g.get('leverage', ''))}){bn_mark}<br>{esc(g.get('description', ''))}</li>"
            gears_html = f"<h3>⚙️ 因果歯車</h3><ul>{gears_items}</ul>"

        # 死穴
        traps = dao.get("death_traps", [])
        traps_html = ""
        if traps:
            traps_items = "".join(
                f'<li class="danger">⚠️ <strong>{esc(t.get("action", ""))}</strong> ({esc(t.get("severity", ""))}): {esc(t.get("reason", ""))}</li>'
                for t in traps
            )
            traps_html = f"<h3>💀 死穴（禁忌）</h3><ul>{traps_items}</ul>"

        problem_type = self._get_value(dao.get("problem_type", ""))
        problem_nature = self._get_value(dao.get("problem_nature", ""))

        return f"""<div class="section">
<h2>🎯 道 / 本質分析 v3.0</h2>
<p><strong>問題タイプ:</strong> {esc(problem_type)}</p>
<p><strong>問題の本質的性質:</strong> {esc(problem_nature)}</p>
<div class="highlight"><strong>📍 本質（一文）:</strong> {esc(dao.get("essence", "N/A"))}</div>
{ed_html}
{alts_html}
{constraints_html}
{assumptions_html}
{gears_html}
{traps_html}
</div>"""

    def _build_fa_html(self, fa: dict[str, Any], esc: Any) -> str:
        """法セクションのHTML生成."""
        # 戦略的禁止事項
        prohibitions = fa.get("strategic_prohibitions", [])
        prohibitions_html = ""
        if prohibitions:
            prohibitions_items = "".join(
                f'<li class="danger">⛔ <strong>{esc(p.get("prohibition", ""))}</strong>: {esc(p.get("rationale", ""))} → 違反結果: {esc(p.get("violation_consequence", ""))}</li>'
                for p in prohibitions
            )
            prohibitions_html = f"<h3>🚫 戦略的禁止事項（絶対にやってはいけない）</h3><ul>{prohibitions_items}</ul>"

        # 差別化軸
        diff_axis = fa.get("differentiation_axis", {})
        diff_html = ""
        if diff_axis:
            diff_html = f"""<h3>🎯 差別化軸</h3>
<div class="highlight"><strong>勝負する軸:</strong> {esc(diff_axis.get("axis_name", ""))}</div>
<p>理由: {esc(diff_axis.get("why_this_axis", ""))}</p>
<p><strong>勝負しない軸:</strong> {esc(diff_axis.get("not_this_axis", ""))}</p>"""

        # 既存解が使えない理由
        why_existing = fa.get("why_existing_fails", "")
        why_html = (
            f'<div class="warning"><strong>⚠️ 既存解が使えない理由:</strong> {esc(why_existing)}</div>'
            if why_existing
            else ""
        )

        # 推奨パス
        paths = fa.get("recommended_paths", [])
        paths_html = ""
        if paths:
            for path in paths:
                strategy_type = self._get_value(path.get("strategy_type", ""))
                pros = path.get("pros", [])
                cons = path.get("cons", [])
                pros_html = "<br>メリット: " + ", ".join(f"+ {esc(p)}" for p in pros) if pros else ""
                cons_html = "<br>デメリット: " + ", ".join(f"- {esc(c)}" for c in cons) if cons else ""
                paths_html += f"""<div class="highlight">
<strong>📌 {esc(path.get("name", ""))} ({esc(strategy_type)})</strong><br>
{esc(path.get("description", ""))}<br>
成功確率: {(path.get("success_probability", 0) * 100):.0f}% | 価値実現: {esc(path.get("time_to_value", ""))} | 可逆性: {esc(path.get("reversibility", ""))}
{pros_html}{cons_html}
</div>"""

        return f"""<div class="section">
<h2>🛤️ 法 / 戦略選定 v3.0</h2>
{prohibitions_html}
{diff_html}
{why_html}
<h3>📌 推奨パス</h3>
{paths_html}
</div>"""

    def _build_shu_html(self, shu: dict[str, Any], esc: Any) -> str:
        """術セクションのHTML生成."""
        # 最初の一歩
        first_action = shu.get("first_action", "")
        first_html = (
            f'<div class="success"><strong>🎯 最初の一歩:</strong> {esc(first_action)}</div>' if first_action else ""
        )

        # 切り捨てリスト
        cut_list = shu.get("cut_list", [])
        cut_html = ""
        if cut_list:
            cut_html = (
                '<h3>✂️ 切り捨てリスト（最初の30日間でやらないこと）</h3><ul class="warning">'
                + "".join(f"<li>❌ {esc(c)}</li>" for c in cut_list)
                + "</ul>"
            )

        # 文脈特化行動
        context_actions = shu.get("context_specific_actions", [])
        context_html = ""
        if context_actions:
            context_items = ""
            for a in context_actions:
                context_items += f"""<li><strong>{esc(a.get("action", ""))}</strong><br>
理由: {esc(a.get("why_this_context", ""))}<br>
<span class="highlight">期待出力: {esc(a.get("expected_output", ""))}</span></li>"""
            context_html = f"<h3>💎 文脈特化行動（この問題固有）</h3><ul>{context_items}</ul>"

        # 単一検証ポイント
        validation = shu.get("single_validation_point", {})
        validation_html = ""
        if validation:
            validation_html = f"""<h3>🔬 単一検証ポイント（PoCで絶対に検証すべき1点）</h3>
<p>検証対象: {esc(validation.get("validation_target", ""))}</p>
<p>成功基準: {esc(validation.get("success_criteria", ""))}</p>
<div class="warning">失敗時行動: {esc(validation.get("failure_action", ""))}</div>"""

        # 撤退基準
        exit_criteria = shu.get("exit_criteria", {})
        exit_html = ""
        if exit_criteria:
            exit_html = f"""<h3>🚪 撤退基準（どこで止めるか）</h3>
<p>チェックポイント: {esc(exit_criteria.get("checkpoint", ""))}</p>
<div class="warning">撤退トリガー: {esc(exit_criteria.get("exit_trigger", ""))}</div>
<p>撤退時行動: {esc(exit_criteria.get("exit_action", ""))}</p>"""

        # フェーズ
        phases = shu.get("phases", [])
        phases_html = ""
        if phases:
            phases_items = ""
            for phase in phases:
                actions = phase.get("actions", [])
                actions_html = "<ul>" + "".join(f"<li>{esc(a)}</li>" for a in actions[:3]) + "</ul>" if actions else ""
                phases_items += f"""<li><strong>Phase {phase.get("phase_number", "?")}: {esc(phase.get("name", ""))}</strong> ({esc(phase.get("duration", ""))}){actions_html}</li>"""
            phases_html = f"<h3>📅 フェーズ</h3><ul>{phases_items}</ul>"

        return f"""<div class="section">
<h2>📋 術 / 実行計画 v3.0</h2>
{first_html}
{cut_html}
{context_html}
{validation_html}
{exit_html}
{phases_html}
</div>"""

    def _build_qi_html(self, qi: dict[str, Any], esc: Any) -> str:
        """器セクションのHTML生成."""
        # ドメイン固有技術
        domain_techs = qi.get("domain_technologies", [])
        techs_html = ""
        if domain_techs:
            techs_items = ""
            for t in domain_techs:
                alts = t.get("alternatives", [])
                alts_str = f" (代替: {', '.join(esc(a) for a in alts)})" if alts else ""
                techs_items += f'<li class="highlight"><strong>{esc(t.get("technology_name", ""))}</strong> ({esc(t.get("category", ""))}): {esc(t.get("why_required", ""))}{alts_str}</li>'
            techs_html = f"<h3>🛠️ ドメイン固有技術（具体名詞）</h3><ul>{techs_items}</ul>"

        # 規制対応
        regulations = qi.get("regulatory_considerations", [])
        reg_html = ""
        if regulations:
            reg_rows = "".join(
                f"<tr><td>{esc(r.get('region', ''))}</td><td>{esc(r.get('regulation', ''))}</td><td>{esc(r.get('requirement', ''))}</td><td>{esc(r.get('implementation_impact', ''))}</td></tr>"
                for r in regulations
            )
            reg_html = f"""<h3>📜 規制対応事項</h3>
<table><tr><th>地域</th><th>規制</th><th>要件</th><th>影響</th></tr>{reg_rows}</table>"""

        # 地理的考慮
        geographics = qi.get("geographic_considerations", [])
        geo_html = ""
        if geographics:
            geo_items = "".join(
                f"<li>{esc(g.get('region', ''))}: レイテンシ {esc(g.get('latency_requirement', ''))} | インフラ {esc(g.get('infrastructure_need', ''))}</li>"
                for g in geographics
            )
            geo_html = f"<h3>🌍 地理的考慮事項</h3><ul>{geo_items}</ul>"

        # 実装要素
        implementations = qi.get("implementations", [])
        impl_html = ""
        if implementations:
            impl_items = ""
            for impl in implementations:
                risks = impl.get("risks", [])
                risks_str = (
                    f' <span class="warning">⚠️ リスク: {", ".join(esc(r) for r in risks)}</span>' if risks else ""
                )
                impl_items += f"<li>{esc(impl.get('component', ''))}: {esc(impl.get('technology', ''))} ({esc(impl.get('estimated_effort', ''))}){risks_str}</li>"
            impl_html = f"<h3>🔧 実装要素</h3><ul>{impl_items}</ul>"

        # ツール推奨
        tool_recommendations = qi.get("tool_recommendations", [])
        tools_html = ""
        if tool_recommendations:
            tools_items = ""
            for rec in tool_recommendations:
                if isinstance(rec, dict):
                    category = esc(rec.get("category", ""))
                    tools_list = rec.get("tools", [])
                    rationale = esc(rec.get("rationale", ""))
                    if tools_list and isinstance(tools_list[0], str):
                        tools_str = ", ".join(esc(t) for t in tools_list)
                    else:
                        tools_str = str(tools_list)
                    tools_items += f"<li><strong>{category}:</strong> {tools_str}"
                    if rationale:
                        tools_items += f" <em>({rationale})</em>"
                    tools_items += "</li>"
                elif isinstance(rec, str):
                    tools_items += f"<li>{esc(rec)}</li>"
            tools_html = f"<h3>🧰 ツール推奨</h3><ul>{tools_items}</ul>"

        return f"""<div class="section">
<h2>🔧 器 / 技術実装 v3.0</h2>
{techs_html}
{reg_html}
{geo_html}
{impl_html}
{tools_html}
</div>"""

    def _build_review_html(self, review: dict[str, Any], esc: Any) -> str:
        """検証セクションのHTML生成."""
        verdict = self._get_value(review.get("overall_verdict", "N/A"))
        confidence = review.get("confidence_score", 0)
        verdict_class = "success" if verdict == "PASS" else "warning"

        # 指摘事項
        findings = review.get("findings", [])
        findings_html = ""
        if findings:
            findings_items = ""
            for f in findings:
                severity = f.get("severity", "")
                sev_class = "danger" if severity in ["CRITICAL", "WARNING"] else ""
                revision = f.get("suggested_revision", "")
                revision_html = f"<br>💡 修正提案: {esc(revision)}" if revision else ""
                findings_items += (
                    f'<li class="{sev_class}">[{esc(severity)}] {esc(f.get("description", ""))}{revision_html}</li>'
                )
            findings_html = f"<h3>📋 指摘事項</h3><ul>{findings_items}</ul>"

        # 最終警告
        final_warnings = review.get("final_warnings", [])
        warnings_html = ""
        if final_warnings:
            warnings_html = (
                '<h3>⚠️ 最終警告（意思決定者への注意事項）</h3><ul class="warning">'
                + "".join(f"<li>{esc(w)}</li>" for w in final_warnings)
                + "</ul>"
            )

        return f"""<div class="section">
<h2>🔍 検証 / ReviewAgent</h2>
<div class="{verdict_class}"><strong>総合判定: {esc(verdict)}</strong> | 信頼度: {confidence * 100:.0f}%</div>
{findings_html}
{warnings_html}
</div>"""


__all__ = [
    "PDFExportConfig",
    "PDFExportSkill",
]
