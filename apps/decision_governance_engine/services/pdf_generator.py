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
            from reportlab.lib.pagesizes import A4  # noqa: F401
            return True
        except ImportError:
            self._logger.warning("ReportLab not installed. PDF export will use HTML fallback.")
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

        try:
            if self._has_reportlab:
                return self._generate_with_reportlab(report)
            return self._generate_html_fallback(report)
        except Exception as e:
            self._logger.error(
                f"PDF generation failed: {type(e).__name__}: {e}",
                exc_info=True,
            )
            raise RuntimeError(f"PDF生成に失敗しました: {e}") from e

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
        doc = SimpleDocTemplate(buffer, pagesize=A4, topMargin=2*cm, bottomMargin=2*cm)
        styles = getSampleStyleSheet()
        elements: list[Any] = []

        # dictに変換
        dao = self._to_dict(report.dao)
        fa = self._to_dict(report.fa)
        shu = self._to_dict(report.shu)
        qi = self._to_dict(report.qi)
        review = self._to_dict(report.review)

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
        elements.append(Paragraph("決策レポート v3.0", title_style))
        elements.append(Paragraph(f"Report ID: {report.report_id}", normal_style))
        elements.append(Paragraph(
            f"生成日時: {report.created_at.strftime('%Y-%m-%d %H:%M')} | Version: {report.version}", normal_style
        ))
        elements.append(Spacer(1, 0.5*cm))

        # ========== エグゼクティブサマリー ==========
        elements.append(Paragraph("📊 エグゼクティブサマリー", heading_style))
        summary = report.executive_summary
        elements.append(Paragraph(f"<b>結論:</b> {summary.one_line_decision}", highlight_style))

        if hasattr(summary, "essence_statement") and summary.essence_statement:
            elements.append(Paragraph(f"<b>本質:</b> {summary.essence_statement}", normal_style))
        elements.append(Paragraph(f"<b>推奨アクション:</b> {summary.recommended_action}", normal_style))
        elements.append(Paragraph(f"<b>最初の一歩:</b> 🎯 {summary.first_step}", normal_style))

        if hasattr(summary, "strategic_prohibition_summary") and summary.strategic_prohibition_summary:
            elements.append(Paragraph(f"<b>⛔ 戦略的禁止:</b> {summary.strategic_prohibition_summary}", warning_style))
        if hasattr(summary, "exit_criteria_summary") and summary.exit_criteria_summary:
            elements.append(Paragraph(f"<b>🚪 撤退基準:</b> {summary.exit_criteria_summary}", warning_style))

        if summary.key_risks:
            elements.append(Paragraph("<b>主要リスク:</b>", normal_style))
            for risk in summary.key_risks:
                elements.append(Paragraph(f"  ⚠️ {risk}", normal_style))
        elements.append(Spacer(1, 0.5*cm))

        # ========== 道セクション ==========
        elements.append(Paragraph("🎯 道 - 本質分析", heading_style))
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
            elements.append(Paragraph("🔍 本質導出プロセス", subheading_style))
            elements.append(Paragraph(f"表面的問題: {ed.get('surface_problem', '')}", normal_style))
            elements.append(Paragraph(f"一段深い理由: {ed.get('underlying_why', '')}", normal_style))
            elements.append(Paragraph(f"根本制約: {ed.get('root_constraint', '')}", normal_style))
            elements.append(Paragraph(f"<b>本質の一文:</b> {ed.get('essence_statement', '')}", highlight_style))

        # 既存代替手段
        alternatives = dao.get("existing_alternatives", [])
        if alternatives:
            elements.append(Paragraph("🔄 既存代替手段", subheading_style))
            for alt in alternatives:
                elements.append(Paragraph(
                    f"• <b>{alt.get('name', '')}</b>: {alt.get('why_not_viable', '')} (制約: {alt.get('specific_constraint', '')})",
                    normal_style
                ))

        # 死穴
        traps = dao.get("death_traps", [])
        if traps:
            elements.append(Paragraph("💀 死穴（禁忌）", subheading_style))
            for trap in traps:
                elements.append(Paragraph(
                    f"⚠️ <b>{trap.get('action', '')}</b> ({trap.get('severity', '')}): {trap.get('reason', '')}",
                    warning_style
                ))
        elements.append(Spacer(1, 0.3*cm))

        # ========== 法セクション ==========
        elements.append(PageBreak())
        elements.append(Paragraph("⚖️ 法 - 戦略選定", heading_style))

        # 戦略的禁止事項
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            elements.append(Paragraph("🚫 戦略的禁止事項", subheading_style))
            for p in prohibitions:
                elements.append(Paragraph(
                    f"⛔ <b>{p.get('prohibition', '')}</b>: {p.get('rationale', '')} → {p.get('violation_consequence', '')}",
                    warning_style
                ))

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
            elements.append(Paragraph(f"<b>既存解が使えない理由:</b> {why_existing}", warning_style))

        # 推奨パス
        for path in fa.get("recommended_paths", []):
            strategy_type = path.get("strategy_type", "")
            if hasattr(strategy_type, "value"):
                strategy_type = strategy_type.value
            elements.append(Paragraph(f"📌 {path.get('name', '')} ({strategy_type})", subheading_style))
            elements.append(Paragraph(path.get("description", ""), normal_style))
            elements.append(Paragraph(
                f"成功確率: {path.get('success_probability', 0)*100:.0f}% | "
                f"価値実現: {path.get('time_to_value', '')} | "
                f"可逆性: {path.get('reversibility', '')}",
                normal_style
            ))
        elements.append(Spacer(1, 0.3*cm))

        # ========== 術セクション ==========
        elements.append(Paragraph("📋 術 - 実行計画", heading_style))

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
            elements.append(Paragraph("🎯 文脈特化行動", subheading_style))
            for a in context_actions:
                elements.append(Paragraph(
                    f"• <b>{a.get('action', '')}</b> → {a.get('expected_output', '')}",
                    normal_style
                ))

        # 単一検証ポイント
        validation = shu.get("single_validation_point", {})
        if validation:
            elements.append(Paragraph("🔬 単一検証ポイント", subheading_style))
            elements.append(Paragraph(
                f"検証: {validation.get('validation_target', '')} | "
                f"基準: {validation.get('success_criteria', '')} | "
                f"失敗時: {validation.get('failure_action', '')}",
                warning_style
            ))

        # 撤退基準
        exit_criteria = shu.get("exit_criteria", {})
        if exit_criteria:
            elements.append(Paragraph("🚪 撤退基準", subheading_style))
            elements.append(Paragraph(
                f"チェック: {exit_criteria.get('checkpoint', '')} | "
                f"トリガー: {exit_criteria.get('exit_trigger', '')} | "
                f"行動: {exit_criteria.get('exit_action', '')}",
                warning_style
            ))

        # フェーズ
        elements.append(Paragraph("📅 フェーズ", subheading_style))
        for phase in shu.get("phases", []):
            elements.append(Paragraph(
                f"Phase {phase.get('phase_number', '?')}: {phase.get('name', '')} ({phase.get('duration', '')})",
                normal_style
            ))
            actions = phase.get("actions", [])
            if actions:
                for action in actions[:3]:
                    elements.append(Paragraph(f"    • {action}", normal_style))
        elements.append(Spacer(1, 0.3*cm))

        # ========== 器セクション ==========
        elements.append(PageBreak())
        elements.append(Paragraph("🔧 器 - 技術実装", heading_style))

        # ドメイン固有技術
        domain_techs = qi.get("domain_technologies", [])
        if domain_techs:
            elements.append(Paragraph("🛠️ ドメイン固有技術", subheading_style))
            for t in domain_techs:
                elements.append(Paragraph(
                    f"• <b>{t.get('technology_name', '')}</b> ({t.get('category', '')}): {t.get('why_required', '')}",
                    highlight_style
                ))

        # 規制対応
        regulations = qi.get("regulatory_considerations", [])
        if regulations:
            elements.append(Paragraph("📜 規制対応事項", subheading_style))
            reg_data = [["地域", "規制", "要件", "影響"]]
            for r in regulations:
                reg_data.append([
                    r.get("region", ""),
                    r.get("regulation", ""),
                    r.get("requirement", ""),
                    r.get("implementation_impact", ""),
                ])
            reg_table = Table(reg_data, colWidths=[2*cm, 3*cm, 5*cm, 5*cm])
            reg_table.setStyle(TableStyle([
                ("GRID", (0, 0), (-1, -1), 0.5, colors.black),
                ("FONTNAME", (0, 0), (-1, -1), cjk_font),
                ("FONTSIZE", (0, 0), (-1, -1), 8),
                ("BACKGROUND", (0, 0), (-1, 0), colors.Color(0.9, 0.9, 0.9)),
            ]))
            elements.append(reg_table)

        # 地理的考慮
        geographics = qi.get("geographic_considerations", [])
        if geographics:
            elements.append(Paragraph("🌍 地理的考慮事項", subheading_style))
            for g in geographics:
                elements.append(Paragraph(
                    f"• {g.get('region', '')}: {g.get('latency_requirement', '')} | {g.get('infrastructure_need', '')}",
                    normal_style
                ))

        # 実装要素
        for impl in qi.get("implementations", []):
            elements.append(Paragraph(
                f"🔧 {impl.get('component', '')}: {impl.get('technology', '')} ({impl.get('estimated_effort', '')})",
                normal_style
            ))
        elements.append(Spacer(1, 0.3*cm))

        # ========== 検証セクション ==========
        elements.append(Paragraph("✅ 検証 - 最終判定", heading_style))
        verdict = review.get("overall_verdict", "N/A")
        if hasattr(verdict, "value"):
            verdict = verdict.value
        confidence = review.get("confidence_score", 0)
        verdict_style = highlight_style if verdict == "PASS" else warning_style
        elements.append(Paragraph(f"<b>判定: {verdict}</b> | 信頼度: {confidence*100:.0f}%", verdict_style))

        findings = review.get("findings", [])
        if findings:
            for f in findings:
                elements.append(Paragraph(
                    f"• {f.get('severity', '')}: {f.get('description', '')}",
                    normal_style
                ))
        elements.append(Spacer(1, 0.5*cm))

        # ========== 署名欄 ==========
        elements.append(Paragraph("✍️ 署名欄", heading_style))
        sig_data = [["承認者", "", "日付", ""], ["署名", "", "", ""]]
        sig_table = Table(sig_data, colWidths=[3*cm, 6*cm, 2*cm, 4*cm])
        sig_table.setStyle(TableStyle([
            ("GRID", (0, 0), (-1, -1), 0.5, colors.black),
            ("FONTNAME", (0, 0), (-1, -1), cjk_font),
        ]))
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
body{{font-family:'Yu Gothic','Hiragino Sans','Meiryo',sans-serif;max-width:900px;margin:0 auto;padding:30px;color:#333;line-height:1.8}}
.cover{{text-align:center;padding:60px 0;border:3px double #2c3e50;margin-bottom:40px;background:linear-gradient(135deg,#fafbfc,#f0f2f5)}}
.cover-title{{font-size:32px;font-weight:bold;color:#2c3e50;margin:20px 0 10px;letter-spacing:2px}}
.cover-title-en{{font-size:14px;color:#7f8c8d;font-family:'Helvetica Neue',Arial,sans-serif;letter-spacing:1px}}
.cover-subtitle{{font-size:16px;color:#555;margin:20px 0}}
.cover-case-id{{font-size:12px;color:#999;font-family:monospace}}
.cover-date{{font-size:14px;color:#555;margin-top:40px}}
.cover-author{{font-size:14px;color:#555;margin-top:10px}}
h1{{border-bottom:3px solid #2c3e50;padding-bottom:10px;color:#2c3e50;font-size:24px}}
h2{{color:#34495e;margin-top:30px;border-left:4px solid #3498db;padding-left:12px;font-size:18px}}
h3{{color:#7f8c8d;margin-top:20px;font-size:14px}}
.meta{{color:#7f8c8d;font-size:0.9em;margin-bottom:20px}}
.toc{{background:#f8f9fa;padding:20px;border-radius:8px;margin:20px 0}}
.toc h2{{border:none;margin-top:0}}
.toc ol{{margin:0;padding-left:25px}}
.toc li{{margin:8px 0}}
.summary{{background:linear-gradient(135deg,#f5f7fa,#e4e9f2);padding:25px;border-radius:12px;margin:20px 0;border:1px solid #ddd}}
.card{{background:#fff;border:1px solid #e0e0e0;border-radius:8px;padding:15px;margin:15px 0;box-shadow:0 2px 4px rgba(0,0,0,0.05)}}
.prohibition{{background:#fff5f5;border-left:4px solid #e74c3c;padding:10px 15px;margin:10px 0}}
.highlight{{background:#e8f6ff;border-left:4px solid #3498db;padding:10px 15px;margin:10px 0}}
.success{{background:#f0fff4;border-left:4px solid #27ae60;padding:10px 15px;margin:10px 0}}
.warning{{background:#fffbeb;border-left:4px solid #f39c12;padding:10px 15px;margin:10px 0}}
ul{{padding-left:20px}}
li{{margin:5px 0}}
table{{border-collapse:collapse;width:100%}}
td,th{{border:1px solid #ddd;padding:12px;text-align:left}}
th{{background:#f5f5f5;font-weight:bold}}
.signature-section{{margin-top:60px;page-break-inside:avoid}}
.signature-table{{border:2px solid #333}}
.signature-table th{{background:#f0f0f0;width:100px}}
.signature-table td{{height:60px;vertical-align:top}}
.label{{font-weight:bold;color:#555}}
.essence{{font-size:1.1em;color:#2c3e50;font-weight:bold}}
.footer{{text-align:center;color:#999;font-size:11px;margin-top:40px;padding-top:20px;border-top:1px solid #ddd}}
.section-number{{color:#3498db;font-weight:bold;margin-right:8px}}
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
<p>案件ID: {case_id} | Version: {report.version} | 生成日時: {report.created_at.strftime('%Y-%m-%d %H:%M')}</p>
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
        if hasattr(summary, "strategic_prohibition_summary") and summary.strategic_prohibition_summary:
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
<p><span class="label">表面的問題:</span> {ed.get('surface_problem', '')}</p>
<p><span class="label">一段深い理由:</span> {ed.get('underlying_why', '')}</p>
<p><span class="label">根本制約:</span> {ed.get('root_constraint', '')}</p>
<p class="essence"><span class="label">本質の一文:</span> {ed.get('essence_statement', '')}</p>
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
            gears_html = f"<h3>⚙️ 因果齿轮</h3><ul>{items}</ul><p>🎯 ボトルネック: Gear {bottleneck}</p>"

        # 死穴
        death_traps_html = ""
        traps = dao.get("death_traps", [])
        if traps:
            items = "".join(
                f'<div class="prohibition"><strong>⚠️ {t.get("action", "")}</strong> ({t.get("severity", "")})<br/>理由: {t.get("reason", "")}</div>'
                for t in traps
            )
            death_traps_html = f"<h3>💀 死穴（禁忌）</h3>{items}"

        return f"""<h2>🎯 道 - 本質分析</h2>
<div class="card">
<p><span class="label">問題タイプ:</span> {problem_type}</p>
<p><span class="label">問題の本質的性質:</span> {problem_nature}</p>
<p class="essence"><span class="label">本質:</span> {dao.get('essence', 'N/A')}</p>
</div>
{essence_derivation_html}
{alternatives_html}
{constraints_html}
{assumptions_html}
{gears_html}
{death_traps_html}"""

    def _build_fa_html(self, fa: dict) -> str:
        """法セクションHTMLを構築 v3.0."""
        # 推奨パス
        paths_html = ""
        for path in fa.get("recommended_paths", []):
            strategy_type = path.get("strategy_type", "")
            if hasattr(strategy_type, "value"):
                strategy_type = strategy_type.value

            pros = "".join(f"<li>✅ {p}</li>" for p in path.get("pros", []))
            cons = "".join(f"<li>❌ {c}</li>" for c in path.get("cons", []))

            paths_html += f"""<div class="card">
<h3>📌 {path.get('name', '')} ({strategy_type})</h3>
<p>{path.get('description', '')}</p>
<p><span class="label">成功確率:</span> {path.get('success_probability', 0)*100:.0f}%</p>
<p><span class="label">価値実現時間:</span> {path.get('time_to_value', '')}</p>
<p><span class="label">可逆性:</span> {path.get('reversibility', '')}</p>
<h4>メリット</h4><ul>{pros}</ul>
<h4>デメリット</h4><ul>{cons}</ul>
</div>"""

        # v3.0: 戦略的禁止事項
        prohibitions_html = ""
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            items = "".join(
                f'<div class="prohibition"><strong>⛔ {p.get("prohibition", "")}</strong><br/>'
                f'理由: {p.get("rationale", "")}<br/>'
                f'違反結果: {p.get("violation_consequence", "")}</div>'
                for p in prohibitions
            )
            prohibitions_html = f"<h3>🚫 戦略的禁止事項（絶対にやってはいけない）</h3>{items}"

        # v3.0: 差別化軸
        diff_axis_html = ""
        diff_axis = fa.get("differentiation_axis", {})
        if diff_axis:
            diff_axis_html = f"""<div class="highlight">
<h3>🎯 差別化軸</h3>
<p><span class="label">勝負する軸:</span> <strong>{diff_axis.get('axis_name', '')}</strong></p>
<p><span class="label">理由:</span> {diff_axis.get('why_this_axis', '')}</p>
<p><span class="label">勝負しない軸:</span> {diff_axis.get('not_this_axis', '')}</p>
</div>"""

        # v3.0: 既存解が使えない理由
        why_existing_fails = fa.get("why_existing_fails", "")
        why_existing_html = ""
        if why_existing_fails:
            why_existing_html = f'<div class="warning"><span class="label">既存解が使えない理由:</span> {why_existing_fails}</div>'

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
<p>{comparison.get('recommendation_summary', '')}</p>"""

        return f"""<h2>⚖️ 法 - 戦略選定</h2>
{prohibitions_html}
{diff_axis_html}
{why_existing_html}
{paths_html}
{comparison_html}"""

    def _build_shu_html(self, shu: dict) -> str:
        """術セクションHTMLを構築 v3.0."""
        # フェーズ
        phases_html = ""
        for phase in shu.get("phases", []):
            actions = "".join(f"<li>{a}</li>" for a in phase.get("actions", []))
            deliverables = "".join(f"<li>{d}</li>" for d in phase.get("deliverables", []))
            criteria = "".join(f"<li>{c}</li>" for c in phase.get("success_criteria", []))
            phases_html += f"""<div class="card">
<h3>Phase {phase.get('phase_number', '?')}: {phase.get('name', '')} ({phase.get('duration', '')})</h3>
<h4>行動</h4><ul>{actions}</ul>
<h4>成果物</h4><ul>{deliverables}</ul>
<h4>完了条件</h4><ul>{criteria}</ul>
</div>"""

        # 最初の一歩
        first_action = shu.get("first_action", "")
        first_action_html = f'<div class="success"><strong>🎯 最初の一歩:</strong> {first_action}</div>' if first_action else ""

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
<p><span class="label">検証対象:</span> {validation.get('validation_target', '')}</p>
<p><span class="label">成功基準:</span> {validation.get('success_criteria', '')}</p>
<p><span class="label">失敗時行動:</span> {validation.get('failure_action', '')}</p>
</div>"""

        # v3.0: 撤退基準
        exit_html = ""
        exit_criteria = shu.get("exit_criteria", {})
        if exit_criteria:
            exit_html = f"""<div class="prohibition">
<h3>🚪 撤退基準（どこで止めるか）</h3>
<p><span class="label">チェックポイント:</span> {exit_criteria.get('checkpoint', '')}</p>
<p><span class="label">撤退トリガー:</span> {exit_criteria.get('exit_trigger', '')}</p>
<p><span class="label">撤退時行動:</span> {exit_criteria.get('exit_action', '')}</p>
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
<p><span class="label">聚焦:</span> <strong>{focus.get('name', '')}</strong></p>
<p>{focus.get('description', '')}</p>
<p><span class="label">成功指標:</span> {focus.get('success_metric', '')}</p>
<h4>この期間やらないこと</h4><ul>{avoid_items}</ul>
<p><span class="label">チェックポイント:</span> {rhythm.get('checkpoint_date', '')}</p>
<p><span class="label">次の判断:</span> {rhythm.get('next_decision_point', '')}</p>
</div>"""

        return f"""<h2>📋 術 - 実行計画</h2>
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
<h3>🔧 {impl.get('component', '')}</h3>
<p><span class="label">技術:</span> {impl.get('technology', '')}</p>
<p><span class="label">工数:</span> {impl.get('estimated_effort', '')}</p>
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

        return f"""<h2>🔧 器 - 技術実装</h2>
{domain_tech_html}
{regulatory_html}
{geographic_html}
{impl_html}
{tools_html}
{integration_html}
{warnings_html}"""

    def _build_review_html(self, review: dict) -> str:
        """検証セクションHTMLを構築."""
        verdict = review.get("overall_verdict", "N/A")
        if hasattr(verdict, "value"):
            verdict = verdict.value

        confidence = review.get("confidence_score", 0)

        # 所見
        findings_html = ""
        findings = review.get("findings", [])
        if findings:
            items = "".join(
                f"<li><strong>{f.get('severity', '')}</strong> ({f.get('category', '')}): {f.get('description', '')}"
                f"<br/>影響Agent: {f.get('affected_agent', '')} | 修正提案: {f.get('suggested_revision', '')}</li>"
                for f in findings
            )
            findings_html = f"<h3>📝 検証所見</h3><ul>{items}</ul>"

        # 最終警告
        warnings_html = ""
        warnings = review.get("final_warnings", [])
        if warnings:
            items = "".join(f"<li>⚠️ {w}</li>" for w in warnings)
            warnings_html = f"<h3>⚠️ 最終警告</h3><ul>{items}</ul>"

        verdict_class = "success" if verdict == "PASS" else "prohibition" if verdict == "REJECT" else "warning"

        return f"""<h2>✅ 検証 - 最終判定</h2>
<div class="{verdict_class}">
<p><span class="label">判定:</span> <strong>{verdict}</strong></p>
<p><span class="label">信頼度:</span> {confidence*100:.0f}%</p>
</div>
{findings_html}
{warnings_html}"""

