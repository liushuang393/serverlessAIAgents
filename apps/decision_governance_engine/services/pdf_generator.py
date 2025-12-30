# -*- coding: utf-8 -*-
"""PDF出力サービス.

DecisionReportをPDF形式でエクスポートする。
署名欄付きの企業向けレポート出力。
"""

import io
import logging
from datetime import datetime
from typing import Any

from apps.decision_governance_engine.schemas.output_schemas import DecisionReport


class PDFGeneratorService:
    """Decision レポート PDF 生成サービス.

    DecisionReportを署名可能なPDF形式に変換。
    ReportLabまたはWeasyPrintを使用（オプション）。
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
        """
        if self._has_reportlab:
            return self._generate_with_reportlab(report)
        return self._generate_html_fallback(report)

    def _generate_with_reportlab(self, report: DecisionReport) -> bytes:
        """ReportLabでPDF生成."""
        from reportlab.lib import colors
        from reportlab.lib.pagesizes import A4
        from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
        from reportlab.lib.units import cm
        from reportlab.platypus import (
            Paragraph,
            SimpleDocTemplate,
            Spacer,
            Table,
            TableStyle,
        )

        buffer = io.BytesIO()
        doc = SimpleDocTemplate(buffer, pagesize=A4, topMargin=2*cm, bottomMargin=2*cm)
        styles = getSampleStyleSheet()
        elements = []

        # タイトル
        title_style = ParagraphStyle("Title", parent=styles["Title"], fontSize=18)
        elements.append(Paragraph("決策レポート", title_style))
        elements.append(Paragraph(f"Report ID: {report.report_id}", styles["Normal"]))
        elements.append(Paragraph(f"生成日時: {report.created_at.strftime('%Y-%m-%d %H:%M')}", styles["Normal"]))
        elements.append(Spacer(1, 0.5*cm))

        # エグゼクティブサマリー
        elements.append(Paragraph("エグゼクティブサマリー", styles["Heading2"]))
        summary = report.executive_summary
        elements.append(Paragraph(f"<b>結論:</b> {summary.one_line_decision}", styles["Normal"]))
        elements.append(Paragraph(f"<b>推奨アクション:</b> {summary.recommended_action}", styles["Normal"]))
        elements.append(Paragraph(f"<b>最初の一歩:</b> {summary.first_step}", styles["Normal"]))
        elements.append(Spacer(1, 0.3*cm))

        # 道セクション
        elements.append(Paragraph("道 - 本質分析", styles["Heading2"]))
        elements.append(Paragraph(f"問題タイプ: {report.dao.get('problem_type', 'N/A')}", styles["Normal"]))
        elements.append(Paragraph(f"本質: {report.dao.get('essence', 'N/A')}", styles["Normal"]))
        elements.append(Spacer(1, 0.3*cm))

        # 法セクション
        elements.append(Paragraph("法 - 戦略選定", styles["Heading2"]))
        for path in report.fa.get("recommended_paths", []):
            elements.append(Paragraph(f"• {path.get('name', '')}: {path.get('description', '')}", styles["Normal"]))
        elements.append(Spacer(1, 0.3*cm))

        # 術セクション
        elements.append(Paragraph("術 - 実行計画", styles["Heading2"]))
        for phase in report.shu.get("phases", []):
            elements.append(Paragraph(
                f"Phase {phase.get('phase_number', '?')}: {phase.get('name', '')} ({phase.get('duration', '')})",
                styles["Normal"]
            ))
        elements.append(Spacer(1, 0.3*cm))

        # 署名欄
        elements.append(Spacer(1, 1*cm))
        elements.append(Paragraph("署名欄", styles["Heading2"]))
        sig_data = [["承認者", "", "日付", ""], ["署名", "", "", ""]]
        sig_table = Table(sig_data, colWidths=[3*cm, 6*cm, 2*cm, 4*cm])
        sig_table.setStyle(TableStyle([
            ("GRID", (0, 0), (-1, -1), 0.5, colors.black),
            ("FONTNAME", (0, 0), (-1, -1), "Helvetica"),
        ]))
        elements.append(sig_table)

        doc.build(elements)
        return buffer.getvalue()

    def _generate_html_fallback(self, report: DecisionReport) -> bytes:
        """HTML形式でのフォールバック出力."""
        # Pydanticオブジェクトをdictに変換
        dao = report.dao.model_dump() if hasattr(report.dao, "model_dump") else report.dao
        fa = report.fa.model_dump() if hasattr(report.fa, "model_dump") else report.fa
        shu = report.shu.model_dump() if hasattr(report.shu, "model_dump") else report.shu

        problem_type = dao.get("problem_type", "N/A")
        if hasattr(problem_type, "value"):
            problem_type = problem_type.value

        paths_html = "".join(
            f"<p>• {p.get('name','')}: {p.get('description','')}</p>"
            for p in fa.get("recommended_paths", [])
        )
        phases_html = "".join(
            f"<p>Phase {p.get('phase_number','?')}: {p.get('name','')} ({p.get('duration','')})</p>"
            for p in shu.get("phases", [])
        )

        html = f"""<!DOCTYPE html>
<html lang="ja">
<head><meta charset="UTF-8"><title>決策レポート {report.report_id}</title>
<style>body{{font-family:sans-serif;max-width:800px;margin:0 auto;padding:20px}}
h1{{border-bottom:2px solid #333}}h2{{color:#444;margin-top:20px}}
.summary{{background:#f5f5f5;padding:15px;border-radius:8px}}
.signature{{margin-top:40px;border:1px solid #999;padding:20px}}</style></head>
<body>
<h1>決策レポート</h1>
<p>Report ID: {report.report_id} | 生成日時: {report.created_at.strftime('%Y-%m-%d %H:%M')}</p>
<div class="summary">
<h2>エグゼクティブサマリー</h2>
<p><strong>結論:</strong> {report.executive_summary.one_line_decision}</p>
<p><strong>推奨:</strong> {report.executive_summary.recommended_action}</p>
<p><strong>最初の一歩:</strong> {report.executive_summary.first_step}</p>
</div>
<h2>道 - 本質分析</h2>
<p>問題タイプ: {problem_type}</p>
<p>本質: {dao.get('essence', 'N/A')}</p>
<h2>法 - 戦略選定</h2>
{paths_html}
<h2>術 - 実行計画</h2>
{phases_html}
<div class="signature"><h2>署名欄</h2>
<table border="1" cellpadding="10"><tr><td>承認者</td><td width="200"></td><td>日付</td><td width="100"></td></tr>
<tr><td>署名</td><td colspan="3" height="50"></td></tr></table></div>
</body></html>"""
        return html.encode("utf-8")

