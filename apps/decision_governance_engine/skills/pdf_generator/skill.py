"""PDF Generation Skill - 決策レポートのPDF/HTML出力能力.

このモジュールは、DecisionReport オブジェクトを A2UI デザインに準拠した
PDF および HTML フォーマットで出力する Skill を提供します。
"""

import io
import logging
from typing import Any

from apps.decision_governance_engine.schemas.output_schemas import DecisionReport, SignatureBlock


# Optional: ReportLab dependency
try:
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

    HAS_REPORTLAB = True
except ImportError:
    HAS_REPORTLAB = False


class PDFGenerationSkill:
    """PDF/HTML 生成スキル.

    v3.2 改善:
    - 空白ページの防止 (PageBreak の条件付け)
    - 署名欄の正確なレンダリングと視覚的印影
    - スタイルの一貫性向上
    """

    def __init__(self) -> None:
        self._logger = logging.getLogger("pdf_generation_skill")
        self._has_content_on_page = False

        if HAS_REPORTLAB:
            try:
                # 日本語フォント登録
                pdfmetrics.registerFont(UnicodeCIDFont("HeiseiKakuGo-W5"))
            except Exception as e:
                self._logger.warning(f"Font registration failed: {e}")

    async def export_report(
        self,
        report: DecisionReport,
        format: str = "pdf",
        signed_data: dict | None = None,
    ) -> bytes:
        """レポートをエクスポートする.

        Args:
            report: 決策レポート対象
            format: "pdf" または "html"
            signed_data: 署名データ（あれば優先）

        Returns:
            生成されたバイナリデータ
        """
        if format.lower() == "html" or not HAS_REPORTLAB:
            return self._generate_html(report, signed_data)

        return self._generate_pdf(report, signed_data)

    def _generate_pdf(self, report: DecisionReport, signed_data: dict | None = None) -> bytes:
        """ReportLabを使用してPDFを生成."""
        buffer = io.BytesIO()
        doc = SimpleDocTemplate(
            buffer,
            pagesize=A4,
            rightMargin=2 * cm,
            leftMargin=2 * cm,
            topMargin=2 * cm,
            bottomMargin=2 * cm,
        )

        getSampleStyleSheet()
        # 日本語スタイル定義
        title_style = ParagraphStyle(
            name="JPTitle",
            fontName="HeiseiKakuGo-W5",
            fontSize=22,
            leading=28,
            alignment=1,  # Center
            spaceAfter=20,
        )
        heading_style = ParagraphStyle(
            name="JPHeading",
            fontName="HeiseiKakuGo-W5",
            fontSize=16,
            leading=20,
            spaceBefore=15,
            spaceAfter=10,
            borderPadding=5,
            leftIndent=0,
        )
        normal_style = ParagraphStyle(
            name="JPNormal",
            fontName="HeiseiKakuGo-W5",
            fontSize=10,
            leading=14,
        )

        elements: list[Any] = []
        self._has_content_on_page = False

        def add_element(elem: Any) -> None:
            elements.append(elem)
            self._has_content_on_page = True

        def conditional_page_break() -> None:
            if self._has_content_on_page:
                elements.append(PageBreak())
                self._has_content_on_page = False

        # --- 表紙 ---
        add_element(Spacer(1, 5 * cm))
        add_element(Paragraph(report.proposal_title.title_ja, title_style))
        add_element(
            Paragraph(
                report.proposal_title.title_en, ParagraphStyle("EN", fontSize=12, alignment=1, textColor=colors.grey)
            )
        )
        add_element(Spacer(1, 1 * cm))
        if report.proposal_title.subtitle:
            add_element(Paragraph(report.proposal_title.subtitle, ParagraphStyle("Sub", fontSize=14, alignment=1)))

        add_element(Spacer(1, 10 * cm))

        # 作成者情報 (表紙下部)
        sig = report.signature_block
        author_info = f"{sig.author_department}<br/>{sig.author_position} {sig.author_name}"
        add_element(Paragraph(author_info, ParagraphStyle("Author", fontSize=12, alignment=2)))
        add_element(Paragraph(f"作成日: {sig.created_date}", ParagraphStyle("Date", fontSize=10, alignment=2)))

        conditional_page_break()

        # --- 各セクション ---
        # 1. 概要
        add_element(Paragraph("1. エグゼクティブサマリー", heading_style))
        add_element(Paragraph(f"<b>結論:</b> {report.executive_summary.one_line_decision}", normal_style))
        if hasattr(report.executive_summary, "essence_statement") and report.executive_summary.essence_statement:
            add_element(Paragraph(f"<b>本質:</b> {report.executive_summary.essence_statement}", normal_style))
        add_element(Spacer(1, 0.5 * cm))

        # 2. 道
        if report.dao:
            add_element(Paragraph("2. 現状の課題・問題点（道）", heading_style))
            add_element(Paragraph(str(report.dao.get("essence", "")), normal_style))
            add_element(Spacer(1, 0.5 * cm))

        # 3. 法
        if report.fa:
            add_element(Paragraph("3. 提案内容・解決策（法）", heading_style))
            # ... 詳細は簡略化 ...
            add_element(Paragraph("戦略の詳細はHTML版またはシステム画面を参照してください。", normal_style))
            add_element(Spacer(1, 0.5 * cm))

        # 4. 術
        if report.shu:
            add_element(Paragraph("4. 実行計画・スケジュール（術）", heading_style))
            add_element(Paragraph(f"<b>最初の一歩:</b> {report.shu.get('first_action', '')}", normal_style))
            add_element(Spacer(1, 0.5 * cm))

        # --- 署名欄 ---
        conditional_page_break()
        add_element(Paragraph("署名 / 承認状況", heading_style))

        # 署名データの決定 (引数優先)
        sig_block = report.signature_block
        if signed_data:
            sig_block = SignatureBlock(**signed_data)

        # テーブル形式の署名欄
        table_data = [
            ["作成", "部署", sig_block.author_department, "役職", sig_block.author_position],
            ["", "氏名", sig_block.author_name, "日付", sig_block.created_date],
            ["承認", "部署", sig_block.approver_department or "", "役職", sig_block.approver_position or ""],
            ["", "氏名", sig_block.approver_name or "", "日付", sig_block.approved_date or ""],
        ]

        sig_table = Table(table_data, colWidths=[1.5 * cm, 2 * cm, 5 * cm, 2 * cm, 5 * cm])
        sig_table.setStyle(
            TableStyle(
                [
                    ("FONT", (0, 0), (-1, -1), "HeiseiKakuGo-W5"),
                    ("GRID", (0, 0), (-1, -1), 0.5, colors.grey),
                    ("VALIGN", (0, 0), (-1, -1), "MIDDLE"),
                    ("BACKGROUND", (0, 0), (1, 0), colors.whitesmoke),
                    ("BACKGROUND", (0, 2), (1, 2), colors.whitesmoke),
                    ("SPAN", (0, 0), (0, 1)),
                    ("SPAN", (0, 2), (0, 3)),
                ]
            )
        )
        add_element(sig_table)

        # 印影シミュレーション (もし承認済みなら赤丸を表示)
        if sig_block.is_signed:
            add_element(Spacer(1, 0.5 * cm))
            # 単純なテキストと罫線での印影表現
            seal_data = [["印"]]
            seal_table = Table(seal_data, colWidths=[2 * cm], rowHeights=[2 * cm])
            seal_table.setStyle(
                TableStyle(
                    [
                        ("FONT", (0, 0), (-1, -1), "HeiseiKakuGo-W5", 20),
                        ("GRID", (0, 0), (-1, -1), 2, colors.red),
                        ("TEXTCOLOR", (0, 0), (-1, -1), colors.red),
                        ("ALIGN", (0, 0), (-1, -1), "CENTER"),
                        ("VALIGN", (0, 0), (-1, -1), "MIDDLE"),
                        ("ROUNDRECT", (0, 0), (-1, -1), colors.red, 1 * cm, 1, None),
                    ]
                )
            )
            add_element(Paragraph("<font color='red'><b>[ 電子承認済み ]</b></font>", normal_style))
            add_element(seal_table)

        doc.build(elements)
        return buffer.getvalue()

    def _generate_html(self, report: DecisionReport, signed_data: dict | None = None) -> bytes:
        """A2UIデザインに準拠したプレミアムHTMLを生成."""
        # 既存の _generate_html_fallback をベースに改善
        sig = report.signature_block
        if signed_data:
            sig = SignatureBlock(**signed_data)

        # 印影バッジ
        signed_badge = ""
        if sig.is_signed:
            signed_badge = """
            <div style="position:absolute; bottom:50px; right:50px; width:100px; height:100px;
                        border:3px solid #ef4444; border-radius:50%; color:#ef4444;
                        display:flex; align-items:center; justify-content:center;
                        font-weight:bold; transform:rotate(-15deg); font-size:20px;
                        background:rgba(239, 68, 68, 0.1);">
                承認済
            </div>
            """

        html = f"""<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>{report.proposal_title.title_ja}</title>
    <style>
        :root {{
            --primary: #6366f1;
            --bg: #0a0a0f;
            --card-bg: #12121a;
            --text: #e2e8f0;
            --border: #334155;
        }}
        body {{
            font-family: 'Inter', 'Yu Gothic', sans-serif;
            background: var(--bg);
            color: var(--text);
            line-height: 1.6;
            max-width: 900px;
            margin: 0 auto;
            padding: 40px;
        }}
        .cover {{
            text-align: center;
            padding: 100px 0;
            border: 1px solid var(--border);
            border-radius: 20px;
            background: linear-gradient(135deg, #12121a, #1e1e2d);
            margin-bottom: 60px;
        }}
        h1 {{ font-size: 2.5em; margin-bottom: 10px; }}
        .subtitle {{ color: #94a3b8; font-size: 1.2em; }}
        .section {{ margin-top: 40px; padding: 20px; background: var(--card-bg); border-radius: 12px; border: 1px solid var(--border); }}
        .signature-table {{ width: 100%; border-collapse: collapse; margin-top: 20px; }}
        .signature-table th, .signature-table td {{ border: 1px solid var(--border); padding: 15px; text-align: left; }}
        .signature-table th {{ background: #1f2937; color: #94a3b8; width: 15%; }}
        .badge-signed {{ color: #ef4444; font-weight: bold; border: 1px solid #ef4444; padding: 2px 8px; border-radius: 4px; }}
    </style>
</head>
<body>
    <div class="cover" style="position:relative;">
        <h1>{report.proposal_title.title_ja}</h1>
        <div class="subtitle">{report.proposal_title.title_en}</div>
        <p>{report.proposal_title.subtitle}</p>
        <div style="margin-top: 50px; text-align: right; padding-right: 50px;">
            <p>{sig.author_department}</p>
            <p>{sig.author_position} {sig.author_name}</p>
            <p>作成日: {sig.created_date}</p>
        </div>
        {signed_badge}
    </div>

    <div class="section">
        <h2>📊 エグゼクティブサマリー</h2>
        <p><strong>結論:</strong> {report.executive_summary.one_line_decision}</p>
        <p><strong>推奨アクション:</strong> {report.executive_summary.recommended_action}</p>
    </div>

    <div class="section" id="signature">
        <h2>✍️ 署名欄</h2>
        <table class="signature-table">
            <tr>
                <th rowspan="2">作成者</th>
                <td>{sig.author_department}</td>
                <td>{sig.author_position}</td>
                <td>{sig.author_name}</td>
                <td>{sig.created_date}</td>
            </tr>
            <tr>
                <td colspan="4" style="height:40px;">（電子署名：{sig.signature_timestamp or "未"}）</td>
            </tr>
            <tr>
                <th rowspan="2">承認者</th>
                <td>{sig.approver_department or "-"}</td>
                <td>{sig.approver_position or "-"}</td>
                <td>{sig.approver_name or "-"}</td>
                <td>{sig.approved_date or "-"}</td>
            </tr>
            <tr>
                <td colspan="4" style="height:40px;">
                    {'<span class="badge-signed">APPROVED</span>' if sig.is_signed else "（未承認）"}
                </td>
            </tr>
        </table>
    </div>
</body>
</html>
"""
        return html.encode("utf-8")
