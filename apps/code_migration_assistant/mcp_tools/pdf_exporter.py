"""PDF Exporter Tool using ReportLab."""

from agentflow.providers.tool_provider import OperationType, RegisteredTool, RiskLevel


def export_to_pdf(markdown_content: str, output_path: str) -> str:
    """Markdown形式のテキストをPDFとして出力する.

    Args:
        markdown_content: Markdownテキスト
        output_path: 保存先パス

    Returns:
        保存されたパス
    """
    try:
        from reportlab.lib.pagesizes import A4
        from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
        from reportlab.pdfbase import pdfmetrics
        from reportlab.pdfbase.ttfonts import TTFont
        from reportlab.platypus import Paragraph, SimpleDocTemplate, Spacer

        doc = SimpleDocTemplate(output_path, pagesize=A4)
        styles = getSampleStyleSheet()

        # 日本語フォントの設定（フォントファイルがある場合）
        # 実行環境にIPAフォント等があることを期待するか、標準フォントで代用
        # ここでは標準フォントを使うが、日本語は通常別途設定が必要

        elements = []

        # 簡易的なMarkdownパース（行単位）
        lines = markdown_content.split("\n")
        for line in lines:
            if line.startswith("# "):
                elements.append(Paragraph(line[2:], styles["Title"]))
            elif line.startswith("## "):
                elements.append(Paragraph(line[3:], styles["Heading2"]))
            elif line.startswith("### "):
                elements.append(Paragraph(line[4:], styles["Heading3"]))
            elif line.strip():
                elements.append(Paragraph(line, styles["Normal"]))
            else:
                elements.append(Spacer(1, 12))

        doc.build(elements)
        return output_path

    except ImportError:
        # reportlabがない場合は例外
        msg = "reportlab library is not installed. Please install it to use PDF export."
        raise ImportError(msg)
    except Exception as e:
        msg = f"Failed to generate PDF: {e}"
        raise Exception(msg)


# Tool Registration
PDF_EXPORT_TOOL = RegisteredTool(
    name="migration.export_pdf",
    description="報告書をPDF形式で出力",
    func=export_to_pdf,
    operation_type=OperationType.WRITE,
    risk_level=RiskLevel.LOW,
    audit_required=True,
)
