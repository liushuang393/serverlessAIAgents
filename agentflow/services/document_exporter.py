# -*- coding: utf-8 -*-
"""Document Exporter Service - 汎用ドキュメントエクスポート.

レポートやドキュメントを複数フォーマットでエクスポートする汎用サービス。

対応フォーマット:
- PDF (ReportLab)
- HTML
- Markdown
- JSON

使用例:
    >>> from agentflow.services import DocumentExporter
    >>>
    >>> exporter = DocumentExporter()
    >>> # HTMLエクスポート
    >>> html = await exporter.export_html(content, template="report")
    >>> # PDFエクスポート
    >>> pdf = await exporter.export_pdf(content, title="レポート")
"""

from __future__ import annotations

import io
import json
import logging
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from agentflow.services.base import (
    ServiceBase,
    ServiceEvent,
    ResultEvent,
)

_logger = logging.getLogger(__name__)


class ExportFormat(str, Enum):
    """エクスポートフォーマット."""
    PDF = "pdf"
    HTML = "html"
    MARKDOWN = "markdown"
    JSON = "json"


class TemplateType(str, Enum):
    """テンプレートタイプ."""
    REPORT = "report"
    PROPOSAL = "proposal"
    ANALYSIS = "analysis"
    SIMPLE = "simple"


@dataclass
class ExportConfig:
    """エクスポート設定."""
    format: ExportFormat = ExportFormat.HTML
    template: TemplateType = TemplateType.REPORT
    title: str = "Document"
    author: str = ""
    include_toc: bool = True
    include_timestamp: bool = True
    page_size: str = "A4"
    font_family: str = "sans-serif"


@dataclass
class Section:
    """ドキュメントセクション."""
    title: str
    content: str
    level: int = 1
    subsections: list["Section"] = field(default_factory=list)


class DocumentExporter(ServiceBase[bytes]):
    """汎用ドキュメントエクスポーター.

    Actions:
    - export: コンテンツをエクスポート
    - convert: フォーマット変換
    """

    def __init__(self, config: ExportConfig | None = None) -> None:
        """初期化."""
        super().__init__()
        self._config = config or ExportConfig()
        self._has_reportlab = self._check_reportlab()

    def _check_reportlab(self) -> bool:
        """ReportLabが利用可能か確認."""
        try:
            from reportlab.lib.pagesizes import A4  # noqa: F401
            return True
        except ImportError:
            _logger.info("ReportLabなし。PDF出力はHTML代替を使用。")
            return False

    async def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """内部実行ロジック."""
        action = kwargs.get("action", "export")
        content = kwargs.get("content", {})
        fmt = kwargs.get("format", self._config.format)

        if isinstance(fmt, str):
            fmt = ExportFormat(fmt)

        yield self._emit_progress(execution_id, 10, f"エクスポート開始: {fmt.value}")

        if action == "export":
            if fmt == ExportFormat.PDF:
                result = await self._export_pdf(content, kwargs)
            elif fmt == ExportFormat.HTML:
                result = await self._export_html(content, kwargs)
            elif fmt == ExportFormat.MARKDOWN:
                result = await self._export_markdown(content, kwargs)
            else:
                result = await self._export_json(content, kwargs)

            yield self._emit_progress(execution_id, 90, "エクスポート完了")
            yield self._emit_result(
                execution_id,
                {"data": result, "format": fmt.value, "size": len(result)},
            )
        else:
            yield self._emit_error(execution_id, "invalid_action", f"不明: {action}")

    async def _export_html(
        self,
        content: dict[str, Any],
        options: dict[str, Any],
    ) -> bytes:
        """HTMLエクスポート."""
        title = options.get("title", self._config.title)
        sections = content.get("sections", [])
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M")

        sections_html = self._build_sections_html(sections)

        html = f"""<!DOCTYPE html>
<html lang="ja">
<head>
<meta charset="UTF-8">
<title>{title}</title>
<style>
body{{font-family:{self._config.font_family};max-width:900px;margin:0 auto;padding:30px;line-height:1.8}}
h1{{border-bottom:2px solid #333;padding-bottom:10px}}
h2{{color:#2c3e50;border-left:4px solid #3498db;padding-left:10px}}
.meta{{color:#666;font-size:0.9em}}
.toc{{background:#f5f5f5;padding:20px;border-radius:8px;margin:20px 0}}
</style>
</head>
<body>
<h1>{title}</h1>
<p class="meta">生成日時: {timestamp}</p>
{sections_html}
</body>
</html>"""
        return html.encode("utf-8")

    def _build_sections_html(self, sections: list[dict[str, Any]]) -> str:
        """セクションHTMLを構築."""
        html_parts = []
        for sec in sections:
            level = sec.get("level", 2)
            title = sec.get("title", "")
            content = sec.get("content", "")
            html_parts.append(f"<h{level}>{title}</h{level}>")
            html_parts.append(f"<p>{content}</p>")
        return "\n".join(html_parts)

    async def _export_markdown(
        self,
        content: dict[str, Any],
        options: dict[str, Any],
    ) -> bytes:
        """Markdownエクスポート."""
        title = options.get("title", self._config.title)
        sections = content.get("sections", [])
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M")

        md_parts = [f"# {title}", f"*生成日時: {timestamp}*", ""]

        for sec in sections:
            level = sec.get("level", 2)
            sec_title = sec.get("title", "")
            sec_content = sec.get("content", "")
            md_parts.append(f"{'#' * level} {sec_title}")
            md_parts.append(sec_content)
            md_parts.append("")

        return "\n".join(md_parts).encode("utf-8")

    async def _export_json(
        self,
        content: dict[str, Any],
        options: dict[str, Any],
    ) -> bytes:
        """JSONエクスポート."""
        title = options.get("title", self._config.title)
        timestamp = datetime.now().isoformat()

        export_data = {
            "title": title,
            "generated_at": timestamp,
            "content": content,
        }
        return json.dumps(export_data, ensure_ascii=False, indent=2).encode("utf-8")

    async def _export_pdf(
        self,
        content: dict[str, Any],
        options: dict[str, Any],
    ) -> bytes:
        """PDFエクスポート."""
        if not self._has_reportlab:
            # HTML代替
            html_bytes = await self._export_html(content, options)
            return html_bytes

        from reportlab.lib.pagesizes import A4
        from reportlab.lib.styles import getSampleStyleSheet
        from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer

        title = options.get("title", self._config.title)
        sections = content.get("sections", [])

        buffer = io.BytesIO()
        doc = SimpleDocTemplate(buffer, pagesize=A4)
        styles = getSampleStyleSheet()
        elements = []

        elements.append(Paragraph(title, styles["Title"]))
        elements.append(Spacer(1, 20))

        for sec in sections:
            sec_title = sec.get("title", "")
            sec_content = sec.get("content", "")
            elements.append(Paragraph(sec_title, styles["Heading2"]))
            elements.append(Paragraph(sec_content, styles["Normal"]))
            elements.append(Spacer(1, 10))

        doc.build(elements)
        return buffer.getvalue()


__all__ = [
    "DocumentExporter",
    "ExportConfig",
    "ExportFormat",
    "TemplateType",
    "Section",
]

