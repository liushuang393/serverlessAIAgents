"""FAQ システムファイルパーサー.

PDF, Word, Excel, CSV などのファイルをテキスト + メタデータに変換する。
統合インターフェース ``ParseResult`` で全形式の結果を返す。
"""

from __future__ import annotations

import csv
import io
import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import IO, Any


logger = logging.getLogger(__name__)

# --- オプショナル依存 ---

try:
    from pypdf import PdfReader
except ImportError:
    PdfReader = None  # type: ignore[assignment,misc]

try:
    import pdfplumber
except ImportError:
    pdfplumber = None  # type: ignore[assignment]

try:
    from docx import Document as DocxDocument
except ImportError:
    DocxDocument = None  # type: ignore[assignment,misc]

try:
    import openpyxl
except ImportError:
    openpyxl = None  # type: ignore[assignment]


# ---------------------------------------------------------------------------
# 統合パース結果
# ---------------------------------------------------------------------------


@dataclass
class ParseResult:
    """パーサー出力の統一型.

    Attributes:
        content: 抽出されたテキスト全体
        metadata: ファイル由来のメタ情報（タイトル・著者・ページ数等）
        file_type: MIME タイプまたは拡張子
        sections: 構造的セクション（見出し・シート名など）
    """

    content: str
    metadata: dict[str, Any] = field(default_factory=dict)
    file_type: str = ""
    sections: list[str] = field(default_factory=list)


# ---------------------------------------------------------------------------
# 個別パーサー
# ---------------------------------------------------------------------------


class FileParser:
    """ファイルパーサークラス.

    各 ``parse_*`` メソッドは ``ParseResult`` を返す。
    後方互換のため、テキストのみを返す旧 API も維持する。
    """

    # ---- PDF ---------------------------------------------------------------

    @staticmethod
    def parse_pdf(file_stream: IO[bytes]) -> str:
        """PDF からテキスト抽出（後方互換）."""
        return FileParser.parse_pdf_rich(file_stream).content

    @staticmethod
    def parse_pdf_rich(file_stream: IO[bytes]) -> ParseResult:
        """PDF からテキスト + メタデータを抽出."""
        metadata: dict[str, Any] = {"parser": "pdf"}
        sections: list[str] = []

        # pdfplumber 優先
        if pdfplumber is not None:
            try:
                file_stream.seek(0)
                with pdfplumber.open(file_stream) as pdf:
                    pages: list[str] = []
                    for i, page in enumerate(pdf.pages):
                        text = page.extract_text() or ""
                        if text:
                            pages.append(text)
                            sections.append(f"Page {i + 1}")
                    metadata["page_count"] = len(pdf.pages)
                    if hasattr(pdf, "metadata") and pdf.metadata:
                        _merge_pdf_metadata(pdf.metadata, metadata)
                    return ParseResult(
                        content="\n\n".join(pages),
                        metadata=metadata,
                        file_type="application/pdf",
                        sections=sections,
                    )
            except Exception:
                logger.debug("pdfplumber 失敗、pypdf にフォールバック")

        # pypdf フォールバック
        if PdfReader is None:
            msg = "PDF パースには pdfplumber または pypdf が必要: pip install pdfplumber pypdf"
            raise ImportError(msg)

        file_stream.seek(0)
        reader = PdfReader(file_stream)
        pages_text: list[str] = []
        for i, page in enumerate(reader.pages):
            text = page.extract_text() or ""
            if text:
                pages_text.append(text)
                sections.append(f"Page {i + 1}")
        metadata["page_count"] = len(reader.pages)
        if reader.metadata:
            _merge_pdf_metadata(
                {k: getattr(reader.metadata, k, None) for k in ("title", "author", "subject", "creator")},
                metadata,
            )
        return ParseResult(
            content="\n\n".join(pages_text),
            metadata=metadata,
            file_type="application/pdf",
            sections=sections,
        )

    # ---- DOCX --------------------------------------------------------------

    @staticmethod
    def parse_docx(file_stream: IO[bytes]) -> str:
        """Word (docx) からテキスト抽出（後方互換）."""
        return FileParser.parse_docx_rich(file_stream).content

    @staticmethod
    def parse_docx_rich(file_stream: IO[bytes]) -> ParseResult:
        """Word (docx) からテキスト + メタデータを抽出."""
        if DocxDocument is None:
            msg = "python-docx が必要: pip install python-docx"
            raise ImportError(msg)

        file_stream.seek(0)
        doc = DocxDocument(file_stream)
        metadata: dict[str, Any] = {"parser": "docx"}
        sections: list[str] = []

        # ドキュメントプロパティ
        props = doc.core_properties
        if props:
            if props.title:
                metadata["title"] = props.title
            if props.author:
                metadata["author"] = props.author
            if props.subject:
                metadata["subject"] = props.subject
            if props.created:
                metadata["created_at"] = str(props.created)

        # 段落テキスト + 見出し抽出
        paragraphs: list[str] = []
        for para in doc.paragraphs:
            text = para.text.strip()
            if not text:
                continue
            paragraphs.append(text)
            if para.style and para.style.name and para.style.name.startswith("Heading"):
                sections.append(text)

        # テーブルからもテキスト抽出
        table_texts: list[str] = []
        for table in doc.tables:
            rows: list[str] = []
            for row in table.rows:
                cells = [cell.text.strip() for cell in row.cells if cell.text.strip()]
                if cells:
                    rows.append(" | ".join(cells))
            if rows:
                table_texts.append("\n".join(rows))

        content_parts = paragraphs
        if table_texts:
            content_parts.append("\n---\n".join(table_texts))

        metadata["paragraph_count"] = len(paragraphs)
        metadata["table_count"] = len(doc.tables)

        return ParseResult(
            content="\n".join(content_parts),
            metadata=metadata,
            file_type="application/vnd.openxmlformats-officedocument.wordprocessingml.document",
            sections=sections,
        )

    # ---- Excel -------------------------------------------------------------

    @staticmethod
    def parse_excel(file_stream: IO[bytes]) -> str:
        """Excel (.xlsx) からテキスト抽出（後方互換）."""
        return FileParser.parse_excel_rich(file_stream).content

    @staticmethod
    def parse_excel_rich(file_stream: IO[bytes]) -> ParseResult:
        """Excel (.xlsx) からテキスト + メタデータを抽出.

        各シートを「シート名: ヘッダー行ベースの Key-Value」形式で変換する。
        """
        if openpyxl is None:
            msg = "openpyxl が必要: pip install openpyxl"
            raise ImportError(msg)

        file_stream.seek(0)
        wb = openpyxl.load_workbook(file_stream, read_only=True, data_only=True)
        metadata: dict[str, Any] = {
            "parser": "excel",
            "sheet_names": wb.sheetnames,
            "sheet_count": len(wb.sheetnames),
        }

        # ワークブックプロパティ
        if wb.properties:
            if wb.properties.title:
                metadata["title"] = wb.properties.title
            if wb.properties.creator:
                metadata["author"] = wb.properties.creator

        sections: list[str] = []
        sheet_texts: list[str] = []

        for sheet_name in wb.sheetnames:
            ws = wb[sheet_name]
            sections.append(sheet_name)
            rows_data: list[list[str]] = []
            for row in ws.iter_rows(values_only=True):
                cell_values = [str(c) if c is not None else "" for c in row]
                if any(v.strip() for v in cell_values):
                    rows_data.append(cell_values)

            if not rows_data:
                continue

            # 1行目をヘッダーとして扱う
            headers = rows_data[0]
            row_texts: list[str] = []
            for data_row in rows_data[1:]:
                parts: list[str] = []
                for i, val in enumerate(data_row):
                    if not val.strip():
                        continue
                    header = headers[i] if i < len(headers) and headers[i].strip() else f"Col{i + 1}"
                    parts.append(f"{header}: {val}")
                if parts:
                    row_texts.append(", ".join(parts))

            if row_texts:
                sheet_text = f"[シート: {sheet_name}]\n" + "\n".join(row_texts)
            else:
                # ヘッダーのみのシート
                sheet_text = f"[シート: {sheet_name}]\n" + " | ".join(h for h in headers if h.strip())
            sheet_texts.append(sheet_text)

        wb.close()

        metadata["total_rows"] = sum(
            1 for st in sheet_texts for _line in st.split("\n") if _line.strip()
        )

        return ParseResult(
            content="\n\n".join(sheet_texts),
            metadata=metadata,
            file_type="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
            sections=sections,
        )

    # ---- CSV ---------------------------------------------------------------

    @staticmethod
    def parse_csv(file_stream: IO[str]) -> str:
        """CSV からテキスト抽出（後方互換）."""
        return FileParser.parse_csv_rich(file_stream).content

    @staticmethod
    def parse_csv_rich(file_stream: IO[str]) -> ParseResult:
        """CSV からテキスト + メタデータを抽出."""
        reader = csv.reader(file_stream)
        rows = list(reader)
        if not rows:
            return ParseResult(content="", metadata={"parser": "csv"}, file_type="text/csv")

        headers = rows[0]
        text_chunks: list[str] = []
        for row in rows[1:]:
            chunk_parts: list[str] = []
            for i, val in enumerate(row):
                if i < len(headers):
                    chunk_parts.append(f"{headers[i]}: {val}")
                else:
                    chunk_parts.append(val)
            text_chunks.append(", ".join(chunk_parts))

        return ParseResult(
            content="\n---\n".join(text_chunks),
            metadata={
                "parser": "csv",
                "row_count": len(rows) - 1,
                "column_count": len(headers),
                "columns": headers,
            },
            file_type="text/csv",
            sections=headers,
        )

    # ---- プレーンテキスト ---------------------------------------------------

    @staticmethod
    def parse_text(file_content: str) -> str:
        """プレーンテキスト（後方互換）."""
        return file_content

    @staticmethod
    def parse_text_rich(file_content: str, filename: str = "") -> ParseResult:
        """プレーンテキスト + メタデータ."""
        return ParseResult(
            content=file_content,
            metadata={"parser": "text", "char_count": len(file_content)},
            file_type="text/plain",
        )

    # ---- ストリームから自動判定 -------------------------------------------

    @staticmethod
    def parse_auto(
        file_bytes: bytes,
        filename: str,
    ) -> ParseResult:
        """ファイル名の拡張子から適切なパーサーを自動選択して実行.

        Args:
            file_bytes: ファイルバイナリ
            filename: ファイル名（拡張子で判定）

        Returns:
            ParseResult

        Raises:
            ValueError: テキスト抽出に失敗した場合
        """
        ext = Path(filename).suffix.lower()
        try:
            if ext == ".pdf":
                return FileParser.parse_pdf_rich(io.BytesIO(file_bytes))
            if ext in {".docx", ".doc"}:
                return FileParser.parse_docx_rich(io.BytesIO(file_bytes))
            if ext in {".xlsx", ".xls"}:
                return FileParser.parse_excel_rich(io.BytesIO(file_bytes))
            if ext == ".csv":
                text = file_bytes.decode("utf-8")
                return FileParser.parse_csv_rich(io.StringIO(text))
            # テキスト系フォールバック
            text = file_bytes.decode("utf-8")
            return FileParser.parse_text_rich(text, filename)
        except ImportError:
            raise
        except UnicodeDecodeError as exc:
            msg = f"ファイル '{filename}' のテキスト抽出に失敗（エンコーディング不正）"
            raise ValueError(msg) from exc
        except Exception as exc:
            msg = f"ファイル '{filename}' のパースに失敗: {exc}"
            raise ValueError(msg) from exc


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


def _merge_pdf_metadata(source: dict[str, Any] | Any, target: dict[str, Any]) -> None:
    """PDF メタデータを target 辞書にマージ."""
    if not isinstance(source, dict):
        return
    for key in ("title", "author", "subject", "creator"):
        value = source.get(key)
        if value and isinstance(value, str) and value.strip():
            target[key] = value.strip()
