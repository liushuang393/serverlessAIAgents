"""FAQ システムファイルパーサー.

PDF, Word, CSV などのファイルをテキストに変換する。
"""

import csv
from typing import IO


try:
    from pypdf import PdfReader
except ImportError:
    PdfReader = None

try:
    from docx import Document
except ImportError:
    Document = None


class FileParser:
    """ファイルパーサークラス."""

    @staticmethod
    def parse_pdf(file_stream: IO[bytes]) -> str:
        """PDF からテキスト抽出."""
        if not PdfReader:
            msg = "pypdf is not installed"
            raise ImportError(msg)

        reader = PdfReader(file_stream)
        text = []
        for page in reader.pages:
            content = page.extract_text()
            if content:
                text.append(content)
        return "\n".join(text)

    @staticmethod
    def parse_docx(file_stream: IO[bytes]) -> str:
        """Word (docx) からテキスト抽出."""
        if not Document:
            msg = "python-docx is not installed"
            raise ImportError(msg)

        doc = Document(file_stream)
        text = [p.text for p in doc.paragraphs if p.text.strip()]
        return "\n".join(text)

    @staticmethod
    def parse_csv(file_stream: IO[str]) -> str:
        """CSV からテキスト抽出."""
        # Assume CSV is text, read line by line or use csv reader
        # Return format: "Header: value, Header: value..." for each row?
        # Or just raw text?
        # For RAG, structured format is better.
        reader = csv.reader(file_stream)
        rows = list(reader)
        if not rows:
            return ""

        headers = rows[0]
        text_chunks = []
        for row in rows[1:]:
            # Simple "Header: Value" pairs
            chunk_parts = []
            for i, val in enumerate(row):
                if i < len(headers):
                    chunk_parts.append(f"{headers[i]}: {val}")
                else:
                    chunk_parts.append(val)
            text_chunks.append(", ".join(chunk_parts))

        return "\n---\n".join(text_chunks)

    @staticmethod
    def parse_text(file_content: str) -> str:
        """プレーンテキスト."""
        return file_content
