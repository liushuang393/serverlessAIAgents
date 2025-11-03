"""
Parser（パーサー）コンポーネント

このモジュールは、多種ドキュメントをテキストに変換するための抽象インターフェースと
具体的な実装を提供します。
"""

import json
import re
from abc import ABC, abstractmethod
from io import BytesIO
from typing import Any, Dict, List, Optional

from ..utils.logging import get_logger
from .models import ParsedDocument

logger = get_logger(__name__)


class ParserInterface(ABC):
    """ドキュメント変換のための抽象インターフェース"""

    @abstractmethod
    async def parse(
        self,
        content: bytes,
        content_type: str,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> ParsedDocument:
        """
        ドキュメントをテキストに変換する

        Args:
            content: ドキュメントのバイナリデータ
            content_type: コンテンツタイプ（MIME type）
            metadata: 追加のメタデータ

        Returns:
            ParsedDocument: パース済みドキュメント
        """
        pass

    @abstractmethod
    def supports(self, content_type: str) -> bool:
        """
        指定されたコンテンツタイプをサポートするか

        Args:
            content_type: チェックするコンテンツタイプ

        Returns:
            bool: サポートしている場合True
        """
        pass


class TextParser(ParserInterface):
    """プレーンテキストパーサー"""

    SUPPORTED_TYPES = [
        "text/plain",
        "text/markdown",
        "text/csv",
        "application/json",
        "application/xml",
        "text/xml",
    ]

    async def parse(
        self,
        content: bytes,
        content_type: str,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> ParsedDocument:
        """
        テキストドキュメントをパースする

        Args:
            content: ドキュメントのバイナリデータ
            content_type: コンテンツタイプ
            metadata: 追加のメタデータ

        Returns:
            ParsedDocument: パース済みドキュメント
        """
        if not self.supports(content_type):
            raise ValueError(f"サポートされていないコンテンツタイプです: {content_type}")

        # エンコーディングを推測してテキストをデコード
        text = self._decode_text(content)

        # コンテンツタイプに応じて特別な処理
        if content_type == "application/json":
            text = self._format_json(text)
        elif content_type in ["application/xml", "text/xml"]:
            text = self._format_xml(text)
        elif content_type == "text/csv":
            text = self._format_csv(text)

        doc_metadata = metadata or {}
        doc_metadata.update(
            {
                "content_type": content_type,
                "encoding": "utf-8",
                "length": len(text),
            }
        )

        logger.debug(f"テキストドキュメントをパースしました（長さ: {len(text)}文字）")

        return ParsedDocument(
            text=text, metadata=doc_metadata, chunks=None, source_type=content_type
        )

    def supports(self, content_type: str) -> bool:
        """
        指定されたコンテンツタイプをサポートするか

        Args:
            content_type: チェックするコンテンツタイプ

        Returns:
            bool: サポートしている場合True
        """
        return content_type in self.SUPPORTED_TYPES

    def _decode_text(self, content: bytes) -> str:
        """
        バイナリデータをテキストにデコードする

        Args:
            content: バイナリデータ

        Returns:
            str: デコードされたテキスト
        """
        # 一般的なエンコーディングを試行
        encodings = ["utf-8", "utf-16", "shift_jis", "euc-jp", "iso-2022-jp"]

        for encoding in encodings:
            try:
                return content.decode(encoding)
            except UnicodeDecodeError:
                continue

        # 全て失敗した場合はエラーを無視してデコード
        return content.decode("utf-8", errors="ignore")

    def _format_json(self, text: str) -> str:
        """
        JSONテキストを整形する

        Args:
            text: JSONテキスト

        Returns:
            str: 整形されたテキスト
        """
        try:
            data = json.loads(text)
            return json.dumps(data, ensure_ascii=False, indent=2)
        except json.JSONDecodeError:
            return text

    def _format_xml(self, text: str) -> str:
        """
        XMLテキストを整形する

        Args:
            text: XMLテキスト

        Returns:
            str: 整形されたテキスト
        """
        # 簡単なXML整形（本格的なパーサーではない）
        text = re.sub(r"><", ">\n<", text)
        return text

    def _format_csv(self, text: str) -> str:
        """
        CSVテキストを整形する

        Args:
            text: CSVテキスト

        Returns:
            str: 整形されたテキスト
        """
        lines = text.strip().split("\n")
        if not lines:
            return text

        # ヘッダー行を特定
        header = lines[0]
        formatted_lines = [f"ヘッダー: {header}"]

        # データ行を追加
        for i, line in enumerate(lines[1:], 1):
            formatted_lines.append(f"行{i}: {line}")

        return "\n".join(formatted_lines)


class HTMLParser(ParserInterface):
    """HTMLパーサー"""

    SUPPORTED_TYPES = ["text/html", "application/xhtml+xml"]

    def __init__(self):
        """HTMLパーサーを初期化する"""
        try:
            from bs4 import BeautifulSoup

            self._BeautifulSoup = BeautifulSoup
            self._available = True
        except ImportError:
            logger.warning("BeautifulSoup4がインストールされていません。HTMLパーサーは利用できません。")
            self._available = False

    async def parse(
        self,
        content: bytes,
        content_type: str,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> ParsedDocument:
        """
        HTMLドキュメントをパースする

        Args:
            content: ドキュメントのバイナリデータ
            content_type: コンテンツタイプ
            metadata: 追加のメタデータ

        Returns:
            ParsedDocument: パース済みドキュメント
        """
        if not self._available:
            raise RuntimeError("BeautifulSoup4がインストールされていません")

        if not self.supports(content_type):
            raise ValueError(f"サポートされていないコンテンツタイプです: {content_type}")

        # HTMLをパース
        html_text = content.decode("utf-8", errors="ignore")
        soup = self._BeautifulSoup(html_text, "html.parser")

        # メタデータを抽出
        doc_metadata = metadata or {}
        doc_metadata.update(
            {
                "content_type": content_type,
                "title": soup.title.string if soup.title else None,
                "encoding": "utf-8",
            }
        )

        # メタタグから追加情報を抽出
        meta_tags = soup.find_all("meta")
        for meta in meta_tags:
            if meta.get("name") == "description":
                doc_metadata["description"] = meta.get("content")
            elif meta.get("name") == "keywords":
                doc_metadata["keywords"] = meta.get("content")

        # テキストを抽出
        text = soup.get_text(separator="\n", strip=True)

        # 空行を削除
        lines = [line.strip() for line in text.split("\n") if line.strip()]
        text = "\n".join(lines)

        doc_metadata["length"] = len(text)

        logger.debug(f"HTMLドキュメントをパースしました（長さ: {len(text)}文字）")

        return ParsedDocument(
            text=text, metadata=doc_metadata, chunks=None, source_type=content_type
        )

    def supports(self, content_type: str) -> bool:
        """
        指定されたコンテンツタイプをサポートするか

        Args:
            content_type: チェックするコンテンツタイプ

        Returns:
            bool: サポートしている場合True
        """
        return self._available and content_type in self.SUPPORTED_TYPES


class PDFParser(ParserInterface):
    """PDFパーサー"""

    SUPPORTED_TYPES = ["application/pdf"]

    def __init__(self):
        """PDFパーサーを初期化する"""
        try:
            from pypdf import PdfReader

            self._PdfReader = PdfReader
            self._available = True
        except ImportError:
            logger.warning("pypdfがインストールされていません。PDFパーサーは利用できません。")
            self._available = False

    async def parse(
        self,
        content: bytes,
        content_type: str,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> ParsedDocument:
        """
        PDFドキュメントをパースする

        Args:
            content: ドキュメントのバイナリデータ
            content_type: コンテンツタイプ
            metadata: 追加のメタデータ

        Returns:
            ParsedDocument: パース済みドキュメント
        """
        if not self._available:
            raise RuntimeError("pypdfがインストールされていません")

        if not self.supports(content_type):
            raise ValueError(f"サポートされていないコンテンツタイプです: {content_type}")

        # PDFを読み込み
        pdf_stream = BytesIO(content)
        reader = self._PdfReader(pdf_stream)

        # メタデータを抽出
        doc_metadata = metadata or {}
        doc_metadata.update(
            {
                "content_type": content_type,
                "page_count": len(reader.pages),
            }
        )

        # PDFメタデータを追加
        if reader.metadata:
            pdf_meta = reader.metadata
            doc_metadata.update(
                {
                    "title": pdf_meta.get("/Title"),
                    "author": pdf_meta.get("/Author"),
                    "subject": pdf_meta.get("/Subject"),
                    "creator": pdf_meta.get("/Creator"),
                    "producer": pdf_meta.get("/Producer"),
                    "creation_date": pdf_meta.get("/CreationDate"),
                    "modification_date": pdf_meta.get("/ModDate"),
                }
            )

        # 全ページからテキストを抽出
        text_parts = []
        for page_num, page in enumerate(reader.pages, 1):
            try:
                page_text = page.extract_text()
                if page_text.strip():
                    text_parts.append(f"=== ページ {page_num} ===\n{page_text}")
            except Exception as e:
                logger.warning(f"ページ {page_num} のテキスト抽出に失敗しました: {e}")
                text_parts.append(f"=== ページ {page_num} ===\n[テキスト抽出エラー]")

        text = "\n\n".join(text_parts)
        doc_metadata["length"] = len(text)

        logger.debug(f"PDFドキュメントをパースしました（{len(reader.pages)}ページ、{len(text)}文字）")

        return ParsedDocument(
            text=text, metadata=doc_metadata, chunks=None, source_type=content_type
        )

    def supports(self, content_type: str) -> bool:
        """
        指定されたコンテンツタイプをサポートするか

        Args:
            content_type: チェックするコンテンツタイプ

        Returns:
            bool: サポートしている場合True
        """
        return self._available and content_type in self.SUPPORTED_TYPES


class MultiParser(ParserInterface):
    """複数のパーサーを統合したパーサー"""

    def __init__(self, parsers: List[ParserInterface] = None):
        """
        マルチパーサーを初期化する

        Args:
            parsers: 使用するパーサーのリスト（Noneの場合はデフォルトパーサーを使用）
        """
        if parsers is None:
            self._parsers = [
                TextParser(),
                HTMLParser(),
                PDFParser(),
            ]
        else:
            self._parsers = parsers

        logger.info(f"マルチパーサーを初期化しました（{len(self._parsers)}個のパーサー）")

    async def parse(
        self,
        content: bytes,
        content_type: str,
        metadata: Optional[Dict[str, Any]] = None,
    ) -> ParsedDocument:
        """
        ドキュメントをパースする

        Args:
            content: ドキュメントのバイナリデータ
            content_type: コンテンツタイプ
            metadata: 追加のメタデータ

        Returns:
            ParsedDocument: パース済みドキュメント
        """
        # 対応するパーサーを探す
        for parser in self._parsers:
            if parser.supports(content_type):
                return await parser.parse(content, content_type, metadata)

        # 対応するパーサーが見つからない場合
        raise ValueError(f"サポートされていないコンテンツタイプです: {content_type}")

    def supports(self, content_type: str) -> bool:
        """
        指定されたコンテンツタイプをサポートするか

        Args:
            content_type: チェックするコンテンツタイプ

        Returns:
            bool: サポートしている場合True
        """
        return any(parser.supports(content_type) for parser in self._parsers)

    def get_supported_types(self) -> List[str]:
        """
        サポートされているコンテンツタイプの一覧を取得する

        Returns:
            List[str]: サポートされているコンテンツタイプのリスト
        """
        supported_types = set()
        for parser in self._parsers:
            if hasattr(parser, "SUPPORTED_TYPES"):
                supported_types.update(parser.SUPPORTED_TYPES)
        return list(supported_types)
