"""ドキュメントローダーモジュール.

様々なファイル形式からドキュメントを読み込み、チャンキングを行います。

対応形式:
- PDF（PyPDF2/pdfplumber）
- Markdown
- CSV
- JSON / JSONL
- HTML
- プレーンテキスト

設計原則:
- 統一インターフェース: 全ローダーが同じ API を提供
- 松耦合: 外部ライブラリはオプショナル
- チャンキング内蔵: 大きなドキュメントを自動分割
"""

from __future__ import annotations

import csv
import hashlib
import json
import logging
import re
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import UTC, datetime
from pathlib import Path
from typing import Any


logger = logging.getLogger(__name__)


@dataclass
class DocumentChunk:
    """ドキュメントチャンク.

    Attributes:
        id: チャンク ID
        content: チャンクの内容
        metadata: メタデータ
        source: ソースファイルパス
        chunk_index: チャンクインデックス
        total_chunks: 全チャンク数
        embedding: 埋め込みベクトル（オプション）
    """

    id: str
    content: str
    metadata: dict[str, Any] = field(default_factory=dict)
    source: str = ""
    chunk_index: int = 0
    total_chunks: int = 1
    embedding: list[float] | None = None

    @classmethod
    def create(
        cls,
        content: str,
        source: str = "",
        metadata: dict[str, Any] | None = None,
        chunk_index: int = 0,
        total_chunks: int = 1,
    ) -> DocumentChunk:
        """チャンクを作成.

        Args:
            content: チャンクの内容
            source: ソースファイルパス
            metadata: メタデータ
            chunk_index: チャンクインデックス
            total_chunks: 全チャンク数

        Returns:
            DocumentChunk インスタンス
        """
        # ID を生成（内容のハッシュ）
        content_hash = hashlib.sha256(content.encode()).hexdigest()[:12]
        chunk_id = f"{content_hash}_{chunk_index}"

        return cls(
            id=chunk_id,
            content=content,
            metadata=metadata or {},
            source=source,
            chunk_index=chunk_index,
            total_chunks=total_chunks,
        )


@dataclass
class ChunkingConfig:
    """チャンキング設定.

    Attributes:
        chunk_size: チャンクサイズ（文字数）
        chunk_overlap: チャンク間のオーバーラップ（文字数）
        separator: 分割に使用するセパレータ
        keep_separator: セパレータを保持するか
    """

    chunk_size: int = 1000
    chunk_overlap: int = 200
    separator: str = "\n\n"
    keep_separator: bool = True


class DocumentLoader(ABC):
    """ドキュメントローダー基底クラス."""

    def __init__(
        self,
        chunking_config: ChunkingConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            chunking_config: チャンキング設定
        """
        self._chunking_config = chunking_config or ChunkingConfig()
        self._logger = logging.getLogger(self.__class__.__name__)

    @abstractmethod
    async def load(self, source: str | Path) -> list[DocumentChunk]:
        """ドキュメントを読み込み.

        Args:
            source: ソースパス

        Returns:
            DocumentChunk のリスト
        """

    @abstractmethod
    def supports(self, source: str | Path) -> bool:
        """このローダーがソースをサポートするか判定.

        Args:
            source: ソースパス

        Returns:
            サポートする場合 True
        """

    def _chunk_text(self, text: str, source: str = "") -> list[DocumentChunk]:
        """テキストをチャンクに分割.

        Args:
            text: 入力テキスト
            source: ソースパス

        Returns:
            DocumentChunk のリスト
        """
        config = self._chunking_config

        # セパレータで分割
        if config.separator:
            parts = text.split(config.separator)
            if config.keep_separator and len(parts) > 1:
                parts = [(config.separator if i > 0 else "") + part for i, part in enumerate(parts)]
        else:
            parts = [text]

        # チャンクを構築
        chunks: list[str] = []
        current_chunk = ""

        for part in parts:
            if len(current_chunk) + len(part) <= config.chunk_size:
                current_chunk += part
            else:
                if current_chunk:
                    chunks.append(current_chunk)
                current_chunk = part

                # 長すぎるパートは分割
                while len(current_chunk) > config.chunk_size:
                    chunks.append(current_chunk[: config.chunk_size])
                    current_chunk = current_chunk[config.chunk_size - config.chunk_overlap :]

        if current_chunk:
            chunks.append(current_chunk)

        # DocumentChunk を作成
        total_chunks = len(chunks)
        return [
            DocumentChunk.create(
                content=chunk.strip(),
                source=source,
                chunk_index=i,
                total_chunks=total_chunks,
            )
            for i, chunk in enumerate(chunks)
            if chunk.strip()
        ]


class TextLoader(DocumentLoader):
    """プレーンテキストローダー."""

    async def load(self, source: str | Path) -> list[DocumentChunk]:
        """テキストファイルを読み込み."""
        path = Path(source)
        content = path.read_text(encoding="utf-8")

        chunks = self._chunk_text(content, str(path))
        for chunk in chunks:
            chunk.metadata.update(
                {
                    "loader": "text",
                    "file_size": path.stat().st_size,
                    "loaded_at": datetime.now(UTC).isoformat(),
                }
            )

        self._logger.info(f"Loaded {len(chunks)} chunks from {path}")
        return chunks

    def supports(self, source: str | Path) -> bool:
        """テキストファイルをサポート."""
        path = Path(source)
        return path.suffix.lower() in {".txt", ".text"}


class MarkdownLoader(DocumentLoader):
    """Markdown ローダー."""

    async def load(self, source: str | Path) -> list[DocumentChunk]:
        """Markdown ファイルを読み込み."""
        path = Path(source)
        content = path.read_text(encoding="utf-8")

        # Markdown のセクションで分割
        chunks = self._chunk_markdown(content, str(path))
        for chunk in chunks:
            chunk.metadata.update(
                {
                    "loader": "markdown",
                    "file_size": path.stat().st_size,
                    "loaded_at": datetime.now(UTC).isoformat(),
                }
            )

        self._logger.info(f"Loaded {len(chunks)} chunks from {path}")
        return chunks

    def _chunk_markdown(self, content: str, source: str) -> list[DocumentChunk]:
        """Markdown をセクションで分割."""
        # ヘッダーで分割
        sections = re.split(r"(^#+\s+.*$)", content, flags=re.MULTILINE)

        chunks: list[DocumentChunk] = []
        current_section = ""
        current_header = ""

        for part in sections:
            if re.match(r"^#+\s+", part):
                # ヘッダー行
                if current_section:
                    chunks.extend(self._chunk_text(current_section, source))
                current_header = part
                current_section = part
            else:
                current_section += part

        if current_section:
            chunks.extend(self._chunk_text(current_section, source))

        # チャンクにセクション情報を追加
        for chunk in chunks:
            if current_header:
                chunk.metadata["section"] = current_header.strip()

        return chunks

    def supports(self, source: str | Path) -> bool:
        """Markdown ファイルをサポート."""
        path = Path(source)
        return path.suffix.lower() in {".md", ".markdown"}


class CSVLoader(DocumentLoader):
    """CSV ローダー."""

    def __init__(
        self,
        chunking_config: ChunkingConfig | None = None,
        content_columns: list[str] | None = None,
        metadata_columns: list[str] | None = None,
    ) -> None:
        """初期化.

        Args:
            chunking_config: チャンキング設定
            content_columns: コンテンツとして使用する列名
            metadata_columns: メタデータとして使用する列名
        """
        super().__init__(chunking_config)
        self._content_columns = content_columns
        self._metadata_columns = metadata_columns

    async def load(self, source: str | Path) -> list[DocumentChunk]:
        """CSV ファイルを読み込み."""
        path = Path(source)
        chunks: list[DocumentChunk] = []

        with path.open(encoding="utf-8") as f:
            reader = csv.DictReader(f)
            for i, row in enumerate(reader):
                # コンテンツ列を決定
                if self._content_columns:
                    content_parts = [row.get(col, "") for col in self._content_columns]
                    content = " | ".join(filter(None, content_parts))
                else:
                    content = " | ".join(str(v) for v in row.values())

                # メタデータ列を決定
                if self._metadata_columns:
                    metadata = {col: row.get(col, "") for col in self._metadata_columns}
                else:
                    metadata = dict(row)

                metadata.update(
                    {
                        "loader": "csv",
                        "row_index": i,
                        "loaded_at": datetime.now(UTC).isoformat(),
                    }
                )

                chunk = DocumentChunk.create(
                    content=content,
                    source=str(path),
                    metadata=metadata,
                    chunk_index=i,
                )
                chunks.append(chunk)

        self._logger.info(f"Loaded {len(chunks)} rows from {path}")
        return chunks

    def supports(self, source: str | Path) -> bool:
        """CSV ファイルをサポート."""
        path = Path(source)
        return path.suffix.lower() in {".csv", ".tsv"}


class JSONLoader(DocumentLoader):
    """JSON / JSONL ローダー."""

    def __init__(
        self,
        chunking_config: ChunkingConfig | None = None,
        content_key: str | None = None,
        metadata_keys: list[str] | None = None,
        jq_filter: str | None = None,
    ) -> None:
        """初期化.

        Args:
            chunking_config: チャンキング設定
            content_key: コンテンツとして使用するキー
            metadata_keys: メタデータとして使用するキー
            jq_filter: jq 式フィルタ（オプション）
        """
        super().__init__(chunking_config)
        self._content_key = content_key
        self._metadata_keys = metadata_keys
        self._jq_filter = jq_filter

    async def load(self, source: str | Path) -> list[DocumentChunk]:
        """JSON/JSONL ファイルを読み込み."""
        path = Path(source)
        chunks: list[DocumentChunk] = []

        if path.suffix.lower() == ".jsonl":
            # JSONL: 行ごとに JSON
            with path.open(encoding="utf-8") as f:
                for i, line in enumerate(f):
                    if line.strip():
                        obj = json.loads(line)
                        chunk = self._create_chunk_from_object(obj, str(path), i)
                        chunks.append(chunk)
        else:
            # JSON: 単一オブジェクトまたは配列
            with path.open(encoding="utf-8") as f:
                data = json.load(f)

            if isinstance(data, list):
                for i, obj in enumerate(data):
                    chunk = self._create_chunk_from_object(obj, str(path), i)
                    chunks.append(chunk)
            else:
                chunk = self._create_chunk_from_object(data, str(path), 0)
                chunks.append(chunk)

        self._logger.info(f"Loaded {len(chunks)} objects from {path}")
        return chunks

    def _create_chunk_from_object(self, obj: dict[str, Any], source: str, index: int) -> DocumentChunk:
        """オブジェクトからチャンクを作成."""
        # コンテンツを抽出
        if self._content_key and self._content_key in obj:
            content = str(obj[self._content_key])
        else:
            content = json.dumps(obj, ensure_ascii=False, indent=2)

        # メタデータを抽出
        if self._metadata_keys:
            metadata = {k: obj.get(k) for k in self._metadata_keys if k in obj}
        else:
            metadata = {}

        metadata.update(
            {
                "loader": "json",
                "object_index": index,
                "loaded_at": datetime.now(UTC).isoformat(),
            }
        )

        return DocumentChunk.create(
            content=content,
            source=source,
            metadata=metadata,
            chunk_index=index,
        )

    def supports(self, source: str | Path) -> bool:
        """JSON/JSONL ファイルをサポート."""
        path = Path(source)
        return path.suffix.lower() in {".json", ".jsonl"}


class PDFLoader(DocumentLoader):
    """PDF ローダー（PyPDF2 または pdfplumber）."""

    async def load(self, source: str | Path) -> list[DocumentChunk]:
        """PDF ファイルを読み込み."""
        path = Path(source)
        text = await self._extract_text(path)

        chunks = self._chunk_text(text, str(path))
        for chunk in chunks:
            chunk.metadata.update(
                {
                    "loader": "pdf",
                    "file_size": path.stat().st_size,
                    "loaded_at": datetime.now(UTC).isoformat(),
                }
            )

        self._logger.info(f"Loaded {len(chunks)} chunks from {path}")
        return chunks

    async def _extract_text(self, path: Path) -> str:
        """PDF からテキストを抽出."""
        # pdfplumber を試行
        try:
            import pdfplumber

            with pdfplumber.open(path) as pdf:
                pages = [page.extract_text() or "" for page in pdf.pages]
                return "\n\n".join(pages)
        except ImportError:
            pass

        # PyPDF2 を試行
        try:
            from pypdf import PdfReader

            reader = PdfReader(path)
            pages = [page.extract_text() or "" for page in reader.pages]
            return "\n\n".join(pages)
        except ImportError:
            pass

        msg = "PDF reading requires pdfplumber or pypdf: pip install pdfplumber pypdf"
        raise ImportError(msg)

    def supports(self, source: str | Path) -> bool:
        """PDF ファイルをサポート."""
        path = Path(source)
        return path.suffix.lower() == ".pdf"


class HTMLLoader(DocumentLoader):
    """HTML ローダー."""

    async def load(self, source: str | Path) -> list[DocumentChunk]:
        """HTML ファイルを読み込み."""
        path = Path(source)
        content = path.read_text(encoding="utf-8")

        # HTML タグを除去
        text = self._strip_html(content)

        chunks = self._chunk_text(text, str(path))
        for chunk in chunks:
            chunk.metadata.update(
                {
                    "loader": "html",
                    "file_size": path.stat().st_size,
                    "loaded_at": datetime.now(UTC).isoformat(),
                }
            )

        self._logger.info(f"Loaded {len(chunks)} chunks from {path}")
        return chunks

    def _strip_html(self, html: str) -> str:
        """HTML タグを除去."""
        try:
            from bs4 import BeautifulSoup

            soup = BeautifulSoup(html, "html.parser")

            # script, style タグを除去
            for tag in soup(["script", "style", "nav", "footer", "header"]):
                tag.decompose()

            return str(soup.get_text(separator="\n", strip=True))
        except ImportError:
            # BeautifulSoup がない場合は正規表現で除去
            text = re.sub(r"<script[^>]*>.*?</script>", "", html, flags=re.DOTALL)
            text = re.sub(r"<style[^>]*>.*?</style>", "", text, flags=re.DOTALL)
            return re.sub(r"<[^>]+>", "", text)

    def supports(self, source: str | Path) -> bool:
        """HTML ファイルをサポート."""
        path = Path(source)
        return path.suffix.lower() in {".html", ".htm"}


class UniversalLoader:
    """統一ドキュメントローダー.

    ファイル拡張子に基づいて適切なローダーを自動選択します。

    Example:
        >>> loader = UniversalLoader()
        >>> chunks = await loader.load("document.pdf")
        >>> chunks = await loader.load("data.csv")
    """

    def __init__(
        self,
        chunking_config: ChunkingConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            chunking_config: チャンキング設定
        """
        self._chunking_config = chunking_config or ChunkingConfig()
        self._loaders: list[DocumentLoader] = [
            PDFLoader(self._chunking_config),
            MarkdownLoader(self._chunking_config),
            HTMLLoader(self._chunking_config),
            CSVLoader(self._chunking_config),
            JSONLoader(self._chunking_config),
            TextLoader(self._chunking_config),
        ]
        self._logger = logging.getLogger(__name__)

    async def load(self, source: str | Path) -> list[DocumentChunk]:
        """ドキュメントを読み込み（自動ローダー選択）.

        Args:
            source: ソースパス

        Returns:
            DocumentChunk のリスト

        Raises:
            ValueError: サポートされていないファイル形式の場合
        """
        path = Path(source)

        for loader in self._loaders:
            if loader.supports(path):
                return await loader.load(path)

        # テキストとしてフォールバック
        self._logger.warning(f"Unknown file type: {path.suffix}, loading as text")
        return await TextLoader(self._chunking_config).load(path)

    async def load_directory(
        self,
        directory: str | Path,
        pattern: str = "*",
        recursive: bool = True,
    ) -> list[DocumentChunk]:
        """ディレクトリ内のドキュメントを一括読み込み.

        Args:
            directory: ディレクトリパス
            pattern: ファイルパターン（glob）
            recursive: 再帰的に読み込むか

        Returns:
            DocumentChunk のリスト
        """
        dir_path = Path(directory)
        all_chunks: list[DocumentChunk] = []

        files = list(dir_path.rglob(pattern)) if recursive else list(dir_path.glob(pattern))

        for file_path in files:
            if file_path.is_file():
                try:
                    chunks = await self.load(file_path)
                    all_chunks.extend(chunks)
                except Exception as e:
                    self._logger.warning(f"Failed to load {file_path}: {e}")

        self._logger.info(f"Loaded {len(all_chunks)} chunks from {len(files)} files in {directory}")
        return all_chunks
