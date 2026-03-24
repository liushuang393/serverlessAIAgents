"""FileSystemBackend - ファイルシステム検索バックエンド.

指定ディレクトリ配下のファイルを全文検索する。
ベクトルDB を使わないシンプルなファイル検索ルート。

使用例:
    >>> backend = FileSystemBackend(search_dirs=["/data/docs"])
    >>> result = await backend.retrieve(RetrievalQuery(query="返品ポリシー"))
"""

from __future__ import annotations

import logging
import uuid
from pathlib import Path
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    BackendType,
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)

# サポートするファイル拡張子
_SUPPORTED_EXTENSIONS: frozenset[str] = frozenset({
    ".txt", ".md", ".csv", ".json", ".yaml", ".yml",
    ".html", ".xml", ".log", ".rst", ".toml", ".ini",
})


class FileSystemBackend(RetrievalBackend):
    """ファイルシステム検索バックエンド.

    指定ディレクトリ配下のテキストファイルをスキャンし、
    クエリにマッチするコンテンツを返す。
    """

    def __init__(
        self,
        search_dirs: list[str] | None = None,
        max_file_size_bytes: int = 1_000_000,
        encoding: str = "utf-8",
    ) -> None:
        """初期化.

        Args:
            search_dirs: 検索対象ディレクトリ一覧
            max_file_size_bytes: 読み込み上限ファイルサイズ（バイト）
            encoding: ファイルエンコーディング
        """
        super().__init__(backend_type=BackendType.FILE_SYSTEM, name="file_system")
        self._search_dirs = [Path(d) for d in (search_dirs or [])]
        self._max_file_size = max_file_size_bytes
        self._encoding = encoding

    async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
        """ファイル検索を実行.

        Args:
            query: 統一検索クエリ（options.search_dirs で動的にディレクトリ指定可）

        Returns:
            統一検索結果
        """
        # 動的ディレクトリ指定をサポート
        extra_dirs = query.options.get("search_dirs", [])
        dirs = self._search_dirs + [Path(d) for d in extra_dirs]

        if not dirs:
            return RetrievalResult(
                query=query.query,
                backend_type=BackendType.FILE_SYSTEM,
                metadata={"error": "検索ディレクトリ未指定"},
            )

        documents: list[RetrievedDocument] = []
        query_lower = query.query.lower()
        query_terms = query_lower.split()

        for search_dir in dirs:
            if not search_dir.exists() or not search_dir.is_dir():
                self._logger.warning("ディレクトリ不存在: %s", search_dir)
                continue
            documents.extend(self._search_directory(search_dir, query_terms))

        # スコア降順ソート → top_k 件
        documents.sort(key=lambda d: -d.score)
        documents = documents[: query.top_k]

        return RetrievalResult(
            documents=documents,
            query=query.query,
            total_found=len(documents),
            backend_type=BackendType.FILE_SYSTEM,
            metadata={"search_dirs": [str(d) for d in dirs]},
        )

    async def health_check(self) -> bool:
        """ヘルスチェック."""
        return any(d.exists() and d.is_dir() for d in self._search_dirs)

    def _search_directory(
        self,
        directory: Path,
        query_terms: list[str],
    ) -> list[RetrievedDocument]:
        """ディレクトリ内のファイルを検索."""
        results: list[RetrievedDocument] = []
        for file_path in directory.rglob("*"):
            if not file_path.is_file():
                continue
            if file_path.suffix.lower() not in _SUPPORTED_EXTENSIONS:
                continue
            if file_path.stat().st_size > self._max_file_size:
                continue
            doc = self._search_file(file_path, query_terms)
            if doc is not None:
                results.append(doc)
        return results

    def _search_file(
        self,
        file_path: Path,
        query_terms: list[str],
    ) -> RetrievedDocument | None:
        """ファイル内を検索し、マッチした場合にドキュメントを返す."""
        try:
            content = file_path.read_text(encoding=self._encoding, errors="replace")
        except Exception:
            self._logger.debug("ファイル読み取り失敗: %s", file_path)
            return None

        content_lower = content.lower()
        matched = sum(1 for t in query_terms if t in content_lower)
        if matched == 0:
            return None

        # スコア = マッチ率
        score = matched / max(len(query_terms), 1)

        # コンテキスト抽出（最初のマッチ周辺 500 文字）
        snippet = self._extract_snippet(content, query_terms, max_len=500)

        return RetrievedDocument(
            doc_id=uuid.uuid4().hex[:12],
            content=snippet,
            score=score,
            source=str(file_path),
            metadata={"file_name": file_path.name, "file_size": file_path.stat().st_size},
        )

    def _extract_snippet(self, content: str, terms: list[str], max_len: int = 500) -> str:
        """マッチ周辺のスニペットを抽出."""
        content_lower = content.lower()
        best_pos = 0
        for term in terms:
            pos = content_lower.find(term)
            if pos >= 0:
                best_pos = pos
                break

        start = max(0, best_pos - max_len // 4)
        end = min(len(content), start + max_len)
        snippet = content[start:end]
        if start > 0:
            snippet = "..." + snippet
        if end < len(content):
            snippet = snippet + "..."
        return snippet

