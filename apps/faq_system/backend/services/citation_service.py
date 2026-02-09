"""引用サービス（Citation Service）.

回答の引用ソース管理と表示。

機能:
- 引用情報の構造化
- ソース検証
- 引用フォーマット生成

使用例:
    >>> from apps.faq_system.backend.services import CitationService
    >>>
    >>> service = CitationService()
    >>> citation = service.create_citation(document, snippet)
    >>> formatted = service.format_citations([citation])
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class CitationStyle(str, Enum):
    """引用スタイル."""

    INLINE = "inline"  # [1] 形式
    FOOTNOTE = "footnote"  # 脚注形式
    FULL = "full"  # 完全引用
    COMPACT = "compact"  # 簡略引用


class SourceType(str, Enum):
    """ソースタイプ."""

    DOCUMENT = "document"
    PDF = "pdf"
    WEB = "web"
    EMAIL = "email"
    DATABASE = "database"
    WIKI = "wiki"


@dataclass
class SourceInfo:
    """ソース情報.

    Attributes:
        source_id: ソースID
        source_type: ソースタイプ
        title: タイトル
        url: URL
        path: ファイルパス
        version: バージョン
        created_at: 作成日時
        updated_at: 更新日時
        effective_date: 有効開始日
        expiry_date: 有効期限
        author: 著者
        owner_department: オーナー部門
        classification: 機密分類
    """

    source_id: str
    source_type: SourceType = SourceType.DOCUMENT
    title: str = ""
    url: str = ""
    path: str = ""
    version: str = ""
    created_at: datetime | None = None
    updated_at: datetime | None = None
    effective_date: datetime | None = None
    expiry_date: datetime | None = None
    author: str = ""
    owner_department: str = ""
    classification: str = "internal"


@dataclass
class Citation:
    """引用.

    Attributes:
        citation_id: 引用ID
        index: 引用番号
        source: ソース情報
        snippet: 引用箇所
        page_number: ページ番号
        section: セクション
        line_range: 行範囲
        relevance_score: 関連度スコア
        verification_status: 検証ステータス
    """

    citation_id: str
    index: int
    source: SourceInfo
    snippet: str = ""
    page_number: int | None = None
    section: str = ""
    line_range: tuple[int, int] | None = None
    relevance_score: float = 0.0
    verification_status: str = "unverified"  # verified, unverified, expired
    created_at: datetime = field(default_factory=datetime.now)


@dataclass
class CitationServiceConfig:
    """引用サービス設定."""

    # スタイル
    default_style: CitationStyle = CitationStyle.INLINE

    # 表示
    max_snippet_length: int = 200
    show_page_number: bool = True
    show_version: bool = True
    show_update_date: bool = True
    show_owner_department: bool = True
    show_effective_date: bool = True

    # 検証
    verify_source_exists: bool = True
    warn_on_expired: bool = True


class CitationService:
    """引用サービス.

    回答の引用ソース管理と表示。

    Example:
        >>> service = CitationService()
        >>>
        >>> # 引用作成
        >>> source = SourceInfo(
        ...     source_id="doc-001",
        ...     title="就業規則",
        ...     version="2024.1",
        ... )
        >>> citation = service.create_citation(
        ...     source=source,
        ...     snippet="年次有給休暇は...",
        ...     page_number=15,
        ... )
        >>>
        >>> # フォーマット
        >>> formatted = service.format_citations([citation])
    """

    def __init__(
        self,
        config: CitationServiceConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or CitationServiceConfig()
        self._citations: dict[str, Citation] = {}
        self._counter = 0
        self._logger = logging.getLogger(__name__)

    def create_citation(
        self,
        source: SourceInfo,
        snippet: str = "",
        page_number: int | None = None,
        section: str = "",
        relevance_score: float = 0.0,
    ) -> Citation:
        """引用を作成.

        Args:
            source: ソース情報
            snippet: 引用箇所
            page_number: ページ番号
            section: セクション
            relevance_score: 関連度スコア

        Returns:
            引用
        """
        self._counter += 1

        citation_id = f"cite-{self._counter:04d}"

        # スニペットの長さ調整
        if len(snippet) > self._config.max_snippet_length:
            snippet = snippet[:self._config.max_snippet_length] + "..."

        # 検証ステータス
        verification_status = "unverified"
        if self._config.warn_on_expired and source.expiry_date:
            if source.expiry_date < datetime.now():
                verification_status = "expired"

        citation = Citation(
            citation_id=citation_id,
            index=self._counter,
            source=source,
            snippet=snippet,
            page_number=page_number,
            section=section,
            relevance_score=relevance_score,
            verification_status=verification_status,
        )

        self._citations[citation_id] = citation

        return citation

    def format_citations(
        self,
        citations: list[Citation],
        style: CitationStyle | None = None,
    ) -> str:
        """引用をフォーマット.

        Args:
            citations: 引用リスト
            style: 引用スタイル

        Returns:
            フォーマット済み文字列
        """
        style = style or self._config.default_style

        if style == CitationStyle.INLINE:
            return self._format_inline(citations)
        if style == CitationStyle.FOOTNOTE:
            return self._format_footnote(citations)
        if style == CitationStyle.FULL:
            return self._format_full(citations)
        return self._format_compact(citations)

    def _format_inline(self, citations: list[Citation]) -> str:
        """インラインスタイル."""
        lines = ["\n---\n**引用:**\n"]

        for c in citations:
            line = f"[{c.index}] {c.source.title}"

            if self._config.show_version and c.source.version:
                line += f" (ver. {c.source.version})"

            if self._config.show_page_number and c.page_number:
                line += f", p.{c.page_number}"

            if self._config.show_update_date and c.source.updated_at:
                line += f", 更新: {c.source.updated_at.strftime('%Y-%m-%d')}"

            if c.verification_status == "expired":
                line += " ⚠️有効期限切れ"

            lines.append(line)

        return "\n".join(lines)

    def _format_footnote(self, citations: list[Citation]) -> str:
        """脚注スタイル."""
        lines = []

        for c in citations:
            lines.append(
                f"^[{c.index}]: {c.source.title}"
            )
            if c.snippet:
                lines.append(f"    > {c.snippet}")

        return "\n".join(lines)

    def _format_full(self, citations: list[Citation]) -> str:
        """完全引用スタイル."""
        lines = ["\n---\n## 引用情報\n"]

        for c in citations:
            lines.append(f"### [{c.index}] {c.source.title}")
            lines.append("")

            if c.source.url:
                lines.append(f"- **URL**: {c.source.url}")
            if c.source.path:
                lines.append(f"- **パス**: {c.source.path}")
            if c.source.version:
                lines.append(f"- **バージョン**: {c.source.version}")
            if c.source.updated_at:
                lines.append(
                    f"- **更新日**: {c.source.updated_at.strftime('%Y-%m-%d')}"
                )
            if c.source.effective_date:
                lines.append(
                    f"- **有効開始**: {c.source.effective_date.strftime('%Y-%m-%d')}"
                )
            if c.source.expiry_date:
                lines.append(
                    f"- **有効期限**: {c.source.expiry_date.strftime('%Y-%m-%d')}"
                )
            if c.source.owner_department:
                lines.append(f"- **管理部門**: {c.source.owner_department}")
            if c.page_number:
                lines.append(f"- **ページ**: {c.page_number}")
            if c.section:
                lines.append(f"- **セクション**: {c.section}")

            lines.append("")

            if c.snippet:
                lines.append(f"> {c.snippet}")
                lines.append("")

            if c.verification_status == "expired":
                lines.append("⚠️ **注意**: この情報は有効期限が切れています")
                lines.append("")

        return "\n".join(lines)

    def _format_compact(self, citations: list[Citation]) -> str:
        """簡略引用スタイル."""
        items = []

        for c in citations:
            item = f"[{c.index}] {c.source.title}"
            if c.source.version:
                item += f" v{c.source.version}"
            items.append(item)

        return "引用: " + ", ".join(items)

    def insert_citation_markers(
        self,
        text: str,
        citations: list[Citation],
    ) -> str:
        """テキストに引用マーカーを挿入.

        Args:
            text: テキスト
            citations: 引用リスト

        Returns:
            マーカー挿入済みテキスト
        """
        # 各引用のスニペットを検索してマーカーを挿入
        result = text

        for c in citations:
            if c.snippet and len(c.snippet) > 20:
                # スニペットの一部を検索
                snippet_part = c.snippet[:50]
                if snippet_part in result:
                    # 最初の出現にマーカーを追加
                    result = result.replace(
                        snippet_part,
                        f"{snippet_part} [{c.index}]",
                        1,
                    )

        return result

    def validate_citations(
        self, citations: list[Citation]
    ) -> list[dict[str, Any]]:
        """引用を検証.

        Args:
            citations: 引用リスト

        Returns:
            検証結果リスト
        """
        results = []

        for c in citations:
            issues = []

            # 有効期限チェック
            if c.source.expiry_date and c.source.expiry_date < datetime.now():
                issues.append({
                    "type": "expired",
                    "message": "ソースが有効期限切れです",
                })

            # バージョンチェック（形式のみ）
            if not c.source.version:
                issues.append({
                    "type": "warning",
                    "message": "バージョン情報がありません",
                })

            # 更新日チェック
            if c.source.updated_at:
                days_old = (datetime.now() - c.source.updated_at).days
                if days_old > 365:
                    issues.append({
                        "type": "warning",
                        "message": f"1年以上前の情報です（{days_old}日前）",
                    })

            results.append({
                "citation_id": c.citation_id,
                "source_title": c.source.title,
                "issues": issues,
                "valid": len([i for i in issues if i["type"] == "expired"]) == 0,
            })

        return results

    def get_citation_summary(
        self, citations: list[Citation]
    ) -> dict[str, Any]:
        """引用サマリーを取得.

        Args:
            citations: 引用リスト

        Returns:
            サマリー
        """
        return {
            "total_citations": len(citations),
            "unique_sources": len({c.source.source_id for c in citations}),
            "average_relevance": (
                sum(c.relevance_score for c in citations) / len(citations)
                if citations else 0
            ),
            "expired_count": len([
                c for c in citations if c.verification_status == "expired"
            ]),
            "sources": [
                {
                    "title": c.source.title,
                    "type": c.source.source_type.value,
                    "version": c.source.version,
                }
                for c in citations
            ],
        }


__all__ = [
    "Citation",
    "CitationService",
    "CitationServiceConfig",
    "CitationStyle",
    "SourceInfo",
    "SourceType",
]
