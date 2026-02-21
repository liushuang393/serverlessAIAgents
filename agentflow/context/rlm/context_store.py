"""Context Store - 長文コンテキストの外部保存.

長いコンテキストを外部ストア（メモリ、ファイル、DB）に保存し、
LLMには軽量な「ハンドル」のみを渡す。

設計原則:
- LLMはハンドル経由で必要な部分だけアクセス
- 全文をプロンプトに詰め込まない
- 要約・メタデータで概要を提供

使用例:
    >>> store = ContextStore()
    >>> handle = await store.store("長い文書内容...")
    >>> print(handle.summary)  # "この文書は...についての説明です"
    >>> lines = store.peek(handle.handle_id, start_line=100, num_lines=50)
"""

from __future__ import annotations

import hashlib
import re
import time
import uuid
from dataclasses import dataclass, field
from typing import Any


@dataclass
class ContextHandle:
    """コンテキストハンドル.

    外部保存されたコンテキストへの参照。
    LLMには全文ではなくこのハンドル情報のみを提示。

    Attributes:
        handle_id: ユニークID
        total_tokens: 推定Token数
        total_lines: 行数
        total_chars: 文字数
        summary: 自動生成された要約
        metadata: 追加メタデータ
        content_hash: コンテンツのハッシュ（重複検出用）
        created_at: 作成タイムスタンプ
    """

    handle_id: str
    total_tokens: int
    total_lines: int
    total_chars: int
    summary: str = ""
    metadata: dict[str, Any] = field(default_factory=dict)
    content_hash: str = ""
    created_at: float = field(default_factory=time.time)

    def to_prompt_reference(self) -> str:
        """LLMプロンプト用の参照文字列を生成.

        Returns:
            ハンドル参照文字列
        """
        return (
            f"[Context Handle: {self.handle_id}]\n"
            f"- Lines: {self.total_lines}\n"
            f"- Tokens: ~{self.total_tokens}\n"
            f"- Summary: {self.summary}\n"
            f"Use ctx_peek(), ctx_regex_find(), ctx_keyword_find() to access content."
        )

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換.

        Returns:
            ハンドル情報の辞書
        """
        return {
            "handle_id": self.handle_id,
            "total_tokens": self.total_tokens,
            "total_lines": self.total_lines,
            "total_chars": self.total_chars,
            "summary": self.summary,
            "metadata": self.metadata,
            "content_hash": self.content_hash,
            "created_at": self.created_at,
        }


@dataclass
class StructureInfo:
    """コンテンツ構造情報.

    Attributes:
        headings: 見出し一覧（行番号, レベル, テキスト）
        sections: セクション一覧（開始行, 終了行, タイトル）
        code_blocks: コードブロック位置（開始行, 終了行, 言語）
        tables: テーブル位置（開始行, 終了行）
    """

    headings: list[tuple[int, int, str]] = field(default_factory=list)
    sections: list[tuple[int, int, str]] = field(default_factory=list)
    code_blocks: list[tuple[int, int, str]] = field(default_factory=list)
    tables: list[tuple[int, int]] = field(default_factory=list)

    def to_outline(self, max_depth: int = 3) -> str:
        """アウトライン文字列を生成.

        Args:
            max_depth: 最大見出しレベル

        Returns:
            アウトライン文字列
        """
        lines = []
        for line_no, level, text in self.headings:
            if level <= max_depth:
                indent = "  " * (level - 1)
                lines.append(f"{indent}- L{line_no}: {text}")
        return "\n".join(lines) if lines else "(No headings found)"


class ContextStore:
    """コンテキスト外部ストア.

    長いコンテキストを外部に保存し、ハンドル経由でアクセス。
    デフォルトはインメモリ実装。

    使用例:
        >>> store = ContextStore()
        >>> handle = await store.store("長い文書...")
        >>> lines = store.peek(handle.handle_id, 0, 100)
        >>> matches = store.regex_find(handle.handle_id, r"def \\w+")
    """

    def __init__(self) -> None:
        """初期化."""
        # handle_id -> (content, lines, handle)
        self._storage: dict[str, tuple[str, list[str], ContextHandle]] = {}
        # content_hash -> handle_id（重複検出用）
        self._hash_index: dict[str, str] = {}
        # 構造キャッシュ
        self._structure_cache: dict[str, StructureInfo] = {}

    def _estimate_tokens(self, text: str) -> int:
        """Token数を推定.

        日本語/中国語は1文字≒1.5token、英語は4文字≒1tokenで概算。

        Args:
            text: 対象テキスト

        Returns:
            推定Token数
        """
        if not text:
            return 0

        # CJK文字をカウント
        cjk_chars = len(re.findall(r"[\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff]", text))
        # ASCII文字をカウント
        ascii_chars = len(re.findall(r"[a-zA-Z0-9\s]", text))
        # その他
        other_chars = len(text) - cjk_chars - ascii_chars

        return int(cjk_chars * 1.5 + ascii_chars * 0.25 + other_chars)

    def _generate_summary(self, content: str, max_length: int = 200) -> str:
        """簡易要約を生成.

        先頭と構造から要約を生成。LLMを使った高品質要約はSubCallManagerで実行。

        Args:
            content: コンテンツ
            max_length: 最大長

        Returns:
            要約文字列
        """
        lines = content.split("\n")

        # 最初の非空行を探す
        first_content = ""
        for line in lines[:10]:
            stripped = line.strip()
            if stripped and not stripped.startswith("#"):
                first_content = stripped
                break

        # 見出しを抽出
        headings = []
        for line in lines[:50]:
            if line.startswith("#"):
                headings.append(line.lstrip("#").strip())
                if len(headings) >= 3:
                    break

        # 要約構築
        parts = []
        if headings:
            parts.append(f"Topics: {', '.join(headings[:3])}")
        if first_content:
            truncated = first_content[:100] + "..." if len(first_content) > 100 else first_content
            parts.append(truncated)

        summary = " | ".join(parts) if parts else "(No summary available)"
        return summary[:max_length]

    def _compute_hash(self, content: str) -> str:
        """コンテンツハッシュを計算.

        Args:
            content: コンテンツ

        Returns:
            SHA256ハッシュ（先頭16文字）
        """
        return hashlib.sha256(content.encode()).hexdigest()[:16]

    async def store(
        self,
        content: str,
        metadata: dict[str, Any] | None = None,
        deduplicate: bool = True,
    ) -> ContextHandle:
        """コンテンツを保存.

        Args:
            content: 保存するコンテンツ
            metadata: 追加メタデータ
            deduplicate: 重複検出を行うか

        Returns:
            ContextHandle
        """
        content_hash = self._compute_hash(content)

        # 重複チェック
        if deduplicate and content_hash in self._hash_index:
            existing_id = self._hash_index[content_hash]
            if existing_id in self._storage:
                return self._storage[existing_id][2]

        # 新規保存
        lines = content.split("\n")
        handle = ContextHandle(
            handle_id=f"ctx_{uuid.uuid4().hex[:12]}",
            total_tokens=self._estimate_tokens(content),
            total_lines=len(lines),
            total_chars=len(content),
            summary=self._generate_summary(content),
            metadata=metadata or {},
            content_hash=content_hash,
        )

        self._storage[handle.handle_id] = (content, lines, handle)
        self._hash_index[content_hash] = handle.handle_id

        return handle

    def get_handle(self, handle_id: str) -> ContextHandle | None:
        """ハンドルを取得.

        Args:
            handle_id: ハンドルID

        Returns:
            ContextHandle、または None
        """
        if handle_id in self._storage:
            return self._storage[handle_id][2]
        return None

    def peek(
        self,
        handle_id: str,
        start_line: int = 0,
        num_lines: int = 100,
    ) -> str:
        """指定行範囲を取得.

        Args:
            handle_id: ハンドルID
            start_line: 開始行（0-indexed）
            num_lines: 取得行数

        Returns:
            行内容（行番号付き）

        Raises:
            KeyError: ハンドルが見つからない場合
        """
        if handle_id not in self._storage:
            msg = f"Handle not found: {handle_id}"
            raise KeyError(msg)

        _, lines, _ = self._storage[handle_id]
        end_line = min(start_line + num_lines, len(lines))

        result_lines = []
        for i in range(start_line, end_line):
            result_lines.append(f"{i + 1:5d}| {lines[i]}")

        return "\n".join(result_lines)

    def regex_find(
        self,
        handle_id: str,
        pattern: str,
        max_matches: int = 20,
        context_lines: int = 2,
    ) -> list[dict[str, Any]]:
        """正規表現で検索.

        Args:
            handle_id: ハンドルID
            pattern: 正規表現パターン
            max_matches: 最大マッチ数
            context_lines: 前後のコンテキスト行数

        Returns:
            マッチ結果リスト

        Raises:
            KeyError: ハンドルが見つからない場合
        """
        if handle_id not in self._storage:
            msg = f"Handle not found: {handle_id}"
            raise KeyError(msg)

        _, lines, _ = self._storage[handle_id]
        results: list[dict[str, Any]] = []

        try:
            regex = re.compile(pattern, re.IGNORECASE)
        except re.error as e:
            return [{"error": f"Invalid regex: {e}"}]

        for i, line in enumerate(lines):
            if len(results) >= max_matches:
                break

            match = regex.search(line)
            if match:
                # コンテキスト行を取得
                start = max(0, i - context_lines)
                end = min(len(lines), i + context_lines + 1)
                context = "\n".join(f"{j + 1:5d}{'>' if j == i else ' '}| {lines[j]}" for j in range(start, end))

                results.append(
                    {
                        "line_number": i + 1,
                        "match": match.group(),
                        "line": line,
                        "context": context,
                    }
                )

        return results

    def keyword_find(
        self,
        handle_id: str,
        keywords: list[str],
        operator: str = "OR",
        max_matches: int = 20,
        context_lines: int = 2,
    ) -> list[dict[str, Any]]:
        """キーワードで検索.

        Args:
            handle_id: ハンドルID
            keywords: キーワードリスト
            operator: "AND" または "OR"
            max_matches: 最大マッチ数
            context_lines: 前後のコンテキスト行数

        Returns:
            マッチ結果リスト

        Raises:
            KeyError: ハンドルが見つからない場合
        """
        if handle_id not in self._storage:
            msg = f"Handle not found: {handle_id}"
            raise KeyError(msg)

        _, lines, _ = self._storage[handle_id]
        results: list[dict[str, Any]] = []
        keywords_lower = [kw.lower() for kw in keywords]

        for i, line in enumerate(lines):
            if len(results) >= max_matches:
                break

            line_lower = line.lower()

            # マッチ判定
            if operator == "AND":
                matched = all(kw in line_lower for kw in keywords_lower)
            else:  # OR
                matched = any(kw in line_lower for kw in keywords_lower)

            if matched:
                # コンテキスト行を取得
                start = max(0, i - context_lines)
                end = min(len(lines), i + context_lines + 1)
                context = "\n".join(f"{j + 1:5d}{'>' if j == i else ' '}| {lines[j]}" for j in range(start, end))

                # マッチしたキーワードを特定
                matched_keywords = [
                    kw for kw, kw_lower in zip(keywords, keywords_lower, strict=False) if kw_lower in line_lower
                ]

                results.append(
                    {
                        "line_number": i + 1,
                        "matched_keywords": matched_keywords,
                        "line": line,
                        "context": context,
                    }
                )

        return results

    def get_structure(self, handle_id: str) -> StructureInfo:
        """構造情報を取得.

        見出し、セクション、コードブロック等を抽出。

        Args:
            handle_id: ハンドルID

        Returns:
            StructureInfo

        Raises:
            KeyError: ハンドルが見つからない場合
        """
        if handle_id not in self._storage:
            msg = f"Handle not found: {handle_id}"
            raise KeyError(msg)

        # キャッシュチェック
        if handle_id in self._structure_cache:
            return self._structure_cache[handle_id]

        _, lines, _ = self._storage[handle_id]
        structure = self._extract_structure(lines)
        self._structure_cache[handle_id] = structure

        return structure

    def _extract_structure(self, lines: list[str]) -> StructureInfo:
        """構造を抽出.

        Args:
            lines: 行リスト

        Returns:
            StructureInfo
        """
        headings: list[tuple[int, int, str]] = []
        sections: list[tuple[int, int, str]] = []
        code_blocks: list[tuple[int, int, str]] = []
        tables: list[tuple[int, int]] = []

        in_code_block = False
        code_block_start = 0
        code_block_lang = ""

        current_section_start = 0
        current_section_title = ""

        in_table = False
        table_start = 0

        for i, line in enumerate(lines):
            stripped = line.strip()

            # コードブロック検出
            if stripped.startswith("```"):
                if not in_code_block:
                    in_code_block = True
                    code_block_start = i + 1
                    code_block_lang = stripped[3:].strip()
                else:
                    in_code_block = False
                    code_blocks.append((code_block_start, i + 1, code_block_lang))
                continue

            if in_code_block:
                continue

            # 見出し検出（Markdown形式）
            if stripped.startswith("#"):
                level = len(stripped) - len(stripped.lstrip("#"))
                text = stripped.lstrip("#").strip()
                headings.append((i + 1, level, text))

                # セクション終了
                if current_section_title:
                    sections.append((current_section_start, i, current_section_title))

                current_section_start = i + 1
                current_section_title = text

            # テーブル検出（Markdown形式）
            if "|" in stripped and not in_table:
                in_table = True
                table_start = i + 1
            elif in_table and "|" not in stripped:
                in_table = False
                tables.append((table_start, i))

        # 最後のセクションを閉じる
        if current_section_title:
            sections.append((current_section_start, len(lines), current_section_title))

        # 最後のテーブルを閉じる
        if in_table:
            tables.append((table_start, len(lines)))

        return StructureInfo(
            headings=headings,
            sections=sections,
            code_blocks=code_blocks,
            tables=tables,
        )

    def get_full_content(self, handle_id: str) -> str:
        """全文を取得（注意: 大きいコンテンツでは非推奨）.

        Args:
            handle_id: ハンドルID

        Returns:
            全文

        Raises:
            KeyError: ハンドルが見つからない場合
        """
        if handle_id not in self._storage:
            msg = f"Handle not found: {handle_id}"
            raise KeyError(msg)

        content, _, _ = self._storage[handle_id]
        return content

    def delete(self, handle_id: str) -> bool:
        """コンテキストを削除.

        Args:
            handle_id: ハンドルID

        Returns:
            削除成功ならTrue
        """
        if handle_id not in self._storage:
            return False

        _, _, handle = self._storage[handle_id]

        # ハッシュインデックスから削除
        if handle.content_hash in self._hash_index:
            del self._hash_index[handle.content_hash]

        # 構造キャッシュから削除
        if handle_id in self._structure_cache:
            del self._structure_cache[handle_id]

        # ストレージから削除
        del self._storage[handle_id]

        return True

    def list_handles(self) -> list[ContextHandle]:
        """全ハンドルを取得.

        Returns:
            ハンドルリスト
        """
        return [entry[2] for entry in self._storage.values()]

    def clear(self) -> None:
        """全コンテキストをクリア."""
        self._storage.clear()
        self._hash_index.clear()
        self._structure_cache.clear()

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報辞書
        """
        total_tokens = sum(h.total_tokens for h in self.list_handles())
        total_chars = sum(h.total_chars for h in self.list_handles())
        total_lines = sum(h.total_lines for h in self.list_handles())

        return {
            "context_count": len(self._storage),
            "total_tokens": total_tokens,
            "total_chars": total_chars,
            "total_lines": total_lines,
        }
