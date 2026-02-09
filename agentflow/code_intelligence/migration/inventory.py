"""CodeInventory - 代码盘点.

遗留系统代码的发现、盘点、依赖分析。

使用例:
    >>> inventory = CodeInventory()
    >>> result = await inventory.scan_directory(Path("./legacy"))
    >>> print(result.total_files, result.total_loc)
    >>> deps = await inventory.analyze_dependencies(result)
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import UTC, datetime
from pathlib import Path
from typing import Any


_logger = logging.getLogger(__name__)


@dataclass
class FileInfo:
    """ファイル情報.

    Attributes:
        path: ファイルパス
        language: 言語
        loc: コード行数
        blank_lines: 空行数
        comment_lines: コメント行数
        size_bytes: ファイルサイズ
        last_modified: 最終更新日時
        encoding: エンコーディング
    """

    path: str
    language: str = ""
    loc: int = 0
    blank_lines: int = 0
    comment_lines: int = 0
    size_bytes: int = 0
    last_modified: datetime | None = None
    encoding: str = "utf-8"

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "path": self.path,
            "language": self.language,
            "loc": self.loc,
            "blank_lines": self.blank_lines,
            "comment_lines": self.comment_lines,
            "size_bytes": self.size_bytes,
            "last_modified": self.last_modified.isoformat() if self.last_modified else None,
            "encoding": self.encoding,
        }


@dataclass
class DependencyInfo:
    """依存情報.

    Attributes:
        source_file: ソースファイル
        target_file: ターゲットファイル
        dependency_type: 依存タイプ (import, include, call, etc.)
        line_number: 行番号
        details: 詳細情報
    """

    source_file: str
    target_file: str
    dependency_type: str
    line_number: int = 0
    details: str = ""

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "source_file": self.source_file,
            "target_file": self.target_file,
            "dependency_type": self.dependency_type,
            "line_number": self.line_number,
            "details": self.details,
        }


@dataclass
class InventoryResult:
    """盘点結果.

    Attributes:
        root_path: ルートパス
        files: ファイル情報リスト
        total_files: 総ファイル数
        total_loc: 総コード行数
        languages: 言語別統計
        dependencies: 依存関係
        scan_time_ms: スキャン時間
    """

    root_path: str
    files: list[FileInfo] = field(default_factory=list)
    total_files: int = 0
    total_loc: int = 0
    languages: dict[str, dict[str, int]] = field(default_factory=dict)
    dependencies: list[DependencyInfo] = field(default_factory=list)
    scan_time_ms: float = 0.0
    scanned_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "root_path": self.root_path,
            "files": [f.to_dict() for f in self.files],
            "total_files": self.total_files,
            "total_loc": self.total_loc,
            "languages": self.languages,
            "dependencies": [d.to_dict() for d in self.dependencies],
            "scan_time_ms": self.scan_time_ms,
            "scanned_at": self.scanned_at.isoformat(),
        }


class CodeInventory:
    """コード盘点.

    コードベースのスキャン、分析を行います。
    """

    # 言語別拡張子マッピング
    EXTENSION_MAP: dict[str, str] = {
        ".cob": "cobol",
        ".cbl": "cobol",
        ".cpy": "cobol",
        ".bas": "vb6",
        ".frm": "vb6",
        ".cls": "vb6",
        ".pas": "delphi",
        ".dfm": "delphi",
        ".dpr": "delphi",
        ".java": "java",
        ".py": "python",
        ".ts": "typescript",
        ".tsx": "typescript",
        ".js": "javascript",
        ".jsx": "javascript",
        ".cs": "csharp",
        ".cpp": "cpp",
        ".c": "c",
        ".h": "c",
        ".go": "go",
        ".rs": "rust",
        ".rb": "ruby",
        ".php": "php",
        ".sql": "sql",
        ".xml": "xml",
        ".json": "json",
        ".yaml": "yaml",
        ".yml": "yaml",
    }

    # 言語別コメントパターン
    COMMENT_PATTERNS: dict[str, dict[str, Any]] = {
        "cobol": {
            "line": re.compile(r"^\s*\*"),
            "block_start": None,
            "block_end": None,
        },
        "java": {
            "line": re.compile(r"^\s*//"),
            "block_start": re.compile(r"/\*"),
            "block_end": re.compile(r"\*/"),
        },
        "python": {
            "line": re.compile(r"^\s*#"),
            "block_start": re.compile(r'"""'),
            "block_end": re.compile(r'"""'),
        },
        "default": {
            "line": re.compile(r"^\s*//"),
            "block_start": re.compile(r"/\*"),
            "block_end": re.compile(r"\*/"),
        },
    }

    # 言語別依存パターン
    DEPENDENCY_PATTERNS: dict[str, list[dict[str, Any]]] = {
        "cobol": [
            {
                "type": "copy",
                "pattern": re.compile(r"COPY\s+(\S+)", re.IGNORECASE),
            },
            {
                "type": "call",
                "pattern": re.compile(r"CALL\s+['\"]?(\S+)['\"]?", re.IGNORECASE),
            },
        ],
        "java": [
            {
                "type": "import",
                "pattern": re.compile(r"import\s+([\w.]+);"),
            },
        ],
        "python": [
            {
                "type": "import",
                "pattern": re.compile(r"^import\s+([\w.]+)"),
            },
            {
                "type": "from_import",
                "pattern": re.compile(r"^from\s+([\w.]+)\s+import"),
            },
        ],
        "typescript": [
            {
                "type": "import",
                "pattern": re.compile(r"import\s+.*\s+from\s+['\"]([^'\"]+)['\"]"),
            },
        ],
    }

    def __init__(self) -> None:
        """初期化."""

    def _detect_language(self, file_path: Path) -> str:
        """言語を検出."""
        ext = file_path.suffix.lower()
        return self.EXTENSION_MAP.get(ext, "unknown")

    def _detect_encoding(self, file_path: Path) -> str:
        """エンコーディングを検出."""
        # 簡易実装（本格実装では chardet を使用）
        encodings = ["utf-8", "shift_jis", "cp1252", "latin-1"]
        for encoding in encodings:
            try:
                file_path.read_text(encoding=encoding)
                return encoding
            except (UnicodeDecodeError, LookupError):
                continue
        return "utf-8"

    def _count_lines(
        self,
        content: str,
        language: str,
    ) -> tuple[int, int, int]:
        """行数をカウント.

        Returns:
            (コード行数, 空行数, コメント行数)
        """
        lines = content.splitlines()
        total = len(lines)
        blank = 0
        comment = 0

        patterns = self.COMMENT_PATTERNS.get(language, self.COMMENT_PATTERNS["default"])
        line_pattern = patterns.get("line")
        in_block = False
        block_start = patterns.get("block_start")
        block_end = patterns.get("block_end")

        for line in lines:
            stripped = line.strip()
            if not stripped:
                blank += 1
                continue

            # ブロックコメント
            if block_start and block_end:
                if in_block:
                    comment += 1
                    if block_end.search(line):
                        in_block = False
                    continue
                if block_start.search(line):
                    comment += 1
                    if not block_end.search(line):
                        in_block = True
                    continue

            # 行コメント
            if line_pattern and line_pattern.match(line):
                comment += 1
                continue

        loc = total - blank - comment
        return loc, blank, comment

    async def scan_file(self, file_path: Path) -> FileInfo:
        """ファイルをスキャン."""
        language = self._detect_language(file_path)
        encoding = self._detect_encoding(file_path)

        try:
            content = file_path.read_text(encoding=encoding, errors="ignore")
            loc, blank, comment = self._count_lines(content, language)
        except Exception as e:
            _logger.warning(f"Failed to read file {file_path}: {e}")
            loc, blank, comment = 0, 0, 0

        stat = file_path.stat()

        return FileInfo(
            path=str(file_path),
            language=language,
            loc=loc,
            blank_lines=blank,
            comment_lines=comment,
            size_bytes=stat.st_size,
            last_modified=datetime.fromtimestamp(stat.st_mtime, tz=UTC),
            encoding=encoding,
        )

    async def scan_directory(
        self,
        root_path: Path | str,
        extensions: list[str] | None = None,
        exclude_patterns: list[str] | None = None,
    ) -> InventoryResult:
        """ディレクトリをスキャン.

        Args:
            root_path: ルートパス
            extensions: スキャン対象拡張子
            exclude_patterns: 除外パターン

        Returns:
            盘点結果
        """
        import time

        start = time.time()
        root_path = Path(root_path)
        result = InventoryResult(root_path=str(root_path))

        exclude_patterns = exclude_patterns or [
            "__pycache__",
            "node_modules",
            ".git",
            ".venv",
            "venv",
            "dist",
            "build",
        ]

        # 拡張子フィルタ
        target_extensions = set(extensions) if extensions else set(self.EXTENSION_MAP.keys())

        # ファイル収集
        for file_path in root_path.rglob("*"):
            if not file_path.is_file():
                continue

            # 除外チェック
            skip = False
            for pattern in exclude_patterns:
                if pattern in str(file_path):
                    skip = True
                    break
            if skip:
                continue

            # 拡張子チェック
            if file_path.suffix.lower() not in target_extensions:
                continue

            # ファイルスキャン
            file_info = await self.scan_file(file_path)
            result.files.append(file_info)

            # 言語別統計
            lang = file_info.language
            if lang not in result.languages:
                result.languages[lang] = {"files": 0, "loc": 0, "bytes": 0}
            result.languages[lang]["files"] += 1
            result.languages[lang]["loc"] += file_info.loc
            result.languages[lang]["bytes"] += file_info.size_bytes

        # 集計
        result.total_files = len(result.files)
        result.total_loc = sum(f.loc for f in result.files)
        result.scan_time_ms = (time.time() - start) * 1000

        _logger.info(
            f"Scanned {result.total_files} files, {result.total_loc} LOC "
            f"in {result.scan_time_ms:.1f}ms"
        )

        return result

    async def analyze_dependencies(
        self,
        inventory: InventoryResult,
    ) -> list[DependencyInfo]:
        """依存関係を分析.

        Args:
            inventory: 盘点結果

        Returns:
            依存関係リスト
        """
        dependencies: list[DependencyInfo] = []

        for file_info in inventory.files:
            file_path = Path(file_info.path)
            patterns = self.DEPENDENCY_PATTERNS.get(file_info.language, [])

            if not patterns:
                continue

            try:
                content = file_path.read_text(
                    encoding=file_info.encoding, errors="ignore"
                )
                lines = content.splitlines()

                for line_no, line in enumerate(lines, 1):
                    for dep_pattern in patterns:
                        match = dep_pattern["pattern"].search(line)
                        if match:
                            dep = DependencyInfo(
                                source_file=file_info.path,
                                target_file=match.group(1),
                                dependency_type=dep_pattern["type"],
                                line_number=line_no,
                                details=line.strip(),
                            )
                            dependencies.append(dep)

            except Exception as e:
                _logger.warning(f"Failed to analyze dependencies in {file_path}: {e}")

        inventory.dependencies = dependencies
        return dependencies

    def get_dependency_graph(
        self,
        dependencies: list[DependencyInfo],
    ) -> dict[str, list[str]]:
        """依存グラフを取得.

        Args:
            dependencies: 依存関係リスト

        Returns:
            依存グラフ（ファイル -> 依存先リスト）
        """
        graph: dict[str, list[str]] = {}
        for dep in dependencies:
            if dep.source_file not in graph:
                graph[dep.source_file] = []
            if dep.target_file not in graph[dep.source_file]:
                graph[dep.source_file].append(dep.target_file)
        return graph

    def find_entry_points(
        self,
        inventory: InventoryResult,
    ) -> list[str]:
        """エントリーポイントを検出.

        Args:
            inventory: 盘点結果

        Returns:
            エントリーポイントファイルリスト
        """
        # 依存されていないファイルを検出
        all_files = {f.path for f in inventory.files}
        dependent_files = {d.target_file for d in inventory.dependencies}

        # 正規化（ファイル名のみで比較）
        dependent_names = set()
        for dep in dependent_files:
            # パスの最後の部分を取得
            name = Path(dep).name if "/" in dep or "\\" in dep else dep
            dependent_names.add(name.lower())

        entry_points = []
        for file_path in all_files:
            name = Path(file_path).name.lower()
            if name not in dependent_names:
                entry_points.append(file_path)

        return sorted(entry_points)

    def calculate_complexity(
        self,
        inventory: InventoryResult,
    ) -> dict[str, float]:
        """複雑度を計算.

        Args:
            inventory: 盘点結果

        Returns:
            ファイル別複雑度
        """
        complexity: dict[str, float] = {}

        dep_graph = self.get_dependency_graph(inventory.dependencies)

        for file_info in inventory.files:
            # 簡易複雑度計算
            # = LOC * 0.1 + 依存数 * 5 + 被依存数 * 3
            loc_score = file_info.loc * 0.1

            # 依存数
            outgoing = len(dep_graph.get(file_info.path, []))
            outgoing_score = outgoing * 5

            # 被依存数
            incoming = sum(
                1 for deps in dep_graph.values() if file_info.path in deps
            )
            incoming_score = incoming * 3

            complexity[file_info.path] = loc_score + outgoing_score + incoming_score

        return complexity


__all__ = [
    "CodeInventory",
    "DependencyInfo",
    "FileInfo",
    "InventoryResult",
]
