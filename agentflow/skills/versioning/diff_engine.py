"""Diff Engine - 差异对比引擎.

技能版本间的差异对比。

使用例:
    >>> engine = DiffEngine()
    >>> result = engine.diff(old_content, new_content)
    >>> print(result.summary)
"""

from __future__ import annotations

import difflib
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class DiffType(str, Enum):
    """差异类型."""

    ADDED = "added"
    REMOVED = "removed"
    MODIFIED = "modified"
    UNCHANGED = "unchanged"


@dataclass
class DiffLine:
    """差异行.

    Attributes:
        line_number_old: 旧文件行号
        line_number_new: 新文件行号
        content: 内容
        diff_type: 差异类型
    """

    content: str
    diff_type: DiffType
    line_number_old: int | None = None
    line_number_new: int | None = None

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "content": self.content,
            "diff_type": self.diff_type.value,
            "line_number_old": self.line_number_old,
            "line_number_new": self.line_number_new,
        }


@dataclass
class DiffHunk:
    """差异块.

    Attributes:
        old_start: 旧文件起始行
        old_count: 旧文件行数
        new_start: 新文件起始行
        new_count: 新文件行数
        lines: 差异行列表
    """

    old_start: int
    old_count: int
    new_start: int
    new_count: int
    lines: list[DiffLine] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "old_start": self.old_start,
            "old_count": self.old_count,
            "new_start": self.new_start,
            "new_count": self.new_count,
            "lines": [line.to_dict() for line in self.lines],
        }


@dataclass
class DiffResult:
    """差异结果.

    Attributes:
        hunks: 差异块列表
        lines_added: 添加行数
        lines_removed: 删除行数
        lines_modified: 修改行数
        similarity: 相似度 (0-1)
    """

    hunks: list[DiffHunk] = field(default_factory=list)
    lines_added: int = 0
    lines_removed: int = 0
    lines_modified: int = 0
    similarity: float = 1.0

    @property
    def has_changes(self) -> bool:
        """是否有变更."""
        return self.lines_added > 0 or self.lines_removed > 0

    @property
    def total_changes(self) -> int:
        """总变更行数."""
        return self.lines_added + self.lines_removed

    @property
    def summary(self) -> str:
        """变更摘要."""
        if not self.has_changes:
            return "No changes"
        parts = []
        if self.lines_added > 0:
            parts.append(f"+{self.lines_added}")
        if self.lines_removed > 0:
            parts.append(f"-{self.lines_removed}")
        return ", ".join(parts)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "hunks": [hunk.to_dict() for hunk in self.hunks],
            "lines_added": self.lines_added,
            "lines_removed": self.lines_removed,
            "lines_modified": self.lines_modified,
            "similarity": self.similarity,
            "has_changes": self.has_changes,
            "summary": self.summary,
        }

    def to_unified_diff(
        self,
        old_name: str = "old",
        new_name: str = "new",
    ) -> str:
        """生成统一差异格式."""
        lines = [
            f"--- {old_name}",
            f"+++ {new_name}",
        ]

        for hunk in self.hunks:
            header = f"@@ -{hunk.old_start},{hunk.old_count} +{hunk.new_start},{hunk.new_count} @@"
            lines.append(header)

            for diff_line in hunk.lines:
                if diff_line.diff_type == DiffType.ADDED:
                    lines.append(f"+{diff_line.content}")
                elif diff_line.diff_type == DiffType.REMOVED:
                    lines.append(f"-{diff_line.content}")
                else:
                    lines.append(f" {diff_line.content}")

        return "\n".join(lines)


class DiffEngine:
    """差异对比引擎.

    使用标准difflib进行文本差异对比。
    """

    def __init__(self, context_lines: int = 3) -> None:
        """初始化.

        Args:
            context_lines: 上下文行数
        """
        self._context_lines = context_lines

    def diff(
        self,
        old_content: str,
        new_content: str,
    ) -> DiffResult:
        """对比两个内容.

        Args:
            old_content: 旧内容
            new_content: 新内容

        Returns:
            差异结果
        """
        old_lines = old_content.splitlines(keepends=True)
        new_lines = new_content.splitlines(keepends=True)

        # 计算相似度
        matcher = difflib.SequenceMatcher(None, old_content, new_content)
        similarity = matcher.ratio()

        # 生成差异
        differ = difflib.unified_diff(
            old_lines,
            new_lines,
            n=self._context_lines,
        )

        result = DiffResult(similarity=similarity)
        current_hunk: DiffHunk | None = None
        old_line_num = 0
        new_line_num = 0

        for line in differ:
            # 跳过文件头
            if line.startswith(("---", "+++")):
                continue

            # 解析hunk头
            if line.startswith("@@"):
                if current_hunk:
                    result.hunks.append(current_hunk)

                # 解析 @@ -1,3 +1,4 @@
                parts = line.split()
                old_info = parts[1][1:]  # 去掉 -
                new_info = parts[2][1:]  # 去掉 +

                old_parts = old_info.split(",")
                new_parts = new_info.split(",")

                old_start = int(old_parts[0])
                old_count = int(old_parts[1]) if len(old_parts) > 1 else 1
                new_start = int(new_parts[0])
                new_count = int(new_parts[1]) if len(new_parts) > 1 else 1

                current_hunk = DiffHunk(
                    old_start=old_start,
                    old_count=old_count,
                    new_start=new_start,
                    new_count=new_count,
                )
                old_line_num = old_start
                new_line_num = new_start
                continue

            if current_hunk is None:
                continue

            # 解析差异行
            content = line[1:].rstrip("\n\r")  # 去掉前缀和换行

            if line.startswith("+"):
                diff_line = DiffLine(
                    content=content,
                    diff_type=DiffType.ADDED,
                    line_number_new=new_line_num,
                )
                result.lines_added += 1
                new_line_num += 1
            elif line.startswith("-"):
                diff_line = DiffLine(
                    content=content,
                    diff_type=DiffType.REMOVED,
                    line_number_old=old_line_num,
                )
                result.lines_removed += 1
                old_line_num += 1
            else:
                diff_line = DiffLine(
                    content=content,
                    diff_type=DiffType.UNCHANGED,
                    line_number_old=old_line_num,
                    line_number_new=new_line_num,
                )
                old_line_num += 1
                new_line_num += 1

            current_hunk.lines.append(diff_line)

        if current_hunk:
            result.hunks.append(current_hunk)

        return result

    def three_way_diff(
        self,
        base_content: str,
        ours_content: str,
        theirs_content: str,
    ) -> tuple[DiffResult, DiffResult]:
        """三方对比.

        Args:
            base_content: 基础内容
            ours_content: 我们的内容
            theirs_content: 他们的内容

        Returns:
            (base->ours差异, base->theirs差异)
        """
        ours_diff = self.diff(base_content, ours_content)
        theirs_diff = self.diff(base_content, theirs_content)
        return ours_diff, theirs_diff

    def semantic_diff(
        self,
        old_content: str,
        new_content: str,
    ) -> dict[str, Any]:
        """语义差异分析.

        分析变更的语义含义（函数、类等）。

        Args:
            old_content: 旧内容
            new_content: 新内容

        Returns:
            语义差异信息
        """
        result = self.diff(old_content, new_content)

        # 简单的语义分析
        analysis = {
            "functions_added": [],
            "functions_removed": [],
            "functions_modified": [],
            "classes_added": [],
            "classes_removed": [],
            "imports_added": [],
            "imports_removed": [],
        }

        # 检测函数变更
        import re

        func_pattern = re.compile(r"^\s*(def|async def)\s+(\w+)")
        class_pattern = re.compile(r"^\s*class\s+(\w+)")
        import_pattern = re.compile(r"^\s*(import|from)\s+")

        for hunk in result.hunks:
            for line in hunk.lines:
                if line.diff_type == DiffType.ADDED:
                    if func_match := func_pattern.match(line.content):
                        analysis["functions_added"].append(func_match.group(2))
                    elif class_match := class_pattern.match(line.content):
                        analysis["classes_added"].append(class_match.group(1))
                    elif import_pattern.match(line.content):
                        analysis["imports_added"].append(line.content.strip())
                elif line.diff_type == DiffType.REMOVED:
                    if func_match := func_pattern.match(line.content):
                        analysis["functions_removed"].append(func_match.group(2))
                    elif class_match := class_pattern.match(line.content):
                        analysis["classes_removed"].append(class_match.group(1))
                    elif import_pattern.match(line.content):
                        analysis["imports_removed"].append(line.content.strip())

        return analysis


__all__ = [
    "DiffEngine",
    "DiffHunk",
    "DiffLine",
    "DiffResult",
    "DiffType",
]
