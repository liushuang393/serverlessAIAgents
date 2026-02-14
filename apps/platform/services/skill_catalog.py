# -*- coding: utf-8 -*-
"""Skill Catalog Service — agentflow/skills/builtin/ の SKILL.md スキャン.

ビルトインスキルディレクトリを走査し、SKILL.md の YAML frontmatter を
パースしてスキルカタログを構築する。

使用例:
    >>> catalog = SkillCatalogService()
    >>> await catalog.scan()
    >>> skills = catalog.list_skills()
"""

from __future__ import annotations

import logging
import re
from pathlib import Path
from typing import Any

import yaml


_logger = logging.getLogger(__name__)

# YAML frontmatter パターン
_FRONTMATTER_RE = re.compile(r"^---\s*\n(.*?)\n---\s*\n", re.DOTALL)

# デフォルトのビルトインスキルディレクトリ
_DEFAULT_SKILLS_DIR = "agentflow/skills/builtin"


class SkillInfo:
    """スキルメタデータ.

    Attributes:
        name: スキル名
        description: 説明
        version: バージョン
        author: 作成者
        tags: タグ一覧
        triggers: トリガーワード一覧
        requirements: 必要パッケージ
        examples: 使用例
        path: SKILL.md のパス
    """

    __slots__ = (
        "name", "description", "version", "author",
        "tags", "triggers", "requirements", "examples", "path",
    )

    def __init__(
        self,
        name: str,
        description: str = "",
        version: str = "1.0.0",
        author: str = "",
        tags: list[str] | None = None,
        triggers: list[str] | None = None,
        requirements: list[str] | None = None,
        examples: list[str] | None = None,
        path: str = "",
    ) -> None:
        """初期化."""
        self.name = name
        self.description = description
        self.version = version
        self.author = author
        self.tags = tags or []
        self.triggers = triggers or []
        self.requirements = requirements or []
        self.examples = examples or []
        self.path = path

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "description": self.description,
            "version": self.version,
            "author": self.author,
            "tags": self.tags,
            "triggers": self.triggers,
            "requirements": self.requirements,
            "examples": self.examples,
            "path": self.path,
        }


class SkillCatalogService:
    """ビルトインスキルカタログサービス.

    Attributes:
        _skills_dir: スキルディレクトリの絶対パス
        _skills: スキル名 → SkillInfo のマッピング
    """

    def __init__(self, skills_dir: Path | None = None) -> None:
        """初期化.

        Args:
            skills_dir: スキルディレクトリの絶対パス。
                        省略時はカレントディレクトリ配下のデフォルトパスを使用。
        """
        if skills_dir is None:
            skills_dir = Path.cwd() / _DEFAULT_SKILLS_DIR
        self._skills_dir = skills_dir
        self._skills: dict[str, SkillInfo] = {}

    async def scan(self) -> int:
        """スキルディレクトリをスキャンし、SKILL.md を解析.

        Returns:
            検出されたスキル数
        """
        self._skills.clear()

        if not self._skills_dir.is_dir():
            _logger.warning("スキルディレクトリが存在しません: %s", self._skills_dir)
            return 0

        for skill_md in sorted(self._skills_dir.glob("*/SKILL.md")):
            self._parse_skill_md(skill_md)

        _logger.info("SkillCatalog スキャン完了: %d 件検出", len(self._skills))
        return len(self._skills)

    def list_skills(self) -> list[SkillInfo]:
        """全スキルを一覧取得.

        Returns:
            SkillInfo のリスト（名前順）
        """
        return sorted(self._skills.values(), key=lambda s: s.name)

    def get_skill(self, name: str) -> SkillInfo | None:
        """スキル名で検索.

        Args:
            name: スキル名

        Returns:
            見つかった SkillInfo、なければ None
        """
        return self._skills.get(name)

    def search_by_tag(self, tag: str) -> list[SkillInfo]:
        """タグでスキルを検索.

        Args:
            tag: 検索タグ

        Returns:
            マッチした SkillInfo のリスト
        """
        tag_lower = tag.lower()
        return [
            s for s in self.list_skills()
            if any(tag_lower in t.lower() for t in s.tags)
        ]

    def all_tags(self) -> list[dict[str, Any]]:
        """全タグとその出現回数を取得.

        Returns:
            [{"tag": str, "count": int}]
        """
        tag_counts: dict[str, int] = {}
        for skill in self._skills.values():
            for tag in skill.tags:
                tag_counts[tag] = tag_counts.get(tag, 0) + 1
        return sorted(
            [{"tag": t, "count": c} for t, c in tag_counts.items()],
            key=lambda x: x["count"],
            reverse=True,
        )

    def stats(self) -> dict[str, Any]:
        """スキル統計情報.

        Returns:
            統計辞書
        """
        skills = self.list_skills()
        all_tags = {t for s in skills for t in s.tags}
        all_triggers = {t for s in skills for t in s.triggers}
        return {
            "total_skills": len(skills),
            "total_tags": len(all_tags),
            "total_triggers": len(all_triggers),
        }

    # ------------------------------------------------------------------
    # 内部メソッド
    # ------------------------------------------------------------------

    def _parse_skill_md(self, skill_md_path: Path) -> None:
        """SKILL.md の YAML frontmatter をパースして登録."""
        try:
            content = skill_md_path.read_text("utf-8")
            match = _FRONTMATTER_RE.match(content)
            if not match:
                _logger.warning("frontmatter なし: %s", skill_md_path)
                return

            data = yaml.safe_load(match.group(1))
            if not isinstance(data, dict):
                _logger.warning("frontmatter が辞書でない: %s", skill_md_path)
                return

            name = str(data.get("name", skill_md_path.parent.name))
            skill = SkillInfo(
                name=name,
                description=str(data.get("description", "")).strip(),
                version=str(data.get("version", "1.0.0")),
                author=str(data.get("author", "")),
                tags=self._ensure_list(data.get("tags")),
                triggers=self._ensure_list(data.get("triggers")),
                requirements=self._ensure_list(data.get("requirements")),
                examples=self._ensure_list(data.get("examples")),
                path=str(
                    skill_md_path.relative_to(Path.cwd())
                    if skill_md_path.is_relative_to(Path.cwd())
                    else skill_md_path
                ),
            )
            self._skills[name] = skill
        except Exception as exc:  # noqa: BLE001
            _logger.warning("SKILL.md パースエラー (%s): %s", skill_md_path, exc)

    @staticmethod
    def _ensure_list(value: Any) -> list[str]:
        """値をリストに変換."""
        if value is None:
            return []
        if isinstance(value, list):
            return [str(v) for v in value]
        return [str(value)]

