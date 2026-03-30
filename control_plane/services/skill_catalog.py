"""Skill Catalog Service — kernel/skills/builtin/ の SKILL.md スキャン.

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

from control_plane.services.capability_registry import CapabilityRegistry


_logger = logging.getLogger(__name__)

# YAML frontmatter パターン
_FRONTMATTER_RE = re.compile(r"^---\s*\n(.*?)\n---\s*\n", re.DOTALL)

# デフォルトのビルトインスキルディレクトリ
_DEFAULT_SKILLS_DIR = "kernel/skills/builtin"
_PRIMARY_CONFIG_DIR_NAME = ".bizcore"

# ------------------------------------------------------------------
# 利用シナリオベースのカテゴリ定義
# ------------------------------------------------------------------

# カテゴリ ID → 表示順序（UI のセクション並び順に使用）
SKILL_CATEGORIES: dict[str, dict[str, Any]] = {
    "common": {
        "order": 0,
        "icon": "🔧",
    },
    "code_development": {
        "order": 1,
        "icon": "💻",
    },
    "web_search": {
        "order": 2,
        "icon": "🔍",
    },
    "enterprise_office": {
        "order": 3,
        "icon": "🏢",
    },
    "ad_marketing": {
        "order": 4,
        "icon": "📢",
    },
    "enterprise_workflow": {
        "order": 5,
        "icon": "⚙️",
    },
    "ai_assistant": {
        "order": 6,
        "icon": "🤖",
    },
    "media_creative": {
        "order": 7,
        "icon": "🎨",
    },
}

# タグ・名前キーワードからカテゴリを推論するマッピング
_CATEGORY_INFERENCE_MAP: dict[str, str] = {
    # common（共通基盤）
    "retrieval": "common",
    "knowledge": "common",
    "search": "common",
    "core-skill": "common",
    "authentication": "common",
    "security": "common",
    "identity": "common",
    "export": "common",
    "backup": "common",
    # code_development（コード開発・DevOps）
    "code-analysis": "code_development",
    "migration": "code_development",
    "scaffolding": "code_development",
    "fullstack": "code_development",
    "docker": "code_development",
    "database": "code_development",
    "alembic": "code_development",
    "workflow": "code_development",
    "development": "code_development",
    "agile": "code_development",
    "deployment": "code_development",
    "infrastructure": "code_development",
    "serverless": "code_development",
    "backend": "code_development",
    # web_search（Web検索・情報収集）
    "trend-analysis": "web_search",
    "market-intelligence": "web_search",
    "news-analysis": "web_search",
    "sentiment": "web_search",
    "web-search": "web_search",
    # enterprise_office（企業オフィス）
    "calendar": "enterprise_office",
    "schedule": "enterprise_office",
    "meeting": "enterprise_office",
    "productivity": "enterprise_office",
    "analytics": "enterprise_office",
    "business-intelligence": "enterprise_office",
    # ad_marketing（広告・マーケティング）
    "ecommerce": "ad_marketing",
    "cross-border": "ad_marketing",
    # enterprise_workflow（企業ワークフロー）
    "payment": "enterprise_workflow",
    "billing": "enterprise_workflow",
    "subscription": "enterprise_workflow",
    # ai_assistant（AIアシスタント）
    "conversation": "ai_assistant",
    "dialogue": "ai_assistant",
    "assistant": "ai_assistant",
    "voice": "ai_assistant",
    "speech": "ai_assistant",
    "audio": "ai_assistant",
    "tts": "ai_assistant",
    "stt": "ai_assistant",
    # media_creative（メディア・クリエイティブ）
    "design": "media_creative",
    "image-generation": "media_creative",
    "comfyui": "media_creative",
    "creative": "media_creative",
    "vision": "media_creative",
    "image": "media_creative",
    "ocr": "media_creative",
    "multimodal": "media_creative",
}


def infer_category(tags: list[str], name: str) -> str:
    """タグとスキル名からカテゴリを推論する.

    Args:
        tags: スキルのタグ一覧
        name: スキル名

    Returns:
        推論されたカテゴリ ID
    """
    # タグからの推論（出現頻度が高いカテゴリを採用）
    category_votes: dict[str, int] = {}
    for tag in tags:
        tag_lower = tag.lower().strip()
        cat = _CATEGORY_INFERENCE_MAP.get(tag_lower)
        if cat:
            category_votes[cat] = category_votes.get(cat, 0) + 1

    if category_votes:
        return max(category_votes, key=lambda k: category_votes[k])

    # 名前ベースのフォールバック
    name_lower = name.lower()
    for keyword, cat in _CATEGORY_INFERENCE_MAP.items():
        if keyword in name_lower:
            return cat

    return "common"


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
        "author",
        "category",
        "description",
        "examples",
        "label",
        "name",
        "path",
        "requirements",
        "tags",
        "tags_legacy",
        "triggers",
        "version",
    )

    def __init__(
        self,
        name: str,
        label: str | None = None,
        description: str = "",
        version: str = "1.0.0",
        author: str = "",
        tags: list[str] | None = None,
        tags_legacy: list[str] | None = None,
        triggers: list[str] | None = None,
        requirements: list[str] | None = None,
        examples: list[str] | None = None,
        path: str = "",
        category: str = "",
    ) -> None:
        """初期化."""
        self.name = name
        self.label = (label or name).strip() or name
        self.description = description
        self.version = version
        self.author = author
        self.tags = tags or []
        self.tags_legacy = tags_legacy or []
        self.triggers = triggers or []
        self.requirements = requirements or []
        self.examples = examples or []
        self.path = path
        self.category = category

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "label": self.label,
            "description": self.description,
            "version": self.version,
            "author": self.author,
            "tags": self.tags,
            "tags_legacy": self.tags_legacy,
            "triggers": self.triggers,
            "requirements": self.requirements,
            "examples": self.examples,
            "path": self.path,
            "category": self.category,
        }


class SkillCatalogService:
    """ビルトインスキルカタログサービス.

    Attributes:
        _skills_dir: スキルディレクトリの絶対パス
        _skills: スキル名 → SkillInfo のマッピング
    """

    def __init__(
        self,
        skills_dir: Path | None = None,
        skills_dirs: list[Path] | None = None,
    ) -> None:
        """初期化.

        Args:
            skills_dir: スキルディレクトリの絶対パス。
                        省略時はカレントディレクトリ配下のデフォルトパスを使用。
            skills_dirs: スキルディレクトリの一覧。指定時は skills_dir より優先。
        """
        resolved_dirs = skills_dirs or ([skills_dir] if skills_dir is not None else self._default_skill_dirs())
        self._skills_dirs = [path.resolve() for path in resolved_dirs]
        self._skills: dict[str, SkillInfo] = {}
        self._capability_registry = CapabilityRegistry()

    async def scan(self) -> int:
        """スキルディレクトリをスキャンし、SKILL.md を解析.

        Returns:
            検出されたスキル数
        """
        self._skills.clear()

        scanned_any = False
        for skills_dir in self._skills_dirs:
            if not skills_dir.is_dir():
                continue
            scanned_any = True
            for skill_md in sorted(skills_dir.glob("*/SKILL.md")):
                self._parse_skill_md(skill_md)

        if not scanned_any:
            _logger.warning("スキルディレクトリが存在しません: %s", self._skills_dirs)
            return 0

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
            s
            for s in self.list_skills()
            if any(tag_lower in t.lower() for t in s.tags) or any(tag_lower in t.lower() for t in s.tags_legacy)
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
    # カテゴリ関連メソッド
    # ------------------------------------------------------------------

    def get_categories(self) -> list[dict[str, Any]]:
        """利用可能なカテゴリ一覧を取得.

        Returns:
            カテゴリ情報のリスト（表示順ソート済み）
        """
        # 各カテゴリに属するスキル数をカウント
        cat_counts: dict[str, int] = {}
        for skill in self._skills.values():
            cat_counts[skill.category] = cat_counts.get(skill.category, 0) + 1

        result = []
        for cat_id, meta in SKILL_CATEGORIES.items():
            result.append(
                {
                    "id": cat_id,
                    "icon": meta["icon"],
                    "order": meta["order"],
                    "skill_count": cat_counts.get(cat_id, 0),
                }
            )
        return sorted(result, key=lambda x: x["order"])

    def get_skills_by_category(self, category: str) -> list[SkillInfo]:
        """指定カテゴリのスキルを取得.

        Args:
            category: カテゴリ ID

        Returns:
            マッチした SkillInfo のリスト（名前順）
        """
        return sorted(
            [s for s in self._skills.values() if s.category == category],
            key=lambda s: s.name,
        )

    def get_skills_grouped_by_category(self) -> list[dict[str, Any]]:
        """全スキルをカテゴリ別にグループ化して取得.

        Returns:
            カテゴリ情報とスキルリストを含む辞書のリスト（表示順ソート済み）
        """
        grouped: dict[str, list[SkillInfo]] = {}
        for skill in self._skills.values():
            grouped.setdefault(skill.category, []).append(skill)

        result = []
        for cat_id, meta in sorted(SKILL_CATEGORIES.items(), key=lambda x: x[1]["order"]):
            skills = sorted(grouped.get(cat_id, []), key=lambda s: s.name)
            if not skills:
                continue
            result.append(
                {
                    "id": cat_id,
                    "icon": meta["icon"],
                    "order": meta["order"],
                    "skills": [s.to_dict() for s in skills],
                }
            )
        return result

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
            raw_tags = self._ensure_list(data.get("tags"))
            canonical_tags = self._capability_registry.canonicalize_many(raw_tags)
            # カテゴリ: frontmatter に明示指定があればそれを使い、なければ推論
            raw_category = str(data.get("category", "")).strip()
            if raw_category and raw_category in SKILL_CATEGORIES:
                category = raw_category
            else:
                category = infer_category(raw_tags, name)

            skill = SkillInfo(
                name=name,
                label=str(data.get("label", name)),
                description=str(data.get("description", "")).strip(),
                version=str(data.get("version", "1.0.0")),
                author=str(data.get("author", "")),
                tags=[tag.id for tag in canonical_tags],
                tags_legacy=raw_tags,
                triggers=self._ensure_list(data.get("triggers")),
                requirements=self._ensure_list(data.get("requirements")),
                examples=self._ensure_list(data.get("examples")),
                path=str(
                    skill_md_path.relative_to(Path.cwd()) if skill_md_path.is_relative_to(Path.cwd()) else skill_md_path
                ),
                category=category,
            )
            self._skills[name] = skill
        except Exception as exc:
            _logger.warning("SKILL.md パースエラー (%s): %s", skill_md_path, exc)

    @staticmethod
    def _ensure_list(value: Any) -> list[str]:
        """値をリストに変換."""
        if value is None:
            return []
        if isinstance(value, list):
            return [str(v) for v in value]
        return [str(value)]

    @staticmethod
    def _default_skill_dirs() -> list[Path]:
        """既定のスキル検索パスを返す."""
        cwd = Path.cwd()
        home = Path.home()
        return [
            cwd / _DEFAULT_SKILLS_DIR,
            home / _PRIMARY_CONFIG_DIR_NAME / "skills",
            cwd / _PRIMARY_CONFIG_DIR_NAME / "skills",
        ]
