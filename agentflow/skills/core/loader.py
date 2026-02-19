"""Skill ローダーとレジストリ.

このモジュールは Skill の自動検出とレジストリ管理を提供します：
- ディレクトリスキャン
- 自動登録
- 依存解決
"""

import logging
from pathlib import Path

from agentflow.core.registry import Registry
from agentflow.skills.core.base import Skill


class SkillRegistry(Registry[Skill]):
    """Skill レジストリ - Skill の一元管理.

    Example:
        >>> registry = SkillRegistry()
        >>> skill = Skill.load(Path("./my-skill"))
        >>> registry.register(skill.name, skill)
        >>> loaded = registry.get("my-skill")
    """

    _instance: "SkillRegistry | None" = None

    def __new__(cls) -> "SkillRegistry":
        """シングルトンパターン."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def register_skill(self, skill: Skill) -> None:
        """Skill を名前で自動登録.

        Args:
            skill: 登録する Skill
        """
        self.register(skill.name, skill)

    def get_by_tag(self, tag: str) -> list[Skill]:
        """タグで Skill を検索.

        Args:
            tag: 検索タグ

        Returns:
            マッチした Skill リスト
        """
        return [skill for skill in self._items.values() if tag in skill.metadata.tags]


class SkillLoader:
    """Skill ローダー - ディレクトリから Skill を自動検出.

    Example:
        >>> loader = SkillLoader()
        >>> skills = loader.load_directory(Path("./skills"))
        >>> print(f"Loaded {len(skills)} skills")
    """

    def __init__(self, registry: SkillRegistry | None = None) -> None:
        """初期化.

        Args:
            registry: 登録先レジストリ（省略時は新規作成）
        """
        self._registry = registry or SkillRegistry()
        self._logger = logging.getLogger(__name__)

    @property
    def registry(self) -> SkillRegistry:
        """レジストリを取得."""
        return self._registry

    def load_skill(self, skill_path: Path) -> Skill | None:
        """単一の Skill を読み込み.

        Args:
            skill_path: Skill パス

        Returns:
            Skill インスタンス、失敗時 None
        """
        try:
            skill = Skill.load(skill_path)
            self._registry.register_skill(skill)
            self._logger.info(f"Loaded skill: {skill.name}")
            return skill
        except (FileNotFoundError, ValueError) as e:
            self._logger.warning(f"Failed to load skill from {skill_path}: {e}")
            return None

    def load_directory(self, directory: Path, recursive: bool = False) -> list[Skill]:
        """ディレクトリから Skill を一括読み込み.

        Args:
            directory: スキャン対象ディレクトリ
            recursive: サブディレクトリも検索するか

        Returns:
            読み込まれた Skill リスト
        """
        skills: list[Skill] = []

        if not directory.exists():
            self._logger.warning(f"Directory not found: {directory}")
            return skills

        # SKILL.md を検索
        pattern = "**" if recursive else "*"
        for skill_file in directory.glob(f"{pattern}/SKILL.md"):
            skill = self.load_skill(skill_file.parent)
            if skill:
                skills.append(skill)

        self._logger.info(f"Loaded {len(skills)} skills from {directory}")
        return skills

    def load_default_paths(self) -> list[Skill]:
        """Claude Code CLI 互換パスからスキルを自動読み込み.

        Scan order:
        1. ~/.claude/skills/ (user-level)
        2. .claude/skills/  (project-level, relative to cwd)
        3. agentflow/skills/builtin/ (framework builtin)

        Returns:
            読み込まれた Skill リスト
        """
        skills: list[Skill] = []
        scan_dirs = [
            Path.home() / ".claude" / "skills",
            Path.cwd() / ".claude" / "skills",
            Path(__file__).parent.parent / "builtin",
        ]
        for d in scan_dirs:
            if d.exists():
                skills.extend(self.load_directory(d, recursive=False))
        return skills

    def resolve_dependencies(self, skill: Skill) -> list[Skill]:
        """Skill の依存関係を解決.

        Args:
            skill: 解決対象の Skill

        Returns:
            依存 Skill リスト（依存順）

        Raises:
            KeyError: 依存 Skill が見つからない場合
        """
        resolved: list[Skill] = []
        visited: set[str] = set()

        def _resolve(s: Skill) -> None:
            if s.name in visited:
                return
            visited.add(s.name)

            for dep_name in s.metadata.dependencies:
                dep = self._registry.get(dep_name)
                if dep is None:
                    msg = f"Dependency not found: {dep_name}"
                    raise KeyError(msg)
                _resolve(dep)

            resolved.append(s)

        _resolve(skill)
        return resolved
