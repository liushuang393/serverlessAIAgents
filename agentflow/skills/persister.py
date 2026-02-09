"""Skill 固化器 - 学習した Skill の永続化.

このモジュールは生成された Skill をファイルシステムに保存する機能を提供します：
- learned skills ディレクトリへの保存
- バージョン管理
- バックアップ
"""

import logging
import shutil
from datetime import datetime
from pathlib import Path

from agentflow.skills.base import Skill
from agentflow.skills.validator import SkillValidator


class SkillPersister:
    """Skill 固化器 - 学習 Skill の永続化.

    Example:
        >>> persister = SkillPersister()
        >>> path = persister.save(skill)
        >>> print(f"Saved to: {path}")
    """

    # デフォルト保存先
    DEFAULT_LEARNED_DIR = Path.home() / ".agentflow" / "learned_skills"
    DEFAULT_PROJECT_DIR = Path(".agentflow") / "skills"

    def __init__(
        self,
        learned_dir: Path | None = None,
        project_dir: Path | None = None,
        validator: SkillValidator | None = None,
        validate_before_save: bool = True,
    ) -> None:
        """初期化.

        Args:
            learned_dir: グローバル学習 Skill ディレクトリ
            project_dir: プロジェクト Skill ディレクトリ
            validator: 検証器
            validate_before_save: 保存前に検証するか
        """
        self._learned_dir = learned_dir or self.DEFAULT_LEARNED_DIR
        self._project_dir = project_dir or self.DEFAULT_PROJECT_DIR
        self._validator = validator or SkillValidator()
        self._validate_before_save = validate_before_save
        self._logger = logging.getLogger(__name__)

        # ディレクトリ作成
        self._learned_dir.mkdir(parents=True, exist_ok=True)

    def save(
        self,
        skill: Skill,
        *,
        scope: str = "learned",
        force: bool = False,
    ) -> Path:
        """Skill を保存.

        Args:
            skill: 保存する Skill
            scope: 保存先スコープ ("learned" | "project")
            force: 検証エラーを無視するか

        Returns:
            保存先パス

        Raises:
            ValueError: 検証失敗時（force=False の場合）
            FileExistsError: 同名 Skill が存在する場合
        """
        # 検証
        if self._validate_before_save and not force:
            result = self._validator.validate(skill)
            if not result.valid:
                errors = "; ".join(result.errors)
                msg = f"Skill validation failed: {errors}"
                raise ValueError(msg)

        # 保存先決定
        base_dir = self._learned_dir if scope == "learned" else self._project_dir
        save_path = base_dir / skill.name

        # 既存チェック
        if save_path.exists():
            if force:
                self._backup(save_path)
            else:
                msg = f"Skill already exists: {save_path}"
                raise FileExistsError(msg)

        # 保存
        skill.save(save_path)
        self._logger.info(f"Saved skill '{skill.name}' to {save_path}")
        return save_path

    def _backup(self, path: Path) -> Path:
        """既存 Skill をバックアップ.

        Args:
            path: バックアップ対象パス

        Returns:
            バックアップ先パス
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_path = path.with_name(f"{path.name}.backup_{timestamp}")
        shutil.copytree(path, backup_path)
        self._logger.info(f"Backed up to {backup_path}")
        return backup_path

    def delete(self, name: str, scope: str = "learned") -> bool:
        """Skill を削除.

        Args:
            name: Skill 名
            scope: スコープ

        Returns:
            削除成功したか
        """
        base_dir = self._learned_dir if scope == "learned" else self._project_dir
        skill_path = base_dir / name

        if skill_path.exists():
            shutil.rmtree(skill_path)
            self._logger.info(f"Deleted skill: {name}")
            return True
        return False

    def list_learned(self) -> list[str]:
        """学習済み Skill 名を取得.

        Returns:
            Skill 名リスト
        """
        if not self._learned_dir.exists():
            return []
        return [d.name for d in self._learned_dir.iterdir() if d.is_dir()]

    @property
    def learned_dir(self) -> Path:
        """学習 Skill ディレクトリ."""
        return self._learned_dir

    @property
    def project_dir(self) -> Path:
        """プロジェクト Skill ディレクトリ."""
        return self._project_dir

