"""Skill 検証器 - 生成された Skill の品質検証.

このモジュールは Skill の品質と安全性を検証する機能を提供します：
- 必須フィールドの確認
- フォーマット検証
- 依存パッケージの確認
- セキュリティチェック
"""

import logging
import re
from dataclasses import dataclass, field

from agentflow.skills.core.base import Skill


@dataclass
class ValidationResult:
    """検証結果.

    Attributes:
        valid: 検証に合格したか
        errors: エラーリスト（致命的）
        warnings: 警告リスト（推奨）
    """

    valid: bool
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)

    def add_error(self, msg: str) -> None:
        """エラーを追加."""
        self.errors.append(msg)
        self.valid = False

    def add_warning(self, msg: str) -> None:
        """警告を追加."""
        self.warnings.append(msg)


class SkillValidator:
    """Skill 検証器 - 品質と安全性のチェック.

    Example:
        >>> validator = SkillValidator()
        >>> result = validator.validate(skill)
        >>> if result.valid:
        ...     print("Skill is valid!")
        >>> else:
        ...     for error in result.errors:
        ...         print(f"Error: {error}")
    """

    # 禁止パターン（セキュリティリスク）
    FORBIDDEN_PATTERNS = [
        r"rm\s+-rf\s+/",  # 危険な削除コマンド
        r"eval\s*\(",  # eval 関数
        r"exec\s*\(",  # exec 関数
        r"__import__",  # 動的インポート
        r"subprocess\.call.*shell\s*=\s*True",  # シェルインジェクション
    ]

    # 必須フィールド
    REQUIRED_FIELDS = ["name", "description"]

    def __init__(self, strict: bool = False) -> None:
        """初期化.

        Args:
            strict: 厳格モード（警告もエラーとして扱う）
        """
        self._strict = strict
        self._logger = logging.getLogger(__name__)

    def validate(self, skill: Skill) -> ValidationResult:
        """Skill を検証.

        Args:
            skill: 検証対象の Skill

        Returns:
            検証結果
        """
        result = ValidationResult(valid=True)

        # 必須フィールドチェック
        self._check_required_fields(skill, result)

        # フォーマットチェック
        self._check_format(skill, result)

        # セキュリティチェック
        self._check_security(skill, result)

        # 品質チェック
        self._check_quality(skill, result)

        # 厳格モードでは警告もエラーに
        if self._strict and result.warnings:
            result.valid = False
            result.errors.extend(result.warnings)

        return result

    def _check_required_fields(self, skill: Skill, result: ValidationResult) -> None:
        """必須フィールドをチェック."""
        meta = skill.metadata

        if not meta.name or meta.name == "unknown":
            result.add_error("Missing required field: name")

        if not meta.description:
            result.add_error("Missing required field: description")

        if not skill.instructions.strip():
            result.add_error("Missing instructions content")

    def _check_format(self, skill: Skill, result: ValidationResult) -> None:
        """フォーマットをチェック."""
        meta = skill.metadata

        # name は kebab-case であるべき
        if not re.match(r"^[a-z0-9]+(-[a-z0-9]+)*$", meta.name):
            result.add_warning(f"Name '{meta.name}' should be kebab-case")

        # version は semver 形式であるべき
        if not re.match(r"^\d+\.\d+\.\d+", meta.version):
            result.add_warning(f"Version '{meta.version}' should be semver format")

        # description は十分な長さであるべき
        if len(meta.description) < 20:
            result.add_warning("Description is too short (< 20 chars)")

    def _check_security(self, skill: Skill, result: ValidationResult) -> None:
        """セキュリティをチェック."""
        content = skill.instructions

        for pattern in self.FORBIDDEN_PATTERNS:
            if re.search(pattern, content, re.IGNORECASE):
                result.add_error(f"Security risk detected: pattern '{pattern}'")

    def _check_quality(self, skill: Skill, result: ValidationResult) -> None:
        """品質をチェック."""
        meta = skill.metadata

        # triggers が空の場合は警告
        if not meta.triggers:
            result.add_warning("No triggers defined (may reduce discoverability)")

        # 自動学習 Skill で信頼度が低い場合は警告
        if meta.learned and meta.confidence < 0.5:
            result.add_warning(f"Low confidence ({meta.confidence:.2f}) for learned skill")

    def is_valid(self, skill: Skill) -> bool:
        """Skill が有効かどうかを簡易チェック.

        Args:
            skill: チェック対象

        Returns:
            有効かどうか
        """
        return self.validate(skill).valid
