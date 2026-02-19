"""Version Migration - 版本迁移.

技能版本间的迁移和回滚。

使用例:
    >>> migration = VersionMigration(store)
    >>> result = await migration.migrate("my-skill", "1.0.0", "2.0.0")
    >>> if not result.success:
    ...     await migration.rollback("my-skill", "1.0.0")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow.skills.versioning.diff_engine import DiffEngine, DiffResult
from agentflow.skills.versioning.skill_version import (
    SkillSnapshot,
    SkillVersion,
)


if TYPE_CHECKING:
    from collections.abc import Callable, Coroutine

    from agentflow.skills.versioning.version_store import VersionStore


_logger = logging.getLogger(__name__)


class MigrationStatus(str, Enum):
    """迁移状态."""

    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    ROLLED_BACK = "rolled_back"


@dataclass
class MigrationStep:
    """迁移步骤.

    Attributes:
        name: 步骤名称
        description: 描述
        action: 执行函数
        rollback_action: 回滚函数
        required: 是否必需
    """

    name: str
    description: str = ""
    action: Callable[..., Coroutine[Any, Any, Any]] | None = None
    rollback_action: Callable[..., Coroutine[Any, Any, Any]] | None = None
    required: bool = True

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "name": self.name,
            "description": self.description,
            "required": self.required,
        }


@dataclass
class StepResult:
    """步骤结果.

    Attributes:
        step_name: 步骤名称
        success: 是否成功
        error: 错误信息
        duration_ms: 执行时间
    """

    step_name: str
    success: bool = True
    error: str = ""
    duration_ms: float = 0.0

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "step_name": self.step_name,
            "success": self.success,
            "error": self.error,
            "duration_ms": self.duration_ms,
        }


@dataclass
class MigrationResult:
    """迁移结果.

    Attributes:
        skill_name: 技能名称
        from_version: 源版本
        to_version: 目标版本
        status: 状态
        steps: 步骤结果
        diff: 差异信息
        started_at: 开始时间
        completed_at: 完成时间
        error: 错误信息
    """

    skill_name: str
    from_version: str
    to_version: str
    status: MigrationStatus = MigrationStatus.PENDING
    steps: list[StepResult] = field(default_factory=list)
    diff: DiffResult | None = None
    started_at: datetime | None = None
    completed_at: datetime | None = None
    error: str = ""

    @property
    def success(self) -> bool:
        """是否成功."""
        return self.status == MigrationStatus.COMPLETED

    @property
    def duration_ms(self) -> float:
        """总执行时间."""
        if self.started_at and self.completed_at:
            return (self.completed_at - self.started_at).total_seconds() * 1000
        return 0.0

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "skill_name": self.skill_name,
            "from_version": self.from_version,
            "to_version": self.to_version,
            "status": self.status.value,
            "success": self.success,
            "steps": [s.to_dict() for s in self.steps],
            "diff": self.diff.to_dict() if self.diff else None,
            "started_at": self.started_at.isoformat() if self.started_at else None,
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "duration_ms": self.duration_ms,
            "error": self.error,
        }


class VersionMigration:
    """版本迁移管理器.

    管理技能版本间的迁移和回滚。
    """

    def __init__(
        self,
        store: VersionStore,
        diff_engine: DiffEngine | None = None,
    ) -> None:
        """初始化.

        Args:
            store: 版本存储
            diff_engine: 差异引擎
        """
        self._store = store
        self._diff_engine = diff_engine or DiffEngine()
        self._migration_steps: list[MigrationStep] = []
        self._history: list[MigrationResult] = []

    def add_step(self, step: MigrationStep) -> None:
        """添加迁移步骤.

        Args:
            step: 迁移步骤
        """
        self._migration_steps.append(step)

    def clear_steps(self) -> None:
        """清除迁移步骤."""
        self._migration_steps.clear()

    async def migrate(
        self,
        skill_name: str,
        from_version: str,
        to_version: str,
        dry_run: bool = False,
    ) -> MigrationResult:
        """执行迁移.

        Args:
            skill_name: 技能名称
            from_version: 源版本
            to_version: 目标版本
            dry_run: 是否仅模拟

        Returns:
            迁移结果
        """
        result = MigrationResult(
            skill_name=skill_name,
            from_version=from_version,
            to_version=to_version,
            started_at=datetime.now(UTC),
        )

        try:
            # 加载版本
            old_snapshot = self._store.load(skill_name, from_version)
            new_snapshot = self._store.load(skill_name, to_version)

            if old_snapshot is None:
                result.status = MigrationStatus.FAILED
                result.error = f"Source version not found: {from_version}"
                return result

            if new_snapshot is None:
                result.status = MigrationStatus.FAILED
                result.error = f"Target version not found: {to_version}"
                return result

            # 计算差异
            result.diff = self._diff_engine.diff(
                old_snapshot.content,
                new_snapshot.content,
            )

            if dry_run:
                result.status = MigrationStatus.COMPLETED
                result.completed_at = datetime.now(UTC)
                _logger.info(f"Dry run migration {skill_name}: {from_version} -> {to_version}")
                return result

            # 执行迁移步骤
            result.status = MigrationStatus.IN_PROGRESS
            completed_steps: list[MigrationStep] = []

            for step in self._migration_steps:
                import time

                start = time.time()

                try:
                    if step.action:
                        await step.action(
                            skill_name=skill_name,
                            old_snapshot=old_snapshot,
                            new_snapshot=new_snapshot,
                        )

                    step_result = StepResult(
                        step_name=step.name,
                        success=True,
                        duration_ms=(time.time() - start) * 1000,
                    )
                    result.steps.append(step_result)
                    completed_steps.append(step)

                except Exception as e:
                    step_result = StepResult(
                        step_name=step.name,
                        success=False,
                        error=str(e),
                        duration_ms=(time.time() - start) * 1000,
                    )
                    result.steps.append(step_result)

                    if step.required:
                        # 回滚已完成的步骤
                        await self._rollback_steps(
                            completed_steps,
                            skill_name,
                            old_snapshot,
                            new_snapshot,
                        )
                        result.status = MigrationStatus.FAILED
                        result.error = f"Step '{step.name}' failed: {e}"
                        result.completed_at = datetime.now(UTC)
                        self._history.append(result)
                        return result

            result.status = MigrationStatus.COMPLETED
            result.completed_at = datetime.now(UTC)
            self._history.append(result)

            _logger.info(
                f"Migration completed {skill_name}: {from_version} -> {to_version} "
                f"({result.duration_ms:.1f}ms)"
            )

        except Exception as e:
            result.status = MigrationStatus.FAILED
            result.error = str(e)
            result.completed_at = datetime.now(UTC)
            self._history.append(result)
            _logger.exception(f"Migration failed: {e}")

        return result

    async def _rollback_steps(
        self,
        steps: list[MigrationStep],
        skill_name: str,
        old_snapshot: SkillSnapshot,
        new_snapshot: SkillSnapshot,
    ) -> None:
        """回滚步骤.

        Args:
            steps: 已完成的步骤
            skill_name: 技能名称
            old_snapshot: 旧快照
            new_snapshot: 新快照
        """
        # 反序回滚
        for step in reversed(steps):
            if step.rollback_action:
                try:
                    await step.rollback_action(
                        skill_name=skill_name,
                        old_snapshot=old_snapshot,
                        new_snapshot=new_snapshot,
                    )
                    _logger.debug(f"Rolled back step: {step.name}")
                except Exception as e:
                    _logger.warning(f"Failed to rollback step '{step.name}': {e}")

    async def rollback(
        self,
        skill_name: str,
        to_version: str,
    ) -> MigrationResult:
        """回滚到指定版本.

        Args:
            skill_name: 技能名称
            to_version: 目标版本

        Returns:
            迁移结果
        """
        # 获取当前版本
        current = self._store.get_latest(skill_name)
        if current is None:
            return MigrationResult(
                skill_name=skill_name,
                from_version="unknown",
                to_version=to_version,
                status=MigrationStatus.FAILED,
                error="No current version found",
            )

        from_version = str(current.version)

        # 执行迁移（回滚本质上是迁移到旧版本）
        result = await self.migrate(skill_name, from_version, to_version)
        if result.success:
            result.status = MigrationStatus.ROLLED_BACK
        return result

    async def upgrade_path(
        self,
        skill_name: str,
        from_version: str,
        to_version: str,
    ) -> list[str]:
        """计算升级路径.

        Args:
            skill_name: 技能名称
            from_version: 源版本
            to_version: 目标版本

        Returns:
            版本升级路径
        """
        versions = self._store.list_versions(skill_name)
        if not versions:
            return []

        # 构建版本图
        version_list = sorted([SkillVersion.parse(v.version) for v in versions])

        from_v = SkillVersion.parse(from_version)
        to_v = SkillVersion.parse(to_version)

        # 简单实现：直接返回中间版本
        path = []
        in_range = False

        for v in version_list:
            if v == from_v:
                in_range = True
            if in_range:
                path.append(str(v))
            if v == to_v:
                break

        return path

    async def can_migrate(
        self,
        skill_name: str,
        from_version: str,
        to_version: str,
    ) -> tuple[bool, str]:
        """检查是否可以迁移.

        Args:
            skill_name: 技能名称
            from_version: 源版本
            to_version: 目标版本

        Returns:
            (是否可以, 原因)
        """
        # 检查版本存在
        if not self._store.exists(skill_name, from_version):
            return False, f"Source version not found: {from_version}"

        if not self._store.exists(skill_name, to_version):
            return False, f"Target version not found: {to_version}"

        # 检查版本兼容性
        from_v = SkillVersion.parse(from_version)
        to_v = SkillVersion.parse(to_version)

        if not from_v.is_compatible_with(to_v):
            return False, "Versions are not compatible (major version mismatch)"

        return True, "Migration is possible"

    def get_history(
        self,
        skill_name: str | None = None,
    ) -> list[MigrationResult]:
        """获取迁移历史.

        Args:
            skill_name: 技能名称（可选，None表示全部）

        Returns:
            迁移结果列表
        """
        if skill_name is None:
            return list(self._history)
        return [r for r in self._history if r.skill_name == skill_name]

    def clear_history(self) -> None:
        """清除迁移历史."""
        self._history.clear()


__all__ = [
    "MigrationResult",
    "MigrationStatus",
    "MigrationStep",
    "StepResult",
    "VersionMigration",
]
