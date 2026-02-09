"""Skill Version - 技能版本模型.

技能版本的数据模型和版本号管理。

使用例:
    >>> version = SkillVersion.parse("1.2.3")
    >>> next_version = version.bump_minor()
    >>> print(next_version)  # 1.3.0
"""

from __future__ import annotations

import hashlib
import re
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any
from uuid import uuid4


class VersionStatus(str, Enum):
    """版本状态."""

    DRAFT = "draft"  # 草稿
    PUBLISHED = "published"  # 已发布
    DEPRECATED = "deprecated"  # 已弃用
    ARCHIVED = "archived"  # 已归档


@dataclass
class VersionInfo:
    """版本信息.

    Attributes:
        version: 版本号
        created_at: 创建时间
        created_by: 创建者
        message: 版本说明
        parent_version: 父版本
        tags: 标签
        metadata: 元数据
    """

    version: str
    created_at: datetime = field(default_factory=lambda: datetime.now(UTC))
    created_by: str = ""
    message: str = ""
    parent_version: str | None = None
    tags: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "version": self.version,
            "created_at": self.created_at.isoformat(),
            "created_by": self.created_by,
            "message": self.message,
            "parent_version": self.parent_version,
            "tags": self.tags,
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> VersionInfo:
        """从字典创建."""
        return cls(
            version=data["version"],
            created_at=datetime.fromisoformat(data["created_at"]) if "created_at" in data else datetime.now(UTC),
            created_by=data.get("created_by", ""),
            message=data.get("message", ""),
            parent_version=data.get("parent_version"),
            tags=data.get("tags", []),
            metadata=data.get("metadata", {}),
        )


@dataclass
class SkillVersion:
    """技能版本.

    支持语义化版本号 (major.minor.patch)。

    Attributes:
        major: 主版本号
        minor: 次版本号
        patch: 补丁版本号
        prerelease: 预发布标识
        build: 构建元数据
    """

    major: int = 0
    minor: int = 0
    patch: int = 0
    prerelease: str = ""
    build: str = ""

    VERSION_PATTERN = re.compile(
        r"^(?P<major>\d+)\.(?P<minor>\d+)\.(?P<patch>\d+)"
        r"(?:-(?P<prerelease>[0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?"
        r"(?:\+(?P<build>[0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*))?$"
    )

    def __str__(self) -> str:
        """转换为字符串."""
        version = f"{self.major}.{self.minor}.{self.patch}"
        if self.prerelease:
            version += f"-{self.prerelease}"
        if self.build:
            version += f"+{self.build}"
        return version

    def __eq__(self, other: object) -> bool:
        """比较相等."""
        if not isinstance(other, SkillVersion):
            return False
        return (
            self.major == other.major
            and self.minor == other.minor
            and self.patch == other.patch
            and self.prerelease == other.prerelease
        )

    def __lt__(self, other: SkillVersion) -> bool:
        """比较大小."""
        if self.major != other.major:
            return self.major < other.major
        if self.minor != other.minor:
            return self.minor < other.minor
        if self.patch != other.patch:
            return self.patch < other.patch
        # 预发布版本低于正式版本
        if self.prerelease and not other.prerelease:
            return True
        if not self.prerelease and other.prerelease:
            return False
        return self.prerelease < other.prerelease

    def __le__(self, other: SkillVersion) -> bool:
        return self == other or self < other

    def __gt__(self, other: SkillVersion) -> bool:
        return not self <= other

    def __ge__(self, other: SkillVersion) -> bool:
        return not self < other

    def __hash__(self) -> int:
        return hash((self.major, self.minor, self.patch, self.prerelease))

    @classmethod
    def parse(cls, version_string: str) -> SkillVersion:
        """解析版本字符串.

        Args:
            version_string: 版本字符串

        Returns:
            SkillVersion 对象

        Raises:
            ValueError: 无效的版本格式
        """
        match = cls.VERSION_PATTERN.match(version_string)
        if not match:
            msg = f"Invalid version format: {version_string}"
            raise ValueError(msg)

        return cls(
            major=int(match.group("major")),
            minor=int(match.group("minor")),
            patch=int(match.group("patch")),
            prerelease=match.group("prerelease") or "",
            build=match.group("build") or "",
        )

    @classmethod
    def initial(cls) -> SkillVersion:
        """创建初始版本 (0.1.0)."""
        return cls(major=0, minor=1, patch=0)

    def bump_major(self) -> SkillVersion:
        """增加主版本号."""
        return SkillVersion(major=self.major + 1, minor=0, patch=0)

    def bump_minor(self) -> SkillVersion:
        """增加次版本号."""
        return SkillVersion(major=self.major, minor=self.minor + 1, patch=0)

    def bump_patch(self) -> SkillVersion:
        """增加补丁版本号."""
        return SkillVersion(major=self.major, minor=self.minor, patch=self.patch + 1)

    def with_prerelease(self, prerelease: str) -> SkillVersion:
        """设置预发布标识."""
        return SkillVersion(
            major=self.major,
            minor=self.minor,
            patch=self.patch,
            prerelease=prerelease,
            build=self.build,
        )

    def with_build(self, build: str) -> SkillVersion:
        """设置构建元数据."""
        return SkillVersion(
            major=self.major,
            minor=self.minor,
            patch=self.patch,
            prerelease=self.prerelease,
            build=build,
        )

    def to_tuple(self) -> tuple[int, int, int]:
        """转换为元组."""
        return (self.major, self.minor, self.patch)

    def is_compatible_with(self, other: SkillVersion) -> bool:
        """检查是否兼容.

        主版本号相同则兼容。
        """
        return self.major == other.major


@dataclass
class SkillSnapshot:
    """技能快照.

    Attributes:
        id: 快照ID
        skill_name: 技能名称
        version: 版本
        content: 技能内容
        content_hash: 内容哈希
        status: 状态
        info: 版本信息
    """

    skill_name: str
    version: SkillVersion
    content: str
    content_hash: str = ""
    status: VersionStatus = VersionStatus.DRAFT
    info: VersionInfo | None = None
    id: str = field(default_factory=lambda: str(uuid4()))

    def __post_init__(self) -> None:
        """初始化后处理."""
        if not self.content_hash:
            self.content_hash = self._compute_hash()
        if self.info is None:
            self.info = VersionInfo(version=str(self.version))

    def _compute_hash(self) -> str:
        """计算内容哈希."""
        return hashlib.sha256(self.content.encode()).hexdigest()[:12]

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "id": self.id,
            "skill_name": self.skill_name,
            "version": str(self.version),
            "content": self.content,
            "content_hash": self.content_hash,
            "status": self.status.value,
            "info": self.info.to_dict() if self.info else None,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> SkillSnapshot:
        """从字典创建."""
        return cls(
            id=data.get("id", str(uuid4())),
            skill_name=data["skill_name"],
            version=SkillVersion.parse(data["version"]),
            content=data["content"],
            content_hash=data.get("content_hash", ""),
            status=VersionStatus(data.get("status", "draft")),
            info=VersionInfo.from_dict(data["info"]) if data.get("info") else None,
        )


__all__ = [
    "SkillSnapshot",
    "SkillVersion",
    "VersionInfo",
    "VersionStatus",
]
