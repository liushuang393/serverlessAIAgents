"""Version Store - 版本存储.

Git风格的版本存储实现。

使用例:
    >>> store = FileVersionStore(Path("./versions"))
    >>> store.save(snapshot)
    >>> versions = store.list_versions("my-skill")
    >>> old_snapshot = store.load("my-skill", "1.0.0")
"""

from __future__ import annotations

import json
import logging
from abc import ABC, abstractmethod
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from agentflow.skills.versioning.skill_version import (
    SkillSnapshot,
    SkillVersion,
    VersionInfo,
)


_logger = logging.getLogger(__name__)


class VersionStore(ABC):
    """版本存储接口."""

    @abstractmethod
    def save(self, snapshot: SkillSnapshot) -> None:
        """保存快照.

        Args:
            snapshot: 技能快照
        """

    @abstractmethod
    def load(self, skill_name: str, version: str) -> SkillSnapshot | None:
        """加载快照.

        Args:
            skill_name: 技能名称
            version: 版本号

        Returns:
            技能快照或None
        """

    @abstractmethod
    def list_versions(self, skill_name: str) -> list[VersionInfo]:
        """列出所有版本.

        Args:
            skill_name: 技能名称

        Returns:
            版本信息列表
        """

    @abstractmethod
    def get_latest(self, skill_name: str) -> SkillSnapshot | None:
        """获取最新版本.

        Args:
            skill_name: 技能名称

        Returns:
            最新版本快照或None
        """

    @abstractmethod
    def delete(self, skill_name: str, version: str) -> bool:
        """删除版本.

        Args:
            skill_name: 技能名称
            version: 版本号

        Returns:
            是否删除成功
        """

    @abstractmethod
    def exists(self, skill_name: str, version: str) -> bool:
        """检查版本是否存在.

        Args:
            skill_name: 技能名称
            version: 版本号

        Returns:
            是否存在
        """


class InMemoryVersionStore(VersionStore):
    """内存版本存储.

    适用于测试和临时存储。
    """

    def __init__(self) -> None:
        """初始化."""
        # skill_name -> version -> snapshot
        self._store: dict[str, dict[str, SkillSnapshot]] = {}

    def save(self, snapshot: SkillSnapshot) -> None:
        """保存快照."""
        if snapshot.skill_name not in self._store:
            self._store[snapshot.skill_name] = {}
        self._store[snapshot.skill_name][str(snapshot.version)] = snapshot
        _logger.debug(f"Saved {snapshot.skill_name}@{snapshot.version}")

    def load(self, skill_name: str, version: str) -> SkillSnapshot | None:
        """加载快照."""
        if skill_name not in self._store:
            return None
        return self._store[skill_name].get(version)

    def list_versions(self, skill_name: str) -> list[VersionInfo]:
        """列出所有版本."""
        if skill_name not in self._store:
            return []
        versions = []
        for snapshot in self._store[skill_name].values():
            if snapshot.info:
                versions.append(snapshot.info)
        # 按版本号排序
        versions.sort(key=lambda v: SkillVersion.parse(v.version), reverse=True)
        return versions

    def get_latest(self, skill_name: str) -> SkillSnapshot | None:
        """获取最新版本."""
        if skill_name not in self._store:
            return None
        if not self._store[skill_name]:
            return None
        # 找最新版本
        return max(
            self._store[skill_name].values(),
            key=lambda s: s.version,
        )

    def delete(self, skill_name: str, version: str) -> bool:
        """删除版本."""
        if skill_name not in self._store:
            return False
        if version not in self._store[skill_name]:
            return False
        del self._store[skill_name][version]
        _logger.debug(f"Deleted {skill_name}@{version}")
        return True

    def exists(self, skill_name: str, version: str) -> bool:
        """检查版本是否存在."""
        if skill_name not in self._store:
            return False
        return version in self._store[skill_name]

    def clear(self) -> None:
        """清空存储."""
        self._store.clear()


class FileVersionStore(VersionStore):
    """文件版本存储.

    将版本存储为JSON文件。

    目录结构:
        base_path/
            skill_name/
                versions.json     # 版本索引
                1.0.0.json       # 版本快照
                1.1.0.json
    """

    def __init__(self, base_path: Path | str) -> None:
        """初始化.

        Args:
            base_path: 基础路径
        """
        self._base_path = Path(base_path)
        self._base_path.mkdir(parents=True, exist_ok=True)

    def _skill_path(self, skill_name: str) -> Path:
        """获取技能目录路径."""
        return self._base_path / skill_name

    def _version_path(self, skill_name: str, version: str) -> Path:
        """获取版本文件路径."""
        return self._skill_path(skill_name) / f"{version}.json"

    def _index_path(self, skill_name: str) -> Path:
        """获取版本索引文件路径."""
        return self._skill_path(skill_name) / "versions.json"

    def _load_index(self, skill_name: str) -> dict[str, Any]:
        """加载版本索引."""
        index_path = self._index_path(skill_name)
        if not index_path.exists():
            return {"versions": [], "latest": None}
        with index_path.open("r", encoding="utf-8") as f:
            return json.load(f)

    def _save_index(self, skill_name: str, index: dict[str, Any]) -> None:
        """保存版本索引."""
        index_path = self._index_path(skill_name)
        index_path.parent.mkdir(parents=True, exist_ok=True)
        with index_path.open("w", encoding="utf-8") as f:
            json.dump(index, f, indent=2, ensure_ascii=False)

    def save(self, snapshot: SkillSnapshot) -> None:
        """保存快照."""
        # 保存快照文件
        version_path = self._version_path(snapshot.skill_name, str(snapshot.version))
        version_path.parent.mkdir(parents=True, exist_ok=True)
        with version_path.open("w", encoding="utf-8") as f:
            json.dump(snapshot.to_dict(), f, indent=2, ensure_ascii=False)

        # 更新索引
        index = self._load_index(snapshot.skill_name)
        version_str = str(snapshot.version)

        # 添加或更新版本信息
        version_found = False
        for v in index["versions"]:
            if v["version"] == version_str:
                v.update(snapshot.info.to_dict() if snapshot.info else {})
                version_found = True
                break

        if not version_found and snapshot.info:
            index["versions"].append(snapshot.info.to_dict())

        # 更新最新版本
        index["latest"] = version_str
        index["updated_at"] = datetime.now(UTC).isoformat()

        self._save_index(snapshot.skill_name, index)
        _logger.debug(f"Saved {snapshot.skill_name}@{snapshot.version}")

    def load(self, skill_name: str, version: str) -> SkillSnapshot | None:
        """加载快照."""
        version_path = self._version_path(skill_name, version)
        if not version_path.exists():
            return None
        with version_path.open("r", encoding="utf-8") as f:
            data = json.load(f)
        return SkillSnapshot.from_dict(data)

    def list_versions(self, skill_name: str) -> list[VersionInfo]:
        """列出所有版本."""
        index = self._load_index(skill_name)
        versions = []
        for v in index.get("versions", []):
            versions.append(VersionInfo.from_dict(v))
        # 按版本号排序
        versions.sort(key=lambda v: SkillVersion.parse(v.version), reverse=True)
        return versions

    def get_latest(self, skill_name: str) -> SkillSnapshot | None:
        """获取最新版本."""
        index = self._load_index(skill_name)
        latest = index.get("latest")
        if not latest:
            return None
        return self.load(skill_name, latest)

    def delete(self, skill_name: str, version: str) -> bool:
        """删除版本."""
        version_path = self._version_path(skill_name, version)
        if not version_path.exists():
            return False

        # 删除文件
        version_path.unlink()

        # 更新索引
        index = self._load_index(skill_name)
        index["versions"] = [v for v in index["versions"] if v["version"] != version]

        # 更新最新版本
        if index.get("latest") == version:
            if index["versions"]:
                # 找最新的版本
                latest = max(
                    index["versions"],
                    key=lambda v: SkillVersion.parse(v["version"]),
                )
                index["latest"] = latest["version"]
            else:
                index["latest"] = None

        self._save_index(skill_name, index)
        _logger.debug(f"Deleted {skill_name}@{version}")
        return True

    def exists(self, skill_name: str, version: str) -> bool:
        """检查版本是否存在."""
        return self._version_path(skill_name, version).exists()

    def list_skills(self) -> list[str]:
        """列出所有技能."""
        skills = []
        for path in self._base_path.iterdir():
            if path.is_dir() and (path / "versions.json").exists():
                skills.append(path.name)
        return sorted(skills)


__all__ = [
    "FileVersionStore",
    "InMemoryVersionStore",
    "VersionStore",
]
