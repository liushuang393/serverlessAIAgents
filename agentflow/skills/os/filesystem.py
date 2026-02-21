"""ファイルシステムスキル.

安全なファイル操作APIを提供。全ての操作はワークスペース内に制限。

Example:
    >>> fs = FileSystemSkill(config)
    >>> content = await fs.read_file("data.txt")
    >>> files = await fs.list_dir("./")
    >>> exists = await fs.exists("config.json")
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any

from agentflow.skills.os.base import OSSkillBase, OSSkillError, PathSecurityError


if TYPE_CHECKING:
    from agentflow.skills.os.config import OSSkillConfig


@dataclass
class FileInfo:
    """ファイル情報."""

    name: str
    path: str
    size_bytes: int
    is_file: bool
    is_dir: bool
    modified_at: datetime | None = None
    permissions: str = ""

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "path": self.path,
            "size_bytes": self.size_bytes,
            "is_file": self.is_file,
            "is_dir": self.is_dir,
            "modified_at": self.modified_at.isoformat() if self.modified_at else None,
            "permissions": self.permissions,
        }


@dataclass
class FileOperationResult:
    """ファイル操作結果."""

    success: bool
    operation: str
    path: str
    message: str = ""
    details: dict[str, Any] = field(default_factory=dict)


class FileSystemSkill(OSSkillBase):
    """ファイルシステム操作スキル.

    ワークスペース内でのファイル読み書き・一覧取得を提供。
    """

    def __init__(self, config: OSSkillConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)

    async def read_file(self, path: str, encoding: str = "utf-8") -> str:
        """ファイルを読み込む.

        Args:
            path: ファイルパス（ワークスペース相対）
            encoding: 文字エンコーディング

        Returns:
            ファイル内容

        Raises:
            PathSecurityError: パスがワークスペース外の場合
            OSSkillError: ファイルが存在しない/読み込めない場合
        """
        validated_path = self._validate_path(path)

        if not validated_path.exists():
            msg = f"ファイルが存在しません: {path}"
            raise OSSkillError(msg, skill_name="FileSystemSkill")

        if not validated_path.is_file():
            msg = f"ファイルではありません: {path}"
            raise OSSkillError(msg, skill_name="FileSystemSkill")

        # ファイルサイズチェック
        size_mb = validated_path.stat().st_size / (1024 * 1024)
        if size_mb > self._config.max_file_size_mb:
            msg = f"ファイルサイズ超過: {size_mb:.2f}MB > {self._config.max_file_size_mb}MB"
            raise OSSkillError(msg, skill_name="FileSystemSkill")

        self._audit_log("read_file", {"path": str(validated_path)})

        # 小さなローカルファイルI/Oを同期実行し、イベントループのスレッドプール生成を避ける
        return validated_path.read_text(encoding=encoding)

    async def write_file(self, path: str, content: str, encoding: str = "utf-8") -> FileOperationResult:
        """ファイルに書き込む.

        Args:
            path: ファイルパス
            content: 書き込む内容
            encoding: 文字エンコーディング

        Returns:
            操作結果
        """
        self._check_write_permission()
        validated_path = self._validate_path(path)

        # サイズチェック
        size_mb = len(content.encode(encoding)) / (1024 * 1024)
        if size_mb > self._config.max_file_size_mb:
            msg = f"書き込みサイズ超過: {size_mb:.2f}MB > {self._config.max_file_size_mb}MB"
            raise OSSkillError(msg, skill_name="FileSystemSkill")

        self._audit_log("write_file", {"path": str(validated_path), "size_bytes": len(content)})

        # 小さなローカルファイルI/Oを同期実行し、イベントループのスレッドプール生成を避ける
        validated_path.write_text(content, encoding=encoding)

        return FileOperationResult(
            success=True,
            operation="write_file",
            path=str(validated_path),
            message=f"ファイルを書き込みました: {path}",
        )

    async def list_dir(self, path: str = ".") -> list[FileInfo]:
        """ディレクトリ内容を一覧取得.

        Args:
            path: ディレクトリパス

        Returns:
            ファイル情報リスト
        """
        validated_path = self._validate_path(path)

        if not validated_path.is_dir():
            msg = f"ディレクトリではありません: {path}"
            raise OSSkillError(msg, skill_name="FileSystemSkill")

        self._audit_log("list_dir", {"path": str(validated_path)})

        result: list[FileInfo] = []
        for item in validated_path.iterdir():
            stat = item.stat()
            result.append(
                FileInfo(
                    name=item.name,
                    path=str(item.relative_to(self._config.workspace_path)),
                    size_bytes=stat.st_size,
                    is_file=item.is_file(),
                    is_dir=item.is_dir(),
                    modified_at=datetime.fromtimestamp(stat.st_mtime),
                )
            )

        return sorted(result, key=lambda f: (not f.is_dir, f.name.lower()))

    async def exists(self, path: str) -> bool:
        """パスが存在するか確認."""
        try:
            validated_path = self._validate_path(path)
            return validated_path.exists()
        except PathSecurityError:
            return False

    async def get_file_info(self, path: str) -> FileInfo:
        """ファイル/ディレクトリ情報を取得."""
        validated_path = self._validate_path(path)
        if not validated_path.exists():
            msg = f"パスが存在しません: {path}"
            raise OSSkillError(msg, skill_name="FileSystemSkill")

        stat = validated_path.stat()
        return FileInfo(
            name=validated_path.name,
            path=str(validated_path.relative_to(self._config.workspace_path)),
            size_bytes=stat.st_size,
            is_file=validated_path.is_file(),
            is_dir=validated_path.is_dir(),
            modified_at=datetime.fromtimestamp(stat.st_mtime),
        )
