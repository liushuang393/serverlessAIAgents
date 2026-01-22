# -*- coding: utf-8 -*-
"""ワークスペース管理.

Daytonaのワークスペース概念を参考に、サンドボックスとファイルシステムを
統合した開発環境を提供。

設計原則:
- 統合環境: サンドボックス + ファイルシステム + 状態管理
- 状態永続化: ワークスペースの保存と復元
- セッション管理: 作業の継続性を確保
- マネージャー: 複数ワークスペースの統一管理

使用例:
    >>> from agentflow.sandbox.workspace import Workspace
    >>>
    >>> # ワークスペース作成
    >>> workspace = await Workspace.create(name="my-project")
    >>> await workspace.start()
    >>>
    >>> # ファイル操作
    >>> await workspace.write_file("main.py", b"print('Hello')")
    >>> result = await workspace.execute("exec(open('main.py').read())")
    >>>
    >>> # 状態保存
    >>> state = await workspace.save_state()
    >>> # 後で復元可能

マネージャー使用例:
    >>> from agentflow.sandbox import get_workspace_manager
    >>>
    >>> manager = get_workspace_manager()
    >>> ws = await manager.create("my-project")
    >>> await ws.start()
"""

from __future__ import annotations

import json
import logging
import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from agentflow.sandbox.base import ExecutionResult, SandboxConfig, SandboxState
from agentflow.sandbox.lifecycle import ManagedSandbox

logger = logging.getLogger(__name__)


@dataclass
class FileInfo:
    """ファイル情報.

    Attributes:
        path: ファイルパス
        size: サイズ（バイト）
        modified_at: 更新時刻
        is_directory: ディレクトリかどうか
    """

    path: str
    size: int = 0
    modified_at: datetime = field(default_factory=lambda: datetime.now(UTC))
    is_directory: bool = False

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "path": self.path,
            "size": self.size,
            "modified_at": self.modified_at.isoformat(),
            "is_directory": self.is_directory,
        }


@dataclass
class WorkspaceState:
    """ワークスペース状態.

    保存・復元可能なワークスペースの状態。

    Attributes:
        workspace_id: ワークスペースID
        name: ワークスペース名
        files: ファイル内容
        metadata: メタデータ
        saved_at: 保存時刻
    """

    workspace_id: str
    name: str
    files: dict[str, bytes] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)
    saved_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def to_json(self) -> str:
        """JSON文字列に変換."""
        import base64

        data = {
            "workspace_id": self.workspace_id,
            "name": self.name,
            "files": {k: base64.b64encode(v).decode() for k, v in self.files.items()},
            "metadata": self.metadata,
            "saved_at": self.saved_at.isoformat(),
        }
        return json.dumps(data, ensure_ascii=False, indent=2)

    @classmethod
    def from_json(cls, json_str: str) -> "WorkspaceState":
        """JSON文字列から復元."""
        import base64

        data = json.loads(json_str)
        return cls(
            workspace_id=data["workspace_id"],
            name=data["name"],
            files={k: base64.b64decode(v) for k, v in data["files"].items()},
            metadata=data.get("metadata", {}),
            saved_at=datetime.fromisoformat(data["saved_at"]),
        )


class Workspace:
    """ワークスペース.

    サンドボックスとファイルシステムを統合した開発環境。

    主な機能:
    - サンドボックス管理: コード実行環境
    - ファイル操作: 読み書き・一覧
    - 状態管理: 保存・復元

    Attributes:
        workspace_id: ワークスペースID
        name: ワークスペース名
        sandbox: 関連付けられたサンドボックス

    Example:
        >>> workspace = await Workspace.create(name="my-project")
        >>> await workspace.start()
        >>> await workspace.write_file("test.py", b"x = 1 + 1\\nprint(x)")
        >>> result = await workspace.run_file("test.py")
        >>> print(result.stdout)  # "2"
    """

    def __init__(
        self,
        sandbox: ManagedSandbox,
        name: str = "",
        workspace_id: str | None = None,
    ) -> None:
        """初期化.

        Args:
            sandbox: サンドボックス
            name: ワークスペース名
            workspace_id: ワークスペースID
        """
        self._sandbox = sandbox
        self.name = name or f"workspace-{datetime.now().strftime('%Y%m%d-%H%M%S')}"
        self.workspace_id = workspace_id or f"ws-{uuid.uuid4().hex[:12]}"
        self._files: dict[str, bytes] = {}
        self._metadata: dict[str, Any] = {}
        self.created_at = datetime.now(UTC)
        self._logger = logging.getLogger(f"{__name__}.{self.workspace_id}")

    @property
    def sandbox(self) -> ManagedSandbox:
        """関連付けられたサンドボックス."""
        return self._sandbox

    @property
    def is_running(self) -> bool:
        """実行中かどうか."""
        return self._sandbox.is_running

    @property
    def state(self) -> SandboxState:
        """サンドボックスの状態."""
        return self._sandbox.state

    @classmethod
    async def create(
        cls,
        name: str = "",
        provider: str = "docker",
        config: SandboxConfig | None = None,
    ) -> "Workspace":
        """ファクトリメソッド: ワークスペースを作成.

        Args:
            name: ワークスペース名
            provider: サンドボックスプロバイダ
            config: サンドボックス設定

        Returns:
            Workspace インスタンス
        """
        sandbox = await ManagedSandbox.create(provider, config)
        return cls(sandbox, name)

    async def start(self) -> None:
        """ワークスペースを起動."""
        await self._sandbox.start()
        self._logger.info(f"ワークスペースを起動: {self.name}")

    async def stop(self) -> None:
        """ワークスペースを停止."""
        await self._sandbox.stop()
        self._logger.info(f"ワークスペースを停止: {self.name}")

    async def delete(self) -> None:
        """ワークスペースを削除."""
        await self._sandbox.delete()
        self._files.clear()
        self._logger.info(f"ワークスペースを削除: {self.name}")

    # ファイル操作
    async def write_file(self, path: str, content: bytes | str) -> None:
        """ファイルを書き込み.

        Args:
            path: ファイルパス
            content: 内容
        """
        if isinstance(content, str):
            content = content.encode("utf-8")
        self._files[path] = content
        self._logger.debug(f"ファイル書き込み: {path} ({len(content)} bytes)")

    async def read_file(self, path: str) -> bytes | None:
        """ファイルを読み込み.

        Args:
            path: ファイルパス

        Returns:
            ファイル内容 または None
        """
        return self._files.get(path)

    async def delete_file(self, path: str) -> bool:
        """ファイルを削除.

        Args:
            path: ファイルパス

        Returns:
            削除成功かどうか
        """
        if path in self._files:
            del self._files[path]
            return True
        return False

    async def list_files(self, directory: str = "") -> list[FileInfo]:
        """ファイル一覧を取得.

        Args:
            directory: ディレクトリパス

        Returns:
            ファイル情報リスト
        """
        result: list[FileInfo] = []
        for path, content in self._files.items():
            if directory and not path.startswith(directory):
                continue
            result.append(FileInfo(
                path=path,
                size=len(content),
            ))
        return result

    # コード実行
    async def execute(
        self,
        code: str,
        *,
        timeout: float | None = None,
        packages: list[str] | None = None,
        env: dict[str, str] | None = None,
    ) -> ExecutionResult:
        """コードを実行.

        ワークスペース内のファイルにアクセス可能。

        Args:
            code: Python コード
            timeout: タイムアウト秒
            packages: パッケージ
            env: 環境変数

        Returns:
            ExecutionResult
        """
        return await self._sandbox.execute(
            code,
            timeout=timeout,
            packages=packages,
            env=env,
            files=self._files,
        )

    async def run_file(
        self,
        path: str,
        *,
        timeout: float | None = None,
        packages: list[str] | None = None,
    ) -> ExecutionResult:
        """ファイルを実行.

        Args:
            path: ファイルパス
            timeout: タイムアウト秒
            packages: パッケージ

        Returns:
            ExecutionResult
        """
        content = self._files.get(path)
        if not content:
            return ExecutionResult(
                exit_code=1,
                error=f"ファイルが見つかりません: {path}",
            )

        code = content.decode("utf-8")
        return await self.execute(code, timeout=timeout, packages=packages)

    # 状態管理
    async def save_state(self) -> WorkspaceState:
        """状態を保存.

        Returns:
            WorkspaceState
        """
        return WorkspaceState(
            workspace_id=self.workspace_id,
            name=self.name,
            files=dict(self._files),
            metadata=dict(self._metadata),
        )

    async def restore_state(self, state: WorkspaceState) -> None:
        """状態を復元.

        Args:
            state: WorkspaceState
        """
        self._files = dict(state.files)
        self._metadata = dict(state.metadata)
        self._logger.info(f"状態を復元: {len(self._files)} ファイル")

    def set_metadata(self, key: str, value: Any) -> None:
        """メタデータを設定."""
        self._metadata[key] = value

    def get_metadata(self, key: str, default: Any = None) -> Any:
        """メタデータを取得."""
        return self._metadata.get(key, default)

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "workspace_id": self.workspace_id,
            "name": self.name,
            "state": self.state.value,
            "file_count": len(self._files),
            "total_size": sum(len(c) for c in self._files.values()),
            "created_at": self.created_at.isoformat(),
            "sandbox_stats": self._sandbox.stats,
        }

    async def __aenter__(self) -> "Workspace":
        """async with サポート."""
        await self.start()
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """async with サポート."""
        await self.delete()


class WorkspaceManager:
    """ワークスペースマネージャー.

    複数のワークスペースを統一管理。Daytonaスタイルの管理機能を提供。

    主な機能:
    - ワークスペースの作成・取得・削除
    - 一覧表示・フィルタリング
    - 状態の保存・復元
    - 自動クリーンアップ

    Example:
        >>> manager = WorkspaceManager()
        >>> ws = await manager.create("my-project")
        >>> await ws.start()
        >>> # 作業...
        >>> await manager.delete(ws.workspace_id)
    """

    _instance: "WorkspaceManager | None" = None

    def __new__(cls) -> "WorkspaceManager":
        """シングルトンパターン."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False
        return cls._instance

    def __init__(self) -> None:
        """初期化."""
        if self._initialized:
            return
        self._workspaces: dict[str, Workspace] = {}
        self._logger = logging.getLogger(f"{__name__}.WorkspaceManager")
        self._initialized = True

    @classmethod
    def get_instance(cls) -> "WorkspaceManager":
        """シングルトンインスタンスを取得."""
        return cls()

    async def create(
        self,
        name: str = "",
        provider: str = "docker",
        config: SandboxConfig | None = None,
        workspace_id: str | None = None,
    ) -> Workspace:
        """ワークスペースを作成.

        Args:
            name: ワークスペース名
            provider: サンドボックスプロバイダ
            config: サンドボックス設定
            workspace_id: ワークスペースID（省略時は自動生成）

        Returns:
            Workspace インスタンス
        """
        workspace = await Workspace.create(name, provider, config)
        if workspace_id:
            workspace.workspace_id = workspace_id
        self._workspaces[workspace.workspace_id] = workspace
        self._logger.info(f"ワークスペース作成: {workspace.workspace_id}")
        return workspace

    def get(self, workspace_id: str) -> Workspace | None:
        """ワークスペースを取得.

        Args:
            workspace_id: ワークスペースID

        Returns:
            Workspace または None
        """
        return self._workspaces.get(workspace_id)

    def list(
        self,
        state: SandboxState | None = None,
        name_filter: str | None = None,
    ) -> list[Workspace]:
        """ワークスペース一覧を取得.

        Args:
            state: 状態でフィルタ
            name_filter: 名前でフィルタ（部分一致）

        Returns:
            Workspace リスト
        """
        result: list[Workspace] = []
        for ws in self._workspaces.values():
            if state and ws.state != state:
                continue
            if name_filter and name_filter not in ws.name:
                continue
            result.append(ws)
        return result

    async def delete(self, workspace_id: str) -> bool:
        """ワークスペースを削除.

        Args:
            workspace_id: ワークスペースID

        Returns:
            削除成功かどうか
        """
        workspace = self._workspaces.pop(workspace_id, None)
        if workspace:
            await workspace.delete()
            self._logger.info(f"ワークスペース削除: {workspace_id}")
            return True
        return False

    async def delete_all(self) -> int:
        """全ワークスペースを削除.

        Returns:
            削除数
        """
        count = 0
        for ws_id in list(self._workspaces.keys()):
            if await self.delete(ws_id):
                count += 1
        return count

    async def save_state(self, workspace_id: str, path: Path | str) -> bool:
        """ワークスペース状態をファイルに保存.

        Args:
            workspace_id: ワークスペースID
            path: 保存先パス

        Returns:
            保存成功かどうか
        """
        workspace = self._workspaces.get(workspace_id)
        if not workspace:
            return False

        state = await workspace.save_state()
        path = Path(path)
        path.write_text(state.to_json(), encoding="utf-8")
        self._logger.info(f"状態保存: {workspace_id} -> {path}")
        return True

    async def restore_state(
        self,
        path: Path | str,
        provider: str = "docker",
    ) -> Workspace | None:
        """ファイルからワークスペース状態を復元.

        Args:
            path: 状態ファイルパス
            provider: サンドボックスプロバイダ

        Returns:
            復元されたWorkspace または None
        """
        path = Path(path)
        if not path.exists():
            return None

        json_str = path.read_text(encoding="utf-8")
        state = WorkspaceState.from_json(json_str)

        workspace = await Workspace.create(state.name, provider)
        workspace.workspace_id = state.workspace_id
        await workspace.restore_state(state)

        self._workspaces[workspace.workspace_id] = workspace
        self._logger.info(f"状態復元: {workspace.workspace_id}")
        return workspace

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        state_counts: dict[str, int] = {}
        for ws in self._workspaces.values():
            state_name = ws.state.value
            state_counts[state_name] = state_counts.get(state_name, 0) + 1

        return {
            "total_workspaces": len(self._workspaces),
            "state_counts": state_counts,
            "workspaces": [ws.get_stats() for ws in self._workspaces.values()],
        }


def get_workspace_manager() -> WorkspaceManager:
    """WorkspaceManagerシングルトンを取得."""
    return WorkspaceManager.get_instance()

