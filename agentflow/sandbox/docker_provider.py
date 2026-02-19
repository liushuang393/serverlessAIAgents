"""Docker サンドボックスプロバイダ.

Docker コンテナを使用したサンドボックス実行。
開発・テスト環境向け。Daytonaスタイルの生命周期管理を統合。

前提条件:
    - Docker がインストールされていること
    - docker デーモンが起動していること

使用例:
    >>> from agentflow.sandbox import get_sandbox
    >>>
    >>> sandbox = get_sandbox(provider="docker")
    >>> result = await sandbox.execute("print('Hello from Docker!')")
"""

from __future__ import annotations

import asyncio
import logging
import tempfile
import time
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from agentflow.sandbox.base import (
    ExecutionResult,
    ResourceUsage,
    SandboxConfig,
    SandboxProvider,
    SandboxState,
)


logger = logging.getLogger(__name__)


class DockerProvider(SandboxProvider):
    """Docker サンドボックスプロバイダ.

    Docker コンテナで Python コードを実行。
    Daytonaスタイルの生命周期追跡を含む。

    Attributes:
        state: 現在の状態
        resource_usage: リソース使用状況
        execution_count: 実行回数
    """

    def __init__(self, config: SandboxConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)
        self._image = config.image if config else "python:3.13-slim"

        # 生命周期追跡（Daytonaスタイル）
        self._state = SandboxState.CREATED
        self._created_at = datetime.now(UTC)
        self._last_activity_at = self._created_at
        self._execution_count = 0
        self._total_execution_ms = 0.0
        self._last_resource_usage: ResourceUsage | None = None

        logger.info(f"Docker provider initialized: image={self._image}")

    @property
    def state(self) -> SandboxState:
        """現在の状態."""
        return self._state

    @property
    def execution_count(self) -> int:
        """実行回数."""
        return self._execution_count

    @property
    def resource_usage(self) -> ResourceUsage | None:
        """最新のリソース使用状況."""
        return self._last_resource_usage

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "provider": "docker",
            "state": self._state.value,
            "image": self._image,
            "created_at": self._created_at.isoformat(),
            "last_activity_at": self._last_activity_at.isoformat(),
            "execution_count": self._execution_count,
            "total_execution_ms": self._total_execution_ms,
            "avg_execution_ms": (
                self._total_execution_ms / self._execution_count
                if self._execution_count > 0
                else 0.0
            ),
        }

    async def execute(
        self,
        code: str,
        *,
        timeout: float | None = None,
        packages: list[str] | None = None,
        env: dict[str, str] | None = None,
        files: dict[str, bytes] | None = None,
    ) -> ExecutionResult:
        """コードを Docker コンテナで実行.

        Args:
            code: Python コード
            timeout: タイムアウト秒
            packages: インストールするパッケージ
            env: 環境変数
            files: コンテナに配置するファイル

        Returns:
            ExecutionResult
        """
        # 状態を STARTED に遷移（初回実行時）
        if self._state == SandboxState.CREATED:
            self._state = SandboxState.STARTED

        start_time = time.time()
        effective_timeout = timeout or self._config.timeout
        self._last_activity_at = datetime.now(UTC)

        try:
            # 一時ディレクトリにコードを保存
            with tempfile.TemporaryDirectory() as tmpdir:
                tmpdir_path = Path(tmpdir)

                # スクリプトファイル作成
                script_path = tmpdir_path / "script.py"

                # パッケージインストールコードを追加
                full_code = ""
                if packages:
                    pip_install = (
                        "import subprocess, sys\n"
                        f"subprocess.run([sys.executable, '-m', 'pip', 'install', '-q', "
                        f"{', '.join(repr(p) for p in packages)}], check=True)\n\n"
                    )
                    full_code += pip_install

                full_code += code
                script_path.write_text(full_code, encoding="utf-8")

                # 追加ファイルを配置
                if files:
                    for filename, content in files.items():
                        file_path = tmpdir_path / filename
                        file_path.parent.mkdir(parents=True, exist_ok=True)
                        file_path.write_bytes(content)

                # Docker コマンド構築
                docker_cmd = [
                    "docker",
                    "run",
                    "--rm",
                    f"--memory={self._config.memory_mb}m",
                    f"--cpus={self._config.cpus}",
                    "--network=none",  # ネットワーク無効化（セキュリティ）
                    "-v",
                    f"{tmpdir_path}:/app:ro",
                    "-w",
                    "/app",
                ]

                # 環境変数追加
                if env:
                    for key, value in env.items():
                        docker_cmd.extend(["-e", f"{key}={value}"])

                docker_cmd.extend([self._image, "python", "/app/script.py"])

                # 実行
                process = await asyncio.create_subprocess_exec(
                    *docker_cmd,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                )

                try:
                    stdout, stderr = await asyncio.wait_for(
                        process.communicate(),
                        timeout=effective_timeout,
                    )
                except TimeoutError:
                    process.kill()
                    return ExecutionResult(
                        exit_code=124,
                        error=f"Timeout after {effective_timeout}s",
                        duration_ms=(time.time() - start_time) * 1000,
                    )

            duration_ms = (time.time() - start_time) * 1000

            # 実行統計を更新
            self._execution_count += 1
            self._total_execution_ms += duration_ms
            self._last_activity_at = datetime.now(UTC)

            return ExecutionResult(
                stdout=stdout.decode("utf-8", errors="replace").strip(),
                stderr=stderr.decode("utf-8", errors="replace").strip(),
                exit_code=process.returncode or 0,
                duration_ms=duration_ms,
            )

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            self._execution_count += 1
            self._total_execution_ms += duration_ms
            logger.exception(f"Docker execution error: {e}")
            return ExecutionResult(
                exit_code=1,
                duration_ms=duration_ms,
                error=f"{type(e).__name__}: {e}",
            )

    async def close(self) -> None:
        """リソース解放."""
        self._state = SandboxState.STOPPED
        logger.info("Docker provider closed")
