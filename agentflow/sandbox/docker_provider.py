# -*- coding: utf-8 -*-
"""Docker サンドボックスプロバイダ.

Docker コンテナを使用したサンドボックス実行。
開発・テスト環境向け。

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
from pathlib import Path
from typing import Any

from agentflow.sandbox.base import (
    ExecutionResult,
    SandboxConfig,
    SandboxProvider,
)

logger = logging.getLogger(__name__)


class DockerProvider(SandboxProvider):
    """Docker サンドボックスプロバイダ.

    Docker コンテナで Python コードを実行。
    """

    def __init__(self, config: SandboxConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)
        self._image = config.image if config else "python:3.13-slim"
        logger.info(f"Docker provider initialized: image={self._image}")

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
        start_time = time.time()
        effective_timeout = timeout or self._config.timeout

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
                    "docker", "run",
                    "--rm",
                    f"--memory={self._config.memory_mb}m",
                    f"--cpus={self._config.cpus}",
                    "--network=none",  # ネットワーク無効化（セキュリティ）
                    "-v", f"{tmpdir_path}:/app:ro",
                    "-w", "/app",
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
                except asyncio.TimeoutError:
                    process.kill()
                    return ExecutionResult(
                        exit_code=124,
                        error=f"Timeout after {effective_timeout}s",
                        duration_ms=(time.time() - start_time) * 1000,
                    )

            duration_ms = (time.time() - start_time) * 1000

            return ExecutionResult(
                stdout=stdout.decode("utf-8", errors="replace").strip(),
                stderr=stderr.decode("utf-8", errors="replace").strip(),
                exit_code=process.returncode or 0,
                duration_ms=duration_ms,
            )

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            logger.error(f"Docker execution error: {e}")
            return ExecutionResult(
                exit_code=1,
                duration_ms=duration_ms,
                error=f"{type(e).__name__}: {e}",
            )

