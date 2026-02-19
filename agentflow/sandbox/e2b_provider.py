"""E2B サンドボックスプロバイダ.

e2b.dev クラウドサービスを使用したサンドボックス実行。
Daytonaスタイルの生命周期管理を統合。

前提条件:
    - E2B_API_KEY 環境変数が設定されていること
    - pip install e2b-code-interpreter

使用例:
    >>> from agentflow.sandbox import get_sandbox
    >>>
    >>> sandbox = get_sandbox(provider="e2b")
    >>> result = await sandbox.execute("print('Hello from e2b!')")
"""

from __future__ import annotations

import logging
import os
import time
from datetime import UTC, datetime
from typing import Any

from agentflow.sandbox.base import (
    ExecutionResult,
    SandboxConfig,
    SandboxProvider,
    SandboxState,
)


logger = logging.getLogger(__name__)


class E2BProvider(SandboxProvider):
    """E2B クラウドサンドボックスプロバイダ.

    https://e2b.dev のクラウドサービスを使用。
    Daytonaスタイルの生命周期追跡を含む。

    Attributes:
        state: 現在の状態
        execution_count: 実行回数
    """

    def __init__(self, config: SandboxConfig | None = None) -> None:
        """初期化.

        Args:
            config: サンドボックス設定

        Raises:
            ValueError: E2B_API_KEY が設定されていない
        """
        super().__init__(config)

        # API キー
        self._api_key = config.api_key if config and config.api_key else os.getenv("E2B_API_KEY")

        if not self._api_key:
            logger.warning(
                "E2B_API_KEY が設定されていません。https://e2b.dev で API キーを取得してください。"
            )

        # 生命周期追跡（Daytonaスタイル）
        self._state = SandboxState.CREATED
        self._created_at = datetime.now(UTC)
        self._last_activity_at = self._created_at
        self._execution_count = 0
        self._total_execution_ms = 0.0

        # SDK インポートチェック
        try:
            from e2b_code_interpreter import Sandbox

            self._sdk_available = True
        except ImportError:
            self._sdk_available = False
            logger.warning(
                "e2b-code-interpreter パッケージがインストールされていません。"
                "pip install e2b-code-interpreter を実行してください。"
            )

        logger.info("E2B provider initialized")

    @property
    def state(self) -> SandboxState:
        """現在の状態."""
        return self._state

    @property
    def execution_count(self) -> int:
        """実行回数."""
        return self._execution_count

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "provider": "e2b",
            "state": self._state.value,
            "sdk_available": self._sdk_available,
            "api_key_set": bool(self._api_key),
            "created_at": self._created_at.isoformat(),
            "last_activity_at": self._last_activity_at.isoformat(),
            "execution_count": self._execution_count,
            "total_execution_ms": self._total_execution_ms,
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
        """コードを e2b クラウドで実行.

        Args:
            code: Python コード
            timeout: タイムアウト秒
            packages: インストールするパッケージ
            env: 環境変数
            files: サンドボックスに配置するファイル

        Returns:
            ExecutionResult
        """
        if not self._sdk_available:
            return ExecutionResult(
                exit_code=1,
                error="e2b-code-interpreter SDK がインストールされていません",
            )

        if not self._api_key:
            return ExecutionResult(
                exit_code=1,
                error="E2B_API_KEY が設定されていません",
            )

        # 状態を STARTED に遷移（初回実行時）
        if self._state == SandboxState.CREATED:
            self._state = SandboxState.STARTED

        start_time = time.time()
        self._last_activity_at = datetime.now(UTC)

        try:
            from e2b_code_interpreter import Sandbox

            # サンドボックス作成
            sandbox = Sandbox(api_key=self._api_key)

            try:
                # パッケージインストール
                if packages:
                    pip_cmd = f"!pip install -q {' '.join(packages)}"
                    sandbox.run_code(pip_cmd)

                # ファイル配置
                if files:
                    for filename, content in files.items():
                        sandbox.files.write(filename, content)

                # コード実行
                execution = sandbox.run_code(code)
                output = execution.text or ""
                error = execution.error or ""

                duration_ms = (time.time() - start_time) * 1000

                # 実行統計を更新
                self._execution_count += 1
                self._total_execution_ms += duration_ms
                self._last_activity_at = datetime.now(UTC)

                return ExecutionResult(
                    stdout=output.strip(),
                    stderr=error,
                    exit_code=1 if error else 0,
                    duration_ms=duration_ms,
                    error=error if error else None,
                )

            finally:
                sandbox.close()

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            self._execution_count += 1
            self._total_execution_ms += duration_ms
            logger.exception(f"E2B execution error: {e}")
            return ExecutionResult(
                exit_code=1,
                duration_ms=duration_ms,
                error=f"{type(e).__name__}: {e}",
            )

    async def close(self) -> None:
        """リソース解放."""
        self._state = SandboxState.STOPPED
        logger.info("E2B provider closed")
