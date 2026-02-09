"""DeployExecutor - デプロイ執行器実装.

IDeployExecutor インターフェースを実装し、
各プラットフォームへのデプロイを統一的に処理します。
"""

from __future__ import annotations

import logging
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING, Any

from agentflow.core.interfaces import (
    ConfigField,
    DeployConfig,
    DeployEvent,
    DeployResult,
    DeployTarget,
    GeneratedCode,
    IDeployExecutor,
    ValidationResult,
)
from agentflow.deploy.targets.aws_lambda import AWSLambdaTarget
from agentflow.deploy.targets.docker import DockerTarget
from agentflow.deploy.targets.github_actions import GitHubActionsTarget
from agentflow.deploy.targets.vercel import VercelTarget


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

    from agentflow.deploy.targets.base import BaseDeployTarget


logger = logging.getLogger(__name__)


class DeployExecutor(IDeployExecutor):
    """デプロイ執行器.

    生成されたコードを各プラットフォームにデプロイします。

    サポートするターゲット:
    - Vercel: Serverless Functions
    - Docker: コンテナビルド＆プッシュ
    - AWS Lambda: Lambda Functions
    - GitHub Actions: CI/CD ワークフロー生成
    """

    def __init__(self) -> None:
        """初期化."""
        self._targets: dict[DeployTarget, BaseDeployTarget] = {
            DeployTarget.VERCEL: VercelTarget(),
            DeployTarget.DOCKER: DockerTarget(),
            DeployTarget.AWS_LAMBDA: AWSLambdaTarget(),
            DeployTarget.GITHUB_ACTIONS: GitHubActionsTarget(),
        }

    async def deploy(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """デプロイを実行.

        Args:
            source: 生成されたコード、またはソースディレクトリ
            target: デプロイターゲット
            config: デプロイ設定

        Yields:
            デプロイイベント
        """
        deployer = self._targets.get(target)
        if deployer is None:
            yield DeployEvent(
                type="error",
                message=f"Unsupported deploy target: {target}",
            )
            return

        # ソースをディレクトリに展開
        if isinstance(source, GeneratedCode):
            with tempfile.TemporaryDirectory() as tmpdir:
                source_path = Path(tmpdir)
                for file_path, content in source.files.items():
                    full_path = source_path / file_path
                    full_path.parent.mkdir(parents=True, exist_ok=True)
                    full_path.write_text(content, encoding="utf-8")

                async for event in deployer.deploy(source_path, config):
                    yield event
        else:
            async for event in deployer.deploy(source, config):
                yield event

    async def deploy_sync(
        self,
        source: GeneratedCode | Path,
        target: DeployTarget,
        config: DeployConfig,
    ) -> DeployResult:
        """デプロイを同期実行.

        Args:
            source: 生成されたコード、またはソースディレクトリ
            target: デプロイターゲット
            config: デプロイ設定

        Returns:
            デプロイ結果
        """
        logs: list[str] = []
        deployment_id: str | None = None
        url: str | None = None
        success = False
        error: str | None = None

        async for event in self.deploy(source, target, config):
            logs.append(event.message)

            if event.type == "success":
                success = True
                if event.data:
                    deployment_id = event.data.get("deployment_id")
                    url = event.data.get("url")
            elif event.type == "error":
                error = event.message

        return DeployResult(
            success=success,
            deployment_id=deployment_id,
            url=url,
            logs=logs,
            error=error,
        )

    async def get_required_config(
        self,
        target: DeployTarget,
    ) -> list[ConfigField]:
        """ターゲットに必要な設定フィールドを取得.

        Args:
            target: デプロイターゲット

        Returns:
            設定フィールドのリスト
        """
        deployer = self._targets.get(target)
        if deployer is None:
            return []
        return deployer.get_config_fields()

    async def validate_config(
        self,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> ValidationResult:
        """設定を検証.

        Args:
            target: デプロイターゲット
            config: 検証する設定

        Returns:
            検証結果
        """
        deployer = self._targets.get(target)
        if deployer is None:
            return ValidationResult(
                valid=False,
                errors={"target": f"Unsupported target: {target}"},
            )
        return deployer.validate_config(config)

    def get_supported_targets(self) -> list[DeployTarget]:
        """サポートされているターゲットを取得.

        Returns:
            ターゲットのリスト
        """
        return list(self._targets.keys())


__all__ = ["DeployExecutor"]
