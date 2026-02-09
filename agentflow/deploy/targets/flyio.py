"""FlyioTarget - Fly.io デプロイターゲット.

Fly.io へのグローバル分散デプロイを実装します。
"""

from __future__ import annotations

import logging
import subprocess
from typing import TYPE_CHECKING, Any

from agentflow.core.interfaces import (
    ConfigField,
    DeployConfig,
    DeployEvent,
    ValidationResult,
)
from agentflow.deploy.targets.base import BaseDeployTarget


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from pathlib import Path


logger = logging.getLogger(__name__)


class FlyioTarget(BaseDeployTarget):
    """Fly.io デプロイターゲット."""

    @property
    def name(self) -> str:
        return "Fly.io"

    @property
    def description(self) -> str:
        return "Deploy applications globally to Fly.io edge network"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Fly.io にデプロイ."""
        api_token = config.credentials.get("fly_api_token")
        app_name = config.settings.get("app_name", "agentflow-app")
        region = config.settings.get("region", "nrt")  # Tokyo

        if not api_token:
            yield DeployEvent(type="error", message="Fly.io API token is required")
            return

        yield DeployEvent(
            type="progress",
            message="Initializing Fly.io deployment...",
            progress=10,
            phase="init",
        )

        try:
            import os
            env = os.environ.copy()
            env["FLY_API_TOKEN"] = api_token

            # fly.toml が存在するか確認
            fly_toml = source_path / "fly.toml"
            if not fly_toml.exists():
                yield DeployEvent(
                    type="progress",
                    message="Creating fly.toml configuration...",
                    progress=20,
                    phase="config",
                )
                fly_config = self._generate_fly_toml(app_name, region, config)
                fly_toml.write_text(fly_config, encoding="utf-8")

            yield DeployEvent(
                type="progress",
                message=f"Deploying app: {app_name}",
                progress=40,
                phase="deploy",
            )

            # flyctl deploy 実行
            result = subprocess.run(
                ["flyctl", "deploy", "--remote-only", "--yes"],
                capture_output=True,
                text=True,
                cwd=str(source_path),
                env=env,
            )

            if result.returncode != 0:
                # アプリが存在しない場合は作成
                if "Could not find App" in result.stderr:
                    yield DeployEvent(
                        type="progress",
                        message="Creating new Fly.io app...",
                        progress=50,
                        phase="create",
                    )
                    create_result = subprocess.run(
                        ["flyctl", "apps", "create", app_name, "--org", "personal"],
                        capture_output=True,
                        text=True,
                        env=env,
                    )
                    if create_result.returncode != 0:
                        yield DeployEvent(
                            type="error",
                            message=f"Failed to create app: {create_result.stderr}",
                        )
                        return

                    # 再度デプロイ
                    result = subprocess.run(
                        ["flyctl", "deploy", "--remote-only", "--yes"],
                        capture_output=True,
                        text=True,
                        cwd=str(source_path),
                        env=env,
                    )

            if result.returncode != 0:
                yield DeployEvent(
                    type="error",
                    message=f"Deployment failed: {result.stderr}",
                )
                return

            app_url = f"https://{app_name}.fly.dev"

            yield DeployEvent(
                type="success",
                message=f"Successfully deployed to {app_url}",
                data={"app_name": app_name, "url": app_url, "region": region},
            )

        except FileNotFoundError:
            yield DeployEvent(
                type="error",
                message="flyctl CLI is required. Install: curl -L https://fly.io/install.sh | sh",
            )
        except Exception as e:
            logger.exception("Fly.io deployment failed")
            yield DeployEvent(type="error", message=f"Deployment failed: {e}")

    def _generate_fly_toml(
        self, app_name: str, region: str, config: DeployConfig
    ) -> str:
        """fly.toml 設定を生成."""
        return f"""# Fly.io Configuration
app = "{app_name}"
primary_region = "{region}"

[build]

[http_service]
  internal_port = 8080
  force_https = true
  auto_stop_machines = true
  auto_start_machines = true
  min_machines_running = 0

[[vm]]
  cpu_kind = "shared"
  cpus = 1
  memory_mb = {config.settings.get("memory", 256)}
"""

    def get_config_fields(self) -> list[ConfigField]:
        """設定フィールドを取得."""
        return [
            ConfigField(
                name="fly_api_token", label="Fly.io API Token", type="password",
                required=True, description="Fly.io API トークン", group="credentials",
            ),
            ConfigField(
                name="app_name", label="App Name", type="string",
                required=True, placeholder="my-agentflow-app", group="settings",
            ),
            ConfigField(
                name="region", label="Primary Region", type="select",
                required=False, default="nrt",
                options=["nrt", "hnd", "sin", "syd", "lax", "ord", "iad", "lhr", "fra"],
                description="プライマリリージョン", group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        if not config.get("fly_api_token"):
            errors["fly_api_token"] = "Fly.io API token is required"
        if not config.get("app_name"):
            errors["app_name"] = "App name is required"
        return ValidationResult(valid=len(errors) == 0, errors=errors)


__all__ = ["FlyioTarget"]

