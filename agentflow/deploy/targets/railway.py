# -*- coding: utf-8 -*-
"""RailwayTarget - Railway デプロイターゲット.

Railway への簡単なコンテナデプロイを実装します。
"""

from __future__ import annotations

import logging
from collections.abc import AsyncIterator
from pathlib import Path
from typing import Any

from agentflow.core.interfaces import (
    ConfigField,
    DeployConfig,
    DeployEvent,
    ValidationResult,
)
from agentflow.deploy.targets.base import BaseDeployTarget

logger = logging.getLogger(__name__)


class RailwayTarget(BaseDeployTarget):
    """Railway デプロイターゲット."""

    @property
    def name(self) -> str:
        return "Railway"

    @property
    def description(self) -> str:
        return "Deploy applications to Railway with zero configuration"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Railway にデプロイ."""
        api_token = config.credentials.get("railway_token")
        project_id = config.settings.get("project_id")
        service_name = config.settings.get("service_name", "agentflow-app")

        if not api_token:
            yield DeployEvent(type="error", message="Railway API token is required")
            return

        yield DeployEvent(
            type="progress",
            message="Initializing Railway deployment...",
            progress=10,
            phase="init",
        )

        try:
            import httpx

            headers = {
                "Authorization": f"Bearer {api_token}",
                "Content-Type": "application/json",
            }

            async with httpx.AsyncClient() as client:
                yield DeployEvent(
                    type="progress",
                    message="Connecting to Railway API...",
                    progress=20,
                    phase="connect",
                )

                # プロジェクト情報を取得または作成
                if not project_id:
                    # 新規プロジェクト作成
                    response = await client.post(
                        "https://backboard.railway.app/graphql/v2",
                        headers=headers,
                        json={
                            "query": """
                                mutation {
                                    projectCreate(input: {name: "%s"}) {
                                        id
                                        name
                                    }
                                }
                            """ % service_name
                        },
                    )
                    data = response.json()
                    project_id = data.get("data", {}).get("projectCreate", {}).get("id")

                yield DeployEvent(
                    type="progress",
                    message=f"Deploying to project: {project_id}",
                    progress=50,
                    phase="deploy",
                )

                # デプロイトリガー (GitHub 連携または直接デプロイ)
                # Railway は主に GitHub 連携でデプロイするため、
                # ここでは Deploy Hook を使用
                deploy_hook = config.settings.get("deploy_hook")
                if deploy_hook:
                    response = await client.post(deploy_hook)
                    if response.status_code == 200:
                        yield DeployEvent(
                            type="success",
                            message="Deployment triggered successfully",
                            data={"project_id": project_id, "service": service_name},
                        )
                    else:
                        yield DeployEvent(
                            type="error",
                            message=f"Deploy hook failed: {response.text}",
                        )
                else:
                    yield DeployEvent(
                        type="success",
                        message="Project configured. Connect GitHub repo to deploy.",
                        data={"project_id": project_id, "service": service_name},
                    )

        except ImportError:
            yield DeployEvent(
                type="error",
                message="httpx is required. Run: pip install httpx",
            )
        except Exception as e:
            logger.exception("Railway deployment failed")
            yield DeployEvent(type="error", message=f"Deployment failed: {e}")

    def get_config_fields(self) -> list[ConfigField]:
        """設定フィールドを取得."""
        return [
            ConfigField(
                name="railway_token", label="Railway Token", type="password",
                required=True, description="Railway API トークン", group="credentials",
            ),
            ConfigField(
                name="project_id", label="Project ID", type="string",
                required=False, description="既存プロジェクト ID (空で新規作成)", group="settings",
            ),
            ConfigField(
                name="service_name", label="Service Name", type="string",
                required=True, placeholder="my-agentflow-app", group="settings",
            ),
            ConfigField(
                name="deploy_hook", label="Deploy Hook URL", type="string",
                required=False, description="Railway Deploy Hook URL", group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        if not config.get("railway_token"):
            errors["railway_token"] = "Railway token is required"
        return ValidationResult(valid=len(errors) == 0, errors=errors)


__all__ = ["RailwayTarget"]

