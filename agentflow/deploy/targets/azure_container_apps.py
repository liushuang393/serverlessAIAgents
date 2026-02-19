"""AzureContainerAppsTarget - Azure Container Apps デプロイターゲット.

Azure Container Apps へのコンテナデプロイを実装します。
"""

from __future__ import annotations

import logging
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


class AzureContainerAppsTarget(BaseDeployTarget):
    """Azure Container Apps デプロイターゲット."""

    @property
    def name(self) -> str:
        return "Azure Container Apps"

    @property
    def description(self) -> str:
        return "Deploy containerized applications to Azure Container Apps"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Azure Container Apps にデプロイ."""
        subscription_id = config.credentials.get("subscription_id")
        resource_group = config.settings.get("resource_group")
        app_name = config.settings.get("app_name", "agentflow-app")
        environment_name = config.settings.get("environment_name")
        image_url = config.settings.get("image_url")

        if not subscription_id or not resource_group:
            yield DeployEvent(
                type="error",
                message="Azure subscription ID and resource group are required",
            )
            return

        yield DeployEvent(
            type="progress",
            message="Initializing Azure Container Apps deployment...",
            progress=10,
            phase="init",
        )

        try:
            from azure.identity import DefaultAzureCredential
            from azure.mgmt.appcontainers import ContainerAppsAPIClient
            from azure.mgmt.appcontainers.models import (
                Configuration,
                Container,
                ContainerApp,
                Ingress,
                Template,
            )

            credential = DefaultAzureCredential()
            client = ContainerAppsAPIClient(credential, subscription_id)

            yield DeployEvent(
                type="progress",
                message=f"Deploying app: {app_name}",
                progress=30,
                phase="deploy",
            )

            # Container App 設定
            container_app = ContainerApp(
                location=config.settings.get("location", "eastus"),
                managed_environment_id=f"/subscriptions/{subscription_id}/resourceGroups/{resource_group}/providers/Microsoft.App/managedEnvironments/{environment_name}",
                configuration=Configuration(
                    ingress=Ingress(
                        external=True,
                        target_port=8080,
                        transport="auto",
                    ),
                ),
                template=Template(
                    containers=[
                        Container(
                            name=app_name,
                            image=image_url,
                            resources={
                                "cpu": config.settings.get("cpu", 0.5),
                                "memory": config.settings.get("memory", "1Gi"),
                            },
                            env=[{"name": k, "value": v} for k, v in config.env_vars.items()],
                        )
                    ],
                    scale={
                        "minReplicas": config.settings.get("min_replicas", 0),
                        "maxReplicas": config.settings.get("max_replicas", 10),
                    },
                ),
            )

            yield DeployEvent(
                type="progress",
                message="Creating/updating container app...",
                progress=60,
                phase="creating",
            )

            # デプロイ実行
            poller = client.container_apps.begin_create_or_update(
                resource_group, app_name, container_app
            )
            result = poller.result()

            app_url = f"https://{result.configuration.ingress.fqdn}"

            yield DeployEvent(
                type="success",
                message=f"Successfully deployed to {app_url}",
                data={"app_name": app_name, "url": app_url},
            )

        except ImportError:
            yield DeployEvent(
                type="error",
                message="azure-mgmt-appcontainers is required. Run: pip install azure-mgmt-appcontainers azure-identity",
            )
        except Exception as e:
            logger.exception("Azure Container Apps deployment failed")
            yield DeployEvent(type="error", message=f"Deployment failed: {e}")

    def get_config_fields(self) -> list[ConfigField]:
        """設定フィールドを取得."""
        return [
            ConfigField(
                name="subscription_id",
                label="Subscription ID",
                type="string",
                required=True,
                description="Azure サブスクリプション ID",
                group="credentials",
            ),
            ConfigField(
                name="resource_group",
                label="Resource Group",
                type="string",
                required=True,
                description="リソースグループ名",
                group="settings",
            ),
            ConfigField(
                name="app_name",
                label="App Name",
                type="string",
                required=True,
                placeholder="my-agentflow-app",
                group="settings",
            ),
            ConfigField(
                name="environment_name",
                label="Environment Name",
                type="string",
                required=True,
                description="Container Apps 環境名",
                group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        for field in ["subscription_id", "resource_group", "app_name", "environment_name"]:
            if not config.get(field):
                errors[field] = f"{field} is required"
        return ValidationResult(valid=len(errors) == 0, errors=errors)


__all__ = ["AzureContainerAppsTarget"]
