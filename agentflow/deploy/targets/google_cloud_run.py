"""GoogleCloudRunTarget - Google Cloud Run デプロイターゲット.

Google Cloud Run へのコンテナデプロイを実装します。
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


class GoogleCloudRunTarget(BaseDeployTarget):
    """Google Cloud Run デプロイターゲット."""

    @property
    def name(self) -> str:
        return "Google Cloud Run"

    @property
    def description(self) -> str:
        return "Deploy containerized applications to Google Cloud Run"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Google Cloud Run にデプロイ."""
        project_id = config.settings.get("project_id")
        service_name = config.settings.get("service_name", "agentflow-app")
        region = config.settings.get("region", "us-central1")
        image_url = config.settings.get("image_url")

        if not project_id:
            yield DeployEvent(type="error", message="Google Cloud project ID is required")
            return

        yield DeployEvent(
            type="progress",
            message="Initializing Google Cloud Run deployment...",
            progress=10,
            phase="init",
        )

        try:
            from google.cloud import run_v2

            # Cloud Run クライアント作成
            client = run_v2.ServicesAsyncClient()

            yield DeployEvent(
                type="progress",
                message=f"Deploying service: {service_name}",
                progress=30,
                phase="deploy",
            )

            # サービス設定
            service = run_v2.Service(
                template=run_v2.RevisionTemplate(
                    containers=[
                        run_v2.Container(
                            image=image_url,
                            ports=[run_v2.ContainerPort(container_port=8080)],
                            resources=run_v2.ResourceRequirements(
                                limits={
                                    "memory": config.settings.get("memory", "512Mi"),
                                    "cpu": config.settings.get("cpu", "1"),
                                }
                            ),
                            env=[
                                run_v2.EnvVar(name=k, value=v) for k, v in config.env_vars.items()
                            ],
                        )
                    ],
                    scaling=run_v2.RevisionScaling(
                        min_instance_count=config.settings.get("min_instances", 0),
                        max_instance_count=config.settings.get("max_instances", 10),
                    ),
                )
            )

            parent = f"projects/{project_id}/locations/{region}"

            # サービスを作成または更新
            operation = await client.create_service(
                parent=parent,
                service=service,
                service_id=service_name,
            )

            yield DeployEvent(
                type="progress",
                message="Waiting for deployment to complete...",
                progress=70,
                phase="waiting",
            )

            result = await operation.result()

            yield DeployEvent(
                type="success",
                message=f"Successfully deployed to {result.uri}",
                data={"service_name": service_name, "url": result.uri, "region": region},
            )

        except ImportError:
            yield DeployEvent(
                type="error",
                message="google-cloud-run is required. Run: pip install google-cloud-run",
            )
        except Exception as e:
            logger.exception("Google Cloud Run deployment failed")
            yield DeployEvent(type="error", message=f"Deployment failed: {e}")

    def get_config_fields(self) -> list[ConfigField]:
        """設定フィールドを取得."""
        return [
            ConfigField(
                name="project_id",
                label="Project ID",
                type="string",
                required=True,
                description="Google Cloud プロジェクト ID",
                group="settings",
            ),
            ConfigField(
                name="service_name",
                label="Service Name",
                type="string",
                required=True,
                description="Cloud Run サービス名",
                placeholder="my-agentflow-app",
                group="settings",
            ),
            ConfigField(
                name="region",
                label="Region",
                type="select",
                required=True,
                default="us-central1",
                options=["us-central1", "us-east1", "europe-west1", "asia-northeast1"],
                description="デプロイリージョン",
                group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        if not config.get("project_id"):
            errors["project_id"] = "Project ID is required"
        if not config.get("service_name"):
            errors["service_name"] = "Service name is required"
        return ValidationResult(valid=len(errors) == 0, errors=errors)


__all__ = ["GoogleCloudRunTarget"]
