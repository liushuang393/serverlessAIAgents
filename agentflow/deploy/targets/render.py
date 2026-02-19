"""RenderTarget - Render デプロイターゲット.

Render への Web サービスデプロイを実装します。
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


class RenderTarget(BaseDeployTarget):
    """Render デプロイターゲット."""

    @property
    def name(self) -> str:
        return "Render"

    @property
    def description(self) -> str:
        return "Deploy web services to Render"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Render にデプロイ."""
        api_key = config.credentials.get("render_api_key")
        service_id = config.settings.get("service_id")
        service_name = config.settings.get("service_name", "agentflow-app")

        if not api_key:
            yield DeployEvent(type="error", message="Render API key is required")
            return

        yield DeployEvent(
            type="progress",
            message="Initializing Render deployment...",
            progress=10,
            phase="init",
        )

        try:
            import httpx

            headers = {
                "Authorization": f"Bearer {api_key}",
                "Content-Type": "application/json",
            }
            base_url = "https://api.render.com/v1"

            async with httpx.AsyncClient() as client:
                if service_id:
                    # 既存サービスをデプロイ
                    yield DeployEvent(
                        type="progress",
                        message=f"Triggering deploy for service: {service_id}",
                        progress=30,
                        phase="deploy",
                    )

                    response = await client.post(
                        f"{base_url}/services/{service_id}/deploys",
                        headers=headers,
                        json={"clearCache": "do_not_clear"},
                    )

                    if response.status_code >= 400:
                        yield DeployEvent(
                            type="error",
                            message=f"Deploy failed: {response.text}",
                        )
                        return

                    deploy_data = response.json()
                    deploy_id = deploy_data.get("id")

                    yield DeployEvent(
                        type="progress",
                        message="Waiting for deployment...",
                        progress=60,
                        phase="waiting",
                    )

                    # デプロイ完了を待機
                    import asyncio

                    for _ in range(60):
                        await asyncio.sleep(5)
                        status_response = await client.get(
                            f"{base_url}/services/{service_id}/deploys/{deploy_id}",
                            headers=headers,
                        )
                        status = status_response.json().get("status")
                        if status == "live":
                            break
                        elif status in ["build_failed", "canceled"]:
                            yield DeployEvent(
                                type="error",
                                message=f"Deployment {status}",
                            )
                            return

                    # サービス URL を取得
                    svc_response = await client.get(
                        f"{base_url}/services/{service_id}",
                        headers=headers,
                    )
                    service_url = svc_response.json().get("serviceDetails", {}).get("url")

                    yield DeployEvent(
                        type="success",
                        message=f"Successfully deployed to {service_url}",
                        data={"service_id": service_id, "url": service_url},
                    )
                else:
                    # Deploy Hook を使用
                    deploy_hook = config.settings.get("deploy_hook")
                    if deploy_hook:
                        response = await client.post(deploy_hook)
                        yield DeployEvent(
                            type="success",
                            message="Deploy hook triggered",
                            data={"service_name": service_name},
                        )
                    else:
                        yield DeployEvent(
                            type="error",
                            message="Service ID or Deploy Hook is required",
                        )

        except ImportError:
            yield DeployEvent(
                type="error",
                message="httpx is required. Run: pip install httpx",
            )
        except Exception as e:
            logger.exception("Render deployment failed")
            yield DeployEvent(type="error", message=f"Deployment failed: {e}")

    def get_config_fields(self) -> list[ConfigField]:
        """設定フィールドを取得."""
        return [
            ConfigField(
                name="render_api_key",
                label="Render API Key",
                type="password",
                required=True,
                description="Render API キー",
                group="credentials",
            ),
            ConfigField(
                name="service_id",
                label="Service ID",
                type="string",
                required=False,
                description="既存サービス ID",
                group="settings",
            ),
            ConfigField(
                name="deploy_hook",
                label="Deploy Hook URL",
                type="string",
                required=False,
                description="Render Deploy Hook URL",
                group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        if not config.get("render_api_key"):
            errors["render_api_key"] = "Render API key is required"
        return ValidationResult(valid=len(errors) == 0, errors=errors)


__all__ = ["RenderTarget"]
