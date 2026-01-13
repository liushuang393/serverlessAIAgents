# -*- coding: utf-8 -*-
"""VercelTarget - Vercel デプロイターゲット."""

from __future__ import annotations

import asyncio
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


class VercelTarget(BaseDeployTarget):
    """Vercel デプロイターゲット.

    Vercel Serverless Functions にデプロイします。
    """

    @property
    def name(self) -> str:
        return "Vercel"

    @property
    def description(self) -> str:
        return "Deploy to Vercel Serverless Functions"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Vercel にデプロイ."""
        token = config.credentials.get("token")
        project_name = config.settings.get("project_name", "agentflow-app")

        if not token:
            yield DeployEvent(
                type="error",
                message="Vercel token is required",
            )
            return

        yield DeployEvent(
            type="progress",
            message="Initializing Vercel deployment...",
            progress=10,
            phase="init",
        )

        try:
            # Vercel API を使用してデプロイ
            import httpx

            async with httpx.AsyncClient() as client:
                yield DeployEvent(
                    type="progress",
                    message="Connecting to Vercel API...",
                    progress=20,
                    phase="connect",
                )

                # プロジェクトを取得または作成
                yield DeployEvent(
                    type="progress",
                    message=f"Setting up project: {project_name}",
                    progress=30,
                    phase="setup",
                )

                # ファイルを読み込み
                files_to_upload = []
                for file_path in source_path.rglob("*"):
                    if file_path.is_file():
                        rel_path = file_path.relative_to(source_path)
                        content = file_path.read_text(encoding="utf-8")
                        files_to_upload.append({
                            "file": str(rel_path),
                            "data": content,
                        })

                yield DeployEvent(
                    type="progress",
                    message=f"Uploading {len(files_to_upload)} files...",
                    progress=50,
                    phase="upload",
                )

                # デプロイメントを作成
                response = await client.post(
                    "https://api.vercel.com/v13/deployments",
                    headers={
                        "Authorization": f"Bearer {token}",
                        "Content-Type": "application/json",
                    },
                    json={
                        "name": project_name,
                        "files": files_to_upload,
                        "projectSettings": {
                            "framework": None,
                        },
                    },
                    timeout=120.0,
                )

                if response.status_code >= 400:
                    error_data = response.json()
                    error_msg = error_data.get("error", {}).get("message", "Unknown error")
                    yield DeployEvent(
                        type="error",
                        message=f"Vercel API error: {error_msg}",
                    )
                    return

                deploy_data = response.json()
                deployment_id = deploy_data.get("id", "")
                deploy_url = f"https://{deploy_data.get('url', '')}"

                yield DeployEvent(
                    type="progress",
                    message="Building deployment...",
                    progress=70,
                    phase="build",
                )

                # デプロイ完了を待機
                for _ in range(60):  # 最大60秒待機
                    await asyncio.sleep(2)

                    status_response = await client.get(
                        f"https://api.vercel.com/v13/deployments/{deployment_id}",
                        headers={"Authorization": f"Bearer {token}"},
                    )
                    status_data = status_response.json()
                    ready_state = status_data.get("readyState", "")

                    if ready_state == "READY":
                        yield DeployEvent(
                            type="progress",
                            message="Deployment ready!",
                            progress=100,
                            phase="complete",
                        )
                        yield DeployEvent(
                            type="success",
                            message=f"Successfully deployed to {deploy_url}",
                            data={
                                "deployment_id": deployment_id,
                                "url": deploy_url,
                            },
                        )
                        return
                    elif ready_state == "ERROR":
                        yield DeployEvent(
                            type="error",
                            message="Deployment failed",
                        )
                        return

                yield DeployEvent(
                    type="error",
                    message="Deployment timeout",
                )

        except ImportError:
            yield DeployEvent(
                type="error",
                message="httpx is required for Vercel deployment. Run: pip install httpx",
            )
        except Exception as e:
            logger.exception("Vercel deployment failed")
            yield DeployEvent(
                type="error",
                message=f"Deployment failed: {e}",
            )

    def get_config_fields(self) -> list[ConfigField]:
        """Vercel 用の設定フィールドを取得."""
        return [
            ConfigField(
                name="token",
                label="Vercel Token",
                type="password",
                required=True,
                description="Vercel API Token (https://vercel.com/account/tokens)",
                group="credentials",
            ),
            ConfigField(
                name="project_name",
                label="Project Name",
                type="string",
                required=True,
                description="Vercel project name",
                placeholder="my-agentflow-app",
                group="settings",
            ),
            ConfigField(
                name="team_id",
                label="Team ID",
                type="string",
                required=False,
                description="Vercel Team ID (for team projects)",
                group="settings",
            ),
            ConfigField(
                name="production",
                label="Production Deployment",
                type="boolean",
                required=False,
                default=False,
                description="Deploy to production environment",
                group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}

        if not config.get("token"):
            errors["token"] = "Vercel token is required"

        if not config.get("project_name"):
            errors["project_name"] = "Project name is required"

        return ValidationResult(
            valid=len(errors) == 0,
            errors=errors,
        )


__all__ = ["VercelTarget"]
