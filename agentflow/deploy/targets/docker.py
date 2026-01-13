# -*- coding: utf-8 -*-
"""DockerTarget - Docker デプロイターゲット."""

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


class DockerTarget(BaseDeployTarget):
    """Docker デプロイターゲット.

    Docker イメージをビルドしてレジストリにプッシュします。
    """

    @property
    def name(self) -> str:
        return "Docker"

    @property
    def description(self) -> str:
        return "Build Docker image and push to registry"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Docker イメージをビルド＆プッシュ."""
        image_name = config.settings.get("image_name", "agentflow-app")
        tag = config.settings.get("tag", "latest")
        registry = config.settings.get("registry", "")
        push = config.settings.get("push", False)

        full_image_name = f"{registry}/{image_name}" if registry else image_name
        full_tag = f"{full_image_name}:{tag}"

        yield DeployEvent(
            type="progress",
            message="Preparing Docker build...",
            progress=10,
            phase="init",
        )

        # Dockerfile が存在するか確認
        dockerfile = source_path / "Dockerfile"
        if not dockerfile.exists():
            yield DeployEvent(
                type="error",
                message="Dockerfile not found in source directory",
            )
            return

        try:
            # Docker ビルド
            yield DeployEvent(
                type="progress",
                message=f"Building image: {full_tag}",
                progress=20,
                phase="build",
            )

            build_process = await asyncio.create_subprocess_exec(
                "docker", "build", "-t", full_tag, str(source_path),
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )

            stdout, stderr = await build_process.communicate()

            if build_process.returncode != 0:
                yield DeployEvent(
                    type="error",
                    message=f"Docker build failed: {stderr.decode()}",
                )
                return

            yield DeployEvent(
                type="progress",
                message="Build completed successfully",
                progress=70,
                phase="build_complete",
            )

            # レジストリにプッシュ
            if push and registry:
                yield DeployEvent(
                    type="progress",
                    message=f"Pushing to registry: {registry}",
                    progress=80,
                    phase="push",
                )

                # レジストリにログイン
                username = config.credentials.get("username")
                password = config.credentials.get("password")

                if username and password:
                    login_process = await asyncio.create_subprocess_exec(
                        "docker", "login", registry,
                        "-u", username,
                        "--password-stdin",
                        stdin=asyncio.subprocess.PIPE,
                        stdout=asyncio.subprocess.PIPE,
                        stderr=asyncio.subprocess.PIPE,
                    )
                    await login_process.communicate(input=password.encode())

                # プッシュ
                push_process = await asyncio.create_subprocess_exec(
                    "docker", "push", full_tag,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                )

                stdout, stderr = await push_process.communicate()

                if push_process.returncode != 0:
                    yield DeployEvent(
                        type="error",
                        message=f"Docker push failed: {stderr.decode()}",
                    )
                    return

                yield DeployEvent(
                    type="progress",
                    message="Push completed",
                    progress=100,
                    phase="complete",
                )

                yield DeployEvent(
                    type="success",
                    message=f"Successfully pushed to {full_tag}",
                    data={
                        "image": full_tag,
                        "registry": registry,
                    },
                )
            else:
                yield DeployEvent(
                    type="progress",
                    message="Build completed (push skipped)",
                    progress=100,
                    phase="complete",
                )

                yield DeployEvent(
                    type="success",
                    message=f"Successfully built: {full_tag}",
                    data={
                        "image": full_tag,
                    },
                )

        except FileNotFoundError:
            yield DeployEvent(
                type="error",
                message="Docker is not installed or not in PATH",
            )
        except Exception as e:
            logger.exception("Docker deployment failed")
            yield DeployEvent(
                type="error",
                message=f"Deployment failed: {e}",
            )

    def get_config_fields(self) -> list[ConfigField]:
        """Docker 用の設定フィールドを取得."""
        return [
            ConfigField(
                name="image_name",
                label="Image Name",
                type="string",
                required=True,
                description="Docker image name",
                placeholder="my-agentflow-app",
                group="settings",
            ),
            ConfigField(
                name="tag",
                label="Tag",
                type="string",
                required=False,
                default="latest",
                description="Image tag",
                group="settings",
            ),
            ConfigField(
                name="registry",
                label="Registry",
                type="string",
                required=False,
                description="Docker registry (e.g., docker.io, ghcr.io)",
                placeholder="docker.io/username",
                group="settings",
            ),
            ConfigField(
                name="push",
                label="Push to Registry",
                type="boolean",
                required=False,
                default=False,
                description="Push image to registry after build",
                group="settings",
            ),
            ConfigField(
                name="username",
                label="Registry Username",
                type="string",
                required=False,
                description="Registry username (for push)",
                group="credentials",
            ),
            ConfigField(
                name="password",
                label="Registry Password",
                type="password",
                required=False,
                description="Registry password or token",
                group="credentials",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        warnings: list[str] = []

        if not config.get("image_name"):
            errors["image_name"] = "Image name is required"

        push = config.get("push", False)
        registry = config.get("registry")

        if push:
            if not registry:
                errors["registry"] = "Registry is required when push is enabled"
            if not config.get("username"):
                warnings.append("Username recommended for registry push")

        return ValidationResult(
            valid=len(errors) == 0,
            errors=errors,
            warnings=warnings,
        )


__all__ = ["DockerTarget"]
