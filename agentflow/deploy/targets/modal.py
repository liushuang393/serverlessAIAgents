# -*- coding: utf-8 -*-
"""ModalTarget - Modal Labs デプロイターゲット.

Modal Labs への GPU/CPU ワークロードデプロイを実装します。
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


class ModalTarget(BaseDeployTarget):
    """Modal Labs デプロイターゲット."""

    @property
    def name(self) -> str:
        return "Modal"

    @property
    def description(self) -> str:
        return "Deploy GPU/CPU workloads to Modal Labs"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Modal にデプロイ."""
        app_name = config.settings.get("app_name", "agentflow-app")
        gpu = config.settings.get("gpu")
        memory = config.settings.get("memory", 1024)

        yield DeployEvent(
            type="progress",
            message="Initializing Modal deployment...",
            progress=10,
            phase="init",
        )

        try:
            import modal

            yield DeployEvent(
                type="progress",
                message=f"Creating Modal app: {app_name}",
                progress=30,
                phase="setup",
            )

            # Modal アプリを動的に作成
            app = modal.App(name=app_name)

            # イメージ設定
            image = modal.Image.debian_slim(python_version="3.11").pip_install(
                "fastapi", "uvicorn", "agentflow"
            )

            if gpu:
                image = image.pip_install("torch", "transformers")

            yield DeployEvent(
                type="progress",
                message="Deploying to Modal...",
                progress=60,
                phase="deploy",
            )

            # デプロイ実行 (CLI 経由)
            import subprocess

            modal_file = source_path / "modal_app.py"
            if not modal_file.exists():
                # デフォルトの Modal アプリファイルを生成
                modal_content = self._generate_modal_app(app_name, gpu, memory)
                modal_file.write_text(modal_content, encoding="utf-8")

            result = subprocess.run(
                ["modal", "deploy", str(modal_file)],
                capture_output=True,
                text=True,
                cwd=str(source_path),
            )

            if result.returncode != 0:
                yield DeployEvent(
                    type="error",
                    message=f"Modal deploy failed: {result.stderr}",
                )
                return

            # URL を抽出
            app_url = f"https://{app_name}--web.modal.run"

            yield DeployEvent(
                type="success",
                message=f"Successfully deployed to Modal: {app_url}",
                data={"app_name": app_name, "url": app_url, "gpu": gpu},
            )

        except ImportError:
            yield DeployEvent(
                type="error",
                message="modal is required. Run: pip install modal",
            )
        except Exception as e:
            logger.exception("Modal deployment failed")
            yield DeployEvent(type="error", message=f"Deployment failed: {e}")

    def _generate_modal_app(self, app_name: str, gpu: str | None, memory: int) -> str:
        """Modal アプリファイルを生成."""
        gpu_config = f'gpu="{gpu}"' if gpu else ""
        return f'''# -*- coding: utf-8 -*-
"""Modal App - {app_name}"""
import modal

app = modal.App(name="{app_name}")

image = modal.Image.debian_slim(python_version="3.11").pip_install(
    "fastapi", "uvicorn", "agentflow"
)

@app.function(image=image, memory={memory}, {gpu_config})
@modal.web_endpoint()
def web():
    from fastapi import FastAPI
    api = FastAPI(title="{app_name}")

    @api.get("/")
    def root():
        return {{"message": "AgentFlow on Modal"}}

    @api.get("/health")
    def health():
        return {{"status": "healthy"}}

    return api
'''

    def get_config_fields(self) -> list[ConfigField]:
        """設定フィールドを取得."""
        return [
            ConfigField(
                name="app_name", label="App Name", type="string",
                required=True, placeholder="my-agentflow-app", group="settings",
            ),
            ConfigField(
                name="gpu", label="GPU Type", type="select",
                required=False, default=None,
                options=[None, "T4", "A10G", "A100"],
                description="GPU タイプ (オプション)", group="settings",
            ),
            ConfigField(
                name="memory", label="Memory (MB)", type="number",
                required=False, default=1024, description="メモリサイズ", group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        if not config.get("app_name"):
            errors["app_name"] = "App name is required"
        return ValidationResult(valid=len(errors) == 0, errors=errors)


__all__ = ["ModalTarget"]

