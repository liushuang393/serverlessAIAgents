"""HuggingFaceSpacesTarget - Hugging Face Spaces デプロイターゲット.

Hugging Face Spaces への AI アプリケーションデプロイを実装します。
"""

from __future__ import annotations

import logging
import shutil
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING, Any

from kernel.core.interfaces import (
    ConfigField,
    DeployConfig,
    DeployEvent,
    ValidationResult,
)
from control_plane.deploy.targets.base import BaseDeployTarget


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


class HuggingFaceSpacesTarget(BaseDeployTarget):
    """Hugging Face Spaces デプロイターゲット."""

    @property
    def name(self) -> str:
        return "Hugging Face Spaces"

    @property
    def description(self) -> str:
        return "Deploy AI applications to Hugging Face Spaces"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """Hugging Face Spaces にデプロイ."""
        hf_token = config.credentials.get("hf_token")
        space_name = config.settings.get("space_name")
        space_sdk = config.settings.get("sdk", "gradio")
        private = config.settings.get("private", False)

        if not hf_token or not space_name:
            yield DeployEvent(
                type="error",
                message="Hugging Face token and space name are required",
            )
            return

        yield DeployEvent(
            type="progress",
            message="Initializing Hugging Face Spaces deployment...",
            progress=10,
            phase="init",
        )

        try:
            from huggingface_hub import HfApi, create_repo

            api = HfApi(token=hf_token)

            yield DeployEvent(
                type="progress",
                message=f"Creating/updating Space: {space_name}",
                progress=20,
                phase="setup",
            )

            # Space リポジトリを作成
            create_repo(
                repo_id=space_name,
                repo_type="space",
                space_sdk=space_sdk,
                private=private,
                exist_ok=True,
                token=hf_token,
            )

            yield DeployEvent(
                type="progress",
                message="Uploading files...",
                progress=50,
                phase="upload",
            )

            # README.md (Space 設定) を生成
            readme_content = f"""---
title: {space_name}
emoji: 🤖
colorFrom: blue
colorTo: purple
sdk: {space_sdk}
sdk_version: "4.0"
app_file: app.py
pinned: false
---

# {space_name}

Deployed with BizCore Framework.
"""
            # 一時ディレクトリにファイルを準備
            with tempfile.TemporaryDirectory() as tmp_dir:
                tmp_path = Path(tmp_dir)

                # ソースファイルをコピー
                for item in source_path.iterdir():
                    if item.is_file():
                        shutil.copy2(item, tmp_path / item.name)
                    elif item.is_dir() and item.name not in ["__pycache__", ".git"]:
                        shutil.copytree(item, tmp_path / item.name)

                # README.md を作成
                (tmp_path / "README.md").write_text(readme_content, encoding="utf-8")

                # アップロード
                api.upload_folder(
                    folder_path=str(tmp_path),
                    repo_id=space_name,
                    repo_type="space",
                    token=hf_token,
                )

            space_url = f"https://huggingface.co/spaces/{space_name}"

            yield DeployEvent(
                type="success",
                message=f"Successfully deployed to {space_url}",
                data={"space_name": space_name, "url": space_url, "sdk": space_sdk},
            )

        except ImportError:
            yield DeployEvent(
                type="error",
                message="huggingface_hub is required. Run: pip install huggingface_hub",
            )
        except Exception as e:
            logger.exception("Hugging Face Spaces deployment failed")
            yield DeployEvent(type="error", message=f"Deployment failed: {e}")

    def get_config_fields(self) -> list[ConfigField]:
        """設定フィールドを取得."""
        return [
            ConfigField(
                name="hf_token",
                label="HF Token",
                type="password",
                required=True,
                description="Hugging Face API トークン",
                group="credentials",
            ),
            ConfigField(
                name="space_name",
                label="Space Name",
                type="string",
                required=True,
                placeholder="username/my-space",
                group="settings",
            ),
            ConfigField(
                name="sdk",
                label="SDK",
                type="select",
                required=False,
                default="gradio",
                options=["gradio", "streamlit", "docker", "static"],
                description="Space SDK タイプ",
                group="settings",
            ),
            ConfigField(
                name="private",
                label="Private",
                type="boolean",
                required=False,
                default=False,
                description="プライベート Space",
                group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        if not config.get("hf_token"):
            errors["hf_token"] = "Hugging Face token is required"
        if not config.get("space_name"):
            errors["space_name"] = "Space name is required"
        return ValidationResult(valid=len(errors) == 0, errors=errors)


__all__ = ["HuggingFaceSpacesTarget"]
