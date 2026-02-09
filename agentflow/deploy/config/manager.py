"""ConfigManager - 設定管理器実装.

IConfigManager インターフェースを実装し、
デプロイ設定のテンプレートとユーザー設定を管理します。
"""

from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import Any

from agentflow.core.interfaces import (
    ConfigField,
    ConfigTemplate,
    DeployTarget,
    IConfigManager,
    ValidationResult,
)


logger = logging.getLogger(__name__)


class ConfigManager(IConfigManager):
    """設定管理器.

    デプロイ設定のテンプレート管理とユーザー設定の処理を行います。
    """

    def __init__(self, config_dir: Path | None = None) -> None:
        """初期化.

        Args:
            config_dir: 設定保存ディレクトリ
        """
        if config_dir is None:
            config_dir = Path.home() / ".agentflow" / "deploy_configs"
        self._config_dir = config_dir
        self._config_dir.mkdir(parents=True, exist_ok=True)

        # テンプレートを初期化
        self._templates = self._initialize_templates()

    def _initialize_templates(self) -> dict[DeployTarget, ConfigTemplate]:
        """テンプレートを初期化."""
        return {
            DeployTarget.VERCEL: ConfigTemplate(
                target=DeployTarget.VERCEL,
                name="Vercel Deployment",
                description="Deploy to Vercel Serverless Functions",
                fields=[
                    ConfigField(
                        name="token",
                        label="Vercel Token",
                        type="password",
                        required=True,
                        description="Vercel API Token",
                        group="credentials",
                    ),
                    ConfigField(
                        name="project_name",
                        label="Project Name",
                        type="string",
                        required=True,
                        description="Vercel project name",
                        group="settings",
                    ),
                    ConfigField(
                        name="team_id",
                        label="Team ID",
                        type="string",
                        required=False,
                        description="Vercel Team ID (optional)",
                        group="settings",
                    ),
                    ConfigField(
                        name="production",
                        label="Production",
                        type="boolean",
                        required=False,
                        default=False,
                        description="Deploy to production",
                        group="settings",
                    ),
                ],
                defaults={
                    "production": False,
                },
            ),
            DeployTarget.DOCKER: ConfigTemplate(
                target=DeployTarget.DOCKER,
                name="Docker Build",
                description="Build and push Docker image",
                fields=[
                    ConfigField(
                        name="image_name",
                        label="Image Name",
                        type="string",
                        required=True,
                        description="Docker image name",
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
                        description="Docker registry URL",
                        group="settings",
                    ),
                    ConfigField(
                        name="push",
                        label="Push to Registry",
                        type="boolean",
                        required=False,
                        default=False,
                        description="Push image after build",
                        group="settings",
                    ),
                    ConfigField(
                        name="username",
                        label="Registry Username",
                        type="string",
                        required=False,
                        description="Registry username",
                        group="credentials",
                    ),
                    ConfigField(
                        name="password",
                        label="Registry Password",
                        type="password",
                        required=False,
                        description="Registry password",
                        group="credentials",
                    ),
                ],
                defaults={
                    "tag": "latest",
                    "push": False,
                },
            ),
            DeployTarget.AWS_LAMBDA: ConfigTemplate(
                target=DeployTarget.AWS_LAMBDA,
                name="AWS Lambda",
                description="Deploy to AWS Lambda",
                fields=[
                    ConfigField(
                        name="aws_access_key_id",
                        label="AWS Access Key ID",
                        type="string",
                        required=True,
                        description="AWS Access Key ID",
                        group="credentials",
                    ),
                    ConfigField(
                        name="aws_secret_access_key",
                        label="AWS Secret Access Key",
                        type="password",
                        required=True,
                        description="AWS Secret Access Key",
                        group="credentials",
                    ),
                    ConfigField(
                        name="function_name",
                        label="Function Name",
                        type="string",
                        required=True,
                        description="Lambda function name",
                        group="settings",
                    ),
                    ConfigField(
                        name="region",
                        label="Region",
                        type="select",
                        required=True,
                        default="us-east-1",
                        options=[
                            "us-east-1", "us-west-2", "eu-west-1",
                            "eu-central-1", "ap-northeast-1", "ap-southeast-1",
                        ],
                        description="AWS region",
                        group="settings",
                    ),
                    ConfigField(
                        name="runtime",
                        label="Runtime",
                        type="select",
                        required=True,
                        default="python3.11",
                        options=["python3.11", "python3.10", "python3.9", "nodejs18.x"],
                        description="Lambda runtime",
                        group="settings",
                    ),
                    ConfigField(
                        name="memory_size",
                        label="Memory (MB)",
                        type="number",
                        required=False,
                        default=256,
                        description="Memory size",
                        group="settings",
                    ),
                    ConfigField(
                        name="timeout",
                        label="Timeout (s)",
                        type="number",
                        required=False,
                        default=30,
                        description="Function timeout",
                        group="settings",
                    ),
                ],
                defaults={
                    "region": "us-east-1",
                    "runtime": "python3.11",
                    "memory_size": 256,
                    "timeout": 30,
                },
            ),
            DeployTarget.GITHUB_ACTIONS: ConfigTemplate(
                target=DeployTarget.GITHUB_ACTIONS,
                name="GitHub Actions",
                description="Generate GitHub Actions workflow",
                fields=[
                    ConfigField(
                        name="workflow_name",
                        label="Workflow Name",
                        type="string",
                        required=False,
                        default="Deploy",
                        description="Workflow name",
                        group="settings",
                    ),
                    ConfigField(
                        name="deploy_target",
                        label="Deploy Target",
                        type="select",
                        required=True,
                        default="vercel",
                        options=["vercel", "docker", "aws", "generic"],
                        description="Target platform for deployment",
                        group="settings",
                    ),
                    ConfigField(
                        name="branch",
                        label="Trigger Branch",
                        type="string",
                        required=False,
                        default="main",
                        description="Branch that triggers deployment",
                        group="settings",
                    ),
                ],
                defaults={
                    "workflow_name": "Deploy",
                    "deploy_target": "vercel",
                    "branch": "main",
                },
            ),
        }

    async def get_template(
        self,
        target: DeployTarget,
    ) -> ConfigTemplate:
        """プラットフォーム設定テンプレートを取得.

        Args:
            target: デプロイターゲット

        Returns:
            設定テンプレート
        """
        template = self._templates.get(target)
        if template is None:
            msg = f"No template for target: {target}"
            raise ValueError(msg)
        return template

    async def get_required_fields(
        self,
        target: DeployTarget,
        current_config: dict[str, Any] | None = None,
    ) -> list[ConfigField]:
        """未入力の必須フィールドを取得.

        Args:
            target: デプロイターゲット
            current_config: 現在の設定

        Returns:
            入力が必要なフィールドのリスト
        """
        template = await self.get_template(target)
        current_config = current_config or {}

        required_fields = []
        for field in template.fields:
            # 必須フィールドで未入力の場合、または全フィールドを返す場合
            if current_config.get(field.name) is None:
                # デフォルト値がある場合はスキップしない（UIで表示）
                required_fields.append(field)

        return required_fields

    async def merge_config(
        self,
        template: ConfigTemplate,
        user_config: dict[str, Any],
    ) -> dict[str, Any]:
        """テンプレートとユーザー設定をマージ.

        Args:
            template: 設定テンプレート
            user_config: ユーザー入力の設定

        Returns:
            マージされた設定
        """
        result = dict(template.defaults)
        result.update(user_config)
        return result

    async def validate(
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
        template = await self.get_template(target)
        errors: dict[str, str] = {}
        warnings: list[str] = []

        for field in template.fields:
            value = config.get(field.name)

            # 必須チェック
            if field.required and (value is None or value == ""):
                errors[field.name] = f"{field.label} is required"
                continue

            # 型チェック
            if value is not None:
                if field.type == "number" and not isinstance(value, (int, float)):
                    try:
                        int(value)
                    except (ValueError, TypeError):
                        errors[field.name] = f"{field.label} must be a number"

                if field.type == "boolean" and not isinstance(value, bool):
                    if value not in ("true", "false", True, False):
                        errors[field.name] = f"{field.label} must be a boolean"

                if field.type == "select" and field.options:
                    if value not in field.options:
                        errors[field.name] = f"{field.label} must be one of: {', '.join(field.options)}"

        return ValidationResult(
            valid=len(errors) == 0,
            errors=errors,
            warnings=warnings,
        )

    async def save_config(
        self,
        name: str,
        target: DeployTarget,
        config: dict[str, Any],
    ) -> None:
        """設定を保存.

        Args:
            name: 設定名
            target: デプロイターゲット
            config: 保存する設定
        """
        # 機密情報を除外（credentials グループ）
        template = await self.get_template(target)
        credential_fields = {
            f.name for f in template.fields if f.group == "credentials"
        }

        safe_config = {
            k: v for k, v in config.items()
            if k not in credential_fields
        }

        config_data = {
            "name": name,
            "target": target.value,
            "config": safe_config,
        }

        config_file = self._config_dir / f"{name}.json"
        config_file.write_text(
            json.dumps(config_data, indent=2),
            encoding="utf-8",
        )
        logger.info(f"Saved config: {config_file}")

    async def load_config(
        self,
        name: str,
    ) -> dict[str, Any] | None:
        """設定を読み込み.

        Args:
            name: 設定名

        Returns:
            設定、または None
        """
        config_file = self._config_dir / f"{name}.json"
        if not config_file.exists():
            return None

        try:
            data = json.loads(config_file.read_text(encoding="utf-8"))
            return data.get("config", {})
        except Exception as e:
            logger.exception(f"Failed to load config {name}: {e}")
            return None

    async def list_configs(
        self,
        target: DeployTarget | None = None,
    ) -> list[str]:
        """保存された設定一覧を取得.

        Args:
            target: フィルタするターゲット

        Returns:
            設定名のリスト
        """
        configs = []
        for config_file in self._config_dir.glob("*.json"):
            try:
                data = json.loads(config_file.read_text(encoding="utf-8"))
                if target is None or data.get("target") == target.value:
                    configs.append(data.get("name", config_file.stem))
            except Exception:
                continue
        return configs


__all__ = ["ConfigManager"]
