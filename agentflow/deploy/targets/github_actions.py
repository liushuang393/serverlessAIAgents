"""GitHubActionsTarget - GitHub Actions デプロイターゲット."""

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


class GitHubActionsTarget(BaseDeployTarget):
    """GitHub Actions デプロイターゲット.

    GitHub Actions CI/CD ワークフローを生成します。
    """

    @property
    def name(self) -> str:
        return "GitHub Actions"

    @property
    def description(self) -> str:
        return "Generate GitHub Actions CI/CD workflow"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """GitHub Actions ワークフローを生成."""
        workflow_name = config.settings.get("workflow_name", "Deploy")
        deploy_target = config.settings.get("deploy_target", "vercel")
        branch = config.settings.get("branch", "main")

        yield DeployEvent(
            type="progress",
            message="Generating GitHub Actions workflow...",
            progress=20,
            phase="generate",
        )

        try:
            # .github/workflows ディレクトリを作成
            workflows_dir = source_path / ".github" / "workflows"
            workflows_dir.mkdir(parents=True, exist_ok=True)

            # ワークフローファイルを生成
            workflow_content = self._generate_workflow(
                workflow_name=workflow_name,
                deploy_target=deploy_target,
                branch=branch,
                config=config,
            )

            workflow_file = workflows_dir / "deploy.yml"
            workflow_file.write_text(workflow_content, encoding="utf-8")

            yield DeployEvent(
                type="progress",
                message=f"Created workflow: {workflow_file}",
                progress=80,
                phase="write",
            )

            # シークレット設定のヒントを生成
            secrets_hint = self._generate_secrets_hint(deploy_target, config)

            yield DeployEvent(
                type="progress",
                message="Workflow generated successfully",
                progress=100,
                phase="complete",
            )

            yield DeployEvent(
                type="success",
                message="GitHub Actions workflow generated",
                data={
                    "workflow_file": str(workflow_file.relative_to(source_path)),
                    "secrets_required": secrets_hint,
                },
            )

        except Exception as e:
            logger.exception("GitHub Actions generation failed")
            yield DeployEvent(
                type="error",
                message=f"Generation failed: {e}",
            )

    def _generate_workflow(
        self,
        workflow_name: str,
        deploy_target: str,
        branch: str,
        config: DeployConfig,
    ) -> str:
        """ワークフローファイルを生成."""
        if deploy_target == "vercel":
            return self._generate_vercel_workflow(workflow_name, branch)
        if deploy_target == "docker":
            return self._generate_docker_workflow(workflow_name, branch, config)
        if deploy_target == "aws":
            return self._generate_aws_workflow(workflow_name, branch)
        return self._generate_generic_workflow(workflow_name, branch)

    def _generate_vercel_workflow(self, name: str, branch: str) -> str:
        """Vercel デプロイワークフローを生成."""
        return f"""name: {name}

on:
  push:
    branches: [{branch}]
  pull_request:
    branches: [{branch}]

jobs:
  deploy:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: '20'
          cache: 'npm'

      - name: Install Dependencies
        run: npm ci

      - name: Build
        run: npm run build

      - name: Deploy to Vercel
        uses: amondnet/vercel-action@v25
        with:
          vercel-token: ${{{{ secrets.VERCEL_TOKEN }}}}
          vercel-org-id: ${{{{ secrets.VERCEL_ORG_ID }}}}
          vercel-project-id: ${{{{ secrets.VERCEL_PROJECT_ID }}}}
          vercel-args: '--prod'
"""

    def _generate_docker_workflow(self, name: str, branch: str, config: DeployConfig) -> str:
        """Docker デプロイワークフローを生成."""
        registry = config.settings.get("registry", "ghcr.io")
        config.settings.get("image_name", "agentflow-app")

        return f"""name: {name}

on:
  push:
    branches: [{branch}]
    tags: ['v*']

env:
  REGISTRY: {registry}
  IMAGE_NAME: ${{{{ github.repository }}}}

jobs:
  build-and-push:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Login to Container Registry
        uses: docker/login-action@v3
        with:
          registry: ${{{{ env.REGISTRY }}}}
          username: ${{{{ github.actor }}}}
          password: ${{{{ secrets.GITHUB_TOKEN }}}}

      - name: Extract metadata
        id: meta
        uses: docker/metadata-action@v5
        with:
          images: ${{{{ env.REGISTRY }}}}/${{{{ env.IMAGE_NAME }}}}

      - name: Build and Push
        uses: docker/build-push-action@v5
        with:
          context: .
          push: true
          tags: ${{{{ steps.meta.outputs.tags }}}}
          labels: ${{{{ steps.meta.outputs.labels }}}}
          cache-from: type=gha
          cache-to: type=gha,mode=max
"""

    def _generate_aws_workflow(self, name: str, branch: str) -> str:
        """AWS Lambda デプロイワークフローを生成."""
        return f"""name: {name}

on:
  push:
    branches: [{branch}]

jobs:
  deploy:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'

      - name: Install Dependencies
        run: |
          python -m pip install --upgrade pip
          pip install -r requirements.txt -t ./package

      - name: Package Lambda
        run: |
          cd package
          zip -r ../lambda.zip .
          cd ..
          zip -g lambda.zip app.py

      - name: Configure AWS Credentials
        uses: aws-actions/configure-aws-credentials@v4
        with:
          aws-access-key-id: ${{{{ secrets.AWS_ACCESS_KEY_ID }}}}
          aws-secret-access-key: ${{{{ secrets.AWS_SECRET_ACCESS_KEY }}}}
          aws-region: ${{{{ secrets.AWS_REGION }}}}

      - name: Deploy to Lambda
        run: |
          aws lambda update-function-code \\
            --function-name ${{{{ secrets.LAMBDA_FUNCTION_NAME }}}} \\
            --zip-file fileb://lambda.zip
"""

    def _generate_generic_workflow(self, name: str, branch: str) -> str:
        """汎用ワークフローを生成."""
        return f"""name: {name}

on:
  push:
    branches: [{branch}]
  pull_request:
    branches: [{branch}]

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout
        uses: actions/checkout@v4

      - name: Setup Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.11'

      - name: Install Dependencies
        run: |
          python -m pip install --upgrade pip
          pip install -r requirements.txt

      - name: Run Tests
        run: pytest

      - name: Build
        run: |
          echo "Add your build commands here"
"""

    def _generate_secrets_hint(
        self,
        deploy_target: str,
        config: DeployConfig,
    ) -> list[str]:
        """必要なシークレットのヒントを生成."""
        secrets_map = {
            "vercel": ["VERCEL_TOKEN", "VERCEL_ORG_ID", "VERCEL_PROJECT_ID"],
            "docker": ["GITHUB_TOKEN (automatic)"],
            "aws": [
                "AWS_ACCESS_KEY_ID",
                "AWS_SECRET_ACCESS_KEY",
                "AWS_REGION",
                "LAMBDA_FUNCTION_NAME",
            ],
        }
        return secrets_map.get(deploy_target, [])

    def get_config_fields(self) -> list[ConfigField]:
        """GitHub Actions 用の設定フィールドを取得."""
        return [
            ConfigField(
                name="workflow_name",
                label="Workflow Name",
                type="string",
                required=False,
                default="Deploy",
                description="GitHub Actions workflow name",
                group="settings",
            ),
            ConfigField(
                name="deploy_target",
                label="Deploy Target",
                type="select",
                required=True,
                default="vercel",
                options=["vercel", "docker", "aws", "generic"],
                description="Deployment target for the workflow",
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
            ConfigField(
                name="include_tests",
                label="Include Tests",
                type="boolean",
                required=False,
                default=True,
                description="Run tests before deployment",
                group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}
        warnings: list[str] = []

        deploy_target = config.get("deploy_target", "generic")
        if deploy_target not in ["vercel", "docker", "aws", "generic"]:
            errors["deploy_target"] = f"Invalid deploy target: {deploy_target}"

        warnings.append("Remember to configure repository secrets for the selected deploy target")

        return ValidationResult(
            valid=len(errors) == 0,
            errors=errors,
            warnings=warnings,
        )


__all__ = ["GitHubActionsTarget"]
