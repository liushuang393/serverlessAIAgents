# -*- coding: utf-8 -*-
"""AWSLambdaTarget - AWS Lambda デプロイターゲット."""

from __future__ import annotations

import asyncio
import logging
import zipfile
from collections.abc import AsyncIterator
from io import BytesIO
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


class AWSLambdaTarget(BaseDeployTarget):
    """AWS Lambda デプロイターゲット.

    AWS Lambda にデプロイします。
    """

    @property
    def name(self) -> str:
        return "AWS Lambda"

    @property
    def description(self) -> str:
        return "Deploy to AWS Lambda Functions"

    async def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """AWS Lambda にデプロイ."""
        function_name = config.settings.get("function_name", "agentflow-function")
        region = config.settings.get("region", "us-east-1")
        runtime = config.settings.get("runtime", "python3.11")
        handler = config.settings.get("handler", "app.handler")
        memory_size = config.settings.get("memory_size", 256)
        timeout = config.settings.get("timeout", 30)

        access_key = config.credentials.get("aws_access_key_id")
        secret_key = config.credentials.get("aws_secret_access_key")

        if not access_key or not secret_key:
            yield DeployEvent(
                type="error",
                message="AWS credentials are required",
            )
            return

        yield DeployEvent(
            type="progress",
            message="Initializing AWS Lambda deployment...",
            progress=10,
            phase="init",
        )

        try:
            import boto3
            from botocore.exceptions import ClientError

            # AWS クライアントを作成
            lambda_client = boto3.client(
                "lambda",
                region_name=region,
                aws_access_key_id=access_key,
                aws_secret_access_key=secret_key,
            )

            yield DeployEvent(
                type="progress",
                message="Creating deployment package...",
                progress=30,
                phase="package",
            )

            # ZIP パッケージを作成
            zip_buffer = BytesIO()
            with zipfile.ZipFile(zip_buffer, "w", zipfile.ZIP_DEFLATED) as zf:
                for file_path in source_path.rglob("*"):
                    if file_path.is_file():
                        rel_path = file_path.relative_to(source_path)
                        zf.write(file_path, rel_path)

            zip_buffer.seek(0)
            zip_content = zip_buffer.read()

            yield DeployEvent(
                type="progress",
                message=f"Deploying function: {function_name}",
                progress=50,
                phase="deploy",
            )

            # Lambda 関数を作成または更新
            try:
                # 既存の関数を更新
                response = lambda_client.update_function_code(
                    FunctionName=function_name,
                    ZipFile=zip_content,
                )
                action = "updated"
            except ClientError as e:
                if e.response["Error"]["Code"] == "ResourceNotFoundException":
                    # 新しい関数を作成
                    role_arn = config.settings.get("role_arn")
                    if not role_arn:
                        yield DeployEvent(
                            type="error",
                            message="Lambda role ARN is required for new function",
                        )
                        return

                    response = lambda_client.create_function(
                        FunctionName=function_name,
                        Runtime=runtime,
                        Role=role_arn,
                        Handler=handler,
                        Code={"ZipFile": zip_content},
                        MemorySize=memory_size,
                        Timeout=timeout,
                        Environment={
                            "Variables": config.env_vars,
                        },
                    )
                    action = "created"
                else:
                    raise

            function_arn = response.get("FunctionArn", "")

            yield DeployEvent(
                type="progress",
                message=f"Function {action} successfully",
                progress=90,
                phase="complete",
            )

            # 設定を更新
            if action == "updated":
                lambda_client.update_function_configuration(
                    FunctionName=function_name,
                    MemorySize=memory_size,
                    Timeout=timeout,
                    Environment={
                        "Variables": config.env_vars,
                    },
                )

            yield DeployEvent(
                type="progress",
                message="Deployment complete",
                progress=100,
                phase="complete",
            )

            yield DeployEvent(
                type="success",
                message=f"Successfully deployed to AWS Lambda: {function_name}",
                data={
                    "function_name": function_name,
                    "function_arn": function_arn,
                    "region": region,
                },
            )

        except ImportError:
            yield DeployEvent(
                type="error",
                message="boto3 is required for AWS deployment. Run: pip install boto3",
            )
        except Exception as e:
            logger.exception("AWS Lambda deployment failed")
            yield DeployEvent(
                type="error",
                message=f"Deployment failed: {e}",
            )

    def get_config_fields(self) -> list[ConfigField]:
        """AWS Lambda 用の設定フィールドを取得."""
        return [
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
                placeholder="my-agentflow-function",
                group="settings",
            ),
            ConfigField(
                name="region",
                label="Region",
                type="select",
                required=True,
                default="us-east-1",
                options=[
                    "us-east-1", "us-west-2", "eu-west-1", "eu-central-1",
                    "ap-northeast-1", "ap-southeast-1",
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
                name="handler",
                label="Handler",
                type="string",
                required=False,
                default="app.handler",
                description="Function handler",
                group="settings",
            ),
            ConfigField(
                name="memory_size",
                label="Memory (MB)",
                type="number",
                required=False,
                default=256,
                description="Function memory size",
                group="settings",
            ),
            ConfigField(
                name="timeout",
                label="Timeout (seconds)",
                type="number",
                required=False,
                default=30,
                description="Function timeout",
                group="settings",
            ),
            ConfigField(
                name="role_arn",
                label="IAM Role ARN",
                type="string",
                required=False,
                description="IAM execution role ARN (required for new functions)",
                placeholder="arn:aws:iam::123456789012:role/lambda-role",
                group="settings",
            ),
        ]

    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証."""
        errors: dict[str, str] = {}

        if not config.get("aws_access_key_id"):
            errors["aws_access_key_id"] = "AWS Access Key ID is required"

        if not config.get("aws_secret_access_key"):
            errors["aws_secret_access_key"] = "AWS Secret Access Key is required"

        if not config.get("function_name"):
            errors["function_name"] = "Function name is required"

        return ValidationResult(
            valid=len(errors) == 0,
            errors=errors,
        )


__all__ = ["AWSLambdaTarget"]
