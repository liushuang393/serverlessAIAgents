"""Serverless テンプレート生成モジュール.

Vercel、AWS Lambda のデプロイ設定を生成します。

特徴:
- Vercel 設定（vercel.json）
- AWS Lambda 設定（serverless.yml）
- 環境変数設定
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


logger = logging.getLogger(__name__)


@dataclass
class ServerlessConfig:
    """Serverless 設定.

    Attributes:
        app_name: アプリケーション名
        python_version: Python バージョン
        entry_point: エントリーポイント
        region: デプロイリージョン
        memory_size: メモリサイズ（MB）
        timeout: タイムアウト（秒）
        env_vars: 環境変数
    """

    app_name: str = "agentflow-app"
    python_version: str = "3.13"
    entry_point: str = "main.handler"
    region: str = "us-east-1"
    memory_size: int = 512
    timeout: int = 30
    env_vars: dict[str, str] = field(default_factory=dict)


VERCEL_CONFIG_TEMPLATE = {
    "$schema": "https://openapi.vercel.sh/vercel.json",
    "version": 2,
    "builds": [
        {
            "src": "main.py",
            "use": "@vercel/python",
            "config": {"maxLambdaSize": "50mb"},
        }
    ],
    "routes": [
        {"src": "/api/(.*)", "dest": "main.py"},
        {"src": "/(.*)", "dest": "main.py"},
    ],
    "env": {},
    "functions": {"main.py": {"memory": 1024, "maxDuration": 30}},
}


VERCEL_MAIN_TEMPLATE = '''# -*- coding: utf-8 -*-
"""Vercel Serverless Function Entry Point.

This file serves as the entry point for Vercel serverless deployment.
"""

from fastapi import FastAPI
from mangum import Mangum

# Import your AgentFlow application
# from your_app import create_app
# app = create_app()

# Or create a simple app
app = FastAPI(title="{app_name}")


@app.get("/")
async def root():
    return {{"message": "AgentFlow is running!"}}


@app.get("/api/health")
async def health():
    return {{"status": "healthy"}}


# Vercel handler
handler = Mangum(app)
'''


AWS_SERVERLESS_TEMPLATE = """# -*- coding: utf-8 -*-
# AWS Lambda Serverless Configuration
#
# Deploy: serverless deploy
# Invoke: serverless invoke -f app

service: {app_name}

frameworkVersion: "3"

provider:
  name: aws
  runtime: python{python_version}
  stage: ${{opt:stage, 'dev'}}
  region: {region}
  memorySize: {memory_size}
  timeout: {timeout}
  environment:
{env_vars_section}

  # IAM Role
  iamRoleStatements:
    - Effect: Allow
      Action:
        - logs:CreateLogGroup
        - logs:CreateLogStream
        - logs:PutLogEvents
      Resource: "*"

functions:
  app:
    handler: handler.handler
    events:
      - http:
          path: /
          method: ANY
          cors: true
      - http:
          path: /{{proxy+}}
          method: ANY
          cors: true

plugins:
  - serverless-python-requirements

custom:
  pythonRequirements:
    dockerizePip: true
    slim: true
    strip: false
    useStaticCache: true
    useDownloadCache: true

package:
  individually: true
  patterns:
    - "!node_modules/**"
    - "!.git/**"
    - "!tests/**"
    - "!*.md"
    - "!.env*"
"""


AWS_HANDLER_TEMPLATE = '''# -*- coding: utf-8 -*-
"""AWS Lambda Handler.

This file serves as the entry point for AWS Lambda deployment.
"""

from mangum import Mangum
from fastapi import FastAPI

# Import your AgentFlow application
# from your_app import create_app
# app = create_app()

# Or create a simple app
app = FastAPI(title="{app_name}")


@app.get("/")
async def root():
    return {{"message": "AgentFlow is running!"}}


@app.get("/health")
async def health():
    return {{"status": "healthy"}}


# Lambda handler
handler = Mangum(app, lifespan="off")
'''


def generate_vercel_config(
    output_dir: str | Path,
    config: ServerlessConfig | None = None,
    **kwargs: Any,
) -> Path:
    """Vercel 設定を生成.

    Args:
        output_dir: 出力ディレクトリ
        config: Serverless 設定
        **kwargs: ServerlessConfig への引数

    Returns:
        生成されたファイルパス
    """
    if config is None:
        config = ServerlessConfig(**kwargs)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # vercel.json
    vercel_config = VERCEL_CONFIG_TEMPLATE.copy()
    vercel_config["env"] = config.env_vars
    vercel_config["functions"]["main.py"]["memory"] = config.memory_size
    vercel_config["functions"]["main.py"]["maxDuration"] = config.timeout

    vercel_path = output_path / "vercel.json"
    vercel_path.write_text(json.dumps(vercel_config, indent=2), encoding="utf-8")

    # main.py (entry point)
    main_content = VERCEL_MAIN_TEMPLATE.format(app_name=config.app_name)
    main_path = output_path / "main.py"
    if not main_path.exists():
        main_path.write_text(main_content, encoding="utf-8")
        logger.info(f"Generated main.py: {main_path}")

    logger.info(f"Generated vercel.json: {vercel_path}")
    return vercel_path


def generate_aws_lambda_config(
    output_dir: str | Path,
    config: ServerlessConfig | None = None,
    **kwargs: Any,
) -> Path:
    """AWS Lambda 設定を生成.

    Args:
        output_dir: 出力ディレクトリ
        config: Serverless 設定
        **kwargs: ServerlessConfig への引数

    Returns:
        生成されたファイルパス
    """
    if config is None:
        config = ServerlessConfig(**kwargs)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # 環境変数セクションを生成
    env_vars_section = ""
    for key, value in config.env_vars.items():
        env_vars_section += f"    {key}: {value}\n"
    if not env_vars_section:
        env_vars_section = "    # Add your environment variables here\n"

    # serverless.yml
    serverless_content = AWS_SERVERLESS_TEMPLATE.format(
        app_name=config.app_name,
        python_version=config.python_version,
        region=config.region,
        memory_size=config.memory_size,
        timeout=config.timeout,
        env_vars_section=env_vars_section,
    )

    serverless_path = output_path / "serverless.yml"
    serverless_path.write_text(serverless_content, encoding="utf-8")

    # handler.py
    handler_content = AWS_HANDLER_TEMPLATE.format(app_name=config.app_name)
    handler_path = output_path / "handler.py"
    if not handler_path.exists():
        handler_path.write_text(handler_content, encoding="utf-8")
        logger.info(f"Generated handler.py: {handler_path}")

    logger.info(f"Generated serverless.yml: {serverless_path}")
    return serverless_path


def generate_requirements_txt(
    output_dir: str | Path,
    extra_packages: list[str] | None = None,
) -> Path:
    """requirements.txt を生成.

    Args:
        output_dir: 出力ディレクトリ
        extra_packages: 追加パッケージ

    Returns:
        生成されたファイルパス
    """
    requirements = [
        "agentflow>=1.0.0",
        "fastapi>=0.100.0",
        "uvicorn>=0.23.0",
        "mangum>=0.17.0",  # AWS Lambda/Vercel adapter
        "pydantic>=2.0.0",
        "httpx>=0.24.0",
    ]

    if extra_packages:
        requirements.extend(extra_packages)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    requirements_path = output_path / "requirements.txt"
    requirements_path.write_text("\n".join(requirements), encoding="utf-8")

    logger.info(f"Generated requirements.txt: {requirements_path}")
    return requirements_path
