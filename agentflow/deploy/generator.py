# -*- coding: utf-8 -*-
"""統合デプロイ生成モジュール.

全てのデプロイ設定を一括生成します。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from agentflow.deploy.docker_generator import (
    DockerConfig,
    generate_dockerfile,
    generate_docker_compose,
    generate_dockerignore,
)
from agentflow.deploy.serverless_generator import (
    ServerlessConfig,
    generate_vercel_config,
    generate_aws_lambda_config,
    generate_requirements_txt,
)
from agentflow.deploy.ci_cd_generator import (
    CICDConfig,
    generate_github_actions,
    generate_gitlab_ci,
    generate_pre_commit_config,
)

logger = logging.getLogger(__name__)


@dataclass
class DeploymentConfig:
    """統合デプロイ設定.

    Attributes:
        app_name: アプリケーション名
        python_version: Python バージョン
        docker: Docker 設定を生成するか
        vercel: Vercel 設定を生成するか
        aws_lambda: AWS Lambda 設定を生成するか
        github_actions: GitHub Actions を生成するか
        gitlab_ci: GitLab CI を生成するか
        pre_commit: pre-commit 設定を生成するか
        env_vars: 環境変数
    """

    app_name: str = "agentflow-app"
    python_version: str = "3.13"
    docker: bool = True
    vercel: bool = False
    aws_lambda: bool = False
    github_actions: bool = True
    gitlab_ci: bool = False
    pre_commit: bool = True
    env_vars: dict[str, str] = field(default_factory=dict)


def generate_all(
    output_dir: str | Path,
    config: DeploymentConfig | None = None,
    **kwargs: Any,
) -> dict[str, Path]:
    """全てのデプロイ設定を生成.

    Args:
        output_dir: 出力ディレクトリ
        config: デプロイ設定
        **kwargs: DeploymentConfig への引数

    Returns:
        生成されたファイルパスの辞書
    """
    if config is None:
        config = DeploymentConfig(**kwargs)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    generated: dict[str, Path] = {}

    # Docker
    if config.docker:
        docker_config = DockerConfig(
            app_name=config.app_name,
            python_version=config.python_version,
            env_vars=config.env_vars,
        )
        generated["dockerfile"] = generate_dockerfile(output_path, docker_config)
        generated["docker_compose"] = generate_docker_compose(output_path, docker_config)
        generated["dockerignore"] = generate_dockerignore(output_path)

    # Vercel
    if config.vercel:
        serverless_config = ServerlessConfig(
            app_name=config.app_name,
            python_version=config.python_version,
            env_vars=config.env_vars,
        )
        generated["vercel"] = generate_vercel_config(output_path, serverless_config)

    # AWS Lambda
    if config.aws_lambda:
        serverless_config = ServerlessConfig(
            app_name=config.app_name,
            python_version=config.python_version,
            env_vars=config.env_vars,
        )
        generated["aws_lambda"] = generate_aws_lambda_config(
            output_path, serverless_config
        )
        generated["requirements"] = generate_requirements_txt(output_path)

    # GitHub Actions
    if config.github_actions:
        ci_config = CICDConfig(
            app_name=config.app_name,
            python_version=config.python_version,
        )
        generated["github_actions"] = generate_github_actions(output_path, ci_config)

    # GitLab CI
    if config.gitlab_ci:
        ci_config = CICDConfig(
            app_name=config.app_name,
            python_version=config.python_version,
        )
        generated["gitlab_ci"] = generate_gitlab_ci(output_path, ci_config)

    # Pre-commit
    if config.pre_commit:
        generated["pre_commit"] = generate_pre_commit_config(output_path)

    logger.info(f"Generated {len(generated)} deployment files in {output_path}")
    return generated


def generate_env_template(output_dir: str | Path) -> Path:
    """`.env.example` テンプレートを生成.

    Args:
        output_dir: 出力ディレクトリ

    Returns:
        生成されたファイルパス
    """
    env_template = """# -*- coding: utf-8 -*-
# AgentFlow Environment Variables
#
# Copy this file to .env and fill in your values
# DO NOT commit .env to version control!

# ============================================================================
# Application
# ============================================================================
APP_NAME=agentflow-app
APP_ENV=development
DEBUG=true
LOG_LEVEL=INFO

# ============================================================================
# LLM Providers (at least one required)
# ============================================================================
# OpenAI
OPENAI_API_KEY=

# Anthropic
ANTHROPIC_API_KEY=

# Google AI
GOOGLE_API_KEY=

# DeepSeek
DEEPSEEK_API_KEY=

# Ollama (local)
OLLAMA_BASE_URL=http://localhost:11434

# ============================================================================
# Database (optional)
# ============================================================================
# Supabase
SUPABASE_URL=
SUPABASE_KEY=

# PostgreSQL
DATABASE_URL=

# Turso
TURSO_URL=
TURSO_AUTH_TOKEN=

# ============================================================================
# Vector Database (optional)
# ============================================================================
# Pinecone
PINECONE_API_KEY=
PINECONE_ENVIRONMENT=

# Qdrant
QDRANT_URL=

# ChromaDB (local)
CHROMA_PERSIST_DIR=./data/chroma

# ============================================================================
# Security
# ============================================================================
# JWT
JWT_SECRET_KEY=your-super-secret-key-change-in-production

# ============================================================================
# Observability (optional)
# ============================================================================
# Sentry
SENTRY_DSN=
SENTRY_ENVIRONMENT=development

# ============================================================================
# External Services (optional)
# ============================================================================
# Stripe
STRIPE_API_KEY=
STRIPE_WEBHOOK_SECRET=

# Redis
REDIS_URL=redis://localhost:6379
"""

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    env_path = output_path / ".env.example"
    env_path.write_text(env_template, encoding="utf-8")

    logger.info(f"Generated .env.example: {env_path}")
    return env_path

