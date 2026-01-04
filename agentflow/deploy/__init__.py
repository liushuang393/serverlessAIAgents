# -*- coding: utf-8 -*-
"""AgentFlow デプロイツールモジュール.

Docker、Serverless、CI/CD のためのテンプレートとツールを提供します。

機能:
- Docker テンプレート生成
- Serverless デプロイ（Vercel、AWS Lambda）
- CI/CD テンプレート（GitHub Actions）
- 環境設定ヘルパー

使用例:
    >>> from agentflow.deploy import generate_docker, generate_ci_cd
    >>>
    >>> # Docker ファイル生成
    >>> generate_docker("./", app_name="my-agent")
    >>>
    >>> # CI/CD 生成
    >>> generate_ci_cd("./", provider="github")
"""

from agentflow.deploy.docker_generator import (
    generate_dockerfile,
    generate_docker_compose,
    generate_dockerignore,
    DockerConfig,
)
from agentflow.deploy.serverless_generator import (
    generate_vercel_config,
    generate_aws_lambda_config,
    ServerlessConfig,
)
from agentflow.deploy.ci_cd_generator import (
    generate_github_actions,
    generate_gitlab_ci,
    CICDConfig,
)
from agentflow.deploy.generator import (
    generate_all,
    DeploymentConfig,
)

__all__ = [
    # Docker
    "generate_dockerfile",
    "generate_docker_compose",
    "generate_dockerignore",
    "DockerConfig",
    # Serverless
    "generate_vercel_config",
    "generate_aws_lambda_config",
    "ServerlessConfig",
    # CI/CD
    "generate_github_actions",
    "generate_gitlab_ci",
    "CICDConfig",
    # All-in-one
    "generate_all",
    "DeploymentConfig",
]

