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

from agentflow.deploy.ci_cd_generator import (
    CICDConfig,
    generate_github_actions,
    generate_gitlab_ci,
)
from agentflow.deploy.docker_generator import (
    DockerConfig,
    generate_docker_compose,
    generate_dockerfile,
    generate_dockerignore,
)
from agentflow.deploy.executor import DeployExecutor
from agentflow.deploy.generator import (
    DeploymentConfig,
    generate_all,
)
from agentflow.deploy.serverless_generator import (
    ServerlessConfig,
    generate_aws_lambda_config,
    generate_vercel_config,
)
from agentflow.deploy.targets import (
    AWSLambdaTarget,
    BaseDeployTarget,
    DockerTarget,
    GitHubActionsTarget,
    VercelTarget,
)
from agentflow.deploy.workflow_generator import (
    CodeGenConfig,
    Workflow,
    WorkflowCodeGenerator,
    WorkflowEdge,
    WorkflowNode,
    generate_workflow_code,
    generate_workflow_zip,
)


__all__ = [
    "AWSLambdaTarget",
    "BaseDeployTarget",
    "CICDConfig",
    "CodeGenConfig",
    # Deploy Executor (v0.4.0)
    "DeployExecutor",
    "DeploymentConfig",
    "DockerConfig",
    "DockerTarget",
    "GitHubActionsTarget",
    "ServerlessConfig",
    "VercelTarget",
    "Workflow",
    "WorkflowCodeGenerator",
    "WorkflowEdge",
    "WorkflowNode",
    # All-in-one
    "generate_all",
    "generate_aws_lambda_config",
    "generate_docker_compose",
    # Docker
    "generate_dockerfile",
    "generate_dockerignore",
    # CI/CD
    "generate_github_actions",
    "generate_gitlab_ci",
    # Serverless
    "generate_vercel_config",
    # Workflow Generator (v0.3.0)
    "generate_workflow_code",
    "generate_workflow_zip",
]

