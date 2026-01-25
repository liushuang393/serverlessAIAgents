# -*- coding: utf-8 -*-
"""Deploy Targets - デプロイターゲット実装.

対応プラットフォーム:
- Vercel: Serverless Functions
- AWS Lambda: AWS Lambda Functions
- Google Cloud Run: コンテナベースサーバーレス
- Azure Container Apps: Azure コンテナアプリ
- Hugging Face Spaces: AI/ML アプリケーション
- Modal: GPU/CPU ワークロード
- Railway: ゼロコンフィグデプロイ
- Render: Web サービス
- Fly.io: グローバル分散デプロイ
- Docker: コンテナレジストリ
"""

from agentflow.deploy.targets.base import BaseDeployTarget
from agentflow.deploy.targets.vercel import VercelTarget
from agentflow.deploy.targets.docker import DockerTarget
from agentflow.deploy.targets.aws_lambda import AWSLambdaTarget
from agentflow.deploy.targets.github_actions import GitHubActionsTarget
from agentflow.deploy.targets.google_cloud_run import GoogleCloudRunTarget
from agentflow.deploy.targets.azure_container_apps import AzureContainerAppsTarget
from agentflow.deploy.targets.huggingface_spaces import HuggingFaceSpacesTarget
from agentflow.deploy.targets.modal import ModalTarget
from agentflow.deploy.targets.railway import RailwayTarget
from agentflow.deploy.targets.render import RenderTarget
from agentflow.deploy.targets.flyio import FlyioTarget

__all__ = [
    "BaseDeployTarget",
    # Serverless
    "VercelTarget",
    "AWSLambdaTarget",
    "GoogleCloudRunTarget",
    "AzureContainerAppsTarget",
    # AI/ML Platforms
    "HuggingFaceSpacesTarget",
    "ModalTarget",
    # PaaS
    "RailwayTarget",
    "RenderTarget",
    "FlyioTarget",
    # Container
    "DockerTarget",
    "GitHubActionsTarget",
]

# プラットフォーム一覧 (UI 表示用)
DEPLOY_TARGETS = {
    "vercel": VercelTarget,
    "aws_lambda": AWSLambdaTarget,
    "google_cloud_run": GoogleCloudRunTarget,
    "azure_container_apps": AzureContainerAppsTarget,
    "huggingface_spaces": HuggingFaceSpacesTarget,
    "modal": ModalTarget,
    "railway": RailwayTarget,
    "render": RenderTarget,
    "flyio": FlyioTarget,
    "docker": DockerTarget,
}
