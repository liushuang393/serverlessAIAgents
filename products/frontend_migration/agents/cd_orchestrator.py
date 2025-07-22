"""
CD オーケストレーター

GitHub Actions、Vercel、Netlify 等への自動デプロイ設定を担当するエージェント。
移行完了後の継続的デプロイメント（CD）パイプラインを自動構築します。

主要機能:
- GitHub Actions ワークフローの生成
- Vercel/Netlify デプロイ設定の作成
- 品質ゲートの設定（テスト、Lighthouse等）
- 段階的デプロイメント戦略の実装
"""

import asyncio
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List

from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.utils.logging import get_logger

logger = get_logger(__name__)


@dataclass
class DeploymentConfig:
    """デプロイメント設定を表すデータクラス"""

    platform: str  # github-actions, vercel, netlify
    environment: str  # development, staging, production
    build_command: str
    output_directory: str
    environment_variables: Dict[str, str]
    quality_gates: List[str]


@dataclass
class CDPipeline:
    """CD パイプラインを格納するデータクラス"""

    name: str
    trigger_events: List[str]
    environments: List[DeploymentConfig]
    workflow_files: Dict[str, str]
    config_files: Dict[str, str]
    quality_checks: List[str]


class CDOrchestrator:
    """
    継続的デプロイメント（CD）を担当するオーケストレーター

    ai_blocks.core.tool.ToolManagerを使用してデプロイメントツールを管理し、
    ai_blocks.core.memory.VectorMemoryを使用してパイプライン設定を記憶します。
    """

    def __init__(self, llm_provider=None):
        """
        CD オーケストレーターを初期化

        Args:
            llm_provider: LLMプロバイダー（オプション）
        """
        self.tool_manager = ToolManager()
        self.memory = VectorMemory()
        self.llm_provider = llm_provider

        # CD用ツールを登録
        self._register_cd_tools()

        logger.info("CDOrchestratorを初期化しました")

    def _register_cd_tools(self) -> None:
        """CD用のツールを登録"""

        @tool(
            name="generate_github_actions_workflow",
            description="GitHub Actions ワークフローを生成",
        )
        def generate_github_actions_workflow(
            project_config: Dict[str, Any], environments: List[str]
        ) -> Dict[str, str]:
            """
            GitHub Actions ワークフローを生成

            Args:
                project_config: プロジェクト設定
                environments: デプロイ環境のリスト

            Returns:
                生成されたワークフローファイル
            """
            workflows = {}

            # メインのCI/CDワークフロー
            main_workflow = f"""
name: Frontend Migration CI/CD

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v4

    - name: Setup Node.js
      uses: actions/setup-node@v4
      with:
        node-version: '18'
        cache: 'npm'

    - name: Install dependencies
      run: npm ci

    - name: Run linting
      run: npm run lint

    - name: Run unit tests
      run: npm run test:unit

    - name: Run integration tests
      run: npm run test:integration

    - name: Build project
      run: npm run build

    - name: Run Lighthouse CI
      uses: treosh/lighthouse-ci-action@v10
      with:
        configPath: './lighthouserc.json'
        uploadArtifacts: true
        temporaryPublicStorage: true

  deploy-staging:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/develop'

    steps:
    - name: Checkout code
      uses: actions/checkout@v4

    - name: Setup Node.js
      uses: actions/setup-node@v4
      with:
        node-version: '18'
        cache: 'npm'

    - name: Install dependencies
      run: npm ci

    - name: Build for staging
      run: npm run build:staging
      env:
        NODE_ENV: staging

    - name: Deploy to Staging
      uses: peaceiris/actions-gh-pages@v3
      with:
        github_token: ${{{{ secrets.GITHUB_TOKEN }}}}
        publish_dir: ./dist
        destination_dir: staging

  deploy-production:
    needs: test
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'

    steps:
    - name: Checkout code
      uses: actions/checkout@v4

    - name: Setup Node.js
      uses: actions/setup-node@v4
      with:
        node-version: '18'
        cache: 'npm'

    - name: Install dependencies
      run: npm ci

    - name: Build for production
      run: npm run build:production
      env:
        NODE_ENV: production

    - name: Deploy to Production
      uses: peaceiris/actions-gh-pages@v3
      with:
        github_token: ${{{{ secrets.GITHUB_TOKEN }}}}
        publish_dir: ./dist
        cname: {project_config.get('domain', 'example.com')}
"""

            workflows["ci-cd.yml"] = main_workflow

            # 品質チェック専用ワークフロー
            quality_workflow = """
name: Quality Checks

on:
  schedule:
    - cron: '0 2 * * *'  # 毎日午前2時に実行
  workflow_dispatch:

jobs:
  quality-audit:
    runs-on: ubuntu-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v4

    - name: Setup Node.js
      uses: actions/setup-node@v4
      with:
        node-version: '18'
        cache: 'npm'

    - name: Install dependencies
      run: npm ci

    - name: Build project
      run: npm run build

    - name: Run accessibility tests
      run: npm run test:a11y

    - name: Run performance tests
      run: npm run test:performance

    - name: Generate QA report
      run: npm run qa:report

    - name: Upload QA artifacts
      uses: actions/upload-artifact@v3
      with:
        name: qa-reports
        path: reports/
"""

            workflows["quality-checks.yml"] = quality_workflow

            return workflows

        @tool(name="generate_vercel_config", description="Vercel デプロイ設定を生成")
        def generate_vercel_config(project_config: Dict[str, Any]) -> Dict[str, str]:
            """
            Vercel デプロイ設定を生成

            Args:
                project_config: プロジェクト設定

            Returns:
                Vercel 設定ファイル
            """
            vercel_config = {
                "version": 2,
                "name": project_config.get("name", "frontend-migration"),
                "builds": [
                    {
                        "src": "package.json",
                        "use": "@vercel/static-build",
                        "config": {"distDir": "dist"},
                    }
                ],
                "routes": [{"src": "/(.*)", "dest": "/$1"}],
                "env": {"NODE_ENV": "production"},
                "build": {"env": {"NODE_ENV": "production"}},
                "functions": {"app/api/**/*.js": {"runtime": "nodejs18.x"}},
                "headers": [
                    {
                        "source": "/(.*)",
                        "headers": [
                            {"key": "X-Content-Type-Options", "value": "nosniff"},
                            {"key": "X-Frame-Options", "value": "DENY"},
                            {"key": "X-XSS-Protection", "value": "1; mode=block"},
                        ],
                    }
                ],
            }

            return {"vercel.json": json.dumps(vercel_config, indent=2)}

        @tool(name="generate_netlify_config", description="Netlify デプロイ設定を生成")
        def generate_netlify_config(project_config: Dict[str, Any]) -> Dict[str, str]:
            """
            Netlify デプロイ設定を生成

            Args:
                project_config: プロジェクト設定

            Returns:
                Netlify 設定ファイル
            """
            netlify_config = """
[build]
  publish = "dist"
  command = "npm run build"

[build.environment]
  NODE_ENV = "production"
  NODE_VERSION = "18"

[[redirects]]
  from = "/*"
  to = "/index.html"
  status = 200

[[headers]]
  for = "/*"
  [headers.values]
    X-Frame-Options = "DENY"
    X-XSS-Protection = "1; mode=block"
    X-Content-Type-Options = "nosniff"
    Referrer-Policy = "strict-origin-when-cross-origin"

[[headers]]
  for = "/static/*"
  [headers.values]
    Cache-Control = "public, max-age=31536000, immutable"

[dev]
  command = "npm run dev"
  port = 3000
  publish = "dist"

[context.staging]
  command = "npm run build:staging"
  environment = { NODE_ENV = "staging" }

[context.production]
  command = "npm run build:production"
  environment = { NODE_ENV = "production" }
"""

            return {"netlify.toml": netlify_config}

        @tool(name="generate_lighthouse_config", description="Lighthouse CI 設定を生成")
        def generate_lighthouse_config(
            quality_thresholds: Dict[str, float]
        ) -> Dict[str, str]:
            """
            Lighthouse CI 設定を生成

            Args:
                quality_thresholds: 品質閾値

            Returns:
                Lighthouse CI 設定ファイル
            """
            lighthouse_config = {
                "ci": {
                    "collect": {
                        "url": [
                            "http://localhost:3000",
                            "http://localhost:3000/about",
                            "http://localhost:3000/contact",
                        ],
                        "startServerCommand": "npm run serve",
                        "startServerReadyPattern": "ready on",
                        "numberOfRuns": 3,
                    },
                    "assert": {
                        "assertions": {
                            "categories:performance": [
                                "error",
                                {
                                    "minScore": quality_thresholds.get(
                                        "performance", 0.8
                                    )
                                },
                            ],
                            "categories:accessibility": [
                                "error",
                                {
                                    "minScore": quality_thresholds.get(
                                        "accessibility", 0.9
                                    )
                                },
                            ],
                            "categories:best-practices": [
                                "error",
                                {
                                    "minScore": quality_thresholds.get(
                                        "best_practices", 0.8
                                    )
                                },
                            ],
                            "categories:seo": [
                                "error",
                                {"minScore": quality_thresholds.get("seo", 0.9)},
                            ],
                            "first-contentful-paint": [
                                "error",
                                {"maxNumericValue": 2000},
                            ],
                            "largest-contentful-paint": [
                                "error",
                                {"maxNumericValue": 4000},
                            ],
                            "cumulative-layout-shift": [
                                "error",
                                {"maxNumericValue": 0.1},
                            ],
                        }
                    },
                    "upload": {"target": "temporary-public-storage"},
                }
            }

            return {"lighthouserc.json": json.dumps(lighthouse_config, indent=2)}

        @tool(name="generate_package_scripts", description="package.json スクリプトを生成")
        def generate_package_scripts(build_config: Dict[str, Any]) -> Dict[str, Any]:
            """
            package.json のスクリプトセクションを生成

            Args:
                build_config: ビルド設定

            Returns:
                package.json スクリプト
            """
            scripts = {
                "dev": "vite",
                "build": "vite build",
                "build:staging": "vite build --mode staging",
                "build:production": "vite build --mode production",
                "serve": "vite preview",
                "lint": "eslint . --ext .ts,.tsx,.js,.jsx",
                "lint:fix": "eslint . --ext .ts,.tsx,.js,.jsx --fix",
                "test": "jest",
                "test:unit": "jest --testPathPattern=unit",
                "test:integration": "jest --testPathPattern=integration",
                "test:a11y": "jest --testPathPattern=accessibility",
                "test:performance": "jest --testPathPattern=performance",
                "test:watch": "jest --watch",
                "test:coverage": "jest --coverage",
                "qa:report": "node scripts/generate-qa-report.js",
                "lighthouse": "lhci autorun",
                "type-check": "tsc --noEmit",
                "format": "prettier --write .",
                "format:check": "prettier --check .",
                "prepare": "husky install",
            }

            return {"scripts": scripts}

        # ツールマネージャーに登録
        self.tool_manager.register_function(generate_github_actions_workflow)
        self.tool_manager.register_function(generate_vercel_config)
        self.tool_manager.register_function(generate_netlify_config)
        self.tool_manager.register_function(generate_lighthouse_config)
        self.tool_manager.register_function(generate_package_scripts)

    async def setup_cd_pipeline(
        self, project_config: Dict[str, Any], deployment_platforms: List[str] = None
    ) -> CDPipeline:
        """
        CD パイプラインをセットアップ

        Args:
            project_config: プロジェクト設定
            deployment_platforms: デプロイメントプラットフォームのリスト

        Returns:
            CDPipeline: セットアップされたCDパイプライン
        """
        logger.info("CDパイプラインのセットアップを開始")

        deployment_platforms = deployment_platforms or ["github-actions", "vercel"]

        workflow_files = {}
        config_files = {}
        environments = []

        # GitHub Actions ワークフローを生成
        if "github-actions" in deployment_platforms:
            github_result = await self.tool_manager.execute(
                "generate_github_actions_workflow",
                {
                    "project_config": project_config,
                    "environments": ["staging", "production"],
                },
            )

            if github_result.success:
                workflow_files.update(github_result.result)

        # Vercel 設定を生成
        if "vercel" in deployment_platforms:
            vercel_result = await self.tool_manager.execute(
                "generate_vercel_config", {"project_config": project_config}
            )

            if vercel_result.success:
                config_files.update(vercel_result.result)

        # Netlify 設定を生成
        if "netlify" in deployment_platforms:
            netlify_result = await self.tool_manager.execute(
                "generate_netlify_config", {"project_config": project_config}
            )

            if netlify_result.success:
                config_files.update(netlify_result.result)

        # Lighthouse CI 設定を生成
        lighthouse_result = await self.tool_manager.execute(
            "generate_lighthouse_config",
            {
                "quality_thresholds": {
                    "performance": 0.8,
                    "accessibility": 0.9,
                    "best_practices": 0.8,
                    "seo": 0.9,
                }
            },
        )

        if lighthouse_result.success:
            config_files.update(lighthouse_result.result)

        # package.json スクリプトを生成
        scripts_result = await self.tool_manager.execute(
            "generate_package_scripts", {"build_config": project_config}
        )

        if scripts_result.success:
            config_files.update(scripts_result.result)

        # デプロイメント環境を設定
        for platform in deployment_platforms:
            if platform == "vercel":
                environments.append(
                    DeploymentConfig(
                        platform="vercel",
                        environment="production",
                        build_command="npm run build:production",
                        output_directory="dist",
                        environment_variables={"NODE_ENV": "production"},
                        quality_gates=["lighthouse", "accessibility", "performance"],
                    )
                )
            elif platform == "netlify":
                environments.append(
                    DeploymentConfig(
                        platform="netlify",
                        environment="production",
                        build_command="npm run build:production",
                        output_directory="dist",
                        environment_variables={"NODE_ENV": "production"},
                        quality_gates=["lighthouse", "accessibility"],
                    )
                )

        # CDパイプラインを作成
        cd_pipeline = CDPipeline(
            name=f"{project_config.get('name', 'frontend-migration')}-cd",
            trigger_events=["push", "pull_request"],
            environments=environments,
            workflow_files=workflow_files,
            config_files=config_files,
            quality_checks=["lint", "test", "lighthouse", "accessibility"],
        )

        # メモリに保存
        await self.memory.store(
            content=f"CD pipeline setup for {project_config.get('name')}",
            metadata={
                "type": "cd_pipeline",
                "project_name": project_config.get("name"),
                "platforms": deployment_platforms,
                "environments_count": len(environments),
            },
        )

        logger.info(f"CDパイプラインセットアップ完了: {len(deployment_platforms)}プラットフォーム")
        return cd_pipeline

    async def save_pipeline_files(
        self, cd_pipeline: CDPipeline, output_dir: str = "."
    ) -> None:
        """
        CDパイプラインファイルを保存

        Args:
            cd_pipeline: 保存するCDパイプライン
            output_dir: 出力ディレクトリ
        """
        output_path = Path(output_dir)

        # GitHub Actions ワークフローファイルを保存
        workflows_dir = output_path / ".github" / "workflows"
        workflows_dir.mkdir(parents=True, exist_ok=True)

        for filename, content in cd_pipeline.workflow_files.items():
            workflow_path = workflows_dir / filename
            with open(workflow_path, "w", encoding="utf-8") as f:
                f.write(content)

        # 設定ファイルを保存
        for filename, content in cd_pipeline.config_files.items():
            if filename == "scripts":
                # package.json のスクリプトセクションは別途処理
                continue

            config_path = output_path / filename
            with open(config_path, "w", encoding="utf-8") as f:
                f.write(content)

        logger.info(f"CDパイプラインファイルを保存しました: {output_dir}")


# 使用例とテスト用のメイン関数
async def main():
    """
    CDOrchestratorの使用例
    """
    orchestrator = CDOrchestrator()

    try:
        # CDパイプラインをセットアップ
        cd_pipeline = await orchestrator.setup_cd_pipeline(
            project_config={
                "name": "frontend-migration-demo",
                "domain": "demo.example.com",
            },
            deployment_platforms=["github-actions", "vercel", "netlify"],
        )

        # パイプラインファイルを保存
        await orchestrator.save_pipeline_files(cd_pipeline, ".")

        print("CDパイプラインセットアップ完了:")
        print(f"  パイプライン名: {cd_pipeline.name}")
        print(f"  環境数: {len(cd_pipeline.environments)}")
        print(f"  ワークフローファイル数: {len(cd_pipeline.workflow_files)}")
        print(f"  設定ファイル数: {len(cd_pipeline.config_files)}")
        print(f"  品質チェック: {', '.join(cd_pipeline.quality_checks)}")

    except Exception as e:
        logger.error(f"CDパイプラインセットアップエラー: {e}")


if __name__ == "__main__":
    asyncio.run(main())
