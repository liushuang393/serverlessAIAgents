"""MigrationPipelineGenerator - 迁移パイプライン生成.

迁移プロジェクト用のCI/CDパイプラインを生成します。

使用例:
    >>> generator = MigrationPipelineGenerator()
    >>> config = PipelineConfig(
    ...     project_name="legacy-migration",
    ...     source_language="cobol",
    ...     target_language="java",
    ... )
    >>> yaml_content = generator.generate_github(config)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Any


_logger = logging.getLogger(__name__)


class PipelineStage(str, Enum):
    """パイプラインステージ."""

    ANALYZE = "analyze"
    TRANSFORM = "transform"
    VALIDATE = "validate"
    TEST = "test"
    BUILD = "build"
    DEPLOY = "deploy"


class CIPlatform(str, Enum):
    """CI/CDプラットフォーム."""

    GITHUB = "github"
    GITLAB = "gitlab"
    AZURE_DEVOPS = "azure_devops"
    JENKINS = "jenkins"


@dataclass
class PipelineConfig:
    """パイプライン設定.

    Attributes:
        project_name: プロジェクト名
        source_language: ソース言語
        target_language: ターゲット言語
        source_dir: ソースディレクトリ
        target_dir: ターゲットディレクトリ
        stages: 有効ステージ
        parallel_jobs: 並列ジョブ数
        quality_threshold: 品質閾値
        test_coverage_threshold: テストカバレッジ閾値
        notifications: 通知設定
        artifacts: アーティファクト設定
        environment: 環境変数
    """

    project_name: str
    source_language: str
    target_language: str
    source_dir: str = "./source"
    target_dir: str = "./target"
    stages: list[PipelineStage] = field(default_factory=lambda: list(PipelineStage))
    parallel_jobs: int = 4
    quality_threshold: float = 80.0
    test_coverage_threshold: float = 70.0
    notifications: dict[str, Any] = field(default_factory=dict)
    artifacts: dict[str, Any] = field(default_factory=dict)
    environment: dict[str, str] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "project_name": self.project_name,
            "source_language": self.source_language,
            "target_language": self.target_language,
            "source_dir": self.source_dir,
            "target_dir": self.target_dir,
            "stages": [s.value for s in self.stages],
            "parallel_jobs": self.parallel_jobs,
            "quality_threshold": self.quality_threshold,
            "test_coverage_threshold": self.test_coverage_threshold,
            "notifications": self.notifications,
            "artifacts": self.artifacts,
            "environment": self.environment,
        }


class MigrationPipelineGenerator:
    """迁移パイプライン生成.

    GitHub Actions, GitLab CI, Azure DevOps 等のCI/CDパイプラインを生成します。
    """

    def __init__(self, templates_dir: Path | str | None = None) -> None:
        """初期化.

        Args:
            templates_dir: テンプレートディレクトリ
        """
        if templates_dir:
            self._templates_dir = Path(templates_dir)
        else:
            self._templates_dir = Path(__file__).parent / "templates"

    def generate(
        self,
        config: PipelineConfig,
        platform: CIPlatform = CIPlatform.GITHUB,
    ) -> str:
        """パイプラインを生成.

        Args:
            config: パイプライン設定
            platform: CI/CDプラットフォーム

        Returns:
            パイプライン定義（YAML等）
        """
        generators = {
            CIPlatform.GITHUB: self._generate_github,
            CIPlatform.GITLAB: self._generate_gitlab,
            CIPlatform.AZURE_DEVOPS: self._generate_azure_devops,
            CIPlatform.JENKINS: self._generate_jenkins,
        }
        generator = generators.get(platform)
        if generator is None:
            msg = f"Unsupported platform: {platform}"
            raise ValueError(msg)
        return generator(config)

    def _generate_github(self, config: PipelineConfig) -> str:
        """GitHub Actions パイプラインを生成."""
        stages_jobs = self._build_github_jobs(config)

        return f"""# Generated Migration Pipeline for {config.project_name}
# Source: {config.source_language} -> Target: {config.target_language}

name: Migration Pipeline - {config.project_name}

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]
  workflow_dispatch:
    inputs:
      stage:
        description: 'Stage to run (all, analyze, transform, validate, test, build, deploy)'
        required: false
        default: 'all'

env:
  SOURCE_LANGUAGE: {config.source_language}
  TARGET_LANGUAGE: {config.target_language}
  SOURCE_DIR: {config.source_dir}
  TARGET_DIR: {config.target_dir}
  QUALITY_THRESHOLD: {config.quality_threshold}
  COVERAGE_THRESHOLD: {config.test_coverage_threshold}
{self._format_env_vars(config.environment, indent=2)}

jobs:
{stages_jobs}
"""

    def _build_github_jobs(self, config: PipelineConfig) -> str:
        """GitHub Actions ジョブを構築."""
        jobs = []

        # Analyze
        if PipelineStage.ANALYZE in config.stages:
            jobs.append("""  analyze:
    runs-on: ubuntu-latest
    outputs:
      file_count: ${{ steps.inventory.outputs.file_count }}
      total_loc: ${{ steps.inventory.outputs.total_loc }}
    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.13'

      - name: Install dependencies
        run: |
          pip install agentflow

      - name: Run code inventory
        id: inventory
        run: |
          agentflow code-inventory scan ${{ env.SOURCE_DIR }} \\
            --language ${{ env.SOURCE_LANGUAGE }} \\
            --output inventory.json
          echo "file_count=$(jq .total_files inventory.json)" >> $GITHUB_OUTPUT
          echo "total_loc=$(jq .total_loc inventory.json)" >> $GITHUB_OUTPUT

      - name: Analyze dependencies
        run: |
          agentflow code-inventory analyze-deps inventory.json \\
            --output dependencies.json

      - name: Upload analysis artifacts
        uses: actions/upload-artifact@v4
        with:
          name: analysis-results
          path: |
            inventory.json
            dependencies.json
""")

        # Transform
        if PipelineStage.TRANSFORM in config.stages:
            needs = "needs: [analyze]" if PipelineStage.ANALYZE in config.stages else ""
            jobs.append(f"""  transform:
    runs-on: ubuntu-latest
    {needs}
    strategy:
      matrix:
        batch: [1, 2, 3, 4]
      max-parallel: {config.parallel_jobs}
    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.13'

      - name: Install dependencies
        run: pip install agentflow

      - name: Download analysis
        uses: actions/download-artifact@v4
        with:
          name: analysis-results

      - name: Transform code batch ${{{{ matrix.batch }}}}
        run: |
          agentflow code-transform run \\
            --source-dir ${{{{ env.SOURCE_DIR }}}} \\
            --target-dir ${{{{ env.TARGET_DIR }}}} \\
            --source-lang ${{{{ env.SOURCE_LANGUAGE }}}} \\
            --target-lang ${{{{ env.TARGET_LANGUAGE }}}} \\
            --batch ${{{{ matrix.batch }}}} \\
            --total-batches 4

      - name: Upload transformed code
        uses: actions/upload-artifact@v4
        with:
          name: transformed-batch-${{{{ matrix.batch }}}}
          path: ${{{{ env.TARGET_DIR }}}}
""")

        # Validate
        if PipelineStage.VALIDATE in config.stages:
            needs_list = []
            if PipelineStage.TRANSFORM in config.stages:
                needs_list.append("transform")
            needs = f"needs: [{', '.join(needs_list)}]" if needs_list else ""
            jobs.append(f"""  validate:
    runs-on: ubuntu-latest
    {needs}
    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: '3.13'

      - name: Install dependencies
        run: pip install agentflow

      - name: Download transformed code
        uses: actions/download-artifact@v4
        with:
          pattern: transformed-batch-*
          merge-multiple: true
          path: ${{{{ env.TARGET_DIR }}}}

      - name: Run quality gates
        run: |
          agentflow quality-gate run \\
            --source-dir ${{{{ env.SOURCE_DIR }}}} \\
            --target-dir ${{{{ env.TARGET_DIR }}}} \\
            --threshold ${{{{ env.QUALITY_THRESHOLD }}}} \\
            --output quality-report.json

      - name: Check quality threshold
        run: |
          SCORE=$(jq .score quality-report.json)
          if (( $(echo "$SCORE < ${{{{ env.QUALITY_THRESHOLD }}}}" | bc -l) )); then
            echo "Quality score $SCORE is below threshold ${{{{ env.QUALITY_THRESHOLD }}}}"
            exit 1
          fi

      - name: Upload quality report
        uses: actions/upload-artifact@v4
        with:
          name: quality-report
          path: quality-report.json
""")

        # Test
        if PipelineStage.TEST in config.stages:
            needs_list = []
            if PipelineStage.VALIDATE in config.stages:
                needs_list.append("validate")
            elif PipelineStage.TRANSFORM in config.stages:
                needs_list.append("transform")
            needs = f"needs: [{', '.join(needs_list)}]" if needs_list else ""
            jobs.append(f"""  test:
    runs-on: ubuntu-latest
    {needs}
    steps:
      - uses: actions/checkout@v4

      - name: Set up {config.target_language.title()}
        uses: actions/setup-java@v4
        if: ${{{{ env.TARGET_LANGUAGE == 'java' }}}}
        with:
          distribution: 'temurin'
          java-version: '21'

      - name: Download transformed code
        uses: actions/download-artifact@v4
        with:
          pattern: transformed-batch-*
          merge-multiple: true
          path: ${{{{ env.TARGET_DIR }}}}

      - name: Run tests
        run: |
          cd ${{{{ env.TARGET_DIR }}}}
          # Run target language tests
          if [ "${{{{ env.TARGET_LANGUAGE }}}}" = "java" ]; then
            mvn test -Dmaven.test.failure.ignore=false
          elif [ "${{{{ env.TARGET_LANGUAGE }}}}" = "python" ]; then
            pytest --cov --cov-fail-under=${{{{ env.COVERAGE_THRESHOLD }}}}
          fi

      - name: Upload test results
        uses: actions/upload-artifact@v4
        if: always()
        with:
          name: test-results
          path: |
            **/target/surefire-reports/
            **/coverage.xml
""")

        # Build
        if PipelineStage.BUILD in config.stages:
            needs_list = []
            if PipelineStage.TEST in config.stages:
                needs_list.append("test")
            needs = f"needs: [{', '.join(needs_list)}]" if needs_list else ""
            jobs.append(f"""  build:
    runs-on: ubuntu-latest
    {needs}
    steps:
      - uses: actions/checkout@v4

      - name: Download transformed code
        uses: actions/download-artifact@v4
        with:
          pattern: transformed-batch-*
          merge-multiple: true
          path: ${{{{ env.TARGET_DIR }}}}

      - name: Build
        run: |
          cd ${{{{ env.TARGET_DIR }}}}
          if [ "${{{{ env.TARGET_LANGUAGE }}}}" = "java" ]; then
            mvn package -DskipTests
          elif [ "${{{{ env.TARGET_LANGUAGE }}}}" = "python" ]; then
            pip install build && python -m build
          fi

      - name: Upload build artifacts
        uses: actions/upload-artifact@v4
        with:
          name: build-artifacts
          path: |
            ${{{{ env.TARGET_DIR }}}}/target/*.jar
            ${{{{ env.TARGET_DIR }}}}/dist/*
""")

        # Deploy
        if PipelineStage.DEPLOY in config.stages:
            needs_list = []
            if PipelineStage.BUILD in config.stages:
                needs_list.append("build")
            needs = f"needs: [{', '.join(needs_list)}]" if needs_list else ""
            jobs.append(f"""  deploy:
    runs-on: ubuntu-latest
    {needs}
    environment: production
    if: github.ref == 'refs/heads/main'
    steps:
      - name: Download build artifacts
        uses: actions/download-artifact@v4
        with:
          name: build-artifacts

      - name: Deploy to staging
        run: |
          echo "Deploying to staging environment..."
          # Add deployment commands here

      - name: Run smoke tests
        run: |
          echo "Running smoke tests..."
          # Add smoke test commands here

      - name: Promote to production
        run: |
          echo "Promoting to production..."
          # Add promotion commands here
""")

        return "\n".join(jobs)

    def _generate_gitlab(self, config: PipelineConfig) -> str:
        """GitLab CI パイプラインを生成."""
        return f"""# Generated Migration Pipeline for {config.project_name}
# Source: {config.source_language} -> Target: {config.target_language}

stages:
  - analyze
  - transform
  - validate
  - test
  - build
  - deploy

variables:
  SOURCE_LANGUAGE: {config.source_language}
  TARGET_LANGUAGE: {config.target_language}
  SOURCE_DIR: {config.source_dir}
  TARGET_DIR: {config.target_dir}
  QUALITY_THRESHOLD: "{config.quality_threshold}"
  COVERAGE_THRESHOLD: "{config.test_coverage_threshold}"
{self._format_env_vars(config.environment, indent=2)}

.python_setup:
  image: python:3.13
  before_script:
    - pip install agentflow

analyze:
  extends: .python_setup
  stage: analyze
  script:
    - agentflow code-inventory scan $SOURCE_DIR --language $SOURCE_LANGUAGE --output inventory.json
    - agentflow code-inventory analyze-deps inventory.json --output dependencies.json
  artifacts:
    paths:
      - inventory.json
      - dependencies.json
    expire_in: 1 week

transform:
  extends: .python_setup
  stage: transform
  needs: [analyze]
  parallel: {config.parallel_jobs}
  script:
    - |
      agentflow code-transform run \\
        --source-dir $SOURCE_DIR \\
        --target-dir $TARGET_DIR \\
        --source-lang $SOURCE_LANGUAGE \\
        --target-lang $TARGET_LANGUAGE \\
        --batch $CI_NODE_INDEX \\
        --total-batches $CI_NODE_TOTAL
  artifacts:
    paths:
      - $TARGET_DIR/
    expire_in: 1 week

validate:
  extends: .python_setup
  stage: validate
  needs: [transform]
  script:
    - |
      agentflow quality-gate run \\
        --source-dir $SOURCE_DIR \\
        --target-dir $TARGET_DIR \\
        --threshold $QUALITY_THRESHOLD \\
        --output quality-report.json
    - |
      SCORE=$(jq .score quality-report.json)
      if (( $(echo "$SCORE < $QUALITY_THRESHOLD" | bc -l) )); then
        echo "Quality score $SCORE is below threshold"
        exit 1
      fi
  artifacts:
    paths:
      - quality-report.json
    expire_in: 1 week

test:
  stage: test
  needs: [validate]
  script:
    - cd $TARGET_DIR
    - |
      if [ "$TARGET_LANGUAGE" = "java" ]; then
        mvn test
      elif [ "$TARGET_LANGUAGE" = "python" ]; then
        pip install pytest pytest-cov
        pytest --cov --cov-fail-under=$COVERAGE_THRESHOLD
      fi
  artifacts:
    reports:
      junit: "**/target/surefire-reports/*.xml"
      coverage_report:
        coverage_format: cobertura
        path: "**/coverage.xml"

build:
  stage: build
  needs: [test]
  script:
    - cd $TARGET_DIR
    - |
      if [ "$TARGET_LANGUAGE" = "java" ]; then
        mvn package -DskipTests
      elif [ "$TARGET_LANGUAGE" = "python" ]; then
        pip install build && python -m build
      fi
  artifacts:
    paths:
      - $TARGET_DIR/target/*.jar
      - $TARGET_DIR/dist/*
    expire_in: 1 month

deploy:
  stage: deploy
  needs: [build]
  only:
    - main
  environment:
    name: production
  script:
    - echo "Deploying to production..."
"""

    def _generate_azure_devops(self, config: PipelineConfig) -> str:
        """Azure DevOps パイプラインを生成."""
        return f"""# Generated Migration Pipeline for {config.project_name}
# Source: {config.source_language} -> Target: {config.target_language}

trigger:
  branches:
    include:
      - main
      - develop

pr:
  branches:
    include:
      - main

variables:
  SOURCE_LANGUAGE: {config.source_language}
  TARGET_LANGUAGE: {config.target_language}
  SOURCE_DIR: {config.source_dir}
  TARGET_DIR: {config.target_dir}
  QUALITY_THRESHOLD: {config.quality_threshold}
  COVERAGE_THRESHOLD: {config.test_coverage_threshold}
{self._format_env_vars(config.environment, indent=2)}

stages:
  - stage: Analyze
    jobs:
      - job: CodeInventory
        pool:
          vmImage: 'ubuntu-latest'
        steps:
          - task: UsePythonVersion@0
            inputs:
              versionSpec: '3.13'
          - script: pip install agentflow
            displayName: 'Install dependencies'
          - script: |
              agentflow code-inventory scan $(SOURCE_DIR) \\
                --language $(SOURCE_LANGUAGE) \\
                --output $(Build.ArtifactStagingDirectory)/inventory.json
            displayName: 'Run code inventory'
          - publish: $(Build.ArtifactStagingDirectory)
            artifact: analysis

  - stage: Transform
    dependsOn: Analyze
    jobs:
      - job: TransformCode
        pool:
          vmImage: 'ubuntu-latest'
        strategy:
          parallel: {config.parallel_jobs}
        steps:
          - download: current
            artifact: analysis
          - task: UsePythonVersion@0
            inputs:
              versionSpec: '3.13'
          - script: pip install agentflow
            displayName: 'Install dependencies'
          - script: |
              agentflow code-transform run \\
                --source-dir $(SOURCE_DIR) \\
                --target-dir $(TARGET_DIR) \\
                --source-lang $(SOURCE_LANGUAGE) \\
                --target-lang $(TARGET_LANGUAGE)
            displayName: 'Transform code'
          - publish: $(TARGET_DIR)
            artifact: transformed

  - stage: Validate
    dependsOn: Transform
    jobs:
      - job: QualityGate
        pool:
          vmImage: 'ubuntu-latest'
        steps:
          - download: current
            artifact: transformed
          - task: UsePythonVersion@0
            inputs:
              versionSpec: '3.13'
          - script: pip install agentflow
          - script: |
              agentflow quality-gate run \\
                --source-dir $(SOURCE_DIR) \\
                --target-dir $(Pipeline.Workspace)/transformed \\
                --threshold $(QUALITY_THRESHOLD)
            displayName: 'Run quality gates'

  - stage: Test
    dependsOn: Validate
    jobs:
      - job: RunTests
        pool:
          vmImage: 'ubuntu-latest'
        steps:
          - download: current
            artifact: transformed
          - script: |
              cd $(Pipeline.Workspace)/transformed
              if [ "$(TARGET_LANGUAGE)" = "java" ]; then
                mvn test
              fi
            displayName: 'Run tests'

  - stage: Build
    dependsOn: Test
    jobs:
      - job: BuildArtifacts
        pool:
          vmImage: 'ubuntu-latest'
        steps:
          - download: current
            artifact: transformed
          - script: |
              cd $(Pipeline.Workspace)/transformed
              if [ "$(TARGET_LANGUAGE)" = "java" ]; then
                mvn package -DskipTests
              fi
            displayName: 'Build'
          - publish: $(Pipeline.Workspace)/transformed/target
            artifact: build

  - stage: Deploy
    dependsOn: Build
    condition: and(succeeded(), eq(variables['Build.SourceBranch'], 'refs/heads/main'))
    jobs:
      - deployment: Production
        environment: production
        strategy:
          runOnce:
            deploy:
              steps:
                - script: echo "Deploying to production..."
"""

    def _generate_jenkins(self, config: PipelineConfig) -> str:
        """Jenkins パイプラインを生成."""
        return f"""// Generated Migration Pipeline for {config.project_name}
// Source: {config.source_language} -> Target: {config.target_language}

pipeline {{
    agent any

    environment {{
        SOURCE_LANGUAGE = '{config.source_language}'
        TARGET_LANGUAGE = '{config.target_language}'
        SOURCE_DIR = '{config.source_dir}'
        TARGET_DIR = '{config.target_dir}'
        QUALITY_THRESHOLD = '{config.quality_threshold}'
        COVERAGE_THRESHOLD = '{config.test_coverage_threshold}'
    }}

    stages {{
        stage('Analyze') {{
            steps {{
                sh '''
                    pip install agentflow
                    agentflow code-inventory scan $SOURCE_DIR \\
                        --language $SOURCE_LANGUAGE \\
                        --output inventory.json
                '''
                archiveArtifacts artifacts: 'inventory.json'
            }}
        }}

        stage('Transform') {{
            steps {{
                sh '''
                    agentflow code-transform run \\
                        --source-dir $SOURCE_DIR \\
                        --target-dir $TARGET_DIR \\
                        --source-lang $SOURCE_LANGUAGE \\
                        --target-lang $TARGET_LANGUAGE
                '''
            }}
        }}

        stage('Validate') {{
            steps {{
                sh '''
                    agentflow quality-gate run \\
                        --source-dir $SOURCE_DIR \\
                        --target-dir $TARGET_DIR \\
                        --threshold $QUALITY_THRESHOLD \\
                        --output quality-report.json
                '''
                archiveArtifacts artifacts: 'quality-report.json'
            }}
        }}

        stage('Test') {{
            steps {{
                script {{
                    if (env.TARGET_LANGUAGE == 'java') {{
                        sh 'cd $TARGET_DIR && mvn test'
                    }} else if (env.TARGET_LANGUAGE == 'python') {{
                        sh 'cd $TARGET_DIR && pytest --cov'
                    }}
                }}
            }}
            post {{
                always {{
                    junit '**/target/surefire-reports/*.xml'
                }}
            }}
        }}

        stage('Build') {{
            steps {{
                script {{
                    if (env.TARGET_LANGUAGE == 'java') {{
                        sh 'cd $TARGET_DIR && mvn package -DskipTests'
                    }} else if (env.TARGET_LANGUAGE == 'python') {{
                        sh 'cd $TARGET_DIR && pip install build && python -m build'
                    }}
                }}
            }}
            post {{
                success {{
                    archiveArtifacts artifacts: '$TARGET_DIR/target/*.jar, $TARGET_DIR/dist/*'
                }}
            }}
        }}

        stage('Deploy') {{
            when {{
                branch 'main'
            }}
            steps {{
                echo 'Deploying to production...'
            }}
        }}
    }}

    post {{
        always {{
            cleanWs()
        }}
        success {{
            echo 'Pipeline completed successfully!'
        }}
        failure {{
            echo 'Pipeline failed!'
        }}
    }}
}}
"""

    def _format_env_vars(self, env: dict[str, str], indent: int = 0) -> str:
        """環境変数をフォーマット."""
        if not env:
            return ""
        prefix = " " * indent
        lines = [f"{prefix}{k}: {v}" for k, v in env.items()]
        return "\n".join(lines)

    def save_pipeline(
        self,
        config: PipelineConfig,
        platform: CIPlatform,
        output_path: Path | str,
    ) -> Path:
        """パイプラインをファイルに保存.

        Args:
            config: パイプライン設定
            platform: CI/CDプラットフォーム
            output_path: 出力パス

        Returns:
            保存したファイルパス
        """
        content = self.generate(config, platform)
        output_path = Path(output_path)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(content, encoding="utf-8")
        _logger.info(f"Saved pipeline to {output_path}")
        return output_path


__all__ = [
    "CIPlatform",
    "MigrationPipelineGenerator",
    "PipelineConfig",
    "PipelineStage",
]
