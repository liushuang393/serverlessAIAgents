"""CI/CD テンプレート生成モジュール.

GitHub Actions、GitLab CI の設定を生成します。

特徴:
- テスト自動化
- リント・型チェック
- 自動デプロイ
- マルチ環境対応
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


logger = logging.getLogger(__name__)


@dataclass
class CICDConfig:
    """CI/CD 設定.

    Attributes:
        app_name: アプリケーション名
        python_version: Python バージョン
        node_version: Node.js バージョン
        test_command: テストコマンド
        lint_command: リントコマンド
        deploy_target: デプロイ先（vercel / aws / docker）
        branches: CI を実行するブランチ
    """

    app_name: str = "agentflow-app"
    python_version: str = "3.13"
    node_version: str = "20"
    test_command: str = "pytest"
    lint_command: str = "ruff check . && mypy ."
    deploy_target: str = "docker"
    branches: list[str] = field(default_factory=lambda: ["main", "develop"])


GITHUB_ACTIONS_TEMPLATE = """# -*- coding: utf-8 -*-
# AgentFlow CI/CD Pipeline
#
# This workflow runs tests, linting, and deployment on push/PR

name: AgentFlow CI/CD

on:
  push:
    branches: [{branches}]
  pull_request:
    branches: [{branches}]

env:
  PYTHON_VERSION: "{python_version}"

jobs:
  # ============================================================================
  # Lint & Type Check
  # ============================================================================
  lint:
    name: Lint & Type Check
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: ${{{{ env.PYTHON_VERSION }}}}
          cache: "pip"

      - name: Install dependencies
        run: |
          python -m pip install --upgrade pip
          pip install ruff mypy
          pip install -e ".[dev]"

      - name: Run Ruff (lint)
        run: ruff check .

      - name: Run Ruff (format check)
        run: ruff format --check .

      - name: Run Mypy (type check)
        run: mypy .

  # ============================================================================
  # Test
  # ============================================================================
  test:
    name: Test
    runs-on: ubuntu-latest
    needs: lint

    steps:
      - uses: actions/checkout@v4

      - name: Set up Python
        uses: actions/setup-python@v5
        with:
          python-version: ${{{{ env.PYTHON_VERSION }}}}
          cache: "pip"

      - name: Install dependencies
        run: |
          python -m pip install --upgrade pip
          pip install -e ".[dev]"

      - name: Run tests
        run: {test_command}
        env:
          OPENAI_API_KEY: ${{{{ secrets.OPENAI_API_KEY }}}}

      - name: Upload coverage
        uses: codecov/codecov-action@v4
        with:
          files: ./coverage.xml
          fail_ci_if_error: false

  # ============================================================================
  # Build Docker Image
  # ============================================================================
  build:
    name: Build Docker Image
    runs-on: ubuntu-latest
    needs: test
    if: github.ref == 'refs/heads/main'

    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v3

      - name: Login to Docker Hub
        uses: docker/login-action@v3
        with:
          username: ${{{{ secrets.DOCKER_USERNAME }}}}
          password: ${{{{ secrets.DOCKER_PASSWORD }}}}

      - name: Build and push
        uses: docker/build-push-action@v5
        with:
          context: .
          push: true
          tags: |
            ${{{{ secrets.DOCKER_USERNAME }}}}/{app_name}:latest
            ${{{{ secrets.DOCKER_USERNAME }}}}/{app_name}:${{{{ github.sha }}}}
          cache-from: type=gha
          cache-to: type=gha,mode=max

  # ============================================================================
  # Deploy
  # ============================================================================
  deploy:
    name: Deploy
    runs-on: ubuntu-latest
    needs: build
    if: github.ref == 'refs/heads/main'

    steps:
      - uses: actions/checkout@v4

      - name: Deploy to production
        run: |
          echo "Add your deployment commands here"
          # Examples:
          # - Vercel: vercel --prod
          # - AWS: serverless deploy --stage prod
          # - K8s: kubectl apply -f k8s/
"""


GITLAB_CI_TEMPLATE = """# -*- coding: utf-8 -*-
# AgentFlow GitLab CI/CD Pipeline

stages:
  - lint
  - test
  - build
  - deploy

variables:
  PYTHON_VERSION: "{python_version}"
  PIP_CACHE_DIR: "$CI_PROJECT_DIR/.cache/pip"

cache:
  paths:
    - .cache/pip
    - .venv

# ============================================================================
# Lint & Type Check
# ============================================================================
lint:
  stage: lint
  image: python:${{PYTHON_VERSION}}
  script:
    - pip install ruff mypy
    - pip install -e ".[dev]"
    - ruff check .
    - ruff format --check .
    - mypy .

# ============================================================================
# Test
# ============================================================================
test:
  stage: test
  image: python:${{PYTHON_VERSION}}
  script:
    - pip install -e ".[dev]"
    - {test_command}
  coverage: '/TOTAL.*\\s+(\\d+%)/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml

# ============================================================================
# Build Docker Image
# ============================================================================
build:
  stage: build
  image: docker:24
  services:
    - docker:24-dind
  variables:
    DOCKER_TLS_CERTDIR: "/certs"
  before_script:
    - docker login -u $CI_REGISTRY_USER -p $CI_REGISTRY_PASSWORD $CI_REGISTRY
  script:
    - docker build -t $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA -t $CI_REGISTRY_IMAGE:latest .
    - docker push $CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
    - docker push $CI_REGISTRY_IMAGE:latest
  only:
    - main

# ============================================================================
# Deploy
# ============================================================================
deploy:staging:
  stage: deploy
  script:
    - echo "Deploy to staging"
  environment:
    name: staging
    url: https://staging.example.com
  only:
    - develop

deploy:production:
  stage: deploy
  script:
    - echo "Deploy to production"
  environment:
    name: production
    url: https://example.com
  only:
    - main
  when: manual
"""


def generate_github_actions(
    output_dir: str | Path,
    config: CICDConfig | None = None,
    **kwargs: Any,
) -> Path:
    """GitHub Actions 設定を生成.

    Args:
        output_dir: 出力ディレクトリ
        config: CI/CD 設定
        **kwargs: CICDConfig への引数

    Returns:
        生成されたファイルパス
    """
    if config is None:
        config = CICDConfig(**kwargs)

    output_path = Path(output_dir) / ".github" / "workflows"
    output_path.mkdir(parents=True, exist_ok=True)

    branches = ", ".join(f'"{b}"' for b in config.branches)

    content = GITHUB_ACTIONS_TEMPLATE.format(
        app_name=config.app_name,
        python_version=config.python_version,
        test_command=config.test_command,
        branches=branches,
    )

    workflow_path = output_path / "ci-cd.yml"
    workflow_path.write_text(content, encoding="utf-8")

    logger.info(f"Generated GitHub Actions workflow: {workflow_path}")
    return workflow_path


def generate_gitlab_ci(
    output_dir: str | Path,
    config: CICDConfig | None = None,
    **kwargs: Any,
) -> Path:
    """GitLab CI 設定を生成.

    Args:
        output_dir: 出力ディレクトリ
        config: CI/CD 設定
        **kwargs: CICDConfig への引数

    Returns:
        生成されたファイルパス
    """
    if config is None:
        config = CICDConfig(**kwargs)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    content = GITLAB_CI_TEMPLATE.format(
        python_version=config.python_version,
        test_command=config.test_command,
    )

    gitlab_path = output_path / ".gitlab-ci.yml"
    gitlab_path.write_text(content, encoding="utf-8")

    logger.info(f"Generated GitLab CI config: {gitlab_path}")
    return gitlab_path


def generate_pre_commit_config(output_dir: str | Path) -> Path:
    """pre-commit 設定を生成.

    Args:
        output_dir: 出力ディレクトリ

    Returns:
        生成されたファイルパス
    """
    pre_commit_content = """# -*- coding: utf-8 -*-
# Pre-commit hooks configuration
# Install: pre-commit install
# Run: pre-commit run --all-files

repos:
  # Ruff - Fast Python linter and formatter
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.4.0
    hooks:
      - id: ruff
        args: [--fix]
      - id: ruff-format

  # Mypy - Static type checker
  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.9.0
    hooks:
      - id: mypy
        additional_dependencies:
          - types-pyyaml
          - types-aiofiles

  # General hooks
  - repo: https://github.com/pre-commit/pre-commit-hooks
    rev: v4.5.0
    hooks:
      - id: trailing-whitespace
      - id: end-of-file-fixer
      - id: check-yaml
      - id: check-added-large-files
      - id: check-merge-conflict
      - id: debug-statements
      - id: detect-private-key

  # Detect secrets
  - repo: https://github.com/Yelp/detect-secrets
    rev: v1.4.0
    hooks:
      - id: detect-secrets
        args: ["--baseline", ".secrets.baseline"]
"""

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    pre_commit_path = output_path / ".pre-commit-config.yaml"
    pre_commit_path.write_text(pre_commit_content, encoding="utf-8")

    logger.info(f"Generated pre-commit config: {pre_commit_path}")
    return pre_commit_path

