"""Docker テンプレート生成モジュール.

Dockerfile と docker-compose.yml を生成します。

特徴:
- マルチステージビルド
- 最小イメージサイズ
- セキュリティベストプラクティス
- 本番対応設定
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


logger = logging.getLogger(__name__)


@dataclass
class DockerConfig:
    """Docker 設定.

    Attributes:
        app_name: アプリケーション名
        python_version: Python バージョン
        port: ポート番号
        entry_point: エントリーポイント
        health_check_path: ヘルスチェックパス
        env_vars: 環境変数
        extra_packages: 追加パッケージ
    """

    app_name: str = "agentflow-app"
    python_version: str = "3.13"
    port: int = 8000
    entry_point: str = "main:app"
    health_check_path: str = "/health"
    env_vars: dict[str, str] = field(default_factory=dict)
    extra_packages: list[str] = field(default_factory=list)


DOCKERFILE_TEMPLATE = """# -*- coding: utf-8 -*-
# AgentFlow Docker Image
#
# Build: docker build -t {app_name} .
# Run: docker run -p {port}:{port} {app_name}

# ============================================================================
# Stage 1: Builder
# ============================================================================
FROM python:{python_version}-slim as builder

WORKDIR /app

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \\
    build-essential \\
    curl \\
    && rm -rf /var/lib/apt/lists/*

# Install Python dependencies
COPY pyproject.toml README.md ./
COPY agentflow/ ./agentflow/

# Create virtual environment and install dependencies
RUN python -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

RUN pip install --no-cache-dir --upgrade pip && \\
    pip install --no-cache-dir -e ".[studio]"

# ============================================================================
# Stage 2: Runtime
# ============================================================================
FROM python:{python_version}-slim as runtime

WORKDIR /app

# Create non-root user for security
RUN groupadd -r agentflow && useradd -r -g agentflow agentflow

# Copy virtual environment from builder
COPY --from=builder /opt/venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"

# Copy application code
COPY --from=builder /app /app

# Set environment variables
ENV PYTHONDONTWRITEBYTECODE=1 \\
    PYTHONUNBUFFERED=1 \\
    PORT={port}

# Create necessary directories
RUN mkdir -p /app/data /app/logs && \\
    chown -R agentflow:agentflow /app

# Switch to non-root user
USER agentflow

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \\
    CMD curl -f http://localhost:{port}{health_check_path} || exit 1

# Expose port
EXPOSE {port}

# Entry point
CMD ["uvicorn", "{entry_point}", "--host", "0.0.0.0", "--port", "{port}"]
"""


DOCKER_COMPOSE_TEMPLATE = """# -*- coding: utf-8 -*-
# AgentFlow Docker Compose
#
# Development: docker compose up
# Production: docker compose -f docker-compose.yml -f docker-compose.prod.yml up

version: "3.9"

services:
  # ============================================================================
  # AgentFlow Application
  # ============================================================================
  app:
    build:
      context: .
      dockerfile: Dockerfile
    container_name: {app_name}
    ports:
      - "{port}:{port}"
    environment:
      - PORT={port}
      - LOG_LEVEL=INFO
{env_vars_section}
    volumes:
      - ./data:/app/data
      - ./logs:/app/logs
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:{port}{health_check_path}"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 5s
    restart: unless-stopped
    networks:
      - agentflow-network

  # ============================================================================
  # Redis (Optional - for caching/rate limiting)
  # ============================================================================
  redis:
    image: redis:7-alpine
    container_name: {app_name}-redis
    ports:
      - "6379:6379"
    volumes:
      - redis-data:/data
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 5s
      retries: 5
    restart: unless-stopped
    networks:
      - agentflow-network

  # ============================================================================
  # PostgreSQL (Optional - for persistent storage)
  # ============================================================================
  postgres:
    image: postgres:16-alpine
    container_name: {app_name}-postgres
    ports:
      - "5432:5432"
    environment:
      - POSTGRES_USER=agentflow
      - POSTGRES_PASSWORD=agentflow
      - POSTGRES_DB=agentflow
    volumes:
      - postgres-data:/var/lib/postgresql/data
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U agentflow"]
      interval: 10s
      timeout: 5s
      retries: 5
    restart: unless-stopped
    networks:
      - agentflow-network

networks:
  agentflow-network:
    driver: bridge

volumes:
  redis-data:
  postgres-data:
"""


def generate_dockerfile(
    output_dir: str | Path,
    config: DockerConfig | None = None,
    **kwargs: Any,
) -> Path:
    """Dockerfile を生成.

    Args:
        output_dir: 出力ディレクトリ
        config: Docker 設定
        **kwargs: DockerConfig への引数

    Returns:
        生成されたファイルパス
    """
    if config is None:
        config = DockerConfig(**kwargs)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    dockerfile_content = DOCKERFILE_TEMPLATE.format(
        app_name=config.app_name,
        python_version=config.python_version,
        port=config.port,
        entry_point=config.entry_point,
        health_check_path=config.health_check_path,
    )

    dockerfile_path = output_path / "Dockerfile"
    dockerfile_path.write_text(dockerfile_content, encoding="utf-8")

    logger.info(f"Generated Dockerfile: {dockerfile_path}")
    return dockerfile_path


def generate_docker_compose(
    output_dir: str | Path,
    config: DockerConfig | None = None,
    **kwargs: Any,
) -> Path:
    """docker-compose.yml を生成.

    Args:
        output_dir: 出力ディレクトリ
        config: Docker 設定
        **kwargs: DockerConfig への引数

    Returns:
        生成されたファイルパス
    """
    if config is None:
        config = DockerConfig(**kwargs)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # 環境変数セクションを生成
    env_vars_section = ""
    for key, value in config.env_vars.items():
        env_vars_section += f"      - {key}={value}\n"

    compose_content = DOCKER_COMPOSE_TEMPLATE.format(
        app_name=config.app_name,
        port=config.port,
        health_check_path=config.health_check_path,
        env_vars_section=env_vars_section,
    )

    compose_path = output_path / "docker-compose.yml"
    compose_path.write_text(compose_content, encoding="utf-8")

    logger.info(f"Generated docker-compose.yml: {compose_path}")
    return compose_path


def generate_dockerignore(output_dir: str | Path) -> Path:
    """`.dockerignore` を生成.

    Args:
        output_dir: 出力ディレクトリ

    Returns:
        生成されたファイルパス
    """
    dockerignore_content = """# Git
.git
.gitignore

# Python
__pycache__
*.py[cod]
*$py.class
*.so
.Python
build/
develop-eggs/
dist/
downloads/
eggs/
.eggs/
lib/
lib64/
parts/
sdist/
var/
wheels/
*.egg-info/
.installed.cfg
*.egg

# Virtual environments
.env
.venv
env/
venv/
ENV/
env.bak/
venv.bak/

# IDE
.idea/
.vscode/
*.swp
*.swo

# Testing
.tox/
.coverage
.coverage.*
htmlcov/
.pytest_cache/
coverage.xml
*.cover

# Documentation
docs/_build/
site/

# Local data
data/
logs/
*.log

# Secrets
.env.local
.env.*.local
*.pem
*.key
"""

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    dockerignore_path = output_path / ".dockerignore"
    dockerignore_path.write_text(dockerignore_content, encoding="utf-8")

    logger.info(f"Generated .dockerignore: {dockerignore_path}")
    return dockerignore_path

