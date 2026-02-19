"""Workflow コード生成モジュール.

Studio で作成したワークフローを実行可能なコードに変換します。

機能:
- Workflow JSON → Python コード
- FastAPI アプリケーション生成
- CLI ツール生成
- テストコード生成
"""

from __future__ import annotations

import json
import logging
import zipfile
from dataclasses import dataclass, field
from io import BytesIO
from typing import Any, Literal


logger = logging.getLogger(__name__)


@dataclass
class WorkflowNode:
    """ワークフローノード.

    Attributes:
        id: ノード ID
        agent_type: エージェントタイプ
        config: 設定
        position: 位置
    """

    id: str
    agent_type: str
    config: dict[str, Any] = field(default_factory=dict)
    position: dict[str, float] = field(default_factory=dict)


@dataclass
class WorkflowEdge:
    """ワークフローエッジ.

    Attributes:
        id: エッジ ID
        source: ソースノード ID
        target: ターゲットノード ID
    """

    id: str
    source: str
    target: str


@dataclass
class Workflow:
    """ワークフロー定義.

    Attributes:
        id: ワークフロー ID
        name: ワークフロー名
        description: 説明
        nodes: ノードリスト
        edges: エッジリスト
    """

    id: str
    name: str
    description: str = ""
    nodes: list[WorkflowNode] = field(default_factory=list)
    edges: list[WorkflowEdge] = field(default_factory=list)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> Workflow:
        """辞書からワークフローを作成.

        Args:
            data: ワークフローデータ

        Returns:
            ワークフローインスタンス
        """
        nodes = [
            WorkflowNode(
                id=n["id"],
                agent_type=n.get("data", {}).get("agentType", "UnknownAgent"),
                config=n.get("data", {}).get("config", {}),
                position=n.get("position", {}),
            )
            for n in data.get("nodes", [])
        ]
        edges = [
            WorkflowEdge(
                id=e["id"],
                source=e["source"],
                target=e["target"],
            )
            for e in data.get("edges", [])
        ]
        return cls(
            id=data.get("id", "unknown"),
            name=data.get("name", "Unknown Workflow"),
            description=data.get("description", ""),
            nodes=nodes,
            edges=edges,
        )


@dataclass
class CodeGenConfig:
    """コード生成設定.

    Attributes:
        workflow: ワークフロー
        output_type: 出力タイプ
        app_name: アプリケーション名
        version: バージョン
        include_tests: テストコードを含めるか
        include_readme: README を含めるか
    """

    workflow: Workflow
    output_type: Literal["fastapi", "cli", "package", "vercel", "lambda", "docker"]
    app_name: str = ""
    version: str = "1.0.0"
    include_tests: bool = True
    include_readme: bool = True

    def __post_init__(self) -> None:
        """初期化後処理."""
        if not self.app_name:
            self.app_name = self.workflow.name.lower().replace(" ", "-")


class WorkflowCodeGenerator:
    """ワークフローコード生成器."""

    def __init__(self, config: CodeGenConfig) -> None:
        """初期化.

        Args:
            config: コード生成設定
        """
        self.config = config
        self.workflow = config.workflow

    def _get_execution_order(self) -> list[WorkflowNode]:
        """実行順序を取得（トポロジカルソート）.

        Returns:
            実行順序のノードリスト
        """
        # ノード ID → ノードのマップ
        node_map = {node.id: node for node in self.workflow.nodes}

        # 入次数を計算
        in_degree: dict[str, int] = {node.id: 0 for node in self.workflow.nodes}
        adjacency: dict[str, list[str]] = {node.id: [] for node in self.workflow.nodes}

        for edge in self.workflow.edges:
            in_degree[edge.target] += 1
            adjacency[edge.source].append(edge.target)

        # 入次数が0のノードからスタート
        queue = [node_id for node_id, degree in in_degree.items() if degree == 0]
        result: list[WorkflowNode] = []

        while queue:
            node_id = queue.pop(0)
            result.append(node_map[node_id])

            for next_id in adjacency[node_id]:
                in_degree[next_id] -= 1
                if in_degree[next_id] == 0:
                    queue.append(next_id)

        return result

    def _generate_agent_imports(self) -> str:
        """エージェントインポートを生成.

        Returns:
            インポート文字列
        """
        agent_types = {node.agent_type for node in self.workflow.nodes}
        imports = [
            "from agentflow import create_flow",
            "from agentflow.core.agent_block import AgentBlock",
        ]

        for agent_type in sorted(agent_types):
            # エージェントタイプからインポート文を生成
            # 実際のプロジェクトではエージェントの場所を特定する必要がある
            imports.append(f"# from your_agents import {agent_type}")

        return "\n".join(imports)

    def _generate_flow_code(self) -> str:
        """フローコードを生成.

        Returns:
            フローコード
        """
        ordered_nodes = self._get_execution_order()

        lines = [
            "# ワークフロー定義",
            f'flow = create_flow("{self.workflow.id}")',
        ]

        for node in ordered_nodes:
            config_str = json.dumps(node.config, ensure_ascii=False, indent=8)
            lines.append(f"    .then({node.agent_type}, config={config_str})  # {node.id}")

        lines.append("    .build()")

        return "\n".join(lines)

    def generate_fastapi(self) -> dict[str, str]:
        """FastAPI アプリケーションを生成.

        Returns:
            ファイル名 → 内容のマップ
        """
        files: dict[str, str] = {}

        # app.py
        app_content = f'''# -*- coding: utf-8 -*-
"""Generated FastAPI Application.

Workflow: {self.workflow.name}
Description: {self.workflow.description}
Generated by AgentFlow Studio
"""

from contextlib import asynccontextmanager
from typing import Any

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field

{self._generate_agent_imports()}


# ============================================================================
# ワークフロー定義
# ============================================================================
{self._generate_flow_code()}


# ============================================================================
# リクエスト/レスポンスモデル
# ============================================================================
class WorkflowInput(BaseModel):
    """ワークフロー入力."""

    data: dict[str, Any] = Field(default_factory=dict, description="入力データ")


class WorkflowOutput(BaseModel):
    """ワークフロー出力."""

    status: str = Field(..., description="実行ステータス")
    result: dict[str, Any] | None = Field(None, description="実行結果")
    error: str | None = Field(None, description="エラーメッセージ")


# ============================================================================
# アプリケーション
# ============================================================================
@asynccontextmanager
async def lifespan(app: FastAPI):
    """アプリケーションライフサイクル."""
    # スタートアップ
    yield
    # シャットダウン


app = FastAPI(
    title="{self.workflow.name}",
    description="{self.workflow.description}",
    version="{self.config.version}",
    lifespan=lifespan,
)

# CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/")
async def root() -> dict[str, str]:
    """ルートエンドポイント."""
    return {{"message": "{self.workflow.name} is running!"}}


@app.get("/health")
async def health() -> dict[str, str]:
    """ヘルスチェック."""
    return {{"status": "healthy"}}


@app.post("/run", response_model=WorkflowOutput)
async def run_workflow(input_data: WorkflowInput) -> WorkflowOutput:
    """ワークフローを実行.

    Args:
        input_data: 入力データ

    Returns:
        実行結果
    """
    try:
        result = await flow.run(input_data.data)
        return WorkflowOutput(status="success", result=result)
    except Exception as e:
        return WorkflowOutput(status="error", error=str(e))


@app.post("/stream")
async def stream_workflow(input_data: WorkflowInput) -> StreamingResponse:
    """ワークフローをストリーム実行.

    Args:
        input_data: 入力データ

    Returns:
        SSE ストリーム
    """
    import json

    async def event_generator():
        try:
            async for event in flow.run_stream(input_data.data):
                yield f"data: {{json.dumps(event, ensure_ascii=False)}}\\n\\n"
            yield "data: {{\\"type\\": \\"complete\\"}}\\n\\n"
        except Exception as e:
            yield f"data: {{\\"type\\": \\"error\\", \\"message\\": \\"{{e!s}}\\"}}\\n\\n"

    return StreamingResponse(event_generator(), media_type="text/event-stream")


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
'''
        files["app.py"] = app_content

        # requirements.txt
        files["requirements.txt"] = """agentflow>=1.0.0
fastapi>=0.100.0
uvicorn>=0.23.0
pydantic>=2.0.0
httpx>=0.24.0
"""

        # README
        if self.config.include_readme:
            files["README.md"] = self._generate_readme("fastapi")

        # テスト
        if self.config.include_tests:
            files["tests/__init__.py"] = ""
            files["tests/test_app.py"] = self._generate_tests("fastapi")

        return files

    def generate_cli(self) -> dict[str, str]:
        """CLI ツールを生成.

        Returns:
            ファイル名 → 内容のマップ
        """
        files: dict[str, str] = {}

        cli_content = f'''# -*- coding: utf-8 -*-
"""Generated CLI Tool.

Workflow: {self.workflow.name}
Description: {self.workflow.description}
Generated by AgentFlow Studio
"""

import asyncio
import json
import sys
from pathlib import Path
from typing import Any

import click

{self._generate_agent_imports()}


# ============================================================================
# ワークフロー定義
# ============================================================================
{self._generate_flow_code()}


@click.group()
@click.version_option(version="{self.config.version}")
def cli():
    """{self.workflow.name} CLI."""
    pass


@cli.command()
@click.option("--input", "-i", "input_file", type=click.Path(exists=True), help="入力 JSON ファイル")
@click.option("--output", "-o", "output_file", type=click.Path(), help="出力 JSON ファイル")
@click.option("--data", "-d", help="入力データ (JSON 文字列)")
def run(input_file: str | None, output_file: str | None, data: str | None):
    """ワークフローを実行."""
    # 入力データを準備
    input_data: dict[str, Any] = {{}}

    if input_file:
        with open(input_file, encoding="utf-8") as f:
            input_data = json.load(f)
    elif data:
        input_data = json.loads(data)

    # 実行
    async def _run():
        return await flow.run(input_data)

    try:
        result = asyncio.run(_run())

        if output_file:
            with open(output_file, "w", encoding="utf-8") as f:
                json.dump(result, f, ensure_ascii=False, indent=2)
            click.echo(f"結果を {{output_file}} に保存しました")
        else:
            click.echo(json.dumps(result, ensure_ascii=False, indent=2))

    except Exception as e:
        click.echo(f"エラー: {{e}}", err=True)
        sys.exit(1)


@cli.command()
def info():
    """ワークフロー情報を表示."""
    click.echo(f"ワークフロー: {self.workflow.name}")
    click.echo(f"説明: {self.workflow.description}")
    click.echo(f"バージョン: {self.config.version}")
    click.echo(f"ノード数: {{len(flow.nodes)}}")


if __name__ == "__main__":
    cli()
'''
        files["cli.py"] = cli_content

        # pyproject.toml
        files["pyproject.toml"] = f"""[build-system]
requires = ["hatchling"]
build-backend = "hatchling.build"

[project]
name = "{self.config.app_name}"
version = "{self.config.version}"
description = "{self.workflow.description}"
readme = "README.md"
requires-python = ">=3.11"
dependencies = [
    "agentflow>=1.0.0",
    "click>=8.0.0",
]

[project.scripts]
{self.config.app_name} = "cli:cli"
"""

        if self.config.include_readme:
            files["README.md"] = self._generate_readme("cli")

        return files

    def generate_vercel(self) -> dict[str, str]:
        """Vercel Function を生成.

        Returns:
            ファイル名 → 内容のマップ
        """
        files = self.generate_fastapi()

        # vercel.json
        vercel_config = {
            "$schema": "https://openapi.vercel.sh/vercel.json",
            "version": 2,
            "builds": [
                {
                    "src": "app.py",
                    "use": "@vercel/python",
                    "config": {"maxLambdaSize": "50mb"},
                }
            ],
            "routes": [
                {"src": "/(.*)", "dest": "app.py"},
            ],
            "functions": {"app.py": {"memory": 1024, "maxDuration": 30}},
        }
        files["vercel.json"] = json.dumps(vercel_config, indent=2)

        # requirements.txt に mangum を追加
        files["requirements.txt"] += "mangum>=0.17.0\n"

        return files

    def generate_docker(self) -> dict[str, str]:
        """Docker ファイルを生成.

        Returns:
            ファイル名 → 内容のマップ
        """
        files = self.generate_fastapi()

        # Dockerfile
        files["Dockerfile"] = f"""# -*- coding: utf-8 -*-
# {self.workflow.name} Docker Image
#
# Build: docker build -t {self.config.app_name} .
# Run: docker run -p 8000:8000 {self.config.app_name}

FROM python:3.13-slim

WORKDIR /app

# Install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application
COPY . .

# Create non-root user
RUN groupadd -r appuser && useradd -r -g appuser appuser
USER appuser

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \\
    CMD curl -f http://localhost:8000/health || exit 1

EXPOSE 8000

CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8000"]
"""

        # docker-compose.yml
        files["docker-compose.yml"] = f"""version: "3.9"

services:
  app:
    build: .
    container_name: {self.config.app_name}
    ports:
      - "8000:8000"
    environment:
      - LOG_LEVEL=INFO
    restart: unless-stopped
"""

        # .dockerignore
        files[".dockerignore"] = """__pycache__
*.pyc
.git
.env
.venv
tests/
"""

        return files

    def _generate_readme(self, output_type: str) -> str:
        """README を生成.

        Args:
            output_type: 出力タイプ

        Returns:
            README 内容
        """
        usage = {
            "fastapi": """```bash
# 開発サーバーを起動
uvicorn app:app --reload

# リクエスト例
curl -X POST http://localhost:8000/run \\
    -H "Content-Type: application/json" \\
    -d '{"data": {"question": "Hello!"}}'
```""",
            "cli": f"""```bash
# インストール
pip install -e .

# 実行
{self.config.app_name} run --data '{{"question": "Hello!"}}'

# ファイル入力
{self.config.app_name} run --input input.json --output output.json
```""",
            "vercel": """```bash
# デプロイ
vercel deploy

# プロダクション
vercel deploy --prod
```""",
            "docker": f"""```bash
# ビルド
docker build -t {self.config.app_name} .

# 実行
docker run -p 8000:8000 {self.config.app_name}

# docker-compose
docker compose up
```""",
        }

        return f"""# {self.workflow.name}

{self.workflow.description}

## 概要

このプロジェクトは AgentFlow Studio で作成されたワークフローから自動生成されました。

## セットアップ

```bash
# 依存関係をインストール
pip install -r requirements.txt
```

## 使用方法

{usage.get(output_type, "")}

## ワークフロー構成

| ノード | エージェント | 説明 |
|--------|------------|------|
{chr(10).join(f"| {node.id} | {node.agent_type} | - |" for node in self.workflow.nodes)}

## ライセンス

MIT License

---

*Generated by AgentFlow Studio v0.2.0*
"""

    def _generate_tests(self, output_type: str) -> str:
        """テストコードを生成.

        Args:
            output_type: 出力タイプ

        Returns:
            テストコード
        """
        if output_type == "fastapi":
            return f'''# -*- coding: utf-8 -*-
"""Tests for {self.workflow.name}."""

import pytest
from fastapi.testclient import TestClient

from app import app


@pytest.fixture
def client():
    """テストクライアント."""
    return TestClient(app)


def test_root(client):
    """ルートエンドポイントのテスト."""
    response = client.get("/")
    assert response.status_code == 200
    assert "message" in response.json()


def test_health(client):
    """ヘルスチェックのテスト."""
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json()["status"] == "healthy"


def test_run_workflow(client):
    """ワークフロー実行のテスト."""
    response = client.post(
        "/run",
        json={{"data": {{"test": "input"}}}}
    )
    assert response.status_code == 200
    assert "status" in response.json()
'''
        return ""

    def generate(self) -> dict[str, str]:
        """コードを生成.

        Returns:
            ファイル名 → 内容のマップ
        """
        generators = {
            "fastapi": self.generate_fastapi,
            "cli": self.generate_cli,
            "vercel": self.generate_vercel,
            "docker": self.generate_docker,
            "lambda": self.generate_vercel,  # Lambda も Vercel と同様
            "package": self.generate_cli,  # Package も CLI と同様
        }

        generator = generators.get(self.config.output_type)
        if not generator:
            msg = f"Unknown output type: {self.config.output_type}"
            raise ValueError(msg)

        return generator()

    def generate_zip(self) -> BytesIO:
        """ZIP ファイルを生成.

        Returns:
            ZIP ファイルの BytesIO
        """
        files = self.generate()

        zip_buffer = BytesIO()
        with zipfile.ZipFile(zip_buffer, "w", zipfile.ZIP_DEFLATED) as zip_file:
            for filename, content in files.items():
                zip_file.writestr(filename, content)

        zip_buffer.seek(0)
        return zip_buffer


def generate_workflow_code(
    workflow_data: dict[str, Any],
    output_type: Literal["fastapi", "cli", "package", "vercel", "lambda", "docker"],
    app_name: str = "",
    version: str = "1.0.0",
    include_tests: bool = True,
    include_readme: bool = True,
) -> dict[str, str]:
    """ワークフローからコードを生成.

    Args:
        workflow_data: ワークフローデータ
        output_type: 出力タイプ
        app_name: アプリケーション名
        version: バージョン
        include_tests: テストコードを含めるか
        include_readme: README を含めるか

    Returns:
        ファイル名 → 内容のマップ
    """
    workflow = Workflow.from_dict(workflow_data)
    config = CodeGenConfig(
        workflow=workflow,
        output_type=output_type,
        app_name=app_name,
        version=version,
        include_tests=include_tests,
        include_readme=include_readme,
    )
    generator = WorkflowCodeGenerator(config)
    return generator.generate()


def generate_workflow_zip(
    workflow_data: dict[str, Any],
    output_type: Literal["fastapi", "cli", "package", "vercel", "lambda", "docker"],
    app_name: str = "",
    version: str = "1.0.0",
) -> BytesIO:
    """ワークフローから ZIP ファイルを生成.

    Args:
        workflow_data: ワークフローデータ
        output_type: 出力タイプ
        app_name: アプリケーション名
        version: バージョン

    Returns:
        ZIP ファイルの BytesIO
    """
    workflow = Workflow.from_dict(workflow_data)
    config = CodeGenConfig(
        workflow=workflow,
        output_type=output_type,
        app_name=app_name,
        version=version,
    )
    generator = WorkflowCodeGenerator(config)
    return generator.generate_zip()
