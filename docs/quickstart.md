# AgentFlow クイックスタートガイド

このガイドでは、AgentFlow を使って最初の AI エージェントを作成し、実行する方法を説明します。

## 🎯 核心原則

```
【統一入口 = Engines】
すべてはEngineから始まる。4種類の予定義パターンから選択。

【層構造】
Engine（パターン） → Agent（実行） → Skill（プロンプト）
```

## 前提条件

- Python 3.13 以上（`adk-agui-middleware` 依存のため）
- pip パッケージマネージャー

## インストール

```bash
pip install agentflow
```

## 0. 最速スタート（5行で動く）

```python
import asyncio

from agentflow import AgentBlock, SimpleEngine

class MyAgent(AgentBlock):
    async def run(self, input_data: dict) -> dict:
        return {"result": f"処理: {input_data.get('task', '')}"}

async def main() -> None:
    engine = SimpleEngine(agent=MyAgent)
    result = await engine.run({"task": "hello"})
    print(result)

if __name__ == "__main__":
    asyncio.run(main())
```

## 1. プロジェクトの初期化

新しい AgentFlow プロジェクトを作成します：

```bash
agentflow init my-first-agent
cd my-first-agent
```

これにより、以下のファイルが生成されます：

```
my-first-agent/
├── agent.yaml          # メタデータ（プロトコル/入出力/可視化）
├── main.py             # FastAPI + Flow（create_flow）統合
├── README.md           # 使い方（生成）
├── requirements.txt    # 依存関係（生成）
├── .gitignore          # 生成
└── tests/              # テスト雛形
```

## 2. エージェントメタデータの設定

`agent.yaml` を編集してエージェントの情報を設定します。

Note:
- `agentflow init` / `agentflow create agent` が生成するテンプレートは、リポジトリ内のスキーマ（`agentflow/core/metadata.py`）と一致しないフィールドを含む場合があります。
- `agentflow run` 実行時に `SchemaValidationError` が出た場合は、このセクションの形式に合わせて `agent.yaml` を修正してください。

```yaml
meta:
  id: my-first-agent
  name: My First Agent
  version: 1.0.0
  author: Your Name
  icon: 🤖
  category: general
  description: 私の最初の AgentFlow エージェント

interfaces:
  inputs:
    - name: input
      type: string
      required: true
      description: 入力テキスト
      default: ""

  outputs:
    - name: output
      type: string
      description: 出力テキスト

protocols:
  a2a:
    enabled: true
    skills: ["process"]
    card_path: null
  agui:
    enabled: true
    events: ["flow.start", "node.start", "progress", "node.complete", "flow.complete", "flow.error"]

dependencies:
  agents: []
  tools: []
  packages: []

pocketflow:
  entry: "main.py:flow"
  shared_schema: "schemas.py:SharedSchema"

visual:
  color: "#3B82F6"
  size: "medium"
  ports:
    input: [100, 50]
    output: [300, 50]
```

`schemas.py` も用意します（`pocketflow.shared_schema` の参照先）：

```python
from pydantic import BaseModel


class SharedSchema(BaseModel):
    input: str = ""
    output: str = ""
```

## 3. エージェントの実装（推奨パターン）

`main.py` を編集してエージェントのロジックを実装します：

```python
"""My First Agent - FastAPI + AgentFlow統合."""

import json
from typing import Any

from fastapi import FastAPI
from fastapi.responses import StreamingResponse
from pydantic import BaseModel

from agentflow import AgentBlock, create_flow


# Agent定義
class MyFirstAgent(AgentBlock):
    """メッセージを処理するシンプルなエージェント."""

    name: str = "MyFirstAgent"

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """メッセージを処理."""
        input_text = input_data.get("input", "")
        return {"output": f"Processed: {input_text}"}


# Flow 定義（agent.yaml の pocketflow.entry と一致させる）
flow = create_flow(
    agents=[MyFirstAgent()],
    pattern="sequential",
    enable_memory=True,
    name="MainFlow",
)


# FastAPI アプリ
app = FastAPI(title="My First Agent")


class TaskRequest(BaseModel):
    input: str


@app.post("/api/task")
async def process(request: TaskRequest) -> dict:
    """同期処理エンドポイント."""
    result = await flow.run({"input": request.input})
    return {"status": "success", "data": result}

@app.get("/api/task/stream")
async def stream(input_text: str) -> StreamingResponse:
    """ストリーム処理エンドポイント（SSE）."""
    async def event_generator():
        async for event in flow.run_stream({"input": input_text}):
            yield f"data: {json.dumps(event, ensure_ascii=False)}\n\n"

    return StreamingResponse(event_generator(), media_type="text/event-stream")


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
```

## 4. エージェントの実行

### CLI から実行

```bash
agentflow run . --input '{"input": "hello world"}' --json
```

出力：

```json
{
  "output": "Processed: hello world"
}
```

### workflow YAML を直接実行

`WorkflowService` ベースのワークフローは CLI から直接実行できます。

1. `workflow.yaml` を作成:

```yaml
workflow_type: reflection
task: "Summarize this text"
input_data:
  text: "hello world"
config:
  max_iterations: 1
```

2. 実行:

```bash
agentflow flow run workflow.yaml --json
```

### Python スクリプトから実行

```python
import asyncio
from agentflow.engines import SimpleEngine
from agentflow.core.agent_block import AgentBlock

class MyAgent(AgentBlock):
    async def run(self, input_data: dict) -> dict:
        return {"output": f"処理: {input_data.get('input', '')}"}

async def main():
    # Engine を作成
    engine = SimpleEngine(agent=MyAgent)
    
    # 実行
    result = await engine.run({"input": "hello world"})
    print(f"結果: {result}")

asyncio.run(main())
```

## 5. Engine パターンの選択

AgentFlow は4種類の予定義 Engine パターンを提供します：

### SimpleEngine - 単一Agent問答

```python
from agentflow.engines import SimpleEngine

engine = SimpleEngine(agent=MyAgent)
result = await engine.run({"question": "こんにちは"})
```

### GateEngine - 前置チェック付き

```python
from agentflow.engines import GateEngine

engine = GateEngine(
    gate_agent=ComplianceChecker,
    main_agent=ApprovalAgent,
    gate_check=lambda r: r.get("compliant", False),
)
result = await engine.run({"request": "..."})
```

### PipelineEngine - 複雑なフロー

```python
from agentflow.engines import PipelineEngine

engine = PipelineEngine(
    stages=[
        {"name": "gate", "agent": GateAgent, "gate": True},
        {"name": "analysis", "agents": [DaoAgent, FaAgent], "parallel": True},
        {"name": "review", "agent": ReviewAgent, "review": True},
    ],
    max_revisions=2,
)
result = await engine.run({"question": "..."})
```

### RAGEngine - ナレッジベース増強

```python
from agentflow.engines import RAGEngine

engine = RAGEngine(
    agent=KnowledgeAgent,
    vector_store="company_docs",
    top_k=5,
)
result = await engine.run({"query": "..."})
```

### SSEストリーミング

すべての Engine は `run_stream()` でリアルタイムイベントを配信：

```python
async for event in engine.run_stream({"question": "..."}):
    print(event)  # AG-UI イベント
```

## 6. 入力ファイルから実行

入力データを JSON ファイルに保存：

```json
// input.json
{
  "input": "hello from file"
}
```

ファイルから実行：

```bash
agentflow run . --input input.json --output output.json
```

## 7. 次のステップ

- [インストールガイド](../INSTALLATION_GUIDE_JA.md) - 詳細なセットアップ手順
- [Engine パターンガイド](./engines.md) - 4種類のEngine詳細
- [CLI ガイド](guide-cli.md) - CLI コマンドの詳細
- [Skills ガイド](guide-skills.md) - Skills 自動進化システム
- [サンプル集](./examples/README.md) - ドキュメント内サンプル一覧

## トラブルシューティング

### エラー: `agent.yaml` が見つからない

エージェントディレクトリに `agent.yaml` ファイルが存在することを確認してください。

### エラー: Python バージョンが古い

Python 3.13 以上が必要です。以下のコマンドでバージョンを確認：

```bash
python --version
```

### エラー: モジュールが見つからない

依存関係をインストール：

```bash
pip install -r requirements.txt
```

## サポート

問題が解決しない場合は、以下のリソースをご利用ください：

- 📖 [ドキュメント](https://agentflow.dev/docs)
- 💬 [Discord コミュニティ](https://discord.gg/agentflow)
- 🐛 [GitHub Issues](https://github.com/agentflow/agentflow/issues)
