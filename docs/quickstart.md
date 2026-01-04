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

- Python 3.13 以上
- pip パッケージマネージャー

## インストール

```bash
pip install agentflow
```

## 0. 最速スタート（5行で動く）

```python
from agentflow.engines import SimpleEngine
from agentflow.core.agent_block import AgentBlock

class MyAgent(AgentBlock):
    async def run(self, input_data: dict) -> dict:
        return {"result": f"処理: {input_data.get('task', '')}"}

# SimpleEngine で単一Agentを実行
engine = SimpleEngine(agent=MyAgent)
result = await engine.run({"task": "hello"})
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
├── main.py             # FastAPI + AgentFlow統合
├── engine.py           # Engine定義（推奨）
├── agents/             # Agent実装
├── skills/             # SKILL.md形式プロンプト
├── schemas/            # Pydantic入出力定義
├── agent.yaml          # メタデータ
└── requirements.txt    # 依存関係
```

## 2. エージェントメタデータの設定

`agent.yaml` を編集してエージェントの情報を設定します：

```yaml
meta:
  id: my-first-agent
  name: My First Agent
  version: 0.1.0
  description: 私の最初の AgentFlow エージェント
  author: Your Name
  license: MIT
  icon: 🤖
  category: utility

protocols:
  mcp: true
  a2a:
    enabled: true
    endpoint: http://localhost:8000
  agui: true

inputs:
  - name: message
    type: string
    description: 処理するメッセージ
    required: true

outputs:
  - name: result
    type: string
    description: 処理結果

skills:
  - name: process
    description: メッセージを処理する
    inputs:
      - message
    outputs:
      - result
```

## 3. エージェントの実装（推奨パターン）

`main.py` を編集してエージェントのロジックを実装します：

```python
"""My First Agent - FastAPI + AgentFlow統合."""

from typing import Any
from fastapi import FastAPI
from pydantic import BaseModel

from agentflow.engines import SimpleEngine
from agentflow.core.agent_block import AgentBlock


# Agent定義
class MyFirstAgent(AgentBlock):
    """メッセージを処理するシンプルなエージェント."""

    name: str = "MyFirstAgent"

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """メッセージを処理."""
        message = input_data.get("message", "")
        return {
            "result": f"処理完了: {message.upper()}",
            "original": message,
        }


# Engine定義（SimpleEngine を使用）
engine = SimpleEngine(agent=MyFirstAgent)


# FastAPI アプリ
app = FastAPI(title="My First Agent")


class TaskRequest(BaseModel):
    message: str


@app.post("/api/process")
async def process(request: TaskRequest) -> dict:
    """同期処理エンドポイント."""
    result = await engine.run({"message": request.message})
    return {"status": "success", "data": result}


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
```

## 4. エージェントの実行

### CLI から実行

```bash
agentflow run . --input '{"message": "hello world"}'
```

出力：

```json
{
  "result": "処理完了: HELLO WORLD",
  "original": "hello world",
  "length": 11
}
```

### Python スクリプトから実行

```python
import asyncio
from agentflow.engines import SimpleEngine
from agentflow.core.agent_block import AgentBlock

class MyAgent(AgentBlock):
    async def run(self, input_data: dict) -> dict:
        return {"result": f"処理: {input_data.get('message', '')}"}

async def main():
    # Engine を作成
    engine = SimpleEngine(agent=MyAgent)
    
    # 実行
    result = await engine.run({"message": "hello world"})
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
  "message": "hello from file"
}
```

ファイルから実行：

```bash
agentflow run . --input input.json --output output.json
```

## 7. 次のステップ

- [インストールガイド](../INSTALLATION_GUIDE_JA.md) - 詳細なセットアップ手順
- [Engine パターンガイド](../README.md#方式4-engine-pattern配置即用new) - 4種類のEngine詳細
- [CLI ガイド](guide-cli.md) - CLI コマンドの詳細
- [Skills ガイド](guide-skills.md) - Skills 自動進化システム
- [サンプル集](../examples/) - より高度な例

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
