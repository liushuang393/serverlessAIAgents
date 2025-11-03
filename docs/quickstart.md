# AgentFlow クイックスタートガイド

このガイドでは、AgentFlow を使って最初の AI エージェントを作成し、実行する方法を説明します。

## 前提条件

- Python 3.13 以上
- pip パッケージマネージャー

## インストール

```bash
pip install agentflow
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
├── agent.yaml          # エージェントメタデータ
├── main.py             # エージェント実装
├── requirements.txt    # 依存関係
├── .gitignore          # Git 除外設定
└── README.md           # プロジェクト説明
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

## 3. エージェントの実装

`main.py` を編集してエージェントのロジックを実装します：

```python
"""My First Agent - シンプルなメッセージ処理エージェント."""

from typing import Any
from agentflow.core.agent_block import AgentBlock


class MyFirstAgent(AgentBlock):
    """メッセージを処理するシンプルなエージェント."""

    async def initialize(self) -> None:
        """初期化処理."""
        await super().initialize()
        print("🚀 エージェントを初期化しました")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """
        メッセージを処理.

        Args:
            input_data: 入力データ（message キーを含む）

        Returns:
            処理結果（result キーを含む）
        """
        message = input_data.get("message", "")

        # メッセージを処理（例：大文字に変換）
        result = f"処理完了: {message.upper()}"

        return {
            "result": result,
            "original": message,
            "length": len(message),
        }

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        print("🧹 エージェントをクリーンアップしました")
        await super().cleanup()


# エージェントのエントリーポイント
if __name__ == "__main__":
    import asyncio

    async def main():
        async with MyFirstAgent(metadata_path="agent.yaml") as agent:
            result = await agent.run({"message": "hello world"})
            print(f"結果: {result}")

    asyncio.run(main())
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
from pathlib import Path
from agentflow.core.agent_block import AgentBlock

async def main():
    # エージェントをロード
    agent = MyFirstAgent(metadata_path="agent.yaml")

    # 初期化
    await agent.initialize()

    # 実行
    result = await agent.run({"message": "hello world"})
    print(f"結果: {result}")

    # クリーンアップ
    await agent.cleanup()

asyncio.run(main())
```

### コンテキストマネージャーを使用

```python
import asyncio

async def main():
    async with MyFirstAgent(metadata_path="agent.yaml") as agent:
        result = await agent.run({"message": "hello world"})
        print(f"結果: {result}")

asyncio.run(main())
```

## 5. プロトコル統合

### MCP ツールとして使用

```python
# MCP ツール定義を取得
tools = agent.get_mcp_tools()
print(tools)
```

出力：

```json
[
  {
    "name": "my-first-agent.process",
    "description": "メッセージを処理する",
    "inputSchema": {
      "type": "object",
      "properties": {
        "message": {
          "type": "string",
          "description": "処理するメッセージ"
        }
      },
      "required": ["message"]
    }
  }
]
```

### A2A エージェントとして公開

```python
from agentflow.protocols.a2a_server import A2AServer

# A2A サーバーを作成
server = A2AServer()

# エージェントを登録
card = agent.get_a2a_card()
handlers = {
    "process": lambda inputs: agent.run(inputs)
}
server.register_agent(card, handlers)

# タスクを処理
result = await server.handle_task(
    "My First Agent",
    "process",
    {"message": "hello"}
)
```

### AG-UI イベントストリーミング

```python
# イベントエミッターを作成
emitter = agent.create_agui_emitter(agent.engine)

# フローにアタッチ
await emitter.attach_to_flow("my-flow")

# ログを送信
await emitter.emit_log("info", "処理を開始します", "agent")

# イベントをストリーミング
async for event in emitter.stream_events():
    print(f"イベント: {event.event_type.value} - {event.data}")
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

- [API リファレンス](api.md) - 詳細な API ドキュメント
- [プロトコルガイド](protocols.md) - MCP/A2A/AG-UI の詳細
- [CLI リファレンス](cli.md) - CLI コマンドの詳細
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
