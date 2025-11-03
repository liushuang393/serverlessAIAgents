# 実装ガイド

AgentFlow の各層の実装方法とベストプラクティスを説明します。

## 📋 目次

1. [エージェント層の実装](#エージェント層の実装)
2. [プロトコル層の実装](#プロトコル層の実装)
3. [エンジン層の実装](#エンジン層の実装)
4. [ツール層の実装](#ツール層の実装)
5. [デバッグとトラブルシューティング](#デバッグとトラブルシューティング)

---

## エージェント層の実装

### 基本的なエージェント

```python
from agentflow.core.agent_block import AgentBlock
from typing import Any

class MyAgent(AgentBlock):
    """カスタムエージェント."""
    
    async def initialize(self) -> None:
        """初期化処理."""
        await super().initialize()
        # データベース接続、モデルロードなど
        self.model = await self._load_model()
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """メイン処理."""
        # 入力検証
        if "text" not in input_data:
            raise ValueError("text は必須です")
        
        # 処理実行
        result = await self._process(input_data["text"])
        
        return {"result": result}
    
    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        # リソース解放
        if hasattr(self, "model"):
            await self.model.close()
        await super().cleanup()
```

### プロトコル対応エージェント

`@auto_adapt` デコレーターを使用して自動的にプロトコル対応：

```python
from agentflow.core.auto_adapt import auto_adapt

@auto_adapt(protocols=["mcp", "a2a", "agui"])
class MultiProtocolAgent(AgentBlock):
    """複数プロトコル対応エージェント."""
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # 実装
        return {"result": "..."}

# 自動的に以下のメソッドが追加される：
# - get_mcp_tools()
# - get_a2a_card()
# - create_agui_emitter()
```

---

## プロトコル層の実装

### MCP クライアント

```python
from agentflow.protocols.mcp_client import MCPClient

# クライアント作成
client = MCPClient()

# サーバーに接続
await client.connect("my-mcp-server")

# ツール定義を取得
tools = await client.get_tool_definitions()

# ツールを呼び出し
result = await client.call_tool(
    "mcp://my-mcp-server/my-tool",
    {"param1": "value1"}
)

# 切断
await client.disconnect()
```

### A2A サーバー

```python
from agentflow.protocols.a2a_server import A2AServer
from agentflow.protocols.a2a_types import AgentCard, SkillDefinition

# エージェントカード定義
card = AgentCard(
    agent_id="my-agent",
    name="My Agent",
    description="説明",
    skills=[
        SkillDefinition(
            name="process",
            description="データを処理",
            inputs={"text": "string"},
            outputs={"result": "string"}
        )
    ]
)

# ハンドラー定義
async def handle_process(inputs: dict[str, Any]) -> dict[str, Any]:
    return {"result": inputs["text"].upper()}

handlers = {"process": handle_process}

# サーバー起動
server = A2AServer()
server.register_agent(card, handlers)
await server.start(port=8000)
```

### AG-UI イベントエミッター

```python
from agentflow.protocols.agui_emitter import AGUIEventEmitter

# エミッター作成
emitter = AGUIEventEmitter(engine)

# フローにアタッチ
await emitter.attach_to_flow("my-flow-id")

# イベント送信
await emitter.emit_log("info", "処理開始", "my-agent")
await emitter.emit_progress(50, 100, "処理中...")
await emitter.emit_result({"status": "success"})
```

---

## エンジン層の実装

### ワークフロー定義

```python
from agentflow.core.engine import AgentFlowEngine
from pocketflow import AsyncFlow, AsyncNode

# エンジン作成
engine = AgentFlowEngine()

# ノード定義
async def node1(data: dict) -> dict:
    return {"result": data["input"] * 2}

async def node2(data: dict) -> dict:
    return {"final": data["result"] + 10}

# フロー作成
flow = AsyncFlow(
    nodes=[
        AsyncNode(id="n1", func=node1),
        AsyncNode(id="n2", func=node2),
    ],
    edges=[("n1", "n2")]
)

# フロー登録
engine.register_workflow("my-workflow", flow)

# 実行
result = await engine.execute("my-workflow", {"input": 5})
# result = {"final": 20}
```

---

## ツール層の実装

### MCP ツール作成

```python
from agentflow.tools.base import Tool

class MyTool(Tool):
    """カスタムツール."""
    
    def get_definition(self) -> dict:
        """ツール定義を返す."""
        return {
            "name": "my_tool",
            "description": "ツールの説明",
            "parameters": {
                "type": "object",
                "properties": {
                    "param1": {"type": "string"}
                },
                "required": ["param1"]
            }
        }
    
    async def execute(self, params: dict) -> dict:
        """ツールを実行."""
        result = await self._process(params["param1"])
        return {"result": result}
```

---

## デバッグとトラブルシューティング

### ログ設定

```python
import logging

# ログレベル設定
logging.basicConfig(level=logging.DEBUG)

# AgentFlow のログを有効化
logger = logging.getLogger("agentflow")
logger.setLevel(logging.DEBUG)
```

### よくある問題

#### 1. 型エラー

**問題**: mypy で型エラーが発生

**解決策**:
```python
# 型アノテーションを追加
async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
    ...
```

#### 2. 非同期エラー

**問題**: `RuntimeError: Event loop is closed`

**解決策**:
```python
# コンテキストマネージャーを使用
async with MyAgent() as agent:
    result = await agent.run(data)
```

#### 3. プロトコル接続エラー

**問題**: MCP サーバーに接続できない

**解決策**:
```python
# タイムアウトを設定
client = MCPClient(timeout=30.0)
await client.connect("server-name")
```

### デバッグツール

```bash
# 詳細ログ付きで実行
agentflow run my-agent --input '{}' --verbose

# テストモードで実行
agentflow run my-agent --input '{}' --test

# プロファイリング
python -m cProfile -o profile.stats agent.py
```

---

## パフォーマンス最適化

### 1. 非同期処理の活用

```python
# ❌ 悪い例（順次実行）
result1 = await task1()
result2 = await task2()

# ✅ 良い例（並列実行）
result1, result2 = await asyncio.gather(task1(), task2())
```

### 2. キャッシング

```python
from functools import lru_cache

class MyAgent(AgentBlock):
    @lru_cache(maxsize=128)
    def _expensive_operation(self, key: str) -> str:
        # 重い処理
        return result
```

### 3. バッチ処理

```python
async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
    items = input_data["items"]
    
    # バッチで処理
    batch_size = 10
    results = []
    for i in range(0, len(items), batch_size):
        batch = items[i:i+batch_size]
        batch_results = await self._process_batch(batch)
        results.extend(batch_results)
    
    return {"results": results}
```

---

詳細な API リファレンスは [api.md](api.md) を参照してください。

