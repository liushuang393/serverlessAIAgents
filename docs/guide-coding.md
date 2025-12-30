# コーディング操作ガイド

> **使用シナリオ**: Python コードでエージェントを開発・カスタマイズしたい

AgentFlow は Python コードでエージェントを開発できるフレームワークです。型安全で非同期対応の API を提供し、柔軟なカスタマイズが可能です。

---

## 📋 目次

1. [インストール](#インストール)
2. [基本概念](#基本概念)
3. [エージェントの作成](#エージェントの作成)
4. [エージェントの実行](#エージェントの実行)
5. [プロトコル統合](#プロトコル統合)
6. [協調パターン](#協調パターン)
7. [注意事項](#注意事項)
8. [ベストプラクティス](#ベストプラクティス)
9. [トラブルシューティング](#トラブルシューティング)

---

## 📦 インストール

### 前提条件

- Python 3.13 以上
- pip パッケージマネージャー
- Python の async/await の基本的な理解
- 型ヒントの知識

### ステップ 1: 開発環境のセットアップ

```bash
# Conda 環境を作成（推奨）
conda env create -f environment.yml
conda activate agentflow

# 開発用依存関係をインストール
pip install -e ".[dev]"

# または通常のインストール
pip install -e .
```

### ステップ 2: インストール確認

```bash
# Python でインポート確認
python -c "import agentflow; print(agentflow.__version__)"

# 型チェック（開発環境の場合）
mypy agentflow
```

---

## 🎯 基本概念

### AgentBlock 基底クラス

すべてのエージェントは `AgentBlock` を継承します：

```python
from typing import Any
from agentflow.core.agent_block import AgentBlock

class MyAgent(AgentBlock):
    """カスタムエージェント."""
    
    async def initialize(self) -> None:
        """初期化処理."""
        await super().initialize()
        # ここで初期化処理を実装
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """メイン処理（必須実装）."""
        # ここでエージェントのロジックを実装
        return {"result": "..."}
    
    async def cleanup(self) -> None:
        """終了処理."""
        await super().cleanup()
        # ここでクリーンアップ処理を実装
```

### ライフサイクル

1. **`initialize()`**: エージェントの初期化
2. **`run()`**: メイン処理（必須）
3. **`cleanup()`**: 終了処理

---

## 🔨 エージェントの作成

### ステップ 1: プロジェクトの初期化

```bash
# CLI でプロジェクトを作成
agentflow init my-agent
cd my-agent

# または手動で作成
mkdir my-agent
cd my-agent
```

### ステップ 2: agent.yaml の作成

`agent.yaml` を作成：

```yaml
meta:
  id: my-agent
  name: My Agent
  version: 0.1.0
  description: カスタムエージェントの説明
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

### ステップ 3: main.py の実装

`main.py` を編集：

```python
"""My Agent - カスタムエージェントの実装."""

from typing import Any
from agentflow.core.agent_block import AgentBlock


class MyAgent(AgentBlock):
    """メッセージを処理するカスタムエージェント."""

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
        async with MyAgent(metadata_path="agent.yaml") as agent:
            result = await agent.run({"message": "hello world"})
            print(f"結果: {result}")

    asyncio.run(main())
```

### ステップ 4: テスト実行

```bash
# Python スクリプトとして実行
python main.py

# または CLI から実行
agentflow run . --input '{"message": "hello"}'
```

---

## 🚀 エージェントの実行

### 方法 1: コンテキストマネージャー（推奨）

```python
import asyncio
from my_agent import MyAgent

async def main():
    async with MyAgent(metadata_path="agent.yaml") as agent:
        result = await agent.run({"message": "hello"})
        print(result)

asyncio.run(main())
```

### 方法 2: 手動管理

```python
import asyncio
from my_agent import MyAgent

async def main():
    agent = MyAgent(metadata_path="agent.yaml")
    
    try:
        await agent.initialize()
        result = await agent.run({"message": "hello"})
        print(result)
    finally:
        await agent.cleanup()

asyncio.run(main())
```

### 方法 3: 複数回実行

```python
import asyncio
from my_agent import MyAgent

async def main():
    async with MyAgent(metadata_path="agent.yaml") as agent:
        # 複数回実行
        for message in ["hello", "world", "agentflow"]:
            result = await agent.run({"message": message})
            print(result)

asyncio.run(main())
```

---

## 🔌 プロトコル統合

### MCP ツールとして使用

```python
from agentflow.protocols.mcp_client import MCPClient

async def main():
    agent = MyAgent(metadata_path="agent.yaml")
    
    # MCP ツール定義を取得
    tools = agent.get_mcp_tools()
    print(tools)
    
    # MCP クライアントを作成
    client = MCPClient()
    # ツールを登録
    await client.register_tools(tools)
```

### A2A エージェントとして公開

```python
from agentflow.protocols.a2a_server import A2AServer

async def main():
    agent = MyAgent(metadata_path="agent.yaml")
    await agent.initialize()
    
    # A2A サーバーを作成
    server = A2AServer()
    
    # エージェントを登録
    card = agent.get_a2a_card()
    handlers = {
        "process": lambda inputs: agent.run(inputs)
    }
    server.register_agent(card, handlers)
    
    # サーバーを起動
    await server.start()
```

### AG-UI イベントストリーミング

```python
from agentflow.protocols.agui_emitter import AGUIEmitter

async def main():
    agent = MyAgent(metadata_path="agent.yaml")
    await agent.initialize()
    
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

---

## 🤝 協調パターン

### Supervisor パターン

```python
from agentflow.patterns.supervisor import SupervisorCoordinator

async def main():
    # スーパーバイザーエージェント
    supervisor = SupervisorAgent()
    
    # ワーカーエージェント
    workers = {
        "research": ResearchAgent(),
        "write": WriteAgent(),
    }
    
    # コーディネーターを作成
    coordinator = SupervisorCoordinator(
        supervisor=supervisor,
        workers=workers,
        max_iterations=10
    )
    
    # タスクを実行
    result = await coordinator.execute("市場調査レポート作成")
    print(result)
```

### Hierarchical パターン

```python
from agentflow.patterns.hierarchical import HierarchicalCoordinator

async def main():
    # 階層構造を定義
    hierarchy = {
        "manager": ManagerAgent(),
        "workers": {
            "task1": Task1Agent(),
            "task2": Task2Agent(),
        }
    }
    
    # コーディネーターを作成
    coordinator = HierarchicalCoordinator(hierarchy=hierarchy)
    
    # タスクを実行
    result = await coordinator.execute("複雑なタスク")
    print(result)
```

### Sequential パターン

```python
from agentflow.patterns.coordinator import CoordinatorBase

async def main():
    agents = [
        Agent1(),
        Agent2(),
        Agent3(),
    ]
    
    # 順次実行
    coordinator = CoordinatorBase(agents=agents, mode="sequential")
    result = await coordinator.execute(input_data)
    print(result)
```

---

## ⚠️ 注意事項

### 1. 型アノテーション

**必須**: すべての関数に完全な型アノテーションが必要です。

```python
# ✅ 良い例
async def process_data(
    input_data: dict[str, Any],
    *,
    timeout: float = 30.0,
) -> dict[str, Any]:
    ...

# ❌ 悪い例
async def process_data(input_data, timeout=30.0):
    ...
```

### 2. Async/Await

**必須**: すべての I/O 操作に async/await を使用。

```python
# ✅ 良い例
async def load_config(path: str) -> dict[str, Any]:
    async with aiofiles.open(path) as f:
        content = await f.read()
    return yaml.safe_load(content)

# ❌ 悪い例
def load_config(path: str) -> dict[str, Any]:
    with open(path) as f:  # ブロッキング!
        content = f.read()
    return yaml.safe_load(content)
```

### 3. エラーハンドリング

**必須**: 具体的な例外を使用し、bare `except` は使用しない。

```python
# ✅ 良い例
try:
    result = await agent.run(input_data)
except AgentError as e:
    logger.error(f"エージェントエラー: {e}")
    raise
except TimeoutError:
    logger.warning("タイムアウトしました")
    return default_result

# ❌ 悪い例
try:
    result = await agent.run(input_data)
except Exception:  # 広すぎる!
    pass
```

### 4. Docstring

**必須**: すべての公開関数、クラス、メソッドに Docstring が必要。

```python
def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
) -> ExecutionResult:
    """指定された入力でワークフローを実行.

    Args:
        workflow_id: ワークフローの一意識別子.
        inputs: ワークフローの入力パラメーター.

    Returns:
        ステータスと出力を含む ExecutionResult.

    Raises:
        WorkflowNotFoundError: ワークフローが存在しない場合.

    Example:
        >>> result = execute_workflow("my-workflow", {"key": "value"})
        >>> print(result.status)
        success
    """
    ...
```

### 5. リソース管理

**必須**: リソースは適切にクリーンアップする。

```python
# ✅ 良い例（コンテキストマネージャー）
async with MyAgent() as agent:
    result = await agent.run(input_data)

# ✅ 良い例（手動管理）
agent = MyAgent()
try:
    await agent.initialize()
    result = await agent.run(input_data)
finally:
    await agent.cleanup()

# ❌ 悪い例（リソースリーク）
agent = MyAgent()
result = await agent.run(input_data)  # cleanup が呼ばれない
```

---

## 💡 ベストプラクティス

### 1. 単一責任の原則

各エージェントは**1つの責任**だけを持つように設計：

```python
# ✅ 良い例: テキスト処理専用
class TextProcessorAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        text = input_data["text"]
        return {"processed": text.upper()}

# ❌ 悪い例: 複数の責任
class TextAndImageAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # テキスト処理と画像処理が混在
        ...
```

### 2. 設定の外部化

設定は `agent.yaml` や環境変数から読み込む：

```python
import os
from agentflow.core.agent_block import AgentBlock

class ConfigurableAgent(AgentBlock):
    async def initialize(self) -> None:
        await super().initialize()
        # 環境変数から設定を読み込み
        self.timeout = float(os.getenv("AGENT_TIMEOUT", "30.0"))
        self.api_key = os.getenv("API_KEY")
```

### 3. ロギング

適切なロギングを使用：

```python
import logging
from agentflow.core.agent_block import AgentBlock

logger = logging.getLogger(__name__)

class LoggingAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        logger.info("処理を開始します")
        try:
            result = self.process(input_data)
            logger.info("処理が完了しました")
            return result
        except Exception as e:
            logger.error(f"エラーが発生しました: {e}", exc_info=True)
            raise
```

### 4. テスト

テストを書く：

```python
import pytest
from my_agent import MyAgent

@pytest.mark.asyncio
async def test_my_agent():
    async with MyAgent(metadata_path="agent.yaml") as agent:
        result = await agent.run({"message": "hello"})
        assert "result" in result
        assert result["result"] == "処理完了: HELLO"
```

### 5. エラーメッセージ

明確なエラーメッセージを提供：

```python
class MyAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        if "message" not in input_data:
            raise ValueError(
                "入力データに 'message' キーが必要です。"
                f"提供されたキー: {list(input_data.keys())}"
            )
        ...
```

---

## 🔧 トラブルシューティング

### 問題 1: 型エラー

**症状**: `mypy` で型エラーが発生

**解決方法**:

```python
# 型を明示的に指定
from typing import Any

async def process(
    input_data: dict[str, Any]  # 型を明示
) -> dict[str, Any]:  # 戻り値の型を明示
    ...
```

### 問題 2: 非同期エラー

**症状**: `RuntimeError: This event loop is already running`

**解決方法**:

```python
# ✅ 良い例: asyncio.run() を使用
asyncio.run(main())

# ❌ 悪い例: 既存のイベントループ内で実行
loop = asyncio.get_event_loop()
loop.run_until_complete(main())  # エラーになる可能性
```

### 問題 3: リソースリーク

**症状**: リソースが解放されない

**解決方法**:

```python
# コンテキストマネージャーを使用
async with MyAgent() as agent:
    result = await agent.run(input_data)
# 自動的に cleanup() が呼ばれる
```

### 問題 4: メタデータエラー

**症状**: `agent.yaml` が見つからない、または形式が不正

**解決方法**:

```bash
# YAML の形式を確認
python -c "import yaml; yaml.safe_load(open('agent.yaml'))"

# パスを確認
agent = MyAgent(metadata_path="./agent.yaml")  # 相対パス
agent = MyAgent(metadata_path="/absolute/path/agent.yaml")  # 絶対パス
```

### 問題 5: インポートエラー

**症状**: `ModuleNotFoundError`

**解決方法**:

```bash
# インストール確認
pip show agentflow

# 開発モードで再インストール
pip install -e .

# Python パスを確認
python -c "import sys; print(sys.path)"
```

---

## 📚 次のステップ

- [Studio UI 操作ガイド](guide-studio-ui.md) - ビジュアルエディタでの操作
- [CLI 操作ガイド](guide-cli.md) - コマンドラインでの操作
- [API リファレンス](api.md) - 詳細な API ドキュメント
- [プロトコルガイド](protocols.md) - MCP/A2A/AG-UI の詳細
- [開発規範](DEVELOPMENT_STANDARDS_JA.md) - コーディング規約

---

## 🎓 実践例

### 例 1: シンプルなエージェント

```python
"""シンプルなテキスト処理エージェント."""

from typing import Any
from agentflow.core.agent_block import AgentBlock

class SimpleTextAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        text = input_data.get("text", "")
        return {"result": text.upper()}

if __name__ == "__main__":
    import asyncio
    async def main():
        async with SimpleTextAgent() as agent:
            result = await agent.run({"text": "hello"})
            print(result)
    asyncio.run(main())
```

### 例 2: 外部 API を呼び出すエージェント

```python
"""外部 API を呼び出すエージェント."""

import aiohttp
from typing import Any
from agentflow.core.agent_block import AgentBlock

class APIAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        url = input_data.get("url")
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                data = await response.json()
                return {"result": data}
```

### 例 3: 状態を持つエージェント

```python
"""状態を持つエージェント."""

from typing import Any
from agentflow.core.agent_block import AgentBlock

class StatefulAgent(AgentBlock):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.counter = 0
    
    async def initialize(self) -> None:
        await super().initialize()
        self.counter = 0
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        self.counter += 1
        return {"count": self.counter, "input": input_data}
```

---

**Python コードで AI エージェントを開発しましょう！** 🐍

