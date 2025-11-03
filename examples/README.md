# AgentFlow サンプル集

AgentFlow の実装例とベストプラクティスを紹介します。

## 📁 サンプル一覧

### 1. Text Processor Agent

**ディレクトリ:** `text_processor_agent/`
**カテゴリ:** テキスト処理
**難易度:** ⭐ 初級

**説明:** テキスト処理エージェントの基本実装例

**機能:**

- テキストの大文字/小文字変換
- 文字数カウント
- 単語数カウント
- カスタム初期化とクリーンアップ

**使用方法:**

```bash
# エージェントを実行
agentflow run examples/text_processor_agent \
  --input '{"text": "hello world", "operation": "upper"}'

# Python から実行
cd examples/text_processor_agent
python agent.py
```

**学べること:**

- `AgentBlock` の基本的な使い方
- `initialize()` と `cleanup()` のオーバーライド
- 入力検証とエラーハンドリング
- コンテキストマネージャーの使用

---

### 2. Weather Agent

**ディレクトリ:** `weather_agent/`
**カテゴリ:** ユーティリティ
**難易度:** ⭐ 初級

**説明:** 天気情報を取得するエージェント

**機能:**

- 指定された場所の天気情報を取得
- 気温、湿度、風速などの情報を提供
- 摂氏/華氏の単位変換

**使用方法:**

```bash
# エージェントを実行
agentflow run examples/weather_agent \
  --input '{"location": "東京", "units": "celsius"}'

# Python から実行
cd examples/weather_agent
python agent.py
```

**学べること:**

- 外部 API 統合のパターン
- ダミーデータの生成
- 単位変換の実装

---

### 3. Translator Agent

**ディレクトリ:** `translator_agent/`
**カテゴリ:** 言語処理
**難易度:** ⭐⭐ 中級

**説明:** テキストを翻訳するエージェント

**機能:**

- 複数言語間の翻訳
- 自動言語検出
- 翻訳信頼度の提供

**使用方法:**

```bash
# エージェントを実行
agentflow run examples/translator_agent \
  --input '{"text": "こんにちは", "target_lang": "en"}'

# Python から実行
cd examples/translator_agent
python agent.py
```

**学べること:**

- 言語検出の実装
- 辞書ベースの翻訳
- 信頼度スコアの計算

---

### 4. Calculator Agent

**ディレクトリ:** `calculator_agent/`
**カテゴリ:** ユーティリティ
**難易度:** ⭐⭐ 中級

**説明:** 数式を安全に計算するエージェント

**機能:**

- 数式の安全な評価（AST 使用）
- 基本的な算術演算のサポート
- エラーハンドリング

**使用方法:**

```bash
# エージェントを実行
agentflow run examples/calculator_agent \
  --input '{"expression": "2 + 3 * 4"}'

# Python から実行
cd examples/calculator_agent
python agent.py
```

**学べること:**

- AST を使用した安全な数式評価
- 演算子のマッピング
- セキュリティを考慮した実装

---

## 🎯 カテゴリ別サンプル

### 基本編

#### Hello World Agent

最もシンプルなエージェント実装：

```python
from agentflow.core.agent_block import AgentBlock
from typing import Any

class HelloAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        name = input_data.get("name", "World")
        return {"message": f"Hello, {name}!"}
```

#### Echo Agent

入力をそのまま返すエージェント：

```python
class EchoAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return {"echo": input_data}
```

### プロトコル統合編

#### MCP Tool Agent

MCP ツールを使用するエージェント：

```python
from agentflow.protocols.mcp_client import MCPClient

class FileReaderAgent(AgentBlock):
    async def initialize(self) -> None:
        await super().initialize()
        self.mcp = MCPClient()
        await self.mcp.connect("filesystem")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        content = await self.mcp.call_tool(
            "mcp://filesystem/read_file",
            {"path": input_data["file_path"]}
        )
        return {"content": content}

    async def cleanup(self) -> None:
        await self.mcp.disconnect("filesystem")
        await super().cleanup()
```

#### A2A Collaboration Agent

複数エージェントを協調させるエージェント：

```python
from agentflow.protocols.a2a_client import A2AClient

class OrchestratorAgent(AgentBlock):
    async def initialize(self) -> None:
        await super().initialize()
        self.a2a = A2AClient()

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # エージェント 1: データ取得
        data = await self.a2a.call_remote_agent(
            "http://localhost:8001",
            "fetch_data",
            {"source": input_data["source"]}
        )

        # エージェント 2: データ処理
        result = await self.a2a.call_remote_agent(
            "http://localhost:8002",
            "process_data",
            {"data": data}
        )

        return result
```

#### AG-UI Streaming Agent

リアルタイムログを送信するエージェント：

```python
class StreamingAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        emitter = self.create_agui_emitter(self.engine)
        await emitter.attach_to_flow("processing")

        await emitter.emit_log("info", "処理開始", "agent")

        # 長時間処理をシミュレート
        for i in range(5):
            await emitter.emit_log("info", f"ステップ {i+1}/5", "agent")
            await asyncio.sleep(1)

        await emitter.emit_log("success", "処理完了", "agent")
        await emitter.detach_from_flow("processing")

        return {"status": "completed"}
```

### 実用編

#### Data Pipeline Agent

データパイプラインを実装するエージェント：

```python
class DataPipelineAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # ステップ 1: データ読み込み
        data = await self.load_data(input_data["source"])

        # ステップ 2: データクリーニング
        cleaned = await self.clean_data(data)

        # ステップ 3: データ変換
        transformed = await self.transform_data(cleaned)

        # ステップ 4: データ保存
        await self.save_data(transformed, input_data["destination"])

        return {
            "status": "success",
            "records_processed": len(transformed)
        }

    async def load_data(self, source: str) -> list[dict]:
        # データ読み込みロジック
        pass

    async def clean_data(self, data: list[dict]) -> list[dict]:
        # データクリーニングロジック
        pass

    async def transform_data(self, data: list[dict]) -> list[dict]:
        # データ変換ロジック
        pass

    async def save_data(self, data: list[dict], destination: str) -> None:
        # データ保存ロジック
        pass
```

#### API Integration Agent

外部 API を統合するエージェント：

```python
import httpx

class APIAgent(AgentBlock):
    async def initialize(self) -> None:
        await super().initialize()
        self.client = httpx.AsyncClient()

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        response = await self.client.get(
            input_data["url"],
            params=input_data.get("params", {})
        )
        response.raise_for_status()
        return {"data": response.json()}

    async def cleanup(self) -> None:
        await self.client.aclose()
        await super().cleanup()
```

#### Database Agent

データベースを操作するエージェント：

```python
import asyncpg

class DatabaseAgent(AgentBlock):
    async def initialize(self) -> None:
        await super().initialize()
        self.pool = await asyncpg.create_pool(
            host="localhost",
            database="mydb",
            user="user",
            password="password"
        )

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        query = input_data["query"]
        params = input_data.get("params", [])

        async with self.pool.acquire() as conn:
            rows = await conn.fetch(query, *params)
            return {"results": [dict(row) for row in rows]}

    async def cleanup(self) -> None:
        await self.pool.close()
        await super().cleanup()
```

### 高度な例

#### State Machine Agent

ステートマシンを実装するエージェント：

```python
from enum import Enum

class State(Enum):
    IDLE = "idle"
    PROCESSING = "processing"
    COMPLETED = "completed"
    ERROR = "error"

class StateMachineAgent(AgentBlock):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.state = State.IDLE

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        self.state = State.PROCESSING

        try:
            result = await self.process(input_data)
            self.state = State.COMPLETED
            return result
        except Exception as e:
            self.state = State.ERROR
            raise

    async def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # 処理ロジック
        pass
```

#### Retry Agent

リトライ機能を持つエージェント：

```python
import asyncio
from typing import Any

class RetryAgent(AgentBlock):
    MAX_RETRIES = 3
    RETRY_DELAY = 1.0

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        for attempt in range(self.MAX_RETRIES):
            try:
                return await self.process(input_data)
            except Exception as e:
                if attempt == self.MAX_RETRIES - 1:
                    raise
                await asyncio.sleep(self.RETRY_DELAY * (attempt + 1))

    async def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # 処理ロジック（失敗する可能性あり）
        pass
```

#### Caching Agent

キャッシュ機能を持つエージェント：

```python
from functools import lru_cache
import hashlib
import json

class CachingAgent(AgentBlock):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.cache: dict[str, Any] = {}

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        cache_key = self.get_cache_key(input_data)

        if cache_key in self.cache:
            return self.cache[cache_key]

        result = await self.process(input_data)
        self.cache[cache_key] = result
        return result

    def get_cache_key(self, input_data: dict[str, Any]) -> str:
        data_str = json.dumps(input_data, sort_keys=True)
        return hashlib.md5(data_str.encode()).hexdigest()

    async def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        # 処理ロジック
        pass
```

---

## 🧪 テスト例

### ユニットテスト

```python
import pytest
from my_agent import MyAgent

@pytest.mark.asyncio
async def test_agent_basic():
    agent = MyAgent(metadata_path="agent.yaml")
    await agent.initialize()

    result = await agent.run({"input": "test"})
    assert result["output"] == "expected"

    await agent.cleanup()

@pytest.mark.asyncio
async def test_agent_with_context_manager():
    async with MyAgent(metadata_path="agent.yaml") as agent:
        result = await agent.run({"input": "test"})
        assert result["output"] == "expected"
```

---

## 📚 ベストプラクティス

1. **常に `initialize()` と `cleanup()` を実装**
   - リソースの適切な管理
   - コンテキストマネージャーの使用を推奨

2. **入力検証を行う**
   - 必須フィールドのチェック
   - 型の検証
   - エラーメッセージの明確化

3. **ログを活用**
   - AG-UI でリアルタイムログ
   - デバッグ情報の記録

4. **エラーハンドリング**
   - 適切な例外処理
   - ユーザーフレンドリーなエラーメッセージ

5. **テストを書く**
   - ユニットテスト
   - 統合テスト
   - エッジケースのテスト

---

## 🔗 関連リンク

- [クイックスタートガイド](../docs/quickstart.md)
- [API リファレンス](../docs/api.md)
- [プロトコルガイド](../docs/protocols.md)
- [CLI リファレンス](../docs/cli.md)
