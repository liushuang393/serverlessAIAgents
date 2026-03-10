# Agent 定義パターン ガイド

AgentFlow では、エージェントを定義する方法が 3 種類あります。
このガイドでは、それぞれのパターンの特徴と使い分けを説明します。

---

## どれを使うべきか（デシジョンツリー）

```
新しいエージェントを作りたい
│
├─ プロトタイプ・スクリプト・実験的なコード？
│   └─ → @agent デコレータ を使う
│
├─ 本番コード・型安全・リトライが必要？
│   └─ → ResilientAgent（= Agent）を使う ← 標準推奨
│
└─ 既存の AgentBlock サブクラスを保守している？
    └─ → 将来的に ResilientAgent に移行する（AgentBlock は非推奨）
```

---

## 1. `Agent` / `ResilientAgent` — 標準・本番向け（推奨）

`Agent` は `ResilientAgent` の公式エイリアスです。新規コードはこちらを使ってください。

### 特徴

- Pydantic による入出力の型安全
- タイムアウト制御（`asyncio.timeout`）
- 自動リトライ機構（最大 `max_retries` 回）
- LLM クライアントの自動注入（松耦合）
- `SKILL.md` 形式のプロンプト読み込み

### コード例

```python
from pydantic import BaseModel
from agentflow import Agent  # ResilientAgent の標準エイリアス


class SummaryInput(BaseModel):
    text: str
    language: str = "ja"


class SummaryOutput(BaseModel):
    summary: str
    word_count: int


class SummaryAgent(Agent[SummaryInput, SummaryOutput]):
    """テキスト要約エージェント."""

    name = "SummaryAgent"
    timeout_seconds = 60
    max_retries = 3

    def _parse_input(self, input_data: dict) -> SummaryInput:
        return SummaryInput.model_validate(input_data)

    async def process(self, input_data: SummaryInput) -> SummaryOutput:
        prompt = f"以下のテキストを要約してください:\n{input_data.text}"
        response = await self._call_llm(prompt)
        return SummaryOutput(
            summary=response,
            word_count=len(response),
        )


# 使用方法
agent = SummaryAgent()
result = await agent.run({"text": "長いテキスト...", "language": "ja"})
# result は dict[str, Any] 形式で返る
```

---

## 2. `@agent` デコレータ — 最短・最も簡単（プロトタイプ・スクリプト向け）

クラスを継承せずに、シンプルなクラスをエージェントとして定義できます。

### 特徴

- `AgentBlock` を継承不要
- 最小限のコードでエージェントを定義
- グローバルレジストリへの自動登録
- `AgentClient.get("名前").invoke(...)` で呼び出し

### 向いている用途

- スクリプト、Jupyter ノートブック
- プロトタイプ、概念実証（PoC）
- 型安全やリトライが不要な単純なユースケース

### コード例

```python
from agentflow import agent, AgentClient


@agent
class GreetingAgent:
    """挨拶エージェント."""

    system_prompt = "あなたは親切な挨拶係です。"

    async def process(self, input_data: dict) -> dict:
        name = input_data.get("name", "ゲスト")
        return {"greeting": f"こんにちは、{name}さん！"}


# 使用方法
result = await AgentClient.get("GreetingAgent").invoke({"name": "太郎"})
# result == {"greeting": "こんにちは、太郎さん！"}
```

```python
# デコレータにパラメータを渡す場合
@agent(name="カスタム名", temperature=0.3, skills=["chatbot"])
class AdvancedAgent:
    pass
```

---

## 3. `AgentBlock` — レガシー（非推奨）

### 非推奨の理由

`AgentBlock` は AgentFlow の初期設計で使われていた基底クラスです。
現在は `ResilientAgent` がより優れた代替手段を提供しているため、
`AgentBlock` を直接継承することは**非推奨**です。

`AgentBlock` を直接継承すると、Python の `DeprecationWarning` が発行されます:

```
SomeAgent: AgentBlock の直接継承は非推奨です。ResilientAgent を使用してください。
```

### 移行方法

`AgentBlock` の直接サブクラスは、以下のように `ResilientAgent` へ移行してください:

```python
# 旧（非推奨）
from agentflow.core.agent_block import AgentBlock

class OldAgent(AgentBlock):
    async def run(self, input_data: dict) -> dict:
        return {"result": "ok"}


# 新（推奨）
from pydantic import BaseModel
from agentflow import Agent

class MyInput(BaseModel):
    query: str

class MyOutput(BaseModel):
    result: str

class NewAgent(Agent[MyInput, MyOutput]):
    name = "NewAgent"

    def _parse_input(self, input_data: dict) -> MyInput:
        return MyInput.model_validate(input_data)

    async def process(self, input_data: MyInput) -> MyOutput:
        return MyOutput(result="ok")
```

---

## まとめ比較表

| 項目 | `Agent`（= `ResilientAgent`） | `@agent` デコレータ | `AgentBlock`（非推奨） |
|------|-------------------------------|---------------------|----------------------|
| 推奨度 | **標準推奨** | プロトタイプ向け | 非推奨（レガシー） |
| 型安全 | Pydantic による完全な型安全 | なし（任意） | なし |
| リトライ | 自動（設定可能） | なし | なし |
| タイムアウト | 自動（設定可能） | なし | なし |
| LLM 自動注入 | あり | あり（`AgentClient` 経由） | なし |
| コード量 | やや多い | 最小 | 中程度 |
| 用途 | 本番コード | PoC・スクリプト | 既存コードの保守のみ |

---

## インポートパス

```python
# 推奨インポート（公開 API）
from agentflow import Agent            # = ResilientAgent（標準エイリアス）
from agentflow import ResilientAgent   # 同上（明示的エイリアス）
from agentflow import agent            # @agent デコレータ
from agentflow import AgentClient      # @agent エージェントの呼び出し

# 非推奨（移行中の既存コードのみ）
from agentflow import AgentBlock       # 非推奨 - DeprecationWarning が発行される
```
