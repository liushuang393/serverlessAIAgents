# AgentFlow クイックプロンプト

> **バージョン**: 0.2.0 (2024-12-30更新)

## 🆕 v0.2.0: 3つの開発方式

```
【方式1】@agent デコレータ（最も簡単・推奨）
  @agent class MyAgent: ...
  result = await AgentClient.get("MyAgent").invoke(input)

【方式2】create_flow（複数Agent協調）
  flow = create_flow([A, B, C], pattern="sequential")
  result = await flow.run(input)

【方式3】AgentCoordinator（高度な制御）
  coordinator = AgentCoordinator(agents=[...])
  result = await coordinator.execute(input)
```

## 🎯 核心原則

```
【層構造】
Decorator/Flow/Coordinator（編排）
  └── Agent（実行）
        ├── Skill（プロンプト）
        ├── RAG（知識）← オプション
        └── MCP（ツール）← オプション

【Provider統一】
  LLMProvider.default()     → OpenAI (デフォルト)
  ToolProvider.discover()   → @tool 自動発見
  DataProvider.sql/vector/cache → 統一データアクセス
  EventProvider.create()    → SSE/WS統一

【API選択】
  同期 → REST（デフォルト）
  進捗 → SSE（リアルタイム必要時）
```

## 📁 推奨プロジェクト構造

```
apps/[あなたのアプリ]/
├── main.py          # FastAPI (REST + SSE)
├── workflow.py      # AgentCoordinator
├── agents/          # AgentBlock継承
├── skills/          # SKILL.md形式
└── schemas/         # Pydantic入出力
```

---

## 📋 チェックリスト

| 必須項目 | 実装方法 |
|---------|---------|
| **Agent基底** | `AgentBlock`継承、`run()`実装 |
| **協調実行** | `AgentCoordinator` or `MultiAgentWorkflow.create()` |
| **状態共有** | `SharedContext(enable_memory=True)` |
| **プロンプト** | `skills/*/SKILL.md`形式で定義 |
| **プロトコル** | `@auto_adapt(protocols=["mcp","a2a","agui"])` |
| **画面生成** | `A2UIComponent`で宣言式UI |

---

## 🔧 コアパターン

### 0. @agent デコレータ（v0.2.0 推奨）
```python
from agentflow import agent, tool, AgentClient

@agent
class QAAgent:
    system_prompt = "親切なアシスタント"
    skills = ["chatbot", "rag"]  # Claude Code Skills 自動読み込み
    
    @tool
    def search(self, query: str) -> list:
        return []

# 呼び出し
result = await AgentClient.get("QAAgent").invoke({"question": "..."})
```

### 0.1 Skills 利用（Claude Code Skills 互換）
```python
from agentflow import agent, get_skill, list_skills

# 利用可能な Skills 一覧
print(list_skills())  # ['chatbot', 'rag', 'auth-provider', 'database-manager', ...]

# Agent に Skills を設定（2つの方法）
@agent(skills=["chatbot", "rag"])  # デコレータ引数
class Agent1: ...

@agent
class Agent2:
    skills = ["rag", "database-manager"]  # クラス属性
```

### 0.5 create_flow（v0.2.0 推奨）
```python
from agentflow import create_flow

flow = create_flow([Agent1(), Agent2()], pattern="sequential")
result = await flow.run({"task": "..."})
```

### 1. Sequential協調
```python
from agentflow.patterns.multi_agent import AgentCoordinator, SharedContext

coordinator = AgentCoordinator(
    agents=[agent1, agent2, agent3],
    pattern="sequential",
    shared_context=SharedContext(enable_memory=True)
)
result = await coordinator.execute(task)
```

### 2. Supervisorパターン
```python
from agentflow.patterns.supervisor import SupervisorCoordinator

coordinator = SupervisorCoordinator(
    supervisor=supervisor_agent,
    workers={"research": research_agent, "write": write_agent},
    max_iterations=10
)
result = await coordinator.execute("タスク")
```

### 3. Reflectionパターン
```python
from agentflow.patterns.reflection import ReflectionLoop

loop = ReflectionLoop(
    generator=gen_agent,
    reflector=reflector_agent,
    improver=improver_agent,
    max_iterations=3
)
result = await loop.execute("タスク")
```

### 4. Skills自動進化
```python
from agentflow.skills import SkillEngine

engine = SkillEngine(auto_learn=True)
result = await engine.resolve("PDFからテキスト抽出")
# result.instructions → プロンプトとしてLLMに渡す
```

### 5. A2UI画面生成
```python
from agentflow.protocols.a2ui import CardComponent, TextComponent

ui = CardComponent(
    title="結果",
    children=[TextComponent(content="データ")]
)
await emitter.emit_component(ui)
```

---

## 📁 SKILL.md形式

```markdown
---
name: my-skill
description: 説明（LLMがこれで判断する重要フィールド）
version: 1.0.0
triggers:
  - キーワード1
  - キーワード2
requirements:
  - package1
  - package2
---

# 指示内容
ここにLLMへの指示を記述
```

---

## 🌐 API実装（コピペ用）

### REST（同期）
```python
@app.post("/api/task")
async def process_task(req: TaskRequest):
    result = await coordinator.execute({"question": req.question})
    return {"status": "success", "result": result}
```

### SSE（ストリーム）
```python
@app.get("/api/task/stream")
async def stream_task(question: str):
    async def generator():
        # AGUIEmitterでイベント送信
        result = await coordinator.execute({"question": question})
        yield f"data: {json.dumps({'type': 'complete', 'result': result})}\n\n"
    return StreamingResponse(generator(), media_type="text/event-stream")
```

### フロントエンド（SSE受信）
```typescript
const es = new EventSource(`/api/task/stream?question=${q}`);
es.onmessage = (e) => {
  const data = JSON.parse(e.data);
  if (data.type === 'complete') { setResult(data.result); es.close(); }
};
```

---

## 🔗 詳細参照

- **完全版**: `AI_PROMPT_TEMPLATE.md`
- **アーキテクチャ**: `docs/architecture.md`
- **Skills**: `docs/guide-skills.md`
- **内蔵Skills**: `agentflow/skills/builtin/*/SKILL.md`
