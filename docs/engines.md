# Engine パターン詳細ガイド

AgentFlow の Engine は AI エージェント実行の統一入口です。4種類の予定義パターンを提供します。

---

## 🏗️ アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│                        BaseEngine                            │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ run()              → 非ストリーム実行（結果 dict）      ││
│  │ run_stream()       → イベントストリーム（dict を yield）││
│  │ HITL（任意）        → 中断・再開（checkpointer）        ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
           ↓ 継承
┌──────────────┬──────────────┬──────────────┬───────────────┐
│ SimpleEngine │ GateEngine   │PipelineEngine│  RAGEngine    │
│ 単一 Agent   │ Gate+Main    │ 複数ステージ │ ベクトル検索  │
└──────────────┴──────────────┴──────────────┴───────────────┘
```

---

## 1. SimpleEngine（単一 Agent）

最もシンプルなパターン。単一の Agent を実行します。

### 使用例

```python
from agentflow import SimpleEngine, AgentBlock

class QAAgent(AgentBlock):
    """質問応答 Agent"""
    
    async def run(self, input_data: dict) -> dict:
        question = input_data.get("question", "")
        # LLM 呼び出しなど
        return {"answer": f"回答: {question}"}

# Engine 作成
engine = SimpleEngine(agent=QAAgent)

# 実行
result = await engine.run({"question": "AIとは何ですか？"})
print(result)  # {"answer": "回答: AIとは何ですか？"}
```

### 設定オプション

```python
from agentflow import (
    EngineConfig,
    LightningRuntimeConfig,
    SimpleEngine,
    resolve_lightning_store,
)

runtime = LightningRuntimeConfig(
    enabled=False,                 # 既定: 収集しない
    backend="auto",                # auto|builtin|microsoft
    enable_training=False,         # 既定: 学習しない
    enable_api_optimization=False, # 既定: 最適化しない
)

engine = SimpleEngine(
    agent=QAAgent,
    config=EngineConfig(
        name="qa-engine",          # Engine名（flow_id プレフィックス）
        timeout_seconds=60,        # グローバルタイムアウト秒
        max_retries=3,             # グローバル最大リトライ回数
        enable_events=True,        # AG-UIイベントを発行
        enable_memory=True,        # メモリ機能を有効化（対応Engine/Flowで利用）
        llm_config={               # LLM設定（Provider側で参照）
            "model": "gpt-4.1-mini",
            "temperature": 0.2,
        },
        # 学習連携（既定は無効、必要時に opt-in）
        lightning=runtime,
        lightning_store=resolve_lightning_store(runtime),
        reward_evaluator=lambda result: 1.0 if result.get("success") else -1.0,
    )
)
```

### 学習連携フック（NEW）

`EngineConfig` の以下項目で、実行と改善ループを疎結合に接続できる:

- `lightning`: 収集/学習/最適化の有効化とバックエンド選択
- `lightning_store`: 標準化イベント/報酬の保存先
- `reward_evaluator`: 実行結果を報酬へ変換する関数

`BaseEngine.train_lightning()` を呼び出すと、保存済みトレースを学習投入できる。

Note:
- 既定は `lightning.enabled=False`（収集/学習しない）
- `backend="microsoft"` かつライブラリ未導入時は `builtin` へフォールバック
- `strict_backend=True` を指定するとフォールバックせずエラー化

### 運用手順（実行と訓練を分離）

1. 通常実行（推奨デフォルト）
   - `lightning.enabled=False`
   - `lightning.enable_training=False`
2. 限定収集（必要案件のみ）
   - `lightning.enabled=True`
   - `lightning.enable_training=False`
   - `reward_evaluator` を設定
3. オフライン訓練（別ジョブ）
   - `train_lightning()` を明示呼び出し
   - 必要に応じて `apply_optimized_profile=False` で先に評価

設計原則:
- 実行フローの責務は実行のみ
- 訓練フローの責務は訓練のみ
- 収集は常時有効化せず、必要時のみ

---

## 2. GateEngine（Gate + Main）

前置チェック付きの2段階パターン。Gate Agent で入力を検証し、通過した場合のみ Main Agent を実行。

### 使用例

```python
from agentflow import GateEngine, AgentBlock

class ComplianceChecker(AgentBlock):
    """コンプライアンスチェック Gate"""
    
    async def run(self, input_data: dict) -> dict:
        # 入力検証ロジック
        is_compliant = "禁止ワード" not in input_data.get("text", "")
        return {"compliant": is_compliant, "reason": "OK" if is_compliant else "禁止ワード検出"}

class ProcessingAgent(AgentBlock):
    """メイン処理 Agent"""
    
    async def run(self, input_data: dict) -> dict:
        return {"result": f"処理完了: {input_data['text']}"}

# Engine 作成
engine = GateEngine(
    gate_agent=ComplianceChecker,
    main_agent=ProcessingAgent,
    gate_check=lambda r: r.get("compliant", False),  # Gate 通過条件
)

# 実行
result = await engine.run({"text": "正常なテキスト"})
# Gate 通過 → Main 実行
print(result)  # {"status": "success", "gate_result": {...}, "result": {...}}

result = await engine.run({"text": "禁止ワード含む"})
# Gate 拒否 → Main 実行されず
print(result)  # {"status": "rejected", "reason": "禁止ワード検出", "gate_result": {...}}
```

---

## 3. PipelineEngine（複数ステージ）

複数ステージを順次・並列実行。Review ステージでの差し戻しにも対応。

### 基本構造

```python
from agentflow import PipelineEngine

engine = PipelineEngine(
    stages=[
        # Gate ステージ: 入力検証
        {"name": "gate", "agent": GateAgent, "gate": True},
        
        # 分析ステージ: 順次実行
        {"name": "analysis", "agents": [AnalysisAgent1, AnalysisAgent2]},
        
        # 並列ステージ: 同時実行
        {"name": "parallel", "agents": [AgentA, AgentB], "parallel": True},
        
        # Review ステージ: 品質チェック（PASS/REVISE/REJECT）
        {"name": "review", "agent": ReviewAgent, "review": True},
    ],
    max_revisions=2,  # 最大差し戻し回数
)
```

### ステージ設定

| パラメータ | 型 | 説明 |
|-----------|-----|------|
| `name` | str | ステージ名（必須） |
| `agent` | type | 単一 Agent クラス |
| `agents` | list[type] | 複数 Agent クラス |
| `gate` | bool | Gate ステージとして扱う |
| `parallel` | bool | 並列実行 |
| `review` | bool | Review ステージ（PASS/REVISE/REJECT） |

### Review Agent の実装

```python
class ReviewAgent(AgentBlock):
    """品質レビュー Agent"""
    
    async def run(self, input_data: dict) -> dict:
        analysis_result = input_data.get("analysis", {})
        
        # 品質スコア計算
        score = self._calculate_score(analysis_result)
        
        if score >= 80:
            return {"verdict": "PASS", "score": score}
        elif score >= 50:
            return {"verdict": "REVISE", "feedback": "要改善点あり", "score": score}
        else:
            return {"verdict": "REJECT", "reason": "品質不足", "score": score}
```

---

## 4. RAGEngine（ベクトル検索増強）

ベクトルデータベースから関連情報を検索し、コンテキストとして Agent に渡します。

### 使用例

```python
from agentflow import RAGEngine, AgentBlock

class KnowledgeAgent(AgentBlock):
    """ナレッジベース Agent"""
    
    async def run(self, input_data: dict) -> dict:
        query = input_data.get("query", "")
        # RAGEngine は以下を注入する:
        # - context: 文字列（検索結果を整形したコンテキスト）
        # - documents: list[dict]（検索結果の生データ）
        # - augmented_prompt: context_template で整形したプロンプト
        context = input_data.get("context", "")
        documents = input_data.get("documents", [])
        
        # コンテキストを使って回答生成
        return {"answer": f"回答（{len(documents)}件の参考資料使用）"}

engine = RAGEngine(
    agent=KnowledgeAgent,
    vector_store="company_docs",  # VectorDB コレクション名
    top_k=5,                       # 検索件数
    chunk_size=500,                # チャンクサイズ（RAGPipeline 側で利用）
)

result = await engine.run({"query": "社内規定について教えて"})
```

---

## 🔄 SSE ストリーミング

全ての Engine は `run_stream()` でリアルタイムイベントを配信：

```python
from agentflow.integrations.fastapi_integration import create_sse_response

@app.get("/api/stream")
async def stream_endpoint(question: str):
    async def generate():
        async for event in engine.run_stream({"question": question}):
            yield event
    
    return create_sse_response(generate())
```

### イベント形式（AG-UI 準拠）

```json
{"event_type":"flow.start","timestamp":1730000000.0,"flow_id":"qa-engine-acde1234","data":{"engine":"SimpleEngine"}}
{"event_type":"node.start","timestamp":1730000001.0,"flow_id":"qa-engine-acde1234","node_id":"QAAgent","node_name":"QAAgent","data":{}}
{"event_type":"node.complete","timestamp":1730000002.0,"flow_id":"qa-engine-acde1234","node_id":"QAAgent","node_name":"QAAgent","data":{"answer":"..."}}
{"type":"result","data":{"answer":"..."}}
{"event_type":"flow.complete","timestamp":1730000003.0,"flow_id":"qa-engine-acde1234","data":{}}
```

Note:
- `event_type` は AG-UI 標準イベント（`flow.start` / `node.start` / `progress` など）。
- 一部 Engine は互換性のため `type` ベースの補助イベント（例: `result`, `review_verdict`）も併用します。

---

## 🔧 カスタム Engine 作成

独自パターンが必要な場合は `BaseEngine` を継承：

```python
from agentflow.engines import BaseEngine, EngineConfig

class MyCustomEngine(BaseEngine):
    """カスタム Engine"""
    
    def __init__(self, agents: list, config: EngineConfig | None = None):
        super().__init__(config=config)
        self._agents = agents
    
    async def _initialize(self) -> None:
        """初期化処理（オプション）"""
        pass
    
    async def _execute(self, inputs: dict) -> dict:
        """コア実行ロジック（必須）"""
        results = []
        for agent_cls in self._agents:
            agent = agent_cls()
            result = await agent.run(inputs)
            results.append(result)
            inputs.update(result)  # 次の Agent に渡す
        return {"results": results}
```

---

## 📚 関連ドキュメント

- [quickstart.md](./quickstart.md) - 5分で動かす
- [guide-coding.md](./guide-coding.md) - Agent 開発
- [PATTERNS_GUIDE.md](./PATTERNS_GUIDE.md) - デザインパターン
