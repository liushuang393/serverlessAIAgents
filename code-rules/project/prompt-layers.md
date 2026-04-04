# プロンプトレイヤー規約

> **バージョン**: 1.0.0
> **適用範囲**: LLM を使用する全 Agent のプロンプト構築規約
> **最終更新**: 2026-04-04
> **実装**: `kernel/prompts/`

---

## 概要

Agent のプロンプトは 6 層に分離し、タスクに応じて必要十分なレイヤーのみを合成する。
「全部入れる」ではなく「必要なものだけ入れる」が原則。

---

## 6 層モデル

| 層 | 名称 | 内容 | 注入条件 |
|---|---|---|---|
| L1 | **CoreSystem** | Agent の役割・成功基準・禁止事項 | 常時（固定） |
| L2 | **TaskSystem** | 今回の目標・成果物・制約・完了条件 | タスク開始時（動的） |
| L3 | **RuntimeContext** | ユーザーリクエスト要約・参照情報 | 毎回（動的） |
| L4 | **ConversationState** | 確定事項・未解決・次アクション | マルチターン時のみ |
| L5 | **MemoryProfile** | 長期記憶・嗜好・KeyNotes | 必要時のみ |
| L6 | **ToolEnvironment** | 利用可能ツール・環境制約 | ツール使用時のみ |

### 原則

1. **system に何でも書かない** — system は憲法、task は作戦、runtime は現場情報
2. **履歴は状態として持つ** — 全履歴ではなく「確定事項・未解決・次アクション」
3. **memory は倉庫であって常時表示板ではない** — 必要時だけ取り出す
4. **資料は原文投入ではなく意味抽出して入れる** — token 爆発防止
5. **最後に「削れるか」を見る** — 良い prompt は削っても壊れない prompt

---

## 4 パターン

### パターン判定フロー

```
この Agent はどう動く？
│
├── ツールを使うか？
│   └── YES → TOOL_AUGMENTED（L1+L2+L3+L6）
│
├── パイプライン（複数 Agent 連携）か？
│   └── YES → MULTI_STEP（L1+L2+L3 +L4?+L5?）
│
├── 会話ターンが 2 以上か？
│   └── YES → MULTI_TURN（L1+L3+L4 +L2?+L5?）
│
└── それ以外
    └── SINGLE_TASK（L1+L2+L3）
```

### パターン詳細

| パターン | 必須レイヤー | オプション | 用途 |
|---|---|---|---|
| **SINGLE_TASK** | L1, L2, L3 | — | 単発 Q&A、1 回で完結する処理 |
| **MULTI_STEP** | L1, L2, L3 | L4, L5 | パイプライン処理（Agent 連携） |
| **MULTI_TURN** | L1, L3, L4 | L2, L5 | 会話継続（状態圧縮あり） |
| **TOOL_AUGMENTED** | L1, L2, L3, L6 | L5 | ツール利用タスク |

---

## 各 App の適用パターン

| App | 呼び出しパターン | プロンプトパターン | 理由 |
|---|---|---|---|
| **code_migration** | B-2（パイプライン） | **MULTI_STEP** | 分析→設計→変換→テスト→検証の連鎖 |
| **decision_governance** | B-1/B-2（パイプライン） | **MULTI_STEP** | 道→法→術→器→レビューの連鎖 |
| **legacy_modernization** | B-2（パイプライン） | **MULTI_STEP** | Brand→Signal→Score→Content の連鎖 |
| **market_trend** | B-1（パイプライン） | **MULTI_STEP** | Collector→Analyzer→Scorer の連鎖 |
| **messaging_hub** | B-Coordinator | **MULTI_TURN** | Intent分類→specialist→summary の会話 |
| **design_skills_engine** | B-1（パイプライン） | **MULTI_STEP** | Intent→Prompt→Workflow の連鎖 |
| **orchestration_guardian** | A（単発） | **SINGLE_TASK** | 決定論的検証（LLM不使用） |
| **faq_system** | A + C（SSE） | **SINGLE_TASK** | 質問→回答の 1 ショット |

---

## 適用ガイドライン

### MULTI_STEP パイプライン Agent の場合

各 Agent は前段の出力を受け取り、自身のタスクを実行する。

```python
from kernel.prompts import (
    PromptAssembler,
    PromptLayerSet,
    PromptPattern,
    CoreSystemLayer,
    TaskSystemLayer,
    RuntimeContextLayer,
)

class MyPipelineAgent(ResilientAgent[MyInput, MyOutput]):
    name = "MyPipelineAgent"

    # L1: 固定（クラス属性として定義）
    _CORE = CoreSystemLayer(
        role="あなたはコード変換エージェントです。",
        success_criteria=["正確な変換結果を返す"],
        prohibitions=["推測で補完しない", "元コードの意味を変えない"],
        output_principles=["JSON形式で返す"],
    )

    async def process(self, input_data: MyInput) -> MyOutput:
        layers = PromptLayerSet(
            core_system=self._CORE,
            # L2: タスクごとに動的
            task_system=TaskSystemLayer(
                goal=f"{input_data.source_lang} → {input_data.target_lang} 変換",
                deliverables=["変換後コード", "変換理由"],
                constraints=["破壊的変更禁止"],
            ),
            # L3: 今回の入力
            runtime_context=RuntimeContextLayer(
                user_request=input_data.description,
                extracted_points=[f"対象ファイル: {input_data.file_path}"],
                reference_summaries=input_data.context_summaries,
            ),
        )
        result = self._build_prompt(layers, PromptPattern.MULTI_STEP)
        response = await self._call_llm(result.system_prompt)
        return self._parse_output(response)
```

### MULTI_TURN 会話 Agent の場合

```python
from kernel.prompts import (
    PromptLayerSet,
    PromptPattern,
    CoreSystemLayer,
    RuntimeContextLayer,
    ConversationStateLayer,
)

class ConversationAgent(ResilientAgent[ChatInput, ChatOutput]):
    name = "ConversationAgent"

    _CORE = CoreSystemLayer(
        role="あなたは対話アシスタントです。",
        success_criteria=["ユーザーの意図を正確に把握する"],
        prohibitions=["話題を勝手に変えない"],
    )

    async def process(self, input_data: ChatInput) -> ChatOutput:
        layers = PromptLayerSet(
            core_system=self._CORE,
            runtime_context=RuntimeContextLayer(
                user_request=input_data.message,
            ),
            # L4: 履歴から抽出した状態（全履歴ではない）
            conversation_state=ConversationStateLayer(
                decisions=input_data.confirmed_decisions,
                open_items=input_data.pending_items,
                next_actions=input_data.suggested_actions,
                turn_count=input_data.turn_count,
            ),
        )
        result = self._build_prompt(layers, PromptPattern.MULTI_TURN)
        response = await self._call_llm(result.system_prompt)
        return ChatOutput(reply=response)
```

### やってはいけないこと

| ❌ NG パターン | ✅ 正解 |
|---|---|
| 全レイヤーを常に注入 | パターンに応じた必須レイヤーのみ |
| 会話履歴を全文注入 | ConversationState に要約して注入 |
| 長文仕様書を L3 に丸ごと | 結論・制約・使用箇所に要約 |
| L1 にタスク固有情報を混入 | L1 は役割のみ、タスクは L2 |
| memory を常時 system に全注入 | ContextBuilder で relevance gating |

---

## 品質チェック

プロンプト合成後に `PromptQualityChecker` で検証可能:

```python
from kernel.prompts import PromptQualityChecker, get_pattern_config

checker = PromptQualityChecker()
config = get_pattern_config(PromptPattern.MULTI_STEP)
report = checker.full_check(result, config)

# report.passed → True/False
# report.issues → ["必須レイヤー欠落: task_system", ...]
# report.suggestions → ["欠落レイヤーのデータを提供してください", ...]
```
