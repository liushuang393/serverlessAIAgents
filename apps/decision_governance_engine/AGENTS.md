# apps/decision_governance_engine/

## Overview
- Enterprise decision support demo built on `PipelineEngine`.

## Structure
```
apps/decision_governance_engine/
├── engine.py
├── api.py
├── main.py
├── agents/
└── frontend/
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Engine entrypoint | `apps/decision_governance_engine/engine.py` | `DecisionEngine` (PipelineEngine subclass).
| API server | `apps/decision_governance_engine/api.py` | FastAPI app; routes under `routers/`.
| CLI entry | `apps/decision_governance_engine/main.py` | CLI runner.
| Agents | `apps/decision_governance_engine/agents/` | Dao/Fa/Shu/Qi + Gate/Review.
| Frontend | `apps/decision_governance_engine/frontend/` | React/TS UI.

## Run (Typical)
```bash
# backend (example ports vary; see README)
python -m uvicorn apps.decision_governance_engine.api:app --host 0.0.0.0 --port 8001 --reload

# frontend
cd apps/decision_governance_engine/frontend && npm install && npm run dev
```

## Pipeline Input Dict ルール（重要）

`build_input_dict()` が生成する入力辞書は **パイプライン上の全Agent に共有** される。

### フィールド追加・変更時の必須チェック

1. `schemas/agent_schemas.py` 内の **全 AgentInput 型** で同名フィールドの型を確認する:
   - `CognitiveGateInput`, `GatekeeperInput`, `ClarificationInput`
   - `DaoInput`, `FaInput`, `ShuInput`, `QiInput`, `ReviewInput`
2. **型衝突がないこと** を確認してから追加する
3. 既存フィールドと同名で異なる型を渡すと Pydantic バリデーションエラーになる

### 既知の型制約

| フィールド名 | 使用する AgentInput | 型 | 注意 |
|-------------|--------------------|----|------|
| `stakeholders` | `DaoInput` | `list[str]` | dict で渡すと型エラー。`["ラベル: 値", ...]` 形式で渡すこと |
| `constraints` | 複数 | `list[str]` | 文字列リスト形式 |
| `raw_question` | `CognitiveGateInput` | `str` | — |
| `question` | `DaoInput` 他 | `str` | — |

## Notes
- Ports vary across README/examples; treat `api.py` as authoritative.
