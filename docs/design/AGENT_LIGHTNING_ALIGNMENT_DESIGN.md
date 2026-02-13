# Agent Lightning Alignment Design

更新日: 2026-02-12  
対象: `agentflow/` と `apps/code_migration_assistant/`

---

## 1. 目的

Microsoft Agent Lightning の思想を取り込み、AgentFlow の実行系を「学習可能な実行ログ」に接続する。

- 既存 Agent 実装を壊さない
- 実行ロジックと改善ロジックを疎結合に保つ
- 段階導入可能な最小実装から開始する

---

## 2. 公式情報の確認結果（要点）

公式ソース（GitHub / 公式 docs / Microsoft Research / arXiv）から確認した事実:

1. Agent Lightning は **Training-Agent Disaggregation** を中核に据え、既存 Agent を大改修せず学習ループへ接続する設計。  
2. 観測層は **Tracer + Store** を提供し、実行時のイベントと報酬を標準化して蓄積。  
3. 実行側と学習側は **client/server 分離** でスケール可能（shared-memory / client-server 両戦略）。  
4. 学習データは trajectory / transition へ変換され、長期・分岐タスクでも credit assignment しやすい構成。  
5. 公式 docs（2026-02-12 時点）では APO と VERL ベースの学習構成が提示され、VERL 設定例に GRPO パラメータがある。

---

## 3. 現状差分（AgentFlow vs Agent Lightning）

| 観点 | AgentFlow 現状 | Gap |
|---|---|---|
| 実行記録 | `RunRecord` は run 単位メタ情報中心 | ステップ/イベント粒度の標準化記録が弱い |
| run_stream 監査 | これまで `RunStore` 保存なし | stream 実行の学習向け追跡が不足 |
| 報酬 | 明示的 reward 契約なし | 最適化ループへ接続しにくい |
| 学習変換 | 実行ログ→(s,a,r,s') 変換層なし | RL/APO/SFT 前処理が都度実装になる |
| 実行/改善分離 | Engine は拡張可能だが改善フックが未統一 | Training-Agent 解耦が弱い |

---

## 4. 改善方針

### 4.1 設計原則

- 非侵襲: 既存 Engine/Agent の I/F を維持
- 追加フック: `EngineConfig` で opt-in
- 標準化: Event / Reward / Transition の型を明確化

### 4.2 導入コンポーネント

1. `LightningStore`（Protocol）
2. `MemoryLightningStore`（最小実装）
3. `LightningTracer`（実行イベント正規化）
4. `TrajectoryAdapter`（イベント→学習トランジション）
5. `LightningRuntimeConfig`（収集/学習/最適化の opt-in 設定）
6. `MicrosoftLightningStore`（Agent Lightning 連携アダプタ）
7. `train_with_lightning_backend()`（microsoft/builtin 切替）

---

## 5. 実装内容（この変更）

### 5.1 フレームワーク

- 追加: `agentflow/run/lightning.py`
  - `LightningEventRecord`, `RewardSignal`
  - `LightningStore`, `MemoryLightningStore`
  - `LightningTracer`
  - `TrajectoryAdapter`
- 追加: `agentflow/run/lightning_backend.py`
  - `LightningRuntimeConfig`, `LightningTrainingRequest`, `LightningTrainingResult`
  - `resolve_lightning_store()`（`auto|builtin|microsoft`）
  - `MicrosoftLightningStore`（`agentlightning` の `trace_context` / `emit_*` と接続）
  - `train_with_lightning_backend()`（Microsoft Trainer 優先、失敗時 fallback）
- 更新: `agentflow/run/__init__.py`
  - 新コンポーネントを公開 API に追加
- 更新: `agentflow/engines/base.py`
  - `run()` / `run_stream()` の両方で `RunRecord` を保存
  - イベント記録フック（`_trace_event`, `_trace_custom_event`）
  - 報酬評価フック（`reward_evaluator` → `RewardSignal`）
  - `run_stream()` でも run lifecycle を統一管理
  - `train_lightning()` を追加（run履歴→学習サンプル→backend学習）
- 更新: `agentflow/__init__.py`
  - 新しい run API を再エクスポート

### 5.2 app（必要反映）

- 追加: `apps/code_migration_assistant/lightning.py`
  - `score_migration_result()`
  - `create_lightning_engine_config()`
- 更新: `apps/code_migration_assistant/engine.py`
  - デフォルトは lightning 無効（`enable_collection=False`, `enable_training=False`）
  - `get_transition_samples()`, `get_latest_transition_samples()` 追加
  - `train_latest_run()`, `get_optimized_llm_profile()` 追加
- 更新: `apps/code_migration_assistant/orchestrator.py`
  - `get_latest_training_samples()` 追加
  - `train_latest_run()`, `get_optimized_llm_profile()` 追加

---

## 6. テスト

追加テスト:

- `tests/unit/run/test_lightning.py`
  - Store/Tracer の保存
  - terminal reward の割当
  - BaseEngine 統合（run / run_stream）
- `tests/unit/run/test_lightning_backend.py`
  - backend 解決（disabled / strict microsoft）
  - 学習フォールバック（builtin）
- `apps/code_migration_assistant/tests/test_lightning.py`
  - 報酬関数
  - Engine デフォルト設定（無効）
  - opt-in 設定（有効）

---

## 7. 既知の制約

1. Microsoft backend はライブラリ導入が前提（未導入時は builtin fallback）  
2. transition 生成は汎用最小ロジック（高度 credit assignment は未実装）  
3. 学習サーバー（optimizer server）の分散構成は次段

---

## 8. 運用手順（実行/訓練分離）

1. 通常運用:
   - `enabled=False`
   - `enable_training=False`
2. 収集フェーズ（限定）:
   - 対象案件のみ `enabled=True`
   - `enable_training=False`
   - `reward_evaluator` を必須設定
3. 訓練フェーズ（別ジョブ）:
   - `train_lightning()` を実行
   - 先に `apply_optimized_profile=False` で評価
4. 適用フェーズ:
   - 指標確認後に段階適用（canary推奨）

注意:
- 実行経路から自動で訓練を起動しない。
- 収集・訓練の有効化は常時運用にしない。

---

## 9. 次段実装案

1. `LightningStore` の Redis/Postgres 実装  
2. 学習サーバー連携（client-server 方式）  
3. trajectory 分解の高度化（branch/parallel を明示）  
4. reward 合成（品質・コスト・レイテンシの多目的化）

---

## 10. 参照（公式）

- GitHub: https://github.com/microsoft/agent-lightning  
- Docs: https://microsoft.github.io/agent-lightning/latest/  
- Microsoft Research: https://www.microsoft.com/en-us/research/publication/agent-lightning-train-any-ai-agents-with-reinforcement-learning/  
- arXiv: https://arxiv.org/abs/2508.03680  
