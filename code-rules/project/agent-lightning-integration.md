# Agent Lightning 統合規約

> **バージョン**: 1.0.0  
> **適用範囲**: `agentflow/run/`、`agentflow/engines/`、`apps/*` の学習連携実装  
> **最終更新**: 2026-02-12

## 1. 規約の参照階層（必須）

この文書は以下の階層で適用する。

1. 親規約（最上位）: `code-rules/CLAUDE.md`
2. 設計規約（上位）: `code-rules/project/architecture.md`
3. 本規約（本書）: `code-rules/project/agent-lightning-integration.md`
4. 実装詳細（参考）: `docs/design/AGENT_LIGHTNING_ALIGNMENT_DESIGN.md`

実装判断が衝突した場合は、番号の小さい上位規約を優先する。

## 2. 基本原則

- 外部契約優先: 既存の Engine/Agent 公開I/Fを壊さない。
- 高内聚・低耦合: ベンダー依存は `agentflow/run` の後端適配層に閉じ込める。
- 設定駆動: 収集・学習・最適化は opt-in で有効化する。
- 既定安全: 既定値は `enabled=False`, `enable_training=False` とする。

## 3. 実装ルール

- ベンダー実装の直接呼び出しは `agentflow/run/lightning_backend.py` 経由のみ。
- `apps/` 層は `EngineConfig` と公開 API (`train_lightning` など) のみ利用する。
- `backend=\"microsoft\"` 指定時:
  - ライブラリ導入済みなら Microsoft backend を使用する。
  - 未導入時は `strict_backend=False` なら fallback、`True` なら失敗させる。
- fallback 実装時も入出力スキーマを維持し、`message` と `backend` を必ず返す。

## 4. 入出力契約

- 学習要求/結果は型定義を必須化する（Pydantic推奨）。
- 最低限以下を含める:
  - Request: `run_id`, `max_samples`, `backend`, `algorithm`, `apply_optimized_profile`
  - Result: `success`, `backend`, `trained`, `optimized`, `num_samples`, `metrics`, `message`
- `optimized_llm_profile` は辞書で保持し、`llm_config` へ反映時は後方互換を壊さない。

## 5. テスト規約

- 以下のケースを unit test で必須化する:
  - 既定無効（収集/学習しない）
  - opt-in 有効時の収集
  - strict microsoft 未導入時の失敗
  - fallback builtin の継続動作
  - 最適化プロファイル反映の妥当性

## 6. ドキュメント更新規約

- 学習連携の実装変更時は以下を同時更新する:
  - `docs/engines.md`
  - `docs/design/AGENT_LIGHTNING_ALIGNMENT_DESIGN.md`
  - 影響アプリ `README.md`
  - 本規約（必要時）

## 7. AI/ユーザー注意事項

- 実行と訓練を混在させない:
  - 実行は `run()` / `run_stream()` の責務
  - 訓練は `train_lightning()` の責務
- 既定で訓練は実行しない。実行フロー内から自動訓練を呼ばない。
- 収集は必要案件のみ有効化する。常時有効化は原則禁止。
- 本番適用前に `optimized_llm_profile` を人間レビューする。

## 8. 運用手順（推奨）

### 8.1 通常運用（既定）

- `enabled=False`
- `enable_training=False`
- 目的: 通常の推論/実行のみ

### 8.2 限定収集（必要案件のみ）

- `enabled=True`
- `enable_training=False`
- `reward_evaluator` を明示
- 目的: 後段の訓練用データ収集

### 8.3 オフライン訓練

- 本番実行経路とは別ジョブで `train_lightning()` を実行
- `backend="microsoft"` を優先し、不可時は fallback 方針を明示
- 結果を確認:
  - `success`, `backend`, `num_samples`, `metrics`, `message`

### 8.4 プロファイル適用

- `apply_optimized_profile=False` でまず評価
- 問題なければ段階適用（canary → 本番）
