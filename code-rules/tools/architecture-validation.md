# アーキテクチャ検証ツール

## 概要

本プロジェクトでは以下の自動検証ツールで 7コア層 + Apps のアーキテクチャルールを強制する。

## ツール一覧

| ツール | スクリプト | 検証内容 | 統合先 |
|--------|-----------|---------|--------|
| レイヤー境界チェック | `scripts/check_layer_boundaries.py` | 7層の import 方向ルール | check.sh, pre-commit, CI |
| プロバイダ直接 import 禁止 | `scripts/check_no_direct_provider_calls.py` | LLM SDK 直接 import 禁止 | check.sh, pre-commit, CI |
| App コンプライアンス | `scripts/check_app_compliance.py` | 各 app のフレームワーク活用度 | check.sh, CI |
| ルールコンプライアンス | `scripts/check_rules_compliance.py` | CLAUDE.md ルール総合遵守 | check.sh, CI |

## 実行方法

### 一括実行

```bash
./check.sh all
```

### 個別実行

```bash
conda run -n agentflow python scripts/check_layer_boundaries.py
conda run -n agentflow python scripts/check_no_direct_provider_calls.py
conda run -n agentflow python scripts/check_app_compliance.py
conda run -n agentflow python scripts/check_app_compliance.py --json --app faq_system
conda run -n agentflow python scripts/check_rules_compliance.py
conda run -n agentflow python scripts/check_rules_compliance.py --json --strict
```

## CI/CD 統合

### pre-commit (ローカル)

- `check-layer-boundaries`: コミット前にレイヤー境界違反を検出
- `check-no-direct-provider`: コミット前にプロバイダ直接 import を検出

### GitHub Actions (CI)

- `quality-gate.yml`: Lint → **Architecture boundary** → **App compliance** → **Rules compliance** → Type check → Security → Tests
- 境界違反は pytest 経由の間接検出ではなく、スクリプト直接呼出で明示的にチェック

## 閾値設定

`scripts/check_rules_compliance.py` の `THRESHOLDS` で管理:

| カテゴリ | 閾値 | 説明 |
|---------|------|------|
| layer_boundary_violations | 25 | レイヤー境界違反数 (段階的に 0 へ) |
| provider_direct_imports | 0 | プロバイダ直接 import (即時ゼロ) |
| file_size_violations | 5 | 1000行超ファイル数 |
| type_ignore_without_reason | 20 | 理由なし type: ignore |
| cast_usage | 10 | cast() 使用数 |

## AI コーディング助手向け

**コード生成・編集後に以下を確認すること:**

1. `conda run -n agentflow python scripts/check_layer_boundaries.py` → 新規違反なし
2. 新規ファイルの import が上位層→下位層の方向のみ
3. LLM SDK は `infrastructure.llm` 経由のみ
4. 型アノテーション率 100% を目標
