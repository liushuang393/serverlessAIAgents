# Check → Report → Gate 統合システム 設計ドキュメント

**日付**: 2026-02-20
**対象**: AgentFlow コード品質CI/CDパイプライン

---

## 背景と課題

AIコーディングアシスタント（Claude Code等）が生成するコードに、型エラー・リント違反・セキュリティ問題が頻発している。`check.sh all` が通らないケースが多く、lint/type-check/test/アーキテクチャルール違反が複合的に発生する。

### 特定した課題

1. **セキュリティスキャンの不足**: 既存CIはTrivy/pip-audit（依存関係スキャン）のみ。コードレベルのSAST（bandit等）が未導入
2. **AIへのフィードバックが非構造的**: check.sh失敗時、AIはターミナル出力を見るが、どのファイルの何行目を直すべきか整理されていない
3. **CIゲートが不完全**: `lint.yml`はlint+mypyのみ。securityとtestがCIでブロックしない

---

## 解決策

### 1. bandit SAST（`check.sh security`）

Pythonコードに対してbanditを実行し、HIGH severity / HIGH confidenceの問題のみ報告する。

- フラグ: `-lll`（HIGH severity以上）`-iii`（HIGH confidence以上）
- 対象: `agentflow/` ディレクトリ
- 除外: `tests/`, `.venv/`, `venv/`, `studio/`, `node_modules/`
- B101（assert文）はスキップ（テストでの多用を考慮）

### 2. AI向け構造化レポート（`check.sh report`）

全チェック（lint/type-check/security/test）を実行し、`check-report.md`を生成する。
AIアシスタントに貼り付けるだけで修正依頼が可能な形式。

### 3. CI品質ゲート（`quality-gate.yml`）

PRマージを以下の失敗でブロックする:
- Ruff lint エラー
- MyPy type エラー
- bandit HIGH severity findings
- pytest テスト失敗

失敗時は`check-report.md`アーティファクトとして保存する。

---

## 変更ファイル一覧

| ファイル | 変更種別 | 内容 |
|---------|---------|------|
| `check.sh` | 修正 | `security`・`report` コマンド追加、`all` に security を組み込み |
| `.pre-commit-config.yaml` | 修正 | bandit hook を追加（1.7.10） |
| `pyproject.toml` | 修正 | `[tool.bandit]` 設定セクション追加、`bandit>=1.7.0` を dev 依存に追加 |
| `.github/workflows/quality-gate.yml` | 新規 | 統合品質ゲートワークフロー |

---

## 使い方

```bash
# banditインストール（初回のみ）
pip install bandit

# セキュリティチェック単体
./check.sh security

# AI向けレポート生成
./check.sh report
# → check-report.md が生成される

# 全チェック（security を含む）
./check.sh all
```

---

## 既存機能への影響

- `lint.yml`・`test.yml` は既存のまま維持（互換性を保つ）
- `quality-gate.yml` は新規追加（既存ワークフローを置き換えない）
- `check.sh` の既存コマンド（format/lint/type-check/test/all/pre-commit/clean）の動作は変わらない
- `all` コマンドのみ security ステップが追加される

---

## セキュリティポリシー

banditのスキップルール:
- `B101` (assert_used): テストコードで多用されるため全体スキップ
- HIGH severity未満: CIではブロックしない（開発者判断に委ねる）

将来的な拡張として、MEDIUM severityの警告レポートのみ生成する `--format json` モードの追加も検討できる。
