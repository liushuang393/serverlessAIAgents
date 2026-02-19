# 依存関係管理規約

> **バージョン**: 1.0.0  
> **適用範囲**: 全パッケージ（Python / npm / GitHub Actions / Docker）  
> **最終更新**: 2026-02-19

## 1. 目的

- **Dependabot による「低レベルな更新 PR」の氾濫を防ぐ**：グループ化と方針で PR 数を抑え、一括レビュー・マージを可能にする。
- **依存のずれ（drift）を防ぐ**：バージョン方針を揃え、セキュリティ・互換性を維持する。
- **AI エージェント・フロント・バックの技術スタックを明文化**し、更新時のチェック項目を漏れなく行う。

## 2. 技術スタックと更新対象

| 領域 | 技術 | 主なディレクトリ | 更新ツール |
|------|------|------------------|------------|
| バックエンド | Python 3.13+, FastAPI, Pydantic, Ruff, mypy, pytest | `/`, `agentflow/`, `apps/*/` | pip, Dependabot (pip) |
| フロントエンド | React, Vite, TypeScript, ESLint, Prettier | `studio/`, `apps/*/frontend`, `agentflow/sdk/frontend` | npm, Dependabot (npm) |
| CI/CD | GitHub Actions | `.github/workflows/` | Dependabot (github-actions) |
| AI 関連 | LLM クライアント・プロバイダ、MCP/A2A 等 | `agentflow/` | pip（他と同一） |
| コンテナ | Docker | `agentflow/deploy/templates` | Dependabot (docker) |

※ Next.js を採用する App がある場合は上表に追記し、同様に npm グループで管理する。

## 3. バージョン方針

### 3.1 Python (pip)

- **推奨**: `pyproject.toml` / `requirements*.txt` では可能な範囲で **マイナーまで許容**（例: `^1.2.0` または `>=1.2.0,<2.0.0`）。
- **破壊的更新**: メジャーアップデートは別 Issue で計画し、テスト・型チェック・ドキュメントを更新してから取り込む。
- **Dependabot**: 週次で **minor + patch を 1 グループ** にまとめ、PR は 1 本に集約する（既存の `python-minor-patch` グループを維持）。

### 3.2 npm (React / Vite / TypeScript 等)

- **推奨**: `package.json` では **^ 指定** とし、Dependabot の **グループ** で minor + patch を 1 PR にまとめる。
- **peer 依存**: ESLint と @typescript-eslint など peer が合わない場合は、`npm install --legacy-peer-deps` を一時利用可。ルートまたは `studio/` に `.npmrc` で `legacy-peer-deps=true` を書く場合は理由を README に記載する。
- **Dependabot**: 各 npm ディレクトリごとに **1 グループ**（例: `npm-minor-patch`）とし、**open-pull-requests-limit: 1** を推奨。同一週の複数更新は 1 PR にまとめる。

### 3.3 GitHub Actions

- **推奨**: アクションのバージョンは **メジャー指定**（例: `actions/setup-python@v6`）。Dependabot は **github-actions 用の 1 グループ** とし、複数アクションの更新を **1 PR** にまとめる。
- **運用**: 週次で 1 本の PR をマージし、CI が通ることを確認する。

### 3.4 Docker

- ベースイメージは **タグを固定**（例: `python:3.13-slim`）。更新は Dependabot の Docker 更新 PR で受け、マージ前にビルド確認を行う。

## 4. Dependabot 運用（「小机器人」対策）

### 4.1 原則

- **PR は「まとめて 1 本」を基本** とする。グループ化により、バラバラの minor/patch 更新が複数 PR にならないようにする。
- **マージ前には必ず** 本規約で定める **提交前チェック**（`./check.sh all` および必要に応じてフロントビルド・npm audit）を実行する。
- Dependabot が出した PR を **無視しない**。週次で一括レビューし、テスト・ビルドが通ればマージする。

### 4.2 設定上のポイント（.github/dependabot.yml）

- **pip**: `groups.python-minor-patch` で `update-types: [minor, patch]` を指定（既存のまま）。
- **npm**: 各 `directory` ごとに `groups.npm-minor-patch`（または同一グループ名）で `update-types: [minor, patch]`、`open-pull-requests-limit: 1` を推奨。
- **github-actions**: `groups.github-actions` を追加し、全アクションを **1 グループ** にまとめ、`open-pull-requests-limit: 1` とする。

これにより「同じ週に複数の Dependabot PR が開きっぱなし」になる事態を減らす。

## 5. 提交前チェックとの対応

- 依存関係を更新した場合（Dependabot マージ含む）は、必ず **`./check.sh all`** を実行する。
- フロントエンド（React/TS）を触った場合は **studio のビルド**（`cd studio && npm run build`）および **ESLint / TypeScript** が通ることを確認する。
- バックエンド（Python）を触った場合は **ruff / mypy / pytest** が通ることを確認する。
- 詳細なチェック項目は **CLAUDE.md の「コミット前チェック」** に記載し、本ファイルからは「依存更新時も同じチェックを適用する」と参照する。

## 6. 関連ドキュメント

- [CI/CD ガイドライン](ci-cd-guidelines.md)
- [リポジトリ構造](repo-structure.md)
- ルート **CLAUDE.md**：コミット前チェック一覧・自動化コマンド

---

*最終更新: 2026-02-19 | バージョン: 1.0.0*
