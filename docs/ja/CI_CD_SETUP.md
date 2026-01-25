# CI/CD 設定ガイド

AgentFlow フレームワークの CI/CD システム設定ガイドです。

## 目次

1. [概要](#概要)
2. [共通設定](#共通設定)
3. [個別 App 設定](#個別-app-設定)
4. [デプロイプラットフォーム](#デプロイプラットフォーム)
5. [トラブルシューティング](#トラブルシューティング)

---

## 概要

本 CI/CD システムは以下の特徴を持ちます：

- **再利用可能ワークフロー**: 共通処理を一箇所に集約
- **最小限の設定**: 個別 App は必要な設定のみ記述
- **9 プラットフォーム対応**: 様々なデプロイ先をサポート
- **セキュリティ重視**: 自動脆弱性スキャン、Dependabot 統合

### アーキテクチャ

```
.github/
├── workflows/
│   ├── reusable-python-ci.yml      # 共通: Python CI
│   ├── reusable-docker-build.yml   # 共通: Docker ビルド
│   ├── reusable-security-scan.yml  # 共通: セキュリティスキャン
│   ├── reusable-e2e-test.yml       # 共通: E2E テスト
│   ├── reusable-deploy-*.yml       # 共通: 各プラットフォームデプロイ
│   └── release.yml                 # 共通: リリース自動化
├── dependabot.yml                  # 依存関係自動更新
apps/
├── [app_name]/
│   └── .github/workflows/ci.yml    # 個別: App 専用ワークフロー
```

---

## 共通設定

### 1. Python CI (`reusable-python-ci.yml`)

```yaml
jobs:
  ci:
    uses: ./.github/workflows/reusable-python-ci.yml
    with:
      working-directory: apps/your-app
      python-version: "3.13"
      coverage-threshold: 80
```

**パラメータ:**
| パラメータ | 必須 | デフォルト | 説明 |
|-----------|------|-----------|------|
| `working-directory` | × | `.` | 作業ディレクトリ |
| `python-version` | × | `3.13` | Python バージョン |
| `coverage-threshold` | × | `80` | カバレッジ閾値 |

### 2. Docker ビルド (`reusable-docker-build.yml`)

```yaml
jobs:
  docker:
    uses: ./.github/workflows/reusable-docker-build.yml
    with:
      image-name: my-app
      dockerfile: Dockerfile
    secrets:
      REGISTRY_USERNAME: ${{ secrets.GHCR_USERNAME }}
      REGISTRY_PASSWORD: ${{ secrets.GHCR_TOKEN }}
```

### 3. セキュリティスキャン (`reusable-security-scan.yml`)

```yaml
jobs:
  security:
    uses: ./.github/workflows/reusable-security-scan.yml
    with:
      scan-type: "python"  # python, nodejs, fullstack
```

---

## 個別 App 設定

### 最小構成例

```yaml
# apps/your-app/.github/workflows/ci.yml
name: Your App CI/CD

on:
  push:
    paths: ["apps/your-app/**"]

jobs:
  ci:
    uses: ./.github/workflows/reusable-python-ci.yml
    with:
      working-directory: apps/your-app

  deploy:
    needs: ci
    uses: ./.github/workflows/reusable-deploy-railway.yml
    with:
      service-name: your-app
    secrets:
      RAILWAY_TOKEN: ${{ secrets.RAILWAY_TOKEN }}
```

### 必要な GitHub Secrets

リポジトリの **Settings > Secrets and variables > Actions** で設定：

| Secret 名 | 用途 |
|-----------|------|
| `GHCR_USERNAME` | GitHub Container Registry ユーザー名 |
| `GHCR_TOKEN` | GitHub Container Registry トークン |
| `GCP_PROJECT_ID` | Google Cloud プロジェクト ID |
| `GCP_SA_KEY` | GCP サービスアカウントキー (JSON) |
| `RAILWAY_TOKEN` | Railway API トークン |
| `FLY_API_TOKEN` | Fly.io API トークン |
| `VERCEL_TOKEN` | Vercel トークン |
| `VERCEL_ORG_ID` | Vercel 組織 ID |
| `VERCEL_PROJECT_ID` | Vercel プロジェクト ID |

---

## デプロイプラットフォーム

詳細は [DEPLOYMENT_PLATFORMS.md](./DEPLOYMENT_PLATFORMS.md) を参照。

| プラットフォーム | 推奨用途 | 無料枠 |
|-----------------|---------|--------|
| Vercel | フロントエンド、Serverless | ✅ |
| Google Cloud Run | コンテナ API | ✅ |
| Railway | フルスタック | $5/月 |
| Fly.io | グローバル分散 | $5/月 |
| Hugging Face Spaces | AI デモ | ✅ |
| Modal | GPU ワークロード | $30/月 |

---

## トラブルシューティング

### ワークフローが見つからない

```
Error: ./.github/workflows/reusable-*.yml not found
```

**解決策**: 再利用可能ワークフローはリポジトリルートの `.github/workflows/` に配置してください。

### 権限エラー

```
Error: Resource not accessible by integration
```

**解決策**: リポジトリの Settings > Actions > General で、Workflow permissions を "Read and write" に設定。

### Docker ビルド失敗

```
Error: Dockerfile not found
```

**解決策**: `agentflow/deploy/templates/` から Dockerfile をコピー:
```bash
cp agentflow/deploy/templates/Dockerfile.python apps/your-app/Dockerfile
```

