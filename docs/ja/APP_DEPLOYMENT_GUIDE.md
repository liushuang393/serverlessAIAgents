# アプリケーション個別デプロイ手順

新規 App を CI/CD パイプラインに追加する手順です。

---

## クイックスタート (5分)

### Step 1: ワークフローファイル作成

`apps/your-app/.github/workflows/ci.yml` を作成:

```yaml
name: Your App CI/CD

on:
  push:
    branches: [main]
    paths: ["apps/your-app/**"]
  pull_request:
    paths: ["apps/your-app/**"]

jobs:
  ci:
    uses: ./.github/workflows/reusable-python-ci.yml
    with:
      working-directory: apps/your-app

  deploy:
    needs: ci
    if: github.ref == 'refs/heads/main'
    uses: ./.github/workflows/reusable-deploy-railway.yml
    with:
      service-name: your-app
    secrets:
      RAILWAY_TOKEN: ${{ secrets.RAILWAY_TOKEN }}
```

### Step 2: Dockerfile を配置 (コンテナ系のみ)

```bash
# Python API の場合
cp agentflow/deploy/templates/Dockerfile.python apps/your-app/Dockerfile

# フルスタック (Python + React) の場合
cp agentflow/deploy/templates/Dockerfile.fullstack apps/your-app/Dockerfile
```

### Step 3: GitHub Secrets を設定

リポジトリの **Settings > Secrets** で必要な認証情報を追加。

### Step 4: Push してデプロイ

```bash
git add apps/your-app/
git commit -m "feat: add CI/CD for your-app"
git push
```

---

## 詳細設定

### 複数プラットフォームへのデプロイ

```yaml
jobs:
  ci:
    uses: ./.github/workflows/reusable-python-ci.yml
    with:
      working-directory: apps/your-app

  # フロントエンドは Vercel へ
  deploy-frontend:
    needs: ci
    uses: ./.github/workflows/reusable-deploy-vercel.yml
    with:
      working-directory: apps/your-app/frontend
    secrets:
      VERCEL_TOKEN: ${{ secrets.VERCEL_TOKEN }}
      VERCEL_ORG_ID: ${{ secrets.VERCEL_ORG_ID }}
      VERCEL_PROJECT_ID: ${{ secrets.VERCEL_PROJECT_ID }}

  # バックエンドは Cloud Run へ
  deploy-backend:
    needs: ci
    uses: ./.github/workflows/reusable-deploy-gcp-cloudrun.yml
    with:
      service-name: your-app-api
      working-directory: apps/your-app/backend
    secrets:
      GCP_PROJECT_ID: ${{ secrets.GCP_PROJECT_ID }}
      GCP_SA_KEY: ${{ secrets.GCP_SA_KEY }}
```

### 手動デプロイ (workflow_dispatch)

```yaml
on:
  workflow_dispatch:
    inputs:
      deploy_target:
        description: "デプロイ先"
        type: choice
        options: [none, railway, flyio, gcp]

jobs:
  deploy-railway:
    if: github.event.inputs.deploy_target == 'railway'
    uses: ./.github/workflows/reusable-deploy-railway.yml
    # ...
```

### E2E テストの追加

```yaml
jobs:
  e2e:
    needs: ci
    uses: ./.github/workflows/reusable-e2e-test.yml
    with:
      working-directory: apps/your-app/frontend
      base-url: http://localhost:3000
```

---

## チェックリスト

### 必須項目

- [ ] `.github/workflows/ci.yml` を作成
- [ ] `requirements.txt` または `pyproject.toml` が存在
- [ ] GitHub Secrets を設定

### 推奨項目

- [ ] Dockerfile を配置 (コンテナ系プラットフォーム)
- [ ] `tests/` ディレクトリにテストを追加
- [ ] `README.md` にデプロイ情報を記載

### コンテナ系プラットフォーム用

- [ ] Dockerfile が存在
- [ ] `/health` エンドポイントを実装
- [ ] `PORT` 環境変数に対応

---

## プラットフォーム別クイックリファレンス

### Railway (最も簡単)

```yaml
uses: ./.github/workflows/reusable-deploy-railway.yml
with:
  service-name: my-app
secrets:
  RAILWAY_TOKEN: ${{ secrets.RAILWAY_TOKEN }}
```

### Fly.io (グローバル)

```yaml
uses: ./.github/workflows/reusable-deploy-flyio.yml
with:
  app-name: my-app
  region: nrt
secrets:
  FLY_API_TOKEN: ${{ secrets.FLY_API_TOKEN }}
```

### Google Cloud Run (エンタープライズ)

```yaml
uses: ./.github/workflows/reusable-deploy-gcp-cloudrun.yml
with:
  service-name: my-api
  region: asia-northeast1
secrets:
  GCP_PROJECT_ID: ${{ secrets.GCP_PROJECT_ID }}
  GCP_SA_KEY: ${{ secrets.GCP_SA_KEY }}
```

### Hugging Face Spaces (AI デモ)

```yaml
uses: ./.github/workflows/reusable-deploy-huggingface.yml
with:
  space-name: username/my-demo
  space-sdk: gradio
secrets:
  HF_TOKEN: ${{ secrets.HF_TOKEN }}
```

---

## サポート

問題が発生した場合:

1. [CI_CD_SETUP.md](./CI_CD_SETUP.md) のトラブルシューティングを確認
2. GitHub Actions のログを確認
3. Issue を作成

