# デプロイプラットフォーム一覧

AgentFlow がサポートする全デプロイプラットフォームの詳細ガイドです。

---

## 📋 プラットフォーム比較表

| プラットフォーム | タイプ | GPU | 無料枠 | 推奨用途 |
|-----------------|--------|-----|--------|---------|
| **Vercel** | Serverless | ❌ | ✅ | フロントエンド、Edge Functions |
| **AWS Lambda** | Serverless | ❌ | ✅ | バックエンド API |
| **Google Cloud Run** | Container | ❌ | ✅ | コンテナ API |
| **Azure Container Apps** | Container | ❌ | ✅ | Microsoft 連携 |
| **Hugging Face Spaces** | AI Platform | ✅ | ✅ | AI デモ、ML モデル |
| **Modal** | AI Platform | ✅ | $30/月 | GPU 処理、ML 推論 |
| **Railway** | PaaS | ❌ | $5/月 | フルスタック |
| **Render** | PaaS | ❌ | ✅* | Web サービス |
| **Fly.io** | Edge | ❌ | $5/月 | グローバル分散 |

*Render 無料枠はスリープあり

---

## 🚀 Vercel

**推奨**: フロントエンド、Next.js、Edge Functions

### 必要な Secrets
```
VERCEL_TOKEN          # Vercel アクセストークン
VERCEL_ORG_ID         # 組織 ID (Settings > General)
VERCEL_PROJECT_ID     # プロジェクト ID
```

### ワークフロー例
```yaml
deploy:
  uses: ./.github/workflows/reusable-deploy-vercel.yml
  with:
    working-directory: apps/my-app/frontend
    production: true
  secrets:
    VERCEL_TOKEN: ${{ secrets.VERCEL_TOKEN }}
    VERCEL_ORG_ID: ${{ secrets.VERCEL_ORG_ID }}
    VERCEL_PROJECT_ID: ${{ secrets.VERCEL_PROJECT_ID }}
```

---

## ☁️ Google Cloud Run

**推奨**: コンテナベース API、スケーラブルサービス

### 必要な Secrets
```
GCP_PROJECT_ID        # GCP プロジェクト ID
GCP_SA_KEY            # サービスアカウントキー (JSON)
```

### ワークフロー例
```yaml
deploy:
  uses: ./.github/workflows/reusable-deploy-gcp-cloudrun.yml
  with:
    service-name: my-api
    region: asia-northeast1
  secrets:
    GCP_PROJECT_ID: ${{ secrets.GCP_PROJECT_ID }}
    GCP_SA_KEY: ${{ secrets.GCP_SA_KEY }}
```

---

## 🚂 Railway

**推奨**: フルスタック、簡単セットアップ

### 必要な Secrets
```
RAILWAY_TOKEN         # Railway API トークン
```

### ワークフロー例
```yaml
deploy:
  uses: ./.github/workflows/reusable-deploy-railway.yml
  with:
    service-name: my-app
  secrets:
    RAILWAY_TOKEN: ${{ secrets.RAILWAY_TOKEN }}
```

---

## 🪁 Fly.io

**推奨**: グローバル分散、低レイテンシ

### 必要な Secrets
```
FLY_API_TOKEN         # Fly.io API トークン
```

### ワークフロー例
```yaml
deploy:
  uses: ./.github/workflows/reusable-deploy-flyio.yml
  with:
    app-name: my-app
    region: nrt          # 東京
  secrets:
    FLY_API_TOKEN: ${{ secrets.FLY_API_TOKEN }}
```

### リージョン一覧
- `nrt` - 東京
- `hnd` - 羽田
- `sin` - シンガポール
- `lax` - ロサンゼルス
- `lhr` - ロンドン

---

## 🤗 Hugging Face Spaces

**推奨**: AI デモ、ML モデル公開

### 必要な Secrets
```
HF_TOKEN              # Hugging Face トークン
```

### ワークフロー例
```yaml
deploy:
  uses: ./.github/workflows/reusable-deploy-huggingface.yml
  with:
    space-name: username/my-demo
    space-sdk: gradio    # gradio, streamlit, docker
  secrets:
    HF_TOKEN: ${{ secrets.HF_TOKEN }}
```

---

## ⚡ Modal

**推奨**: GPU ワークロード、ML 推論

### 必要な Secrets
```
MODAL_TOKEN_ID        # Modal トークン ID
MODAL_TOKEN_SECRET    # Modal トークンシークレット
```

### ワークフロー例
```yaml
deploy:
  uses: ./.github/workflows/reusable-deploy-modal.yml
  with:
    app-name: my-ml-app
    modal-file: modal_app.py
  secrets:
    MODAL_TOKEN_ID: ${{ secrets.MODAL_TOKEN_ID }}
    MODAL_TOKEN_SECRET: ${{ secrets.MODAL_TOKEN_SECRET }}
```

---

## 🎨 Render

**推奨**: Web サービス、静的サイト

### 必要な Secrets
```
RENDER_API_KEY        # Render API キー
RENDER_DEPLOY_HOOK    # または Deploy Hook URL
```

### ワークフロー例
```yaml
deploy:
  uses: ./.github/workflows/reusable-deploy-render.yml
  with:
    service-id: srv-xxxxxxxxxxxx
  secrets:
    RENDER_API_KEY: ${{ secrets.RENDER_API_KEY }}
```

