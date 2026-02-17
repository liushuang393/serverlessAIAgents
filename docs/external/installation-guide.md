# インストール手順（顧客環境向け）

## 1. 前提

1. Python 3.13 以上
2. Node.js 22 以上
3. Docker / Docker Compose
4. リポジトリ取得済み

## 2. バックエンド起動

1. `conda activate agentflow`
2. `pip install -e "[apps,dev]"`
3. `python -m apps.platform.main serve --port 8000`

## 3. フロントエンド起動

1. `cd apps/platform/frontend`
2. `npm install`
3. `npm run dev`

## 4. 動作確認

1. `http://localhost:8000/health`
2. `http://localhost:8000/docs`
3. `http://localhost:3000`

## 5. 初回利用

1. Studio を選択
2. テンプレートを選択
3. データと権限を設定
4. 実行して成果物を確認
