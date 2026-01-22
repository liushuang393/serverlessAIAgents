# -*- coding: utf-8 -*-
# {{ app_name }} API Dockerfile
# Python 3.12 + FastAPI + uvicorn

FROM python:3.12-slim

# 作業ディレクトリ設定
WORKDIR /app

# システム依存パッケージをインストール
RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc \
    libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Python 依存パッケージをインストール
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# アプリケーションコードをコピー
COPY . .

# ポート公開
EXPOSE 8000

# ヘルスチェックエンドポイント
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:8000/health')"

# アプリケーション起動
CMD ["uvicorn", "api:app", "--host", "0.0.0.0", "--port", "8000"]

