# ==============================================================================
# FAQ System - Dockerfile
# AgentFlow フレームワーク用統一 Docker イメージ
# ==============================================================================

# -----------------------------------------------------------------------------
# Stage 1: 依存関係インストール
# -----------------------------------------------------------------------------
FROM python:3.13-slim AS dependencies

WORKDIR /app

# システム依存関係をインストール
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Python 依存関係をインストール
COPY backend/requirements.txt ./requirements.txt
RUN pip install --no-cache-dir -r requirements.txt

# -----------------------------------------------------------------------------
# Stage 2: 本番イメージ
# -----------------------------------------------------------------------------
FROM python:3.13-slim AS production

WORKDIR /app

# 非 root ユーザーを作成
RUN groupadd -r appuser && useradd -r -g appuser appuser

# 依存関係をコピー
COPY --from=dependencies /usr/local/lib/python3.13/site-packages /usr/local/lib/python3.13/site-packages
COPY --from=dependencies /usr/local/bin /usr/local/bin

# アプリケーションコードをコピー
COPY backend/ ./backend/
COPY main.py ./
COPY __init__.py ./

# 権限を設定
RUN chown -R appuser:appuser /app
USER appuser

# ポートを公開
EXPOSE 8080

# 環境変数
ENV PYTHONUNBUFFERED=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PORT=8080

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:${PORT}/health || exit 1

# アプリケーションを起動
CMD ["uvicorn", "main:app", "--host", "0.0.0.0", "--port", "8080"]

