FROM python:3.13-slim

# ポート設定（ビルド時に override 可能、実行時も ENV として引き継ぐ）
ARG PORT=8002
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1 \
    PYTHONPATH=/app \
    PORT=${PORT}

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

COPY pyproject.toml README.md /app/
COPY contracts /app/contracts
COPY infrastructure /app/infrastructure
COPY shared /app/shared
COPY kernel /app/kernel
COPY harness /app/harness
COPY control_plane /app/control_plane
COPY domain /app/domain
COPY apps/market_trend_monitor /app/apps/market_trend_monitor

RUN pip install --no-cache-dir -e ".[apps]" \
    && pip install --no-cache-dir -r /app/apps/market_trend_monitor/backend/requirements.txt

# ポート公開（ARG PORT のビルド時値を使用）
EXPOSE ${PORT}

# ヘルスチェック（${PORT} は実行時 ENV から展開される）
HEALTHCHECK --interval=30s --timeout=10s --start-period=20s --retries=3 \
  CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:${PORT}/health')" || exit 1

# 起動コマンド（uvicorn 直接起動で ${PORT} を実行時に展開）
CMD ["sh", "-c", "exec uvicorn apps.market_trend_monitor.backend.api.main:app --host 0.0.0.0 --port ${PORT}"]
