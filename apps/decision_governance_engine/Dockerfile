# ==============================================================================
# Decision Governance Engine Backend - Dockerfile
# ==============================================================================
# Python + FastAPI バックエンド
#
# ビルド:
#   docker build -t dge-backend -f Dockerfile ../..
#   （リポジトリルートから agentflow パッケージを含める）
#
# 実行:
#   docker run -p 8001:8001 dge-backend
# ==============================================================================

FROM python:3.12-slim AS base

# 環境変数
ENV PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    PIP_NO_CACHE_DIR=1 \
    PIP_DISABLE_PIP_VERSION_CHECK=1

WORKDIR /app

# ---------------------------------------------------------------------------
# Stage: 依存関係インストール
# ---------------------------------------------------------------------------
FROM base AS dependencies

# システム依存関係
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# pyproject.toml から依存関係をインストール
# コンテキストはリポジトリルート想定
COPY pyproject.toml README.md ./
# agentflow パッケージをコピー（editable install に必要）
COPY agentflow/ /app/agentflow/
RUN pip install --no-cache-dir -e ".[dev]"

# DGE 固有の追加依存関係（PDF生成用）
RUN pip install --no-cache-dir reportlab

# ---------------------------------------------------------------------------
# Stage: 本番イメージ
# ---------------------------------------------------------------------------
FROM base AS production

# 非 root ユーザー作成
RUN groupadd -r appgroup && useradd -r -g appgroup appuser

# 依存関係をコピー
COPY --from=dependencies /usr/local/lib/python3.12/site-packages /usr/local/lib/python3.12/site-packages
COPY --from=dependencies /usr/local/bin /usr/local/bin

# アプリケーションコードをコピー
# コンテキストはリポジトリルート想定
COPY agentflow/ /app/agentflow/
COPY apps/decision_governance_engine/ /app/apps/decision_governance_engine/

# PYTHONPATH 設定
ENV PYTHONPATH=/app

# パーミッション設定
RUN chown -R appuser:appgroup /app
USER appuser

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
    CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:8001/api/health')" || exit 1

# ポート公開
EXPOSE 8001

# 起動コマンド（app_config.json のポート 8001 を使用）
CMD ["python", "-m", "apps.decision_governance_engine.api"]

