# ==============================================================================
# Auth Service - Dockerfile
# ==============================================================================
# スタンドアロン認証サービス（JWT / OAuth2 / LDAP / SAML / MFA）
#
# ビルド:
#   docker build -t auth-service -f apps/auth_service/Dockerfile ../..
#   （リポジトリルートから agentflow パッケージを含める）
#
# 実行:
#   docker run -p 8010:8010 auth-service
#
# ※ 通常は docker compose 経由で起動してください:
#   cd apps/auth_service && docker compose up --build
# ==============================================================================

FROM python:3.13-slim AS base

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
RUN pip install --no-cache-dir -e ".[apps]"

# ---------------------------------------------------------------------------
# Stage: 本番イメージ
# ---------------------------------------------------------------------------
FROM base AS production

# 非 root ユーザー作成
RUN groupadd -r appgroup && useradd -r -g appgroup appuser

# 依存関係をコピー
COPY --from=dependencies /usr/local/lib/python3.13/site-packages /usr/local/lib/python3.13/site-packages
COPY --from=dependencies /usr/local/bin /usr/local/bin

# アプリケーションコードをコピー
# コンテキストはリポジトリルート想定
COPY agentflow/ /app/agentflow/
COPY apps/auth_service/ /app/apps/auth_service/

# PYTHONPATH 設定
ENV PYTHONPATH=/app

# データディレクトリ（SQLite 用）
RUN mkdir -p /app/data && chown -R appuser:appgroup /app

# パーミッション設定
USER appuser

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
    CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:8010/health')" || exit 1

# ポート公開
EXPOSE 8010

# 起動コマンド（app_config.json のポート 8010 を使用）
CMD ["python", "-m", "apps.auth_service.main"]
