# ==============================================================================
# Auth Service - Dockerfile
# ==============================================================================
# スタンドアロン認証サービス（JWT / OAuth2 / LDAP / SAML / MFA）
#
# ビルド:
#   docker build -t auth-service -f shared/auth_service/Dockerfile ../..
#   （リポジトリルートから BizCore コア層を含める）
#
# 実行:
#   docker run -p 8010:8010 auth-service
#
# ※ 通常は docker compose 経由で起動してください:
#   cd shared/auth_service && docker compose up --build
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
# 分層パッケージをコピー（editable install に必要）
COPY contracts/ /app/contracts/
COPY infrastructure/ /app/infrastructure/
COPY shared/ /app/shared/
COPY kernel/ /app/kernel/
COPY harness/ /app/harness/
COPY control_plane/ /app/control_plane/
COPY domain/ /app/domain/
RUN pip install --no-cache-dir -e ".[apps]"

# ---------------------------------------------------------------------------
# Stage: 本番イメージ
# ---------------------------------------------------------------------------
FROM base AS production

# ポート設定（ビルド時に override 可能、実行時も ENV として引き継ぐ）
ARG PORT=8010
ENV PORT=${PORT}

# 非 root ユーザー作成
RUN groupadd -r appgroup && useradd -r -g appgroup appuser

# 依存関係をコピー
COPY --from=dependencies /usr/local/lib/python3.13/site-packages /usr/local/lib/python3.13/site-packages
COPY --from=dependencies /usr/local/bin /usr/local/bin

# アプリケーションコードをコピー
# コンテキストはリポジトリルート想定
COPY contracts/ /app/contracts/
COPY infrastructure/ /app/infrastructure/
COPY shared/ /app/shared/
COPY kernel/ /app/kernel/
COPY harness/ /app/harness/
COPY control_plane/ /app/control_plane/
COPY domain/ /app/domain/
COPY shared/auth_service/ /app/shared/auth_service/

# PYTHONPATH 設定
ENV PYTHONPATH=/app

# データディレクトリ（SQLite 用）
RUN mkdir -p /app/data && chown -R appuser:appgroup /app

# パーミッション設定
USER appuser

# ヘルスチェック（${PORT} は実行時 ENV から展開される）
HEALTHCHECK --interval=10s --timeout=10s --start-period=60s --retries=5 \
    CMD python -c "import urllib.request; urllib.request.urlopen('http://localhost:${PORT}/health')" || exit 1

# ポート公開（ARG PORT のビルド時値を使用）
EXPOSE ${PORT}

# 起動コマンド（sh -c 経由で ${PORT} を実行時に展開）
CMD ["sh", "-c", "exec uvicorn shared.auth_service.main:app --host 0.0.0.0 --port ${PORT}"]
