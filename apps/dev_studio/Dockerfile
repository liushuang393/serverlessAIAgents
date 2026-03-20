# ==============================================================================
# Developer Studio - Dockerfile
# ==============================================================================
# 独立した開発支援ツールサーバー。
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
    curl \
    && rm -rf /var/lib/apt/lists/*

# pyproject.toml から依存関係をインストール
COPY pyproject.toml README.md ./
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

# 非 root ユーザー作成
RUN groupadd -r appgroup && useradd -r -g appgroup appuser

# 依存関係をコピー
COPY --from=dependencies /usr/local/lib/python3.13/site-packages /usr/local/lib/python3.13/site-packages
COPY --from=dependencies /usr/local/bin /usr/local/bin

# アプリケーションコードをコピー
COPY contracts/ /app/contracts/
COPY infrastructure/ /app/infrastructure/
COPY shared/ /app/shared/
COPY kernel/ /app/kernel/
COPY harness/ /app/harness/
COPY control_plane/ /app/control_plane/
COPY domain/ /app/domain/
COPY apps/dev_studio/ /app/apps/dev_studio/

# PYTHONPATH 設定
ENV PYTHONPATH=/app

# パーミッション設定
RUN chown -R appuser:appgroup /app
USER appuser

# ヘルスチェック (Port 8011)
HEALTHCHECK --interval=30s --timeout=10s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8011/health || exit 1

# ポート公開 (Dev Studio Port)
EXPOSE 8011

# 起動コマンド: apps.dev_studio.main を実行
CMD ["python", "-m", "apps.dev_studio.main"]
