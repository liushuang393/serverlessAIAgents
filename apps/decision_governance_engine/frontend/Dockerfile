# ==============================================================================
# Decision Governance Engine Frontend - マルチステージ Dockerfile
# ==============================================================================
# ステージ1: Node.js でビルド
# ステージ2: Nginx で静的ファイル配信
#
# ビルド:
#   docker build -t dge-frontend .
#
# 実行:
#   docker run -p 80:80 dge-frontend
#
# docker-compose と連携:
#   docker-compose up --build
# ==============================================================================

# ---------------------------------------------------------------------------
# Stage 1: ビルド
# ---------------------------------------------------------------------------
FROM node:20-alpine AS builder

WORKDIR /app

# パッケージファイルをコピー
COPY package.json package-lock.json ./

# 依存関係インストール（CI 環境用に clean install）
RUN npm ci --silent

# ソースコードをコピー
COPY . .

# 本番環境変数を使用してビルド
# ARG で外部から上書き可能
ARG VITE_API_URL=
ARG VITE_DEBUG_MODE=false
ENV VITE_API_URL=$VITE_API_URL
ENV VITE_DEBUG_MODE=$VITE_DEBUG_MODE

# TypeScript チェック + 本番ビルド
RUN npm run build

# ---------------------------------------------------------------------------
# Stage 2: Nginx で配信
# ---------------------------------------------------------------------------
FROM nginx:alpine AS production

# セキュリティ: root 以外のユーザーで実行
RUN addgroup -g 1001 -S nginx-group && \
    adduser -S -D -H -u 1001 -h /var/cache/nginx -s /sbin/nologin -G nginx-group nginx-user

# Nginx 設定をコピー
COPY nginx/nginx.conf /etc/nginx/nginx.conf

# ビルド成果物をコピー
COPY --from=builder /app/dist /usr/share/nginx/html

# パーミッション設定
RUN chown -R nginx-user:nginx-group /usr/share/nginx/html && \
    chown -R nginx-user:nginx-group /var/cache/nginx && \
    chown -R nginx-user:nginx-group /var/log/nginx && \
    touch /var/run/nginx.pid && \
    chown -R nginx-user:nginx-group /var/run/nginx.pid

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost/health || exit 1

# ポート公開
EXPOSE 80

# 非 root ユーザーで実行
USER nginx-user

# Nginx をフォアグラウンドで起動
CMD ["nginx", "-g", "daemon off;"]

