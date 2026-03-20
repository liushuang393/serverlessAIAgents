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

# 依存関係インストール（eslint@10 と eslint-plugin-react-hooks の peer dep 競合を許容）
RUN npm ci --silent --legacy-peer-deps

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

# Nginx グローバル設定をコピー（server block は templates/ から動的生成）
COPY nginx/nginx.conf /etc/nginx/nginx.conf

# ビルド成果物をコピー
COPY --from=builder /app/dist /usr/share/nginx/html

# -----------------------------------------------------------------------
# 根本解決: DNS リゾルバーを /etc/resolv.conf から動的取得
#
# upstream ブロックに直接ホスト名を書くと nginx 起動時に静的解決され、
# backend が未起動・再起動中の場合にクラッシュする。
# 変数経由 (set $backend ...) にするとリクエスト時に解決されるため安全。
# resolver IP を固定（127.0.0.11）にすると Docker 専用になるため、
# /etc/resolv.conf から動的取得して K8s/Podman にも対応する。
# -----------------------------------------------------------------------

# Step 1: 起動時に /etc/resolv.conf から DNS を読み取るスクリプト
#   *.envsh は nginx entrypoint に source されるため export が後続スクリプトに伝搬する
RUN cat > /docker-entrypoint.d/01-set-resolver.envsh <<'ENVSH'
#!/bin/sh
RESOLVER=$(awk '/^nameserver/{print $2; exit}' /etc/resolv.conf)
export RESOLVER
echo "[nginx] DNS resolver: ${RESOLVER}"
ENVSH
RUN chmod +x /docker-entrypoint.d/01-set-resolver.envsh

# Step 2: server block テンプレート（${RESOLVER} は起動時に envsubst で展開）
#   20-envsubst-on-templates.sh が /etc/nginx/templates/*.template を処理する
RUN mkdir -p /etc/nginx/templates
RUN cat > /etc/nginx/templates/default.conf.template <<'NGINX'
server {
    listen 80;
    listen [::]:80;
    server_name localhost;

    root /usr/share/nginx/html;
    index index.html;

    # セキュリティヘッダー
    add_header X-Frame-Options "SAMEORIGIN" always;
    add_header X-Content-Type-Options "nosniff" always;
    add_header X-XSS-Protection "1; mode=block" always;
    add_header Referrer-Policy "strict-origin-when-cross-origin" always;

    # DNS リゾルバー（/etc/resolv.conf から動的取得・Docker/K8s/Podman 対応）
    resolver ${RESOLVER} valid=30s ipv6=off;

    # フロントエンド静的ファイル
    location / {
        try_files $uri $uri/ /index.html;

        location ~* \.html$ {
            add_header Cache-Control "no-cache, no-store, must-revalidate";
        }

        location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2)$ {
            expires 1y;
            add_header Cache-Control "public, immutable";
        }
    }

    # API リバースプロキシ
    # set 変数経由で proxy_pass するとリクエスト時に DNS 解決（起動時解決を回避）
    location /api/ {
        set $backend http://backend:8000;
        proxy_pass $backend;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_connect_timeout 60s;
        proxy_send_timeout 60s;
        proxy_read_timeout 300s;
    }

    # SSE ストリーミング専用エンドポイント
    location /api/decision/stream {
        set $backend http://backend:8000;
        proxy_pass $backend;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_set_header Connection '';
        proxy_buffering off;
        proxy_cache off;
        chunked_transfer_encoding off;
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
        proxy_set_header Accept 'text/event-stream';
    }

    # ヘルスチェック
    location /health {
        access_log off;
        return 200 "healthy\n";
        add_header Content-Type text/plain;
    }
}
NGINX

# パーミッション設定
# /etc/nginx/conf.d/ と /etc/nginx/templates/ は 20-envsubst-on-templates.sh が書き込む
# nginx-user で実行するため、これらのディレクトリへの書き込み権限が必要
RUN chown -R nginx-user:nginx-group /usr/share/nginx/html && \
    chown -R nginx-user:nginx-group /var/cache/nginx && \
    chown -R nginx-user:nginx-group /var/log/nginx && \
    chown -R nginx-user:nginx-group /etc/nginx/conf.d && \
    chown -R nginx-user:nginx-group /etc/nginx/templates && \
    touch /var/run/nginx.pid && \
    chown -R nginx-user:nginx-group /var/run/nginx.pid

# ヘルスチェック（127.0.0.1 を使用: localhost は IPv6 ::1 に解決される可能性があり不安定）
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://127.0.0.1/health || exit 1

# ポート公開
EXPOSE 80

# 非 root ユーザーで実行
USER nginx-user

# Nginx をフォアグラウンドで起動
CMD ["nginx", "-g", "daemon off;"]

