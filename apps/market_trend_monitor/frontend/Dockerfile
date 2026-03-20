FROM node:20-alpine AS build

WORKDIR /app

COPY apps/market_trend_monitor/frontend/package*.json ./
RUN npm ci --legacy-peer-deps

COPY apps/market_trend_monitor/frontend/ ./
# Docker環境ではnginxリバースプロキシ経由のため、相対URL "/api" をデフォルト。
# 前後台を別マシンに分離する場合は build-arg で絶対URLを指定:
#   --build-arg VITE_API_BASE_URL=http://192.168.166.41:8002/api
ARG VITE_API_BASE_URL=/api
ENV VITE_API_BASE_URL=${VITE_API_BASE_URL}

RUN npm run build

FROM nginx:1.27-alpine

# ビルド成果物をコピー
COPY --from=build /app/dist /usr/share/nginx/html

# -----------------------------------------------------------------------
# 根本解決: DNS リゾルバーを /etc/resolv.conf から動的取得
#
# proxy_pass に直接ホスト名を書くと nginx 起動時に静的解決され、
# backend が一時停止中でもクラッシュする。変数経由にするとリクエスト時解決。
# resolver IP を固定（127.0.0.11）にすると Docker 専用になるため、
# /etc/resolv.conf から動的取得して K8s/Podman にも対応する。
# -----------------------------------------------------------------------

# Step 1: 起動時に /etc/resolv.conf から DNS を読み取るスクリプト
RUN cat > /docker-entrypoint.d/01-set-resolver.envsh <<'ENVSH'
#!/bin/sh
RESOLVER=$(awk '/^nameserver/{print $2; exit}' /etc/resolv.conf)
export RESOLVER
echo "[nginx] DNS resolver: ${RESOLVER}"
ENVSH
RUN chmod +x /docker-entrypoint.d/01-set-resolver.envsh

# Step 2: nginx 設定テンプレート（${RESOLVER} と ${BACKEND_HOST} は起動時に展開）
RUN mkdir -p /etc/nginx/templates
RUN cat > /etc/nginx/templates/default.conf.template <<'NGINX'
server {
    listen 80;
    server_name _;

    root /usr/share/nginx/html;
    index index.html;

    # DNS リゾルバー（/etc/resolv.conf から動的取得・Docker/K8s/Podman 対応）
    resolver ${RESOLVER} valid=30s ipv6=off;

    location /api/ {
        # set 変数経由で proxy_pass するとリクエスト時に DNS 解決（起動時解決を回避）
        set $backend http://backend:8002;
        proxy_pass $backend;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    location /health {
        access_log off;
        return 200 'ok';
        add_header Content-Type text/plain;
    }

    location / {
        try_files $uri $uri/ /index.html;
    }
}
NGINX

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
