# --- ステージ 1: ビルド ---
FROM node:22-alpine AS build

WORKDIR /app
# app_config.json をルートに配置（vite.config.js が /app_config.json を参照するため）
COPY app_config.json /app_config.json
# フロントエンドの依存関係をインストール
COPY frontend/package.json frontend/package-lock.json* ./
RUN npm ci
# フロントエンドソースをコピー
COPY frontend/ .
RUN npm run build

# --- ステージ 2: nginx 配信 ---
FROM nginx:alpine

# SPA 用 nginx 設定
RUN rm /etc/nginx/conf.d/default.conf
COPY --from=build /app/dist /usr/share/nginx/html

# -----------------------------------------------------------------------
# 根本解決: DNS リゾルバーを /etc/resolv.conf から動的取得
#
# 背景:
#   nginx は upstream ホストを「起動時」に静的解決する。
#   resolver ディレクティブを固定 IP（例: 127.0.0.11）にすると
#   Docker 専用になり、Kubernetes・Podman 等では失敗する。
#
# 解決策:
#   nginx:alpine の .envsh 機構を利用し、コンテナ起動時に
#   /etc/resolv.conf の nameserver を読み取って ${RESOLVER} を設定。
#   20-envsubst-on-templates.sh がテンプレートに展開するため
#   Docker / Kubernetes / Podman すべての環境で動作する。
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

# Step 2: nginx 設定テンプレート（${RESOLVER} は起動時に展開）
#   20-envsubst-on-templates.sh が /etc/nginx/templates/*.template を処理する
RUN mkdir -p /etc/nginx/templates
RUN cat > /etc/nginx/templates/default.conf.template <<'NGINX'
server {
    listen 80;
    listen [::]:80;
    server_name _;
    root /usr/share/nginx/html;
    index index.html;

    # DNS リゾルバー（/etc/resolv.conf から動的取得・Docker/K8s/Podman 対応）
    resolver ${RESOLVER} valid=30s ipv6=off;

    # API リクエストをバックエンドへプロキシ
    # set 変数経由で proxy_pass するとリクエスト時に DNS 解決（起動時解決を回避）
    location /auth/ {
        set $backend http://auth-service:8010;
        proxy_pass $backend;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }

    # SPA フォールバック
    location / {
        try_files $uri $uri/ /index.html;
    }
}
NGINX

EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
