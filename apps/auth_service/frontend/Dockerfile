# --- ステージ 1: ビルド ---
FROM node:22-alpine AS build

WORKDIR /app
COPY package.json package-lock.json* ./
RUN npm ci
COPY . .
RUN npm run build

# --- ステージ 2: nginx 配信 ---
FROM nginx:alpine

# SPA 用 nginx 設定
RUN rm /etc/nginx/conf.d/default.conf
COPY --from=build /app/dist /usr/share/nginx/html

# nginx.conf を生成（SPA フォールバック + API プロキシ）
RUN cat > /etc/nginx/conf.d/default.conf <<'NGINX'
server {
    listen 80;
    server_name _;
    root /usr/share/nginx/html;
    index index.html;

    # API リクエストをバックエンドへプロキシ
    location /auth/ {
        proxy_pass http://auth-service:8010;
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
