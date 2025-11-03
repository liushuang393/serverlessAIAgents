# AI学習プラットフォーム フロントエンド Dockerfile
FROM node:16-alpine as build

WORKDIR /app

# package.jsonとpackage-lock.jsonをコピー
COPY package*.json ./

# 依存関係のインストール
RUN npm ci

# アプリケーションコードをコピー
COPY . .

# アプリケーションをビルド
RUN npm run build

# 本番用のNginxイメージ
FROM nginx:alpine

# ビルドされたアプリケーションをNginxの公開ディレクトリにコピー
COPY --from=build /app/build /usr/share/nginx/html

# Nginxの設定ファイルをコピー（オプション）
# COPY nginx.conf /etc/nginx/nginx.conf

# ポートを公開
EXPOSE 80

# Nginxを起動
CMD ["nginx", "-g", "daemon off;"]
