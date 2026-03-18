#!/usr/bin/env bash
# Qdrant ローカルセットアップスクリプト
# BizCore Platform の「Qdrant セットアップ」ボタンから自動実行される
set -euo pipefail

QDRANT_PORT="${QDRANT_PORT:-6333}"
QDRANT_GRPC_PORT="${QDRANT_GRPC_PORT:-6334}"
CONTAINER_NAME="bizcore_qdrant"

echo "[BizCore] Qdrant セットアップを開始..."

# Docker 確認
if ! command -v docker &> /dev/null; then
    echo "[ERROR] Docker がインストールされていません"
    echo "  インストール方法: https://docs.docker.com/get-docker/"
    exit 1
fi

# 既存コンテナチェック（起動中）
if docker ps --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
    echo "[OK] Qdrant は既に起動中です (port: ${QDRANT_PORT})"
    exit 0
fi

# 停止中コンテナがあれば起動
if docker ps -a --format '{{.Names}}' | grep -q "^${CONTAINER_NAME}$"; then
    echo "[INFO] 既存コンテナを起動します..."
    docker start "${CONTAINER_NAME}"
else
    echo "[INFO] Qdrant コンテナを作成・起動します..."
    docker run -d \
        --name "${CONTAINER_NAME}" \
        -p "${QDRANT_PORT}:6333" \
        -p "${QDRANT_GRPC_PORT}:6334" \
        -v bizcore_qdrant_storage:/qdrant/storage:z \
        qdrant/qdrant:latest
fi

# 起動待機（ヘルスチェック）
echo "[INFO] Qdrant 起動待機中..."
for i in $(seq 1 30); do
    if curl -sf "http://localhost:${QDRANT_PORT}/healthz" > /dev/null 2>&1; then
        echo "[OK] Qdrant が起動しました: http://localhost:${QDRANT_PORT}"
        echo ""
        echo "  環境変数に以下を設定してください:"
        echo "  QDRANT_URL=http://localhost:${QDRANT_PORT}"
        exit 0
    fi
    sleep 1
done

echo "[ERROR] Qdrant の起動タイムアウト（30秒）"
echo "  docker logs ${CONTAINER_NAME} でログを確認してください"
exit 1
