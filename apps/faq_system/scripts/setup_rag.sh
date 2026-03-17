#!/usr/bin/env bash
# FAQ System RAG セットアップスクリプト
# Qdrant 起動 + faq_knowledge コレクション作成 + .env 設定 を一括実行する
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
APP_DIR="${REPO_ROOT}/apps/faq_system"

echo "[FAQ] RAG セットアップを開始..."

# 1. Qdrant セットアップ
echo "[1/3] Qdrant をセットアップ..."
bash "${REPO_ROOT}/scripts/setup_qdrant.sh"

# 2. コレクション作成（BizCore ランタイム経由）
echo "[2/3] faq_knowledge コレクションを作成..."
cd "${REPO_ROOT}"
# conda activate は subshell では効かないため、conda run を使う
conda run -n bizcore python -c "
import asyncio
from infrastructure.storage.database import DatabaseConfig, DatabaseManager
from shared.rag.collection_manager import CollectionManager
from shared.services.rag_service import ChunkStrategy, RetrievalMethod

async def main():
    db = DatabaseManager(config=DatabaseConfig())
    await db.init()
    await db.create_all_tables()
    mgr = CollectionManager(session_factory=db.session_factory)
    await mgr.create_collection(
        collection_name='faq_knowledge',
        display_name='FAQ ナレッジベース',
        app_name='faq_system',
        chunk_strategy=ChunkStrategy.SENTENCE.value,
        chunk_size=500,
        chunk_overlap=80,
        retrieval_method=RetrievalMethod.HYBRID.value,
        top_k=8,
        min_similarity=0.25,
    )
    print('[OK] faq_knowledge コレクション作成完了')
    await db.close()

asyncio.run(main())
" 2>&1 || echo "[WARN] コレクション作成をスキップ（既存 or DB 未接続）"

# 3. 環境変数確認
echo "[3/3] 環境変数を確認..."
if [ -f "${APP_DIR}/.env" ]; then
    if ! grep -q "QDRANT_URL" "${APP_DIR}/.env"; then
        echo "QDRANT_URL=http://localhost:6333" >> "${APP_DIR}/.env"
        echo "[OK] .env に QDRANT_URL を追加しました"
    else
        echo "[OK] .env に QDRANT_URL は既に設定済みです"
    fi
else
    echo "QDRANT_URL=http://localhost:6333" > "${APP_DIR}/.env"
    echo "[OK] .env を作成しました"
fi

echo ""
echo "[FAQ] RAG セットアップ完了!"
echo "  次のステップ: アプリを起動してください"
echo "  cd ${REPO_ROOT} && conda run -n bizcore python -m apps.faq_system.main"
