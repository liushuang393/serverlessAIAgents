
# backend/app/main.py
# AI学習プラットフォーム メインアプリケーション
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
import os
import logging

from app.api.v1.api import api_router
from app.core.config import settings
from app.database import create_tables

# ログ設定
logging.basicConfig(
    level=getattr(logging, settings.LOG_LEVEL),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger(__name__)

# FastAPIアプリケーションを作成
app = FastAPI(
    title=settings.PROJECT_NAME,
    version=settings.PROJECT_VERSION,
    description=settings.PROJECT_DESCRIPTION,
    openapi_url=f"{settings.API_V1_STR}/openapi.json",
    docs_url="/docs",
    redoc_url="/redoc"
)

# CORS設定
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.BACKEND_CORS_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# 静的ファイル配信設定
if not os.path.exists(settings.UPLOAD_DIR):
    os.makedirs(settings.UPLOAD_DIR)

if not os.path.exists(settings.CONTENT_DIR):
    os.makedirs(settings.CONTENT_DIR)

app.mount("/uploads", StaticFiles(directory=settings.UPLOAD_DIR), name="uploads")
app.mount("/contents", StaticFiles(directory=settings.CONTENT_DIR), name="contents")

# APIルーターを登録
app.include_router(api_router, prefix=settings.API_V1_STR)

@app.get("/")
def read_root():
    """
    ルートエンドポイント - ヘルスチェック

    Returns:
        アプリケーション情報
    """
    return {
        "message": "AI学習プラットフォーム バックエンドが正常に動作しています",
        "project": settings.PROJECT_NAME,
        "version": settings.PROJECT_VERSION,
        "docs_url": "/docs",
        "api_url": settings.API_V1_STR
    }

@app.get("/health")
async def health_check():
    health_status = {"status": "healthy"}
    
    # データベース接続チェック
    try:
        db = next(get_db())
        db.execute("SELECT 1")
        health_status["database"] = "connected"
    except Exception as e:
        health_status["database"] = f"error: {str(e)}"
        health_status["status"] = "unhealthy"
    
    # OpenAI API設定チェック
    health_status["openai"] = "configured" if settings.OPENAI_API_KEY else "not_configured"
    
    return health_status

@app.on_event("startup")
async def startup_event():
    """
    アプリケーション起動時の処理
    """
    logger.info(f"{settings.PROJECT_NAME} v{settings.PROJECT_VERSION} を起動中...")

    # データベーステーブルを作成
    try:
        create_tables()
        logger.info("データベーステーブルの作成/確認が完了しました")
    except Exception as e:
        logger.error(f"データベース初期化エラー: {e}")

    logger.info("アプリケーションの起動が完了しました")

@app.on_event("shutdown")
async def shutdown_event():
    """
    アプリケーション終了時の処理
    """
    logger.info("アプリケーションを終了しています...")
