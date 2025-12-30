"""FastAPI メインアプリケーション.

REST API と WebSocket エンドポイントを提供します。
"""

import logging
from contextlib import asynccontextmanager
from typing import Any, AsyncGenerator

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware

from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.workflow import workflow

# ロギング設定
logging.basicConfig(
    level=getattr(logging, config.log_level),
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger(__name__)


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None, None]:
    """アプリケーションライフサイクル管理.

    Args:
        app: FastAPI アプリケーション

    Yields:
        None
    """
    # 起動時処理
    logger.info("Starting Market Trend Monitor API")
    await workflow.initialize()

    yield

    # 終了時処理
    logger.info("Shutting down Market Trend Monitor API")
    await workflow.cleanup()


# FastAPI アプリケーション作成
app = FastAPI(
    title="Market Trend Monitor API",
    description="市場動向監視システム REST API",
    version="1.0.0",
    lifespan=lifespan,
)

# CORS 設定
app.add_middleware(
    CORSMiddleware,
    allow_origins=config.api.cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/")
async def root() -> dict[str, str]:
    """ルートエンドポイント.

    Returns:
        ウェルカムメッセージ
    """
    return {
        "message": "Market Trend Monitor API",
        "version": "1.0.0",
        "status": "running",
    }


@app.get("/health")
async def health() -> dict[str, str]:
    """ヘルスチェックエンドポイント.

    Returns:
        ヘルスステータス
    """
    return {"status": "healthy"}


@app.post("/api/collect")
async def trigger_collection(params: dict[str, Any] | None = None) -> dict[str, Any]:
    """手動データ収集トリガー.

    Args:
        params: 収集パラメータ（オプション）
            - keywords: 検索キーワード
            - sources: データソース

    Returns:
        ワークフロー実行結果

    Raises:
        HTTPException: ワークフロー実行エラー
    """
    try:
        logger.info("Manual collection triggered")
        result = await workflow.run(params)
        return {"status": "success", "result": result}

    except Exception as e:
        logger.error(f"Collection failed: {e}")
        raise HTTPException(status_code=500, detail=str(e)) from e


@app.get("/api/trends")
async def get_trends() -> dict[str, Any]:
    """トレンド一覧取得.

    Returns:
        トレンドリスト

    Note:
        現在はデモ実装。本番環境ではデータベースから取得。
    """
    # TODO: データベースから取得
    return {
        "trends": [],
        "total": 0,
        "message": "データベース統合は未実装です",
    }


@app.get("/api/trends/{trend_id}")
async def get_trend(trend_id: str) -> dict[str, Any]:
    """トレンド詳細取得.

    Args:
        trend_id: トレンドID

    Returns:
        トレンド詳細

    Raises:
        HTTPException: トレンドが見つからない
    """
    # TODO: データベースから取得
    raise HTTPException(status_code=404, detail="Trend not found")


@app.get("/api/reports")
async def get_reports() -> dict[str, Any]:
    """レポート一覧取得.

    Returns:
        レポートリスト
    """
    # TODO: データベースから取得
    return {
        "reports": [],
        "total": 0,
        "message": "データベース統合は未実装です",
    }


@app.get("/api/reports/{report_id}")
async def get_report(report_id: str) -> dict[str, Any]:
    """レポート詳細取得.

    Args:
        report_id: レポートID

    Returns:
        レポート詳細

    Raises:
        HTTPException: レポートが見つからない
    """
    # TODO: データベースから取得
    raise HTTPException(status_code=404, detail="Report not found")


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(
        "apps.market_trend_monitor.backend.api.main:app",
        host=config.api.host,
        port=config.api.port,
        reload=True,
    )

