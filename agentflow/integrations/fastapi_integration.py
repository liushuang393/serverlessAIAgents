# -*- coding: utf-8 -*-
"""FastAPI 統合モジュール.

このモジュールは、AgentをFastAPI APIとして公開する機能を提供します。

使用例:
    >>> from agentflow.integrations.fastapi import AgentRouter, create_app
    >>> # 方式1: 既存のFastAPIアプリに追加
    >>> app.include_router(
    ...     AgentRouter(agents=["MyAgent", "OtherAgent"]),
    ...     prefix="/api"
    ... )
    >>> # 方式2: アプリごと生成
    >>> app = create_app(agents=[MyAgentClass])
"""

import json
import logging
from typing import Any

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class AgentRequest(BaseModel):
    """Agent呼び出しリクエスト.

    Args:
        input: 入力データ
        context: コンテキスト情報
        stream: ストリームモード
    """

    input: dict[str, Any] = Field(default_factory=dict, description="入力データ")
    context: dict[str, Any] | None = Field(default=None, description="コンテキスト")
    stream: bool = Field(default=False, description="ストリームモード")


class AgentResponse(BaseModel):
    """Agent呼び出しレスポンス.

    Args:
        success: 成功フラグ
        data: 出力データ
        error: エラーメッセージ
    """

    success: bool = Field(..., description="成功フラグ")
    data: dict[str, Any] | None = Field(default=None, description="出力データ")
    error: str | None = Field(default=None, description="エラー")


def AgentRouter(
    agents: list[str] | None = None,
    workflows: list[str] | None = None,
    prefix: str = "",
) -> Any:
    """Agent API ルーターを作成.

    Args:
        agents: 公開するAgent名リスト
        workflows: 公開するワークフロー名リスト
        prefix: URLプレフィックス

    Returns:
        FastAPI APIRouter
    """
    try:
        from fastapi import APIRouter, HTTPException
        from fastapi.responses import StreamingResponse
    except ImportError:
        logger.warning("FastAPI not installed. AgentRouter not available.")
        return None

    from agentflow.agent_decorator import AgentClient

    router = APIRouter(prefix=prefix, tags=["agents"])

    # Agent一覧エンドポイント
    @router.get("/agents")
    async def list_agents() -> dict[str, Any]:
        """登録されたAgent一覧を取得."""
        available = AgentClient.list_agents()
        if agents:
            available = [a for a in available if a in agents]
        return {"agents": available}

    # Agent呼び出しエンドポイント（POST /agents/{agent_id}/invoke）
    @router.post("/agents/{agent_id}/invoke")
    async def invoke_agent(agent_id: str, request: AgentRequest) -> AgentResponse:
        """Agentを呼び出し."""
        try:
            if agents and agent_id not in agents:
                raise HTTPException(status_code=404, detail=f"Agent not found: {agent_id}")

            client = AgentClient.get(agent_id)
            result = await client.invoke(request.input, request.context)
            return AgentResponse(success=True, data=result)
        except ValueError as e:
            raise HTTPException(status_code=404, detail=str(e))
        except Exception as e:
            logger.exception(f"Agent invocation failed: {e}")
            return AgentResponse(success=False, error=str(e))

    # Agentストリームエンドポイント（GET /agents/{agent_id}/stream）
    @router.post("/agents/{agent_id}/stream")
    async def stream_agent(agent_id: str, request: AgentRequest) -> StreamingResponse:
        """Agentをストリームモードで呼び出し."""
        try:
            if agents and agent_id not in agents:
                raise HTTPException(status_code=404, detail=f"Agent not found: {agent_id}")

            client = AgentClient.get(agent_id)

            async def event_generator():
                async for chunk in client.stream(request.input, request.context):
                    data = json.dumps(chunk) if isinstance(chunk, dict) else str(chunk)
                    yield f"data: {data}\n\n"

            return StreamingResponse(
                event_generator(),
                media_type="text/event-stream",
            )
        except ValueError as e:
            raise HTTPException(status_code=404, detail=str(e))

    # Agentスキーマエンドポイント（GET /agents/{agent_id}/schema）
    @router.get("/agents/{agent_id}/schema")
    async def get_agent_schema(agent_id: str) -> dict[str, Any]:
        """Agentのスキーマ情報を取得."""
        try:
            client = AgentClient.get(agent_id)
            # TODO: より詳細なスキーマ情報
            return {
                "name": client.name,
                "input_schema": {"type": "object"},
                "output_schema": {"type": "object"},
            }
        except ValueError as e:
            raise HTTPException(status_code=404, detail=str(e))

    return router


def create_app(
    agents: list[type] | None = None,
    title: str = "AgentFlow API",
    version: str = "1.0.0",
    **kwargs: Any,
) -> Any:
    """FastAPIアプリを作成.

    Args:
        agents: Agentクラスリスト（@agentデコレータ付き）
        title: APIタイトル
        version: APIバージョン
        **kwargs: FastAPI追加引数

    Returns:
        FastAPI アプリケーション
    """
    try:
        from fastapi import FastAPI
        from fastapi.middleware.cors import CORSMiddleware
    except ImportError:
        logger.error("FastAPI not installed. Please install: pip install fastapi")
        raise

    app = FastAPI(title=title, version=version, **kwargs)

    # CORS設定（開発用）
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

    # Agentを登録
    agent_names = []
    if agents:
        for agent_cls in agents:
            if hasattr(agent_cls, "_agent_name"):
                agent_names.append(agent_cls._agent_name)

    # ルーターを追加
    router = AgentRouter(agents=agent_names if agent_names else None)
    if router:
        app.include_router(router, prefix="/api")

    # ヘルスチェック
    @app.get("/health")
    async def health_check() -> dict[str, str]:
        return {"status": "healthy"}

    return app

