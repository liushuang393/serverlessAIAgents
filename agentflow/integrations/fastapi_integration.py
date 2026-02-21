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
    >>>
    >>> # Flow定義と結果APIも自動追加
    >>> # GET /api/flows/{flow_id}/definition
    >>> # GET /api/results/{result_id}
"""

import json
import logging
from collections.abc import AsyncIterator
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

            async def event_generator() -> AsyncIterator[str]:
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


def FlowRouter(prefix: str = "") -> Any:
    """Flow 定義・結果 API ルーターを作成.

    提供するエンドポイント:
    - GET /flows: Flow 定義一覧
    - GET /flows/{flow_id}/definition: Flow 定義取得
    - GET /flows/{flow_id}/typescript: TypeScript 型定義生成
    - GET /results/{result_id}: 結果取得
    - GET /results: 結果一覧
    """
    try:
        from fastapi import APIRouter, HTTPException
    except ImportError:
        logger.warning("FastAPI not installed. FlowRouter not available.")
        return None

    from agentflow.core.flow_definition import FlowDefinitionRegistry
    from agentflow.core.result_store import ResultStoreManager

    router = APIRouter(prefix=prefix, tags=["flows"])

    @router.get("/flows")
    async def list_flows() -> dict[str, Any]:
        """登録された Flow 定義一覧を取得."""
        registry = FlowDefinitionRegistry.get_instance()
        flows = registry.list_all()
        return {"flows": [{"flow_id": f.flow_id, "name": f.name, "version": f.version} for f in flows]}

    @router.get("/flows/{flow_id}/definition")
    async def get_flow_definition(flow_id: str) -> dict[str, Any]:
        """Flow 定義を取得（前端同期用）."""
        registry = FlowDefinitionRegistry.get_instance()
        definition = registry.get(flow_id)
        if not definition:
            raise HTTPException(status_code=404, detail=f"Flow not found: {flow_id}")
        return definition.to_dict()

    @router.get("/flows/{flow_id}/typescript")
    async def get_flow_typescript(flow_id: str) -> dict[str, str]:
        """Flow の TypeScript 型定義を生成."""
        registry = FlowDefinitionRegistry.get_instance()
        definition = registry.get(flow_id)
        if not definition:
            raise HTTPException(status_code=404, detail=f"Flow not found: {flow_id}")
        return {"typescript": definition.to_typescript()}

    @router.get("/results/{result_id}")
    async def get_result(result_id: str) -> dict[str, Any]:
        """結果を取得."""
        result = await ResultStoreManager.get(result_id)
        if not result:
            raise HTTPException(status_code=404, detail=f"Result not found: {result_id}")
        return result.model_dump()

    @router.get("/results")
    async def list_results(flow_id: str | None = None, limit: int = 100) -> dict[str, Any]:
        """結果一覧を取得."""
        results = await ResultStoreManager.list_results(flow_id, limit)
        return {"results": [r.model_dump() for r in results]}

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
        logger.exception("FastAPI not installed. Please install: pip install fastapi")
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

    # Agent ルーターを追加
    agent_router = AgentRouter(agents=agent_names if agent_names else None)
    if agent_router:
        app.include_router(agent_router, prefix="/api")

    # Flow/Result ルーターを追加
    flow_router = FlowRouter()
    if flow_router:
        app.include_router(flow_router, prefix="/api")

    # ヘルスチェック
    @app.get("/health")
    async def health_check() -> dict[str, str]:
        return {"status": "healthy"}

    return app


def create_sse_response(
    event_generator: Any,
    media_type: str = "text/event-stream",
    headers: dict[str, str] | None = None,
) -> Any:
    """AG-UI イベントから SSE StreamingResponse を作成.

    フレームワーク標準の SSE レスポンス生成。
    AGUIEvent の to_sse() メソッドを使用するため、
    フィールドの漏れがない。

    Args:
        event_generator: AGUIEvent を yield する非同期ジェネレーター
        media_type: Content-Type（デフォルト: text/event-stream）
        headers: 追加ヘッダー

    Returns:
        FastAPI StreamingResponse

    使用例:
        async def my_endpoint():
            async def generator():
                async for event in engine.process_with_events(request):
                    yield event  # AGUIEvent インスタンス

            return create_sse_response(generator())
    """
    try:
        from fastapi.responses import StreamingResponse
    except ImportError:
        msg = "FastAPI is required for create_sse_response"
        raise ImportError(msg)

    from agentflow.protocols.agui_events import AGUIEvent

    async def sse_generator() -> AsyncIterator[str]:
        """AGUIEvent を SSE 文字列に変換するジェネレーター."""
        try:
            async for event in event_generator:
                if isinstance(event, AGUIEvent):
                    # フレームワーク標準: to_sse() を使用
                    yield event.to_sse()
                elif isinstance(event, dict):
                    # 辞書の場合は直接 JSON 化
                    yield f"data: {json.dumps(event, ensure_ascii=False)}\n\n"
                else:
                    # その他は文字列化
                    yield f"data: {event!s}\n\n"
        except Exception as e:
            # エラー時も SSE 形式でエラーを送信
            error_data = {
                "event_type": "flow.error",
                "error_message": str(e),
                "error_type": type(e).__name__,
            }
            yield f"data: {json.dumps(error_data, ensure_ascii=False)}\n\n"

    default_headers = {
        "Cache-Control": "no-cache",
        "Connection": "keep-alive",
        "X-Accel-Buffering": "no",  # nginx バッファリング無効化
    }
    if headers:
        default_headers.update(headers)

    return StreamingResponse(
        sse_generator(),
        media_type=media_type,
        headers=default_headers,
    )
