"""API 路由工厂.

AgentFlow アプリケーション用の標準 API エンドポイントを自動生成。

Example:
    >>> from agentflow.sdk.api import create_agent_router
    >>> from my_app.engine import MyEngine
    >>>
    >>> router = create_agent_router(
    ...     engine=MyEngine,
    ...     prefix="/api/my-app",
    ...     tags=["my-app"],
    ... )
    >>> app.include_router(router)

自動生成されるエンドポイント:
- GET  /health       - ヘルスチェック
- GET  /agents       - Agent 一覧
- POST /process      - 同期処理
- GET  /stream       - SSE ストリーム
- GET  /result/{id}  - 結果取得
"""

from collections.abc import Callable
from typing import Any, Protocol, TypeVar, runtime_checkable

from fastapi import APIRouter, HTTPException, Query, Request
from pydantic import BaseModel

from agentflow.sdk.api.errors import AgentApiException, ErrorCode
from agentflow.sdk.api.sse import create_sse_response


# ========================================
# Engine プロトコル
# ========================================


@runtime_checkable
class AgentEngineProtocol(Protocol):
    """Agent Engine プロトコル.

    create_agent_router で使用可能な Engine の最小インターフェース。
    """

    def get_agents(self) -> list[dict[str, Any]]:
        """利用可能な Agent 一覧を取得."""
        ...

    async def process(self, request: dict[str, Any]) -> dict[str, Any]:
        """同期処理."""
        ...

    async def process_with_events(self, request: dict[str, Any]) -> Any:  # AsyncGenerator
        """イベント付きストリーム処理."""
        ...


T = TypeVar("T", bound=AgentEngineProtocol)


# ========================================
# レスポンスモデル
# ========================================


class HealthResponse(BaseModel):
    """ヘルスチェックレスポンス."""

    status: str = "ok"
    version: str = "1.0.0"


class AgentInfo(BaseModel):
    """Agent 情報."""

    id: str
    name: str
    label: str
    description: str | None = None


class AgentsResponse(BaseModel):
    """Agent 一覧レスポンス."""

    agents: list[AgentInfo]


# ========================================
# 路由工厂
# ========================================


def create_agent_router(
    engine: type[T] | T | Callable[[], T],
    prefix: str = "/api",
    tags: list[str] | None = None,
    version: str = "1.0.0",
    include_health: bool = True,
    include_agents: bool = True,
    include_sync: bool = True,
    include_stream: bool = True,
    include_result: bool = True,
) -> APIRouter:
    """Agent API 路由を生成.

    指定された Engine から標準 API エンドポイントを自動生成。

    Args:
        engine: Engine クラス、インスタンス、またはファクトリ関数
        prefix: API プレフィックス
        tags: OpenAPI タグ
        version: API バージョン
        include_health: /health エンドポイントを含む
        include_agents: /agents エンドポイントを含む
        include_sync: /process エンドポイントを含む
        include_stream: /stream エンドポイントを含む
        include_result: /result/{id} エンドポイントを含む

    Returns:
        FastAPI APIRouter

    Example:
        >>> router = create_agent_router(
        ...     engine=DecisionEngine,
        ...     prefix="/api/decision",
        ...     tags=["decision"],
        ... )
    """
    router = APIRouter(prefix=prefix, tags=tags or [])

    # Engine インスタンス取得
    def get_engine() -> T:
        if callable(engine) and not isinstance(engine, type):
            return engine()
        if isinstance(engine, type):
            return engine()
        return engine

    # 結果ストア（簡易実装）
    results_store: dict[str, dict[str, Any]] = {}

    # ----------------------------------------
    # GET /health
    # ----------------------------------------
    if include_health:

        @router.get("/health", response_model=HealthResponse)
        async def health() -> HealthResponse:
            """ヘルスチェック."""
            return HealthResponse(status="ok", version=version)

    # ----------------------------------------
    # GET /agents
    # ----------------------------------------
    if include_agents:

        @router.get("/agents", response_model=AgentsResponse)
        async def get_agents() -> AgentsResponse:
            """利用可能な Agent 一覧を取得."""
            eng = get_engine()
            agents_data = eng.get_agents()
            return AgentsResponse(agents=[AgentInfo(**a) for a in agents_data])

    # ----------------------------------------
    # POST /process
    # ----------------------------------------
    if include_sync:

        @router.post("/process")
        async def process(request: Request) -> dict[str, Any]:
            """同期処理.

            リクエストボディを Engine に渡して処理。
            """
            try:
                body = await request.json()
                eng = get_engine()
                return await eng.process(body)
            except AgentApiException:
                raise
            except Exception as e:
                raise AgentApiException(
                    code=ErrorCode.SERVER_ERROR,
                    message=f"処理中にエラーが発生しました: {e!s}",
                    retryable=True,
                    status_code=500,
                ) from e

    # ----------------------------------------
    # GET /stream
    # ----------------------------------------
    if include_stream:

        @router.get("/stream")
        async def stream(
            question: str = Query(..., description="処理対象の質問"),
            budget: float | None = Query(None, description="予算（オプション）"),
            timeline_months: int | None = Query(None, description="期間（月）"),
        ):
            """SSE ストリーム処理.

            AG-UI イベントをリアルタイムで配信。
            """
            request_data = {
                "question": question,
                "budget": budget,
                "timeline_months": timeline_months,
            }

            eng = get_engine()
            return create_sse_response(eng.process_with_events(request_data))

    # ----------------------------------------
    # GET /result/{result_id}
    # ----------------------------------------
    if include_result:

        @router.get("/result/{result_id}")
        async def get_result(result_id: str) -> dict[str, Any]:
            """結果を取得.

            ストアされた処理結果を ID で取得。
            """
            if result_id not in results_store:
                raise HTTPException(status_code=404, detail="結果が見つかりません")
            return results_store[result_id]

    return router
