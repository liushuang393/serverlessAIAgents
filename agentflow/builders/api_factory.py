"""APIFactory - FastAPIエンドポイント自動生成.

目的: REST/SSEエンドポイントの自動生成
職責:
- RESTエンドポイント作成
- SSEストリームエンドポイント作成
- エラーハンドリング統一
"""

from __future__ import annotations

import json
import logging
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from fastapi import FastAPI

    from agentflow.quick import FlowWrapper


class APIFactory:
    """FastAPIエンドポイント生成ファクトリ.

    Example:
        >>> factory = APIFactory(app, flow)
        >>> factory.create_rest_endpoint("/api/task")
        >>> factory.create_sse_endpoint("/api/task/stream")
    """

    def __init__(self, app: FastAPI, flow: FlowWrapper) -> None:
        """初期化.

        Args:
            app: FastAPIインスタンス
            flow: FlowWrapperインスタンス
        """
        self._app = app
        self._flow = flow
        self._logger = logging.getLogger(__name__)

    def create_rest_endpoint(
        self,
        path: str,
        *,
        method: str = "POST",
        tags: list[str] | None = None,
    ) -> None:
        """RESTエンドポイントを作成.

        Args:
            path: エンドポイントパス
            method: HTTPメソッド
            tags: OpenAPIタグ
        """
        from fastapi import HTTPException
        from pydantic import BaseModel

        flow = self._flow

        # 動的リクエストモデル
        class TaskRequest(BaseModel):
            """タスクリクエスト."""

            inputs: dict[str, Any]

        class TaskResponse(BaseModel):
            """タスクレスポンス."""

            status: str
            result: dict[str, Any]
            flow_name: str

        async def endpoint(request: TaskRequest) -> TaskResponse:
            """RESTエンドポイント処理."""
            try:
                result = await flow.run(request.inputs)
                return TaskResponse(
                    status="success",
                    result=result,
                    flow_name=flow.name,
                )
            except Exception as e:
                self._logger.exception(f"Flow実行エラー: {e}")
                raise HTTPException(status_code=500, detail=str(e)) from e

        # ルート登録
        self._app.add_api_route(
            path,
            endpoint,
            methods=[method],
            tags=tags or ["AgentFlow"],
            summary=f"Execute {flow.name}",
            response_model=TaskResponse,
        )

        self._logger.info(f"RESTエンドポイント作成: {method} {path}")

    def create_sse_endpoint(
        self,
        path: str,
        *,
        tags: list[str] | None = None,
    ) -> None:
        """SSEストリームエンドポイントを作成.

        Args:
            path: エンドポイントパス
            tags: OpenAPIタグ
        """
        from fastapi import Query
        from fastapi.responses import StreamingResponse

        flow = self._flow

        async def stream_endpoint(
            inputs: str = Query(..., description="JSON形式の入力データ"),
        ) -> StreamingResponse:
            """SSEストリームエンドポイント処理."""

            async def event_generator() -> Any:
                """イベント生成."""
                try:
                    # 入力をパース
                    input_data = json.loads(inputs)

                    # 開始イベント
                    yield f"data: {json.dumps({'type': 'start', 'flow': flow.name})}\n\n"

                    # Flow実行
                    result = await flow.run(input_data)

                    # 完了イベント
                    yield f"data: {json.dumps({'type': 'complete', 'result': result})}\n\n"

                except json.JSONDecodeError as e:
                    yield f"data: {json.dumps({'type': 'error', 'message': f'Invalid JSON: {e}'})}\n\n"
                except Exception as e:
                    self._logger.exception(f"Stream実行エラー: {e}")
                    yield f"data: {json.dumps({'type': 'error', 'message': str(e)})}\n\n"

            return StreamingResponse(
                event_generator(),
                media_type="text/event-stream",
                headers={
                    "Cache-Control": "no-cache",
                    "Connection": "keep-alive",
                },
            )

        # ルート登録
        self._app.add_api_route(
            path,
            stream_endpoint,
            methods=["GET"],
            tags=tags or ["AgentFlow"],
            summary=f"Stream {flow.name}",
        )

        self._logger.info(f"SSEエンドポイント作成: GET {path}")

