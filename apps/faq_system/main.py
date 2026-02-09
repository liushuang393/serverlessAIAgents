"""FAQ System Demo - フレームワーク級Agent/サービスのデモアプリ.

このアプリはフレームワーク層のAgentとサービスを呼び出すのみで、
業務ロジックはフレームワーク側で実装されています。

アーキテクチャ:
    App層（薄い）          → フレームワーク層（厚い）
    ─────────────────────────────────────────────
    /api/chat              → FAQAgent
    /api/rag/*             → RAGService
    /api/sql/*             → Text2SQLService

機能:
- RAG 検索（ナレッジベース）
- Text2SQL（データベースクエリ）
- チャート生成
- フォローアップ提案

使用例:
    uvicorn apps.faq_system.main:app --reload --port 8001
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Any

from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import FileResponse, HTMLResponse, StreamingResponse
from pydantic import BaseModel, Field

# フレームワーク層 Agent/サービスをインポート
# ⚠️ 注意: Agent は agentflow/agents/ に実装されています
from agentflow.agents import (
    FAQAgent,
    FAQAgentConfig,
    SalesAgent,
    SalesAgentConfig,
)
from agentflow.services import (
    RAGConfig,
    RAGService,
    SuggestionConfig,
    SuggestionService,
    Text2SQLConfig,
    Text2SQLService,
)


# =============================================================================
# アプリケーション設定
# =============================================================================


app = FastAPI(
    title="FAQ System Demo",
    description="AgentFlow フレームワーク級サービスのデモアプリ",
    version="1.0.0",
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# =============================================================================
# リクエスト/レスポンスモデル
# =============================================================================


class ChatRequest(BaseModel):
    """チャットリクエスト."""
    message: str = Field(..., description="ユーザーメッセージ")
    session_id: str | None = Field(None, description="セッションID")


class RAGQueryRequest(BaseModel):
    """RAGクエリリクエスト."""
    question: str = Field(..., description="質問")
    collection: str = Field("default", description="コレクション名")
    top_k: int = Field(5, description="取得件数")


class SQLQueryRequest(BaseModel):
    """SQLクエリリクエスト."""
    question: str = Field(..., description="質問")


class AddDocumentRequest(BaseModel):
    """ドキュメント追加リクエスト."""
    content: str = Field(..., description="ドキュメント内容")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


# =============================================================================
# サービスインスタンス（遅延初期化・私有）
# ⚠️ 注意: これらは内部実装です。外部から直接呼び出さないでください。
# =============================================================================


_services: dict[str, Any] = {}
_artifact_registry: dict[str, Path] = {}


def _get_rag_service() -> RAGService:
    """RAGサービス取得（私有）."""
    if "rag" not in _services:
        _services["rag"] = RAGService(RAGConfig(
            collection=os.getenv("RAG_COLLECTION", "faq_knowledge"),
            chunk_strategy="semantic",
            reranker="bm25",
        ))
    return _services["rag"]


def _get_sql_service() -> Text2SQLService:
    """SQLサービス取得（私有）."""
    if "sql" not in _services:
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["sql"] = Text2SQLService(Text2SQLConfig(
            schema=schema,
            auto_chart=True,
        ))
    return _services["sql"]


def _get_suggestion_service() -> SuggestionService:
    """提案サービス取得（私有）."""
    if "suggestion" not in _services:
        _services["suggestion"] = SuggestionService(SuggestionConfig(
            max_suggestions=5,
            language="ja",
        ))
    return _services["suggestion"]


def _get_faq_agent() -> FAQAgent:
    """FAQAgent取得（私有）."""
    if "faq_agent" not in _services:
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["faq_agent"] = FAQAgent(FAQAgentConfig(
            rag_collection=os.getenv("RAG_COLLECTION", "faq_knowledge"),
            sql_schema=schema,
        ))
    return _services["faq_agent"]


def _get_sales_agent() -> SalesAgent:
    """SalesAgent取得（私有）."""
    if "sales_agent" not in _services:
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["sales_agent"] = SalesAgent(SalesAgentConfig(
            sql_schema=schema,
        ))
    return _services["sales_agent"]


def _register_artifacts(payload: dict[str, Any]) -> dict[str, Any]:
    """生成アセットを登録し、ダウンロードURLを注入."""
    artifacts = payload.get("artifacts")
    if not isinstance(artifacts, list):
        return payload

    for artifact in artifacts:
        if not isinstance(artifact, dict):
            continue
        artifact_id = str(artifact.get("artifact_id", "")).strip()
        file_path = str(artifact.get("file_path", "")).strip()
        if not artifact_id or not file_path:
            continue

        path_obj = Path(file_path)
        if not path_obj.exists() or not path_obj.is_file():
            continue

        _artifact_registry[artifact_id] = path_obj.resolve()
        artifact["download_url"] = f"/api/assets/{artifact_id}/download"

    rich_response = payload.get("rich_response")
    if isinstance(rich_response, dict):
        components = rich_response.get("components")
        if isinstance(components, list):
            for component in components:
                if not isinstance(component, dict):
                    continue
                props = component.get("props")
                if not isinstance(props, dict):
                    continue
                url = props.get("url")
                if not isinstance(url, str) or not url.startswith("artifact://"):
                    continue
                artifact_id = url.replace("artifact://", "", 1)
                if artifact_id in _artifact_registry:
                    props["url"] = f"/api/assets/{artifact_id}/download"
    return payload


# =============================================================================
# API エンドポイント
# =============================================================================


@app.get("/", response_class=HTMLResponse)
async def index() -> str:
    """トップページ."""
    return """
    <!DOCTYPE html>
    <html>
    <head>
        <title>FAQ System Demo</title>
        <style>
            body { font-family: sans-serif; max-width: 800px; margin: 0 auto; padding: 20px; }
            .chat-box { border: 1px solid #ccc; height: 400px; overflow-y: auto; padding: 10px; margin-bottom: 10px; }
            .input-box { display: flex; }
            .input-box input { flex: 1; padding: 10px; }
            .input-box button { padding: 10px 20px; }
            .message { margin: 10px 0; }
            .user { text-align: right; color: blue; }
            .assistant { text-align: left; color: green; }
        </style>
    </head>
    <body>
        <h1>FAQ System Demo</h1>
        <p>AgentFlow フレームワーク級サービスを使用したFAQシステムのデモです。</p>
        <div class="chat-box" id="chat-box"></div>
        <div class="input-box">
            <input type="text" id="message" placeholder="質問を入力..." />
            <button onclick="sendMessage()">送信</button>
        </div>
        <script>
            async function sendMessage() {
                const input = document.getElementById('message');
                const chatBox = document.getElementById('chat-box');
                const message = input.value.trim();
                if (!message) return;

                // ユーザーメッセージ表示
                chatBox.innerHTML += '<div class="message user">👤 ' + message + '</div>';
                input.value = '';

                // API呼び出し
                try {
                    const response = await fetch('/api/chat', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ message }),
                    });
                    const data = await response.json();

                    // アシスタント回答表示
                    chatBox.innerHTML += '<div class="message assistant">🤖 ' + data.answer + '</div>';

                    // 提案表示
                    if (data.suggestions && data.suggestions.length > 0) {
                        let suggestHtml = '<div class="message assistant">💡 提案: ';
                        data.suggestions.forEach(s => {
                            suggestHtml += '<button onclick="fillMessage(\\'' + s.text + '\\')">' + s.text + '</button> ';
                        });
                        suggestHtml += '</div>';
                        chatBox.innerHTML += suggestHtml;
                    }

                    chatBox.scrollTop = chatBox.scrollHeight;
                } catch (e) {
                    chatBox.innerHTML += '<div class="message assistant">❌ エラー: ' + e.message + '</div>';
                }
            }

            function fillMessage(text) {
                document.getElementById('message').value = text;
            }

            document.getElementById('message').addEventListener('keypress', (e) => {
                if (e.key === 'Enter') sendMessage();
            });
        </script>
    </body>
    </html>
    """


@app.post("/api/chat")
async def chat(request: ChatRequest) -> dict[str, Any]:
    """チャット API.

    FAQAgent を呼び出し、質問に回答します。
    FAQAgent が内部でクエリタイプを判定し、適切なサービスを使用します。
    """
    agent = _get_faq_agent()
    result = await agent.run({"question": request.message})
    return _register_artifacts(result)


@app.post("/api/maq/chat")
async def maq_chat(request: ChatRequest) -> dict[str, Any]:
    """MAQ統合チャット API.

    社内FAQ・SQL分析・営業資料画像生成を単一入口で処理する。
    """
    return await chat(request)


@app.post("/api/chat/stream")
async def chat_stream(request: ChatRequest) -> StreamingResponse:
    """チャット API（ストリーム版）.

    SSE でリアルタイムに進捗を返します。
    """
    agent = _get_faq_agent()

    async def event_generator():
        async for event in agent.run_stream({"question": request.message}):
            if event.get("type") == "result" and isinstance(event.get("data"), dict):
                event["data"] = _register_artifacts(event["data"])
            yield f"data: {json.dumps(event)}\n\n"

    return StreamingResponse(event_generator(), media_type="text/event-stream")


@app.post("/api/rag/query")
async def rag_query(request: RAGQueryRequest) -> dict[str, Any]:
    """RAG クエリ API."""
    service = RAGService(RAGConfig(
        collection=request.collection,
        top_k=request.top_k,
    ))
    result = await service.execute(action="query", question=request.question)
    return result.data


@app.post("/api/rag/add")
async def rag_add_document(request: AddDocumentRequest) -> dict[str, Any]:
    """ドキュメント追加 API."""
    service = _get_rag_service()
    result = await service.execute(
        action="add_document",
        content=request.content,
        metadata=request.metadata,
    )
    return result.data


@app.post("/api/sql/query")
async def sql_query(request: SQLQueryRequest) -> dict[str, Any]:
    """SQL クエリ API."""
    service = _get_sql_service()
    result = await service.execute(action="query", question=request.question)
    return result.data


@app.post("/api/sales/analyze")
async def sales_analyze(request: SQLQueryRequest) -> dict[str, Any]:
    """売上分析 API.

    SalesAgent を使用して売上データを分析します。
    """
    agent = _get_sales_agent()
    return await agent.run({"question": request.question})


@app.get("/api/assets/{artifact_id}/download")
async def download_artifact(artifact_id: str) -> FileResponse:
    """生成アセットをダウンロード."""
    path_obj = _artifact_registry.get(artifact_id)
    if path_obj is None:
        raise HTTPException(status_code=404, detail="artifact not found")
    if not path_obj.exists() or not path_obj.is_file():
        raise HTTPException(status_code=404, detail="artifact file missing")
    return FileResponse(
        path=str(path_obj),
        filename=path_obj.name,
        media_type="application/octet-stream",
    )


@app.get("/api/a2a/card")
async def get_a2a_card() -> dict[str, Any]:
    """A2A AgentCard 相当の情報を取得."""
    agent = _get_faq_agent()
    card = agent.get_a2a_card()
    if card is not None and hasattr(card, "to_a2a_format"):
        return card.to_a2a_format()

    return {
        "name": "faq-system-maq-router",
        "description": "社内FAQ/SQL分析/営業資料画像生成を振り分けるマルチ機能Agent",
        "version": "1.1.0",
        "skills": [
            {"name": "knowledge_search", "description": "社内知識検索と回答生成"},
            {"name": "sql_analytics", "description": "自然言語からSQL生成し、表とチャートを返却"},
            {"name": "design_skills", "description": "営業資料向け画像セットを生成"},
        ],
    }


@app.get("/api/health")
async def health_check() -> dict[str, str]:
    """ヘルスチェック."""
    return {"status": "ok", "service": "faq-system-demo"}


# =============================================================================
# エントリポイント
# =============================================================================


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8001)
