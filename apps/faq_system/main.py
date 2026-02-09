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
from typing import Any

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse, StreamingResponse
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
    return await agent.run({"question": request.message})


@app.post("/api/chat/stream")
async def chat_stream(request: ChatRequest) -> StreamingResponse:
    """チャット API（ストリーム版）.

    SSE でリアルタイムに進捗を返します。
    """
    agent = _get_faq_agent()

    async def event_generator():
        async for event in agent.run_stream({"question": request.message}):
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
