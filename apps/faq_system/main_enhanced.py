"""FAQ System Enhanced - 強化版FAQシステム.

改善点：
1. 富文本レスポンス（Markdown、コード、表格、チャート）
2. リアルタイム進捗表示
3. 引用/ソース表示
4. WebSocket 双方向通信
5. 知識ベースギャップ分析

使用例:
    uvicorn apps.faq_system.main_enhanced:app --reload --port 8002
"""

from __future__ import annotations

import json
import logging
from datetime import datetime
from typing import Any

# Knowledge Discovery Skill
from agentflow.skills.builtin.knowledge_discovery.manager import KnowledgeDiscoveryManager
from fastapi import FastAPI, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse, StreamingResponse
from pydantic import BaseModel, Field

from agentflow.integrations import RealtimeStateSync

# フレームワーク層
from agentflow.state import GlobalStateStore


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# アプリケーション設定
# =============================================================================


app = FastAPI(
    title="FAQ System Enhanced",
    description="強化版FAQシステム - 富文本 + リアルタイム + 引用",
    version="2.0.0",
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# =============================================================================
# グローバル状態管理
# =============================================================================


_state_store = GlobalStateStore()
_realtime_sync = RealtimeStateSync(_state_store)
_knowledge_manager: KnowledgeDiscoveryManager | None = None

def _get_knowledge_manager() -> KnowledgeDiscoveryManager:
    """KnowledgeDiscoveryManager取得."""
    global _knowledge_manager
    if _knowledge_manager is None:
        _knowledge_manager = KnowledgeDiscoveryManager()
    return _knowledge_manager


# =============================================================================
# リクエスト/レスポンスモデル
# =============================================================================


class ChatRequest(BaseModel):
    """チャットリクエスト."""

    message: str = Field(..., description="ユーザーメッセージ")
    session_id: str | None = Field(None, description="セッションID")
    options: dict[str, Any] = Field(default_factory=dict, description="オプション")


class FeedbackRequest(BaseModel):
    """フィードバックリクエスト."""

    message_id: str = Field(..., description="メッセージID")
    helpful: bool = Field(..., description="役に立ったか")
    comment: str | None = Field(None, description="コメント")


# =============================================================================
# WebSocket 接続管理
# =============================================================================


class ConnectionManager:
    """WebSocket接続マネージャ."""

    def __init__(self) -> None:
        """初期化."""
        self.active_connections: dict[str, WebSocket] = {}

    async def connect(self, websocket: WebSocket, client_id: str) -> None:
        """接続."""
        await websocket.accept()
        self.active_connections[client_id] = websocket
        logger.info("WebSocket connected: %s", client_id)

    def disconnect(self, client_id: str) -> None:
        """切断."""
        if client_id in self.active_connections:
            del self.active_connections[client_id]
            logger.info("WebSocket disconnected: %s", client_id)

    async def send_message(self, client_id: str, message: dict[str, Any]) -> None:
        """メッセージ送信."""
        if client_id in self.active_connections:
            await self.active_connections[client_id].send_json(message)

    async def broadcast(self, message: dict[str, Any]) -> None:
        """ブロードキャスト."""
        for ws in self.active_connections.values():
            await ws.send_json(message)


manager = ConnectionManager()


# =============================================================================
# API エンドポイント
# =============================================================================


@app.get("/", response_class=HTMLResponse)
async def index() -> str:
    """トップページ - 強化版UI."""
    return """
<!DOCTYPE html>
<html lang="ja">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>FAQ System Enhanced</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
    <style>
        .chat-message { animation: fadeIn 0.3s ease-in; }
        @keyframes fadeIn { from { opacity: 0; transform: translateY(10px); } to { opacity: 1; transform: translateY(0); } }
        .typing-indicator span { animation: bounce 1.4s infinite ease-in-out both; }
        .typing-indicator span:nth-child(1) { animation-delay: -0.32s; }
        .typing-indicator span:nth-child(2) { animation-delay: -0.16s; }
        @keyframes bounce { 0%, 80%, 100% { transform: scale(0); } 40% { transform: scale(1); } }
        pre code { border-radius: 8px; }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #e5e7eb; padding: 8px 12px; text-align: left; }
        th { background: #f9fafb; font-weight: 600; }
        .citation { background: #f3f4f6; border-left: 4px solid #3b82f6; padding: 8px 12px; margin: 8px 0; border-radius: 4px; }
    </style>
</head>
<body class="bg-gray-100 h-screen flex flex-col">
    <!-- Header -->
    <header class="bg-white shadow-sm border-b px-6 py-4">
        <div class="flex items-center justify-between max-w-4xl mx-auto">
            <h1 class="text-xl font-bold text-gray-800">🤖 FAQ System Enhanced</h1>
            <span id="status" class="text-sm text-green-600">● 接続中</span>
        </div>
    </header>

    <!-- Chat Container -->
    <main class="flex-1 overflow-hidden max-w-4xl mx-auto w-full">
        <div id="chat-messages" class="h-full overflow-y-auto p-4 space-y-4"></div>
    </main>

    <!-- Progress Bar -->
    <div id="progress-container" class="hidden bg-white border-t px-6 py-2">
        <div class="max-w-4xl mx-auto">
            <div class="flex items-center gap-3">
                <div class="flex-1 bg-gray-200 rounded-full h-2">
                    <div id="progress-bar" class="bg-blue-600 h-2 rounded-full transition-all duration-300" style="width: 0%"></div>
                </div>
                <span id="progress-text" class="text-sm text-gray-600">処理中...</span>
            </div>
        </div>
    </div>

    <!-- Input -->
    <footer class="bg-white border-t px-6 py-4">
        <form id="chat-form" class="max-w-4xl mx-auto flex gap-3">
            <input type="text" id="message-input" placeholder="質問を入力..."
                class="flex-1 px-4 py-3 border rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500">
            <button type="submit" id="send-btn"
                class="px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed">
                送信
            </button>
        </form>
    </footer>

    <script>
        const chatMessages = document.getElementById('chat-messages');
        const chatForm = document.getElementById('chat-form');
        const messageInput = document.getElementById('message-input');
        const sendBtn = document.getElementById('send-btn');
        const progressContainer = document.getElementById('progress-container');
        const progressBar = document.getElementById('progress-bar');
        const progressText = document.getElementById('progress-text');
        const statusEl = document.getElementById('status');

        let ws = null;
        let sessionId = 'session-' + Date.now();

        // WebSocket 接続
        function connectWebSocket() {
            ws = new WebSocket(`ws://${window.location.host}/ws/${sessionId}`);
            ws.onopen = () => {
                statusEl.textContent = '● 接続中';
                statusEl.className = 'text-sm text-green-600';
            };
            ws.onclose = () => {
                statusEl.textContent = '○ 切断';
                statusEl.className = 'text-sm text-red-600';
                setTimeout(connectWebSocket, 3000);
            };
            ws.onmessage = (event) => handleMessage(JSON.parse(event.data));
        }
        connectWebSocket();

        // メッセージ処理
        function handleMessage(msg) {
            if (msg.type === 'progress') {
                showProgress(msg.progress, msg.message);
            } else if (msg.type === 'result') {
                hideProgress();
                addAssistantMessage(msg.data);
            } else if (msg.type === 'error') {
                hideProgress();
                addErrorMessage(msg.message);
            }
        }

        // 進捗表示
        function showProgress(value, text) {
            progressContainer.classList.remove('hidden');
            progressBar.style.width = value + '%';
            progressText.textContent = text;
        }

        function hideProgress() {
            progressContainer.classList.add('hidden');
        }

        // メッセージ追加
        function addUserMessage(text) {
            const div = document.createElement('div');
            div.className = 'chat-message flex justify-end';
            div.innerHTML = `
                <div class="max-w-2xl bg-blue-600 text-white rounded-lg px-4 py-3">
                    <p>${escapeHtml(text)}</p>
                </div>
            `;
            chatMessages.appendChild(div);
            scrollToBottom();
        }

        function addAssistantMessage(data) {
            const div = document.createElement('div');
            div.className = 'chat-message flex justify-start';

            let content = '';

            // 富文本レスポンス
            if (data.rich_response && data.rich_response.components) {
                content = renderRichComponents(data.rich_response.components);
            } else {
                content = `<div class="prose">${marked.parse(data.answer || '')}</div>`;
            }

            // 引用
            if (data.citations && data.citations.length > 0) {
                content += '<div class="mt-4"><h4 class="text-sm font-semibold text-gray-600 mb-2">📚 参照ソース</h4>';
                data.citations.forEach((c, i) => {
                    content += `
                        <div class="citation">
                            <strong>[${i+1}] ${escapeHtml(c.title || 'ソース')}</strong>
                            <p class="text-sm text-gray-600 mt-1">${escapeHtml(c.snippet || '')}</p>
                        </div>
                    `;
                });
                content += '</div>';
            }

            // 提案
            if (data.suggestions && data.suggestions.length > 0) {
                content += '<div class="mt-4 flex flex-wrap gap-2">';
                data.suggestions.forEach(s => {
                    content += `
                        <button onclick="fillMessage('${escapeHtml(s.text)}')"
                            class="px-3 py-1 bg-gray-100 hover:bg-gray-200 rounded-full text-sm text-gray-700">
                            ${escapeHtml(s.text)}
                        </button>
                    `;
                });
                content += '</div>';
            }

            div.innerHTML = `
                <div class="max-w-3xl bg-white shadow rounded-lg px-4 py-3">
                    ${content}
                </div>
            `;

            chatMessages.appendChild(div);

            // チャート初期化
            if (data.chart) {
                setTimeout(() => initCharts(), 100);
            }

            // コードハイライト
            div.querySelectorAll('pre code').forEach(el => hljs.highlightElement(el));

            scrollToBottom();
        }

        function addErrorMessage(text) {
            const div = document.createElement('div');
            div.className = 'chat-message flex justify-start';
            div.innerHTML = `
                <div class="max-w-2xl bg-red-50 border border-red-200 text-red-700 rounded-lg px-4 py-3">
                    <p>❌ ${escapeHtml(text)}</p>
                </div>
            `;
            chatMessages.appendChild(div);
            scrollToBottom();
        }

        // 富文本コンポーネント描画
        function renderRichComponents(components) {
            return components.map(c => {
                switch (c.type) {
                    case 'markdown':
                        return `<div class="prose max-w-none">${marked.parse(c.props.content || '')}</div>`;
                    case 'code_block':
                        return `
                            <div class="my-3">
                                ${c.props.title ? `<div class="text-sm text-gray-500 mb-1">${escapeHtml(c.props.title)}</div>` : ''}
                                <pre><code class="language-${c.props.language || 'text'}">${escapeHtml(c.props.code || '')}</code></pre>
                            </div>
                        `;
                    case 'data_table':
                        return renderTable(c.props);
                    case 'chart':
                        return `<div id="chart-${Date.now()}" class="w-full h-64 my-3" data-chart='${JSON.stringify(c.props.data)}'></div>`;
                    case 'citation':
                        return `
                            <div class="citation">
                                <strong>${escapeHtml(c.props.title || '')}</strong>
                                <p class="text-sm text-gray-600">${escapeHtml(c.props.snippet || '')}</p>
                            </div>
                        `;
                    case 'alert':
                        const alertColors = {
                            info: 'bg-blue-50 border-blue-200 text-blue-700',
                            success: 'bg-green-50 border-green-200 text-green-700',
                            warning: 'bg-yellow-50 border-yellow-200 text-yellow-700',
                            error: 'bg-red-50 border-red-200 text-red-700',
                        };
                        return `
                            <div class="my-2 p-3 rounded border ${alertColors[c.props.alertType] || alertColors.info}">
                                ${c.props.title ? `<strong>${escapeHtml(c.props.title)}</strong><br>` : ''}
                                ${escapeHtml(c.props.message || '')}
                            </div>
                        `;
                    default:
                        return '';
                }
            }).join('');
        }

        function renderTable(props) {
            if (!props.data || !props.data.length) return '';
            const columns = props.columns || Object.keys(props.data[0]).map(k => ({key: k, label: k}));
            let html = '<div class="overflow-x-auto my-3">';
            if (props.title) html += `<div class="text-sm font-semibold text-gray-700 mb-2">${escapeHtml(props.title)}</div>`;
            html += '<table class="min-w-full"><thead><tr>';
            columns.forEach(col => { html += `<th>${escapeHtml(col.label || col.key)}</th>`; });
            html += '</tr></thead><tbody>';
            props.data.slice(0, props.pageSize || 10).forEach(row => {
                html += '<tr>';
                columns.forEach(col => { html += `<td>${escapeHtml(String(row[col.key] ?? ''))}</td>`; });
                html += '</tr>';
            });
            html += '</tbody></table></div>';
            return html;
        }

        function initCharts() {
            document.querySelectorAll('[data-chart]').forEach(el => {
                if (!el.dataset.chartInit) {
                    const chart = echarts.init(el);
                    chart.setOption(JSON.parse(el.dataset.chart));
                    el.dataset.chartInit = 'true';
                }
            });
        }

        // フォーム送信
        chatForm.addEventListener('submit', async (e) => {
            e.preventDefault();
            const message = messageInput.value.trim();
            if (!message) return;

            addUserMessage(message);
            messageInput.value = '';
            sendBtn.disabled = true;
            showProgress(0, '処理を開始...');

            try {
                if (ws && ws.readyState === WebSocket.OPEN) {
                    ws.send(JSON.stringify({ type: 'chat', message, sessionId }));
                } else {
                    // フォールバック: REST API
                    const response = await fetch('/api/v2/chat/stream', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ message, session_id: sessionId }),
                    });

                    const reader = response.body.getReader();
                    const decoder = new TextDecoder();

                    while (true) {
                        const { done, value } = await reader.read();
                        if (done) break;
                        const lines = decoder.decode(value).split('\\n');
                        for (const line of lines) {
                            if (line.startsWith('data: ')) {
                                handleMessage(JSON.parse(line.slice(6)));
                            }
                        }
                    }
                }
            } catch (error) {
                hideProgress();
                addErrorMessage(error.message);
            } finally {
                sendBtn.disabled = false;
            }
        });

        function fillMessage(text) {
            messageInput.value = text;
            messageInput.focus();
        }

        function escapeHtml(text) {
            const div = document.createElement('div');
            div.textContent = text;
            return div.innerHTML;
        }

        function scrollToBottom() {
            chatMessages.scrollTop = chatMessages.scrollHeight;
        }
    </script>
</body>
</html>
"""


@app.post("/api/v2/chat")
async def chat_v2(request: ChatRequest) -> dict[str, Any]:
    """チャット API v2（同期）."""
    manager = _get_knowledge_manager()
    return await manager.discover(request.message, {"session_id": request.session_id})


@app.post("/api/v2/chat/stream")
async def chat_stream_v2(request: ChatRequest) -> StreamingResponse:
    """チャット API v2（ストリーム）."""
    agent = _get_faq_agent()

    async def event_generator():
        async for event in agent.run_stream({
            "question": request.message,
            "session_id": request.session_id,
        }):
            yield f"data: {json.dumps(event, ensure_ascii=False, default=str)}\n\n"

    return StreamingResponse(
        event_generator(),
        media_type="text/event-stream",
    )


@app.websocket("/ws/{client_id}")
async def websocket_endpoint(websocket: WebSocket, client_id: str) -> None:
    """WebSocket エンドポイント."""
    await manager.connect(websocket, client_id)

    try:
        while True:
            data = await websocket.receive_json()

            if data.get("type") == "chat":
                message = data.get("message", "")
                session_id = data.get("sessionId", client_id)

                agent = _get_faq_agent()

                async for event in agent.run_stream({
                    "question": message,
                    "session_id": session_id,
                }):
                    await manager.send_message(client_id, event)

    except WebSocketDisconnect:
        manager.disconnect(client_id)
    except Exception as e:
        logger.exception("WebSocket error: %s", e)
        await manager.send_message(client_id, {
            "type": "error",
            "message": str(e),
        })


@app.post("/api/v2/feedback")
async def submit_feedback(request: FeedbackRequest) -> dict[str, str]:
    """フィードバック送信."""
    logger.info(
        "Feedback: message_id=%s, helpful=%s, comment=%s",
        request.message_id,
        request.helpful,
        request.comment,
    )
    # 実際はDBに保存
    return {"status": "ok", "message": "フィードバックを受け付けました"}


@app.get("/api/v2/health")
async def health_check() -> dict[str, Any]:
    """ヘルスチェック."""
    return {
        "status": "ok",
        "service": "faq-system-enhanced",
        "version": "2.0.0",
        "timestamp": datetime.now().isoformat(),
    }


# =============================================================================
# エントリポイント
# =============================================================================


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8002)
