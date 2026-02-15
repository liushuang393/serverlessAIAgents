"""FAQ System - AgentFlow フレームワーク級サービス.

薄い App 層として設計。業務ロジックは agentflow/ フレームワーク側で実装。

アーキテクチャ:
    App層（薄い）          → フレームワーク層（厚い）
    ─────────────────────────────────────────────
    /api/chat              → FAQAgent
    /api/rag/*             → RAGService
    /api/sql/*             → Text2SQLService
    /ws/{client_id}        → WebSocket リアルタイム通信

機能:
- RAG 検索（ナレッジベース）
- Text2SQL（データベースクエリ）
- チャート生成
- フォローアップ提案
- WebSocket 双方向通信
- 富文本レスポンス（Markdown・コード・表・チャート）
- リアルタイム進捗表示
- 引用/ソース表示
- フィードバック収集

使用例:
    uvicorn apps.faq_system.main:app --reload --port 8001
"""

from __future__ import annotations

import json
import logging
import os
from contextlib import asynccontextmanager
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any
from uuid import uuid4


if TYPE_CHECKING:
    from collections.abc import AsyncIterator

# 認証モジュール
from apps.faq_system.backend.auth import auth_router
from apps.faq_system.backend.auth.dependencies import require_auth, require_role, resolve_user
from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.backend.auth.service import get_auth_service
from apps.faq_system.backend.config import (
    KnowledgeBaseType,
    KnowledgeBaseUpdateRequest,
    kb_registry,
)
from apps.faq_system.backend.db import close_db, ensure_database_ready
from apps.faq_system.backend.db.session import create_all_tables, get_database_url
from apps.faq_system.backend.services.chat_history_service import ChatHistoryService
from fastapi import Depends, FastAPI, HTTPException, WebSocket, WebSocketDisconnect
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
from agentflow.integrations import RealtimeStateSync
from agentflow.services import (
    RAGConfig,
    RAGService,
    SuggestionConfig,
    SuggestionService,
    Text2SQLConfig,
    Text2SQLService,
)
from agentflow.state import GlobalStateStore


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# アプリケーション設定
# =============================================================================


@asynccontextmanager
async def lifespan(_app: FastAPI) -> AsyncIterator[None]:
    """アプリ起動/終了時の初期化."""
    await ensure_database_ready()
    if get_database_url().startswith("sqlite"):
        await create_all_tables()
    await kb_registry.ensure_initialized()
    await get_auth_service().ensure_bootstrap_data()
    yield
    await close_db()


app = FastAPI(
    title="FAQ System",
    description="AgentFlow フレームワーク級サービス - WebSocket + 富文本対応",
    version="2.0.0",
    lifespan=lifespan,
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# 認証ルーター登録
app.include_router(auth_router)


# =============================================================================
# グローバル状態管理
# =============================================================================


_state_store = GlobalStateStore()
_realtime_sync = RealtimeStateSync(_state_store)


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


class RAGQueryRequest(BaseModel):
    """RAGクエリリクエスト."""

    question: str = Field(..., description="質問")
    kb_type: KnowledgeBaseType = Field(
        default=KnowledgeBaseType.INTERNAL,
        description="KB種別 (internal / external / confidential)",
    )
    collection: str | None = Field(None, description="明示コレクション名（指定時は優先）")
    top_k: int = Field(5, description="取得件数")


class SQLQueryRequest(BaseModel):
    """SQLクエリリクエスト."""

    question: str = Field(..., description="質問")


class AddDocumentRequest(BaseModel):
    """ドキュメント追加リクエスト."""

    kb_type: KnowledgeBaseType = Field(
        default=KnowledgeBaseType.INTERNAL,
        description="KB種別 (internal / external / confidential)",
    )
    content: str = Field(..., description="ドキュメント内容")
    collection: str | None = Field(None, description="明示コレクション名（指定時は優先）")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


# =============================================================================
# サービスインスタンス（遅延初期化・私有）
# ⚠️ 注意: これらは内部実装です。外部から直接呼び出さないでください。
# =============================================================================


_services: dict[str, Any] = {}
_artifact_registry: dict[str, Path] = {}
_chat_history_service = ChatHistoryService()


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


_ws_manager = ConnectionManager()


def _resolve_default_collection() -> str:
    """既定コレクション名を解決."""
    env_collection = os.getenv("RAG_COLLECTION")
    if env_collection:
        return env_collection
    return kb_registry.resolve_collection()


def _resolve_session_id(session_id: str | None) -> str:
    """セッションIDを正規化."""
    if session_id and session_id.strip():
        return session_id.strip()
    return f"session-{uuid4().hex}"


def _extract_assistant_content(payload: dict[str, Any]) -> str:
    """保存用に回答本文を抽出."""
    answer = payload.get("answer")
    if isinstance(answer, str) and answer.strip():
        return answer
    try:
        return json.dumps(payload, ensure_ascii=False)
    except TypeError:
        return str(payload)


def _get_rag_service(collection: str | None = None) -> RAGService:
    """RAGサービス取得（私有）."""
    resolved_collection = collection or _resolve_default_collection()
    service_key = f"rag:{resolved_collection}"
    if service_key not in _services:
        _services[service_key] = RAGService(
            RAGConfig(
                collection=resolved_collection,
                chunk_strategy="semantic",
                reranker="bm25",
            )
        )
    return _services[service_key]


def _get_sql_service() -> Text2SQLService:
    """SQLサービス取得（私有）."""
    if "sql" not in _services:
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["sql"] = Text2SQLService(
            Text2SQLConfig(
                schema=schema,
                auto_chart=True,
            )
        )
    return _services["sql"]


def _get_suggestion_service() -> SuggestionService:
    """提案サービス取得（私有）."""
    if "suggestion" not in _services:
        _services["suggestion"] = SuggestionService(
            SuggestionConfig(
                max_suggestions=5,
                language="ja",
            )
        )
    return _services["suggestion"]


def _get_faq_agent() -> FAQAgent:
    """FAQAgent取得（私有）."""
    if "faq_agent" not in _services:
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["faq_agent"] = FAQAgent(
            FAQAgentConfig(
                rag_collection=_resolve_default_collection(),
                sql_schema=schema,
            )
        )
    return _services["faq_agent"]


def _get_sales_agent() -> SalesAgent:
    """SalesAgent取得（私有）."""
    if "sales_agent" not in _services:
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["sales_agent"] = SalesAgent(
            SalesAgentConfig(
                sql_schema=schema,
            )
        )
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
# 強化版 HTML UI（WebSocket + Markdown + ECharts + Highlight.js）
# =============================================================================

_ENHANCED_HTML = """
<!DOCTYPE html>
<html lang="ja">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>FAQ System</title>
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
        .login-overlay { position: fixed; inset: 0; background: rgba(0,0,0,0.5); display: flex; align-items: center; justify-content: center; z-index: 1000; }
        .login-box { background: white; border-radius: 12px; padding: 2rem; width: 400px; max-width: 90vw; box-shadow: 0 20px 60px rgba(0,0,0,0.3); }
        .login-box input { width: 100%; padding: 10px 14px; border: 1px solid #d1d5db; border-radius: 8px; margin-bottom: 12px; font-size: 14px; }
        .login-box input:focus { outline: none; border-color: #3b82f6; box-shadow: 0 0 0 3px rgba(59,130,246,0.1); }
        .login-btn { width: 100%; padding: 12px; background: #2563eb; color: white; border: none; border-radius: 8px; font-size: 15px; font-weight: 600; cursor: pointer; }
        .login-btn:hover { background: #1d4ed8; }
        .login-btn:disabled { background: #93c5fd; cursor: not-allowed; }
        .login-error { color: #dc2626; font-size: 13px; margin-bottom: 12px; display: none; }
    </style>
</head>
<body class="bg-gray-100 h-screen flex flex-col">
    <!-- ログインオーバーレイ -->
    <div id="login-overlay" class="login-overlay">
        <div class="login-box">
            <h2 class="text-xl font-bold text-gray-800 mb-1 text-center">🔐 FAQ System</h2>
            <p class="text-sm text-gray-500 mb-6 text-center">ログインしてください</p>
            <div id="login-error" class="login-error"></div>
            <form id="login-form">
                <input type="text" id="login-username" placeholder="ユーザー名" autocomplete="username" required>
                <input type="password" id="login-password" placeholder="パスワード" autocomplete="current-password" required>
                <button type="submit" id="login-btn" class="login-btn">ログイン</button>
            </form>
            <div class="mt-4 text-xs text-gray-400 text-center">
                デモアカウント: admin / admin123
            </div>
        </div>
    </div>
    <header class="bg-white shadow-sm border-b px-6 py-4">
        <div class="flex items-center justify-between max-w-4xl mx-auto">
            <h1 class="text-xl font-bold text-gray-800">🤖 FAQ System</h1>
            <div class="flex items-center gap-4">
                <span id="user-info" class="text-sm text-gray-600 hidden"></span>
                <button id="logout-btn" class="text-sm text-red-500 hover:text-red-700 hidden" onclick="doLogout()">ログアウト</button>
                <span id="status" class="text-sm text-green-600">● 接続中</span>
            </div>
        </div>
    </header>
    <main class="flex-1 overflow-hidden max-w-4xl mx-auto w-full">
        <div id="chat-messages" class="h-full overflow-y-auto p-4 space-y-4"></div>
    </main>
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
        const loginOverlay = document.getElementById('login-overlay');
        const loginForm = document.getElementById('login-form');
        const loginError = document.getElementById('login-error');
        const userInfoEl = document.getElementById('user-info');
        const logoutBtn = document.getElementById('logout-btn');
        let ws = null;
        let sessionId = 'session-' + Date.now();
        let accessToken = localStorage.getItem('faq_access_token');

        /* --- 認証関連 --- */
        function showLogin() { loginOverlay.style.display = 'flex'; }
        function hideLogin() { loginOverlay.style.display = 'none'; }
        function showUserInfo(user) {
            userInfoEl.textContent = '👤 ' + (user.display_name || user.username) + ' (' + user.role + ')';
            userInfoEl.classList.remove('hidden'); logoutBtn.classList.remove('hidden');
        }
        function authHeaders() {
            const h = { 'Content-Type': 'application/json' };
            if (accessToken) h['Authorization'] = 'Bearer ' + accessToken;
            return h;
        }
        loginForm.addEventListener('submit', async (e) => {
            e.preventDefault();
            const btn = document.getElementById('login-btn');
            btn.disabled = true; loginError.style.display = 'none';
            try {
                const res = await fetch('/api/auth/login', {
                    method: 'POST', headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ username: document.getElementById('login-username').value, password: document.getElementById('login-password').value })
                });
                const data = await res.json();
                if (data.success) {
                    accessToken = data.access_token;
                    localStorage.setItem('faq_access_token', accessToken);
                    showUserInfo(data.user); hideLogin(); connectWebSocket();
                } else {
                    loginError.textContent = data.message; loginError.style.display = 'block';
                }
            } catch (err) { loginError.textContent = '接続エラー'; loginError.style.display = 'block'; }
            finally { btn.disabled = false; }
        });
        async function doLogout() {
            try { await fetch('/api/auth/logout', { method: 'POST', headers: authHeaders() }); } catch (_) {}
            accessToken = null; localStorage.removeItem('faq_access_token');
            userInfoEl.classList.add('hidden'); logoutBtn.classList.add('hidden');
            if (ws) { ws.close(); ws = null; }
            showLogin();
        }
        async function checkAuth() {
            if (!accessToken) { showLogin(); return; }
            try {
                const res = await fetch('/api/auth/me', { headers: authHeaders() });
                const data = await res.json();
                if (data.success && data.user) { showUserInfo(data.user); hideLogin(); connectWebSocket(); }
                else { accessToken = null; localStorage.removeItem('faq_access_token'); showLogin(); }
            } catch (_) { showLogin(); }
        }
        checkAuth();

        /* --- WebSocket --- */
        function connectWebSocket() {
            if (ws && ws.readyState === WebSocket.OPEN) return;
            if (!accessToken) return;
            const wsProtocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
            ws = new WebSocket(`${wsProtocol}://${window.location.host}/ws/${sessionId}?access_token=${encodeURIComponent(accessToken)}`);
            ws.onopen = () => { statusEl.textContent = '● 接続中'; statusEl.className = 'text-sm text-green-600'; };
            ws.onclose = () => { statusEl.textContent = '○ 切断'; statusEl.className = 'text-sm text-red-600'; setTimeout(() => { if (accessToken) connectWebSocket(); }, 3000); };
            ws.onmessage = (event) => handleMessage(JSON.parse(event.data));
        }
        function handleMessage(msg) {
            if (msg.type === 'progress') { showProgress(msg.progress, msg.message); }
            else if (msg.type === 'result') { hideProgress(); addAssistantMessage(msg.data); }
            else if (msg.type === 'error') { hideProgress(); addErrorMessage(msg.message); }
        }
        function showProgress(value, text) { progressContainer.classList.remove('hidden'); progressBar.style.width = value + '%'; progressText.textContent = text; }
        function hideProgress() { progressContainer.classList.add('hidden'); }
        function addUserMessage(text) {
            const div = document.createElement('div');
            div.className = 'chat-message flex justify-end';
            div.innerHTML = `<div class="max-w-2xl bg-blue-600 text-white rounded-lg px-4 py-3"><p>${escapeHtml(text)}</p></div>`;
            chatMessages.appendChild(div); scrollToBottom();
        }
        function addAssistantMessage(data) {
            const div = document.createElement('div');
            div.className = 'chat-message flex justify-start';
            let content = '';
            if (data.rich_response && data.rich_response.components) { content = renderRichComponents(data.rich_response.components); }
            else { content = `<div class="prose">${marked.parse(data.answer || '')}</div>`; }
            if (data.citations && data.citations.length > 0) {
                content += '<div class="mt-4"><h4 class="text-sm font-semibold text-gray-600 mb-2">📚 参照ソース</h4>';
                data.citations.forEach((c, i) => { content += `<div class="citation"><strong>[${i+1}] ${escapeHtml(c.title || 'ソース')}</strong><p class="text-sm text-gray-600 mt-1">${escapeHtml(c.snippet || '')}</p></div>`; });
                content += '</div>';
            }
            if (data.suggestions && data.suggestions.length > 0) {
                content += '<div class="mt-4 flex flex-wrap gap-2">';
                data.suggestions.forEach(s => { content += `<button onclick="fillMessage('${escapeHtml(s.text)}')" class="px-3 py-1 bg-gray-100 hover:bg-gray-200 rounded-full text-sm text-gray-700">${escapeHtml(s.text)}</button>`; });
                content += '</div>';
            }
            div.innerHTML = `<div class="max-w-3xl bg-white shadow rounded-lg px-4 py-3">${content}</div>`;
            chatMessages.appendChild(div);
            if (data.chart) { setTimeout(() => initCharts(), 100); }
            div.querySelectorAll('pre code').forEach(el => hljs.highlightElement(el));
            scrollToBottom();
        }
        function addErrorMessage(text) {
            const div = document.createElement('div');
            div.className = 'chat-message flex justify-start';
            div.innerHTML = `<div class="max-w-2xl bg-red-50 border border-red-200 text-red-700 rounded-lg px-4 py-3"><p>❌ ${escapeHtml(text)}</p></div>`;
            chatMessages.appendChild(div); scrollToBottom();
        }
        function renderRichComponents(components) {
            return components.map(c => {
                switch (c.type) {
                    case 'markdown': return `<div class="prose max-w-none">${marked.parse(c.props.content || '')}</div>`;
                    case 'code_block': return `<div class="my-3">${c.props.title ? `<div class="text-sm text-gray-500 mb-1">${escapeHtml(c.props.title)}</div>` : ''}<pre><code class="language-${c.props.language || 'text'}">${escapeHtml(c.props.code || '')}</code></pre></div>`;
                    case 'data_table': return renderTable(c.props);
                    case 'chart': return `<div id="chart-${Date.now()}" class="w-full h-64 my-3" data-chart='${JSON.stringify(c.props.data)}'></div>`;
                    case 'citation': return `<div class="citation"><strong>${escapeHtml(c.props.title || '')}</strong><p class="text-sm text-gray-600">${escapeHtml(c.props.snippet || '')}</p></div>`;
                    case 'alert':
                        const ac = { info: 'bg-blue-50 border-blue-200 text-blue-700', success: 'bg-green-50 border-green-200 text-green-700', warning: 'bg-yellow-50 border-yellow-200 text-yellow-700', error: 'bg-red-50 border-red-200 text-red-700' };
                        return `<div class="my-2 p-3 rounded border ${ac[c.props.alertType] || ac.info}">${c.props.title ? `<strong>${escapeHtml(c.props.title)}</strong><br>` : ''}${escapeHtml(c.props.message || '')}</div>`;
                    default: return '';
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
            props.data.slice(0, props.pageSize || 10).forEach(row => { html += '<tr>'; columns.forEach(col => { html += `<td>${escapeHtml(String(row[col.key] ?? ''))}</td>`; }); html += '</tr>'; });
            html += '</tbody></table></div>';
            return html;
        }
        function initCharts() {
            document.querySelectorAll('[data-chart]').forEach(el => { if (!el.dataset.chartInit) { const chart = echarts.init(el); chart.setOption(JSON.parse(el.dataset.chart)); el.dataset.chartInit = 'true'; } });
        }
        chatForm.addEventListener('submit', async (e) => {
            e.preventDefault();
            if (!accessToken) { showLogin(); return; }
            const message = messageInput.value.trim();
            if (!message) return;
            addUserMessage(message); messageInput.value = ''; sendBtn.disabled = true; showProgress(0, '処理を開始...');
            try {
                if (ws && ws.readyState === WebSocket.OPEN) {
                    ws.send(JSON.stringify({ type: 'chat', message, sessionId }));
                } else {
                    const response = await fetch('/api/chat/stream', { method: 'POST', headers: authHeaders(), body: JSON.stringify({ message, session_id: sessionId }) });
                    if (response.status === 401) { doLogout(); return; }
                    const reader = response.body.getReader();
                    const decoder = new TextDecoder();
                    while (true) {
                        const { done, value } = await reader.read();
                        if (done) break;
                        const lines = decoder.decode(value).split('\\n');
                        for (const line of lines) { if (line.startsWith('data: ')) { handleMessage(JSON.parse(line.slice(6))); } }
                    }
                }
            } catch (error) { hideProgress(); addErrorMessage(error.message); }
            finally { sendBtn.disabled = false; }
        });
        function fillMessage(text) { messageInput.value = text; messageInput.focus(); }
        function escapeHtml(text) { const div = document.createElement('div'); div.textContent = text; return div.innerHTML; }
        function scrollToBottom() { chatMessages.scrollTop = chatMessages.scrollHeight; }
    </script>
</body>
</html>
"""


# =============================================================================
# API エンドポイント
# =============================================================================


@app.get("/", response_class=HTMLResponse)
async def index() -> str:
    """トップページ - WebSocket + 富文本 UI."""
    return _ENHANCED_HTML


@app.post("/api/chat")
async def chat(
    request: ChatRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """チャット API.

    FAQAgent を呼び出し、質問に回答します。
    FAQAgent が内部でクエリタイプを判定し、適切なサービスを使用します。
    認証必須。
    """
    session_id = _resolve_session_id(request.session_id)
    await _chat_history_service.save_message(
        session_id=session_id,
        role="user",
        content=request.message,
        transport="api",
        user=user,
        metadata={"options": request.options},
    )

    agent = _get_faq_agent()
    result = await agent.run(
        {
            "question": request.message,
            "context": {
                "user": {
                    "user_id": user.user_id,
                    "username": user.username,
                    "role": user.role,
                    "department": user.department,
                },
                "session_id": session_id,
                "options": request.options,
            },
        }
    )

    result = _register_artifacts(result)
    result["session_id"] = session_id

    await _chat_history_service.save_message(
        session_id=session_id,
        role="assistant",
        content=_extract_assistant_content(result),
        transport="api",
        user=user,
        metadata={"query_type": result.get("query_type")},
    )

    return result


@app.post("/api/maq/chat")
async def maq_chat(
    request: ChatRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """MAQ統合チャット API.

    社内FAQ・SQL分析・営業資料画像生成を単一入口で処理する。
    認証必須。
    """
    return await chat(request, user)


@app.post("/api/chat/stream")
async def chat_stream(
    request: ChatRequest,
    user: UserInfo = Depends(require_auth),
) -> StreamingResponse:
    """チャット API (ストリーム版).

    SSE でリアルタイムに進捗を返します。認証必須。
    """
    session_id = _resolve_session_id(request.session_id)
    await _chat_history_service.save_message(
        session_id=session_id,
        role="user",
        content=request.message,
        transport="sse",
        user=user,
        metadata={"options": request.options},
    )
    agent = _get_faq_agent()

    async def event_generator() -> AsyncIterator[str]:
        try:
            async for event in agent.run_stream(
                {
                    "question": request.message,
                    "context": {
                        "user": {
                            "user_id": user.user_id,
                            "username": user.username,
                            "role": user.role,
                            "department": user.department,
                        },
                        "session_id": session_id,
                        "options": request.options,
                    },
                }
            ):
                if event.get("type") == "result" and isinstance(event.get("data"), dict):
                    event["data"] = _register_artifacts(event["data"])
                    event["data"]["session_id"] = session_id
                    await _chat_history_service.save_message(
                        session_id=session_id,
                        role="assistant",
                        content=_extract_assistant_content(event["data"]),
                        transport="sse",
                        user=user,
                        metadata={"query_type": event["data"].get("query_type")},
                    )
                yield f"data: {json.dumps(event)}\n\n"
        except Exception as exc:
            logger.exception("SSE chat stream failed: %s", exc)
            await _chat_history_service.save_message(
                session_id=session_id,
                role="system",
                content=str(exc),
                transport="sse",
                user=user,
                metadata={"error": True},
            )
            yield f"data: {json.dumps({'type': 'error', 'message': str(exc)})}\n\n"

    return StreamingResponse(event_generator(), media_type="text/event-stream")


@app.get("/api/chat/history")
async def get_chat_history(
    session_id: str,
    limit: int = 100,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """チャット履歴を取得 (認証必須)."""
    messages = await _chat_history_service.list_messages(
        session_id=session_id,
        limit=limit,
        user=user,
    )
    return {
        "session_id": session_id,
        "count": len(messages),
        "messages": messages,
    }


@app.post("/api/rag/query")
async def rag_query(
    request: RAGQueryRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """RAG クエリ API (認証必須)."""
    collection = kb_registry.resolve_collection(
        kb_type=request.kb_type,
        explicit_collection=request.collection,
    )
    service = RAGService(
        RAGConfig(
            collection=collection,
            top_k=request.top_k,
        )
    )
    result = await service.execute(action="query", question=request.question)
    return result.data


@app.post("/api/rag/add")
async def rag_add_document(
    request: AddDocumentRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメント追加 API (認証必須)."""
    collection = kb_registry.resolve_collection(
        kb_type=request.kb_type,
        explicit_collection=request.collection,
    )
    service = _get_rag_service(collection=collection)
    result = await service.execute(
        action="add_document",
        content=request.content,
        metadata=request.metadata,
    )
    return result.data


@app.post("/api/sql/query")
async def sql_query(
    request: SQLQueryRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """SQL クエリ API (認証必須)."""
    service = _get_sql_service()
    result = await service.execute(action="query", question=request.question)
    return result.data


@app.post("/api/sales/analyze")
async def sales_analyze(
    request: SQLQueryRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """売上分析 API (認証必須).

    SalesAgent を使用して売上データを分析します。
    """
    agent = _get_sales_agent()
    return await agent.run({"question": request.question})


@app.get("/api/assets/{artifact_id}/download")
async def download_artifact(
    artifact_id: str,
    _user: UserInfo = Depends(require_auth),
) -> FileResponse:
    """生成アセットをダウンロード (認証必須)."""
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


@app.get("/api/nodes/service")
async def list_service_nodes() -> dict[str, Any]:
    """利用可能なサービスノード一覧を取得.

    Studio UI からノーコードで使用する際のサービス定義を返す。
    """
    return {
        "nodes": [
            {
                "type": "rag",
                "label": "RAGノード",
                "description": "ナレッジベース検索と回答生成",
                "config_fields": ["collection", "chunk_strategy", "top_k"],
            },
            {
                "type": "text2sql",
                "label": "Text2SQLノード",
                "description": "自然言語からSQLを生成して実行",
                "config_fields": ["dialect", "schema", "max_rows"],
            },
            {
                "type": "chart",
                "label": "チャートノード",
                "description": "データの可視化",
                "config_fields": ["chart_type", "enable_drill_down"],
            },
            {
                "type": "suggestion",
                "label": "提案ノード",
                "description": "フォローアップ質問を提案",
                "config_fields": ["max_suggestions", "types"],
            },
        ],
    }


@app.get("/api/kb/settings")
async def get_kb_settings(
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """知識ベース設定を取得 (認証必須)."""
    await kb_registry.ensure_initialized()
    settings = kb_registry.to_dict()
    settings["resolved_default_collection"] = _resolve_default_collection()
    return settings


@app.patch("/api/kb/settings")
async def update_kb_settings(
    request: KnowledgeBaseUpdateRequest,
    _user: UserInfo = Depends(require_role("admin", "manager")),
) -> dict[str, Any]:
    """知識ベース設定を更新 (admin/manager 限定)."""
    settings = await kb_registry.update(request)

    # 既存キャッシュ破棄（新設定を即時反映）
    stale_keys = [key for key in _services if key.startswith("rag:") or key == "faq_agent"]
    for key in stale_keys:
        _services.pop(key, None)

    return {
        "success": True,
        "message": "Knowledge base settings updated",
        "settings": settings.model_dump(),
    }


@app.websocket("/ws/{client_id}")
async def websocket_endpoint(websocket: WebSocket, client_id: str) -> None:
    """WebSocket エンドポイント - リアルタイム双方向通信."""
    access_token = websocket.query_params.get("access_token")
    authorization_header = websocket.headers.get("authorization")
    authorization = authorization_header or (
        f"Bearer {access_token}" if access_token else None
    )
    session_token = websocket.cookies.get("session_token")

    proxy_user = websocket.headers.get("x-forwarded-user") or websocket.headers.get("x-auth-user")
    proxy_name = (
        websocket.headers.get("x-forwarded-preferred-username")
        or websocket.headers.get("x-auth-name")
    )
    proxy_role = websocket.headers.get("x-forwarded-groups") or websocket.headers.get("x-auth-role")
    proxy_department = (
        websocket.headers.get("x-forwarded-department")
        or websocket.headers.get("x-auth-department")
    )
    proxy_position = (
        websocket.headers.get("x-forwarded-title")
        or websocket.headers.get("x-auth-position")
    )

    user = await resolve_user(
        authorization=authorization,
        session_token=session_token,
        x_auth_user=proxy_user,
        x_auth_name=proxy_name,
        x_auth_role=proxy_role,
        x_auth_department=proxy_department,
        x_auth_position=proxy_position,
        x_auth_timestamp=websocket.headers.get("x-auth-timestamp"),
        x_auth_nonce=websocket.headers.get("x-auth-nonce"),
        x_auth_signature=websocket.headers.get("x-auth-signature"),
        request_method="GET",
        request_path=websocket.url.path,
    )
    if not user:
        await websocket.close(code=4401, reason="Unauthorized")
        return

    await _ws_manager.connect(websocket, client_id)

    try:
        while True:
            data = await websocket.receive_json()

            if data.get("type") == "chat":
                message = data.get("message", "")
                session_id = _resolve_session_id(data.get("sessionId", client_id))
                await _chat_history_service.save_message(
                    session_id=session_id,
                    role="user",
                    content=message,
                    transport="ws",
                    user=user,
                    metadata={"options": data.get("options", {})},
                )

                agent = _get_faq_agent()

                async for event in agent.run_stream(
                    {
                        "question": message,
                        "context": {
                            "user": {
                                "user_id": user.user_id,
                                "username": user.username,
                                "role": user.role,
                                "department": user.department,
                            },
                            "session_id": session_id,
                            "options": data.get("options", {}),
                        },
                    }
                ):
                    if event.get("type") == "result" and isinstance(
                        event.get("data"),
                        dict,
                    ):
                        event["data"] = _register_artifacts(event["data"])
                        event["data"]["session_id"] = session_id
                        await _chat_history_service.save_message(
                            session_id=session_id,
                            role="assistant",
                            content=_extract_assistant_content(event["data"]),
                            transport="ws",
                            user=user,
                            metadata={"query_type": event["data"].get("query_type")},
                        )
                    await _ws_manager.send_message(client_id, event)

    except WebSocketDisconnect:
        _ws_manager.disconnect(client_id)
    except Exception as e:
        logger.exception("WebSocket error: %s", e)
        await _chat_history_service.save_message(
            session_id=client_id,
            role="system",
            content=str(e),
            transport="ws",
            user=user if "user" in locals() else None,
            metadata={"error": True},
        )
        await _ws_manager.send_message(
            client_id,
            {
                "type": "error",
                "message": str(e),
            },
        )


@app.post("/api/feedback")
async def submit_feedback(
    request: FeedbackRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, str]:
    """フィードバック送信 (認証必須)."""
    logger.info(
        "Feedback: user=%s, message_id=%s, helpful=%s, comment=%s",
        user.username,
        request.message_id,
        request.helpful,
        request.comment,
    )
    # TODO: 実際はDBに保存
    return {"status": "ok", "message": "フィードバックを受け付けました"}


@app.get("/api/health")
async def health_check() -> dict[str, Any]:
    """ヘルスチェック."""
    return {
        "status": "ok",
        "service": "faq-system",
        "version": "2.0.0",
        "timestamp": datetime.now(tz=UTC).isoformat(),
    }


# =============================================================================
# エントリポイント
# =============================================================================


if __name__ == "__main__":
    import uvicorn
    import json
    from pathlib import Path

    config_path = Path(__file__).resolve().parent / "app_config.json"
    config_raw: dict = {}
    if config_path.is_file():
        try:
            config_raw = json.loads(config_path.read_text("utf-8"))
        except json.JSONDecodeError:
            config_raw = {}

    api_port = config_raw.get("ports", {}).get("api", 8001)
    uvicorn.run(app, host="0.0.0.0", port=int(api_port))
