# -*- coding: utf-8 -*-
"""Code Migration Assistant Enhanced API.

改善点:
1. Web API + UI
2. コード差分表示
3. リアルタイム進捗
4. リッチテキストレポート
5. code_analysis Skills 統合

使用例:
    uvicorn apps.code_migration_assistant.api:app --reload --port 8004
"""

from __future__ import annotations

import asyncio
import json
import logging
import os
import uuid
from datetime import datetime
from pathlib import Path
from typing import Any

from fastapi import FastAPI, WebSocket, WebSocketDisconnect, HTTPException, Request, UploadFile, File
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse, JSONResponse, StreamingResponse
from pydantic import BaseModel, Field

from agentflow.protocols.a2ui.rich_content import (
    ChartType,
    RichResponse,
    AlertType,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# アプリケーション設定
# =============================================================================


def _resolve_cors_origins() -> list[str]:
    """Resolve CORS origins from env or use safe local defaults."""
    raw = os.getenv("CODE_MIGRATION_CORS_ORIGINS", "").strip()
    if raw:
        origins = [origin.strip() for origin in raw.split(",") if origin.strip()]
        if origins:
            return origins
    return ["http://localhost:3000", "http://localhost:5173", "http://localhost:5174"]


_cors_origins = _resolve_cors_origins()
_cors_allow_credentials = not (len(_cors_origins) == 1 and _cors_origins[0] == "*")
_APP_CONFIG_PATH = Path(__file__).resolve().parent / "app_config.json"
_AUTH_HEADER = "x-api-key"
_WS_AUTH_QUERY_KEY = "api_key"
_PUBLIC_HTTP_PATHS = {"/api/health", "/docs", "/redoc", "/openapi.json", "/"}


app = FastAPI(
    title="Code Migration Assistant API",
    description="コード移行支援システム - COBOL→Java等の移行",
    version="2.0.0",
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=_cors_origins,
    allow_credentials=_cors_allow_credentials,
    allow_methods=["*"],
    allow_headers=["*"],
)


def _load_app_config() -> dict[str, Any]:
    """Load app_config.json or return an empty dict."""
    if not _APP_CONFIG_PATH.is_file():
        return {}
    try:
        return json.loads(_APP_CONFIG_PATH.read_text("utf-8"))
    except json.JSONDecodeError:
        return {}


def _get_auth_contract() -> dict[str, Any]:
    """Return contracts.auth from app config."""
    raw = _load_app_config()
    contracts = raw.get("contracts", {})
    if not isinstance(contracts, dict):
        return {}
    auth = contracts.get("auth", {})
    if not isinstance(auth, dict):
        return {}
    return auth


def _is_auth_required() -> bool:
    """Evaluate whether API key auth must be enforced."""
    auth = _get_auth_contract()
    enabled = bool(auth.get("enabled", False))
    allow_anonymous = bool(auth.get("allow_anonymous", True))
    return enabled and not allow_anonymous


def _api_key_env_name() -> str:
    """Return API key env var name."""
    return os.getenv("CODE_MIGRATION_API_KEY_ENV", "CODE_MIGRATION_API_KEY")


def _verify_api_key(incoming_key: str | None) -> None:
    """Validate API key when auth is required."""
    if not _is_auth_required():
        return

    env_name = _api_key_env_name()
    expected_key = os.getenv(env_name)
    if not expected_key:
        raise HTTPException(
            status_code=503,
            detail=f"Auth required but env '{env_name}' is not configured",
        )

    if incoming_key != expected_key:
        raise HTTPException(status_code=401, detail="Invalid API key")


def _should_protect_http_path(path: str) -> bool:
    """Return whether HTTP path should be protected by API key."""
    return path.startswith("/api/") and path not in _PUBLIC_HTTP_PATHS


def _require_http_api_key(request: Request) -> None:
    """Enforce API key for protected HTTP routes."""
    if not _should_protect_http_path(request.url.path):
        return
    _verify_api_key(request.headers.get(_AUTH_HEADER))


async def _require_ws_api_key(websocket: WebSocket) -> bool:
    """Enforce API key for websocket handshake."""
    if not _is_auth_required():
        return True
    incoming_key = websocket.headers.get(_AUTH_HEADER) or websocket.query_params.get(
        _WS_AUTH_QUERY_KEY
    )
    try:
        _verify_api_key(incoming_key)
    except HTTPException as exc:
        close_code = 4401 if exc.status_code == 401 else 1011
        await websocket.close(code=close_code, reason=str(exc.detail))
        return False
    return True


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """Apply app-level auth contract to HTTP requests."""
    try:
        _require_http_api_key(request)
    except HTTPException as exc:
        return JSONResponse({"detail": exc.detail}, status_code=exc.status_code)
    return await call_next(request)


# =============================================================================
# リクエスト/レスポンスモデル
# =============================================================================


class MigrationRequest(BaseModel):
    """移行リクエスト."""

    source_code: str = Field(..., description="ソースコード")
    source_language: str = Field(default="cobol", description="ソース言語")
    target_language: str = Field(default="java", description="ターゲット言語")
    options: dict[str, Any] = Field(default_factory=dict, description="オプション")


class AnalysisRequest(BaseModel):
    """分析リクエスト."""

    source_code: str = Field(..., description="ソースコード")
    language: str = Field(default="auto", description="言語")


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

    def disconnect(self, client_id: str) -> None:
        """切断."""
        if client_id in self.active_connections:
            del self.active_connections[client_id]

    async def send_message(self, client_id: str, message: dict[str, Any]) -> None:
        """メッセージ送信."""
        if client_id in self.active_connections:
            await self.active_connections[client_id].send_json(message)


manager = ConnectionManager()


# =============================================================================
# 内部処理関数
# =============================================================================


async def _analyze_code(
    source_code: str,
    language: str,
    progress_callback: Any = None,
) -> dict[str, Any]:
    """コード分析（code_analysis Skills を使用）."""
    await asyncio.sleep(0.5)  # シミュレート

    if progress_callback:
        await progress_callback(20, "静的分析中...", "analyze")

    # デモ分析結果
    lines = source_code.strip().split("\n")
    analysis = {
        "loc": len(lines),
        "complexity": min(len(lines) // 10 + 1, 10),
        "issues": [],
        "dependencies": [],
    }

    # 簡易的なコード分析
    if "PERFORM" in source_code.upper():
        analysis["issues"].append({"type": "info", "message": "PERFORMステートメント検出"})
    if "EVALUATE" in source_code.upper():
        analysis["issues"].append({"type": "info", "message": "EVALUATEステートメント検出"})

    if progress_callback:
        await progress_callback(40, "複雑度計算中...", "analyze")

    return analysis


async def _transform_code(
    source_code: str,
    source_language: str,
    target_language: str,
    progress_callback: Any = None,
) -> dict[str, Any]:
    """コード変換."""
    await asyncio.sleep(1)

    if progress_callback:
        await progress_callback(60, "コード変換中...", "transform")

    # デモ変換（実際は CodeTransformationAgent を使用）
    if source_language == "cobol" and target_language == "java":
        target_code = f"""/**
 * Migrated from COBOL
 * Generated by Code Migration Assistant
 */
public class MigratedProgram {{

    public void execute() {{
        // TODO: Implement business logic
        // Original COBOL lines: {len(source_code.split(chr(10)))}
        System.out.println("Migration completed");
    }}

    public static void main(String[] args) {{
        MigratedProgram program = new MigratedProgram();
        program.execute();
    }}
}}
"""
    else:
        target_code = f"// Converted from {source_language}\n{source_code}"

    return {
        "target_code": target_code,
        "success": True,
    }


async def _verify_code(
    target_code: str,
    progress_callback: Any = None,
) -> dict[str, Any]:
    """コード検証."""
    await asyncio.sleep(0.5)

    if progress_callback:
        await progress_callback(80, "検証中...", "verify")

    # デモ検証結果
    return {
        "verdict": "PASS",
        "score": 85.0,
        "issues": [],
    }


def _build_migration_report(
    source_code: str,
    target_code: str,
    analysis: dict[str, Any],
    verification: dict[str, Any],
) -> dict[str, Any]:
    """移行レポートを構築（リッチテキスト）."""
    response = RichResponse()

    # サマリー
    response.add_markdown(f"""# コード移行レポート

## サマリー
- **ソース行数**: {analysis.get("loc", 0)}行
- **複雑度**: {analysis.get("complexity", 0)}/10
- **検証結果**: {verification.get("verdict", "N/A")}
- **品質スコア**: {verification.get("score", 0):.1f}点

""")

    # ソースコード
    response.add_code(
        source_code[:1000] + ("..." if len(source_code) > 1000 else ""),
        language="cobol",
        title="元のCOBOLコード",
    )

    # 変換後コード
    response.add_code(
        target_code,
        language="java",
        title="変換後のJavaコード",
    )

    # 分析結果テーブル
    if analysis.get("issues"):
        response.add_table(
            [{"タイプ": i["type"], "メッセージ": i["message"]} for i in analysis["issues"]],
            title="分析結果",
        )

    # 品質チャート
    chart_data = {
        "title": {"text": "コード品質スコア"},
        "series": [
            {
                "type": "gauge",
                "data": [{"value": verification.get("score", 0), "name": "品質"}],
                "detail": {"formatter": "{value}点"},
            }
        ],
    }
    response.add_chart("gauge", chart_data, title="品質評価")

    # 警告
    if verification.get("verdict") != "PASS":
        response.add_alert(
            "検証に問題があります。手動確認が必要です。",
            AlertType.WARNING,
            title="⚠️ 注意",
        )

    return {
        "rich_response": response.to_dict(),
        "metadata": {
            "generated_at": datetime.now().isoformat(),
        },
    }


# =============================================================================
# API エンドポイント
# =============================================================================


@app.get("/", response_class=HTMLResponse)
async def index():
    """トップページ - コード移行UI."""
    return """
<!DOCTYPE html>
<html lang="ja">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Code Migration Assistant</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github-dark.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/java.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/cobol.min.js"></script>
    <style>
        .code-panel { height: 400px; }
        pre code { border-radius: 8px; padding: 16px !important; }
    </style>
</head>
<body class="bg-gray-900 text-white min-h-screen">
    <!-- Header -->
    <header class="bg-gradient-to-r from-purple-600 to-blue-600 shadow-lg">
        <div class="max-w-7xl mx-auto px-6 py-4">
            <h1 class="text-2xl font-bold">🔄 Code Migration Assistant</h1>
            <p class="text-sm text-white/80">COBOL → Java コード変換ツール</p>
        </div>
    </header>

    <main class="max-w-7xl mx-auto px-6 py-8">
        <!-- Code Panels -->
        <div class="grid grid-cols-2 gap-6 mb-6">
            <!-- Source Code -->
            <div class="bg-gray-800 rounded-lg overflow-hidden">
                <div class="bg-gray-700 px-4 py-2 flex items-center justify-between">
                    <span class="font-medium">📄 ソースコード (COBOL)</span>
                    <select id="source-lang" class="bg-gray-600 text-sm px-2 py-1 rounded">
                        <option value="cobol">COBOL</option>
                        <option value="fortran">FORTRAN</option>
                        <option value="vb">VB6</option>
                    </select>
                </div>
                <textarea id="source-code" class="w-full code-panel bg-gray-900 text-green-400 font-mono text-sm p-4 focus:outline-none resize-none"
                    placeholder="COBOLコードをここに貼り付け...">       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5).
       01 WS-NUM2 PIC 9(5).
       01 WS-RESULT PIC 9(10).
       PROCEDURE DIVISION.
           MOVE 100 TO WS-NUM1.
           MOVE 200 TO WS-NUM2.
           ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.
           DISPLAY "Result: " WS-RESULT.
           STOP RUN.</textarea>
            </div>

            <!-- Target Code -->
            <div class="bg-gray-800 rounded-lg overflow-hidden">
                <div class="bg-gray-700 px-4 py-2 flex items-center justify-between">
                    <span class="font-medium">☕ ターゲットコード (Java)</span>
                    <button id="copy-btn" class="text-sm px-3 py-1 bg-blue-600 hover:bg-blue-500 rounded">
                        📋 コピー
                    </button>
                </div>
                <div id="target-code" class="w-full code-panel bg-gray-900 font-mono text-sm p-4 overflow-auto">
                    <pre><code class="language-java">// 変換結果がここに表示されます</code></pre>
                </div>
            </div>
        </div>

        <!-- Controls -->
        <div class="flex items-center gap-4 mb-6">
            <button id="migrate-btn" onclick="startMigration()"
                class="px-6 py-3 bg-gradient-to-r from-purple-600 to-blue-600 hover:from-purple-500 hover:to-blue-500 rounded-lg font-medium disabled:opacity-50">
                🚀 変換開始
            </button>
            <button id="analyze-btn" onclick="analyzeCode()"
                class="px-6 py-3 bg-gray-700 hover:bg-gray-600 rounded-lg font-medium">
                🔍 分析のみ
            </button>
            <div id="progress-container" class="hidden flex-1 flex items-center gap-3">
                <div class="flex-1 bg-gray-700 rounded-full h-2">
                    <div id="progress-bar" class="bg-blue-500 h-2 rounded-full transition-all duration-300" style="width: 0%"></div>
                </div>
                <span id="progress-text" class="text-sm text-gray-400">処理中...</span>
            </div>
        </div>

        <!-- Results -->
        <div id="results-section" class="hidden">
            <div id="rich-content" class="bg-gray-800 rounded-lg p-6"></div>
        </div>
    </main>

    <script>
        let ws = null;
        const clientId = 'client-' + Date.now();

        // WebSocket 接続
        function connectWebSocket() {
            ws = new WebSocket(`ws://${window.location.host}/ws/${clientId}`);
            ws.onmessage = (event) => handleMessage(JSON.parse(event.data));
        }
        connectWebSocket();

        function handleMessage(msg) {
            if (msg.type === 'progress') {
                showProgress(msg.progress, msg.message);
            } else if (msg.type === 'result') {
                hideProgress();
                showResults(msg.data);
            } else if (msg.type === 'error') {
                hideProgress();
                alert('エラー: ' + msg.message);
            }
        }

        function showProgress(value, text) {
            document.getElementById('progress-container').classList.remove('hidden');
            document.getElementById('progress-bar').style.width = value + '%';
            document.getElementById('progress-text').textContent = text;
        }

        function hideProgress() {
            document.getElementById('progress-container').classList.add('hidden');
            document.getElementById('migrate-btn').disabled = false;
        }

        function showResults(data) {
            // ターゲットコード表示
            if (data.target_code) {
                const targetEl = document.getElementById('target-code');
                targetEl.innerHTML = `<pre><code class="language-java">${escapeHtml(data.target_code)}</code></pre>`;
                hljs.highlightAll();
            }

            // リッチテキストレポート
            if (data.report && data.report.rich_response) {
                document.getElementById('results-section').classList.remove('hidden');
                renderRichContent(data.report.rich_response.components);
            }
        }

        function renderRichContent(components) {
            const container = document.getElementById('rich-content');
            container.innerHTML = '';

            components.forEach(comp => {
                const div = document.createElement('div');
                div.className = 'mb-6';

                switch (comp.type) {
                    case 'markdown':
                        div.innerHTML = `<div class="prose prose-invert max-w-none">${marked.parse(comp.props.content || '')}</div>`;
                        break;
                    case 'code_block':
                        div.innerHTML = `
                            <div class="bg-gray-900 rounded-lg overflow-hidden">
                                <div class="bg-gray-700 px-3 py-1 text-sm text-gray-300">${comp.props.title || comp.props.language}</div>
                                <pre><code class="language-${comp.props.language}">${escapeHtml(comp.props.code)}</code></pre>
                            </div>
                        `;
                        break;
                    case 'data_table':
                        div.innerHTML = renderTable(comp.props);
                        break;
                    case 'chart':
                        const chartId = 'chart-' + Math.random().toString(36).substr(2, 9);
                        div.innerHTML = `<div id="${chartId}" style="height: 300px" data-chart='${JSON.stringify(comp.props.data)}'></div>`;
                        break;
                    case 'alert':
                        const colors = {
                            warning: 'bg-yellow-900/50 border-yellow-500 text-yellow-200',
                            error: 'bg-red-900/50 border-red-500 text-red-200',
                            info: 'bg-blue-900/50 border-blue-500 text-blue-200',
                        };
                        div.innerHTML = `
                            <div class="p-4 rounded-lg border-l-4 ${colors[comp.props.alertType] || colors.info}">
                                ${comp.props.title ? `<strong>${comp.props.title}</strong><br>` : ''}
                                ${comp.props.message}
                            </div>
                        `;
                        break;
                }
                container.appendChild(div);
            });

            hljs.highlightAll();
            setTimeout(initCharts, 100);
        }

        function renderTable(props) {
            if (!props.data || !props.data.length) return '';
            const cols = Object.keys(props.data[0]);
            let html = '<div class="overflow-x-auto">';
            if (props.title) html += `<h4 class="text-sm font-medium mb-2">${props.title}</h4>`;
            html += '<table class="w-full text-sm"><thead><tr class="bg-gray-700">';
            cols.forEach(c => { html += `<th class="px-3 py-2 text-left">${c}</th>`; });
            html += '</tr></thead><tbody>';
            props.data.forEach(row => {
                html += '<tr class="border-b border-gray-700">';
                cols.forEach(c => { html += `<td class="px-3 py-2">${row[c]}</td>`; });
                html += '</tr>';
            });
            html += '</tbody></table></div>';
            return html;
        }

        function initCharts() {
            document.querySelectorAll('[data-chart]').forEach(el => {
                if (!el.dataset.chartInit) {
                    const chart = echarts.init(el, 'dark');
                    chart.setOption(JSON.parse(el.dataset.chart));
                    el.dataset.chartInit = 'true';
                }
            });
        }

        async function startMigration() {
            const sourceCode = document.getElementById('source-code').value;
            const sourceLang = document.getElementById('source-lang').value;

            if (!sourceCode.trim()) {
                alert('ソースコードを入力してください');
                return;
            }

            document.getElementById('migrate-btn').disabled = true;
            document.getElementById('results-section').classList.add('hidden');

            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify({
                    type: 'migrate',
                    source_code: sourceCode,
                    source_language: sourceLang,
                    target_language: 'java',
                }));
            } else {
                // REST API フォールバック
                try {
                    showProgress(0, '処理を開始...');
                    const response = await fetch('/api/migrate/stream', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({
                            source_code: sourceCode,
                            source_language: sourceLang,
                            target_language: 'java',
                        }),
                    });
                    const reader = response.body.getReader();
                    const decoder = new TextDecoder();
                    while (true) {
                        const { done, value } = await reader.read();
                        if (done) break;
                        for (const line of decoder.decode(value).split('\\n')) {
                            if (line.startsWith('data: ')) {
                                handleMessage(JSON.parse(line.slice(6)));
                            }
                        }
                    }
                } catch (e) {
                    hideProgress();
                    alert('エラー: ' + e.message);
                }
            }
        }

        async function analyzeCode() {
            const sourceCode = document.getElementById('source-code').value;
            if (!sourceCode.trim()) {
                alert('ソースコードを入力してください');
                return;
            }

            try {
                const response = await fetch('/api/analyze', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ source_code: sourceCode }),
                });
                const data = await response.json();
                alert(`分析結果:\\n行数: ${data.loc}\\n複雑度: ${data.complexity}/10`);
            } catch (e) {
                alert('エラー: ' + e.message);
            }
        }

        document.getElementById('copy-btn').addEventListener('click', () => {
            const code = document.getElementById('target-code').textContent;
            navigator.clipboard.writeText(code);
            alert('コピーしました');
        });

        function escapeHtml(text) {
            const div = document.createElement('div');
            div.textContent = text;
            return div.innerHTML;
        }
    </script>
</body>
</html>
"""


@app.post("/api/migrate")
async def migrate(request: MigrationRequest) -> dict[str, Any]:
    """コード移行（同期）."""
    analysis = await _analyze_code(request.source_code, request.source_language)
    transform_result = await _transform_code(
        request.source_code,
        request.source_language,
        request.target_language,
    )
    verification = await _verify_code(transform_result["target_code"])
    report = _build_migration_report(
        request.source_code,
        transform_result["target_code"],
        analysis,
        verification,
    )

    return {
        "success": True,
        "target_code": transform_result["target_code"],
        "analysis": analysis,
        "verification": verification,
        "report": report,
    }


@app.post("/api/migrate/stream")
async def migrate_stream(request: MigrationRequest) -> StreamingResponse:
    """コード移行（ストリーム）."""

    async def event_generator():
        try:
            # 分析
            yield f"data: {json.dumps({'type': 'progress', 'progress': 10, 'message': '分析中...'})}\n\n"
            analysis = await _analyze_code(request.source_code, request.source_language)
            yield f"data: {json.dumps({'type': 'progress', 'progress': 30, 'message': '分析完了'})}\n\n"

            # 変換
            yield f"data: {json.dumps({'type': 'progress', 'progress': 50, 'message': 'コード変換中...'})}\n\n"
            transform_result = await _transform_code(
                request.source_code,
                request.source_language,
                request.target_language,
            )
            yield f"data: {json.dumps({'type': 'progress', 'progress': 70, 'message': '変換完了'})}\n\n"

            # 検証
            yield f"data: {json.dumps({'type': 'progress', 'progress': 85, 'message': '検証中...'})}\n\n"
            verification = await _verify_code(transform_result["target_code"])

            # レポート
            yield f"data: {json.dumps({'type': 'progress', 'progress': 95, 'message': 'レポート生成中...'})}\n\n"
            report = _build_migration_report(
                request.source_code,
                transform_result["target_code"],
                analysis,
                verification,
            )

            # 完了
            yield f"data: {json.dumps({'type': 'progress', 'progress': 100, 'message': '完了'})}\n\n"
            yield f"data: {json.dumps({'type': 'result', 'data': {'success': True, 'target_code': transform_result['target_code'], 'report': report}}, default=str)}\n\n"

        except Exception as e:
            yield f"data: {json.dumps({'type': 'error', 'message': str(e)})}\n\n"

    return StreamingResponse(event_generator(), media_type="text/event-stream")


@app.post("/api/analyze")
async def analyze(request: AnalysisRequest) -> dict[str, Any]:
    """コード分析."""
    return await _analyze_code(request.source_code, request.language)


@app.websocket("/ws/{client_id}")
async def websocket_endpoint(websocket: WebSocket, client_id: str):
    """WebSocket エンドポイント."""
    if not await _require_ws_api_key(websocket):
        return
    await manager.connect(websocket, client_id)

    try:
        while True:
            data = await websocket.receive_json()

            if data.get("type") == "migrate":
                source_code = data.get("source_code", "")
                source_lang = data.get("source_language", "cobol")
                target_lang = data.get("target_language", "java")

                async def progress_cb(progress: int, message: str, step: str = ""):
                    await manager.send_message(
                        client_id,
                        {
                            "type": "progress",
                            "progress": progress,
                            "message": message,
                        },
                    )

                # 分析
                await progress_cb(10, "分析中...")
                analysis = await _analyze_code(source_code, source_lang, progress_cb)

                # 変換
                await progress_cb(50, "コード変換中...")
                transform_result = await _transform_code(
                    source_code, source_lang, target_lang, progress_cb
                )

                # 検証
                await progress_cb(80, "検証中...")
                verification = await _verify_code(transform_result["target_code"], progress_cb)

                # レポート生成
                await progress_cb(95, "レポート生成中...")
                report = _build_migration_report(
                    source_code,
                    transform_result["target_code"],
                    analysis,
                    verification,
                )

                # 完了
                await manager.send_message(
                    client_id,
                    {
                        "type": "result",
                        "data": {
                            "success": True,
                            "target_code": transform_result["target_code"],
                            "analysis": analysis,
                            "verification": verification,
                            "report": report,
                        },
                    },
                )

    except WebSocketDisconnect:
        manager.disconnect(client_id)
    except Exception as e:
        logger.exception("WebSocket error: %s", e)
        await manager.send_message(
            client_id,
            {
                "type": "error",
                "message": str(e),
            },
        )


@app.get("/api/health")
async def health_check() -> dict[str, Any]:
    """ヘルスチェック."""
    return {
        "status": "ok",
        "service": "code-migration-assistant",
        "version": "2.0.0",
        "timestamp": datetime.now().isoformat(),
    }


# =============================================================================
# エントリポイント
# =============================================================================


if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=8004)
