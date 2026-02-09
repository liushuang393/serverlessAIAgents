"""Market Trend Monitor Enhanced API.

改善点：
1. 富文本レポート（Markdown、コード、表格、チャート）
2. WebSocket リアルタイム進捗
3. ECharts 互換チャートデータ
4. Intelligence Skills 統合

使用例:
    uvicorn apps.market_trend_monitor.backend.api.main_enhanced:app --reload --port 8003
"""

from __future__ import annotations

import asyncio
import json
import logging
import uuid
from datetime import datetime
from typing import Any

from fastapi import FastAPI, WebSocket, WebSocketDisconnect
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import HTMLResponse, StreamingResponse
from pydantic import BaseModel, Field

from agentflow.protocols.a2ui.rich_content import (
    AlertType,
    ChartType,
    RichResponse,
)


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# アプリケーション設定
# =============================================================================


app = FastAPI(
    title="Market Trend Monitor Enhanced",
    description="強化版市場動向監視システム - 富文本 + リアルタイム + チャート",
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
# リクエスト/レスポンスモデル
# =============================================================================


class CollectRequest(BaseModel):
    """収集リクエスト."""

    keywords: list[str] = Field(default_factory=list, description="検索キーワード")
    sources: list[str] = Field(
        default=["news", "social"],
        description="データソース",
    )
    max_results: int = Field(default=50, description="最大結果数")


class TrendFilter(BaseModel):
    """トレンドフィルタ."""

    min_score: float = Field(default=0.0, description="最小スコア")
    sentiment: str | None = Field(None, description="センチメント")
    period: str | None = Field(None, description="期間")


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
# 内部処理関数
# =============================================================================


async def _collect_data(
    keywords: list[str],
    sources: list[str],
    progress_callback: Any = None,
) -> list[dict[str, Any]]:
    """データ収集（シミュレーション）."""
    # 実際は WebCrawler Skill を使用
    await asyncio.sleep(1)  # シミュレート

    if progress_callback:
        await progress_callback(30, "データ収集中...")

    # デモデータ
    return [
        {
            "id": f"art-{i}",
            "title": f"{keywords[0] if keywords else 'AI'} に関するニュース {i}",
            "url": f"https://example.com/news/{i}",
            "source": sources[0] if sources else "news",
            "published_at": datetime.now().isoformat(),
            "content": f"この記事は {keywords[0] if keywords else 'AI'} について説明しています...",
            "keywords": keywords or ["AI", "LLM"],
        }
        for i in range(10)
    ]



async def _analyze_trends(
    articles: list[dict[str, Any]],
    progress_callback: Any = None,
) -> list[dict[str, Any]]:
    """トレンド分析（シミュレーション）."""
    await asyncio.sleep(1)

    if progress_callback:
        await progress_callback(60, "トレンド分析中...")

    # デモトレンド
    return [
        {
            "id": f"trend-{i}",
            "topic": f"トレンド{i+1}",
            "score": 0.9 - (i * 0.1),
            "articles_count": 10 - i,
            "sentiment": ["positive", "neutral", "negative"][i % 3],
            "growth_rate": 0.3 - (i * 0.1),
            "keywords": ["AI", "LLM", "Agent"][: (3 - i % 3)],
        }
        for i in range(5)
    ]



async def _generate_report(
    trends: list[dict[str, Any]],
    progress_callback: Any = None,
) -> dict[str, Any]:
    """レポート生成（富文本）."""
    await asyncio.sleep(1)

    if progress_callback:
        await progress_callback(90, "レポート生成中...")

    # 富文本レスポンス生成
    response = RichResponse()

    # エグゼクティブサマリー
    summary = f"""## エグゼクティブサマリー

本週の市場動向分析により、{len(trends)}件の主要トレンドを検出しました。

### 主要ハイライト
- **最注目トレンド**: {trends[0]['topic']}（スコア: {trends[0]['score']:.2f}）
- **成長中**: {sum(1 for t in trends if t['growth_rate'] > 0)}件
- **全体センチメント**: 主にポジティブ
"""
    response.add_markdown(summary)

    # トレンドテーブル
    table_data = [
        {
            "トレンド": t["topic"],
            "スコア": round(t["score"], 2),
            "記事数": t["articles_count"],
            "成長率": f"{t['growth_rate']:.1%}",
            "センチメント": t["sentiment"],
        }
        for t in trends
    ]
    response.add_table(table_data, title="トレンド一覧")

    # スコアチャート
    chart_data = {
        "title": {"text": "トレンドスコア"},
        "tooltip": {"trigger": "axis"},
        "xAxis": {
            "type": "category",
            "data": [t["topic"] for t in trends],
        },
        "yAxis": {"type": "value", "max": 1},
        "series": [{
            "type": "bar",
            "data": [t["score"] for t in trends],
            "itemStyle": {"color": "#3b82f6"},
        }],
    }
    response.add_chart(ChartType.BAR, chart_data, title="トレンドスコア分析")

    # 成長率チャート
    growth_chart = {
        "title": {"text": "成長率推移"},
        "tooltip": {"trigger": "axis"},
        "xAxis": {
            "type": "category",
            "data": [t["topic"] for t in trends],
        },
        "yAxis": {"type": "value"},
        "series": [{
            "type": "line",
            "data": [t["growth_rate"] * 100 for t in trends],
            "itemStyle": {"color": "#10b981"},
            "areaStyle": {"opacity": 0.3},
        }],
    }
    response.add_chart(ChartType.LINE, growth_chart, title="成長率分析")

    # センチメント分布（円グラフ）
    sentiment_counts = {}
    for t in trends:
        s = t["sentiment"]
        sentiment_counts[s] = sentiment_counts.get(s, 0) + 1

    pie_chart = {
        "title": {"text": "センチメント分布"},
        "tooltip": {"trigger": "item"},
        "series": [{
            "type": "pie",
            "radius": "50%",
            "data": [
                {"name": k, "value": v}
                for k, v in sentiment_counts.items()
            ],
        }],
    }
    response.add_chart(ChartType.PIE, pie_chart, title="センチメント分析")

    # アラート
    if any(t["growth_rate"] > 0.2 for t in trends):
        response.add_alert(
            "急成長トレンドを検出しました！",
            AlertType.WARNING,
            title="注意",
        )

    return {
        "id": f"report-{uuid.uuid4().hex[:8]}",
        "title": f"市場動向レポート ({datetime.now().strftime('%Y-%m-%d')})",
        "rich_response": response.to_dict(),
        "trends": trends,
        "generated_at": datetime.now().isoformat(),
        "metadata": {
            "trends_count": len(trends),
            "analysis_version": "2.0",
        },
    }


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
    <title>Market Trend Monitor Enhanced</title>
    <script src="https://cdn.tailwindcss.com"></script>
    <script src="https://cdn.jsdelivr.net/npm/marked/marked.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/echarts@5.4.3/dist/echarts.min.js"></script>
    <style>
        .chart-container { min-height: 300px; }
        .fade-in { animation: fadeIn 0.5s ease-in; }
        @keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #e5e7eb; padding: 10px 14px; text-align: left; }
        th { background: #f3f4f6; font-weight: 600; }
        .progress-ring { transition: stroke-dashoffset 0.3s; }
    </style>
</head>
<body class="bg-gray-100 min-h-screen">
    <!-- Header -->
    <header class="bg-gradient-to-r from-blue-600 to-indigo-600 text-white shadow-lg">
        <div class="max-w-6xl mx-auto px-6 py-4 flex items-center justify-between">
            <h1 class="text-2xl font-bold">📊 Market Trend Monitor</h1>
            <span id="status" class="text-sm bg-white/20 px-3 py-1 rounded-full">● 接続中</span>
        </div>
    </header>

    <!-- Main Content -->
    <main class="max-w-6xl mx-auto px-6 py-8">
        <!-- Controls -->
        <div class="bg-white rounded-lg shadow-md p-6 mb-8">
            <h2 class="text-lg font-semibold mb-4">収集設定</h2>
            <div class="grid grid-cols-1 md:grid-cols-3 gap-4">
                <div>
                    <label class="block text-sm text-gray-600 mb-1">キーワード</label>
                    <input type="text" id="keywords" value="AI, LLM, Agent"
                        class="w-full px-3 py-2 border rounded-lg focus:ring-2 focus:ring-blue-500">
                </div>
                <div>
                    <label class="block text-sm text-gray-600 mb-1">データソース</label>
                    <select id="sources" class="w-full px-3 py-2 border rounded-lg">
                        <option value="news,social">ニュース + SNS</option>
                        <option value="news">ニュースのみ</option>
                        <option value="social">SNSのみ</option>
                    </select>
                </div>
                <div class="flex items-end">
                    <button id="analyze-btn" onclick="startAnalysis()"
                        class="w-full px-6 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:opacity-50">
                        分析開始
                    </button>
                </div>
            </div>
        </div>

        <!-- Progress -->
        <div id="progress-section" class="hidden bg-white rounded-lg shadow-md p-6 mb-8">
            <div class="flex items-center gap-4">
                <div class="flex-1">
                    <div class="flex justify-between mb-2">
                        <span id="progress-text" class="text-sm text-gray-600">処理中...</span>
                        <span id="progress-percent" class="text-sm font-semibold">0%</span>
                    </div>
                    <div class="w-full bg-gray-200 rounded-full h-3">
                        <div id="progress-bar" class="bg-blue-600 h-3 rounded-full transition-all duration-500" style="width: 0%"></div>
                    </div>
                </div>
            </div>
            <div id="agent-status" class="mt-4 grid grid-cols-4 gap-2 text-xs">
                <div class="agent-step bg-gray-100 rounded p-2 text-center" data-step="collect">収集</div>
                <div class="agent-step bg-gray-100 rounded p-2 text-center" data-step="analyze">分析</div>
                <div class="agent-step bg-gray-100 rounded p-2 text-center" data-step="report">レポート</div>
                <div class="agent-step bg-gray-100 rounded p-2 text-center" data-step="notify">通知</div>
            </div>
        </div>

        <!-- Results -->
        <div id="results-section" class="hidden">
            <div id="rich-content" class="bg-white rounded-lg shadow-md p-6"></div>
        </div>
    </main>

    <script>
        let ws = null;
        const clientId = 'client-' + Date.now();

        // WebSocket 接続
        function connectWebSocket() {
            ws = new WebSocket(`ws://${window.location.host}/ws/${clientId}`);
            ws.onopen = () => {
                document.getElementById('status').textContent = '● 接続中';
                document.getElementById('status').className = 'text-sm bg-white/20 px-3 py-1 rounded-full';
            };
            ws.onclose = () => {
                document.getElementById('status').textContent = '○ 切断';
                setTimeout(connectWebSocket, 3000);
            };
            ws.onmessage = (event) => handleMessage(JSON.parse(event.data));
        }
        connectWebSocket();

        function handleMessage(msg) {
            if (msg.type === 'progress') {
                updateProgress(msg.progress, msg.message, msg.step);
            } else if (msg.type === 'result') {
                showResults(msg.data);
            } else if (msg.type === 'error') {
                showError(msg.message);
            }
        }

        function updateProgress(value, text, step) {
            document.getElementById('progress-section').classList.remove('hidden');
            document.getElementById('progress-bar').style.width = value + '%';
            document.getElementById('progress-percent').textContent = value + '%';
            document.getElementById('progress-text').textContent = text;

            // Agent ステップ更新
            document.querySelectorAll('.agent-step').forEach(el => {
                el.classList.remove('bg-blue-500', 'text-white');
                el.classList.add('bg-gray-100');
            });
            if (step) {
                const stepEl = document.querySelector(`[data-step="${step}"]`);
                if (stepEl) {
                    stepEl.classList.remove('bg-gray-100');
                    stepEl.classList.add('bg-blue-500', 'text-white');
                }
            }
        }

        function showResults(data) {
            document.getElementById('progress-section').classList.add('hidden');
            document.getElementById('results-section').classList.remove('hidden');
            document.getElementById('analyze-btn').disabled = false;

            // 富文本レンダリング
            const container = document.getElementById('rich-content');
            container.innerHTML = '';

            if (data.rich_response && data.rich_response.components) {
                data.rich_response.components.forEach(comp => {
                    container.appendChild(renderComponent(comp));
                });

                // チャート初期化
                setTimeout(initCharts, 100);
            }
        }

        function renderComponent(comp) {
            const div = document.createElement('div');
            div.className = 'mb-6 fade-in';

            switch (comp.type) {
                case 'markdown':
                    div.innerHTML = `<div class="prose max-w-none">${marked.parse(comp.props.content || '')}</div>`;
                    break;
                case 'data_table':
                    div.innerHTML = renderTable(comp.props);
                    break;
                case 'chart':
                    const chartId = 'chart-' + Math.random().toString(36).substr(2, 9);
                    div.innerHTML = `
                        <div class="bg-gray-50 rounded-lg p-4">
                            <h3 class="text-sm font-semibold text-gray-600 mb-2">${comp.props.title || ''}</h3>
                            <div id="${chartId}" class="chart-container" data-chart='${JSON.stringify(comp.props.data)}'></div>
                        </div>
                    `;
                    break;
                case 'alert':
                    const colors = {
                        info: 'bg-blue-50 border-blue-500 text-blue-700',
                        success: 'bg-green-50 border-green-500 text-green-700',
                        warning: 'bg-yellow-50 border-yellow-500 text-yellow-700',
                        error: 'bg-red-50 border-red-500 text-red-700',
                    };
                    div.innerHTML = `
                        <div class="p-4 rounded-lg border-l-4 ${colors[comp.props.alertType] || colors.info}">
                            ${comp.props.title ? `<strong>${comp.props.title}</strong><br>` : ''}
                            ${comp.props.message}
                        </div>
                    `;
                    break;
            }
            return div;
        }

        function renderTable(props) {
            if (!props.data || !props.data.length) return '<p>データなし</p>';
            const cols = Object.keys(props.data[0]);
            let html = `<div class="overflow-x-auto">`;
            if (props.title) html += `<h3 class="text-sm font-semibold text-gray-600 mb-2">${props.title}</h3>`;
            html += '<table class="min-w-full bg-white rounded-lg overflow-hidden"><thead><tr>';
            cols.forEach(c => { html += `<th>${c}</th>`; });
            html += '</tr></thead><tbody>';
            props.data.forEach(row => {
                html += '<tr class="hover:bg-gray-50">';
                cols.forEach(c => { html += `<td>${row[c] ?? ''}</td>`; });
                html += '</tr>';
            });
            html += '</tbody></table></div>';
            return html;
        }

        function initCharts() {
            document.querySelectorAll('.chart-container[data-chart]').forEach(el => {
                if (!el.dataset.chartInit) {
                    const chart = echarts.init(el);
                    chart.setOption(JSON.parse(el.dataset.chart));
                    el.dataset.chartInit = 'true';
                    window.addEventListener('resize', () => chart.resize());
                }
            });
        }

        async function startAnalysis() {
            const keywords = document.getElementById('keywords').value.split(',').map(k => k.trim());
            const sources = document.getElementById('sources').value.split(',');

            document.getElementById('analyze-btn').disabled = true;
            document.getElementById('results-section').classList.add('hidden');

            if (ws && ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify({ type: 'collect', keywords, sources }));
            } else {
                // フォールバック: REST API
                try {
                    updateProgress(0, '処理を開始...', 'collect');
                    const response = await fetch('/api/v2/collect/stream', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({ keywords, sources }),
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
                    showError(e.message);
                }
            }
        }

        function showError(message) {
            document.getElementById('progress-section').classList.add('hidden');
            document.getElementById('analyze-btn').disabled = false;
            alert('エラー: ' + message);
        }
    </script>
</body>
</html>
"""


@app.post("/api/v2/collect")
async def collect_v2(request: CollectRequest) -> dict[str, Any]:
    """データ収集（同期）."""
    articles = await _collect_data(request.keywords, request.sources)
    trends = await _analyze_trends(articles)
    return await _generate_report(trends)


@app.post("/api/v2/collect/stream")
async def collect_stream_v2(request: CollectRequest) -> StreamingResponse:
    """データ収集（ストリーム）."""

    async def event_generator():
        try:
            # Step 1: 収集
            yield f"data: {json.dumps({'type': 'progress', 'progress': 10, 'message': 'データを収集中...', 'step': 'collect'})}\n\n"

            articles = await _collect_data(request.keywords, request.sources)
            yield f"data: {json.dumps({'type': 'progress', 'progress': 30, 'message': f'{len(articles)}件を収集', 'step': 'collect'})}\n\n"

            # Step 2: 分析
            yield f"data: {json.dumps({'type': 'progress', 'progress': 50, 'message': 'トレンドを分析中...', 'step': 'analyze'})}\n\n"

            trends = await _analyze_trends(articles)
            yield f"data: {json.dumps({'type': 'progress', 'progress': 70, 'message': f'{len(trends)}件のトレンドを検出', 'step': 'analyze'})}\n\n"

            # Step 3: レポート
            yield f"data: {json.dumps({'type': 'progress', 'progress': 85, 'message': 'レポートを生成中...', 'step': 'report'})}\n\n"

            report = await _generate_report(trends)

            # Step 4: 完了
            yield f"data: {json.dumps({'type': 'progress', 'progress': 100, 'message': '完了', 'step': 'notify'})}\n\n"
            yield f"data: {json.dumps({'type': 'result', 'data': report}, default=str)}\n\n"

        except Exception as e:
            yield f"data: {json.dumps({'type': 'error', 'message': str(e)})}\n\n"

    return StreamingResponse(event_generator(), media_type="text/event-stream")


@app.websocket("/ws/{client_id}")
async def websocket_endpoint(websocket: WebSocket, client_id: str) -> None:
    """WebSocket エンドポイント."""
    await manager.connect(websocket, client_id)

    try:
        while True:
            data = await websocket.receive_json()

            if data.get("type") == "collect":
                keywords = data.get("keywords", [])
                sources = data.get("sources", ["news"])

                async def progress_cb(progress: int, message: str, step: str = "") -> None:
                    await manager.send_message(client_id, {
                        "type": "progress",
                        "progress": progress,
                        "message": message,
                        "step": step,
                    })

                # Step 1: 収集
                await progress_cb(10, "データを収集中...", "collect")
                articles = await _collect_data(keywords, sources)
                await progress_cb(30, f"{len(articles)}件を収集", "collect")

                # Step 2: 分析
                await progress_cb(50, "トレンドを分析中...", "analyze")
                trends = await _analyze_trends(articles)
                await progress_cb(70, f"{len(trends)}件のトレンドを検出", "analyze")

                # Step 3: レポート
                await progress_cb(85, "レポートを生成中...", "report")
                report = await _generate_report(trends)

                # Step 4: 完了
                await progress_cb(100, "完了", "notify")
                await manager.send_message(client_id, {
                    "type": "result",
                    "data": report,
                })

    except WebSocketDisconnect:
        manager.disconnect(client_id)
    except Exception as e:
        logger.exception("WebSocket error: %s", e)
        await manager.send_message(client_id, {
            "type": "error",
            "message": str(e),
        })


@app.get("/api/v2/trends")
async def get_trends_v2(filter: TrendFilter | None = None) -> dict[str, Any]:
    """トレンド一覧（デモ）."""
    trends = [
        {
            "id": f"trend-{i}",
            "topic": f"トレンド{i+1}",
            "score": 0.9 - (i * 0.1),
            "articles_count": 10 - i,
            "sentiment": ["positive", "neutral", "negative"][i % 3],
            "growth_rate": 0.3 - (i * 0.1),
        }
        for i in range(5)
    ]

    return {
        "trends": trends,
        "total": len(trends),
        "timestamp": datetime.now().isoformat(),
    }


@app.get("/api/v2/health")
async def health_check() -> dict[str, Any]:
    """ヘルスチェック."""
    return {
        "status": "ok",
        "service": "market-trend-monitor-enhanced",
        "version": "2.0.0",
        "timestamp": datetime.now().isoformat(),
    }


# =============================================================================
# エントリポイント
# =============================================================================


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8003)
