#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Messaging Hub - Multi-Platform AI Chatbot.

统一消息平台网关，支持 Telegram、Slack、Discord 等多平台集成。

特性：
- 多平台消息路由（Telegram/Slack/Discord）
- 统一会话管理
- WebSocket 实时同步
- AI Agent 集成
- 富文本界面（Live Canvas）

架构：
    Message Platforms → Gateway → ChatBot → Agent/Coordinator
                            ↓
                      WebSocket Hub → Frontend

运行方式：
    # 开发模式
    python apps/messaging_hub/main.py

    # 生产模式
    uvicorn apps.messaging_hub.main:app --host 0.0.0.0 --port 8000

环境变量：
    TELEGRAM_BOT_TOKEN: Telegram Bot Token
    SLACK_BOT_TOKEN: Slack Bot Token
    SLACK_SIGNING_SECRET: Slack Signing Secret
    DISCORD_BOT_TOKEN: Discord Bot Token
    OPENAI_API_KEY: OpenAI API Key（或其他 LLM provider）
"""

from __future__ import annotations

import asyncio
import logging
import os
from contextlib import asynccontextmanager
from typing import Any

from fastapi import FastAPI, Request, WebSocket, WebSocketDisconnect
from fastapi.responses import JSONResponse

from agentflow import ChatBotSkill, WebSocketHub, get_llm
from agentflow.channels import (
    DiscordAdapter,
    MessageGateway,
    SignalAdapter,
    SlackAdapter,
    TeamsAdapter,
    TelegramAdapter,
    WhatsAppAdapter,
)
from agentflow.skills import ConversationExportSkill, ExportFormat
from apps.messaging_hub.coordinator import AssistantConfig, PersonalAssistantCoordinator

# 配置日志
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("messaging_hub")


# =========================================================================
# 全局实例
# =========================================================================

# WebSocket Hub
hub = WebSocketHub()

# Personal Assistant Coordinator（新機能）
assistant_config = AssistantConfig(
    enable_os_skills=True,
    enable_browser_skills=True,
    summary_language="ja",
    use_emoji=True,
)
assistant = PersonalAssistantCoordinator(config=assistant_config)

# ChatBot Skill（复用现有、assistantと連携）
chatbot = ChatBotSkill(
    # 可以在这里添加 coordinator 或 rag_skill
    temperature=0.7,
)

# Message Gateway
gateway = MessageGateway(hub, chatbot)

# 后台任务列表
background_tasks: list[asyncio.Task[None]] = []


# =========================================================================
# 生命周期管理
# =========================================================================


@asynccontextmanager
async def lifespan(app: FastAPI) -> Any:
    """应用生命周期管理."""
    logger.info("Starting Messaging Hub...")

    # 1. 注册平台适配器
    await setup_platforms()

    # 2. 启动后台任务（Discord bot）
    await start_background_tasks()

    logger.info("Messaging Hub started successfully")
    logger.info(f"Registered platforms: {gateway.list_channels()}")

    yield

    # 清理资源
    logger.info("Shutting down Messaging Hub...")

    # 停止后台任务
    for task in background_tasks:
        task.cancel()
    await asyncio.gather(*background_tasks, return_exceptions=True)

    # 关闭网关
    await gateway.shutdown()

    logger.info("Messaging Hub shut down")


async def setup_platforms() -> None:
    """设置消息平台适配器."""
    # Telegram
    telegram_token = os.getenv("TELEGRAM_BOT_TOKEN")
    if telegram_token:
        telegram = TelegramAdapter(token=telegram_token)
        gateway.register_channel("telegram", telegram)
        logger.info("✓ Telegram adapter registered")

        # 获取 bot 信息
        try:
            bot_info = await telegram.get_bot_info()
            logger.info(f"  Telegram Bot: @{bot_info.get('username')}")
        except Exception as e:
            logger.warning(f"  Failed to get Telegram bot info: {e}")
    else:
        logger.warning("✗ TELEGRAM_BOT_TOKEN not set, skipping Telegram")

    # Slack
    slack_token = os.getenv("SLACK_BOT_TOKEN")
    slack_secret = os.getenv("SLACK_SIGNING_SECRET")
    if slack_token:
        slack = SlackAdapter(token=slack_token, signing_secret=slack_secret)
        gateway.register_channel("slack", slack)
        logger.info("✓ Slack adapter registered")

        # 获取 bot 信息
        try:
            bot_info = await slack.get_bot_info()
            logger.info(f"  Slack Bot: {bot_info.get('user')}")
        except Exception as e:
            logger.warning(f"  Failed to get Slack bot info: {e}")
    else:
        logger.warning("✗ SLACK_BOT_TOKEN not set, skipping Slack")

    # Discord
    discord_token = os.getenv("DISCORD_BOT_TOKEN")
    if discord_token:
        discord = DiscordAdapter(token=discord_token)
        gateway.register_channel("discord", discord)
        logger.info("✓ Discord adapter registered")
    else:
        logger.warning("✗ DISCORD_BOT_TOKEN not set, skipping Discord")

    # Microsoft Teams
    teams_app_id = os.getenv("TEAMS_APP_ID")
    teams_app_password = os.getenv("TEAMS_APP_PASSWORD")
    if teams_app_id and teams_app_password and TeamsAdapter:
        teams = TeamsAdapter(app_id=teams_app_id, app_password=teams_app_password)
        gateway.register_channel("teams", teams)
        logger.info("✓ Teams adapter registered")
    else:
        logger.warning("✗ TEAMS_APP_ID/PASSWORD not set, skipping Teams")

    # WhatsApp
    whatsapp_phone_id = os.getenv("WHATSAPP_PHONE_NUMBER_ID")
    whatsapp_token = os.getenv("WHATSAPP_ACCESS_TOKEN")
    if whatsapp_phone_id and whatsapp_token and WhatsAppAdapter:
        whatsapp = WhatsAppAdapter(
            phone_number_id=whatsapp_phone_id, access_token=whatsapp_token
        )
        gateway.register_channel("whatsapp", whatsapp)
        logger.info("✓ WhatsApp adapter registered")
    else:
        logger.warning("✗ WHATSAPP credentials not set, skipping WhatsApp")

    # Signal
    signal_api_url = os.getenv("SIGNAL_API_URL")
    signal_phone = os.getenv("SIGNAL_PHONE_NUMBER")
    if signal_api_url and signal_phone and SignalAdapter:
        signal = SignalAdapter(api_url=signal_api_url, phone_number=signal_phone)
        gateway.register_channel("signal", signal)
        logger.info("✓ Signal adapter registered")
    else:
        logger.warning("✗ SIGNAL_API_URL/PHONE not set, skipping Signal")


async def start_background_tasks() -> None:
    """启动后台任务."""
    # Discord bot（长连接模式）
    discord_adapter = gateway.get_channel("discord")
    if discord_adapter:
        task = asyncio.create_task(discord_adapter.start_bot(gateway))
        background_tasks.append(task)
        logger.info("Started Discord bot task")

    # 可以添加其他后台任务（如定期清理会话）


# =========================================================================
# FastAPI 应用
# =========================================================================

app = FastAPI(
    title="Messaging Hub",
    description="Multi-Platform AI Chatbot Gateway",
    version="1.0.0",
    lifespan=lifespan,
)


# =========================================================================
# WebSocket 端点
# =========================================================================


@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket) -> None:
    """WebSocket 连接端点.

    前端可以通过此端点接收实时消息更新。
    """
    client_id = websocket.query_params.get("client_id", "anonymous")

    try:
        await hub.connect(websocket, client_id=client_id)
        logger.info(f"WebSocket client connected: {client_id}")

        # 保持连接
        while True:
            # 接收客户端消息（可选）
            data = await websocket.receive_json()
            logger.debug(f"Received from {client_id}: {data}")

            # 处理客户端消息（可扩展）
            # ...

    except WebSocketDisconnect:
        logger.info(f"WebSocket client disconnected: {client_id}")
    except Exception as e:
        logger.error(f"WebSocket error: {e}", exc_info=True)


# =========================================================================
# Webhook 端点
# =========================================================================


@app.post("/webhook/telegram")
async def telegram_webhook(request: Request) -> JSONResponse:
    """Telegram Webhook 端点.

    配置步骤：
    1. 设置 webhook: curl -X POST https://api.telegram.org/bot<TOKEN>/setWebhook \
                      -d url=https://your-domain.com/webhook/telegram
    2. 接收消息更新
    """
    try:
        update_data = await request.json()

        telegram_adapter = gateway.get_channel("telegram")
        if telegram_adapter:
            await telegram_adapter.handle_webhook(update_data, gateway)

        return JSONResponse({"ok": True})

    except Exception as e:
        logger.error(f"Telegram webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


@app.post("/webhook/slack")
async def slack_webhook(request: Request) -> JSONResponse:
    """Slack Webhook 端点.

    配置步骤：
    1. 创建 Slack App: https://api.slack.com/apps
    2. 启用 Event Subscriptions
    3. 设置 Request URL: https://your-domain.com/webhook/slack
    4. 订阅 Bot Events: message.channels, message.im
    """
    try:
        body = await request.body()
        headers = dict(request.headers)

        slack_adapter = gateway.get_channel("slack")
        if slack_adapter:
            response = await slack_adapter.handle_webhook(body, headers, gateway)
            return JSONResponse(response)

        return JSONResponse({"ok": False, "error": "Slack not configured"})

    except Exception as e:
        logger.error(f"Slack webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


@app.post("/webhook/teams")
async def teams_webhook(request: Request) -> JSONResponse:
    """Microsoft Teams Webhook 端点."""
    try:
        activity_data = await request.json()
        auth_header = request.headers.get("Authorization", "")

        teams_adapter = gateway.get_channel("teams")
        if teams_adapter:
            response = await teams_adapter.handle_webhook(
                activity_data, auth_header, gateway
            )
            return JSONResponse(response)

        return JSONResponse({"ok": False, "error": "Teams not configured"})

    except Exception as e:
        logger.error(f"Teams webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


@app.api_route("/webhook/whatsapp", methods=["GET", "POST"])
async def whatsapp_webhook(request: Request) -> JSONResponse:
    """WhatsApp Webhook 端点（検証とメッセージ両方対応）."""
    try:
        whatsapp_adapter = gateway.get_channel("whatsapp")
        if not whatsapp_adapter:
            return JSONResponse({"ok": False, "error": "WhatsApp not configured"})

        if request.method == "GET":
            # Webhook 検証
            verify_token = os.getenv("WHATSAPP_VERIFY_TOKEN", "")
            params = dict(request.query_params)
            response = await whatsapp_adapter.verify_webhook(params, verify_token)
            return JSONResponse(response)
        else:
            # メッセージ処理
            payload = await request.json()
            response = await whatsapp_adapter.handle_webhook(payload, gateway)
            return JSONResponse(response)

    except Exception as e:
        logger.error(f"WhatsApp webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


@app.post("/webhook/signal")
async def signal_webhook(request: Request) -> JSONResponse:
    """Signal Webhook 端点（signal-cli-rest-api callback モード）."""
    try:
        payload = await request.json()

        signal_adapter = gateway.get_channel("signal")
        if signal_adapter:
            response = await signal_adapter.handle_webhook(payload, gateway)
            return JSONResponse(response)

        return JSONResponse({"ok": False, "error": "Signal not configured"})

    except Exception as e:
        logger.error(f"Signal webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


# =========================================================================
# API 端点
# =========================================================================


@app.get("/")
async def root() -> dict[str, Any]:
    """根端点."""
    return {
        "service": "Messaging Hub",
        "version": "1.0.0",
        "status": "running",
        "platforms": gateway.list_channels(),
    }


@app.get("/health")
async def health() -> dict[str, Any]:
    """健康检查."""
    stats = gateway.get_statistics()
    return {
        "status": "healthy",
        "statistics": stats,
    }


@app.get("/platforms")
async def list_platforms() -> dict[str, Any]:
    """列出已注册平台."""
    platforms = []

    for platform_name in gateway.list_channels():
        adapter = gateway.get_channel(platform_name)
        if adapter:
            try:
                bot_info = await adapter.get_bot_info()
                platforms.append({
                    "name": platform_name,
                    "bot_info": bot_info,
                })
            except Exception as e:
                platforms.append({
                    "name": platform_name,
                    "error": str(e),
                })

    return {"platforms": platforms}


@app.post("/send")
async def send_message(
    platform: str,
    channel_id: str,
    text: str,
) -> JSONResponse:
    """直接发送消息到平台（管理接口）.

    Args:
        platform: 平台名称（telegram, slack, discord）
        channel_id: 频道/用户 ID
        text: 消息文本

    Returns:
        消息 ID
    """
    try:
        message_id = await gateway.send_message_to_platform(
            platform=platform,
            channel_id=channel_id,
            text=text,
        )
        return JSONResponse({"ok": True, "message_id": message_id})

    except Exception as e:
        logger.error(f"Failed to send message: {e}")
        return JSONResponse(
            {"ok": False, "error": str(e)},
            status_code=500,
        )


@app.get("/sessions")
async def list_sessions() -> dict[str, Any]:
    """列出所有活跃会话."""
    sessions = chatbot.list_sessions()
    return {"sessions": sessions, "total": len(sessions)}


# =========================================================================
# 管理画面用 API エンドポイント（Admin Dashboard）
# =========================================================================


@app.get("/api/health")
async def api_health() -> dict[str, Any]:
    """管理画面用ヘルスチェック."""
    import time

    stats = gateway.get_statistics()
    return {
        "status": "healthy",
        "uptime": time.time(),  # TODO: 実際の uptime を計算
        "statistics": stats,
    }


@app.get("/api/stats")
async def api_statistics() -> dict[str, Any]:
    """管理画面用統計情報."""
    stats = gateway.get_statistics()
    sessions = chatbot.list_sessions()

    platform_stats = {}
    for platform_name in gateway.list_channels():
        platform_stats[platform_name] = stats.get(f"{platform_name}_messages", 0)

    return {
        "totalMessages": stats.get("total_messages", 0),
        "totalSessions": len(sessions),
        "activeSessions": sum(1 for s in sessions if s.get("active", True)),
        "platformStats": platform_stats,
    }


@app.get("/api/platforms")
async def api_platforms() -> list[dict[str, Any]]:
    """管理画面用プラットフォーム一覧."""
    platforms = []

    for platform_name in gateway.list_channels():
        adapter = gateway.get_channel(platform_name)
        stats = gateway.get_statistics()

        platform_info = {
            "name": platform_name,
            "connected": adapter is not None,
            "lastActivity": stats.get(f"{platform_name}_last_activity"),
            "messageCount": stats.get(f"{platform_name}_messages", 0),
        }
        platforms.append(platform_info)

    return platforms


@app.get("/api/sessions")
async def api_sessions() -> list[dict[str, Any]]:
    """管理画面用セッション一覧."""
    sessions = chatbot.list_sessions()
    return [
        {
            "id": s.get("session_id", ""),
            "platform": s.get("platform", "unknown"),
            "userId": s.get("user_id", ""),
            "userName": s.get("user_name", "Unknown"),
            "startedAt": s.get("started_at", ""),
            "lastMessageAt": s.get("last_message_at", ""),
            "messageCount": s.get("message_count", 0),
        }
        for s in sessions
    ]


@app.get("/api/export")
async def api_export(format: str = "json") -> JSONResponse:
    """会話履歴エクスポート."""
    try:
        # セッションから会話履歴を取得
        sessions = chatbot.list_sessions()
        messages = []

        for session in sessions:
            session_messages = session.get("messages", [])
            for msg in session_messages:
                messages.append({
                    "timestamp": msg.get("timestamp", ""),
                    "platform": session.get("platform", "unknown"),
                    "user_id": session.get("user_id", ""),
                    "user_name": session.get("user_name", "Unknown"),
                    "role": msg.get("role", "user"),
                    "content": msg.get("content", ""),
                })

        # エクスポート
        exporter = ConversationExportSkill()
        export_format = ExportFormat(format) if format in ["json", "csv", "markdown"] else ExportFormat.JSON
        result = await exporter.export(messages, format=export_format)

        return JSONResponse({"data": result})

    except Exception as e:
        logger.error(f"Export failed: {e}", exc_info=True)
        return JSONResponse({"error": str(e)}, status_code=500)


# =========================================================================
# Personal Assistant エンドポイント（新機能）
# =========================================================================


@app.post("/assistant/process")
async def process_assistant_request(
    message: str,
    user_id: str = "default",
) -> dict[str, Any]:
    """Personal Assistant にリクエストを処理させる.

    自然言語でタスクを指示し、主管向けのサマリーを受け取る。

    Examples:
        - "今日のメールを整理して"
        - "ダウンロードフォルダを片付けて"
        - "AI市場の最新動向を調査して"
        - "競合A社の分析レポートを作成して"

    Args:
        message: 自然言語のリクエスト
        user_id: ユーザーID

    Returns:
        処理結果（summary, key_points, actions, risks を含む）
    """
    try:
        result = await assistant.process(message, user_id=user_id)
        return {
            "ok": True,
            "summary": result.get("summary", ""),
            "headline": result.get("headline", ""),
            "key_points": result.get("key_points", []),
            "actions": result.get("actions", []),
            "risks": result.get("risks", []),
            "intent": result.get("intent", {}),
        }
    except Exception as e:
        logger.error("Assistant processing error: %s", e, exc_info=True)
        return {
            "ok": False,
            "error": str(e),
            "summary": f"❌ 処理エラー: {e}",
        }


@app.get("/assistant/templates")
async def list_assistant_templates() -> dict[str, Any]:
    """利用可能なタスクテンプレート一覧を取得."""
    templates = assistant._intent_router.list_templates()
    return {
        "templates": templates,
        "total": len(templates),
        "examples": [
            {"template": "email_organize", "example": "今日のメールを整理して"},
            {"template": "file_organize", "example": "ダウンロードを整理して"},
            {"template": "system_optimize", "example": "PCを最適化して"},
            {"template": "research", "example": "「AI市場」について調べて"},
            {"template": "competitor_analysis", "example": "「競合A社」を分析して"},
            {"template": "report", "example": "「週次レポート」を作成して"},
        ],
    }


# =========================================================================
# 启动入口
# =========================================================================

if __name__ == "__main__":
    import uvicorn

    # 检查环境变量
    if not any([
        os.getenv("TELEGRAM_BOT_TOKEN"),
        os.getenv("SLACK_BOT_TOKEN"),
        os.getenv("DISCORD_BOT_TOKEN"),
    ]):
        logger.warning(
            "⚠️  No platform tokens configured! "
            "Set TELEGRAM_BOT_TOKEN, SLACK_BOT_TOKEN, or DISCORD_BOT_TOKEN"
        )

    # 检查 LLM provider
    try:
        llm = get_llm()
        logger.info("✓ LLM Provider initialized")
    except Exception as e:
        logger.error("✗ Failed to initialize LLM: %s", e)
        logger.error("Please set OPENAI_API_KEY or other LLM provider keys")

    # 启动服务
    uvicorn.run(
        app,
        host="0.0.0.0",
        port=8000,
        log_level="info",
    )
