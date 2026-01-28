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
from agentflow.channels import DiscordAdapter, MessageGateway, SlackAdapter, TelegramAdapter

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

# ChatBot Skill（复用现有）
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
        logger.info(f"✓ LLM Provider initialized")
    except Exception as e:
        logger.error(f"✗ Failed to initialize LLM: {e}")
        logger.error("Please set OPENAI_API_KEY or other LLM provider keys")

    # 启动服务
    uvicorn.run(
        app,
        host="0.0.0.0",
        port=8000,
        log_level="info",
    )
