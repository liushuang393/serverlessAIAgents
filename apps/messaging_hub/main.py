#!/usr/bin/env python3
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
import json
import logging
import os
import time
import uuid
from contextlib import asynccontextmanager
from datetime import UTC, datetime
from pathlib import Path
from typing import Any
from urllib.parse import urlencode

from apps.messaging_hub.approval_manager import ApprovalManager, ApprovalRequest, ApprovalStatus
from apps.messaging_hub.coordinator import AssistantConfig, PersonalAssistantCoordinator
from apps.messaging_hub.execution_tracker import ExecutionEvent, ExecutionStatus, ExecutionTracker
from apps.messaging_hub.skills_manager import SkillsManager, Workflow, WorkflowStatus
from apps.messaging_hub.storage import SQLiteMessagingHubStore
from fastapi import FastAPI, Request, WebSocket, WebSocketDisconnect
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field

from agentflow import WebSocketHub, get_llm
from agentflow.channels import (
    DiscordAdapter,
    MessageGateway,
    SignalAdapter,
    SlackAdapter,
    TeamsAdapter,
    TelegramAdapter,
    WhatsAppAdapter,
)
from agentflow.security.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig
from agentflow.skills import (
    ChatBotSkill,
    ConversationExportSkill,
    ExportFormat,
    RiskLevel,
    create_skill_gateway,
)
from agentflow.tools.cli.runtime_manager import CLIRuntimeManager


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

# ChatBot Skill（复用现有、assistantと連携）
chatbot = ChatBotSkill(
    # 可以在这里添加 coordinator 或 rag_skill
    temperature=0.7,
)

# Message Gateway
gateway = MessageGateway(hub, chatbot)

# 后台任务列表
background_tasks: list[asyncio.Task[None]] = []
_cli_runtime = CLIRuntimeManager()
_assistant_cli_proposals: dict[str, dict[str, Any]] = {}

_APP_CONFIG_PATH = Path(__file__).resolve().parent / "app_config.json"
_PUBLIC_PATHS = {
    "/",
    "/health",
    "/api/health",
    "/docs",
    "/redoc",
    "/openapi.json",
    "/webhook/telegram",
    "/webhook/slack",
    "/webhook/teams",
    "/webhook/signal",
}
_DEFAULT_SECURITY_MODE = "approval_required"
_VALID_SECURITY_MODES = {"read_only", "approval_required", "autonomous"}
_auth_guard = ContractAuthGuard(
    ContractAuthGuardConfig(
        app_config_path=_APP_CONFIG_PATH,
        public_http_paths=_PUBLIC_PATHS,
        http_path_prefixes=("/",),
        auth_header_name="x-api-key",
        ws_query_key="api_key",
        api_key_env_selector_var="MESSAGING_HUB_API_KEY_ENV",
        default_api_key_env_var="MESSAGING_HUB_API_KEY",
    ),
)


def _load_app_config() -> dict[str, Any]:
    """Load app_config.json or return an empty dict."""
    return _auth_guard.load_app_config()


def _get_security_mode() -> str:
    """Return assistant security mode from app_config."""
    raw = _load_app_config()
    mode = str(raw.get("security_mode", "")).strip().lower()
    if mode in _VALID_SECURITY_MODES:
        return mode
    return _DEFAULT_SECURITY_MODE


def _build_assistant_config() -> AssistantConfig:
    """Build assistant config from security mode."""
    mode = _get_security_mode()
    enable_unsafe_ops = mode == "autonomous"
    return AssistantConfig(
        enable_os_skills=enable_unsafe_ops,
        enable_browser_skills=enable_unsafe_ops,
        security_mode=mode,
        summary_language="ja",
        use_emoji=True,
    )


def _is_auth_required() -> bool:
    """Evaluate whether API key auth must be enforced."""
    return _auth_guard.is_auth_required()


def _verify_api_key(incoming_key: str | None) -> None:
    """Validate API key when auth is required."""
    _auth_guard.verify_api_key(incoming_key)


def _is_public_http_path(path: str) -> bool:
    """Return True when HTTP path can bypass API key auth."""
    return not _auth_guard.should_protect_http_path(path)


async def _require_http_api_key(request: Request) -> None:
    """Enforce API key for protected HTTP routes."""
    await _auth_guard.require_http(request)


async def _require_ws_api_key(websocket: WebSocket) -> bool:
    """Enforce API key for websocket handshake."""
    ok, _ = await _auth_guard.require_ws(websocket)
    return ok


def _domain_whitelist_from_env() -> list[str]:
    """環境変数からドメインホワイトリストを読み込む."""
    raw = os.getenv("MESSAGING_HUB_DOMAIN_WHITELIST", "")
    return [value.strip() for value in raw.split(",") if value.strip()]


def _build_skill_gateway() -> Any:
    """セキュリティモードに応じた SkillGateway を作成する."""
    mode = _get_security_mode()
    return create_skill_gateway(
        workspace_path=Path.cwd(),
        execution_mode="isolated",
        domain_whitelist=_domain_whitelist_from_env(),
        allow_write=mode == "autonomous",
        allow_delete=False,
        require_confirmation=True,
    )


_STANDARD_EVENT_ALIASES: dict[str, list[str]] = {
    "RunStarted": ["execution_started"],
    "StepStarted": ["execution_started"],
    "ToolApprovalRequested": ["approval_request"],
    "ToolExecuted": ["execution_completed"],
    "EvidenceAdded": ["execution_completed"],
    "RunFinished": ["execution_completed"],
}


async def _broadcast_execution_event(event_name: str, payload: dict[str, Any]) -> None:
    """標準イベントと互換イベントを併送する."""
    await hub.broadcast({"type": event_name, "data": payload})
    for legacy_event in _STANDARD_EVENT_ALIASES.get(event_name, []):
        await hub.broadcast({"type": legacy_event, "data": payload})


def _risk_level_from_reason(reason: str) -> RiskLevel:
    """確認理由文字列からリスクレベルを推定する."""
    lowered = reason.lower()
    if "critical" in lowered:
        return RiskLevel.CRITICAL
    if "high" in lowered:
        return RiskLevel.HIGH
    if "medium" in lowered:
        return RiskLevel.MEDIUM
    return RiskLevel.LOW


_store = SQLiteMessagingHubStore.from_default_path()
_skill_gateway = _build_skill_gateway()
_approval_manager = ApprovalManager(websocket_hub=hub)
_execution_tracker = ExecutionTracker(websocket_hub=hub)
_skills_manager = SkillsManager(gateway=_skill_gateway, websocket_hub=hub)
_active_step_events: dict[str, str] = {}
_run_started_at: dict[str, float] = {}
_process_started_at = time.time()


async def _on_approval_request(request: ApprovalRequest) -> None:
    """承認要求を永続化し、標準イベントを通知する."""
    await _store.upsert_approval(request.to_dict())
    await _broadcast_execution_event(
        "ToolApprovalRequested",
        {
            "request_id": request.id,
            "skill_name": request.skill_name,
            "risk_level": request.risk_level.value,
            "status": request.status.value,
            "user_id": request.user_id,
            "params": request.params,
            "created_at": request.created_at.isoformat(),
        },
    )


async def _on_approval_decision(request: ApprovalRequest) -> None:
    """承認決裁を永続化する."""
    await _store.upsert_approval(request.to_dict())


async def _confirmation_handler(
    skill_name: str,
    reason: str,
    params: dict[str, Any],
) -> bool:
    """SkillGateway の承認要求ハンドラ."""
    request_id, auto_approved = await _approval_manager.request_approval(
        skill_name=skill_name,
        params=params,
        risk_level=_risk_level_from_reason(reason),
        user_id="assistant",
        metadata={"reason": reason},
    )
    stored = _approval_manager.find_request(request_id)
    if stored is not None:
        await _store.upsert_approval(stored.to_dict())
    if auto_approved:
        return True
    status = await _approval_manager.wait_for_approval(request_id, timeout_seconds=300)
    decided = _approval_manager.find_request(request_id)
    if decided is not None:
        await _store.upsert_approval(decided.to_dict())
    return status in {ApprovalStatus.APPROVED, ApprovalStatus.AUTO_APPROVED}


async def _assistant_event_emitter(event_name: str, payload: dict[str, Any]) -> None:
    """Coordinator からの実行イベントを追跡・永続化する."""
    await _broadcast_execution_event(event_name, payload)
    run_id = str(payload.get("run_id", ""))
    step_id = str(payload.get("step_id", ""))

    if event_name == "RunStarted":
        started_at = datetime.now(UTC).timestamp()
        _run_started_at[run_id] = started_at
        await _store.upsert_run_record(
            {
                "run_id": run_id,
                "flow_id": "assistant",
                "thread_id": str(payload.get("user_id", "default")),
                "trace_id": run_id,
                "tenant_id": None,
                "status": "running",
                "started_at": started_at,
                "completed_at": None,
                "metrics": {},
            }
        )
        return

    if event_name == "StepStarted":
        started_event = await _execution_tracker.start_execution(
            skill_name=str(payload.get("skill_name", "unknown")),
            params=payload.get("params", {}) if isinstance(payload.get("params"), dict) else {},
            user_id=str(payload.get("user_id", "assistant")),
            metadata={"run_id": run_id, "step_id": step_id},
        )
        _active_step_events[step_id] = started_event.id
        event_row = started_event.to_dict()
        event_row["run_id"] = run_id
        await _store.upsert_execution_event(event_row)
        return

    if event_name == "ToolExecuted":
        status_text = str(payload.get("status", "failed"))
        status = ExecutionStatus.SUCCESS if status_text == "success" else ExecutionStatus.FAILED
        event_id = _active_step_events.pop(step_id, None)
        artifacts = payload.get("artifacts", [])
        if isinstance(artifacts, list):
            for artifact in artifacts:
                artifact_payload = artifact if isinstance(artifact, dict) else {"value": artifact}
                await _store.add_artifact(
                    run_id=run_id,
                    step_id=step_id if step_id else None,
                    artifact_type=str(artifact_payload.get("type", "tool_artifact")),
                    location=str(artifact_payload.get("location")) if artifact_payload.get("location") else None,
                    payload=artifact_payload,
                )
        rollback_handle = payload.get("rollback_handle")
        if rollback_handle is not None:
            rollback_payload = rollback_handle if isinstance(rollback_handle, dict) else {"value": rollback_handle}
            await _store.add_artifact(
                run_id=run_id,
                step_id=step_id if step_id else None,
                artifact_type="rollback_handle",
                location=None,
                payload=rollback_payload,
            )
        if event_id is not None:
            completed_event = await _execution_tracker.complete_execution(
                event_id=event_id,
                status=status,
                result=payload,
                error=str(payload.get("error", "")) if payload.get("error") else None,
            )
            if completed_event is not None:
                completed_row = completed_event.to_dict()
                completed_row["run_id"] = run_id
                await _store.upsert_execution_event(completed_row)
        return

    if event_name == "EvidenceAdded":
        await _store.add_evidence(
            run_id=run_id,
            step_id=step_id if step_id else None,
            payload=payload,
        )
        return

    if event_name == "RunFinished":
        started_at = _run_started_at.pop(run_id, datetime.now(UTC).timestamp())
        completed_at = datetime.now(UTC).timestamp()
        metrics = {"duration_ms": max(completed_at - started_at, 0.0) * 1000}
        await _store.upsert_run_record(
            {
                "run_id": run_id,
                "flow_id": "assistant",
                "thread_id": str(payload.get("user_id", "default")),
                "trace_id": run_id,
                "tenant_id": None,
                "status": str(payload.get("status", "completed")),
                "started_at": started_at,
                "completed_at": completed_at,
                "metrics": metrics,
            }
        )


_skill_gateway.set_confirmation_handler(_confirmation_handler)
_approval_manager.on_request(_on_approval_request)
_approval_manager.on_decision(_on_approval_decision)

# Personal Assistant Coordinator（新機能）
assistant_config = _build_assistant_config()
assistant = PersonalAssistantCoordinator(
    config=assistant_config,
    skill_gateway=_skill_gateway,
    event_emitter=_assistant_event_emitter,
)


def _standard_event_names() -> list[str]:
    """標準イベント一覧を返す."""
    return [
        "RunStarted",
        "StepStarted",
        "ToolApprovalRequested",
        "ToolExecuted",
        "EvidenceAdded",
        "RunFinished",
    ]


async def _execute_skill_with_tracking(
    *,
    skill_name: str,
    params: dict[str, Any],
    user_id: str,
    dry_run: bool,
) -> dict[str, Any]:
    """管理 API からのスキル実行をトラッキング付きで実行する."""
    run_id = f"run_{uuid.uuid4().hex}"
    step_id = f"step_{uuid.uuid4().hex}"
    await _assistant_event_emitter(
        "RunStarted",
        {
            "run_id": run_id,
            "user_id": user_id,
            "message": f"skill:{skill_name}",
        },
    )
    await _assistant_event_emitter(
        "StepStarted",
        {
            "run_id": run_id,
            "step_id": step_id,
            "skill_name": skill_name,
            "params": params,
            "user_id": user_id,
        },
    )
    try:
        result = await _skill_gateway.call(skill_name, params, dry_run=dry_run)
        normalized_cost = result.cost or {"duration_ms": result.duration_ms, "token_estimate": 0}
        await _assistant_event_emitter(
            "ToolExecuted",
            {
                "run_id": run_id,
                "step_id": step_id,
                "skill_name": skill_name,
                "status": "success" if result.success else "failed",
                "duration_ms": result.duration_ms,
                "error": result.error,
                "cost": normalized_cost,
                "risk_flags": result.risk_flags,
                "artifacts": result.artifacts,
                "rollback_handle": result.rollback_handle,
            },
        )
        if result.evidence:
            await _assistant_event_emitter(
                "EvidenceAdded",
                {
                    "run_id": run_id,
                    "step_id": step_id,
                    "skill_name": skill_name,
                    "count": len(result.evidence),
                },
            )
        await _assistant_event_emitter(
            "RunFinished",
            {
                "run_id": run_id,
                "user_id": user_id,
                "status": "completed" if result.success else "failed",
            },
        )
        payload = result.to_dict()
        payload["run_id"] = run_id
        payload["step_id"] = step_id
        return payload
    except Exception as e:
        await _assistant_event_emitter(
            "ToolExecuted",
            {
                "run_id": run_id,
                "step_id": step_id,
                "skill_name": skill_name,
                "status": "error",
                "error": str(e),
                "cost": {"duration_ms": 0.0, "token_estimate": 0},
                "risk_flags": ["execution_error"],
            },
        )
        await _assistant_event_emitter(
            "RunFinished",
            {
                "run_id": run_id,
                "user_id": user_id,
                "status": "failed",
                "error": str(e),
            },
        )
        raise


# =========================================================================
# 生命周期管理
# =========================================================================


@asynccontextmanager
async def lifespan(app: FastAPI) -> Any:
    """应用生命周期管理."""
    logger.info("Starting Messaging Hub...")

    # 1. 永続化層の初期化と復元
    await _store.initialize()
    approval_rows = await _store.list_approvals(limit=5000)
    pending_requests: list[ApprovalRequest] = []
    history_requests: list[ApprovalRequest] = []
    for row in approval_rows:
        request = ApprovalRequest.from_dict(row)
        if request.status == ApprovalStatus.PENDING:
            pending_requests.append(request)
        else:
            history_requests.append(request)
    _approval_manager.restore_requests(pending=pending_requests, history=history_requests)

    execution_rows = await _store.list_execution_events(limit=5000)
    restored_events = [ExecutionEvent.from_dict(row) for row in execution_rows]
    _execution_tracker.restore_events(restored_events)

    # 2. 注册平台适配器
    await setup_platforms()

    # 3. 启动后台任务（Discord bot）
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
        whatsapp = WhatsAppAdapter(phone_number_id=whatsapp_phone_id, access_token=whatsapp_token)
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


@app.middleware("http")
async def auth_middleware(request: Request, call_next: Any) -> Any:
    """Apply app-level auth contract to HTTP requests."""
    return await _auth_guard.http_middleware(request, call_next)


# =========================================================================
# WebSocket 端点
# =========================================================================


@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket) -> None:
    """WebSocket 连接端点.

    前端可以通过此端点接收实时消息更新。
    """
    if not await _require_ws_api_key(websocket):
        return

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
            response = await teams_adapter.handle_webhook(activity_data, auth_header, gateway)
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
        "security_mode": _get_security_mode(),
        "platforms": gateway.list_channels(),
    }


@app.get("/health")
async def health() -> dict[str, Any]:
    """健康检查."""
    stats = gateway.get_statistics()
    return {
        "status": "healthy",
        "security_mode": _get_security_mode(),
        "statistics": stats,
    }


@app.get("/api/a2a/card")
async def get_a2a_card() -> dict[str, Any]:
    """A2A AgentCard 相当の情報を返す."""
    return {
        "name": "messaging-hub-coordinator",
        "description": "マルチチャネル入力を専門Agentへ振り分ける coordinator",
        "version": "1.0.0",
        "protocol": "a2a",
        "skills": [
            {"name": "intent_routing", "description": "要求に応じた Agent 振り分け"},
            {"name": "meeting_support", "description": "会議要約・アクション抽出"},
            {"name": "file_organize", "description": "ファイル分類と整理提案"},
        ],
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
                platforms.append(
                    {
                        "name": platform_name,
                        "bot_info": bot_info,
                    }
                )
            except Exception as e:
                platforms.append(
                    {
                        "name": platform_name,
                        "error": str(e),
                    }
                )

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
        logger.exception(f"Failed to send message: {e}")
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
    stats = gateway.get_statistics()
    return {
        "status": "healthy",
        "security_mode": _get_security_mode(),
        "uptime": max(time.time() - _process_started_at, 0.0),
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
                messages.append(
                    {
                        "timestamp": msg.get("timestamp", ""),
                        "platform": session.get("platform", "unknown"),
                        "user_id": session.get("user_id", ""),
                        "user_name": session.get("user_name", "Unknown"),
                        "role": msg.get("role", "user"),
                        "content": msg.get("content", ""),
                    }
                )

        # エクスポート
        exporter = ConversationExportSkill()
        export_format = ExportFormat(format) if format in ["json", "csv", "markdown"] else ExportFormat.JSON
        result = await exporter.export(messages, format=export_format)

        return JSONResponse({"data": result})

    except Exception as e:
        logger.error(f"Export failed: {e}", exc_info=True)
        return JSONResponse({"error": str(e)}, status_code=500)


# =========================================================================
# 承認・実行・スキル管理 API
# =========================================================================


class ApprovalDecisionRequest(BaseModel):
    """承認実行リクエスト."""

    approver_id: str = Field(default="admin", min_length=1)


class ApprovalRejectRequest(BaseModel):
    """拒否実行リクエスト."""

    rejecter_id: str = Field(default="admin", min_length=1)
    reason: str = Field(default="")


class SkillGenerateRequest(BaseModel):
    """スキル生成リクエスト."""

    description: str = Field(..., min_length=1)
    examples: list[str] = Field(default_factory=list)


class SkillCallRequest(BaseModel):
    """スキル呼び出しリクエスト."""

    params: dict[str, Any] = Field(default_factory=dict)
    dry_run: bool = Field(default=False)
    user_id: str = Field(default="admin")


@app.get("/api/approvals/pending")
async def api_pending_approvals() -> dict[str, Any]:
    """保留中の承認一覧."""
    pending = _approval_manager.list_pending()
    return {
        "requests": [request.to_dict() for request in pending],
        "total": len(pending),
    }


@app.get("/api/approvals/history")
async def api_approval_history(
    limit: int = 100,
    offset: int = 0,
    status: str | None = None,
) -> dict[str, Any]:
    """承認履歴."""
    status_filter: ApprovalStatus | None = None
    if status:
        try:
            status_filter = ApprovalStatus(status)
        except ValueError:
            status_filter = None
    history = _approval_manager.list_history(limit=limit, offset=offset, status_filter=status_filter)
    return {
        "requests": [request.to_dict() for request in history],
        "total": len(history),
    }


@app.get("/api/approvals/stats")
async def api_approval_stats() -> dict[str, Any]:
    """承認統計."""
    return _approval_manager.get_statistics()


@app.post("/api/approvals/{request_id}/approve")
async def api_approve_request(request_id: str, payload: ApprovalDecisionRequest) -> dict[str, Any]:
    """承認を実行."""
    success = await _approval_manager.approve(request_id=request_id, approver_id=payload.approver_id)
    request = _approval_manager.find_request(request_id)
    if request is not None:
        await _store.upsert_approval(request.to_dict())
    return {"ok": success}


@app.post("/api/approvals/{request_id}/reject")
async def api_reject_request(request_id: str, payload: ApprovalRejectRequest) -> dict[str, Any]:
    """拒否を実行."""
    success = await _approval_manager.reject(
        request_id=request_id,
        rejecter_id=payload.rejecter_id,
        reason=payload.reason,
    )
    request = _approval_manager.find_request(request_id)
    if request is not None:
        await _store.upsert_approval(request.to_dict())
    return {"ok": success}


@app.get("/api/executions")
async def api_executions(
    status: str | None = None,
    skill: str | None = None,
    date: str | None = None,
    limit: int = 200,
) -> dict[str, Any]:
    """実行イベント一覧."""
    events = await _store.list_execution_events(
        status=status,
        skill_name=skill,
        started_date=date,
        limit=limit,
    )
    return {"events": events, "total": len(events)}


@app.get("/api/executions/stats")
async def api_execution_stats() -> dict[str, Any]:
    """実行統計."""
    stats = await _execution_tracker.get_statistics()
    return stats.to_dict()


@app.get("/api/skills")
async def api_skills() -> dict[str, Any]:
    """スキル一覧."""
    skills = await _skills_manager.list_available_skills()
    return {"skills": [skill.to_dict() for skill in skills], "total": len(skills)}


@app.post("/api/skills/{skill_name}/enable")
async def api_enable_skill(skill_name: str) -> dict[str, Any]:
    """スキル有効化."""
    return {"ok": _skills_manager.enable_skill(skill_name)}


@app.post("/api/skills/{skill_name}/disable")
async def api_disable_skill(skill_name: str) -> dict[str, Any]:
    """スキル無効化."""
    return {"ok": _skills_manager.disable_skill(skill_name)}


@app.post("/api/skills/generate")
async def api_generate_skill(request: SkillGenerateRequest) -> dict[str, Any]:
    """自然言語からスキルを生成."""
    return await _skills_manager.generate_skill_from_description(
        description=request.description,
        examples=request.examples,
    )


@app.post("/api/skills/{skill_name}/call")
async def api_call_skill(skill_name: str, request: SkillCallRequest) -> dict[str, Any]:
    """スキルを呼び出す."""
    return await _execute_skill_with_tracking(
        skill_name=skill_name,
        params=request.params,
        user_id=request.user_id,
        dry_run=request.dry_run,
    )


@app.get("/api/workflows")
async def api_workflows(
    status: str | None = None,
) -> dict[str, Any]:
    """ワークフロー一覧."""
    status_filter: WorkflowStatus | None = None
    if status:
        try:
            status_filter = WorkflowStatus(status)
        except ValueError:
            status_filter = None
    workflows: list[Workflow] = _skills_manager.list_workflows(status_filter=status_filter)
    return {"workflows": [workflow.to_dict() for workflow in workflows], "total": len(workflows)}


# =========================================================================
# sr_chat API
# =========================================================================


class SRChatPostRequest(BaseModel):
    """sr_chat 投稿リクエスト."""

    text: str = Field(..., min_length=1)
    user_id: str = Field(default="default", min_length=1)
    conversation_id: str = Field(default="chat:default", min_length=1)
    platform: str | None = None
    channel_id: str | None = None


class SRChatUpdateRequest(BaseModel):
    """sr_chat 更新リクエスト."""

    message_id: str = Field(..., min_length=1)
    conversation_id: str = Field(default="chat:default", min_length=1)
    text: str = Field(..., min_length=1)


class SRFileUploadRequest(BaseModel):
    """sr_chat ファイルアップロードリクエスト."""

    conversation_id: str = Field(default="chat:default", min_length=1)
    file_name: str = Field(..., min_length=1)
    content_base64: str = Field(..., min_length=1)
    mime_type: str = Field(default="application/octet-stream")
    user_id: str = Field(default="default", min_length=1)


class SREventSubscribeRequest(BaseModel):
    """sr_chat イベント購読リクエスト."""

    client_id: str = Field(..., min_length=1)
    conversation_id: str | None = None
    event_types: list[str] = Field(default_factory=_standard_event_names)


@app.post("/api/sr_chat/auth.test")
async def sr_chat_auth_test() -> dict[str, Any]:
    """認証状態の確認."""
    return {
        "ok": True,
        "service": "messaging_hub",
        "requires_auth": _is_auth_required(),
        "user": "authenticated" if _is_auth_required() else "anonymous",
    }


@app.get("/api/sr_chat/conversations.list")
async def sr_chat_conversations_list(limit: int = 100) -> dict[str, Any]:
    """会話一覧."""
    items = await _store.list_sr_conversations(limit=limit)
    return {"ok": True, "conversations": items, "total": len(items)}


@app.get("/api/sr_chat/conversations.history")
async def sr_chat_conversations_history(
    conversation_id: str = "chat:default",
    limit: int = 100,
) -> dict[str, Any]:
    """会話履歴."""
    items = await _store.list_sr_messages(conversation_id=conversation_id, limit=limit)
    return {"ok": True, "conversation_id": conversation_id, "messages": items, "total": len(items)}


@app.post("/api/sr_chat/chat.postMessage")
async def sr_chat_post_message(request: SRChatPostRequest) -> dict[str, Any]:
    """メッセージを投稿してアシスタント応答を生成."""
    run_id = f"run_{uuid.uuid4().hex}"
    user_message_id = f"msg_{uuid.uuid4().hex}"
    assistant_message_id = f"msg_{uuid.uuid4().hex}"
    now_iso = datetime.now(UTC).isoformat()

    await _store.upsert_sr_message(
        {
            "message_id": user_message_id,
            "conversation_id": request.conversation_id,
            "role": "user",
            "content": request.text,
            "created_at": now_iso,
            "updated_at": now_iso,
            "metadata": {"user_id": request.user_id},
        }
    )
    result = await assistant.process(
        request.text,
        user_id=request.user_id,
        context={"run_id": run_id, "conversation_id": request.conversation_id},
    )
    assistant_text = str(result.get("summary", ""))
    response_time = datetime.now(UTC).isoformat()
    await _store.upsert_sr_message(
        {
            "message_id": assistant_message_id,
            "conversation_id": request.conversation_id,
            "role": "assistant",
            "content": assistant_text,
            "created_at": response_time,
            "updated_at": response_time,
            "metadata": {"run_id": run_id, "intent": result.get("intent", {})},
        }
    )

    delivery_error: str | None = None
    if request.platform and request.channel_id:
        try:
            await gateway.send_message_to_platform(
                platform=request.platform,
                channel_id=request.channel_id,
                text=assistant_text,
            )
        except Exception as exc:
            delivery_error = str(exc)

    return {
        "ok": True,
        "conversation_id": request.conversation_id,
        "run_id": run_id,
        "message": {
            "id": assistant_message_id,
            "role": "assistant",
            "text": assistant_text,
        },
        "intent": result.get("intent", {}),
        "delivery_error": delivery_error,
    }


@app.post("/api/sr_chat/chat.update")
async def sr_chat_update_message(request: SRChatUpdateRequest) -> dict[str, Any]:
    """既存メッセージを更新."""
    now_iso = datetime.now(UTC).isoformat()
    await _store.upsert_sr_message(
        {
            "message_id": request.message_id,
            "conversation_id": request.conversation_id,
            "role": "assistant",
            "content": request.text,
            "created_at": now_iso,
            "updated_at": now_iso,
            "metadata": {"updated": True},
        }
    )
    return {"ok": True, "message_id": request.message_id, "updated_at": now_iso}


@app.post("/api/sr_chat/files.upload")
async def sr_chat_files_upload(request: SRFileUploadRequest) -> dict[str, Any]:
    """ファイルアップロード情報を記録."""
    run_id = f"file_{uuid.uuid4().hex}"
    await _store.add_artifact(
        run_id=run_id,
        step_id=None,
        artifact_type="file_upload",
        location=request.file_name,
        payload={
            "conversation_id": request.conversation_id,
            "mime_type": request.mime_type,
            "content_base64": request.content_base64,
            "user_id": request.user_id,
        },
    )
    return {"ok": True, "run_id": run_id, "file_name": request.file_name}


@app.post("/api/sr_chat/events.subscribe")
async def sr_chat_events_subscribe(
    payload: SREventSubscribeRequest,
    request: Request,
) -> dict[str, Any]:
    """イベント購読情報を登録."""
    subscription_id = str(uuid.uuid4())
    await _store.upsert_subscription(
        {
            "subscription_id": subscription_id,
            "client_id": payload.client_id,
            "conversation_id": payload.conversation_id,
            "event_types": payload.event_types,
            "created_at": datetime.now(UTC).isoformat(),
        }
    )
    ws_scheme = "wss" if request.url.scheme == "https" else "ws"
    port = f":{request.url.port}" if request.url.port else ""
    ws_url = f"{ws_scheme}://{request.url.hostname}{port}/ws?{urlencode({'client_id': payload.client_id})}"
    return {
        "ok": True,
        "subscription_id": subscription_id,
        "events": payload.event_types,
        "ws_url": ws_url,
    }


# =========================================================================
# Personal Assistant エンドポイント（新機能）
# =========================================================================


class AssistantProcessRequest(BaseModel):
    """Assistant 処理リクエスト."""

    message: str = Field(..., min_length=1)
    user_id: str = Field(default="default", min_length=1)


class AssistantCLIExecuteRequest(BaseModel):
    """CLI 提案実行リクエスト."""

    proposal_id: str = Field(..., min_length=1)
    confirm: bool = Field(default=False)


@app.post("/assistant/process")
async def process_assistant_request(
    request: AssistantProcessRequest,
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
        run_id = f"run_{uuid.uuid4().hex}"
        result = await assistant.process(
            request.message,
            user_id=request.user_id,
            context={"run_id": run_id},
        )
        response: dict[str, Any] = {
            "ok": True,
            "summary": result.get("summary", ""),
            "headline": result.get("headline", ""),
            "key_points": result.get("key_points", []),
            "actions": result.get("actions", []),
            "risks": result.get("risks", []),
            "intent": result.get("intent", {}),
            "run_id": result.get("run_id", run_id),
        }
        if result.get("needs_cli_confirmation") is True:
            proposal = result.get("cli_proposal")
            if isinstance(proposal, dict):
                proposal_id = str(proposal.get("proposal_id", "")).strip()
                if proposal_id:
                    _assistant_cli_proposals[proposal_id] = proposal
                response["needs_cli_confirmation"] = True
                response["cli_proposal"] = proposal
        return response
    except Exception as e:
        logger.error("Assistant processing error: %s", e, exc_info=True)
        return {
            "ok": False,
            "error": str(e),
            "summary": f"❌ 処理エラー: {e}",
        }


@app.post("/assistant/cli/execute")
async def execute_assistant_cli_proposal(request: AssistantCLIExecuteRequest) -> dict[str, Any]:
    """承認済み CLI 提案を実行する."""
    proposal = _assistant_cli_proposals.get(request.proposal_id)
    if proposal is None:
        return {
            "ok": False,
            "error": "proposal_not_found",
            "summary": "指定された提案が見つかりません",
        }
    if request.confirm is not True:
        return {
            "ok": False,
            "error": "confirmation_required",
            "summary": "CLI 実行には confirm=true が必要です",
        }

    prompt = str(proposal.get("prompt", "")).strip()
    if not prompt:
        return {
            "ok": False,
            "error": "proposal_prompt_missing",
            "summary": "提案に実行可能なプロンプトがありません",
        }

    diagnostic = await _cli_runtime.run_diagnostic_prompt(
        prompt=prompt,
        mode_override="read_only",
    )
    output = "\n".join(
        text
        for text in (diagnostic.get("stdout", ""), diagnostic.get("stderr", ""))
        if isinstance(text, str) and text.strip()
    ).strip()

    return {
        "ok": bool(diagnostic.get("success")),
        "proposal_id": request.proposal_id,
        "tool": diagnostic.get("tool"),
        "command": diagnostic.get("command"),
        "summary": output.splitlines()[0] if output else "CLI diagnostics executed",
        "recommendation": "Review diagnostic output and apply fixes manually.",
        "raw_output": output[-5000:] if output else "",
        "error": diagnostic.get("error"),
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
    import argparse

    import uvicorn

    parser = argparse.ArgumentParser(description="Messaging Hub - Multi-Platform Chatbot")
    parser.add_argument(
        "--reload",
        action="store_true",
        help="開発モード（ホットリロード有効）",
    )
    parser.add_argument(
        "--host",
        default=None,
        help="ホスト（省略時: 環境変数 MSGHUB_HOST / デフォルト 0.0.0.0）",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=None,
        help="ポート（省略時: 環境変数 MSGHUB_PORT / app_config.json）",
    )
    args = parser.parse_args()

    # 检查环境变量
    if not any(
        [
            os.getenv("TELEGRAM_BOT_TOKEN"),
            os.getenv("SLACK_BOT_TOKEN"),
            os.getenv("DISCORD_BOT_TOKEN"),
        ]
    ):
        logger.warning(
            "⚠️  No platform tokens configured! Set TELEGRAM_BOT_TOKEN, SLACK_BOT_TOKEN, or DISCORD_BOT_TOKEN"
        )

    # 检查 LLM provider
    try:
        llm = get_llm()
        logger.info("✓ LLM Provider initialized")
    except Exception as e:
        logger.exception("✗ Failed to initialize LLM: %s", e)
        logger.exception("Please set OPENAI_API_KEY or other LLM provider keys")

    # 启动服务
    config_path = Path(__file__).resolve().parent / "app_config.json"
    config_raw: dict = {}
    if config_path.is_file():
        try:
            config_raw = json.loads(config_path.read_text("utf-8"))
        except json.JSONDecodeError:
            config_raw = {}

    _default_port = config_raw.get("ports", {}).get("api", 8004)
    _host = args.host or os.getenv("MSGHUB_HOST", "0.0.0.0")
    _port = args.port or int(os.getenv("MSGHUB_PORT", str(_default_port)))

    print(f"[Messaging Hub] Starting on {_host}:{_port} (reload={args.reload})")

    if args.reload:
        uvicorn.run(
            "apps.messaging_hub.main:app",
            host=_host,
            port=_port,
            reload=True,
            reload_dirs=["apps/messaging_hub", "agentflow"],
            log_level="info",
        )
    else:
        uvicorn.run(app, host=_host, port=_port, log_level="info")
