#!/usr/bin/env python3
"""Messaging Hub の FastAPI エントリポイント。

複数のメッセージプラットフォームを単一ゲートウェイに統合し、
Telegram・Slack・Discord などを横断して扱う。

主な特徴:
- マルチプラットフォームのメッセージルーティング
- 統一セッション管理
- WebSocket によるリアルタイム同期
- AI Agent / Skill の統合
- リッチ UI（Live Canvas）

アーキテクチャ:
    Message Platforms → Gateway → ChatBot → Agent/Coordinator
                            ↓
                      WebSocket Hub → Frontend

起動方法:
    # 開発モード
    conda run -n agentflow python apps/messaging_hub/scripts/dev.py --reload

    # Docker / 発布
    conda run -n agentflow python apps/messaging_hub/scripts/compose.py publish

環境変数:
    TELEGRAM_BOT_TOKEN: Telegram Bot Token
    SLACK_BOT_TOKEN: Slack Bot Token
    SLACK_SIGNING_SECRET: Slack Signing Secret
    DISCORD_BOT_TOKEN: Discord Bot Token
    MSGHUB_HOST: backend bind host override（未指定時は app_config.json）
    MSGHUB_PORT: backend bind port override（未指定時は app_config.json）
    OPENAI_API_KEY: OpenAI API Key（または他の LLM provider）
"""

from __future__ import annotations

import asyncio
import logging
import os
import re
import time
import uuid
from contextlib import asynccontextmanager, suppress
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any
from urllib.parse import urlencode

from dotenv import load_dotenv
from fastapi import FastAPI, Request, WebSocket, WebSocketDisconnect
from fastapi.responses import JSONResponse, PlainTextResponse
from pydantic import BaseModel, Field

from apps.messaging_hub.agents.file_organizer_agent import FileOrganizerAgent
from apps.messaging_hub.agents.flight_watch_agent import FlightWatchAgent
from apps.messaging_hub.agents.runtime_artifact_agent import RuntimeArtifactAgentRegistry
from apps.messaging_hub.approval_manager import ApprovalManager, ApprovalRequest, ApprovalStatus
from apps.messaging_hub.coordinator import AssistantConfig, PersonalAssistantCoordinator
from apps.messaging_hub.execution_substrate import ExecutionSubstrateService, SQLiteExecutionSubstrateStore
from apps.messaging_hub.execution_tracker import ExecutionEvent, ExecutionStatus, ExecutionTracker
from apps.messaging_hub.flight_watch import FlightSearchRequest, FlightWatchService, SubscriptionStatus
from apps.messaging_hub.generated_artifact_manager import ArtifactType, GeneratedArtifactManager
from apps.messaging_hub.harness_memory import HarnessMemoryService
from apps.messaging_hub.lazy_tool_loader import LazyToolLoader
from apps.messaging_hub.mcp_manager import MCPInstallRequest, MCPManager
from apps.messaging_hub.orchestration_service import OrchestrationService, OrchestrationTaskResult
from apps.messaging_hub.skills_manager import SkillsManager, Workflow, WorkflowStatus
from apps.messaging_hub.storage import SQLiteMessagingHubStore
from harness.budget.service import BudgetConfig, TokenBudgetManager
from harness.gating.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig
from harness.scoring.service import DimensionScore, ExecutionScorer, ScoreDimension, ScoringResult
from kernel.protocols.a2ui.components import CardComponent, TextComponent
from kernel.protocols.agui_events import A2UIComponentEvent, ApprovalRequiredEvent
from kernel.runtime import WebSocketHub
from kernel.skills import (
    ChatBotSkill,
    ConversationExportSkill,
    ExportFormat,
    RiskLevel,
    create_skill_gateway,
)
from kernel.tools.cli.runtime_manager import CLIRuntimeManager
from infrastructure.observability.startup import log_startup_info
from shared.channels import (
    DiscordAdapter,
    MessageGateway,
    SignalAdapter,
    SlackAdapter,
    TeamsAdapter,
    TelegramAdapter,
    WhatsAppAdapter,
)


if TYPE_CHECKING:
    from kernel.skills.conversation_export import ExportMessage
    from shared.channels.base import MessageChannelAdapter


# ログ設定
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)
logger = logging.getLogger("messaging_hub")


# =========================================================================
# グローバルインスタンス
# =========================================================================

# WebSocket Hub
hub = WebSocketHub()

# ChatBot Skill（既存実装を再利用し、assistant と連携）
chatbot = ChatBotSkill(
    # coordinator や rag_skill は必要に応じて追加する
    temperature=0.7,
)

# Message Gateway
gateway = MessageGateway(hub, chatbot)

# バックグラウンドタスク一覧
background_tasks: list[asyncio.Task[None]] = []
_cli_runtime = CLIRuntimeManager()
_assistant_cli_proposals: dict[str, dict[str, Any]] = {}

_APP_CONFIG_PATH = Path(__file__).resolve().parent / "app_config.json"
_LOCAL_ENV_PATH = Path(__file__).resolve().parent / ".env"
_PUBLIC_PATHS = {
    "/",
    "/health",
    "/api/health",
    "/api/admin-key",
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

# Token 予算管理: メッセージ履歴・入力のコンテキスト予算を管理
_budget_manager = TokenBudgetManager(
    config=BudgetConfig(
        system_prompt_budget=500,
        history_budget=3000,  # メッセージング用に会話履歴予算を確保
        total_budget=8000,
    )
)

# 実行品質スコアラー: アシスタント応答を多次元評価
_scorer = ExecutionScorer()


def _load_local_env_file() -> bool:
    """Load local .env file without overriding existing environment variables."""
    if not _LOCAL_ENV_PATH.exists():
        return False
    loaded = load_dotenv(dotenv_path=_LOCAL_ENV_PATH, override=False)
    if loaded:
        logger.info("Loaded local env file: %s", _LOCAL_ENV_PATH)
    return loaded


def _auth_api_key_env_name() -> str:
    """Resolve the API key env var name from selector/default configuration."""
    selected = os.getenv("MESSAGING_HUB_API_KEY_ENV", "").strip()
    if selected:
        return selected
    return "MESSAGING_HUB_API_KEY"


def _is_auth_key_configured() -> bool:
    """Return whether required API key env variable is configured."""
    if not _is_auth_required():
        return True
    return bool(os.getenv(_auth_api_key_env_name(), "").strip())


_load_local_env_file()


def _load_app_config() -> dict[str, Any]:
    """Load app_config.json or return an empty dict."""
    return _auth_guard.load_app_config()


def _log_startup_summary() -> None:
    """統一 startup summary を出力する."""
    app_config = _load_app_config()
    log_startup_info(
        app_name=str(app_config.get("display_name") or "Messaging Hub"),
        app_config_path=_APP_CONFIG_PATH,
        runtime_overrides={
            "db": {
                "backend": "sqlite",
                "url": f"sqlite:///{_store.db_path}",
            },
            "vectordb": {
                "backend": "",
                "path": "",
                "collection": "",
                "index": "",
            },
        },
        extra_info={
            "version": str(app_config.get("version") or "1.0.0"),
        },
    )


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
_harness_memory_service = HarnessMemoryService()

_approval_manager = ApprovalManager(websocket_hub=hub)
_execution_tracker = ExecutionTracker(websocket_hub=hub)
_skills_manager = SkillsManager(gateway=_skill_gateway, websocket_hub=hub)
_mcp_manager = MCPManager()
_lazy_tool_loader = LazyToolLoader(
    mcp_manager=_mcp_manager,
    skills_manager=_skills_manager,
)
_file_organizer_agent = FileOrganizerAgent(gateway=_skill_gateway)

# ビジネスアドバイザーエージェント
from apps.messaging_hub.agents.business_advisor_agent import BusinessAdvisorAgent


_business_advisor_agent = BusinessAdvisorAgent(gateway=_skill_gateway)

# A2AHub に Agent を登録
from kernel.protocols.a2a_hub import get_hub as _get_a2a_hub


_a2a_hub = _get_a2a_hub()


def _register_runtime_agent(agent_instance: Any) -> None:
    """A2A hub に runtime agent を登録する."""
    if _a2a_hub.discover(agent_instance.name) is None:
        _a2a_hub.register(agent_instance)
        return
    _a2a_hub.register(agent_instance, replace=True)


_register_runtime_agent(_file_organizer_agent)
_register_runtime_agent(_business_advisor_agent)

_active_step_events: dict[str, str] = {}
_run_started_at: dict[str, float] = {}
_process_started_at = time.time()
_platform_runtime_tasks: dict[str, asyncio.Task[None]] = {}
_LOCAL_PLATFORM_ID = "bizcore_nexus"
_EXTERNAL_PLATFORM_NAMES: tuple[str, ...] = (
    "telegram",
    "slack",
    "discord",
    "teams",
    "whatsapp",
    "signal",
)
_PLATFORM_ORDER: tuple[str, ...] = (_LOCAL_PLATFORM_ID, *_EXTERNAL_PLATFORM_NAMES)
_MAX_SECRET_LENGTH = 512
_MAX_CHAT_INPUT_CHARS = 4000
_DANGEROUS_INPUT_PATTERNS: tuple[tuple[re.Pattern[str], str], ...] = (
    (re.compile(r"\brm\s+-rf\b", re.IGNORECASE), "危険な削除コマンドが検出されました"),
    (re.compile(r"\b(?:sudo\s+)?(shutdown|reboot|halt)\b", re.IGNORECASE), "システム停止コマンドは許可されません"),
    (re.compile(r"\bcurl\b[^\\n]*\|\s*(bash|sh)\b", re.IGNORECASE), "パイプ実行コマンドは許可されません"),
    (re.compile(r"\bwget\b[^\\n]*\|\s*(bash|sh)\b", re.IGNORECASE), "パイプ実行コマンドは許可されません"),
    (
        re.compile(r"\b(powershell|pwsh)\b[^\\n]*(encodedcommand|-enc)\b", re.IGNORECASE),
        "エンコード済み PowerShell 実行は許可されません",
    ),
    (re.compile(r"\bdd\s+if=", re.IGNORECASE), "ディスク破壊につながるコマンドは許可されません"),
    (re.compile(r"\bmkfs(\.|\\s)", re.IGNORECASE), "ファイルシステム初期化コマンドは許可されません"),
)
_PLATFORM_CATALOG: dict[str, dict[str, Any]] = {
    _LOCAL_PLATFORM_ID: {
        "display_name": "AgentFlow Sovereign Nexus",
        "icon": "🛡️",
        "description": "Messaging Hub 内蔵の統合チャネル。監査と承認フローを標準装備。",
        "auth_mode": "managed",
        "auth_url": None,
        "docs_url": None,
        "credential_fields": [],
    },
    "telegram": {
        "display_name": "Telegram Command Cloud",
        "icon": "📱",
        "description": "BotFather で Bot を発行し、Webhook で接続します。",
        "auth_mode": "oauth_and_api_key",
        "auth_url": "https://t.me/BotFather",
        "docs_url": "https://core.telegram.org/bots",
        "credential_fields": [
            {
                "key": "bot_token",
                "label": "Bot Token",
                "env_var": "TELEGRAM_BOT_TOKEN",
                "required": True,
                "placeholder": "123456789:AA...",
            }
        ],
    },
    "slack": {
        "display_name": "Slack Enterprise Bridge",
        "icon": "💼",
        "description": "Slack App を作成して Bot Token と Signing Secret を設定します。",
        "auth_mode": "oauth_and_api_key",
        "auth_url": "https://api.slack.com/apps",
        "docs_url": "https://api.slack.com/authentication",
        "credential_fields": [
            {
                "key": "bot_token",
                "label": "Bot Token",
                "env_var": "SLACK_BOT_TOKEN",
                "required": True,
                "placeholder": "xoxb-...",
            },
            {
                "key": "signing_secret",
                "label": "Signing Secret",
                "env_var": "SLACK_SIGNING_SECRET",
                "required": False,
                "placeholder": "Slack Signing Secret",
            },
        ],
    },
    "discord": {
        "display_name": "Discord Ops Relay",
        "icon": "🎮",
        "description": "Discord Developer Portal で Bot Token を発行して接続します。",
        "auth_mode": "api_key",
        "auth_url": "https://discord.com/developers/applications",
        "docs_url": "https://discord.com/developers/docs/quick-start/getting-started",
        "credential_fields": [
            {
                "key": "bot_token",
                "label": "Bot Token",
                "env_var": "DISCORD_BOT_TOKEN",
                "required": True,
                "placeholder": "Discord Bot Token",
            }
        ],
    },
    "teams": {
        "display_name": "Microsoft Teams Secure Link",
        "icon": "🏢",
        "description": "Azure Bot の App ID / Password を設定して接続します。",
        "auth_mode": "oauth_and_api_key",
        "auth_url": "https://portal.azure.com/",
        "docs_url": "https://learn.microsoft.com/azure/bot-service/bot-service-overview",
        "credential_fields": [
            {
                "key": "app_id",
                "label": "App ID",
                "env_var": "TEAMS_APP_ID",
                "required": True,
                "placeholder": "Azure Bot App ID",
            },
            {
                "key": "app_password",
                "label": "App Password",
                "env_var": "TEAMS_APP_PASSWORD",
                "required": True,
                "placeholder": "Azure Bot App Password",
            },
        ],
    },
    "whatsapp": {
        "display_name": "WhatsApp Business Gateway",
        "icon": "💬",
        "description": "Meta Developer Console で Phone Number ID / Access Token を設定します。",
        "auth_mode": "oauth_and_api_key",
        "auth_url": "https://developers.facebook.com/apps/",
        "docs_url": "https://developers.facebook.com/docs/whatsapp/cloud-api",
        "credential_fields": [
            {
                "key": "phone_number_id",
                "label": "Phone Number ID",
                "env_var": "WHATSAPP_PHONE_NUMBER_ID",
                "required": True,
                "placeholder": "WhatsApp Phone Number ID",
            },
            {
                "key": "access_token",
                "label": "Access Token",
                "env_var": "WHATSAPP_ACCESS_TOKEN",
                "required": True,
                "placeholder": "Meta Access Token",
            },
            {
                "key": "verify_token",
                "label": "Verify Token",
                "env_var": "WHATSAPP_VERIFY_TOKEN",
                "required": False,
                "placeholder": "Webhook Verify Token",
            },
        ],
    },
    "signal": {
        "display_name": "Signal Private Channel",
        "icon": "🔒",
        "description": "signal-cli REST API と接続用番号を設定します。",
        "auth_mode": "api_key",
        "auth_url": "https://github.com/bbernhard/signal-cli-rest-api",
        "docs_url": "https://github.com/AsamK/signal-cli",
        "credential_fields": [
            {
                "key": "api_url",
                "label": "Signal API URL",
                "env_var": "SIGNAL_API_URL",
                "required": True,
                "placeholder": "http://localhost:8080",
            },
            {
                "key": "phone_number",
                "label": "Phone Number",
                "env_var": "SIGNAL_PHONE_NUMBER",
                "required": True,
                "placeholder": "+8190...",
            },
        ],
    },
}


def _mask_secret(secret: str) -> str:
    """Return masked secret for UI display."""
    value = secret.strip()
    if not value:
        return ""
    if len(value) <= 4:
        return "*" * len(value)
    return f"{'*' * (len(value) - 4)}{value[-4:]}"


def _platform_config(platform_name: str) -> dict[str, Any] | None:
    """Return platform catalog config."""
    return _PLATFORM_CATALOG.get(platform_name)


def _credential_specs(platform_name: str) -> list[dict[str, Any]]:
    """Return credential specs for platform."""
    cfg = _platform_config(platform_name)
    if cfg is None:
        return []
    raw_fields = cfg.get("credential_fields", [])
    if not isinstance(raw_fields, list):
        return []
    return [field for field in raw_fields if isinstance(field, dict)]


def _credential_env_name(platform_name: str, field_key: str) -> str | None:
    """Resolve env var from platform credential key."""
    for spec in _credential_specs(platform_name):
        if str(spec.get("key", "")).strip() == field_key:
            env_var = str(spec.get("env_var", "")).strip()
            return env_var or None
    return None


def _credential_status(platform_name: str) -> list[dict[str, Any]]:
    """Build credential status payload for UI."""
    result: list[dict[str, Any]] = []
    for spec in _credential_specs(platform_name):
        env_var = str(spec.get("env_var", "")).strip()
        key = str(spec.get("key", "")).strip()
        if not env_var or not key:
            continue
        raw_value = os.getenv(env_var, "").strip()
        result.append(
            {
                "key": key,
                "label": str(spec.get("label", key)),
                "required": bool(spec.get("required", True)),
                "placeholder": str(spec.get("placeholder", "")),
                "configured": bool(raw_value),
                "maskedValue": _mask_secret(raw_value) if raw_value else None,
            }
        )
    return result


def _validate_credential_value(platform_name: str, field_key: str, value: str) -> str | None:
    """Validate credential value before saving."""
    normalized = value.strip()
    if not normalized:
        return "空の値は保存できません"
    if len(normalized) > _MAX_SECRET_LENGTH:
        return "値が長すぎます"
    if any(ord(ch) < 32 for ch in normalized):
        return "制御文字を含む値は許可されません"

    if platform_name == "telegram" and field_key == "bot_token" and ":" not in normalized:
        return "Telegram Bot Token の形式が不正です"
    if platform_name == "slack" and field_key == "bot_token" and not normalized.startswith("xoxb-"):
        return "Slack Bot Token は xoxb- で始まる必要があります"
    if platform_name == "signal" and field_key == "api_url" and not normalized.startswith(("http://", "https://")):
        return "Signal API URL は http(s):// で始めてください"
    if platform_name == "teams" and field_key == "app_id" and len(normalized) < 8:
        return "Teams App ID が短すぎます"
    return None


def _missing_required_credentials(platform_name: str) -> list[str]:
    """Return missing required credential labels."""
    missing: list[str] = []
    for spec in _credential_specs(platform_name):
        if not bool(spec.get("required", True)):
            continue
        env_var = str(spec.get("env_var", "")).strip()
        if not env_var:
            continue
        if not os.getenv(env_var, "").strip():
            missing.append(str(spec.get("label", env_var)))
    return missing


def _is_platform_connected(platform_name: str, registered_channels: set[str]) -> bool:
    """Determine runtime platform connection status."""
    if platform_name == _LOCAL_PLATFORM_ID:
        return True
    return platform_name in registered_channels and gateway.get_channel(platform_name) is not None


def _build_platform_info(
    platform_name: str,
    *,
    stats: dict[str, Any],
    registered_channels: set[str],
) -> dict[str, Any]:
    """Build unified platform status payload for admin UI."""
    cfg = _platform_config(platform_name) or {}
    return {
        "name": platform_name,
        "displayName": str(cfg.get("display_name", platform_name)),
        "icon": str(cfg.get("icon", "📡")),
        "description": str(cfg.get("description", "")),
        "authMode": str(cfg.get("auth_mode", "api_key")),
        "authUrl": cfg.get("auth_url"),
        "docsUrl": cfg.get("docs_url"),
        "managed": platform_name == _LOCAL_PLATFORM_ID,
        "connected": _is_platform_connected(platform_name, registered_channels),
        "lastActivity": stats.get(f"{platform_name}_last_activity"),
        "messageCount": int(stats.get(f"{platform_name}_messages", 0)),
        "credentialFields": _credential_status(platform_name),
    }


def _detect_unsafe_input_reason(text: str) -> str | None:
    """Detect dangerous command-like input via deterministic rules."""
    normalized = text.strip()
    if not normalized:
        return "空の入力は処理できません"
    if len(normalized) > _MAX_CHAT_INPUT_CHARS:
        return f"入力が長すぎます（最大 {_MAX_CHAT_INPUT_CHARS} 文字）"
    for pattern, reason in _DANGEROUS_INPUT_PATTERNS:
        if pattern.search(normalized):
            return reason
    return None


def _non_empty_text(value: Any) -> str | None:
    """Return stripped text when value is a non-empty string."""
    if not isinstance(value, str):
        return None
    normalized = value.strip()
    return normalized or None


def _extract_text_fields(payload: dict[str, Any]) -> str | None:
    """Extract user-facing text from heterogeneous assistant payloads."""
    for key in ("answer", "content", "text", "message"):
        text = _non_empty_text(payload.get(key))
        if text is not None:
            return text
    return None


def _resolve_assistant_message_text(result: dict[str, Any]) -> str:
    """Resolve best assistant message for chat UI.

    `assistant.process()` returns a manager-oriented summary by default.
    For chat UX, prefer direct answer/content when available.
    """
    raw_results = result.get("raw_results")
    if isinstance(raw_results, dict):
        direct = _extract_text_fields(raw_results)
        if direct is not None:
            return direct
        nested_result = raw_results.get("result")
        if isinstance(nested_result, dict):
            nested = _extract_text_fields(nested_result)
            if nested is not None:
                return nested
        nested_error = _non_empty_text(raw_results.get("error"))
        if nested_error is not None:
            return f"⚠️ 応答生成に失敗しました: {nested_error[:240]}"

    top_level = _extract_text_fields(result)
    if top_level is not None:
        return top_level

    summary = _non_empty_text(result.get("summary"))
    if summary is not None:
        return summary

    generic_error = _non_empty_text(result.get("error"))
    if generic_error is not None:
        return f"⚠️ 応答生成に失敗しました: {generic_error[:240]}"
    return "⚠️ 応答を生成できませんでした。設定を確認して再試行してください。"


def _resolve_orchestration_message_text(result: OrchestrationTaskResult) -> str:
    """オーケストレーション結果からチャット表示文面を解決する."""
    payload = result.result if isinstance(result.result, dict) else None
    if payload is not None:
        if "raw_results" in payload or "summary" in payload or "answer" in payload:
            return _resolve_assistant_message_text(payload)
        nested_output = payload.get("output")
        if isinstance(nested_output, dict):
            nested_text = _extract_text_fields(nested_output)
            if nested_text is not None:
                return nested_text
            nested_summary = _non_empty_text(nested_output.get("summary"))
            if nested_summary is not None:
                return nested_summary
    return result.summary


def _score_assistant_response(
    *,
    input_text: str,
    response_text: str,
    elapsed_seconds: float,
    is_safe: bool = True,
) -> ScoringResult:
    """アシスタント応答を多次元スコアリングする.

    Args:
        input_text: ユーザー入力テキスト
        response_text: アシスタント応答テキスト
        elapsed_seconds: 処理時間（秒）
        is_safe: 安全チェック通過済みか

    Returns:
        ScoringResult（総合スコアと次元別スコア）
    """
    input_tokens = _budget_manager.count_tokens(input_text)
    response_tokens = _budget_manager.count_tokens(response_text)

    # 完全性スコア: 応答が非空で十分な長さか（50token以上で満点）
    completeness = min(1.0, response_tokens / 50.0) if response_tokens > 0 else 0.0

    # レイテンシスコア: 5秒以内で満点、30秒で0点
    latency_score = max(0.0, 1.0 - (elapsed_seconds / 30.0))

    # コストスコア: 入力+応答合計が予算内か（8000token以内で満点）
    total_tokens = input_tokens + response_tokens
    cost_score = max(0.0, 1.0 - (total_tokens / 8000.0))

    # 安全スコア
    safety_score = 1.0 if is_safe else 0.0

    dimension_scores = [
        DimensionScore(dimension=ScoreDimension.COMPLETENESS, score=completeness, weight=1.5),
        DimensionScore(dimension=ScoreDimension.LATENCY, score=latency_score, weight=1.0),
        DimensionScore(dimension=ScoreDimension.COST, score=cost_score, weight=0.5),
        DimensionScore(dimension=ScoreDimension.SAFETY, score=safety_score, weight=2.0),
    ]
    result = _scorer.score(dimension_scores)
    _budget_manager.reset_usage()
    return result


gateway.set_input_policy(
    _detect_unsafe_input_reason,
    blocked_response_text=(
        "⚠️ セキュリティポリシーにより、この入力は実行できません。安全な依頼に書き換えて再送してください。"
    ),
)


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
    approval_event = ApprovalRequiredEvent(
        timestamp=time.time(),
        flow_id=request.id,
        data={"user_id": request.user_id, "skill_name": request.skill_name},
        request_id=request.id,
        action=request.skill_name,
        reason=request.reason,
        risk_level=request.risk_level.value,
        context={"params": request.params},
    )
    approval_card = CardComponent(
        title="Approval Required",
        children=[
            TextComponent(f"skill: {request.skill_name}"),
            TextComponent(f"reason: {request.reason}"),
            TextComponent(f"risk: {request.risk_level.value}"),
        ],
    )
    component_event = A2UIComponentEvent(
        timestamp=time.time(),
        flow_id=request.id,
        surface_id=f"user:{request.user_id}",
        component=approval_card.to_dict(),
        data={"request_id": request.id},
    )
    for message in (
        {"type": approval_event.event_type.value, "data": approval_event.to_dict()},
        {"type": component_event.event_type.value, "data": component_event.to_dict()},
    ):
        await hub.broadcast_room(f"user:{request.user_id}", message)
        await hub.broadcast(message)


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
    lazy_tool_loader=_lazy_tool_loader,
)

_register_runtime_agent(assistant)

_generated_artifact_manager: GeneratedArtifactManager
_execution_substrate_service: ExecutionSubstrateService
_flight_watch_service: FlightWatchService
_flight_watch_agent: FlightWatchAgent
_orchestration_service: OrchestrationService
_runtime_artifact_registry: RuntimeArtifactAgentRegistry


def _initialize_orchestration_services() -> None:
    """orchestration 関連サービスを再初期化する."""
    global _generated_artifact_manager
    global _execution_substrate_service
    global _flight_watch_agent
    global _flight_watch_service
    global _orchestration_service
    global _runtime_artifact_registry

    _execution_substrate_service = ExecutionSubstrateService(SQLiteExecutionSubstrateStore(db_path=_store.db_path))
    _generated_artifact_manager = GeneratedArtifactManager(
        store=_store,
        skills_manager=_skills_manager,
    )
    _runtime_artifact_registry = RuntimeArtifactAgentRegistry(
        skills_manager=_skills_manager,
        memory_service=_harness_memory_service,
    )
    _flight_watch_service = FlightWatchService(
        store=_store,
        websocket_hub=hub,
        skill_gateway=_skill_gateway,
    )
    _flight_watch_agent = FlightWatchAgent(service=_flight_watch_service)
    _register_runtime_agent(_flight_watch_agent)
    _orchestration_service = OrchestrationService(
        store=_store,
        websocket_hub=hub,
        assistant=assistant,
        flight_watch_service=_flight_watch_service,
        generated_artifact_manager=_generated_artifact_manager,
        security_mode=_get_security_mode(),
        skill_gateway=_skill_gateway,
        mcp_manager=_mcp_manager,
        memory_service=_harness_memory_service,
        runtime_artifact_registry=_runtime_artifact_registry,
        execution_substrate=_execution_substrate_service,
    )


_initialize_orchestration_services()


def _standard_event_names() -> list[str]:
    """標準イベント一覧を返す."""
    return [
        "RunStarted",
        "StepStarted",
        "ToolApprovalRequested",
        "ToolExecuted",
        "EvidenceAdded",
        "RunFinished",
        "flow.start",
        "node.start",
        "progress",
        "clarification.required",
        "approval_required",
        "a2ui.component",
        "flow.complete",
        "flow.error",
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
        payload_raw = result.to_dict()
        payload = payload_raw if isinstance(payload_raw, dict) else {}
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
# ライフサイクル管理
# =========================================================================


@asynccontextmanager
async def lifespan(app: FastAPI) -> Any:
    """アプリケーションのライフサイクルを管理する。"""
    logger.info("Starting Messaging Hub...")

    # 1. 永続化層の初期化と復元
    await _store.initialize()
    await _harness_memory_service.start()
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

    # 2. スキルパック GitHub 同期 → ロード → ワークフロー登録
    from apps.messaging_hub.skills.entrepreneurship.loader import (
        discover_and_load_all_packs,
    )
    from apps.messaging_hub.skills.entrepreneurship.sync import sync_all_packs
    from apps.messaging_hub.skills.entrepreneurship.workflows import (
        get_default_workflows,
    )

    sync_results = await sync_all_packs()
    for pack_name, updated in sync_results.items():
        if updated:
            logger.info("スキルパック '%s' を GitHub から更新しました", pack_name)

    discover_and_load_all_packs(_skill_gateway)

    for wf_def in get_default_workflows():
        await _skills_manager.create_workflow(wf_def)
    logger.info("ビジネスワークフロー %d 件登録完了", len(get_default_workflows()))
    restored_artifacts = await _generated_artifact_manager.restore_runtime_bindings()
    restored_agent_names = await _runtime_artifact_registry.restore(_generated_artifact_manager)
    if restored_agent_names:
        logger.info("復元した runtime artifact agents: %s", ", ".join(restored_agent_names))
    elif restored_artifacts:
        logger.info("復元した runtime artifacts: %d", len(restored_artifacts))

    # 3. プラットフォームアダプターを登録
    await setup_platforms()

    # 4. ツールインデックスの構築（遅延ロード）
    await _lazy_tool_loader.build_index()

    # 5. バックグラウンドタスクを開始（Discord bot）
    await start_background_tasks()
    _log_startup_summary()

    logger.info("Messaging Hub started successfully")
    logger.info(f"Registered platforms: {gateway.list_channels()}")

    yield

    # リソースを解放
    logger.info("Shutting down Messaging Hub...")

    # バックグラウンドタスクを停止
    for task in background_tasks:
        task.cancel()
    await asyncio.gather(*background_tasks, return_exceptions=True)

    # ゲートウェイを停止
    await gateway.shutdown()
    await _harness_memory_service.stop()

    logger.info("Messaging Hub shut down")


async def setup_platforms() -> None:
    """登録可能な外部プラットフォームを環境変数から接続する."""
    for platform_name in _EXTERNAL_PLATFORM_NAMES:
        connected, message = await _connect_platform_from_env(platform_name)
        if connected:
            logger.info("✓ %s connected (%s)", platform_name, message)
        else:
            logger.warning("✗ %s not connected (%s)", platform_name, message)


async def start_background_tasks() -> None:
    """バックグラウンドタスクを開始する。"""
    await _ensure_discord_runtime_task()
    await _ensure_flight_watch_monitor_task()

    # 必要に応じて他のバックグラウンドタスクも追加する


async def _ensure_discord_runtime_task() -> None:
    """Ensure discord runtime task is running when adapter is connected."""
    current_task = _platform_runtime_tasks.get("discord")
    discord_adapter = gateway.get_channel("discord")
    if not isinstance(discord_adapter, DiscordAdapter):
        if current_task and not current_task.done():
            current_task.cancel()
            with suppress(asyncio.CancelledError):
                await current_task
        _platform_runtime_tasks.pop("discord", None)
        return

    if current_task and not current_task.done():
        return

    task = asyncio.create_task(discord_adapter.start_bot(gateway))
    _platform_runtime_tasks["discord"] = task
    background_tasks.append(task)
    logger.info("Started Discord bot task")


async def _ensure_flight_watch_monitor_task() -> None:
    """Flight watch の定期監視タスクを起動する."""
    current_task = _platform_runtime_tasks.get("flight_watch_monitor")
    if current_task and not current_task.done():
        return

    task = asyncio.create_task(_flight_watch_monitor_loop())
    _platform_runtime_tasks["flight_watch_monitor"] = task
    background_tasks.append(task)
    logger.info("Started flight watch monitor task")


async def _flight_watch_monitor_loop() -> None:
    """機票監視購読を定期実行する."""
    interval_seconds = max(int(os.getenv("MESSAGING_HUB_FLIGHT_WATCH_LOOP_SECONDS", "60")), 30)
    while True:
        try:
            result = await _flight_watch_service.check_due_subscriptions()
            checked = int(result.get("checked", 0))
            notified = int(result.get("notified", 0))
            if checked > 0 or notified > 0:
                logger.info(
                    "Flight watch monitor tick: checked=%d notified=%d",
                    checked,
                    notified,
                )
        except asyncio.CancelledError:
            raise
        except Exception as exc:
            logger.warning("Flight watch monitor loop failed: %s", exc, exc_info=True)
        await asyncio.sleep(interval_seconds)


async def _connect_platform_from_env(platform_name: str) -> tuple[bool, str]:
    """Create adapter from env and register platform."""
    missing = _missing_required_credentials(platform_name)
    if missing:
        gateway.unregister_channel(platform_name)
        if platform_name == "discord":
            await _ensure_discord_runtime_task()
        return False, f"missing required credentials: {', '.join(missing)}"

    try:
        if platform_name == "telegram":
            token = os.getenv("TELEGRAM_BOT_TOKEN", "").strip()
            telegram_adapter = TelegramAdapter(token=token)
            gateway.register_channel(platform_name, telegram_adapter)
            try:
                bot_info = await telegram_adapter.get_bot_info()
                logger.info("Telegram bot: @%s", bot_info.get("username"))
            except Exception as exc:
                logger.warning("Telegram get_bot_info failed: %s", exc)
            return True, "token configured"

        if platform_name == "slack":
            token = os.getenv("SLACK_BOT_TOKEN", "").strip()
            signing_secret = os.getenv("SLACK_SIGNING_SECRET", "").strip() or None
            slack_adapter = SlackAdapter(token=token, signing_secret=signing_secret)
            gateway.register_channel(platform_name, slack_adapter)
            try:
                bot_info = await slack_adapter.get_bot_info()
                logger.info("Slack bot: %s", bot_info.get("user"))
            except Exception as exc:
                logger.warning("Slack get_bot_info failed: %s", exc)
            return True, "token configured"

        if platform_name == "discord":
            token = os.getenv("DISCORD_BOT_TOKEN", "").strip()
            discord_adapter = DiscordAdapter(token=token)
            gateway.register_channel(platform_name, discord_adapter)
            return True, "token configured"

        if platform_name == "teams":
            app_id = os.getenv("TEAMS_APP_ID", "").strip()
            app_password = os.getenv("TEAMS_APP_PASSWORD", "").strip()
            teams_adapter = TeamsAdapter(app_id=app_id, app_password=app_password)
            gateway.register_channel(platform_name, teams_adapter)
            return True, "app credentials configured"

        if platform_name == "whatsapp":
            phone_number_id = os.getenv("WHATSAPP_PHONE_NUMBER_ID", "").strip()
            access_token = os.getenv("WHATSAPP_ACCESS_TOKEN", "").strip()
            verify_token = os.getenv("WHATSAPP_VERIFY_TOKEN", "").strip() or None
            whatsapp_adapter = WhatsAppAdapter(
                phone_number_id=phone_number_id,
                access_token=access_token,
                verify_token=verify_token,
            )
            gateway.register_channel(platform_name, whatsapp_adapter)
            return True, "business credentials configured"

        if platform_name == "signal":
            api_url = os.getenv("SIGNAL_API_URL", "").strip()
            phone_number = os.getenv("SIGNAL_PHONE_NUMBER", "").strip()
            signal_adapter = SignalAdapter(api_url=api_url, phone_number=phone_number)
            gateway.register_channel(platform_name, signal_adapter)
            return True, "signal-cli credentials configured"
    except Exception as exc:
        gateway.unregister_channel(platform_name)
        if platform_name == "discord":
            await _ensure_discord_runtime_task()
        return False, str(exc)

    return False, "unsupported platform"


async def _get_platform_bot_info(adapter: MessageChannelAdapter) -> dict[str, Any]:
    """Bot 情報取得に対応している adapter のみ問い合わせる."""
    if isinstance(adapter, TelegramAdapter | SlackAdapter | DiscordAdapter | TeamsAdapter):
        return await adapter.get_bot_info()
    return {}


# =========================================================================
# FastAPI アプリケーション
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
# WebSocket エンドポイント
# =========================================================================


@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket) -> None:
    """WebSocket 接続エンドポイント。

    フロントエンドはこのエンドポイントからリアルタイム更新を受信できる。
    """
    if not await _require_ws_api_key(websocket):
        return

    client_id = websocket.query_params.get("client_id", "anonymous")

    try:
        await hub.handle_connection(websocket, client_id=client_id)
    except WebSocketDisconnect:
        logger.info(f"WebSocket client disconnected: {client_id}")
    except Exception as e:
        logger.error(f"WebSocket error: {e}", exc_info=True)


# =========================================================================
# Webhook エンドポイント
# =========================================================================


@app.post("/webhook/telegram")
async def telegram_webhook(request: Request) -> JSONResponse:
    """Telegram Webhook エンドポイント。"""
    try:
        update_data = await request.json()

        telegram_adapter = gateway.get_channel("telegram")
        if isinstance(telegram_adapter, TelegramAdapter):
            await telegram_adapter.handle_webhook(update_data, gateway)

        return JSONResponse({"ok": True})

    except Exception as e:
        logger.error(f"Telegram webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


@app.post("/webhook/slack")
async def slack_webhook(request: Request) -> JSONResponse:
    """Slack Webhook エンドポイント。"""
    try:
        body = await request.body()
        headers = dict(request.headers)

        slack_adapter = gateway.get_channel("slack")
        if isinstance(slack_adapter, SlackAdapter):
            response = await slack_adapter.handle_webhook(body, headers, gateway)
            return JSONResponse(response)

        return JSONResponse({"ok": False, "error": "Slack not configured"})

    except Exception as e:
        logger.error(f"Slack webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


@app.post("/webhook/teams")
async def teams_webhook(request: Request) -> JSONResponse:
    """Microsoft Teams Webhook エンドポイント。"""
    try:
        activity_data = await request.json()
        auth_header = request.headers.get("Authorization", "")

        teams_adapter = gateway.get_channel("teams")
        if isinstance(teams_adapter, TeamsAdapter):
            response = await teams_adapter.handle_webhook(activity_data, auth_header, gateway)
            return JSONResponse(response)

        return JSONResponse({"ok": False, "error": "Teams not configured"})

    except Exception as e:
        logger.error(f"Teams webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


@app.api_route("/webhook/whatsapp", methods=["GET", "POST"], response_model=None)
async def whatsapp_webhook(request: Request) -> JSONResponse | PlainTextResponse:
    """WhatsApp Webhook エンドポイント（検証とメッセージ処理の両方に対応）。"""
    try:
        whatsapp_adapter = gateway.get_channel("whatsapp")
        if not isinstance(whatsapp_adapter, WhatsAppAdapter):
            return JSONResponse({"ok": False, "error": "WhatsApp not configured"})

        if request.method == "GET":
            # Webhook 検証
            params = dict(request.query_params)
            challenge = whatsapp_adapter.verify_webhook(
                str(params.get("hub.mode", "")),
                str(params.get("hub.verify_token", "")),
                str(params.get("hub.challenge", "")),
            )
            if challenge is None:
                return JSONResponse({"ok": False, "error": "verification_failed"}, status_code=403)
            return PlainTextResponse(challenge)
        # メッセージ処理
        payload = await request.json()
        response = await whatsapp_adapter.handle_webhook(payload, gateway)
        return JSONResponse(response)

    except Exception as e:
        logger.error(f"WhatsApp webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


@app.post("/webhook/signal")
async def signal_webhook(request: Request) -> JSONResponse:
    """Signal Webhook エンドポイント（signal-cli-rest-api callback モード）。"""
    try:
        payload = await request.json()

        signal_adapter = gateway.get_channel("signal")
        if isinstance(signal_adapter, SignalAdapter):
            response = await signal_adapter.handle_webhook(payload, gateway)
            return JSONResponse(response)

        return JSONResponse({"ok": False, "error": "Signal not configured"})

    except Exception as e:
        logger.error(f"Signal webhook error: {e}", exc_info=True)
        return JSONResponse({"ok": False, "error": str(e)}, status_code=500)


# =========================================================================
# API エンドポイント
# =========================================================================


@app.get("/")
async def root() -> dict[str, Any]:
    """ルートエンドポイント。"""
    return {
        "service": "Messaging Hub",
        "version": "1.0.0",
        "status": "running",
        "security_mode": _get_security_mode(),
        "platforms": gateway.list_channels(),
    }


@app.get("/health")
async def health() -> dict[str, Any]:
    """ヘルスチェック。"""
    stats = gateway.get_statistics()
    return {
        "status": "healthy",
        "security_mode": _get_security_mode(),
        "statistics": stats,
    }


@app.get("/api/a2a/card")
async def get_a2a_card() -> dict[str, Any]:
    """A2A AgentCard 相当の情報を返す."""
    from kernel.protocols.a2a_hub import get_hub

    a2a_hub = get_hub()
    for agent_name in (assistant.name, _file_organizer_agent.name, _business_advisor_agent.name):
        card = a2a_hub.discover(agent_name)
        if card is not None and hasattr(card, "to_a2a_format"):
            return card.to_a2a_format()

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


@app.get("/api/a2a/agents")
async def list_a2a_agents() -> list[dict[str, Any]]:
    """A2AHub 登録済み Agent の AgentCard 一覧を返す."""
    from kernel.protocols.a2a_hub import get_hub

    hub = get_hub()
    cards = hub.list_agents()
    return [card.to_a2a_format() for card in cards]


@app.get("/platforms")
async def list_platforms() -> dict[str, Any]:
    """登録済みプラットフォームを一覧する。"""
    platforms = []

    for platform_name in gateway.list_channels():
        adapter = gateway.get_channel(platform_name)
        if adapter:
            try:
                bot_info = await _get_platform_bot_info(adapter)
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
    """プラットフォームへ直接メッセージを送信する（管理 API）。

    Args:
        platform: プラットフォーム名（telegram, slack, discord）
        channel_id: チャンネルまたはユーザー ID
        text: 送信メッセージ本文

    Returns:
        メッセージ ID
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
    """アクティブなセッションを一覧する。"""
    sessions = chatbot.list_sessions()
    return {"sessions": sessions, "total": len(sessions)}


# =========================================================================
# 管理画面用 API エンドポイント（Admin Dashboard）
# =========================================================================


@app.get("/api/health")
async def api_health() -> dict[str, Any]:
    """管理画面用ヘルスチェック."""
    stats = gateway.get_statistics()
    auth_required = _is_auth_required()
    auth_env_var = _auth_api_key_env_name()
    auth_key_configured = _is_auth_key_configured()
    status = "healthy"
    if auth_required and not auth_key_configured:
        status = "degraded"
    return {
        "status": status,
        "security_mode": _get_security_mode(),
        "auth_required": auth_required,
        "auth_env_var": auth_env_var,
        "auth_key_configured": auth_key_configured,
        "uptime": max(time.time() - _process_started_at, 0.0),
        "statistics": stats,
    }


@app.get("/api/admin-key")
async def api_admin_key() -> dict[str, Any]:
    """管理画面ブートストラップ用：設定済み API キーを返す（認証不要・ローカル管理用途）.

    フロントエンドが localStorage にキーを持っていない場合に自動取得するために使用する。
    キーが未設定の場合は api_key=null を返す（フロントエンドは手動入力にフォールバック）。
    """
    if not _is_auth_required():
        return {"api_key": None, "auth_required": False, "configured": True}
    key = os.getenv(_auth_api_key_env_name(), "").strip()
    if not key:
        return {"api_key": None, "auth_required": True, "configured": False}
    return {"api_key": key, "auth_required": True, "configured": True}


@app.get("/api/stats")
async def api_statistics() -> dict[str, Any]:
    """管理画面用統計情報."""
    stats = gateway.get_statistics()
    sessions = chatbot.list_sessions()

    platform_stats = {}
    for platform_name in _PLATFORM_ORDER:
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
    stats = gateway.get_statistics()
    registered_channels = set(gateway.list_channels())
    return [
        _build_platform_info(
            platform_name,
            stats=stats,
            registered_channels=registered_channels,
        )
        for platform_name in _PLATFORM_ORDER
    ]


class PlatformCredentialSaveRequest(BaseModel):
    """Platform credentials update payload."""

    values: dict[str, str] = Field(default_factory=dict)
    auto_connect: bool = Field(default=True)


@app.post("/api/platforms/{platform_name}/credentials")
async def api_save_platform_credentials(
    platform_name: str,
    payload: PlatformCredentialSaveRequest,
) -> JSONResponse:
    """Save platform credentials securely and optionally connect immediately."""
    if platform_name not in _PLATFORM_CATALOG:
        return JSONResponse(
            {"ok": False, "error": "unknown_platform"},
            status_code=404,
        )
    if platform_name == _LOCAL_PLATFORM_ID:
        return JSONResponse(
            {"ok": False, "error": "managed_platform_not_editable"},
            status_code=400,
        )

    validation_errors: list[str] = []
    updated_fields: list[str] = []
    for field_key, raw_value in payload.values.items():
        env_name = _credential_env_name(platform_name, field_key)
        if env_name is None:
            validation_errors.append(f"未対応フィールド: {field_key}")
            continue
        issue = _validate_credential_value(platform_name, field_key, raw_value)
        if issue:
            validation_errors.append(f"{field_key}: {issue}")
            continue
        os.environ[env_name] = raw_value.strip()
        updated_fields.append(field_key)

    if validation_errors:
        return JSONResponse(
            {
                "ok": False,
                "error": "validation_error",
                "messages": validation_errors,
            },
            status_code=400,
        )

    missing = _missing_required_credentials(platform_name)
    if missing:
        return JSONResponse(
            {
                "ok": False,
                "error": "missing_required_credentials",
                "messages": missing,
            },
            status_code=400,
        )

    connected = False
    connect_message = "credentials_saved"
    if payload.auto_connect:
        connected, connect_message = await _connect_platform_from_env(platform_name)
        if platform_name == "discord" and connected:
            await _ensure_discord_runtime_task()

    stats = gateway.get_statistics()
    registered_channels = set(gateway.list_channels())
    return JSONResponse(
        {
            "ok": True,
            "platform": platform_name,
            "updatedFields": updated_fields,
            "connected": connected
            if payload.auto_connect
            else _is_platform_connected(platform_name, registered_channels),
            "message": connect_message,
            "platformInfo": _build_platform_info(
                platform_name,
                stats=stats,
                registered_channels=registered_channels,
            ),
        }
    )


@app.post("/api/platforms/{platform_name}/connect")
async def api_connect_platform(platform_name: str) -> JSONResponse:
    """Connect platform from current environment credentials."""
    if platform_name not in _PLATFORM_CATALOG:
        return JSONResponse({"ok": False, "error": "unknown_platform"}, status_code=404)
    if platform_name == _LOCAL_PLATFORM_ID:
        return JSONResponse({"ok": True, "platform": platform_name, "connected": True, "message": "managed"})

    connected, message = await _connect_platform_from_env(platform_name)
    if platform_name == "discord" and connected:
        await _ensure_discord_runtime_task()
    stats = gateway.get_statistics()
    registered_channels = set(gateway.list_channels())
    status_code = 200 if connected else 400
    return JSONResponse(
        {
            "ok": connected,
            "platform": platform_name,
            "connected": connected,
            "message": message,
            "platformInfo": _build_platform_info(
                platform_name,
                stats=stats,
                registered_channels=registered_channels,
            ),
        },
        status_code=status_code,
    )


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
        messages: list[ExportMessage | dict[str, Any]] = []

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


def _resolve_export_format(raw_format: str) -> ExportFormat:
    """Normalize export format parameter."""
    normalized = raw_format.strip().lower()
    if normalized in {"json", "csv", "markdown"}:
        return ExportFormat(normalized)
    return ExportFormat.JSON


def _export_extension(export_format: ExportFormat) -> str:
    """Resolve file extension from export format."""
    if export_format is ExportFormat.MARKDOWN:
        return "md"
    return str(export_format.value)


def _safe_filename_part(value: str) -> str:
    """Convert arbitrary identifier to filesystem-safe part."""
    return "".join(ch if ch.isalnum() or ch in {"-", "_"} else "_" for ch in value)


def _build_sr_export_filename(conversation_id: str | None, export_format: ExportFormat) -> str:
    """Build filename for sr_chat export response."""
    extension = _export_extension(export_format)
    if conversation_id:
        return f"sr_chat_{_safe_filename_part(conversation_id)}.{extension}"
    return f"sr_chat_all.{extension}"


async def _collect_sr_export_messages(conversation_id: str | None) -> list[dict[str, Any]]:
    """Collect sr_chat messages for export."""
    conversation_ids: list[str] = []
    if conversation_id:
        conversation_ids.append(conversation_id)
    else:
        rows = await _store.list_sr_conversations(limit=300)
        conversation_ids.extend(
            str(row.get("conversation_id", "")).strip() for row in rows if str(row.get("conversation_id", "")).strip()
        )

    messages: list[dict[str, Any]] = []
    for cid in conversation_ids:
        rows = await _store.list_sr_messages(conversation_id=cid, limit=1000)
        for row in reversed(rows):
            metadata = row.get("metadata", {})
            meta = metadata if isinstance(metadata, dict) else {}
            user_id = str(meta.get("user_id", ""))
            messages.append(
                {
                    "timestamp": row.get("created_at", ""),
                    "platform": str(meta.get("platform", "sr_chat")),
                    "user_id": user_id,
                    "user_name": user_id or "sr_chat_user",
                    "role": row.get("role", "user"),
                    "content": row.get("content", ""),
                    "conversation_id": cid,
                }
            )
    return messages


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
    requested_by: str = Field(default="admin", min_length=1)


class SkillCallRequest(BaseModel):
    """スキル呼び出しリクエスト."""

    params: dict[str, Any] = Field(default_factory=dict)
    dry_run: bool = Field(default=False)
    user_id: str = Field(default="admin")


class GeneratedArtifactApproveRequest(BaseModel):
    """生成物承認リクエスト."""

    approver_id: str = Field(default="admin", min_length=1)


class GeneratedArtifactRejectRequest(BaseModel):
    """生成物却下リクエスト."""

    rejected_by: str = Field(default="admin", min_length=1)
    reason: str = Field(..., min_length=1)


class OrchestrationTaskCreateRequest(BaseModel):
    """オーケストレーション task 作成リクエスト."""

    message: str = Field(..., min_length=1)
    user_id: str = Field(default="default", min_length=1)
    conversation_id: str | None = None
    required_capability: str | None = None
    input_data: dict[str, Any] = Field(default_factory=dict)


class OrchestrationClarificationRequest(BaseModel):
    """clarification 回答リクエスト."""

    answers: dict[str, Any] = Field(default_factory=dict)


class FlightWatchSearchAPIRequest(BaseModel):
    """機票検索 API リクエスト."""

    request: FlightSearchRequest


class FlightWatchSubscriptionAPIRequest(BaseModel):
    """機票監視購読 API リクエスト."""

    request: FlightSearchRequest
    user_id: str = Field(default="default", min_length=1)
    conversation_id: str | None = None


class FileOrganizerAnalyzeRequest(BaseModel):
    """File Organizer 分析リクエスト."""

    path: str = Field(..., min_length=1)
    days_old: int = Field(default=30, ge=1, le=3650)
    recursive: bool = Field(default=True)


class FileOrganizerDuplicatesRequest(BaseModel):
    """File Organizer 重複検出リクエスト."""

    path: str = Field(..., min_length=1)
    by_content: bool = Field(default=True)


class FileOrganizerOrganizeRequest(BaseModel):
    """File Organizer 整理リクエスト."""

    path: str = Field(..., min_length=1)
    dry_run: bool = Field(default=True)
    rules: dict[str, Any] | None = None
    user_id: str = Field(default="admin", min_length=1)
    approval_request_id: str | None = None


async def _prepare_file_organizer_execution(
    payload: FileOrganizerOrganizeRequest,
) -> tuple[bool, dict[str, Any]]:
    """Resolve approval requirements for file organizer execution."""
    if payload.dry_run:
        return True, {}

    if payload.approval_request_id:
        request = _approval_manager.find_request(payload.approval_request_id)
        if request is None:
            return False, {
                "ok": False,
                "error": "approval_request_not_found",
                "requires_approval": True,
            }
        if request.skill_name != "file_organizer.organize":
            return False, {
                "ok": False,
                "error": "approval_request_mismatch",
                "requires_approval": True,
                "request_id": request.id,
            }
        requested_path = str(request.params.get("path", "")).strip()
        if requested_path != payload.path:
            return False, {
                "ok": False,
                "error": "approval_request_path_mismatch",
                "requires_approval": True,
                "request_id": request.id,
            }
        if request.status in {ApprovalStatus.APPROVED, ApprovalStatus.AUTO_APPROVED}:
            return True, {}
        if request.status == ApprovalStatus.PENDING:
            return False, {
                "ok": False,
                "requires_approval": True,
                "request_id": request.id,
                "status": request.status.value,
                "message": "承認待ちです。承認後に再実行してください。",
            }
        return False, {
            "ok": False,
            "requires_approval": True,
            "request_id": request.id,
            "status": request.status.value,
            "message": "この承認リクエストは利用できません。",
        }

    request_id, auto_approved = await _approval_manager.request_approval(
        skill_name="file_organizer.organize",
        params={"path": payload.path, "dry_run": payload.dry_run},
        risk_level=RiskLevel.HIGH,
        user_id=payload.user_id,
        metadata={"source": "admin_ui"},
    )
    request = _approval_manager.find_request(request_id)
    if request is not None:
        await _store.upsert_approval(request.to_dict())

    if auto_approved:
        return True, {}

    return False, {
        "ok": False,
        "requires_approval": True,
        "request_id": request_id,
        "status": "pending",
        "message": "実行には承認が必要です。承認後に再実行してください。",
    }


@app.post("/api/file-organizer/analyze")
async def api_file_organizer_analyze(payload: FileOrganizerAnalyzeRequest) -> dict[str, Any]:
    """ディレクトリ分析."""
    agent = FileOrganizerAgent(gateway=_skill_gateway, days_old_threshold=payload.days_old)
    analysis = await agent.analyze_directory(payload.path, recursive=payload.recursive)
    data = analysis.to_dict()
    data["ok"] = True
    return data


@app.post("/api/file-organizer/duplicates")
async def api_file_organizer_duplicates(payload: FileOrganizerDuplicatesRequest) -> dict[str, Any]:
    """重複ファイル検出."""
    groups = await _file_organizer_agent.find_duplicates(payload.path, by_content=payload.by_content)
    return {
        "ok": True,
        "duplicates": [group.to_dict() for group in groups],
        "total": len(groups),
    }


@app.post("/api/file-organizer/organize")
async def api_file_organizer_organize(payload: FileOrganizerOrganizeRequest) -> dict[str, Any]:
    """ファイル整理（dry_run=false は承認必須）."""
    allowed, approval_payload = await _prepare_file_organizer_execution(payload)
    if not allowed:
        return approval_payload

    result = await _file_organizer_agent.organize(
        payload.path,
        rules=payload.rules,
        dry_run=payload.dry_run,
    )
    data = result.to_dict()
    data["ok"] = True
    return data


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
    """自然言語から runtime artifact を生成する."""
    artifact = await _generated_artifact_manager.create_runtime_artifact(
        description=request.description,
        examples=request.examples,
        requested_by=request.requested_by,
        artifact_type=ArtifactType.SKILL,
    )
    return {
        "ok": True,
        "artifact": artifact.model_dump(mode="json"),
        "status": artifact.status.value,
    }


@app.post("/api/skills/{skill_name}/call")
async def api_call_skill(skill_name: str, request: SkillCallRequest) -> dict[str, Any]:
    """スキルを呼び出す."""
    return await _execute_skill_with_tracking(
        skill_name=skill_name,
        params=request.params,
        user_id=request.user_id,
        dry_run=request.dry_run,
    )


@app.get("/api/generated-artifacts")
async def api_generated_artifacts(status: str | None = None) -> dict[str, Any]:
    """生成物一覧."""
    artifacts = await _generated_artifact_manager.list_artifacts(status=status)
    return {
        "artifacts": [artifact.model_dump(mode="json") for artifact in artifacts],
        "total": len(artifacts),
    }


@app.post("/api/generated-artifacts/{artifact_id}/approve", response_model=None)
async def api_approve_generated_artifact(
    artifact_id: str,
    request: GeneratedArtifactApproveRequest,
) -> JSONResponse | dict[str, Any]:
    """生成物を承認する."""
    artifact = await _generated_artifact_manager.approve_artifact(
        artifact_id,
        approver_id=request.approver_id,
    )
    if artifact is None:
        return JSONResponse({"ok": False, "error": "artifact_not_found"}, status_code=404)
    return {"ok": True, "artifact": artifact.model_dump(mode="json")}


@app.post("/api/generated-artifacts/{artifact_id}/reject", response_model=None)
async def api_reject_generated_artifact(
    artifact_id: str,
    request: GeneratedArtifactRejectRequest,
) -> JSONResponse | dict[str, Any]:
    """生成物を却下する."""
    artifact = await _generated_artifact_manager.reject_artifact(
        artifact_id,
        rejected_by=request.rejected_by,
        reason=request.reason,
    )
    if artifact is None:
        return JSONResponse({"ok": False, "error": "artifact_not_found"}, status_code=404)
    return {"ok": True, "artifact": artifact.model_dump(mode="json")}


@app.post("/api/generated-artifacts/{artifact_id}/promote", response_model=None)
async def api_promote_generated_artifact(artifact_id: str) -> JSONResponse | dict[str, Any]:
    """承認済み生成物を publish へ昇格する."""
    artifact = await _generated_artifact_manager.promote_artifact(artifact_id)
    if artifact is None:
        return JSONResponse({"ok": False, "error": "artifact_not_found"}, status_code=404)
    return {"ok": True, "artifact": artifact.model_dump(mode="json")}


@app.post("/api/orchestration/tasks")
async def api_create_orchestration_task(
    request: OrchestrationTaskCreateRequest,
) -> dict[str, Any]:
    """オーケストレーション task を作成して実行する."""
    result = await _orchestration_service.create_task(
        message=request.message,
        user_id=request.user_id,
        conversation_id=request.conversation_id,
        required_capability=request.required_capability,
        input_data=request.input_data,
    )
    return result.model_dump(mode="json")


@app.get("/api/orchestration/tasks/{task_id}", response_model=None)
async def api_get_orchestration_task(task_id: str) -> JSONResponse | dict[str, Any]:
    """task 詳細を返す."""
    task = await _orchestration_service.get_task(task_id)
    if task is None:
        return JSONResponse({"ok": False, "error": "task_not_found"}, status_code=404)
    return {"ok": True, "task": task}


@app.post("/api/orchestration/tasks/{task_id}/clarifications", response_model=None)
async def api_submit_orchestration_clarification(
    task_id: str,
    request: OrchestrationClarificationRequest,
) -> JSONResponse | dict[str, Any]:
    """clarification 回答を送信して task を再開する."""
    result = await _orchestration_service.resume_task_with_clarifications(
        task_id=task_id,
        answers=request.answers,
    )
    if result is None:
        return JSONResponse({"ok": False, "error": "task_not_found"}, status_code=404)
    return result.model_dump(mode="json")


@app.get("/api/orchestration/tasks/{task_id}/events")
async def api_list_orchestration_task_events(task_id: str) -> dict[str, Any]:
    """task event 一覧を返す."""
    events = await _orchestration_service.list_task_events(task_id)
    return {"ok": True, "events": events, "total": len(events)}


@app.get("/api/orchestration/tasks/{task_id}/execution", response_model=None)
async def api_get_orchestration_task_execution(task_id: str) -> JSONResponse | dict[str, Any]:
    """task execution inspection を返す."""
    execution = await _orchestration_service.get_task_execution(task_id)
    if execution is None:
        return JSONResponse({"ok": False, "error": "task_not_found"}, status_code=404)
    return {"ok": True, "execution": execution}


@app.get("/api/orchestration/tasks/{task_id}/replay", response_model=None)
async def api_get_orchestration_task_replay(task_id: str) -> JSONResponse | dict[str, Any]:
    """task replay を返す."""
    replay = await _orchestration_service.get_task_replay(task_id)
    if replay is None:
        return JSONResponse({"ok": False, "error": "task_not_found"}, status_code=404)
    return {"ok": True, "replay": replay}


@app.get("/api/orchestration/monitoring/summary", response_model=None)
async def api_get_orchestration_monitoring_summary() -> dict[str, Any]:
    """execution monitoring summary を返す."""
    approvals = await _store.list_approvals(limit=5000)
    summary = await _orchestration_service.get_monitoring_summary(approvals)
    return {"ok": True, "summary": summary or {}}


@app.post("/api/flight-watch/search")
async def api_flight_watch_search(request: FlightWatchSearchAPIRequest) -> dict[str, Any]:
    """機票検索を実行する."""
    result = await _flight_watch_service.search(request.request)
    return {"ok": True, "result": result.model_dump(mode="json")}


@app.post("/api/flight-watch/subscriptions")
async def api_create_flight_watch_subscription(
    request: FlightWatchSubscriptionAPIRequest,
) -> dict[str, Any]:
    """機票監視購読を作成する."""
    subscription = await _flight_watch_service.create_subscription(
        request=request.request,
        user_id=request.user_id,
        conversation_id=request.conversation_id,
    )
    return {"ok": True, "subscription": subscription.model_dump(mode="json")}


@app.get("/api/flight-watch/subscriptions")
async def api_list_flight_watch_subscriptions(status: str | None = None) -> dict[str, Any]:
    """機票監視購読一覧."""
    items = await _store.list_flight_watch_subscriptions(status=status)
    return {"ok": True, "subscriptions": items, "total": len(items)}


@app.post("/api/flight-watch/subscriptions/{subscription_id}/pause", response_model=None)
async def api_pause_flight_watch_subscription(subscription_id: str) -> JSONResponse | dict[str, Any]:
    """購読を一時停止する."""
    updated = await _flight_watch_service.update_subscription_status(
        subscription_id,
        SubscriptionStatus.PAUSED,
    )
    if updated is None:
        return JSONResponse({"ok": False, "error": "subscription_not_found"}, status_code=404)
    return {"ok": True, "subscription": updated}


@app.post("/api/flight-watch/subscriptions/{subscription_id}/resume", response_model=None)
async def api_resume_flight_watch_subscription(subscription_id: str) -> JSONResponse | dict[str, Any]:
    """購読を再開する."""
    updated = await _flight_watch_service.update_subscription_status(
        subscription_id,
        SubscriptionStatus.ACTIVE,
    )
    if updated is None:
        return JSONResponse({"ok": False, "error": "subscription_not_found"}, status_code=404)
    return {"ok": True, "subscription": updated}


@app.post("/api/flight-watch/subscriptions/{subscription_id}/cancel", response_model=None)
async def api_cancel_flight_watch_subscription(subscription_id: str) -> JSONResponse | dict[str, Any]:
    """購読を解約する."""
    updated = await _flight_watch_service.update_subscription_status(
        subscription_id,
        SubscriptionStatus.CANCELLED,
    )
    if updated is None:
        return JSONResponse({"ok": False, "error": "subscription_not_found"}, status_code=404)
    return {"ok": True, "subscription": updated}


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
# MCP 管理 API
# =========================================================================


class MCPInstallAPIRequest(BaseModel):
    """MCP サーバーインストールリクエスト."""

    name: str = Field(..., min_length=1, max_length=100)
    command: str = Field(..., min_length=1)
    args: list[str] = Field(default_factory=list)
    env: dict[str, str] = Field(default_factory=dict)
    description: str = Field(default="")
    install_method: str = Field(default="config")


class MCPLazyLoadingPatchRequest(BaseModel):
    """MCP 遅延ロード設定更新リクエスト."""

    enabled: bool | None = None
    threshold: int | None = Field(default=None, ge=1)
    auto_load_on_call: bool | None = None
    cache_session: bool | None = None
    default_load_count: int | None = Field(default=None, ge=1)


class ToolSearchRequest(BaseModel):
    """ツール検索リクエスト."""

    query: str = Field(..., min_length=1)


@app.get("/api/mcp/servers")
async def api_mcp_list_servers() -> dict[str, Any]:
    """MCP サーバー一覧."""
    servers = _mcp_manager.list_servers()
    return {"servers": servers, "total": len(servers)}


@app.get("/api/mcp/servers/{server_name}")
async def api_mcp_get_server(server_name: str) -> dict[str, Any]:
    """MCP サーバー詳細."""
    server = _mcp_manager.get_server(server_name)
    if server is None:
        return JSONResponse(  # type: ignore[return-value]
            status_code=404,
            content={"error": f"サーバー '{server_name}' が見つかりません"},
        )
    return {"server": server}


@app.post("/api/mcp/servers")
async def api_mcp_install_server(request: MCPInstallAPIRequest) -> dict[str, Any]:
    """MCP サーバーインストール."""
    install_req = MCPInstallRequest(
        name=request.name,
        command=request.command,
        args=request.args,
        env=request.env,
        description=request.description,
        install_method=request.install_method,
    )
    server = _mcp_manager.install_server(install_req)
    # インデックスを再構築
    await _lazy_tool_loader.build_index()
    return {"server": server, "ok": True}


@app.post("/api/mcp/servers/{server_name}/enable")
async def api_mcp_enable_server(server_name: str) -> dict[str, Any]:
    """MCP サーバー有効化."""
    ok = _mcp_manager.enable_server(server_name)
    if ok:
        await _lazy_tool_loader.build_index()
    return {"ok": ok}


@app.post("/api/mcp/servers/{server_name}/disable")
async def api_mcp_disable_server(server_name: str) -> dict[str, Any]:
    """MCP サーバー無効化."""
    ok = _mcp_manager.disable_server(server_name)
    if ok:
        await _lazy_tool_loader.build_index()
    return {"ok": ok}


@app.delete("/api/mcp/servers/{server_name}")
async def api_mcp_delete_server(server_name: str) -> dict[str, Any]:
    """MCP サーバー削除."""
    ok = _mcp_manager.delete_server(server_name)
    if ok:
        await _lazy_tool_loader.build_index()
    return {"ok": ok}


@app.get("/api/mcp/lazy-loading")
async def api_mcp_get_lazy_loading() -> dict[str, Any]:
    """遅延ロード設定を取得する."""
    return {"lazy_loading": _mcp_manager.get_lazy_loading_config()}


@app.patch("/api/mcp/lazy-loading")
async def api_mcp_patch_lazy_loading(
    request: MCPLazyLoadingPatchRequest,
) -> dict[str, Any]:
    """遅延ロード設定を更新する."""
    patch_data = request.model_dump(exclude_none=True)
    updated = _mcp_manager.update_lazy_loading_config(patch_data)
    return {"lazy_loading": updated}


@app.get("/api/tools/index")
async def api_tool_index() -> dict[str, Any]:
    """ツールインデックス取得（全ツールの軽量メタデータ）."""
    index = _lazy_tool_loader.get_tool_index()
    return {"tools": index, "total": len(index)}


@app.post("/api/tools/search")
async def api_tool_search(request: ToolSearchRequest) -> dict[str, Any]:
    """ツール検索・ロード."""
    result = _lazy_tool_loader.search_and_load(request.query)
    return {
        "query": result.query,
        "matches": [e.to_dict() for e in result.entries],
        "loaded_count": result.loaded_count,
    }


@app.get("/api/tools/stats")
async def api_tool_stats() -> dict[str, Any]:
    """ツールローダー統計."""
    return _lazy_tool_loader.get_stats()


@app.get("/api/tools/contracts")
async def api_tool_contracts(
    template_name: str | None = None,
    format: str | None = None,
) -> dict[str, Any]:
    """semantic tool contracts を返す."""
    params: dict[str, Any] | None = None
    if format is not None:
        params = {"format": format}
    contracts = assistant.list_semantic_tool_contracts(template_name=template_name, params=params)
    return {"contracts": contracts, "total": len(contracts)}


@app.post("/api/tools/reset")
async def api_tool_reset() -> dict[str, Any]:
    """ツールセッションリセット."""
    _lazy_tool_loader.reset_session()
    return {"ok": True}


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
    user_id: str | None = None
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


@app.get("/api/sr_chat/export")
async def sr_chat_export(
    format: str = "json",
    conversation_id: str | None = None,
) -> dict[str, Any]:
    """sr_chat メッセージ履歴をエクスポート."""
    export_format = _resolve_export_format(format)
    messages_raw = await _collect_sr_export_messages(conversation_id)
    messages: list[ExportMessage | dict[str, Any]] = []
    for message in messages_raw:
        messages.append(message)
    exporter = ConversationExportSkill()
    data = await exporter.export(messages, format=export_format)
    return {
        "ok": True,
        "format": export_format.value,
        "conversation_id": conversation_id,
        "total_messages": len(messages),
        "filename": _build_sr_export_filename(conversation_id, export_format),
        "data": data,
    }


@app.post("/api/sr_chat/chat.postMessage", response_model=None)
async def sr_chat_post_message(request: SRChatPostRequest) -> JSONResponse | dict[str, Any]:
    """メッセージを投稿してアシスタント応答を生成."""
    unsafe_reason = _detect_unsafe_input_reason(request.text)
    if unsafe_reason:
        return JSONResponse(
            {
                "ok": False,
                "blocked": True,
                "detail": f"安全ポリシーにより拒否: {unsafe_reason}",
            },
            status_code=400,
        )

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

    # Token予算チェック: 入力サイズを計測してログ記録
    input_token_count = _budget_manager.count_tokens(request.text)
    if input_token_count > 3000:
        logger.warning("sr_chat: 入力Token数が大きい (tokens=%d, run_id=%s)", input_token_count, run_id)

    start_time = time.monotonic()
    orchestration_result = await _orchestration_service.create_task(
        message=request.text,
        user_id=request.user_id,
        conversation_id=request.conversation_id,
    )
    elapsed = time.monotonic() - start_time

    assistant_text = _resolve_orchestration_message_text(orchestration_result)

    # 応答品質スコアリング
    scoring = _score_assistant_response(
        input_text=request.text,
        response_text=assistant_text,
        elapsed_seconds=elapsed,
        is_safe=True,
    )
    logger.info(
        "sr_chat scoring: overall=%.3f latency=%.2fs tokens_in=%d (run_id=%s)",
        scoring.overall_score,
        elapsed,
        input_token_count,
        run_id,
    )
    await _orchestration_service.record_quality_feedback(
        task_id=orchestration_result.task_id,
        source="sr_chat",
        score=scoring.overall_score,
        elapsed_seconds=round(elapsed, 3),
        input_tokens=input_token_count,
    )
    execution = await _orchestration_service.get_task_execution(orchestration_result.task_id)
    execution_summary = execution.get("execution_summary", {}) if isinstance(execution, dict) else {}

    response_time = datetime.now(UTC).isoformat()
    await _store.upsert_sr_message(
        {
            "message_id": assistant_message_id,
            "conversation_id": request.conversation_id,
            "role": "assistant",
            "content": assistant_text,
            "created_at": response_time,
            "updated_at": response_time,
            "metadata": {
                "run_id": run_id,
                "task_id": orchestration_result.task_id,
                "status": orchestration_result.status.value,
                "intent": (
                    orchestration_result.result.get("intent", {})
                    if isinstance(orchestration_result.result, dict)
                    else {}
                ),
                "score": scoring.overall_score,
                "elapsed_seconds": round(elapsed, 3),
            },
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
        "ok": orchestration_result.ok,
        "conversation_id": request.conversation_id,
        "run_id": run_id,
        "task_id": orchestration_result.task_id,
        "status": orchestration_result.status.value,
        "message": {
            "id": assistant_message_id,
            "role": "assistant",
            "text": assistant_text,
        },
        "summary": orchestration_result.summary,
        "headline": orchestration_result.headline,
        "intent": (
            orchestration_result.result.get("intent", {}) if isinstance(orchestration_result.result, dict) else {}
        ),
        "clarification_ticket_id": orchestration_result.clarification_ticket_id,
        "clarification_questions": orchestration_result.clarification_questions,
        "generated_artifact": orchestration_result.generated_artifact,
        "execution_session_id": orchestration_result.execution_session_id,
        "execution_summary": execution_summary,
        "delivery_error": delivery_error,
        "quality": {
            "score": scoring.overall_score,
            "elapsed_seconds": round(elapsed, 3),
            "input_tokens": input_token_count,
        },
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
    rooms: list[str] = []
    if payload.conversation_id:
        rooms.extend([payload.conversation_id, f"conversation:{payload.conversation_id}"])
    if payload.user_id:
        rooms.append(f"user:{payload.user_id}")
    return {
        "ok": True,
        "subscription_id": subscription_id,
        "events": payload.event_types,
        "ws_url": ws_url,
        "rooms": rooms,
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


@app.post("/assistant/process", response_model=None)
async def process_assistant_request(
    request: AssistantProcessRequest,
) -> JSONResponse | dict[str, Any]:
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
    unsafe_reason = _detect_unsafe_input_reason(request.message)
    if unsafe_reason:
        return JSONResponse(
            {
                "ok": False,
                "blocked": True,
                "error": "blocked_by_safety_policy",
                "summary": f"⚠️ 安全ポリシーにより拒否: {unsafe_reason}",
            },
            status_code=400,
        )

    try:
        run_id = f"run_{uuid.uuid4().hex}"

        # Token予算チェック: 入力サイズを計測してログ記録
        input_token_count = _budget_manager.count_tokens(request.message)
        if input_token_count > 3000:
            logger.warning(
                "assistant/process: 入力Token数が大きい (tokens=%d, run_id=%s)",
                input_token_count,
                run_id,
            )

        start_time = time.monotonic()
        result = await _orchestration_service.create_task(
            message=request.message,
            user_id=request.user_id,
        )
        elapsed = time.monotonic() - start_time

        # 応答品質スコアリング
        summary_text = result.summary
        scoring = _score_assistant_response(
            input_text=request.message,
            response_text=summary_text,
            elapsed_seconds=elapsed,
            is_safe=True,
        )
        logger.info(
            "assistant/process scoring: overall=%.3f latency=%.2fs tokens_in=%d (run_id=%s)",
            scoring.overall_score,
            elapsed,
            input_token_count,
            run_id,
        )
        await _orchestration_service.record_quality_feedback(
            task_id=result.task_id,
            source="assistant_process",
            score=scoring.overall_score,
            elapsed_seconds=round(elapsed, 3),
            input_tokens=input_token_count,
        )
        execution = await _orchestration_service.get_task_execution(result.task_id)
        execution_summary = execution.get("execution_summary", {}) if isinstance(execution, dict) else {}

        response: dict[str, Any] = {
            "ok": result.ok,
            "status": result.status.value,
            "task_id": result.task_id,
            "execution_session_id": result.execution_session_id,
            "execution_summary": execution_summary,
            "summary": result.summary,
            "headline": result.headline,
            "key_points": result.key_points,
            "actions": result.actions,
            "risks": result.risks,
            "intent": result.result.get("intent", {}) if isinstance(result.result, dict) else {},
            "run_id": result.result.get("run_id", run_id) if isinstance(result.result, dict) else run_id,
            "clarification_ticket_id": result.clarification_ticket_id,
            "clarification_questions": result.clarification_questions,
            "generated_artifact": result.generated_artifact,
            "result": result.result,
            "quality": {
                "score": scoring.overall_score,
                "elapsed_seconds": round(elapsed, 3),
                "input_tokens": input_token_count,
            },
        }
        raw_result = result.result if isinstance(result.result, dict) else {}
        if raw_result.get("needs_cli_confirmation") is True:
            proposal = raw_result.get("cli_proposal")
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
