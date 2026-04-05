"""タスクランタイム状態管理（TaskRuntime / active_tasks / ユーティリティ）."""

from __future__ import annotations

import asyncio
import json
import logging
import os
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

from apps.code_migration_assistant.engine import CodeMigrationEngine
from apps.code_migration_assistant.backend.task_store import RedisTaskStore

logger = logging.getLogger("migration_server")

CONTRACT_VERSION = "2026-03-01"


# ---------------------------------------------------------------------------
# 分散ストア
# ---------------------------------------------------------------------------

def create_distributed_store() -> RedisTaskStore[dict[str, Any]] | None:
    """Redis 分散ストアを構築する（未設定なら None）."""
    redis_url = os.getenv("CODE_MIGRATION_REDIS_URL", "").strip()
    if not redis_url:
        return None
    return RedisTaskStore[dict[str, Any]](
        redis_url=redis_url,
        key_prefix="code_migration_assistant",
    )


redis_store: RedisTaskStore[dict[str, Any]] | None = create_distributed_store()


def local_instance_id() -> str:
    """現在プロセスの識別子を返す."""
    store = redis_store
    if store is None:
        return "local-process"
    instance = getattr(store, "instance_id", "local-process")
    return str(instance)


# ---------------------------------------------------------------------------
# グローバルランタイム状態
# ---------------------------------------------------------------------------

active_tasks: dict[str, "TaskRuntime"] = {}
task_websockets: dict[str, set[Any]] = {}


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------

def collect_pending_approvals(engine: CodeMigrationEngine) -> list[dict[str, Any]]:
    """ApprovalFlow の pending request を JSON 化する."""
    pending: list[dict[str, Any]] = []
    try:
        for item in engine.approval_flow.get_pending_requests():
            pending.append(
                {
                    "id": getattr(item, "id", ""),
                    "action": getattr(item, "action", ""),
                    "reason": getattr(item, "reason", ""),
                    "context": getattr(item, "context", {}),
                }
            )
    except Exception:
        logger.debug("collect pending approvals failed", exc_info=True)
    return pending


def build_execution_inputs(
    task_id: str,
    source_code: str,
    migration_type: str,
    module: str | None,
    verification_mode: str,
    expected_outputs: dict[str, Any],
    artifacts_dir: Path,
) -> dict[str, Any]:
    """Engine 実行入力を契約形式で組み立てる."""
    fast_mode = verification_mode == "fast"
    options: dict[str, Any] = {
        "verification_mode": verification_mode,
    }
    return {
        "source_code": source_code,
        "task_id": task_id,
        "module": module or "UNKNOWN",
        "migration_type": migration_type,
        "artifacts_dir": str(artifacts_dir),
        "expected_outputs": expected_outputs,
        "fast_mode": fast_mode,
        "options": options,
    }


def normalize_state_payload(state: dict[str, Any]) -> dict[str, Any]:
    """分散ストアから取得した state を契約形に正規化する."""
    normalized = dict(state)
    normalized.setdefault("contract_version", CONTRACT_VERSION)
    normalized.setdefault("commands", [])
    normalized.setdefault("pending_approvals", [])
    normalized.setdefault("observation_events_count", 0)
    normalized.setdefault("capability_trace", [])
    normalized.setdefault("timeline", [])
    normalized.setdefault("evidence_packets", [])
    normalized.setdefault("retry_decisions", [])
    normalized.setdefault("status", "unknown")
    normalized.setdefault("error", None)
    return normalized


def append_command_log(runtime: "TaskRuntime", record: dict[str, Any]) -> None:
    """コマンド監査ログを JSONL へ追記する."""
    log_path = runtime.artifacts_dir / "logs" / "commands.jsonl"
    log_path.parent.mkdir(parents=True, exist_ok=True)
    with log_path.open("a", encoding="utf-8") as fp:
        fp.write(json.dumps(record, ensure_ascii=False, default=str) + "\n")


def write_feedback_artifact(runtime: "TaskRuntime", record: dict[str, Any]) -> Path:
    """human feedback を成果物として保存する."""
    feedback_dir = runtime.artifacts_dir / "human_feedback"
    feedback_dir.mkdir(parents=True, exist_ok=True)
    filename = f"feedback_{int(time.time() * 1000)}_{uuid.uuid4().hex[:8]}.json"
    artifact_path = feedback_dir / filename
    artifact_path.write_text(
        json.dumps(record, ensure_ascii=False, indent=2, default=str),
        encoding="utf-8",
    )
    return artifact_path


# ---------------------------------------------------------------------------
# TaskRuntime dataclass
# ---------------------------------------------------------------------------

@dataclass
class TaskRuntime:
    """実行中タスクのローカルランタイム状態."""

    task_id: str
    engine: CodeMigrationEngine
    inputs: dict[str, Any]
    flow_context: Any | None
    artifacts_dir: Path
    status: str = "running"
    result: dict[str, Any] | None = None
    error: str | None = None
    command_history: list[dict[str, Any]] = field(default_factory=list)
    observation_events_count: int = 0
    created_at: float = field(default_factory=time.time)
    command_lock: asyncio.Lock = field(default_factory=asyncio.Lock)

    def to_state(self, owner_instance: str | None) -> dict[str, Any]:
        """配布可能な状態スナップショットへ変換する."""
        capability_trace: list[dict[str, Any]] = []
        timeline: list[dict[str, Any]] = []
        evidence_packets: list[dict[str, Any]] = []
        retry_decisions: list[dict[str, Any]] = []
        if isinstance(self.result, dict):
            trace = self.result.get("capability_trace")
            if isinstance(trace, list):
                capability_trace = [item for item in trace if isinstance(item, dict)]
            timeline_raw = self.result.get("timeline")
            if isinstance(timeline_raw, list):
                timeline = [item for item in timeline_raw if isinstance(item, dict)]
            evidence_raw = self.result.get("evidence_packets")
            if isinstance(evidence_raw, list):
                evidence_packets = [item for item in evidence_raw if isinstance(item, dict)]
            retry_raw = self.result.get("retry_decisions")
            if isinstance(retry_raw, list):
                retry_decisions = [item for item in retry_raw if isinstance(item, dict)]

        pending_approvals = collect_pending_approvals(self.engine)

        return {
            "task_id": self.task_id,
            "status": self.status,
            "contract_version": CONTRACT_VERSION,
            "result": self.result,
            "error": self.error,
            "commands": list(self.command_history),
            "pending_approvals": pending_approvals,
            "observation_events_count": self.observation_events_count,
            "capability_trace": capability_trace,
            "timeline": timeline,
            "evidence_packets": evidence_packets,
            "retry_decisions": retry_decisions,
            "owner_instance": owner_instance,
        }
