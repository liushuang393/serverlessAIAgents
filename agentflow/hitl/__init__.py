# -*- coding: utf-8 -*-
"""Human-in-the-Loop (HITL) モジュール.

AI Agent ワークフローに人間の判断を組み込むためのコアモジュール。
LangGraph の interrupt パターンを参考に、業界ベストプラクティスを実装。

主要コンポーネント:
    - interrupt(): 実行を一時停止し、人間の入力を待つ
    - Command: 再開時の指示（approve/reject/update）
    - ApprovalRequest: 承認リクエストのデータモデル
    - Checkpointer: 状態永続化インターフェース

使用例:
    >>> from agentflow.hitl import interrupt, Command, ApprovalRequest
    >>>
    >>> # Agent 内で承認を要求
    >>> async def risky_action(self, data):
    ...     approval = await interrupt(
    ...         ApprovalRequest(
    ...             action="delete_user",
    ...             resource_id=data["user_id"],
    ...             reason="ユーザー削除は不可逆操作です",
    ...             context={"user_name": data["name"]},
    ...         )
    ...     )
    ...     if approval.approved:
    ...         return await self._do_delete(data["user_id"])
    ...     return {"status": "rejected", "reason": approval.rejection_reason}

ベストプラクティス:
    1. interrupt() 呼び出し前に非冪等操作を行わない
    2. 承認リクエストには十分なコンテキストを含める
    3. タイムアウトと代替フローを設定する
    4. すべての承認/拒否をログに記録する
"""

from agentflow.hitl.types import (
    ApprovalRequest,
    ApprovalResponse,
    ApprovalStatus,
    Command,
    CommandType,
    HITLConfig,
    InterruptPayload,
    InterruptType,
)
from agentflow.hitl.interrupt import (
    interrupt,
    InterruptError,
    InterruptTimeoutError,
    InterruptSignal,
    get_current_interrupt,
    is_interrupted,
    set_checkpointer,
    set_thread_id,
    clear_interrupt,
    resume_with_command,
)
from agentflow.hitl.checkpointer import (
    Checkpointer,
    CheckpointData,
    MemoryCheckpointer,
)
from agentflow.hitl.approval_manager import (
    ApprovalManager,
    ApprovalCallback,
)
from agentflow.hitl.api import create_hitl_router

# 工厂函数: 根据环境变量自动选择 Checkpointer
import logging
import os

_checkpointer_logger = logging.getLogger(__name__)


def get_checkpointer(
    backend: str | None = None,
    **kwargs,
) -> Checkpointer:
    """根据配置自动选择 Checkpointer（松耦合）.

    策略模式: 通过环境变量或参数选择后端实现。
    默认使用内存实现（开发/测试用）。

    Args:
        backend: 强制指定后端 ("memory", "redis", "postgres")
        **kwargs: 传递给具体 Checkpointer 的参数

    Returns:
        Checkpointer 实例

    环境变量优先级:
        1. backend 参数（最高优先）
        2. CHECKPOINTER_BACKEND 环境变量
        3. REDIS_URL → RedisCheckpointer
        4. DATABASE_URL → PostgresCheckpointer
        5. 默认 → MemoryCheckpointer

    使用例:
        >>> # 自动检测
        >>> cp = get_checkpointer()
        >>>
        >>> # 强制使用 Redis
        >>> cp = get_checkpointer(backend="redis", url="redis://localhost:6379")
        >>>
        >>> # 通过环境变量
        >>> # export REDIS_URL=redis://localhost:6379
        >>> cp = get_checkpointer()  # 自动选择 Redis
    """
    # 确定后端类型
    resolved_backend = backend or os.getenv("CHECKPOINTER_BACKEND")

    if not resolved_backend:
        # 根据环境变量自动检测
        if os.getenv("REDIS_URL"):
            resolved_backend = "redis"
        elif os.getenv("DATABASE_URL"):
            resolved_backend = "postgres"
        else:
            resolved_backend = "memory"

    # 创建对应的 Checkpointer
    if resolved_backend == "redis":
        try:
            from agentflow.hitl.redis_checkpointer import RedisCheckpointer
            _checkpointer_logger.info("Using RedisCheckpointer")
            return RedisCheckpointer(**kwargs)
        except ImportError:
            _checkpointer_logger.warning("redis package not installed, falling back to memory")
            return MemoryCheckpointer()

    elif resolved_backend == "postgres":
        try:
            from agentflow.hitl.postgres_checkpointer import PostgresCheckpointer
            _checkpointer_logger.info("Using PostgresCheckpointer")
            return PostgresCheckpointer(**kwargs)
        except ImportError:
            _checkpointer_logger.warning("asyncpg package not installed, falling back to memory")
            return MemoryCheckpointer()

    else:
        _checkpointer_logger.info("Using MemoryCheckpointer")
        return MemoryCheckpointer()


__all__ = [
    # 型定義
    "ApprovalRequest",
    "ApprovalResponse",
    "ApprovalStatus",
    "Command",
    "CommandType",
    "HITLConfig",
    "InterruptPayload",
    "InterruptType",
    # interrupt 関数
    "interrupt",
    "InterruptError",
    "InterruptTimeoutError",
    "InterruptSignal",
    "get_current_interrupt",
    "is_interrupted",
    "set_checkpointer",
    "set_thread_id",
    "clear_interrupt",
    "resume_with_command",
    # Checkpointer
    "Checkpointer",
    "CheckpointData",
    "MemoryCheckpointer",
    "get_checkpointer",
    # ApprovalManager
    "ApprovalManager",
    "ApprovalCallback",
    # API
    "create_hitl_router",
]

