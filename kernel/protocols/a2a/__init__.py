"""A2A プロトコル準拠モジュール.

AgentFlow の A2A 実装を python_a2a SDK 水準に引き上げるための
型定義・タスク管理・イベントシステム・AG-UI/A2UI ブリッジを提供する。
"""

from kernel.protocols.a2a.agent_executor import (
    A2AAgentExecutor,
    A2ARequestContext,
    ResilientAgentExecutor,
)
from kernel.protocols.a2a.agui_bridge import A2AToAGUIBridge
from kernel.protocols.a2a.event_queue import A2AEventQueue, A2AQueueManager
from kernel.protocols.a2a.state_bridge import (
    a2a_to_agentflow_state,
    agentflow_to_a2a_state,
)
from kernel.protocols.a2a.task_manager import A2ATaskManager
from kernel.protocols.a2a.task_store import (
    A2ATaskStore,
    InMemoryA2ATaskStore,
)
from kernel.protocols.a2a.types import (
    A2ATask,
    A2ATaskState,
    Artifact,
    DataPart,
    FilePart,
    Message,
    Part,
    Role,
    TaskArtifactUpdateEvent,
    TaskStatus,
    TaskStatusUpdateEvent,
    TextPart,
    TransportProtocol,
)


__all__ = [
    # Agent 実行
    "A2AAgentExecutor",
    # イベントキュー
    "A2AEventQueue",
    "A2AQueueManager",
    "A2ARequestContext",
    # 型
    "A2ATask",
    # タスク管理
    "A2ATaskManager",
    "A2ATaskState",
    "A2ATaskStore",
    # AG-UI/A2UI ブリッジ
    "A2AToAGUIBridge",
    "Artifact",
    "DataPart",
    "FilePart",
    "InMemoryA2ATaskStore",
    "Message",
    "Part",
    "ResilientAgentExecutor",
    "Role",
    "TaskArtifactUpdateEvent",
    "TaskStatus",
    "TaskStatusUpdateEvent",
    "TextPart",
    "TransportProtocol",
    # 状態ブリッジ
    "a2a_to_agentflow_state",
    "agentflow_to_a2a_state",
]
