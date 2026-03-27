"""A2A プロトコル準拠の型定義.

python_a2a SDK の型体系に合わせた Pydantic モデル群。
AgentFlow 内部のタスク管理・イベント配信・Agent 間通信の基盤となる。
"""

from __future__ import annotations

import uuid
from datetime import UTC, datetime
from enum import StrEnum
from typing import Annotated, Any

from pydantic import BaseModel, Field


# ============================================================
# Enum 定義
# ============================================================


class A2ATaskState(StrEnum):
    """A2A タスク状態.

    python_a2a SDK の TaskState に準拠。
    AgentFlow の TaskState（11 状態）とは state_bridge.py で相互変換する。
    """

    SUBMITTED = "submitted"
    WORKING = "working"
    INPUT_REQUIRED = "input_required"
    COMPLETED = "completed"
    CANCELED = "canceled"
    FAILED = "failed"
    REJECTED = "rejected"
    AUTH_REQUIRED = "auth_required"
    UNKNOWN = "unknown"


class Role(StrEnum):
    """メッセージ送信者の役割."""

    AGENT = "agent"
    USER = "user"
    SYSTEM = "system"


class TransportProtocol(StrEnum):
    """A2A 通信トランスポート."""

    JSONRPC = "JSONRPC"
    GRPC = "GRPC"
    HTTP_JSON = "HTTP_JSON"


# ============================================================
# Part（メッセージ/アーティファクトの構成要素）
# ============================================================


class TextPart(BaseModel):
    """テキストコンテンツ."""

    type: str = Field(default="text", description="パート種別")
    text: str = Field(..., description="テキスト内容")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class FilePart(BaseModel):
    """ファイルコンテンツ（URI またはバイナリ参照）."""

    type: str = Field(default="file", description="パート種別")
    uri: str = Field(..., description="ファイル URI")
    mime_type: str = Field(default="application/octet-stream", description="MIME タイプ")
    name: str | None = Field(default=None, description="ファイル名")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class DataPart(BaseModel):
    """構造化データコンテンツ."""

    type: str = Field(default="data", description="パート種別")
    data: dict[str, Any] = Field(default_factory=dict, description="構造化データ")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


# Union 型（判別キーは type フィールド）
Part = Annotated[TextPart | FilePart | DataPart, Field(discriminator="type")]


# ============================================================
# Message（Agent 間メッセージ）
# ============================================================


class Message(BaseModel):
    """A2A メッセージ.

    Agent 間通信の基本単位。parts[] でマルチモーダルコンテンツを格納する。
    """

    message_id: str = Field(
        default_factory=lambda: str(uuid.uuid4()),
        description="メッセージ ID",
    )
    role: Role = Field(..., description="送信者の役割")
    parts: list[TextPart | FilePart | DataPart] = Field(
        default_factory=list,
        description="メッセージコンテンツ",
    )
    task_id: str | None = Field(default=None, description="関連タスク ID")
    context_id: str | None = Field(default=None, description="会話/セッション ID")
    parent_message_id: str | None = Field(default=None, description="親メッセージ ID")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")
    timestamp: datetime = Field(
        default_factory=lambda: datetime.now(tz=UTC),
        description="タイムスタンプ",
    )

    @property
    def text(self) -> str:
        """全 TextPart のテキストを結合して返す（便利プロパティ）."""
        return "\n".join(p.text for p in self.parts if isinstance(p, TextPart))

    @classmethod
    def from_text(cls, text: str, *, role: Role = Role.USER, **kwargs: Any) -> Message:
        """テキストからメッセージを簡易作成."""
        return cls(role=role, parts=[TextPart(text=text)], **kwargs)

    @classmethod
    def from_data(cls, data: dict[str, Any], *, role: Role = Role.AGENT, **kwargs: Any) -> Message:
        """構造化データからメッセージを簡易作成."""
        return cls(role=role, parts=[DataPart(data=data)], **kwargs)


# ============================================================
# TaskStatus / Artifact
# ============================================================


class TaskStatus(BaseModel):
    """タスクの現在状態."""

    state: A2ATaskState = Field(..., description="タスク状態")
    message: Message | None = Field(default=None, description="状態に関するメッセージ")
    timestamp: datetime = Field(
        default_factory=lambda: datetime.now(tz=UTC),
        description="状態更新時刻",
    )


class Artifact(BaseModel):
    """タスク成果物.

    Agent の出力をマルチモーダルな parts[] として格納する。
    """

    artifact_id: str = Field(
        default_factory=lambda: str(uuid.uuid4()),
        description="アーティファクト ID",
    )
    parts: list[TextPart | FilePart | DataPart] = Field(
        default_factory=list,
        description="成果物コンテンツ",
    )
    name: str | None = Field(default=None, description="成果物名")
    description: str | None = Field(default=None, description="成果物の説明")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")

    @classmethod
    def from_text(cls, text: str, **kwargs: Any) -> Artifact:
        """テキストから簡易作成."""
        return cls(parts=[TextPart(text=text)], **kwargs)

    @classmethod
    def from_data(cls, data: dict[str, Any], **kwargs: Any) -> Artifact:
        """構造化データから簡易作成."""
        return cls(parts=[DataPart(data=data)], **kwargs)


# ============================================================
# A2ATask（タスク本体）
# ============================================================


class A2ATask(BaseModel):
    """A2A タスク.

    Agent への実行リクエストとその結果を管理する。
    """

    id: str = Field(
        default_factory=lambda: str(uuid.uuid4()),
        description="タスク ID",
    )
    context_id: str = Field(
        default_factory=lambda: str(uuid.uuid4()),
        description="会話/セッション ID",
    )
    status: TaskStatus = Field(
        default_factory=lambda: TaskStatus(state=A2ATaskState.SUBMITTED),
        description="現在の状態",
    )
    artifacts: list[Artifact] = Field(default_factory=list, description="成果物リスト")
    history: list[Message] = Field(default_factory=list, description="メッセージ履歴")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")

    @property
    def is_terminal(self) -> bool:
        """終端状態かどうか."""
        return self.status.state in {
            A2ATaskState.COMPLETED,
            A2ATaskState.FAILED,
            A2ATaskState.CANCELED,
            A2ATaskState.REJECTED,
        }


# ============================================================
# ストリーミングイベント
# ============================================================


class TaskStatusUpdateEvent(BaseModel):
    """タスク状態更新イベント（ストリーミング用）."""

    type: str = Field(default="status_update", description="イベント種別")
    task_id: str = Field(..., description="タスク ID")
    status: TaskStatus = Field(..., description="更新後の状態")
    final: bool = Field(default=False, description="最終イベントかどうか")


class TaskArtifactUpdateEvent(BaseModel):
    """タスクアーティファクト更新イベント（ストリーミング用）."""

    type: str = Field(default="artifact_update", description="イベント種別")
    task_id: str = Field(..., description="タスク ID")
    artifact: Artifact = Field(..., description="追加されたアーティファクト")
