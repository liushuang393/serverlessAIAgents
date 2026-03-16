"""A2A 型定義のユニットテスト."""

from datetime import datetime

from agentflow.protocols.a2a.types import (
    A2ATask,
    A2ATaskState,
    Artifact,
    DataPart,
    FilePart,
    Message,
    Role,
    TaskArtifactUpdateEvent,
    TaskStatus,
    TaskStatusUpdateEvent,
    TextPart,
    TransportProtocol,
)


# ============================================================
# Enum テスト
# ============================================================


class TestA2ATaskState:
    """A2ATaskState enum のテスト."""

    def test_全状態が定義されている(self) -> None:
        assert len(A2ATaskState) == 9
        expected = {
            "submitted",
            "working",
            "input_required",
            "completed",
            "canceled",
            "failed",
            "rejected",
            "auth_required",
            "unknown",
        }
        assert {s.value for s in A2ATaskState} == expected

    def test_str_enumとして文字列比較可能(self) -> None:
        assert A2ATaskState.WORKING == "working"
        assert A2ATaskState.COMPLETED != "failed"


class TestRole:
    """Role enum のテスト."""

    def test_3種類の役割(self) -> None:
        assert Role.AGENT.value == "agent"
        assert Role.USER.value == "user"
        assert Role.SYSTEM.value == "system"


class TestTransportProtocol:
    """TransportProtocol enum のテスト."""

    def test_3種類のトランスポート(self) -> None:
        assert TransportProtocol.JSONRPC.value == "JSONRPC"
        assert TransportProtocol.GRPC.value == "GRPC"
        assert TransportProtocol.HTTP_JSON.value == "HTTP_JSON"


# ============================================================
# Part テスト
# ============================================================


class TestTextPart:
    """TextPart のテスト."""

    def test_作成(self) -> None:
        part = TextPart(text="こんにちは")
        assert part.type == "text"
        assert part.text == "こんにちは"
        assert part.metadata == {}

    def test_メタデータ付き(self) -> None:
        part = TextPart(text="test", metadata={"lang": "ja"})
        assert part.metadata["lang"] == "ja"


class TestFilePart:
    """FilePart のテスト."""

    def test_作成(self) -> None:
        part = FilePart(uri="file:///tmp/test.png")
        assert part.type == "file"
        assert part.uri == "file:///tmp/test.png"
        assert part.mime_type == "application/octet-stream"
        assert part.name is None

    def test_全フィールド指定(self) -> None:
        part = FilePart(uri="s3://bucket/key", mime_type="image/png", name="photo.png")
        assert part.mime_type == "image/png"
        assert part.name == "photo.png"


class TestDataPart:
    """DataPart のテスト."""

    def test_作成(self) -> None:
        part = DataPart(data={"key": "value"})
        assert part.type == "data"
        assert part.data == {"key": "value"}


# ============================================================
# Message テスト
# ============================================================


class TestMessage:
    """Message のテスト."""

    def test_デフォルト作成(self) -> None:
        msg = Message(role=Role.USER, parts=[TextPart(text="質問")])
        assert msg.role == Role.USER
        assert len(msg.parts) == 1
        assert msg.message_id  # UUID が自動生成
        assert msg.task_id is None
        assert msg.context_id is None

    def test_textプロパティ(self) -> None:
        msg = Message(
            role=Role.AGENT,
            parts=[TextPart(text="行1"), DataPart(data={}), TextPart(text="行2")],
        )
        assert msg.text == "行1\n行2"

    def test_from_text(self) -> None:
        msg = Message.from_text("テスト", role=Role.SYSTEM)
        assert msg.role == Role.SYSTEM
        assert msg.text == "テスト"

    def test_from_data(self) -> None:
        data = {"answer": "42"}
        msg = Message.from_data(data)
        assert msg.role == Role.AGENT
        assert len(msg.parts) == 1
        assert isinstance(msg.parts[0], DataPart)

    def test_タイムスタンプが自動設定される(self) -> None:
        msg = Message(role=Role.USER, parts=[])
        assert isinstance(msg.timestamp, datetime)


# ============================================================
# TaskStatus / Artifact テスト
# ============================================================


class TestTaskStatus:
    """TaskStatus のテスト."""

    def test_作成(self) -> None:
        status = TaskStatus(state=A2ATaskState.WORKING)
        assert status.state == A2ATaskState.WORKING
        assert status.message is None
        assert isinstance(status.timestamp, datetime)

    def test_メッセージ付き(self) -> None:
        msg = Message.from_text("処理中...", role=Role.AGENT)
        status = TaskStatus(state=A2ATaskState.WORKING, message=msg)
        assert status.message is not None
        assert status.message.text == "処理中..."


class TestArtifact:
    """Artifact のテスト."""

    def test_デフォルト作成(self) -> None:
        art = Artifact(parts=[TextPart(text="結果")])
        assert art.artifact_id  # UUID 自動生成
        assert art.name is None
        assert art.description is None

    def test_from_text(self) -> None:
        art = Artifact.from_text("テキスト結果", name="output")
        assert len(art.parts) == 1
        assert isinstance(art.parts[0], TextPart)
        assert art.name == "output"

    def test_from_data(self) -> None:
        art = Artifact.from_data({"score": 0.95}, name="score")
        assert len(art.parts) == 1
        assert isinstance(art.parts[0], DataPart)


# ============================================================
# A2ATask テスト
# ============================================================


class TestA2ATask:
    """A2ATask のテスト."""

    def test_デフォルト作成(self) -> None:
        task = A2ATask()
        assert task.id  # UUID 自動生成
        assert task.context_id  # UUID 自動生成
        assert task.status.state == A2ATaskState.SUBMITTED
        assert task.artifacts == []
        assert task.history == []

    def test_終端状態の判定(self) -> None:
        task = A2ATask()
        assert not task.is_terminal

        task.status = TaskStatus(state=A2ATaskState.COMPLETED)
        assert task.is_terminal

        task.status = TaskStatus(state=A2ATaskState.FAILED)
        assert task.is_terminal

        task.status = TaskStatus(state=A2ATaskState.CANCELED)
        assert task.is_terminal

        task.status = TaskStatus(state=A2ATaskState.REJECTED)
        assert task.is_terminal

    def test_非終端状態(self) -> None:
        for state in [
            A2ATaskState.SUBMITTED,
            A2ATaskState.WORKING,
            A2ATaskState.INPUT_REQUIRED,
        ]:
            task = A2ATask()
            task.status = TaskStatus(state=state)
            assert not task.is_terminal


# ============================================================
# ストリーミングイベント テスト
# ============================================================


class TestTaskStatusUpdateEvent:
    """TaskStatusUpdateEvent のテスト."""

    def test_作成(self) -> None:
        status = TaskStatus(state=A2ATaskState.WORKING)
        event = TaskStatusUpdateEvent(task_id="t-1", status=status)
        assert event.type == "status_update"
        assert event.task_id == "t-1"
        assert not event.final

    def test_最終イベント(self) -> None:
        status = TaskStatus(state=A2ATaskState.COMPLETED)
        event = TaskStatusUpdateEvent(task_id="t-1", status=status, final=True)
        assert event.final


class TestTaskArtifactUpdateEvent:
    """TaskArtifactUpdateEvent のテスト."""

    def test_作成(self) -> None:
        art = Artifact.from_text("結果")
        event = TaskArtifactUpdateEvent(task_id="t-1", artifact=art)
        assert event.type == "artifact_update"
        assert event.task_id == "t-1"
        assert event.artifact.parts[0].text == "結果"  # type: ignore[union-attr]
