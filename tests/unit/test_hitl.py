"""HITL (Human-in-the-Loop) モジュールのユニットテスト."""

import asyncio
from datetime import datetime

import pytest

from agentflow.hitl import (
    ApprovalManager,
    ApprovalRequest,
    ApprovalResponse,
    ApprovalStatus,
    CheckpointData,
    Command,
    CommandType,
    HITLConfig,
    InterruptPayload,
    InterruptSignal,
    InterruptType,
    MemoryCheckpointer,
)


class TestApprovalTypes:
    """承認関連の型定義テスト."""

    def test_approval_request_creation(self) -> None:
        """ApprovalRequest の作成テスト."""
        request = ApprovalRequest(
            action="delete_user",
            resource_id="user-123",
            reason="ユーザー削除は不可逆操作です",
            context={"user_name": "テストユーザー"},
            priority="high",
        )

        assert request.action == "delete_user"
        assert request.resource_id == "user-123"
        assert request.priority == "high"
        assert request.context["user_name"] == "テストユーザー"
        assert request.id is not None
        assert isinstance(request.created_at, datetime)

    def test_approval_response_approved(self) -> None:
        """承認レスポンス（承認）のテスト."""
        response = ApprovalResponse(
            request_id="req-123",
            status=ApprovalStatus.APPROVED,
            approved=True,
            approver="admin",
        )

        assert response.approved is True
        assert response.status == ApprovalStatus.APPROVED

    def test_approval_response_rejected(self) -> None:
        """承認レスポンス（拒否）のテスト."""
        response = ApprovalResponse(
            request_id="req-123",
            status=ApprovalStatus.REJECTED,
            approved=False,
            rejection_reason="権限不足",
        )

        assert response.approved is False
        assert response.rejection_reason == "権限不足"

    def test_command_approve_factory(self) -> None:
        """Command.approve() ファクトリーメソッドのテスト."""
        cmd = Command.approve(value={"modified": True}, approver="admin")

        assert cmd.type == CommandType.APPROVE
        assert cmd.value == {"modified": True}
        assert cmd.issuer == "admin"

    def test_command_reject_factory(self) -> None:
        """Command.reject() ファクトリーメソッドのテスト."""
        cmd = Command.reject(reason="不適切な操作", rejector="reviewer")

        assert cmd.type == CommandType.REJECT
        assert cmd.value == "不適切な操作"
        assert cmd.issuer == "reviewer"


class TestMemoryCheckpointer:
    """MemoryCheckpointer のテスト."""

    @pytest.fixture
    def checkpointer(self) -> MemoryCheckpointer:
        """テスト用 Checkpointer."""
        return MemoryCheckpointer()

    @pytest.mark.asyncio
    async def test_save_and_load(self, checkpointer: MemoryCheckpointer) -> None:
        """保存と読み込みのテスト."""
        data = CheckpointData(
            checkpoint_id="cp-001",
            thread_id="thread-abc",
            flow_id="flow-123",
            state={"key": "value"},
        )

        saved_id = await checkpointer.save(data)
        assert saved_id == "cp-001"

        loaded = await checkpointer.load("cp-001")
        assert loaded is not None
        assert loaded.thread_id == "thread-abc"
        assert loaded.state["key"] == "value"

    @pytest.mark.asyncio
    async def test_load_latest(self, checkpointer: MemoryCheckpointer) -> None:
        """最新チェックポイント読み込みのテスト."""
        thread_id = "thread-xyz"

        await checkpointer.save(CheckpointData(checkpoint_id="cp-1", thread_id=thread_id, state={"v": 1}))
        await checkpointer.save(CheckpointData(checkpoint_id="cp-2", thread_id=thread_id, state={"v": 2}))

        latest = await checkpointer.load_latest(thread_id)
        assert latest is not None
        assert latest.checkpoint_id == "cp-2"

    @pytest.mark.asyncio
    async def test_delete(self, checkpointer: MemoryCheckpointer) -> None:
        """削除のテスト."""
        await checkpointer.save(CheckpointData(checkpoint_id="cp-del", thread_id="t1", state={}))

        result = await checkpointer.delete("cp-del")
        assert result is True

        loaded = await checkpointer.load("cp-del")
        assert loaded is None

    @pytest.mark.asyncio
    async def test_list_by_thread(self, checkpointer: MemoryCheckpointer) -> None:
        """スレッド別リストのテスト."""
        thread_id = "thread-list"

        await checkpointer.save(CheckpointData(checkpoint_id="a", thread_id=thread_id, state={}))
        await checkpointer.save(CheckpointData(checkpoint_id="b", thread_id=thread_id, state={}))
        await checkpointer.save(CheckpointData(checkpoint_id="c", thread_id="other", state={}))

        items = await checkpointer.list_by_thread(thread_id)
        assert len(items) == 2


class TestApprovalManager:
    """ApprovalManager のテスト."""

    @pytest.fixture
    def manager(self) -> ApprovalManager:
        """テスト用 ApprovalManager."""
        return ApprovalManager(config=HITLConfig(default_timeout_seconds=5))

    @pytest.mark.asyncio
    async def test_request_and_approve(self, manager: ApprovalManager) -> None:
        """承認リクエストと承認のテスト."""
        request = ApprovalRequest(
            action="test_action",
            reason="テスト用",
        )

        # 別タスクで承認を実行
        async def approve_later() -> None:
            await asyncio.sleep(0.1)
            await manager.approve(request.id, approver="admin")

        task = asyncio.create_task(approve_later())

        response = await manager.request_approval(request, timeout_seconds=2, notify=False)
        await task  # タスクの完了を待つ
        assert response.approved is True
        assert response.approver == "admin"

    @pytest.mark.asyncio
    async def test_request_and_reject(self, manager: ApprovalManager) -> None:
        """承認リクエストと拒否のテスト."""
        request = ApprovalRequest(
            action="test_action",
            reason="テスト用",
        )

        async def reject_later() -> None:
            await asyncio.sleep(0.1)
            await manager.reject(request.id, rejector="reviewer", reason="不適切")

        task = asyncio.create_task(reject_later())

        response = await manager.request_approval(request, timeout_seconds=2, notify=False)
        await task  # タスクの完了を待つ
        assert response.approved is False
        assert response.rejection_reason == "不適切"

    @pytest.mark.asyncio
    async def test_request_timeout(self, manager: ApprovalManager) -> None:
        """タイムアウトのテスト."""
        request = ApprovalRequest(
            action="timeout_test",
            reason="タイムアウトテスト",
        )

        response = await manager.request_approval(request, timeout_seconds=1, notify=False)
        assert response.status == ApprovalStatus.EXPIRED
        assert response.approved is False

    def test_get_pending_requests(self, manager: ApprovalManager) -> None:
        """保留中リクエスト取得のテスト."""
        # 初期状態では空
        assert len(manager.get_pending_requests()) == 0


class TestInterruptSignal:
    """InterruptSignal のテスト."""

    def test_interrupt_signal_creation(self) -> None:
        """InterruptSignal の作成テスト."""
        payload = InterruptPayload(
            interrupt_type=InterruptType.APPROVAL,
            request=ApprovalRequest(action="test", reason="test"),
            prompt="承認してください",
        )

        signal = InterruptSignal(payload)
        assert signal.payload == payload
        assert "Interrupt:" in str(signal)

    def test_interrupt_signal_is_exception(self) -> None:
        """InterruptSignal が Exception を継承しているかのテスト."""
        payload = InterruptPayload(
            interrupt_type=InterruptType.CONFIRMATION,
            prompt="確認してください",
        )
        signal = InterruptSignal(payload)

        assert isinstance(signal, Exception)

        # try-except でキャッチできることを確認
        caught = False
        try:
            raise signal
        except InterruptSignal as e:
            caught = True
            assert e.payload.interrupt_type == InterruptType.CONFIRMATION  # noqa: PT017

        assert caught is True


class TestHITLConfig:
    """HITLConfig のテスト."""

    def test_default_config(self) -> None:
        """デフォルト設定のテスト."""
        config = HITLConfig()

        assert config.enabled is True
        assert config.default_timeout_seconds == 3600
        assert config.auto_approve_low_risk is False
        assert "delete" in config.require_approval_for

    def test_custom_config(self) -> None:
        """カスタム設定のテスト."""
        config = HITLConfig(
            enabled=False,
            default_timeout_seconds=1800,
            notification_channels=["slack", "email"],
        )

        assert config.enabled is False
        assert config.default_timeout_seconds == 1800
        assert "slack" in config.notification_channels
