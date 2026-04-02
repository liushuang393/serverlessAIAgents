"""Task harness memory service for Messaging Hub."""

from __future__ import annotations

import logging
from typing import Any

from pydantic import BaseModel, Field

from apps.messaging_hub.task_harness import HarnessPlan, MemoryOwner
from kernel.memory.memory_manager import MemoryManager


_LOGGER = logging.getLogger(__name__)


class HarnessMemorySnapshot(BaseModel):
    """Harness memory snapshot."""

    main_agent: dict[str, list[dict[str, Any]]] = Field(default_factory=dict)
    shared_task: dict[str, list[dict[str, Any]]] = Field(default_factory=dict)
    subagent_private: dict[str, dict[str, list[dict[str, Any]]]] = Field(default_factory=dict)


class HarnessMemoryService:
    """Harness plan に沿った分層記憶サービス."""

    def __init__(self, manager: MemoryManager | None = None) -> None:
        """初期化."""
        self._manager = manager or MemoryManager(
            token_threshold=1500,
            enable_auto_distill=False,
            enable_auto_forget=False,
        )
        self._started = False

    async def start(self) -> None:
        """記憶サービスを開始する."""
        if self._started:
            return
        await self._manager.start()
        self._started = True

    async def stop(self) -> None:
        """記憶サービスを停止する."""
        if not self._started:
            return
        await self._manager.stop()
        self._started = False

    async def snapshot_for_plan(
        self,
        *,
        harness_plan: HarnessPlan,
        task_id: str,
        user_id: str,
        conversation_id: str | None,
        agent_names: list[str] | None = None,
    ) -> HarnessMemorySnapshot:
        """Harness plan に対応する既存記憶を取得する."""
        if not await self._ensure_started():
            return HarnessMemorySnapshot()

        snapshot = HarnessMemorySnapshot()
        for entry in harness_plan.memory_plan.entries:
            if entry.owner == MemoryOwner.SUBAGENT_PRIVATE:
                for agent_name in agent_names or []:
                    topic = self._topic_for_entry(
                        owner=entry.owner,
                        key=entry.key,
                        task_id=task_id,
                        user_id=user_id,
                        conversation_id=conversation_id,
                        agent_name=agent_name,
                    )
                    memories = await self._manager.recall(topic=topic, limit=5)
                    normalized = [
                        self._normalize_memory(memory)
                        for memory in memories
                        if str(memory.metadata.get("memory_key", "")) == entry.key
                    ]
                    if normalized:
                        snapshot.subagent_private.setdefault(agent_name, {})[entry.key] = normalized
                continue

            topic = self._topic_for_entry(
                owner=entry.owner,
                key=entry.key,
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                agent_name=None,
            )
            memories = await self._manager.recall(topic=topic, limit=5)
            normalized = [
                self._normalize_memory(memory)
                for memory in memories
                if str(memory.metadata.get("memory_key", "")) == entry.key
            ]
            if not normalized:
                continue
            if entry.owner == MemoryOwner.MAIN_AGENT:
                snapshot.main_agent[entry.key] = normalized
                continue
            snapshot.shared_task[entry.key] = normalized
        return snapshot

    async def remember_from_plan(
        self,
        *,
        harness_plan: HarnessPlan,
        task_id: str,
        user_id: str,
        conversation_id: str | None,
        values: dict[str, Any],
    ) -> None:
        """Plan 定義に従って main/shared 記憶を保存する."""
        if not await self._ensure_started():
            return

        for entry in harness_plan.memory_plan.entries:
            if entry.owner == MemoryOwner.SUBAGENT_PRIVATE:
                continue
            if entry.key not in values:
                continue
            await self._remember_entry(
                key=entry.key,
                owner=entry.owner,
                description=entry.description,
                content=values[entry.key],
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                agent_name=None,
            )

    async def remember_private(
        self,
        *,
        task_id: str,
        user_id: str,
        conversation_id: str | None,
        agent_name: str,
        key: str,
        content: Any,
        description: str,
    ) -> None:
        """Subagent private 記憶を保存する."""
        if not await self._ensure_started():
            return
        await self._remember_entry(
            key=key,
            owner=MemoryOwner.SUBAGENT_PRIVATE,
            description=description,
            content=content,
            task_id=task_id,
            user_id=user_id,
            conversation_id=conversation_id,
            agent_name=agent_name,
        )

    async def _remember_entry(
        self,
        *,
        key: str,
        owner: MemoryOwner,
        description: str,
        content: Any,
        task_id: str,
        user_id: str,
        conversation_id: str | None,
        agent_name: str | None,
    ) -> None:
        """単一記憶を保存する."""
        topic = self._topic_for_entry(
            owner=owner,
            key=key,
            task_id=task_id,
            user_id=user_id,
            conversation_id=conversation_id,
            agent_name=agent_name,
        )
        text = self._serialize_content(key=key, description=description, content=content)
        metadata = {
            "memory_key": key,
            "owner": owner.value,
            "task_id": task_id,
            "user_id": user_id,
            "conversation_id": conversation_id,
            "agent_id": agent_name,
            "description": description,
        }
        entry = await self._manager.remember(
            text=text,
            topic=topic,
            metadata=metadata,
            source_id=task_id,
        )
        if entry is not None:
            await self._manager._long_term.store(entry)

    @staticmethod
    def _serialize_content(*, key: str, description: str, content: Any) -> str:
        """記憶内容を文字列化する."""
        if isinstance(content, str):
            normalized = content.strip()
        else:
            normalized = repr(content)
        return f"{key}: {description}\n{normalized[:4000]}"

    @staticmethod
    def _topic_for_entry(
        *,
        owner: MemoryOwner,
        key: str,
        task_id: str,
        user_id: str,
        conversation_id: str | None,
        agent_name: str | None,
    ) -> str:
        """owner と key から topic を導出する."""
        if owner == MemoryOwner.SUBAGENT_PRIVATE and agent_name:
            return f"messaging_hub.agent.{agent_name}.{task_id}"
        if owner == MemoryOwner.MAIN_AGENT and key == "user_preferences":
            return f"messaging_hub.user.{user_id}"
        if owner == MemoryOwner.MAIN_AGENT and conversation_id:
            return f"messaging_hub.session.{conversation_id}"
        if owner == MemoryOwner.SHARED_TASK and conversation_id:
            return f"messaging_hub.session.{conversation_id}"
        return f"messaging_hub.flow.{task_id}"

    @staticmethod
    def _normalize_memory(memory: Any) -> dict[str, Any]:
        """MemoryEntry を JSON 向け辞書に変換する."""
        timestamp = getattr(memory, "timestamp", None)
        return {
            "id": str(getattr(memory, "id", "")),
            "content": str(getattr(memory, "content", "")),
            "topic": str(getattr(memory, "topic", "")),
            "metadata": dict(getattr(memory, "metadata", {}) or {}),
            "timestamp": timestamp.isoformat() if timestamp is not None else None,
        }

    async def _ensure_started(self) -> bool:
        """必要なら lazy start する."""
        if self._started:
            return True
        try:
            await self.start()
        except Exception as exc:  # pragma: no cover - defensive
            _LOGGER.warning("Harness memory start failed: %s", exc)
            return False
        return True
