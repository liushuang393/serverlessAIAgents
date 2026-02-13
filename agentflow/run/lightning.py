"""Agent Lightning 着想の実行トレース収集モジュール.

実行(Agent/Flow)と学習(最適化)を疎結合に保つために、
以下の最小機能を提供する。

- Tracer: 実行イベントを標準化して収集
- LightningStore: イベントと報酬を保存
- TrajectoryAdapter: 学習用トランジションへ変換

注記:
    本モジュールは AgentFlow 既存実装を大きく変更せず、
    追加フックで段階導入できる設計になっている。
"""

from __future__ import annotations

import copy
import time
import uuid
from dataclasses import dataclass, field
from typing import Any, Protocol, runtime_checkable


@dataclass(frozen=True)
class LightningEventRecord:
    """実行イベント記録."""

    event_id: str
    run_id: str
    flow_id: str
    timestamp: float
    event_type: str
    status: str
    node_id: str | None = None
    node_name: str | None = None
    payload: dict[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class RewardSignal:
    """報酬信号."""

    reward_id: str
    run_id: str
    flow_id: str
    timestamp: float
    value: float
    source: str = "reward_evaluator"
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class TransitionSample:
    """学習用トランジション(s, a, r, s')."""

    run_id: str
    event_id: str
    step: int
    state: dict[str, Any]
    action: str
    reward: float
    next_state: dict[str, Any]
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass(frozen=True)
class PromptRewardSample:
    """簡易 Prompt/Response/Reward サンプル."""

    run_id: str
    event_id: str
    step: int
    prompt: str
    response: str
    reward: float
    metadata: dict[str, Any] = field(default_factory=dict)


@runtime_checkable
class LightningStore(Protocol):
    """Lightning 互換の最小ストア契約."""

    async def save_event(self, event: LightningEventRecord) -> None:
        """イベントを保存."""
        ...

    async def save_reward(self, reward: RewardSignal) -> None:
        """報酬を保存."""
        ...

    async def list_events(self, run_id: str) -> list[LightningEventRecord]:
        """実行IDに紐づくイベント一覧を取得."""
        ...

    async def list_rewards(self, run_id: str) -> list[RewardSignal]:
        """実行IDに紐づく報酬一覧を取得."""
        ...

    async def list_run_ids(self) -> list[str]:
        """保存済み実行ID一覧を取得."""
        ...


@dataclass
class MemoryLightningStore(LightningStore):
    """インメモリ実装の LightningStore."""

    _events: dict[str, list[LightningEventRecord]] = field(default_factory=dict)
    _rewards: dict[str, list[RewardSignal]] = field(default_factory=dict)
    _run_ids: list[str] = field(default_factory=list)

    async def save_event(self, event: LightningEventRecord) -> None:
        """イベントを保存."""
        self._events.setdefault(event.run_id, []).append(event)
        self._remember_run_id(event.run_id)

    async def save_reward(self, reward: RewardSignal) -> None:
        """報酬を保存."""
        self._rewards.setdefault(reward.run_id, []).append(reward)
        self._remember_run_id(reward.run_id)

    async def list_events(self, run_id: str) -> list[LightningEventRecord]:
        """実行IDに紐づくイベント一覧を取得."""
        return list(self._events.get(run_id, []))

    async def list_rewards(self, run_id: str) -> list[RewardSignal]:
        """実行IDに紐づく報酬一覧を取得."""
        return list(self._rewards.get(run_id, []))

    async def list_run_ids(self) -> list[str]:
        """保存済み実行ID一覧を取得."""
        return list(self._run_ids)

    def _remember_run_id(self, run_id: str) -> None:
        if run_id not in self._run_ids:
            self._run_ids.append(run_id)


class LightningTracer:
    """実行イベントを標準化して LightningStore に保存."""

    _EVENT_TYPE_KEYS: tuple[str, ...] = ("event_type", "type", "event")

    def __init__(self, store: LightningStore) -> None:
        """初期化."""
        self._store = store

    async def record_event(
        self,
        *,
        run_id: str,
        flow_id: str,
        event: dict[str, Any],
    ) -> LightningEventRecord:
        """イベントを正規化して保存."""
        event_type = self._resolve_event_type(event)
        node_id, node_name = self._resolve_node_info(event)
        status = self._resolve_status(event_type, event)

        record = LightningEventRecord(
            event_id=f"evt-{uuid.uuid4().hex[:12]}",
            run_id=run_id,
            flow_id=flow_id,
            timestamp=float(event.get("timestamp", time.time())),
            event_type=event_type,
            status=status,
            node_id=node_id,
            node_name=node_name,
            payload=copy.deepcopy(event),
        )
        await self._store.save_event(record)
        return record

    async def record_custom_event(
        self,
        *,
        run_id: str,
        flow_id: str,
        event_type: str,
        payload: dict[str, Any] | None = None,
        node_id: str | None = None,
        node_name: str | None = None,
        status: str = "ok",
    ) -> LightningEventRecord:
        """明示的イベントを保存."""
        record = LightningEventRecord(
            event_id=f"evt-{uuid.uuid4().hex[:12]}",
            run_id=run_id,
            flow_id=flow_id,
            timestamp=time.time(),
            event_type=event_type,
            status=status,
            node_id=node_id,
            node_name=node_name,
            payload=copy.deepcopy(payload) if payload else {},
        )
        await self._store.save_event(record)
        return record

    async def record_reward(
        self,
        *,
        run_id: str,
        flow_id: str,
        value: float,
        source: str = "reward_evaluator",
        metadata: dict[str, Any] | None = None,
    ) -> RewardSignal:
        """報酬を保存."""
        reward = RewardSignal(
            reward_id=f"rew-{uuid.uuid4().hex[:12]}",
            run_id=run_id,
            flow_id=flow_id,
            timestamp=time.time(),
            value=float(value),
            source=source,
            metadata=copy.deepcopy(metadata) if metadata else {},
        )
        await self._store.save_reward(reward)
        return reward

    def _resolve_event_type(self, event: dict[str, Any]) -> str:
        for key in self._EVENT_TYPE_KEYS:
            value = event.get(key)
            if isinstance(value, str) and value:
                return value
        return "unknown"

    def _resolve_node_info(self, event: dict[str, Any]) -> tuple[str | None, str | None]:
        node_id = event.get("node_id") or event.get("node")
        node_name = event.get("node_name") or event.get("node")

        data = event.get("data")
        if isinstance(data, dict):
            if node_id is None:
                node_id = data.get("node_id") or data.get("stage")
            if node_name is None:
                node_name = data.get("node_name") or data.get("stage")

        node_id = node_id.strip() or None if isinstance(node_id, str) else None
        node_name = node_name.strip() or None if isinstance(node_name, str) else None

        return node_id, node_name

    def _resolve_status(self, event_type: str, event: dict[str, Any]) -> str:
        lowered = event_type.lower()
        if "error" in lowered:
            return "error"
        data = event.get("data")
        if isinstance(data, dict) and data.get("success") is False:
            return "error"
        return "ok"


class TrajectoryAdapter:
    """実行イベントを学習データへ変換."""

    @staticmethod
    def to_transition_samples(
        *,
        events: list[LightningEventRecord],
        rewards: list[RewardSignal],
    ) -> list[TransitionSample]:
        """ノード完了イベントからトランジションを生成."""
        node_events = [
            event
            for event in sorted(events, key=lambda item: item.timestamp)
            if event.event_type in {"node.complete", "node_complete"}
        ]
        if not node_events:
            return []

        reward_by_node: dict[str, float] = {}
        terminal_reward = 0.0
        for reward in rewards:
            node_id = reward.metadata.get("node_id")
            if isinstance(node_id, str) and node_id:
                reward_by_node[node_id] = reward_by_node.get(node_id, 0.0) + reward.value
            else:
                terminal_reward += reward.value

        transitions: list[TransitionSample] = []
        previous_state: dict[str, Any] = {}
        last_index = len(node_events) - 1

        for index, event in enumerate(node_events):
            next_state = TrajectoryAdapter._extract_state(event.payload)
            node_key = event.node_id or event.node_name or f"step-{index}"
            reward = reward_by_node.get(node_key, 0.0)
            if index == last_index:
                reward += terminal_reward

            transitions.append(
                TransitionSample(
                    run_id=event.run_id,
                    event_id=event.event_id,
                    step=index,
                    state=copy.deepcopy(previous_state),
                    action=node_key,
                    reward=reward,
                    next_state=copy.deepcopy(next_state),
                    metadata={
                        "event_type": event.event_type,
                        "node_name": event.node_name or "",
                    },
                )
            )
            previous_state = next_state

        return transitions

    @staticmethod
    def to_prompt_reward_samples(
        *,
        events: list[LightningEventRecord],
        rewards: list[RewardSignal],
    ) -> list[PromptRewardSample]:
        """Prompt/Response/Reward 形式へ変換."""
        transitions = TrajectoryAdapter.to_transition_samples(events=events, rewards=rewards)
        if not transitions:
            return []

        samples: list[PromptRewardSample] = []
        for transition in transitions:
            prompt = str(transition.state.get("prompt", transition.state.get("question", "")))
            response = str(
                transition.next_state.get(
                    "response",
                    transition.next_state.get("answer", transition.next_state.get("result", "")),
                )
            )
            samples.append(
                PromptRewardSample(
                    run_id=transition.run_id,
                    event_id=transition.event_id,
                    step=transition.step,
                    prompt=prompt,
                    response=response,
                    reward=transition.reward,
                    metadata=copy.deepcopy(transition.metadata),
                )
            )
        return samples

    @staticmethod
    def _extract_state(event_payload: dict[str, Any]) -> dict[str, Any]:
        data = event_payload.get("data")
        if isinstance(data, dict):
            return copy.deepcopy(data)
        result = event_payload.get("result")
        if isinstance(result, dict):
            return copy.deepcopy(result)
        return {}


__all__ = [
    "LightningEventRecord",
    "LightningStore",
    "LightningTracer",
    "MemoryLightningStore",
    "PromptRewardSample",
    "RewardSignal",
    "TrajectoryAdapter",
    "TransitionSample",
]
