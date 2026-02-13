"""Agent Lightning backend integration module.

Keep existing AgentFlow execution interfaces unchanged while allowing
training and optimization backends to be swapped.

Policy:
    - Keep external contracts (`LightningStore`, `TrajectoryAdapter`)
    - Disabled by default (opt-in)
    - Prefer Microsoft Agent Lightning when available
    - Continue via builtin fallback when unavailable
"""

from __future__ import annotations

import importlib
import logging
from statistics import mean
from typing import Any, Literal

from pydantic import BaseModel, Field

from agentflow.run.lightning import (
    LightningEventRecord,
    LightningStore,
    MemoryLightningStore,
    PromptRewardSample,
    RewardSignal,
)


logger = logging.getLogger(__name__)

LightningBackendName = Literal["auto", "builtin", "microsoft"]
POSITIVE_REWARD_THRESHOLD = 0.8
QUALITY_FIRST_RATIO_THRESHOLD = 0.6


class LightningRuntimeConfig(BaseModel):
    """Lightning 実行/学習設定."""

    enabled: bool = Field(default=False, description="トレース/報酬収集を有効化")
    backend: LightningBackendName = Field(default="auto", description="実装バックエンド")
    strict_backend: bool = Field(
        default=False,
        description="指定バックエンド利用に失敗した場合に例外化",
    )
    enable_training: bool = Field(default=False, description="学習処理を有効化")
    enable_api_optimization: bool = Field(
        default=False,
        description="推論/APIフロー最適化プロファイルを生成",
    )
    algorithm: str = Field(default="apo", min_length=1, description="学習アルゴリズム名")
    strategy: str = Field(default="single", min_length=1, description="実行戦略")
    trainer_kwargs: dict[str, Any] = Field(default_factory=dict, description="Trainer 追加設定")


class LightningTrainingRequest(BaseModel):
    """Lightning 学習リクエスト."""

    run_id: str | None = Field(default=None, description="target run_id (latest if omitted)")
    max_samples: int = Field(default=2000, gt=0, le=100_000, description="最大サンプル数")
    backend: LightningBackendName | None = Field(default=None, description="バックエンド上書き")
    algorithm: str | None = Field(default=None, description="アルゴリズム上書き")
    apply_optimized_profile: bool = Field(
        default=True,
        description="最適化プロファイルを Engine の llm_config へ反映",
    )


class LightningTrainingResult(BaseModel):
    """Lightning 学習結果."""

    success: bool = Field(..., description="処理成功可否")
    backend: str = Field(..., description="実際に使用したバックエンド")
    trained: bool = Field(default=False, description="重み/方策学習を実行したか")
    optimized: bool = Field(default=False, description="最適化プロファイルを生成したか")
    num_samples: int = Field(default=0, ge=0, description="使用サンプル数")
    metrics: dict[str, float] = Field(default_factory=dict, description="集計メトリクス")
    optimized_llm_profile: dict[str, Any] = Field(
        default_factory=dict,
        description="推論最適化プロファイル",
    )
    details: dict[str, Any] = Field(default_factory=dict, description="補助情報")
    message: str = Field(default="", description="結果メッセージ")


class MicrosoftLightningStore(LightningStore):
    """Microsoft Agent Lightning ストアを併用する互換ストア."""

    def __init__(
        self,
        *,
        microsoft_store: Any,
        emit_object: Any,
        emit_reward: Any,
        trace_context: Any,
    ) -> None:
        """初期化."""
        self._store = MemoryLightningStore()
        self._microsoft_store = microsoft_store
        self._emit_object = emit_object
        self._emit_reward = emit_reward
        self._trace_context = trace_context
        self._attempt_by_run: dict[str, int] = {}

    async def save_event(self, event: LightningEventRecord) -> None:
        """イベント保存."""
        await self._store.save_event(event)
        self._forward_event_to_microsoft(event)

    async def save_reward(self, reward: RewardSignal) -> None:
        """報酬保存."""
        await self._store.save_reward(reward)
        self._forward_reward_to_microsoft(reward)

    async def list_events(self, run_id: str) -> list[LightningEventRecord]:
        """run_id 単位イベント取得."""
        return await self._store.list_events(run_id)

    async def list_rewards(self, run_id: str) -> list[RewardSignal]:
        """run_id 単位報酬取得."""
        return await self._store.list_rewards(run_id)

    async def list_run_ids(self) -> list[str]:
        """保存済み run_id 一覧."""
        return await self._store.list_run_ids()

    def _forward_event_to_microsoft(self, event: LightningEventRecord) -> None:
        try:
            payload = {
                "event_id": event.event_id,
                "event_type": event.event_type,
                "status": event.status,
                "node_id": event.node_id,
                "node_name": event.node_name,
                "payload": event.payload,
                "timestamp": event.timestamp,
            }
            with self._trace_context(
                store=self._microsoft_store,
                rollout_id=event.run_id,
                attempt_id=self._get_attempt_id(event.run_id),
            ):
                self._emit_object(payload)
        except Exception as exc:
            logger.debug("microsoft store event forward failed: %s", exc)

    def _forward_reward_to_microsoft(self, reward: RewardSignal) -> None:
        try:
            with self._trace_context(
                store=self._microsoft_store,
                rollout_id=reward.run_id,
                attempt_id=self._get_attempt_id(reward.run_id),
            ):
                self._emit_reward(float(reward.value))
        except Exception as exc:
            logger.debug("microsoft store reward forward failed: %s", exc)

    def _get_attempt_id(self, run_id: str) -> int:
        if run_id not in self._attempt_by_run:
            self._attempt_by_run[run_id] = len(self._attempt_by_run)
        return self._attempt_by_run[run_id]


def is_microsoft_lightning_available() -> bool:
    """Microsoft Agent Lightning ライブラリの利用可否."""
    return _import_agentlightning() is not None


def resolve_lightning_store(config: LightningRuntimeConfig | None) -> LightningStore | None:
    """設定に応じてストア実装を解決."""
    runtime = config or LightningRuntimeConfig()
    if not runtime.enabled:
        return None

    if runtime.backend in {"auto", "microsoft"}:
        microsoft_store = _create_microsoft_store()
        if microsoft_store is not None:
            return microsoft_store
        if runtime.backend == "microsoft" and runtime.strict_backend:
            msg = "Microsoft Agent Lightning backend is required but not available"
            raise RuntimeError(msg)

    return MemoryLightningStore()


async def train_with_lightning_backend(
    *,
    samples: list[PromptRewardSample],
    runtime: LightningRuntimeConfig,
) -> LightningTrainingResult:
    """サンプルを学習/最適化バックエンドへ投入."""
    if not runtime.enable_training:
        return LightningTrainingResult(
            success=True,
            backend="disabled",
            trained=False,
            optimized=False,
            num_samples=len(samples),
            message="training is disabled by runtime config",
        )

    if not samples:
        return LightningTrainingResult(
            success=True,
            backend="none",
            trained=False,
            optimized=False,
            num_samples=0,
            message="no samples to train",
        )

    if runtime.backend in {"auto", "microsoft"}:
        result = await _train_with_microsoft(samples=samples, runtime=runtime)
        if result.success:
            return result
        if runtime.backend == "microsoft" and runtime.strict_backend:
            return result

    return _train_with_builtin(samples=samples, runtime=runtime)


def build_optimized_llm_profile(samples: list[PromptRewardSample]) -> dict[str, Any]:
    """報酬分布から簡易 LLM 最適化プロファイルを生成."""
    if not samples:
        return {}

    rewards = [sample.reward for sample in samples]
    avg_reward = mean(rewards)
    positive_ratio = sum(1 for reward in rewards if reward > 0) / max(1, len(rewards))

    if avg_reward >= POSITIVE_REWARD_THRESHOLD:
        temperature = 0.2
    elif avg_reward >= 0.0:
        temperature = 0.35
    else:
        temperature = 0.15

    routing_mode = (
        "quality_first"
        if positive_ratio < QUALITY_FIRST_RATIO_THRESHOLD
        else "balanced"
    )
    max_tokens = 4096 if positive_ratio < QUALITY_FIRST_RATIO_THRESHOLD else 3072

    return {
        "temperature": temperature,
        "max_tokens": max_tokens,
        "routing": {
            "mode": routing_mode,
            "min_confidence": 0.55 if routing_mode == "quality_first" else 0.45,
        },
    }


async def _train_with_microsoft(
    *,
    samples: list[PromptRewardSample],
    runtime: LightningRuntimeConfig,
) -> LightningTrainingResult:
    module = _import_agentlightning()
    if module is None:
        return LightningTrainingResult(
            success=False,
            backend="microsoft",
            trained=False,
            optimized=False,
            num_samples=len(samples),
            message="microsoft agent lightning package is not installed",
        )

    dataset = [
        {
            "prompt": sample.prompt,
            "reference_response": sample.response,
            "reward": sample.reward,
            "metadata": sample.metadata,
        }
        for sample in samples
    ]

    def _policy(task: dict[str, Any]) -> str:
        return str(task.get("reference_response", ""))

    try:
        agent = module.rollout(_policy)
        trainer_kwargs = dict(runtime.trainer_kwargs)
        trainer_kwargs.setdefault("strategy", runtime.strategy)
        trainer_kwargs.setdefault("algorithm", runtime.algorithm)

        if "store" not in trainer_kwargs and hasattr(module, "InMemoryLightningStore"):
            trainer_kwargs["store"] = module.InMemoryLightningStore(thread_safe=False)

        trainer = module.Trainer(**trainer_kwargs)
        fit_output = trainer.fit(agent, train_dataset=dataset)

        profile = (
            build_optimized_llm_profile(samples)
            if runtime.enable_api_optimization
            else {}
        )
        rewards = [sample.reward for sample in samples]
        metrics = {
            "avg_reward": float(mean(rewards)),
            "max_reward": float(max(rewards)),
            "min_reward": float(min(rewards)),
        }
        return LightningTrainingResult(
            success=True,
            backend="microsoft",
            trained=True,
            optimized=bool(profile),
            num_samples=len(samples),
            metrics=metrics,
            optimized_llm_profile=profile,
            details={"fit_output": str(fit_output)},
            message="training completed with microsoft backend",
        )
    except Exception as exc:
        logger.warning("microsoft lightning training failed: %s", exc)
        return LightningTrainingResult(
            success=False,
            backend="microsoft",
            trained=False,
            optimized=False,
            num_samples=len(samples),
            message=f"microsoft backend failed: {exc}",
        )


def _train_with_builtin(
    *,
    samples: list[PromptRewardSample],
    runtime: LightningRuntimeConfig,
) -> LightningTrainingResult:
    rewards = [sample.reward for sample in samples]
    avg_reward = float(mean(rewards))
    profile = (
        build_optimized_llm_profile(samples)
        if runtime.enable_api_optimization
        else {}
    )

    return LightningTrainingResult(
        success=True,
        backend="builtin",
        trained=False,
        optimized=bool(profile),
        num_samples=len(samples),
        metrics={
            "avg_reward": avg_reward,
            "max_reward": float(max(rewards)),
            "min_reward": float(min(rewards)),
        },
        optimized_llm_profile=profile,
        message="builtin fallback completed (no external trainer)",
    )


def _create_microsoft_store() -> LightningStore | None:
    module = _import_agentlightning()
    if module is None:
        return None
    try:
        store = module.InMemoryLightningStore(thread_safe=False)
        return MicrosoftLightningStore(
            microsoft_store=store,
            emit_object=module.emit_object,
            emit_reward=module.emit_reward,
            trace_context=module.trace_context,
        )
    except Exception as exc:
        logger.warning("failed to initialize microsoft lightning store: %s", exc)
        return None


def _import_agentlightning() -> Any | None:
    for module_name in ("agentlightning", "agent_lightning"):
        try:
            return importlib.import_module(module_name)
        except ImportError:
            continue
    return None


__all__ = [
    "LightningRuntimeConfig",
    "LightningTrainingRequest",
    "LightningTrainingResult",
    "build_optimized_llm_profile",
    "is_microsoft_lightning_available",
    "resolve_lightning_store",
    "train_with_lightning_backend",
]
