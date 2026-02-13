"""Agent Lightning 着想の学習連携ヘルパー."""

from __future__ import annotations

from typing import Any, Literal

from agentflow.engines.base import EngineConfig
from agentflow.run import LightningRuntimeConfig, resolve_lightning_store


def score_migration_result(result: dict[str, Any]) -> float | None:
    """移行結果を簡易報酬へ変換.

    報酬方針:
        - 成功で加点、失敗で減点
        - 品質裁定(PASSED/KNOWN_LEGACY など)で補正
        - 反復回数が多いほど軽微に減点
    """
    if not result:
        return None

    score = 0.0
    score += 1.0 if bool(result.get("success")) else -1.0

    quality_gate = result.get("quality_gate", {})
    decision = ""
    if isinstance(quality_gate, dict):
        raw_decision = quality_gate.get("decision")
        if isinstance(raw_decision, str):
            decision = raw_decision

    decision_scores: dict[str, float] = {
        "PASSED": 1.0,
        "KNOWN_LEGACY": 0.4,
        "DESIGN_ISSUE": -0.6,
        "TRANSFORM_ISSUE": -0.6,
        "TEST_ISSUE": -0.4,
        "ENV_ISSUE": -0.3,
    }
    score += decision_scores.get(decision, 0.0)

    iterations = result.get("iterations", 1)
    if isinstance(iterations, int) and iterations > 1:
        score -= min(0.1 * float(iterations - 1), 0.5)

    # 安定したスケールへ丸め込み
    return max(-2.0, min(2.0, score))


def create_lightning_engine_config(
    *,
    name: str = "code_migration_engine",
    enable_collection: bool = False,
    enable_training: bool = False,
    enable_api_optimization: bool = False,
    backend: Literal["auto", "builtin", "microsoft"] = "auto",
    strict_backend: bool = False,
) -> EngineConfig:
    """CodeMigrationEngine 向け既定設定を生成.

    既定では収集/学習を無効化し、必要時のみ opt-in する。
    """
    runtime = LightningRuntimeConfig(
        enabled=enable_collection,
        backend=backend,
        strict_backend=strict_backend,
        enable_training=enable_training,
        enable_api_optimization=enable_api_optimization,
    )
    return EngineConfig(
        name=name,
        lightning=runtime,
        lightning_store=resolve_lightning_store(runtime),
        reward_evaluator=score_migration_result if enable_collection else None,
    )


__all__ = ["create_lightning_engine_config", "score_migration_result"]
