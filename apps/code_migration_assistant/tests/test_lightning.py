"""Code Migration Assistant の Lightning 連携テスト."""

from __future__ import annotations

import pytest
from apps.code_migration_assistant.engine import CodeMigrationEngine
from apps.code_migration_assistant.lightning import (
    create_lightning_engine_config,
    score_migration_result,
)


EXPECTED_MAX_SCORE = 2.0


def test_score_migration_result_passed() -> None:
    result = {
        "success": True,
        "iterations": 1,
        "quality_gate": {"decision": "PASSED"},
    }
    assert score_migration_result(result) == EXPECTED_MAX_SCORE


def test_score_migration_result_failed() -> None:
    result = {
        "success": False,
        "iterations": 3,
        "quality_gate": {"decision": "TRANSFORM_ISSUE"},
    }
    score = score_migration_result(result)
    assert score is not None
    assert score < 0.0


def test_engine_default_config_disables_lightning_hooks() -> None:
    engine = CodeMigrationEngine()
    assert engine.config.lightning.enabled is False
    assert engine.config.lightning.enable_training is False
    assert engine.config.lightning_store is None
    assert engine.config.reward_evaluator is None


def test_engine_opt_in_config_enables_lightning_hooks() -> None:
    config = create_lightning_engine_config(enable_collection=True, enable_training=True)
    engine = CodeMigrationEngine(config=config)
    assert engine.config.lightning_store is not None
    assert engine.config.reward_evaluator is not None


@pytest.mark.asyncio
async def test_engine_train_latest_run_without_samples() -> None:
    config = create_lightning_engine_config(enable_collection=True, enable_training=True)
    engine = CodeMigrationEngine(config=config)
    result = await engine.train_latest_run()
    assert result["success"] is True
    assert result["trained"] is False
    assert result["num_samples"] == 0
