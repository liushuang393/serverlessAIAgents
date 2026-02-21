"""Provisioning スキーマのユニットテスト."""

from __future__ import annotations

import pytest
from apps.platform.schemas.provisioning_schemas import AppCreateRequest
from pydantic import ValidationError


class TestAppCreateRequest:
    """AppCreateRequest の検証テスト."""

    def test_requires_profile_fields(self) -> None:
        """分類フィールドは必須."""
        with pytest.raises(ValidationError):
            AppCreateRequest(name="my_app", display_name="My App")

    def test_profile_fields_are_normalized(self) -> None:
        """分類フィールドは小文字へ正規化される."""
        req = AppCreateRequest(
            name="studio_app",
            display_name="Studio App",
            product_line="Migration",
            surface_profile="Business",
            audit_profile="Developer",
            plugin_bindings=[],
        )
        assert req.product_line == "migration"
        assert req.surface_profile == "business"
        assert req.audit_profile == "developer"

    def test_plugin_bindings_is_required(self) -> None:
        """plugin_bindings は明示必須."""
        with pytest.raises(ValidationError, match="plugin_bindings"):
            AppCreateRequest(
                name="studio_app",
                display_name="Studio App",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
            )

    def test_assistant_requires_security_mode(self) -> None:
        """assistant は security_mode 必須."""
        with pytest.raises(ValidationError, match="security_mode"):
            AppCreateRequest(
                name="assistant_app",
                display_name="Assistant App",
                product_line="assistant",
                surface_profile="business",
                audit_profile="business",
                plugin_bindings=[],
            )

    def test_rag_requires_vector_db(self) -> None:
        """rag_enabled=true で vector_database=none は拒否する."""
        with pytest.raises(ValidationError, match="vector_database"):
            AppCreateRequest(
                name="rag_app",
                display_name="RAG App",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
                plugin_bindings=[],
                rag_enabled=True,
                vector_database="none",
            )

    def test_evolution_config_is_accepted(self) -> None:
        """Evolution 設定を受け付ける."""
        req = AppCreateRequest(
            name="evo_app",
            display_name="Evolution App",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
            evolution={
                "enabled": True,
                "strategy_service_url": "http://localhost:8089",
                "validator_queue": {
                    "backend": "redis_stream",
                    "redis_url": "redis://localhost:6379/0",
                    "stream_key": "evolution:validate:stream",
                    "consumer_group": "evolution-validator-v1",
                    "max_retries": 5,
                },
                "scope_policy": ["tenant_app", "tenant_product_line", "global_verified"],
                "retrieval": {
                    "high_confidence_skip_threshold": 0.82,
                    "high_complexity_threshold": 0.7,
                    "low_confidence_threshold": 0.55,
                },
                "suspicion": {
                    "max_age_days": 30,
                    "failure_streak_threshold": 2,
                    "performance_drop_ratio": 0.2,
                },
            },
        )
        assert req.evolution is not None
        assert req.evolution.validator_queue.backend == "redis_stream"
