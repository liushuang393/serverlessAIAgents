# -*- coding: utf-8 -*-
"""Provisioning スキーマのユニットテスト."""

from __future__ import annotations

import pytest
from pydantic import ValidationError

from apps.platform.schemas.provisioning_schemas import AppCreateRequest


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
