"""ResourceDefinition モデルのユニットテスト."""

from __future__ import annotations

from shared.auth_service.models.authorization import ResourceDefinition


class TestResourceDefinitionModel:
    """ResourceDefinition モデルの基本検証."""

    def test_tablename(self) -> None:
        assert ResourceDefinition.__tablename__ == "resource_definitions"

    def test_required_fields(self) -> None:
        """必須カラムが定義されている."""
        columns = {c.name for c in ResourceDefinition.__table__.columns}
        required = {
            "id",
            "resource_type",
            "resource_id",
            "display_name",
            "app_name",
            "scope",
            "backend_key",
            "metadata_json",
            "is_active",
            "created_at",
            "updated_at",
        }
        assert required.issubset(columns)

    def test_unique_constraint(self) -> None:
        """resource_type + resource_id のユニーク制約がある."""
        constraints = [
            c.name
            for c in ResourceDefinition.__table__.constraints
            if hasattr(c, "name") and c.name and "resource_def" in c.name
        ]
        assert len(constraints) >= 1
