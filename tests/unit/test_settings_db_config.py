"""DB 設定優先順位の回帰テスト."""

from __future__ import annotations

from infrastructure.config.settings import AgentFlowSettings


def test_get_db_config_prefers_database_url_over_supabase() -> None:
    """ローカル SQLAlchemy URL を Supabase より優先する."""
    settings = AgentFlowSettings(
        _env_file=None,
        database_url="sqlite+aiosqlite:///./data/local.db",
        supabase_url="https://example.supabase.co",
        supabase_key="test-key",
    )

    db_config = settings.get_db_config()

    assert db_config["backend"] == "sqlite"
    assert db_config["url"] == "sqlite+aiosqlite:///./data/local.db"


def test_get_db_config_falls_back_to_supabase_when_database_url_missing() -> None:
    """明示SQL URLが無い場合のみ Supabase を使う."""
    settings = AgentFlowSettings(
        _env_file=None,
        database_url=None,
        supabase_url="https://example.supabase.co",
        supabase_key="test-key",
    )

    db_config = settings.get_db_config()

    assert db_config["backend"] == "supabase"
    assert db_config["url"] == "https://example.supabase.co"
