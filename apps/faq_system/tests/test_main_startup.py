"""FAQ 起動時 DB 設定の回帰テスト."""

from __future__ import annotations

from apps.faq_system import main as faq_main


def test_build_faq_database_config_uses_faq_database_env_key() -> None:
    """FAQ 本体 DB は FAQ_DATABASE_URL を正本とする."""
    config = faq_main._build_faq_database_config()

    assert config.url_env_key == "FAQ_DATABASE_URL"
    assert config.echo_env_key == "FAQ_DB_ECHO"
    assert config.url.startswith("sqlite+aiosqlite:///")
    assert config.url.endswith("faq_system.db")


def test_detect_database_backend_returns_sqlite_for_local_default() -> None:
    """ローカル SQLite URL を sqlite と判定する."""
    backend = faq_main._detect_database_backend("sqlite+aiosqlite:///./apps/faq_system/data/faq_system.db")

    assert backend == "sqlite"


def test_mask_database_url_hides_credentials() -> None:
    """認証情報を含む DB URL はログ用にマスクする."""
    masked = faq_main._mask_database_url("postgresql+asyncpg://user:secret@localhost:5432/faq_system")

    assert masked == "***@localhost:5432/faq_system"


def test_log_faq_startup_uses_unified_startup_summary(monkeypatch) -> None:
    """FAQ 起動時は統一 startup summary API を呼ぶ."""
    recorded: dict[str, object] = {}

    def _fake_log_startup_info(*args, **kwargs):
        recorded["args"] = args
        recorded["kwargs"] = kwargs
        return {}

    monkeypatch.setattr(faq_main, "log_startup_info", _fake_log_startup_info)
    monkeypatch.setattr(
        faq_main,
        "_load_app_config",
        lambda: {
            "display_name": "FAQ System",
            "version": "1.2.3",
            "agents": [{"name": "FAQAgent", "capabilities": ["chat", "rag"]}],
            "services": {},
        },
    )
    monkeypatch.setattr(
        type(faq_main.db_manager),
        "resolved_url",
        property(lambda _self: "postgresql+asyncpg://faq:secret@localhost:5433/faq_system"),
    )

    faq_main._log_faq_startup("0.0.0.0", 8000)

    kwargs = recorded["kwargs"]
    assert kwargs["app_config_path"] == faq_main._APP_CONFIG_PATH
    assert kwargs["runtime_overrides"] == {
        "db": {
            "backend": "postgresql",
            "url": "postgresql+asyncpg://faq:secret@localhost:5433/faq_system",
        }
    }
