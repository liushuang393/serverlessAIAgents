"""RuntimeCommandResolver の単体テスト."""

from __future__ import annotations

from apps.platform.schemas.app_config_schemas import RuntimeCommandsConfig
from apps.platform.services.runtime_command_resolver import RuntimeCommandResolver


class TestRuntimeCommandResolver:
    """README 優先解決の契約テスト."""

    def test_readme_backend_frontend_are_prioritized(self, tmp_path) -> None:
        app_dir = tmp_path / "market_trend_monitor"
        app_dir.mkdir(parents=True)
        (app_dir / "README.md").write_text(
            (
                "```bash\n"
                "python -m apps.market_trend_monitor.backend.api.main --reload\n"
                "cd apps/market_trend_monitor/frontend && npm run dev\n"
                "```\n"
            ),
            encoding="utf-8",
        )
        runtime = RuntimeCommandsConfig(
            backend_dev="python apps/market_trend_monitor/scripts/dev.py",
            frontend_dev="echo runtime frontend",
        )
        resolved = RuntimeCommandResolver().resolve(
            app_name="market_trend_monitor",
            app_dir=app_dir,
            runtime_commands=runtime,
        )
        assert resolved.backend_dev == "python -m apps.market_trend_monitor.backend.api.main --reload"
        assert resolved.frontend_dev == "cd apps/market_trend_monitor/frontend && npm run dev"
        assert resolved.source["backend_dev"] == "readme"
        assert resolved.source["frontend_dev"] == "readme"

    def test_start_publish_ignore_readme_foreground_python(self, tmp_path) -> None:
        app_dir = tmp_path / "sample_app"
        app_dir.mkdir(parents=True)
        (app_dir / "README.md").write_text(
            (
                "```bash\n"
                "python -m apps.sample_app.main --reload\n"
                "python -m apps.platform.main publish ./apps/sample_app --target docker\n"
                "```\n"
            ),
            encoding="utf-8",
        )
        runtime = RuntimeCommandsConfig(
            start="docker compose -f docker-compose.yml up -d",
            publish="docker compose -f docker-compose.yml up -d --build",
        )
        resolved = RuntimeCommandResolver().resolve(
            app_name="sample_app",
            app_dir=app_dir,
            runtime_commands=runtime,
        )
        assert resolved.start == "docker compose -f docker-compose.yml up -d"
        assert resolved.publish == "docker compose -f docker-compose.yml up -d --build"
        assert resolved.source["start"] == "runtime.commands"
        assert resolved.source["publish"] == "runtime.commands"

    def test_readme_explicit_docker_start_publish_still_supported(self, tmp_path) -> None:
        app_dir = tmp_path / "compose_app"
        app_dir.mkdir(parents=True)
        (app_dir / "README.md").write_text(
            (
                "```bash\n"
                "docker compose up -d --build\n"
                "docker compose up -d\n"
                "docker compose down\n"
                "```\n"
            ),
            encoding="utf-8",
        )
        runtime = RuntimeCommandsConfig()
        resolved = RuntimeCommandResolver().resolve(
            app_name="compose_app",
            app_dir=app_dir,
            runtime_commands=runtime,
        )
        assert resolved.publish == "docker compose up -d --build"
        assert resolved.start == "docker compose up -d"
        assert resolved.stop == "docker compose down"
        assert resolved.source["publish"] == "readme"
        assert resolved.source["start"] == "readme"
        assert resolved.source["stop"] == "readme"
