"""Legacy Modernization GEO Platform の実行時設定。"""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path

from kernel.runtime import resolve_app_runtime


APP_ROOT = Path(__file__).resolve().parents[1]
APP_CONFIG_PATH = APP_ROOT / "app_config.json"


@dataclass(slots=True)
class GeoPlatformSettings:
    """環境変数と manifest から解決した実行時設定。"""

    app_root: Path
    host: str
    port: int
    api_key: str
    cors_origins: list[str]
    db_path: Path
    artifacts_dir: Path
    reports_dir: Path
    published_dir: Path
    public_base_url: str
    frontend_dist_dir: Path

    def __post_init__(self) -> None:
        """注入された設定でも必要ディレクトリが存在するようにする。"""
        self.ensure_directories()

    @classmethod
    def from_env(cls, *, app_root: Path | None = None) -> GeoPlatformSettings:
        """環境変数と manifest から設定を組み立てる。"""
        resolved_root = app_root or APP_ROOT
        runtime = resolve_app_runtime(
            APP_CONFIG_PATH,
            env=os.environ,
            backend_host_env="GEO_PLATFORM_HOST",
            backend_port_env="GEO_PLATFORM_PORT",
            backend_url_env="GEO_PLATFORM_PUBLIC_BASE_URL",
        )
        backend_host = runtime.hosts.backend
        backend_port = runtime.ports.api
        backend_url = runtime.urls.backend
        if backend_host is None or backend_port is None or backend_url is None:
            msg = "GEO Platform の backend host / port / URL は app_config.json または明示 env で指定してください。"
            raise RuntimeError(msg)
        api_key_env_name = os.getenv("GEO_PLATFORM_API_KEY_ENV", "GEO_PLATFORM_API_KEY")
        default_origins = _default_frontend_origins(runtime.ports.frontend)
        raw_origins = os.getenv("GEO_PLATFORM_CORS_ORIGINS", ",".join(default_origins))
        cors_origins = [item.strip() for item in raw_origins.split(",") if item.strip()]
        db_path = Path(
            os.getenv("GEO_PLATFORM_DB_PATH", str(resolved_root / "data" / "geo_platform.db")),
        ).resolve()
        artifacts_dir = Path(
            os.getenv(
                "GEO_PLATFORM_ARTIFACTS_DIR",
                str(resolved_root / "data" / "artifacts"),
            ),
        ).resolve()
        reports_dir = Path(
            os.getenv(
                "GEO_PLATFORM_REPORTS_DIR",
                str(resolved_root / "data" / "reports"),
            ),
        ).resolve()
        published_dir = Path(
            os.getenv(
                "GEO_PLATFORM_PUBLISHED_DIR",
                str(resolved_root / "data" / "published"),
            ),
        ).resolve()
        return cls(
            app_root=resolved_root,
            host=backend_host,
            port=backend_port,
            api_key=os.getenv(api_key_env_name, ""),
            cors_origins=cors_origins,
            db_path=db_path,
            artifacts_dir=artifacts_dir,
            reports_dir=reports_dir,
            published_dir=published_dir,
            public_base_url=backend_url.rstrip("/"),
            frontend_dist_dir=(resolved_root / "frontend" / "dist").resolve(),
        )

    def ensure_directories(self) -> None:
        """実行時に必要な可変ディレクトリを作成する。"""
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self.artifacts_dir.mkdir(parents=True, exist_ok=True)
        self.reports_dir.mkdir(parents=True, exist_ok=True)
        self.published_dir.mkdir(parents=True, exist_ok=True)


def _default_frontend_origins(frontend_port: int | None) -> list[str]:
    if frontend_port is None:
        return []
    return [
        f"http://localhost:{frontend_port}",
        f"http://127.0.0.1:{frontend_port}",
    ]
