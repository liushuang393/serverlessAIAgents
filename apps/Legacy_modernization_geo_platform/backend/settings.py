"""Runtime settings for the Legacy Modernization GEO Platform."""

from __future__ import annotations

import os
from dataclasses import dataclass
from pathlib import Path


APP_ROOT = Path(__file__).resolve().parents[1]


@dataclass(slots=True)
class GeoPlatformSettings:
    """Runtime settings resolved from environment variables."""

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
        """Ensure mutable runtime directories exist for injected settings too."""
        self.ensure_directories()

    @classmethod
    def from_env(cls, *, app_root: Path | None = None) -> GeoPlatformSettings:
        """Build settings from environment variables."""
        resolved_root = app_root or APP_ROOT
        api_key_env_name = os.getenv("GEO_PLATFORM_API_KEY_ENV", "GEO_PLATFORM_API_KEY")
        raw_origins = os.getenv(
            "GEO_PLATFORM_CORS_ORIGINS",
            "http://localhost:3010,http://127.0.0.1:3010",
        )
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
        public_base_url = os.getenv(
            "GEO_PLATFORM_PUBLIC_BASE_URL",
            f"http://localhost:{os.getenv('GEO_PLATFORM_PORT', '8010')}",
        )
        settings = cls(
            app_root=resolved_root,
            host=os.getenv("GEO_PLATFORM_HOST", "0.0.0.0"),
            port=int(os.getenv("GEO_PLATFORM_PORT", "8010")),
            api_key=os.getenv(api_key_env_name, ""),
            cors_origins=cors_origins,
            db_path=db_path,
            artifacts_dir=artifacts_dir,
            reports_dir=reports_dir,
            published_dir=published_dir,
            public_base_url=public_base_url.rstrip("/"),
            frontend_dist_dir=(resolved_root / "frontend" / "dist").resolve(),
        )
        return settings

    def ensure_directories(self) -> None:
        """Create mutable directories required at runtime."""
        self.db_path.parent.mkdir(parents=True, exist_ok=True)
        self.artifacts_dir.mkdir(parents=True, exist_ok=True)
        self.reports_dir.mkdir(parents=True, exist_ok=True)
        self.published_dir.mkdir(parents=True, exist_ok=True)
