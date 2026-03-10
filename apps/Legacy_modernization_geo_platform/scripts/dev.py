#!/usr/bin/env python3
"""Development launcher for the GEO Platform."""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path


def ensure_env_file(app_dir: Path) -> None:
    """Create `.env` from `.env.example` if it is missing."""
    env_path = app_dir / ".env"
    if env_path.exists():
        return
    example_path = app_dir / ".env.example"
    if example_path.exists():
        shutil.copy(example_path, env_path)


def main() -> None:
    """Run the FastAPI app in local development mode."""
    script_dir = Path(__file__).resolve().parent
    app_dir = script_dir.parent
    ensure_env_file(app_dir)
    cmd = [sys.executable, "-m", "apps.Legacy_modernization_geo_platform.main"]
    if len(sys.argv) > 1:
        cmd.extend(sys.argv[1:])
    if sys.platform != "win32":
        os.execvp(cmd[0], cmd)
    subprocess.run(cmd, check=False)


if __name__ == "__main__":
    main()

