"""Session preflight and smoke checks."""

from __future__ import annotations

import shutil
import sys
import urllib.error
import urllib.request
from dataclasses import dataclass
from pathlib import Path


@dataclass(slots=True)
class PreflightCheck:
    """Single preflight check result."""

    name: str
    ok: bool
    detail: str


@dataclass(slots=True)
class PreflightReport:
    """Preflight report."""

    ok: bool
    checks: list[PreflightCheck]

    def to_dict(self) -> dict[str, object]:
        return {
            "ok": self.ok,
            "checks": [
                {
                    "name": item.name,
                    "ok": item.ok,
                    "detail": item.detail,
                }
                for item in self.checks
            ],
        }


class PreflightRunner:
    """Run strict startup, health, and smoke checks."""

    def run(
        self,
        *,
        stage: str,
        module: str,
        source_code: str,
        run_root: Path,
        health_url: str | None = None,
    ) -> PreflightReport:
        checks: list[PreflightCheck] = []
        checks.extend(self._check_commands())
        checks.append(self._check_health(health_url))
        checks.append(self._check_smoke(stage=stage, module=module, source_code=source_code, run_root=run_root))
        ok = all(item.ok for item in checks)
        return PreflightReport(ok=ok, checks=checks)

    def _check_commands(self) -> list[PreflightCheck]:
        checks: list[PreflightCheck] = []
        python_ok = Path(sys.executable).exists()
        checks.append(
            PreflightCheck(
                name="dependency_python",
                ok=python_ok,
                detail=sys.executable if python_ok else "python executable not found",
            )
        )
        for cmd in ("java", "javac"):
            resolved = shutil.which(cmd)
            checks.append(
                PreflightCheck(
                    name=f"dependency_{cmd}",
                    ok=resolved is not None,
                    detail=resolved or f"{cmd} command not found",
                )
            )
        return checks

    @staticmethod
    def _check_health(health_url: str | None) -> PreflightCheck:
        if not health_url:
            return PreflightCheck(name="health_check", ok=True, detail="health url not configured")
        try:
            with urllib.request.urlopen(health_url, timeout=3.0) as response:
                status = int(response.status)
            return PreflightCheck(
                name="health_check",
                ok=200 <= status < 300,
                detail=f"http_status={status}",
            )
        except (urllib.error.URLError, TimeoutError) as exc:
            return PreflightCheck(name="health_check", ok=False, detail=str(exc))

    @staticmethod
    def _check_smoke(
        *,
        stage: str,
        module: str,
        source_code: str,
        run_root: Path,
    ) -> PreflightCheck:
        if not source_code.strip():
            return PreflightCheck(name="smoke_source", ok=False, detail="source code is empty")
        module_root = run_root / module
        if stage == "analysis":
            return PreflightCheck(name="smoke_stage", ok=True, detail="analysis smoke passed")
        if not module_root.exists():
            return PreflightCheck(
                name="smoke_stage",
                ok=False,
                detail=f"module artifact root missing: {module_root}",
            )
        return PreflightCheck(name="smoke_stage", ok=True, detail="stage smoke passed")
