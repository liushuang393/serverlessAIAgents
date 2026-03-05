"""Evidence-first completion gate."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any


_REQUIRED_ARTIFACT_KEYS: dict[str, tuple[str, ...]] = {
    "analysis": ("analysis",),
    "business_semantics": ("business_semantics",),
    "design": ("design",),
    "transform": ("code", "code_iterations"),
    "tests": ("tests",),
    "diff": ("diff",),
    "strict_verification": ("diff_strict",),
    "quality": ("quality",),
    "fix": ("fix",),
    "report": ("report",),
}


class EvidenceGate:
    """Validate stage evidence and persist manifest."""

    def evaluate(
        self,
        *,
        stage: str,
        backlog_task_id: str,
        stage_result: dict[str, Any],
        evidence_root: Path,
    ) -> tuple[bool, dict[str, Any]]:
        artifact_paths_raw = stage_result.get("artifact_paths", {})
        artifact_paths = artifact_paths_raw if isinstance(artifact_paths_raw, dict) else {}
        required = _REQUIRED_ARTIFACT_KEYS.get(stage, ())
        missing: list[str] = []
        resolved: dict[str, str] = {}

        for key in required:
            path_raw = artifact_paths.get(key)
            if not isinstance(path_raw, str) or not path_raw:
                missing.append(f"missing artifact key: {key}")
                continue
            path = Path(path_raw)
            if not path.exists():
                missing.append(f"artifact path not found: {path}")
                continue
            resolved[key] = str(path)

        evidence_payload = stage_result.get("evidence", {})
        if stage in {"diff", "strict_verification", "quality"} and not isinstance(evidence_payload, dict):
            missing.append("evidence payload missing")

        manifest = {
            "backlog_task_id": backlog_task_id,
            "stage": stage,
            "required_artifacts": list(required),
            "resolved_artifacts": resolved,
            "missing": missing,
            "result": {
                "success": bool(stage_result.get("success", False)),
                "decision": stage_result.get("decision"),
                "unknown_count": len(stage_result.get("unknowns", []))
                if isinstance(stage_result.get("unknowns"), list)
                else 0,
            },
        }

        task_evidence_dir = evidence_root / backlog_task_id
        task_evidence_dir.mkdir(parents=True, exist_ok=True)
        manifest_path = task_evidence_dir / "manifest.json"
        manifest_path.write_text(
            json.dumps(manifest, ensure_ascii=False, indent=2),
            encoding="utf-8",
        )
        manifest["manifest_path"] = str(manifest_path)
        ok = not missing and bool(stage_result.get("success", False))
        return ok, manifest
