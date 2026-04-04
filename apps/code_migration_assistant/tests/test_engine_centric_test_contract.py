"""Engine-centric test suite contract for code_migration_assistant."""

from __future__ import annotations

from pathlib import Path


_TEST_ROOT = Path(__file__).resolve().parent
_FORBIDDEN_TOKENS = (
    "apps.code_migration_assistant.pipeline.engine",
    "MIGRATION_EXECUTION_BACKEND",
)


def test_suite_does_not_reference_legacy_pipeline_runtime() -> None:
    violations: list[str] = []
    for path in sorted(_TEST_ROOT.rglob("test_*.py")):
        if path.name == Path(__file__).name:
            continue
        text = path.read_text(encoding="utf-8")
        for token in _FORBIDDEN_TOKENS:
            if token in text:
                violations.append(f"{path.name}: {token}")

    assert not violations, "legacy runtime references found:\n" + "\n".join(violations)
