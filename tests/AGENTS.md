# tests/

## Overview
- pytest suite for the framework and integrations.

## Structure
```
tests/
├── conftest.py
├── unit/
├── integration/
└── memory/
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| pytest configuration | `pyproject.toml` | `[tool.pytest.ini_options]` defines markers and defaults.
| Global fixtures | `tests/conftest.py` | Provides shared fixtures (engine, sample_workflow).
| Unit tests | `tests/unit/` | Primary unit test suite.
| Integration tests | `tests/integration/` | Marker-driven integration tests.

## Conventions
- Markers: `unit`, `integration`, `e2e`, `slow` (strict markers enabled).

## Commands
```bash
make test
make test-cov
pytest -m "unit"
pytest -m "integration"
```
