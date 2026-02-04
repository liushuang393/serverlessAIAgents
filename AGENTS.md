# AgentFlow (serverlessAIAgents) - Agent Instructions

This repo is a Python-first AI agent framework with an optional React/Vite Studio UI.
Agentic coding changes should preserve the existing architecture and quality gates.

## Quick Start (Local)

- Recommended runtime: Python 3.13+ (mypy/ruff configs target 3.13; AG-UI requires 3.13)
- Install (dev): `pip install -e ".[dev]"`
- Studio deps: `cd studio && npm install`

Note: `pyproject.toml` currently declares `requires-python >=3.10`, but tooling and
documentation assume 3.13+. For development work, treat 3.13+ as required.

## Build / Lint / Test Commands

### One-command workflows

- Run everything: `make check-all`
- Format all: `make format`
- Lint all: `make lint`
- Type check all: `make type-check`
- Test all: `make test`
- Test with coverage: `make test-cov`

### Cross-platform helper scripts

- Linux/Mac/WSL: `./check.sh all` (also: `format`, `lint`, `type-check`, `test`, `test-cov`)
- Windows PowerShell: `./check.ps1 all`
- Windows CMD: `check.bat all`

### Python (direct)

- Format: `ruff format .`
- Lint: `ruff check .`
- Auto-fix lint: `ruff check --fix .`
- Type check (strict): `mypy agentflow --strict --ignore-missing-imports`

### Tests

- Run all tests: `pytest -v`
- Run a directory: `pytest tests/unit/`
- Run a single file: `pytest tests/unit/test_agent_block.py`
- Run a single test: `pytest tests/unit/test_agent_block.py::test_agent_initialization`
- Run by keyword: `pytest -k "agent_initialization"`
- Run by marker (if tests are marked): `pytest -m unit` / `pytest -m "not slow"`

Pytest config lives in `pyproject.toml` and enforces:
- strict markers/config, asyncio_mode=auto
- coverage on `agentflow` and `--cov-fail-under=80`

### Frontend (studio/)

- Dev server: `cd studio && npm run dev`
- Build: `cd studio && npm run build`
- Lint: `cd studio && npm run lint` (or `npm run lint:fix`)
- Format: `cd studio && npm run format`
- Type check: `cd studio && npm run type-check`

### Build / Dev servers

- Build Python package: `make build`
- Build Studio bundle: `make build-frontend`
- Run backend API (Studio server): `make dev-backend`
- Run Studio dev server: `make dev-frontend`

## Pre-commit / Secrets

- Install hooks: `make install-hooks` or `pre-commit install`
- Run on all files: `make pre-commit` or `pre-commit run --all-files`
- Secrets scanning uses `detect-secrets` with `.secrets.baseline`
- Never commit credentials or real API keys (check `.env.example` for expected vars)

## Cursor / Copilot Rules

- No `.cursor/rules/`, `.cursorrules`, or `.github/copilot-instructions.md` found in this repo.

## Code Style and Engineering Guidelines

### Formatting

- Use Ruff as the single source of truth (formatter + linter)
- Line length: 100; quotes: double (Ruff format)
- Prefer small, focused functions; avoid large files (hard limit: do not create >1000-line files)

### Imports

- Order: standard library, third-party, then first-party (`agentflow`), separated by blank lines
- No wildcard imports
- Prefer absolute imports; relative imports only within a package when they improve clarity
- Use `if TYPE_CHECKING:` to avoid runtime cycles when needed

### Types (mypy strict)

- 100% type annotations for production code (tests are exempt via Ruff per-file ignores)
- Avoid `Any`; if unavoidable, isolate it and explain why
- Prefer built-in generics: `dict[str, X]`, `list[X]`, and `collections.abc` for callables
- Prefer `X | None` unions over `Optional[X]` (unless consistent with surrounding code)
- Do not use `type: ignore` without a specific error code and a short justification

### Async-first I/O

- All I/O must be async (use `aiofiles`, `httpx`, async DB clients)
- Avoid blocking calls in core library code (no synchronous `open()`, no `requests`, no `time.sleep()`)
- If you must call a blocking API in non-critical/demo areas, confine it and document why

### Error handling

- Fail fast: validate inputs early and raise meaningful exceptions
- No bare `except:`; catch specific exceptions
- Prefer project exceptions (see `code-rules/global/error-handling.md`) and keep a clear hierarchy
- When wrapping unexpected errors, log context and re-raise with `raise ... from e`
- Never silently swallow exceptions

### Logging

- Use `structlog` (structured logs); avoid `print()`
- Log errors with context fields (workflow_id, target, provider, etc.) and `exc_info=True` when useful

### Naming

- Modules: `snake_case.py`
- Classes: `PascalCase`
- Functions/vars: `snake_case`
- Constants: `UPPER_SNAKE_CASE`
- Private members: leading underscore (`_internal_state`)

### Architecture constraints (AgentFlow)

- Preserve the 8-layer dependency direction (upper layers depend only on lower layers)
- Prefer interfaces/protocols across layer boundaries over concrete class dependencies
- Use unified provider APIs instead of hardcoding vendor logic:
  - `from agentflow import get_llm, get_db, get_vectordb, get_cache`

### AG-UI events

- When emitting AG-UI events, use standard event classes from `agentflow/protocols/agui_events.py`
- Do not emit ad-hoc dict events like `{type: ..., data: ...}` for AG-UI streams

### Docs

- Public functions/classes need Google-style docstrings
- Keep docstrings practical: Args/Returns/Raises; include examples for non-trivial APIs

## Where To Look (Existing Guidance)

- `CLAUDE.md`: project overview, architecture, and common commands
- `pyproject.toml`: Ruff, mypy, pytest, coverage configuration
- `Makefile` and `check.sh`: canonical developer workflows
- `code-rules/`: detailed rules (style, error handling, testing, AgentFlow-specific patterns)
