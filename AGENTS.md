# AGENTS.md (Repository Instructions)

This file is for agentic coding tools operating in this repo.

Updated: 2026-02-03

## Repo Map
```
./
├── agentflow/                 # framework library (public API: agentflow/__init__.py)
├── apps/                      # sample applications
├── studio/                    # React/Vite Studio frontend
├── tests/                     # pytest suite
├── docs/                      # developer docs + design notes
├── code-rules/                # detailed coding rules (source of truth)
└── .github/workflows/         # CI/CD workflows
```

## Commands (Canonical)

```bash
# Install (dev)
make install-dev

# Format / lint / types
make format
make lint
make type-check

# Run the full test suite (configured with coverage + fail-under)
make test
make test-cov

# Run pre-commit on all files (includes detect-secrets)
make pre-commit

# Studio dev servers
make dev-backend      # FastAPI backend on :8000
make dev-frontend     # Vite frontend on :3000

# Build
make build
make build-frontend
```

### Running A Single Test (Fast Local Loop)

Pytest config in `pyproject.toml` enables coverage and `--cov-fail-under=80` by default.
Know the difference:

```bash
# Single test (uses repo defaults; may fail coverage threshold on small runs)
pytest tests/unit/test_file.py::test_name

# Single test, disable coverage plugin (recommended for quick iteration)
pytest --no-cov tests/unit/test_file.py::test_name

# Run a single test file
pytest --no-cov tests/unit/test_file.py

# Run tests matching a name/pattern
pytest --no-cov -k "workflow" tests/unit

# Run by marker (markers are strict)
pytest --no-cov -m unit
pytest --no-cov -m "not slow"

# Stop on first failure
pytest --no-cov --maxfail=1 -x tests/unit/test_file.py
```

## Code Style (Source Of Truth)

Primary references:
- `CLAUDE.md` (project-wide conventions, especially language + prohibited patterns)
- `pyproject.toml` (Ruff/Mypy/Pytest config)
- `code-rules/CLAUDE.md` + `code-rules/global/*` (deeper standards)

Cursor rules / Copilot rules:
- Not found: `.cursor/rules/`, `.cursorrules`, `.github/copilot-instructions.md`

## Language Rules
- Code comments, docstrings, log messages: Japanese (even if conversation is English).
- Identifiers (vars/functions/classes): English, PEP 8 style.

## Formatting
- Python formatter: Ruff (`ruff format .`), line length 100.
- Quotes: double quotes (Ruff format: `quote-style = "double"`).
- Do not hand-format; run `make format` after edits.

## Imports
- Use Ruff/isort via `ruff check`.
- Group order: standard library, third-party, first-party (`agentflow`), then local.
- Prefer absolute imports (`from agentflow...`) over relative (relative only within same package).
- No wildcard imports.

## Typing
- Mypy strict is expected (see `pyproject.toml` and `make type-check`).
- Add type annotations for all public functions/methods (tests are exempt via Ruff config).
- Prefer `X | None` unions and `collections.abc` types (`Callable`, `Awaitable`, etc.).
- Avoid `Any`; if unavoidable, keep it scoped and explain why.
- Avoid `# type: ignore`; if unavoidable, include a reason and target the narrowest code.

## Async / I/O
- Async-first codebase: prefer `async def` and non-blocking I/O.
- Avoid blocking calls in async paths (`open()`, `time.sleep()`, sync HTTP, etc.).
- Prefer `aiofiles` for files and `httpx` for HTTP.

## Errors & Logging
- Fail fast; do not swallow exceptions.
- No bare `except:`.
- Convert unexpected exceptions at service boundaries and preserve the cause (`raise ... from e`).
- Use structured logging (structlog); do not leave `print()` debugging.
- When logging errors, include contextual fields (ids, target, model, path) and `exc_info=True`.

## Event Protocols (AG-UI)
- Never emit ad-hoc events as `{type: "...", data: {...}}`.
- Use `agentflow/protocols/agui_events.py` event models and `.to_dict()` / `.to_sse()`.
- Progress events must include `current`, `total`, `percentage` (avoid ambiguous progress).

## File Size / Modularity
- Hard limit: keep source files < 1000 lines.
- If a file exceeds ~500 lines or mixes 3+ responsibilities, split it.

## Tests
- Tests live in `tests/` (configured in `pyproject.toml`).
- Markers: `unit`, `integration`, `e2e`, `slow` (strict markers enabled).
- Prefer small, deterministic unit tests; isolate external calls behind providers.

## Secrets
- Do not commit `.env`, API keys, tokens, credentials.
- Pre-commit runs detect-secrets using `.secrets.baseline`.

## Where To Look (Common Tasks)
| Task | Location |
|------|----------|
| Public API surface | `agentflow/__init__.py` |
| CLI behavior | `agentflow/cli/main.py` |
| Engines | `agentflow/engines/` |
| Flow DSL | `agentflow/flow/` |
| Protocols + event models | `agentflow/protocols/` |
| Studio backend | `agentflow/studio/` |
| Studio frontend | `studio/` |
| Deploy + codegen | `agentflow/deploy/`, `agentflow/codegen/` |
| Detailed coding rules | `code-rules/` |
