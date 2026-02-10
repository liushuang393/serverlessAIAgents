# AgentFlow Application Architecture: Code Migration Assistant

> **Version**: 1.0.0
> **Application**: Code Migration Assistant
> **Last Updated**: 2026-01-20

## 1. Overview
The **Code Migration Assistant** is a reference implementation of a complex enterprise application built on the AgentFlow framework. It adheres to the framework's 8-layer architecture and demonstrates best practices for organizing front-end and back-end logic.

## 2. Directory Structure

The application is organized into a clear frontend/backend split, with core logic residing in `engine.py` and modular agents.

```
apps/code_migration_assistant/
├── agents/             # Agent Layer: Business logic agents (A2A-ready)
├── adapters/           # Adapter Layer: Language parsing/generation skills
├── backend/            # Service Layer: FastAPI backend application
│   └── app.py          # Main entry point
├── frontend/           # UI Layer: Static assets (HTML/CSS/JS)
│   ├── index.html
│   ├── app.js
│   └── style.css
├── engine.py           # Engine Layer: Orchestration logic
├── workflow/           # Flow Layer: Data models and pipeline configs
├── docs/               # Documentation Layer: App-specific rules
└── tests/              # Test Layer: Unit and integration tests
```

## 3. Naming Conventions

### Python (Backend)
-   **snake_case**: File names (`test_synthesis_agent.py`), function names, variable names.
-   **PascalCase**: Class names (`TestSynthesisAgent`).
-   **Structure**: 
    -   `agents/*.py`: Contains `@agent` decorated classes.
    -   `adapters/*.py`: Contains language-specific logic.

### JavaScript (Frontend)
-   **camelCase**: Function names (`startMigration`), variable names.
-   **PascalCase**: Class names (`MigrationUI`).
-   **kebab-case**: CSS classes (`log-entry`, `step-active`).

## 4. Key Components

### Backend (`backend/app.py`)
-   **FastAPI**: Provides the REST API and WebSocket endpoints.
-   **AG-UI Protocol**: Standardized event streaming via `agentflow.protocols.agui_events`.
-   **StaticFiles**: Serves the `frontend/` directory.

### Frontend (`frontend/app.js`)
-   **Vanilla JS**: Lightweight, no-build-step frontend for simplicity and speed.
-   **WebSockets**: Real-time event consumption from the backend.
-   **Glassmorphism**: Premium UI design style.

## 5. Reflection & Rules
-   This architecture document serves as the "Memory" for the application structure.
-   Future modifications should consult and update this document to maintain consistency.
