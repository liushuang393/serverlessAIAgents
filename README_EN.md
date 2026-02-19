# AgentFlow

**Platform and base for AI agent development** — a lightweight framework with a unified interface for MCP, A2A, AG-UI, and A2UI.

**Languages**: English | [简体中文](README_ZH.md) | [日本語](README.md)

---

## 1. Overview & Features

AgentFlow provides a **single API surface** for multiple protocols and agent coordination. For customers it is delivered as **3 Studio product lines** (Migration Studio, Enterprise FAQ Studio, Computer Assistant Studio); for developers it is built as a Kernel (`agentflow`) with plugin-based extension.

| Feature                    | Description                                                                               |
| -------------------------- | ----------------------------------------------------------------------------------------- |
| **8-layer architecture**   | Clear separation: Application, UI, Flow, Agent, Tools, Provider, Protocol, Infrastructure |
| **4 protocols unified**    | MCP, A2A, AG-UI, A2UI used from one codebase                                              |
| **3 Studio product lines** | Customer journey unified as: template → configuration → run → artifacts                   |
| **Development options**    | `@agent` decorator / `create_flow` / AgentCoordinator for simple to advanced use          |
| **Engine patterns**        | SimpleEngine, PipelineEngine, GateEngine, RAGEngine, PEVEngine ready to use               |
| **Type-safe & async**      | Full type annotations and async/await-first I/O                                           |
| **Skills auto-evolution**  | Plugin system that extends capabilities with use                                          |

---

## 2. Main Capabilities

- **Engine execution**: `SimpleEngine` (single agent), `PipelineEngine` (multi-stage, review loop), `GateEngine` (entry gate), `RAGEngine` (retrieval-augmented), `PEVEngine` (plan–execute–verify)
- **Agent definition**: `@agent` decorator, `AgentBlock` subclassing, invocation via `AgentClient.get("name").invoke(...)`
- **Flow construction**: `create_flow(...).gate(...).then(...).parallel(...).review(...).build()`
- **Loose-coupled providers**: `get_llm()`, `get_vectordb()`, `get_db()`, `get_embedding()` for environment-driven implementations
- **Channels**: Multi-platform message integration (MessageGateway, MessageChannelAdapter)
- **HITL**: Approval, interrupt, resume (ApprovalManager, Checkpointer, interrupt)
- **Context engineering**: Token budget, turn compression, RetrievalGate, KeyNotes, etc.
- **Built-in skills**: database-manager, stripe-payment, deployment-manager, auth-provider, etc. (optional)

---

## 3. Technical Architecture

**8 layers** (top to bottom): Application → UI → Flow → Agent → Tools → Provider → Protocol → Infrastructure. Upper layers depend only on lower layers; contracts are used via the public API in `agentflow/__init__.py`.

**Stack**: Python 3.13+, FastAPI, Pydantic, Uvicorn (backend); React, Vite, TypeScript (Studio and app frontends); MCP, A2A, AG-UI, A2UI (protocols); PocketFlow and related (workflow base). Quality tooling: Ruff, mypy, pytest.

---

## 4. Kernel, Platform, and Apps

| Layer                        | Role                                                                                                                                                           | Examples                                                                                             |
| ---------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| **Kernel (agentflow)**       | Stable API, engines, providers, protocol abstractions. Extension via plugins. Side-effectful operations go through policy and audit.                           | `agentflow` package, public API                                                                      |
| **Platform (apps/platform)** | Execution path for 3 Studios (template → config → run → artifacts) and Framework management API. Canonical APIs: `/api/studios/*`, `/api/studios/framework/*`. | Backend `apps.platform.main`, frontend `apps/platform/frontend`                                      |
| **Apps (apps/\*)**           | Product and sample applications. Apps aligned to Migration / FAQ / Assistant Studios, plus cross-cutting apps (orchestration, messaging, etc.).                | `code_migration_assistant`, `faq_system`, `decision_governance_engine`, `market_trend_monitor`, etc. |

External messaging is aligned to the 3 Studios; protocol names and internal layers are not exposed in business-facing UI.

---

## 5. Quick Start, Docs, License

**Before running**: Default environment is `conda activate agentflow`. Check `code-rules/CLAUDE.md` and the target app’s README before running commands.

```bash
conda activate agentflow
pip install -e ".[apps,dev]"
python -m apps.platform.main serve --port 8000
# In another terminal: cd apps/platform/frontend && npm install && npm run dev
```

- **Documentation**: Index [docs/index.md](docs/index.md), external [docs/external/README.md](docs/external/README.md), internal [docs/internal/README.md](docs/internal/README.md), 3 Studios [docs/studios.md](docs/studios.md)
- **Repository**: [GitHub](https://github.com/liushuang393/serverlessAIAgents) | [Issues](https://github.com/liushuang393/serverlessAIAgents/issues)
- **License**: [MIT License](LICENSE)

Parts of the execution/training decoupling and trace design were inspired by [Microsoft Agent Lightning](https://github.com/microsoft/agent-lightning).
