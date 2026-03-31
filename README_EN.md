# BizCore AI

**BizCore AI is an enterprise business development foundation for the AI era and the future AGI era, designed to absorb AI system complexity through a layered architecture so developers can continuously focus on business implementation.**

**Languages**: English | [简体中文](README_ZH.md) | [日本語](README.md)

---

## Why BizCore AI

AI models evolve rapidly, protocols continue to advance, and business applications grow explosively.  
In this uncertain future, a clear division is essential: **the foundation absorbs system complexity while developers focus on business implementation**.

BizCore AI is the **full-stack enterprise development foundation** that realizes this philosophy.

---

## Seven Core Layers Plus Apps

BizCore AI is organized into seven core layers, with `apps/` as the outer product-assembly layer.

```
┌─────────────────────────────────────────────┐
│  BizCore Studios (Apps)                     │  ← Product Assembly Layer (outside the core)
│  Migration / FAQ / Assistant / Custom Apps  │
├─────────────────────────────────────────────┤
│  BizCore Control Plane                      │  ← Management / Observability / Delivery
├─────────────────────────────────────────────┤
│  BizCore Domain                             │  ← Business Domains / Industry Templates
├─────────────────────────────────────────────┤
│  BizCore Harness (Governance)               │  ← Governance / Reliability / Evaluation
├─────────────────────────────────────────────┤
│  BizCore Kernel (Runtime)                   │  ← Execution Engine / Agents / Flows
├─────────────────────────────────────────────┤
│  Shared Services                            │  ← Common Services (Gateway/RAG/Access)
├─────────────────────────────────────────────┤
│  Infrastructure                             │  ← Low-level Foundation (LLM/Storage/Cache)
├─────────────────────────────────────────────┤
│  Contracts                                  │  ← Protocol & Interface Definitions
└─────────────────────────────────────────────┘
```

| Layer                     | Directory         | Responsibility                                                                     |
| ------------------------- | ----------------- | ---------------------------------------------------------------------------------- |
| **Contracts**             | `contracts/`      | Cross-layer protocol, type, and interface definitions (Versioned)                  |
| **Infrastructure**        | `infrastructure/` | LLM Providers, Storage, Cache, Queue, Sandbox, and other low-level foundations     |
| **Shared**                | `shared/`         | LLM Gateway, RAG, Access Control, Trace, Audit, and other shared services          |
| **BizCore Kernel**        | `kernel/`         | Agent Runtime, Flow Engine, Orchestration, Protocol implementations                |
| **BizCore Harness**       | `harness/`        | Governance, Policy, Approval, Budget, Evaluation, Guardrails                       |
| **BizCore Domain**        | `domain/`         | Industry and business-domain models, interfaces, templates, and rules              |
| **BizCore Control Plane** | `control_plane/`  | The platform control plane for discovery, lifecycle, delivery, and operator UI/API |
| **BizCore Studios**       | `apps/*/`         | Product layer for Migration / FAQ / Assistant / custom apps                        |

> **Design Principle**: `contracts / infrastructure / shared / kernel / harness / domain` must not depend on `apps/`. `domain` must not depend on `control_plane`. `control_plane` can orchestrate lower layers but is not their source of truth.

---

## Key Features

### Execution Engine (BizCore Kernel)

- **Engine Patterns**: `SimpleEngine` (single Agent) / `PipelineEngine` (multi-stage, Review) / `GateEngine` (entry gate) / `RAGEngine` (retrieval-augmented) / `PEVEngine` (Plan-Execute-Verify)
- **Agent Definition**: `@agent` decorator / `AgentBlock` subclassing / `AgentClient.get("name").invoke(...)`
- **Flow Construction**: `create_flow(...).gate(...).then(...).parallel(...).review(...).build()`
- **Unified Protocols**: MCP / A2A / AG-UI / A2UI via a single API surface

### Governance (BizCore Harness)

- **Policy Engine**: Declaratively configure pre/post-execution checks and constraints
- **HITL**: Approval, interrupt, resume (ApprovalManager / Checkpointer / interrupt)
- **Context Engineering**: Token budget, turn compression, RetrievalGate, KeyNotes
- **Evaluation & Audit**: Evaluation, Replay, Budget management

### Management (BizCore Platform)

- **App Lifecycle**: Create → Configure → Execute → Observe → Deliver
- **Unified LLM Management**: Provider / Model / Secret / Local Engine managed centrally by Platform
- **Unified API**: `/api/studios/*` and `/api/studios/framework/apps/*` as canonical routes

### Business Applications (BizCore Studios)

- **Migration Studio**: Code migration and modernization
- **Enterprise FAQ Studio**: RAG-based FAQ and knowledge management
- **Computer Assistant Studio**: General-purpose AI assistant and automation

---

## LLM Inference Infrastructure (Gateway-First)

BizCore AI forbids direct Provider SDK calls. All LLM requests go through `LLM Orchestrator → LLM Gateway`.

```mermaid
flowchart TB

subgraph APP["BizCore Studios"]
UI[Chat UI / API / Workflow]
end

subgraph KERNEL["BizCore Kernel"]
Runtime[Agent Runtime]
Tools[Tool Execution]
end

subgraph SHARED["Shared Services"]
Orchestrator[LLM Orchestrator]
RAG[RAG Pipeline]
end

subgraph INFRA["Infrastructure"]
Gateway[LLM Gateway]
Vector[Vector DB]
end

subgraph PROVIDER["Model Providers"]
OpenAI[OpenAI]
Claude[Claude]
Gemini[Gemini]
Local[vLLM / SGLang / TGI]
end

APP --> KERNEL
KERNEL --> SHARED
SHARED --> INFRA
INFRA --> PROVIDER
```

---

## Repository Structure

### Seven Core Layers

- `contracts/`: Protocol and interface definitions (Versioned)
- `infrastructure/`: Low-level foundation (LLM Provider / Storage / Cache / Queue)
- `shared/`: Shared services (Gateway / RAG / Access / Trace / Audit)
- `kernel/`: Agent Runtime / Flow Engine / Orchestration / Protocol
- `harness/`: Governance / Policy / Approval / Budget / Evaluation
- `domain/`: Canonical business-domain implementation
- `control_plane/`: Canonical BizCore control-plane implementation

### Product Layer

- `apps/`: BizCore Studios and custom apps, assembled on top of the seven core layers

### Transition Notes

- Some legacy import and config names may remain during migration, but the canonical implementation is consolidated under `contracts/`, `infrastructure/`, `shared/`, `kernel/`, `harness/`, `domain/`, and `control_plane/`

### Development & Operations

- `plugins/`: Extensions (Blocks / Tools / Providers)
- `docs/`: External/internal documentation and design materials
- `tests/`: Automated test suite (Unit / Integration / E2E)
- `code-rules/`: Unified coding standards and lint rules
- `scripts/`: Development and maintenance utility scripts

---

## Tech Stack

| Area               | Technologies                                            |
| ------------------ | ------------------------------------------------------- |
| **Backend**        | Python 3.13+, FastAPI, Pydantic, Uvicorn                |
| **Frontend**       | React, Vite, TypeScript, ESLint, Prettier               |
| **AI Protocols**   | MCP, A2A, AG-UI, A2UI                                   |
| **Infrastructure** | Supabase / PostgreSQL / Turso, Pinecone / Qdrant, Redis |
| **Quality**        | Ruff, mypy, pytest (80%+ coverage), ESLint, tsc         |

---

## Quick Start

> For app-specific setup (FAQ System, Decision Governance Engine, etc.), see each app's README.md.
> This section covers only the shared infrastructure (auth_service + control_plane).

### Prerequisites

| Tool                    | Version | Purpose                                       |
| ----------------------- | ------- | --------------------------------------------- |
| Python                  | 3.13+   | Backend runtime                               |
| conda                   | Any     | Recommended virtual environment (`agentflow`) |
| Docker + Docker Compose | Latest  | DB and container startup                      |
| Node.js                 | 18+     | Frontend development                          |

### 1. Environment Setup

```bash
# Create and activate conda environment (recommended)
conda create -n agentflow python=3.13 -y
conda activate agentflow

# Install dependencies
pip install -e ".[apps,dev]"
```

### 2. auth_service (Authentication)

Supports both SQLite (local dev) and PostgreSQL (Docker/production).

#### Local Start (SQLite, no DB setup needed)

```bash
conda activate agentflow

# Start auth_service (port 8010)
python -m shared.auth_service.main
# Health check: curl http://localhost:8010/health

# Start frontend (separate terminal, port 3000)
cd shared/auth_service/frontend && npm install && npm run dev
```

#### Docker Start (PostgreSQL)

```bash
cd shared/auth_service
docker compose up --build -d
# Health check: curl http://localhost:8010/health
# Stop: docker compose down
```

### 3. control_plane (Platform Management)

Uses SQLite by default, no DB setup needed.

#### Local Start

```bash
conda activate agentflow

# Start control_plane (port 8900)
python -m control_plane.main serve
# Health check: curl http://localhost:8900/health

# Start frontend (separate terminal, port 3200)
cd control_plane/frontend && npm install && npm run dev
```

### 4. dev_studio (Developer Tools)

Code generation, CI/CD configuration, and quality analysis tools.

#### Local Start

```bash
conda activate agentflow
python -m apps.dev_studio.main
# Health check: curl http://localhost:8011/health
```

#### Docker Start

```bash
cd apps/dev_studio
docker compose up --build -d
# Health check: curl http://localhost:8011/health
# Stop: docker compose down
```

### Service Overview

| Service | Backend | Frontend | Description |
|---|---|---|---|
| auth_service | http://localhost:8010 | http://localhost:3000 | Authentication & user management |
| control_plane | http://localhost:8900 | http://localhost:3200 | Platform management |

### App-specific READMEs

| App                        | README                                                                                 | Description                          |
| -------------------------- | -------------------------------------------------------------------------------------- | ------------------------------------ |
| FAQ System                 | [apps/faq_system/README.md](apps/faq_system/README.md)                                 | RAG-based FAQ & knowledge management |
| Decision Governance Engine | [apps/decision_governance_engine/README.md](apps/decision_governance_engine/README.md) | Decision support system              |
| Code Migration Assistant   | [apps/code_migration_assistant/README.md](apps/code_migration_assistant/README.md)     | Code migration support               |

---

## Documentation & Links

- **Documentation**: [docs/index.md](docs/index.md) | [External](docs/external/README.md) | [Internal](docs/internal/README.md) | [Studios](docs/studios.md)
- **Repository**: [GitHub](https://github.com/liushuang393/serverlessAIAgents) | [Issues](https://github.com/liushuang393/serverlessAIAgents/issues)
- **License**: [MIT License](LICENSE)

---

## Acknowledgements — Open Source Technologies

BizCore AI is built on the shoulders of outstanding open source projects, tools, and design philosophies.
We are deeply grateful to every contributor and community behind them.

### 🏗️ Framework Layer

| Project | Role | License |
| --- | --- | --- |
| [FastAPI](https://github.com/fastapi/fastapi) | High-performance async Web API framework (Kernel / Control Plane) | MIT |
| [Pydantic v2](https://github.com/pydantic/pydantic) | Type-safe data validation and schema definition (Contracts layer) | MIT |
| [SQLAlchemy](https://github.com/sqlalchemy/sqlalchemy) | ORM and database abstraction (Control Plane / Auth Service) | MIT |
| [Alembic](https://github.com/sqlalchemy/alembic) | Database migration management | MIT |
| [Uvicorn](https://github.com/encode/uvicorn) | ASGI server (FastAPI runtime) | BSD-3-Clause |
| [React](https://github.com/facebook/react) | Frontend UI framework | MIT |
| [Vite](https://github.com/vitejs/vite) | Fast frontend build tooling | MIT |
| [TypeScript](https://github.com/microsoft/TypeScript) | Type-safe frontend development language | Apache 2.0 |

### 🤖 AI Protocol Layer

| Project | Role | By |
| --- | --- | --- |
| [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) | AI tool integration protocol (Layer 5 Integration standard) | Anthropic |
| [Agent-to-Agent Protocol (A2A)](https://github.com/google-a2a/A2A) | Inter-agent communication and delegation protocol | Google |
| [AG-UI Protocol](https://github.com/ag-ui-protocol/ag-ui) | Agent → UI real-time streaming protocol | Open Standard |

### 🛠️ Tool & Infrastructure Layer

| Project | Role | License |
| --- | --- | --- |
| [Qdrant](https://github.com/qdrant/qdrant) | High-performance vector database (RAG Pipeline) | Apache 2.0 |
| [Redis](https://github.com/redis/redis) | Distributed cache and message queue | BSD-3-Clause |
| [PostgreSQL](https://www.postgresql.org/) | Relational database | PostgreSQL License |
| [Supabase](https://github.com/supabase/supabase) | Managed PostgreSQL + BaaS | Apache 2.0 |
| [Ruff](https://github.com/astral-sh/ruff) | Ultra-fast Python linter and formatter | MIT |
| [mypy](https://github.com/python/mypy) | Python static type checker | MIT |
| [pytest](https://github.com/pytest-dev/pytest) | Python testing framework | MIT |
| [ESLint](https://github.com/eslint/eslint) / [Prettier](https://github.com/prettier/prettier) | Frontend code quality and formatting tools | MIT |
| [Docker](https://www.docker.com/) | Container and deployment infrastructure | Apache 2.0 |

### 🧩 Built-in Skills

The following skills shipped with BizCore Studios are built on top of open source tools and services.

#### web-content-fetcher (Web Content Extraction)

| Project | Role | License |
| --- | --- | --- |
| [Jina Reader](https://github.com/jina-ai/reader) | Cloud service for extracting clean Markdown content from URLs (primary) | Apache 2.0 |
| [Scrapling](https://github.com/D4Vinci/Scrapling) | Anti-bot-aware Python web scraping library (secondary fallback) | MIT |
| [html2text](https://github.com/Alir3z4/html2text) | HTML to Markdown conversion library | GPL-3.0 |

#### design-skills (Design Image Generation)

| Project | Role | License |
| --- | --- | --- |
| [ComfyUI](https://github.com/comfyanonymous/ComfyUI) | Local GPU image generation backend (primary) | GPL-3.0 |
| [Stable Diffusion XL (SDXL)](https://github.com/Stability-AI/generative-models) | High-quality text-to-image model for local inference | CreativeML Open RAIL++-M |
| [OpenAI gpt-image-1](https://platform.openai.com/docs/guides/images) | Cloud fallback when ComfyUI is unavailable | — (Commercial API) |

#### minimalist-entrepreneur-skills (Startup Framework Skill Pack)

A skill pack based on Sahil Lavingia's (Gumroad founder) book *The Minimalist Entrepreneur*, providing 10 structured skills for startup decision-making, used in `apps/messaging_hub`.

| Project | Role | License |
| --- | --- | --- |
| [slavingia/skills](https://github.com/slavingia/skills) | Original repository for the minimalist entrepreneur skill pack (10 skills) | MIT |
| [*The Minimalist Entrepreneur*](https://www.minimalistentrepreneur.com/) — Sahil Lavingia | The book that serves as the philosophical foundation of the skills | — |

### 💡 Design Philosophy & Architecture Inspiration

| Concept / Reference | Summary |
| --- | --- |
| [Microsoft Agent Lightning](https://github.com/microsoft/agent-lightning) | Inspired BizCore's execution/training decoupling model and distributed trace design |
| **Clean Architecture** — Robert C. Martin | Layer separation and dependency-direction principles behind the 7-layer architecture |
| **ReAct Pattern** — Yao et al., 2022 | Agent loop design combining Reasoning + Acting, foundational to the Kernel runtime |
| **RAG (Retrieval-Augmented Generation)** — Lewis et al., 2020 | Knowledge integration via retrieval augmentation; theoretical basis for RAGEngine / RetrievalGate |
| **HITL (Human-in-the-Loop)** | Design philosophy for embedding human oversight into AI governance (Harness / ApprovalManager) |
| **Gateway Pattern** — Enterprise Integration Patterns | Centralized LLM access with fallback and observability; basis for the LLM Gateway design |
| **Contract-First Design** | Define interfaces before implementations to decouple layers; the philosophy behind `contracts/` |
| **12-Factor App** — Heroku | Cloud-native application design principles applied to config externalization, statelessness, and logging |
| **Micro-kernel Agent (100-line Prototype)** — In-house | The original proof-of-concept that introduced the `@agent` decorator pattern for defining agents with a minimal plain class. Started as ~100 lines and has since evolved into `kernel/agent_decorator.py` (Skills integration, Pydantic schema support, AgentRegistry & A2AHub registration) |
