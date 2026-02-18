# AgentFlow

<div align="center">

**Lightweight AI Agent Development Framework**

_Unified Protocol Interface Based on PocketFlow_

[![Python 3.13+](https://img.shields.io/badge/python-3.13+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-434%20passed-brightgreen.svg)](tests/)
[![Coverage](https://img.shields.io/badge/coverage-92.46%25-brightgreen.svg)](htmlcov/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Code style: ruff](https://img.shields.io/badge/code%20style-ruff-000000.svg)](https://github.com/astral-sh/ruff)

[Documentation](docs/) | [Examples](examples/) | [Contributing](CONTRIBUTING.md)

**Languages**: English | [简体中文](README_ZH.md) | [日本語](README.md)

</div>

---

## ⚠️ Project Status

> **Notice**: This project is currently under development.
>
> - ✅ **Automated Tests**: 434 tests, 92.46% coverage
> - 🚧 **Production Use**: Please test thoroughly before use

---

## 🎯 What is AgentFlow

A lightweight AI agent framework providing unified interface for **MCP / A2A / AG-UI / A2UI** protocols.

## 🧩 3 Studio Product Lines (External Surface)

For business-facing delivery, the recommended product narrative is:

- `Migration Studio`
- `Enterprise FAQ Studio`
- `Computer Assistant Studio`

Customer flow is intentionally simplified to:
`Template -> Data/Permission Setup -> Run -> Artifacts`.
See `docs/studios.md` for details.

### ✨ Key Features

| Feature | Description |
|---------|-------------|
| 🚀 **Lightweight** | Core code ~500 lines |
| 🔌 **4 Protocols** | MCP / A2A / AG-UI / A2UI |
| 🎨 **Auto Adapter** | `@auto_adapt` for protocol conversion |
| 🧠 **Skills Auto-Evolution** | Gets smarter with use |
| 📦 **CLI** | `agentflow init/run/create` |
| 🔒 **Type Safe** | 100% type annotations |
| ⚡ **Async** | Fully asynchronous I/O |

### 🎯 Skills Auto-Evolution System (NEW)

Claude Code Skills compatible auto-evolution capability system:

```
User Request → Skill Matching → Execute if exists
                             → Auto-generate if not → Validate → Persist
= Gets smarter with every use
```

```python
from agentflow.skills import SkillEngine

engine = SkillEngine(auto_learn=True)
result = await engine.resolve("Extract text from PDF")

if result.generated:
    print(f"🆕 New skill auto-generated: {result.skill.name}")
```

See [Skills Guide](docs/guide-skills.md) for details.

### 🏗️ Built-in Production-Ready Skills (NEW)

Enterprise-grade skills ready for production:

| Skill | Description | Supported Services |
|-------|-------------|-------------------|
| 🗄️ **database-manager** | DB management, CRUD, RLS | Supabase / Turso / PostgreSQL |
| 💳 **stripe-payment** | Payment & subscription | Stripe Checkout / Billing |
| 🚀 **deployment-manager** | Deploy & environment mgmt | Vercel / Cloudflare Pages |
| 🔐 **auth-provider** | Auth & session mgmt | Supabase Auth / Clerk |
| 🔄 **model-router** | Multi-LLM routing & cost optimization | OpenAI / Anthropic / Google |

```python
# Database integration
from agentflow.skills.builtin.database_manager import DatabaseManager, SupabaseConfig

db = DatabaseManager(provider="supabase", config=SupabaseConfig(
    url="https://xxx.supabase.co",
    anon_key="eyJ...",
))
await db.connect()
users = await db.select("users", filters={"status": "active"})

# Payment integration
from agentflow.skills.builtin.stripe_payment import StripePayment, StripeConfig

stripe = StripePayment(StripeConfig(secret_key="sk_..."))
session = await stripe.create_checkout_session(
    customer_email="user@example.com",
    line_items=[{"price": "price_xxx", "quantity": 1}],
    mode="subscription",
)

# Multi-model routing
from agentflow.llm import ModelRouter, RoutingStrategy

router = ModelRouter.from_env()  # Load API keys from environment
response = await router.chat(messages)  # Auto-select best model
```

See [Built-in Skills Guide](docs/guide-builtin-skills.md) for details.

### 🧠 Coordination Patterns

| Pattern | Description |
|---------|-------------|
| **Supervisor** | Dynamic worker selection |
| **Hierarchical** | Hierarchical task decomposition |
| **Sequential/Concurrent** | Sequential/Parallel execution |

---

## 📦 Installation

```bash
# Conda environment
conda env create -f environment.yml
conda activate agentflow

# Or pip
pip install -e .
```

---

## 🚀 Quick Start

```bash
# Create project
agentflow init my-agent && cd my-agent

# Run
agentflow run . --input '{"text": "hello"}'
```

See [Quick Start](docs/quickstart.md) for details.

---

## ✅ Pre-Run Checklist (Required)

- Default runtime environment: `conda activate agentflow`.
- Before running commands, check `code-rules/CLAUDE.md` and the target app README.
- Ask the customer only when the environment is not documented.
- Once confirmed, update project rules/README to avoid repeated questions.

---

## 🎨 Usage Scenarios

AgentFlow provides three ways to operate. Choose the best method for your use case.

### 1. 🖱️ Studio UI (Visual Editor)

**Create workflows with drag & drop in browser, no coding required**

- ✅ **Beginner-friendly**: No programming knowledge needed
- ✅ **Visual**: Understand and edit workflows visually
- ✅ **Quick**: Create workflows in minutes

📖 [Studio UI Guide](docs/guide-studio-ui.md)

---

### 2. ⚡ CLI (Command Line)

**Quickly run and manage agents from terminal**

- ✅ **Fast**: Operate without GUI
- ✅ **Automation**: Perfect for scripts and batch processing
- ✅ **Simple**: Execute with a single command

📖 [CLI Guide](docs/guide-cli.md)

---

### 3. 🐍 Coding (Python)

**Develop and customize agents with Python code**

- ✅ **Flexibility**: Full customization possible
- ✅ **Type Safe**: 100% type annotation support
- ✅ **Extensible**: Protocol integration and coordination patterns available

📖 [Coding Guide](docs/guide-coding.md)

---

## 📚 Documentation

| Document | Description |
|----------|-------------|
| [Studio UI Guide](docs/guide-studio-ui.md) | Visual editor operation |
| [CLI Guide](docs/guide-cli.md) | Command line operation |
| [Coding Guide](docs/guide-coding.md) | Python development |
| [Docs Index](docs/index.md) | Unified entry for external/internal docs |
| [External Docs](docs/external/README.md) | Customer-facing 3 Studio narrative |
| [Internal Docs](docs/internal/README.md) | Platform build/extend/operate handbook |
| [3 Studio Guide](docs/studios.md) | Product lines, personas, audit policy |
| [Skills Guide](docs/guide-skills.md) | Auto-evolution system |
| [Built-in Skills Guide](docs/guide-builtin-skills.md) | DB/Payment/Auth/Deploy (NEW) |
| [LLM Router](docs/guide-llm-router.md) | Multi-model switching (NEW) |
| [Architecture](docs/architecture.md) | Design & Structure |
| [Agent Lightning Alignment](docs/design/AGENT_LIGHTNING_ALIGNMENT_DESIGN.md) | Agent Lightning inspired design updates (NEW) |
| [Protocols](docs/protocols.md) | MCP/A2A/AG-UI/A2UI |
| [Quick Start](docs/quickstart.md) | Getting Started |
| [Standards](docs/DEVELOPMENT_STANDARDS_JA.md) | Coding Standards |

---

## 🤝 Contributing

- [Contributing Guide](CONTRIBUTING.md)
- [Changelog](CHANGELOG.md)

---

## 🙏 Acknowledgements

Our recent execution/training decoupling and trace/reward design are inspired by  
[Microsoft Agent Lightning](https://github.com/microsoft/agent-lightning).

---

## 📄 License

[MIT License](LICENSE)

---

<div align="center">

**Accelerate AI Agent Development with AgentFlow!**

Made with ❤️ by the AgentFlow Team

</div>
