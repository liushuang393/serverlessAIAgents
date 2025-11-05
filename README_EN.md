# AgentFlow

<div align="center">

**Lightweight AI Agent Development Framework**

_Unified Protocol Interface Based on PocketFlow_

[![Python 3.13+](https://img.shields.io/badge/python-3.13+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-396%20passed-brightgreen.svg)](tests/)
[![Coverage](https://img.shields.io/badge/coverage-89.79%25-brightgreen.svg)](htmlcov/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Code style: ruff](https://img.shields.io/badge/code%20style-ruff-000000.svg)](https://github.com/astral-sh/ruff)

[Documentation](https://github.com/liushuang393/serverlessAIAgents/tree/main/docs) | [Examples](https://github.com/liushuang393/serverlessAIAgents/tree/main/examples) | [Contributing](CONTRIBUTING.md)

**Languages**: English | [简体中文](README_ZH.md) | [日本語](README.md)

</div>

---

## ⚠️ Project Status

> **Notice**: This project is currently under development and **has not yet undergone manual testing**.
> 
> - ✅ **Automated Tests**: 396 tests, 89.79% coverage
> - ⚠️ **Manual Testing**: Not yet performed
> - 🚧 **Production Use**: Please test thoroughly before use
> 
> If you are considering using this in production, please conduct thorough testing beforehand and report any issues via [GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues).

---

## 🎯 What is AgentFlow

AgentFlow is a lightweight AI agent development framework that provides three open protocols through a unified interface: **MCP (Model Context Protocol)**, **A2A (Agent-to-Agent)**, and **AG-UI (Agent-UI)**.

### ✨ Key Features

| Feature | Description | Benefits |
|---------|-------------|----------|
| 🚀 **Lightweight Design** | Core code ~500 lines | Fast startup, low memory footprint |
| 🔌 **Three Protocol Integration** | MCP / A2A / AG-UI | One codebase supports multiple protocols |
| 🎨 **Auto Adapter** | `@auto_adapt` decorator | Automated protocol conversion |
| 📦 **CLI Tools** | Comprehensive command-line tools | Simplified project management |
| 🏪 **Marketplace** | Agent sharing platform | Search and install reusable agents |
| 🧪 **High Quality** | 396 tests, 89.79% coverage | Production-ready reliability |
| 🔒 **Type Safety** | 100% type annotations, mypy strict | Early error detection during development |
| ⚡ **Async First** | Fully asynchronous I/O | High throughput processing |

### 🎁 Advantages of AgentFlow

- **Low Learning Curve**: Simple API, rich examples, comprehensive documentation
- **Protocol Agnostic**: One agent supports multiple protocols
- **Extensibility**: Modular design, easy to customize
- **Production Ready**: High test coverage and type safety
- **Active Development**: Continuous improvement and community support

## 📦 Installation

### Quick Install

```bash
# Install from PyPI
pip install agentflow

# Or use Conda environment
conda env create -f environment.yml
conda activate agentflow
pip install agentflow
```

### Developer Installation

If you want to contribute to development, please refer to the **[Getting Started Guide](docs/getting-started-ja.md)** or **[Development Guide](docs/development.md)**.

### Verify Installation

```bash
agentflow --version
# agentflow, version 1.0.0
```

---

## 🚀 Quick Start

### Create an Agent in 5 Minutes

```bash
# 1. Create project
agentflow init my-agent && cd my-agent

# 2. Implement agent (edit agent.py)
cat > agent.py << 'EOF'
from agentflow.core.agent_block import AgentBlock
from typing import Any

class MyAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return {"result": input_data.get("text", "").upper()}
EOF

# 3. Run
agentflow run . --input '{"text": "hello"}'
# Output: {"result": "HELLO"}
```

For more details, see the [Quick Start Guide](docs/quickstart.md).

---

## 🎯 Feature List

### Core Features

| Feature | Description | Documentation |
|---------|-------------|---------------|
| **AgentBlock** | Agent base class | [API Reference](docs/api.md#agentblock) |
| **@auto_adapt** | Protocol auto-conversion decorator | [API Reference](docs/api.md#auto-adapt) |
| **AgentFlowEngine** | Workflow engine based on PocketFlow | [API Reference](docs/api.md#engine) |
| **CLI** | Command-line tools | [CLI Reference](docs/cli.md) |
| **Marketplace** | Agent sharing platform | [API Reference](docs/api.md#marketplace) |
| **Template System** | Project templates | [Template Guide](docs/templates.md) |

### Protocol Support

| Protocol | Description | Python Version | Documentation |
|----------|-------------|----------------|---------------|
| **MCP** | Model Context Protocol (tool connection) | 3.10+ | [Protocol Guide](docs/protocols.md#mcp) |
| **A2A** | Agent-to-Agent (agent collaboration) | 3.9+ | [Protocol Guide](docs/protocols.md#a2a) |
| **AG-UI** | Agent-UI (frontend integration) | 3.13+ | [Protocol Guide](docs/protocols.md#ag-ui) |

### ⚠️ Known Issues

#### MCP Client (Python 3.13 Compatibility Issue)

- **Issue**: Known compatibility issue with Python 3.13 + Pydantic 2.x
- **Impact**: MCP Client module temporarily unavailable in Python 3.13 environment
- **Measures Taken**:
  - ✅ Introduced Pydantic v1 compatibility layer
  - ✅ Created comprehensive test suite (20 tests, theoretical coverage 98%+)
  - ✅ Detailed status report: [MCP_CLIENT_STATUS_REPORT.md](MCP_CLIENT_STATUS_REPORT.md)
- **Solutions**:
  - Short-term: Use Python 3.12 or lower, or wait for Pydantic 2.13+ fix
  - Mid-term: Add Python 3.12 environment to CI/CD
  - Long-term: Fork MCP library or implement standalone version
- **Note**: Does not affect core functionality (optional module)

### CLI Commands

```bash
agentflow init <project>        # Initialize project
agentflow create agent <name>   # Create agent
agentflow run <path>            # Run agent
agentflow search [query]        # Search marketplace
agentflow install <agent-id>    # Install agent
agentflow template list         # List templates
```

For more details, see the [CLI Reference](docs/cli.md).

---

## 📚 Documentation

### Getting Started

- **[Getting Started Guide](docs/getting-started-ja.md)** - From installation to daily use (recommended for beginners) ⭐
- [Quick Start](docs/quickstart.md) - Create your first agent in 10 minutes
- [Implementation Guide](docs/implementation-guide.md) - Implementation methods and best practices for each layer
- [Example Collection](examples/) - 5 practical agent examples

### Reference Documentation

- [API Reference](docs/api.md) - Complete API documentation
- [Protocol Guide](docs/protocols.md) - Detailed explanation of MCP/A2A/AG-UI
- [CLI Reference](docs/cli.md) - All command descriptions
- [Architecture](docs/architecture.md) - System design and design philosophy

### Developer Documentation

- [Code Quality Check Guide](docs/quality-checks.md) - How to use quality check tools
- [Development Guide](docs/development.md) - Development environment setup and contribution methods
- [Contributing Guide](CONTRIBUTING.md) - Coding standards and PR process
- [Changelog](CHANGELOG.md) - Version history and changes

---

## 🏗️ Architecture

AgentFlow adopts a 4-layer modular architecture:

```
┌──────────────────────────────────────────┐
│  UI Layer (Optional)                     │  ← Visual Studio (React)
├──────────────────────────────────────────┤
│  Protocol Layer                          │  ← MCP / A2A / AG-UI
├──────────────────────────────────────────┤
│  Engine Layer                            │  ← AgentFlowEngine (PocketFlow)
├──────────────────────────────────────────┤
│  Tool Layer                              │  ← LLM / Database / External APIs
└──────────────────────────────────────────┘
```

For more details, see the [Architecture Documentation](docs/architecture.md).

---

## 🤝 Contributing

Contributions to AgentFlow are welcome!

### How to Contribute

Please refer to the following documents to learn how to contribute to AgentFlow:

- **[Development Guide](docs/development.md)** - Development environment setup and development process
- **[Contributing Guide](CONTRIBUTING.md)** - Coding standards and PR process
- **[Code Quality Check Guide](docs/quality-checks.md)** - How to use quality check tools

**Simple Steps**:

1. Fork the repository
2. Set up development environment (refer to [Development Guide](docs/development.md))
3. Create a branch and make changes
4. Run quality checks (`.\check.ps1 all` or `check.bat all`)
5. Create a Pull Request

### Code of Conduct

All contributors are expected to follow the [Code of Conduct](CONTRIBUTING.md#行動規範).

---

## 📄 License

AgentFlow is released under the [MIT License](LICENSE).

---

## 🙏 Acknowledgments

AgentFlow benefits from the support of the following open source projects and communities:

### Core Libraries

- **[PocketFlow](https://github.com/pocketflow/pocketflow)** - Lightweight workflow engine foundation
- **[Pydantic](https://github.com/pydantic/pydantic)** - Data validation and configuration management
- **[Click](https://github.com/pallets/click)** - CLI framework
- **[Rich](https://github.com/Textualize/rich)** - Beautiful terminal output
- **[FastAPI](https://github.com/tiangolo/fastapi)** - High-performance web framework
- **[Ruff](https://github.com/astral-sh/ruff)** - Fast Python linter and formatter

### Protocols

- **[MCP (Model Context Protocol)](https://modelcontextprotocol.io/)** - Anthropic's LLM tool connection protocol
- **[A2A (Agent-to-Agent Protocol)](https://a2a.dev/)** - Standard protocol for inter-agent communication
- **[AG-UI](https://github.com/ag-ui/ag-ui)** - Agent UI integration protocol

### Development Tools

- **[pytest](https://github.com/pytest-dev/pytest)** - Testing framework
- **[mypy](https://github.com/python/mypy)** - Static type checker
- **[pre-commit](https://github.com/pre-commit/pre-commit)** - Git hook framework

---

## 📞 Support

### Community

- 💬 **Discussions**: [GitHub Discussions](https://github.com/liushuang393/serverlessAIAgents/discussions) - Questions, ideas, feedback
- 🐛 **Issues**: [GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues) - Bug reports, feature requests
- 📖 **Documentation**: [docs/](https://github.com/liushuang393/serverlessAIAgents/tree/main/docs) - Comprehensive documentation

### Contact

- 📧 **Email**: <115070984+liushuang393@users.noreply.github.com>
- 👤 **GitHub**: [@liushuang393](https://github.com/liushuang393)
- 📦 **Repository**: [serverlessAIAgents](https://github.com/liushuang393/serverlessAIAgents)

---

<div align="center">

**Accelerate AI Agent Development with AgentFlow!**

[Get Started](docs/quickstart.md) | [Documentation](docs/) | [Examples](examples/) | [Contributing](CONTRIBUTING.md)

Made with ❤️ by the AgentFlow Team

</div>

