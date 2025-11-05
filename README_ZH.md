# AgentFlow

<div align="center">

**轻量级 AI 代理开发框架**

_基于 PocketFlow 的统一协议接口_

[![Python 3.13+](https://img.shields.io/badge/python-3.13+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-396%20passed-brightgreen.svg)](tests/)
[![Coverage](https://img.shields.io/badge/coverage-89.79%25-brightgreen.svg)](htmlcov/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Code style: ruff](https://img.shields.io/badge/code%20style-ruff-000000.svg)](https://github.com/astral-sh/ruff)

[文档](https://github.com/liushuang393/serverlessAIAgents/tree/main/docs) | [示例](https://github.com/liushuang393/serverlessAIAgents/tree/main/examples) | [贡献指南](CONTRIBUTING.md)

**语言**: [English](README_EN.md) | [日本語](README.md) | 简体中文

</div>

---

## ⚠️ 项目状态

> **注意**: 本项目目前处于开发阶段，**尚未经过人工测试**。
> 
> - ✅ **自动化测试**: 396 个测试，89.79% 覆盖率
> - ⚠️ **人工测试**: 未实施
> - 🚧 **生产环境**: 使用前请进行充分测试
> 
> 如果您考虑在生产环境中使用，请事先进行充分测试，如有问题请在 [GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues) 中报告。

---

## 🎯 什么是 AgentFlow

AgentFlow 是一个轻量级 AI 代理开发框架，通过统一接口提供 **MCP（Model Context Protocol）**、**A2A（Agent-to-Agent）** 和 **AG-UI（Agent-UI）** 三种开放协议。

### ✨ 主要特性

| 特性 | 说明 | 优势 |
|------|------|------|
| 🚀 **轻量设计** | 核心代码 ~500 行 | 快速启动，低内存占用 |
| 🔌 **三协议集成** | MCP / A2A / AG-UI | 一套代码支持多种协议 |
| 🎨 **自动适配器** | `@auto_adapt` 装饰器 | 自动化协议转换 |
| 📦 **CLI 工具** | 全面的命令行工具 | 简化项目管理 |
| 🏪 **市场** | 代理共享平台 | 搜索和安装可复用代理 |
| 🧪 **高质量** | 396 个测试，89.79% 覆盖率 | 生产环境就绪的可靠性 |
| 🔒 **类型安全** | 100% 类型注解，mypy strict | 开发时早期发现错误 |
| ⚡ **异步优先** | 完全异步 I/O | 高吞吐量处理 |

### 🎁 AgentFlow 的优势

- **学习成本低**: 简单的 API，丰富的示例，全面的文档
- **协议无关**: 一个代理支持多种协议
- **可扩展性**: 模块化设计，易于定制
- **生产就绪**: 高测试覆盖率和类型安全
- **活跃开发**: 持续改进和社区支持

## 📦 安装

### 快速安装

```bash
# 从 PyPI 安装
pip install agentflow

# 或使用 Conda 环境
conda env create -f environment.yml
conda activate agentflow
pip install agentflow
```

### 开发者安装

如果您想参与开发，请参考 **[入门指南](docs/getting-started-ja.md)** 或 **[开发指南](docs/development.md)**。

### 验证安装

```bash
agentflow --version
# agentflow, version 1.0.0
```

---

## 🚀 快速开始

### 5 分钟创建代理

```bash
# 1. 创建项目
agentflow init my-agent && cd my-agent

# 2. 实现代理（编辑 agent.py）
cat > agent.py << 'EOF'
from agentflow.core.agent_block import AgentBlock
from typing import Any

class MyAgent(AgentBlock):
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        return {"result": input_data.get("text", "").upper()}
EOF

# 3. 运行
agentflow run . --input '{"text": "hello"}'
# Output: {"result": "HELLO"}
```

详情请参考 [快速入门指南](docs/quickstart.md)。

---

## 🎯 功能列表

### 核心功能

| 功能 | 说明 | 文档 |
|------|------|------|
| **AgentBlock** | 代理基类 | [API 参考](docs/api.md#agentblock) |
| **@auto_adapt** | 协议自动转换装饰器 | [API 参考](docs/api.md#auto-adapt) |
| **AgentFlowEngine** | 基于 PocketFlow 的工作流引擎 | [API 参考](docs/api.md#engine) |
| **CLI** | 命令行工具 | [CLI 参考](docs/cli.md) |
| **Marketplace** | 代理共享平台 | [API 参考](docs/api.md#marketplace) |
| **Template System** | 项目模板 | [模板指南](docs/templates.md) |

### 协议支持

| 协议 | 说明 | Python 版本 | 文档 |
|------|------|-------------|------|
| **MCP** | Model Context Protocol（工具连接） | 3.10+ | [协议指南](docs/protocols.md#mcp) |
| **A2A** | Agent-to-Agent（代理协作） | 3.9+ | [协议指南](docs/protocols.md#a2a) |
| **AG-UI** | Agent-UI（前端集成） | 3.13+ | [协议指南](docs/protocols.md#ag-ui) |

### ⚠️ 已知问题

#### MCP Client (Python 3.13 兼容性问题)

- **问题**: Python 3.13 + Pydantic 2.x 存在已知的兼容性问题
- **影响**: MCP Client 模块暂时无法在 Python 3.13 环境下使用
- **已采取的措施**:
  - ✅ 引入 Pydantic v1 兼容层
  - ✅ 创建全面的测试套件（20 个测试，理论覆盖率 98%+）
  - ✅ 详细状态报告：[MCP_CLIENT_STATUS_REPORT.md](MCP_CLIENT_STATUS_REPORT.md)
- **解决方案**:
  - 短期：使用 Python 3.12 或更低版本，或等待 Pydantic 2.13+ 修复
  - 中期：在 CI/CD 中添加 Python 3.12 环境
  - 长期：fork MCP 库或实现独立版本
- **注意**: 不影响核心功能（可选模块）

### CLI 命令

```bash
agentflow init <project>        # 初始化项目
agentflow create agent <name>   # 创建代理
agentflow run <path>            # 运行代理
agentflow search [query]        # 搜索市场
agentflow install <agent-id>    # 安装代理
agentflow template list         # 列出模板
```

详情请参考 [CLI 参考](docs/cli.md)。

---

## 📚 文档

### 入门指南

- **[入门指南](docs/getting-started-ja.md)** - 从安装到日常使用（初学者推荐）⭐
- [快速入门](docs/quickstart.md) - 10 分钟创建第一个代理
- [实现指南](docs/implementation-guide.md) - 各层实现方法和最佳实践
- [示例集](examples/) - 5 个实用代理示例

### 参考文档

- [API 参考](docs/api.md) - 完整的 API 文档
- [协议指南](docs/protocols.md) - MCP/A2A/AG-UI 详解
- [CLI 参考](docs/cli.md) - 所有命令说明
- [架构](docs/architecture.md) - 系统设计和设计理念

### 开发者文档

- [代码质量检查指南](docs/quality-checks.md) - 质量检查工具使用方法
- [开发指南](docs/development.md) - 开发环境设置和贡献方法
- [贡献指南](CONTRIBUTING.md) - 编码规范和 PR 流程
- [变更日志](CHANGELOG.md) - 版本历史和变更内容

---

## 🏗️ 架构

AgentFlow 采用 4 层模块化架构：

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

详情请参考 [架构文档](docs/architecture.md)。

---

## 🤝 贡献

欢迎为 AgentFlow 做出贡献！

### 贡献方式

请参考以下文档了解如何为 AgentFlow 做出贡献：

- **[开发指南](docs/development.md)** - 开发环境设置和开发流程
- **[贡献指南](CONTRIBUTING.md)** - 编码规范和 PR 流程
- **[代码质量检查指南](docs/quality-checks.md)** - 质量检查工具使用方法

**简单步骤**:

1. Fork 仓库
2. 设置开发环境（参考[开发指南](docs/development.md)）
3. 创建分支并进行更改
4. 运行质量检查（`.\check.ps1 all` 或 `check.bat all`）
5. 创建 Pull Request

### 行为准则

所有贡献者都需要遵守 [行为准则](CONTRIBUTING.md#行動規範)。

---

## 📄 许可证

AgentFlow 在 [MIT License](LICENSE) 下发布。

---

## 🙏 致谢

AgentFlow 得益于以下开源项目和社区的支持：

### 核心库

- **[PocketFlow](https://github.com/pocketflow/pocketflow)** - 轻量级工作流引擎基础
- **[Pydantic](https://github.com/pydantic/pydantic)** - 数据验证和配置管理
- **[Click](https://github.com/pallets/click)** - CLI 框架
- **[Rich](https://github.com/Textualize/rich)** - 美观的终端输出
- **[FastAPI](https://github.com/tiangolo/fastapi)** - 高性能 Web 框架
- **[Ruff](https://github.com/astral-sh/ruff)** - 快速 Python linter 和 formatter

### 协议

- **[MCP (Model Context Protocol)](https://modelcontextprotocol.io/)** - Anthropic 的 LLM 工具连接协议
- **[A2A (Agent-to-Agent Protocol)](https://a2a.dev/)** - 代理间通信标准协议
- **[AG-UI](https://github.com/ag-ui/ag-ui)** - 代理 UI 集成协议

### 开发工具

- **[pytest](https://github.com/pytest-dev/pytest)** - 测试框架
- **[mypy](https://github.com/python/mypy)** - 静态类型检查器
- **[pre-commit](https://github.com/pre-commit/pre-commit)** - Git hook 框架

---

## 📞 支持

### 社区

- 💬 **Discussions**: [GitHub Discussions](https://github.com/liushuang393/serverlessAIAgents/discussions) - 问题、想法、反馈
- 🐛 **Issues**: [GitHub Issues](https://github.com/liushuang393/serverlessAIAgents/issues) - Bug 报告、功能请求
- 📖 **Documentation**: [docs/](https://github.com/liushuang393/serverlessAIAgents/tree/main/docs) - 全面的文档

### 联系方式

- 📧 **Email**: <115070984+liushuang393@users.noreply.github.com>
- 👤 **GitHub**: [@liushuang393](https://github.com/liushuang393)
- 📦 **Repository**: [serverlessAIAgents](https://github.com/liushuang393/serverlessAIAgents)

---

<div align="center">

**使用 AgentFlow 加速 AI 代理开发！**

[开始使用](docs/quickstart.md) | [文档](docs/) | [示例](examples/) | [贡献](CONTRIBUTING.md)

Made with ❤️ by the AgentFlow Team

</div>

