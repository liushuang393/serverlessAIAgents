# AgentFlow

<div align="center">

**轻量级 AI 代理开发框架**

_基于 PocketFlow 的统一协议接口_

[![Python 3.13+](https://img.shields.io/badge/python-3.13+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-434%20passed-brightgreen.svg)](tests/)
[![Coverage](https://img.shields.io/badge/coverage-92.46%25-brightgreen.svg)](htmlcov/)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Code style: ruff](https://img.shields.io/badge/code%20style-ruff-000000.svg)](https://github.com/astral-sh/ruff)

[文档](docs/) | [示例](examples/) | [贡献指南](CONTRIBUTING.md)

**语言**: [English](README_EN.md) | [日本語](README.md) | 简体中文

</div>

---

## ⚠️ 项目状态

> **注意**: 本项目目前处于开发阶段。
>
> - ✅ **自动化测试**: 434 个测试，92.46% 覆盖率
> - 🚧 **生产环境**: 使用前请进行充分测试

---

## 🎯 什么是 AgentFlow

轻量级 AI 代理框架，统一接口提供 **MCP / A2A / AG-UI / A2UI** 四种协议。

### ✨ 主要特性

| 特性 | 说明 |
|------|------|
| 🚀 **轻量** | 核心代码 ~500 行 |
| 🔌 **4 协议** | MCP / A2A / AG-UI / A2UI |
| 🎨 **自动适配** | `@auto_adapt` 协议自动转换 |
| 🧠 **Skills 自动进化** | 越用越厉害 |
| 📦 **CLI** | `agentflow init/run/create` |
| 🔒 **类型安全** | 100% 类型注解 |
| ⚡ **异步** | 完全异步 I/O |

### 🎯 Skills 自动进化系统（NEW）

Claude Code Skills 完全兼容的自动进化能力系统：

```
用户需求 → 技能匹配 → 存在则执行
                   → 不存在则自动生成 → 验证 → 固化
= 越用越厉害
```

```python
from agentflow.skills import SkillEngine

engine = SkillEngine(auto_learn=True)
result = await engine.resolve("从PDF提取文本")

if result.generated:
    print(f"🆕 新技能自动生成: {result.skill.name}")
```

详情请参考 [Skills 指南](docs/guide-skills.md)。

### 🏗️ 内置生产级 Skills（NEW）

开箱即用的企业级技能包：

| 技能 | 说明 | 支持服务 |
|------|------|----------|
| 🗄️ **database-manager** | 数据库统一管理、CRUD、RLS | Supabase / Turso / PostgreSQL |
| 💳 **stripe-payment** | 支付与订阅管理 | Stripe Checkout / Billing |
| 🚀 **deployment-manager** | 部署与环境管理 | Vercel / Cloudflare Pages |
| 🔐 **auth-provider** | 认证与会话管理 | Supabase Auth / Clerk |
| 🔄 **model-router** | 多模型切换与成本优化 | OpenAI / Anthropic / Google |

```python
# 数据库集成
from agentflow.skills.builtin.database_manager import DatabaseManager, SupabaseConfig

db = DatabaseManager(provider="supabase", config=SupabaseConfig(
    url="https://xxx.supabase.co",
    anon_key="eyJ...",
))
await db.connect()
users = await db.select("users", filters={"status": "active"})

# 支付集成
from agentflow.skills.builtin.stripe_payment import StripePayment, StripeConfig

stripe = StripePayment(StripeConfig(secret_key="sk_..."))
session = await stripe.create_checkout_session(
    customer_email="user@example.com",
    line_items=[{"price": "price_xxx", "quantity": 1}],
    mode="subscription",
)

# 多模型切换
from agentflow.llm import ModelRouter, RoutingStrategy

router = ModelRouter.from_env()  # 从环境变量加载API密钥
response = await router.chat(messages)  # 自动选择最佳模型
```

详情请参考 [内置 Skills 指南](docs/guide-builtin-skills.md)。

### 🧠 协调模式

| 模式 | 说明 |
|------|------|
| **Supervisor** | 监督者动态选择工作者 |
| **Hierarchical** | 层级式任务分解 |
| **Sequential/Concurrent** | 顺序/并行执行 |

---

## 📦 安装

```bash
# Conda 环境
conda env create -f environment.yml
conda activate agentflow

# 或 pip
pip install -e .
```

---

## 🚀 快速开始

```bash
# 创建项目
agentflow init my-agent && cd my-agent

# 运行
agentflow run . --input '{"text": "hello"}'
```

详情请参考 [快速入门](docs/quickstart.md)。

---

## 🎨 使用场景

AgentFlow 提供三种操作方式，根据用途选择最适合的方式。

### 1. 🖱️ Studio UI（可视化编辑器）

**无需编写代码，在浏览器中拖拽创建工作流**

- ✅ **新手友好**: 无需编程知识
- ✅ **可视化**: 直观理解和编辑工作流
- ✅ **快速**: 几分钟创建工作流

📖 [Studio UI 指南](docs/guide-studio-ui.md)

---

### 2. ⚡ CLI（命令行）

**从终端快速运行和管理代理**

- ✅ **高效**: 无需 GUI 快速操作
- ✅ **自动化**: 适合脚本和批处理
- ✅ **简单**: 一条命令执行

📖 [CLI 指南](docs/guide-cli.md)

---

### 3. 🐍 编码（Python）

**使用 Python 代码开发和自定义代理**

- ✅ **灵活**: 完全可定制
- ✅ **类型安全**: 100% 类型注解支持
- ✅ **可扩展**: 协议集成和协调模式

📖 [编码指南](docs/guide-coding.md)

---

## 📚 文档

| 文档 | 说明 |
|------|------|
| [Studio UI 指南](docs/guide-studio-ui.md) | 可视化编辑器操作 |
| [CLI 指南](docs/guide-cli.md) | 命令行操作 |
| [编码指南](docs/guide-coding.md) | Python 开发 |
| [Skills 指南](docs/guide-skills.md) | 自动进化系统 |
| [内置 Skills 指南](docs/guide-builtin-skills.md) | 数据库/支付/认证/部署（NEW） |
| [LLM 路由器](docs/guide-llm-router.md) | 多模型切换（NEW） |
| [架构](docs/architecture.md) | 设计思想与结构 |
| [协议](docs/protocols.md) | MCP/A2A/AG-UI/A2UI |
| [API](docs/api.md) | API 参考 |
| [CLI](docs/cli.md) | 命令列表 |
| [快速入门](docs/quickstart.md) | 入门指南 |
| [开发规范](docs/DEVELOPMENT_STANDARDS_JA.md) | 编码规范 |

---

## 🤝 贡献

- [贡献指南](CONTRIBUTING.md)
- [变更日志](CHANGELOG.md)

---

## 📄 许可证

[MIT License](LICENSE)

---

<div align="center">

**使用 AgentFlow 加速 AI 代理开发！**

Made with ❤️ by the AgentFlow Team

</div>

