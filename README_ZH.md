# AgentFlow

**AI 代理开发基盘与平台** — 以统一接口支持 MCP、A2A、AG-UI、A2UI 的轻量框架。

**语言**: [English](README_EN.md) | 简体中文 | [日本語](README.md)

---

## 1. 概要与特点

AgentFlow 在**单一 API 面**上统一多协议与代理协同。面向客户以 **3 条 Studio 产品线**（Migration Studio / Enterprise FAQ Studio / Computer Assistant Studio）交付；面向开发以 Kernel（`agentflow`）与插件扩展为设计前提。

| 特点                | 说明                                                                        |
| ------------------- | --------------------------------------------------------------------------- |
| **8 层架构**        | 应用、UI、流程、Agent、工具、Provider、协议、基础设施的职责分离             |
| **4 协议统一**      | MCP / A2A / AG-UI / A2UI 在同一代码库中使用                                 |
| **3 Studio 产品线** | 客户动线统一为「模板 → 配置 → 执行 → 成果物」                               |
| **开发方式可选**    | `@agent` 装饰器 / `create_flow` / AgentCoordinator 覆盖从简单到高级         |
| **Engine 模式**     | SimpleEngine / PipelineEngine / GateEngine / RAGEngine / PEVEngine 开箱即用 |
| **类型安全・异步**  | 100% 类型注解，I/O 以 async/await 为先                                      |
| **Skills 自动进化** | 随使用扩展能力的插件机制                                                    |

---

## 2. 主要机能

- **Engine 执行**: `SimpleEngine`（单 Agent）、`PipelineEngine`（多段・Review 循环）、`GateEngine`（入口审查）、`RAGEngine`（检索增强）、`PEVEngine`（Plan-Execute-Verify）
- **Agent 定义**: `@agent` 装饰器、继承 `AgentBlock`、通过 `AgentClient.get("名称").invoke(...)` 调用
- **流程构建**: `create_flow(...).gate(...).then(...).parallel(...).review(...).build()`
- **松耦合 Provider**: `get_llm()` / `get_vectordb()` / `get_db()` / `get_embedding()` 按环境获取实现
- **通道**: 多平台消息统一（MessageGateway / MessageChannelAdapter）
- **HITL**: 审批・中断・恢复（ApprovalManager / Checkpointer / interrupt）
- **Context Engineering**: 令牌预算、轮次压缩、RetrievalGate、KeyNotes 等
- **内置 Skills**: database-manager / stripe-payment / deployment-manager / auth-provider 等（可选）

---

## 3. 技术架构

**8 层结构**（自上而下）：应用 → UI → 流程 → Agent → 工具 → Provider → 协议 → 基础设施。上层仅依赖下层，契约通过 `agentflow/__init__.py` 的公开 API 使用。

**技术栈**: Python 3.13+ / FastAPI / Pydantic / Uvicorn（后端），React・Vite・TypeScript（Studio 与 apps 前端），MCP・A2A・AG-UI・A2UI（协议），PocketFlow 等（工作流基盘）。质量工具：Ruff、mypy、pytest。

---

## 4. 基盘・Platform・App 的层级与作用

| 层级                          | 作用                                                                                                                        | 示例                                                                                              |
| ----------------------------- | --------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| **Kernel（agentflow）**       | 稳定 API、Engine、Provider、协议抽象；扩展以 Plugin 优先；带副作用操作经策略与审计。                                        | `agentflow` 包、公开 API                                                                          |
| **Platform（apps/platform）** | 3 Studio 的执行动线（模板→配置→执行→成果物）与 Framework 管理 API。正轨 API：`/api/studios/*`、`/api/studios/framework/*`。 | 后端 `apps.platform.main`，前端 `apps/platform/frontend`                                          |
| **Apps（apps/\*）**           | 产品与示例应用。对应 Migration / FAQ / Assistant 等 Studio 的 app，以及编排、消息等横向 app。                               | `code_migration_assistant`、`faq_system`、`decision_governance_engine`、`market_trend_monitor` 等 |

对外说明统一为 3 Studio，协议名与内部层级不在业务面暴露。

---

## 5. 快速开始・文档・许可证

**运行前**: 默认环境为 `conda activate agentflow`。执行命令前请阅读 `code-rules/CLAUDE.md` 及目标 app 的 README。

```bash
conda activate agentflow
pip install -e ".[apps,dev]"
python -m apps.platform.main serve --port 8000
# 另开终端: cd apps/platform/frontend && npm install && npm run dev
```

- **文档**: 目录 [docs/index.md](docs/index.md)、对外 [docs/external/README.md](docs/external/README.md)、对内 [docs/internal/README.md](docs/internal/README.md)、3 Studio [docs/studios.md](docs/studios.md)
- **仓库**: [GitHub](https://github.com/liushuang393/serverlessAIAgents) | [Issues](https://github.com/liushuang393/serverlessAIAgents/issues)
- **许可证**: [MIT License](LICENSE)

执行/训练解耦与轨迹设计部分参考了 [Microsoft Agent Lightning](https://github.com/microsoft/agent-lightning) 的思路。
