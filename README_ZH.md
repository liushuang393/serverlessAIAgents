# BizCore AI

**BizCore AI 是面向 AI 时代与未来 AGI 的企业级业务开发基盘，通过分层架构承接 AI 系统复杂度，使开发者能够长期专注于业务实现。**

**语言**: [English](README_EN.md) | [日本語](README.md) | 简体中文

---

## 为什么选择 BizCore AI

AI 模型快速迭代，协议持续演进，业务应用爆发增长。  
面对这一不确定的未来，**基盘承接系统复杂度、开发者专注业务实现** 的分工不可或缺。

BizCore AI 正是实现这一哲学的 **全栈企业开发基盘**。

---

## 7 个核心层 + Apps 外层

BizCore AI 由 7 个核心层组成，`apps/` 作为外侧的产品装配与交付层存在。

```
┌─────────────────────────────────────────────┐
│  BizCore Studios (Apps)                     │  ← 产品装配层（不计入核心层）
│  Migration / FAQ / Assistant / Custom Apps  │
├─────────────────────────────────────────────┤
│  BizCore Control Plane                      │  ← 管理・观测・分发
├─────────────────────────────────────────────┤
│  BizCore Domain                             │  ← 业务域模型・行业模板
├─────────────────────────────────────────────┤
│  BizCore Harness (Governance)               │  ← 治理・可靠性・评估
├─────────────────────────────────────────────┤
│  BizCore Kernel (Runtime)                   │  ← 执行引擎・Agent・流程
├─────────────────────────────────────────────┤
│  Shared Services                            │  ← 共享功能（Gateway/RAG/Access）
├─────────────────────────────────────────────┤
│  Infrastructure                             │  ← 底层基础设施（LLM/Storage/Cache）
├─────────────────────────────────────────────┤
│  Contracts                                  │  ← 协议・接口定义
└─────────────────────────────────────────────┘
```

| 层                        | 目录              | 职责                                                         |
| ------------------------- | ----------------- | ------------------------------------------------------------ |
| **Contracts**             | `contracts/`      | 全层共享的协议・类型・接口定义（Versioned）                  |
| **Infrastructure**        | `infrastructure/` | LLM Provider、Storage、Cache、Queue、Sandbox 等底层基础设施  |
| **Shared**                | `shared/`         | LLM Gateway、RAG、Access Control、Trace、Audit 等共享服务    |
| **BizCore Kernel**        | `kernel/`         | Agent Runtime、Flow Engine、Orchestration、Protocol 实现     |
| **BizCore Harness**       | `harness/`        | Governance、Policy、Approval、Budget、Evaluation、Guardrails |
| **BizCore Domain**        | `domain/`         | 行业/业务域模型、接口、模板与规则                            |
| **BizCore Control Plane** | `control_plane/`  | 平台控制面，负责发现、生命周期、分发与运营 UI/API            |
| **BizCore Studios**       | `apps/*/`         | 产品层，用于装配 Migration / FAQ / Assistant / custom apps   |

> **设计原则**: `contracts / infrastructure / shared / kernel / harness / domain` 不得依赖 `apps/`。`domain` 不得依赖 `control_plane`。`control_plane` 只能编排下层，不能反向成为下层的正本入口。

---

## 主要功能

### 执行引擎（BizCore Kernel）

- **Engine 模式**: `SimpleEngine`（单 Agent）/ `PipelineEngine`（多段・Review）/ `GateEngine`（入口审查）/ `RAGEngine`（检索增强）/ `PEVEngine`（Plan-Execute-Verify）
- **Agent 定义**: `@agent` 装饰器 / `AgentBlock` 继承 / `AgentClient.get("名称").invoke(...)`
- **流程构建**: `create_flow(...).gate(...).then(...).parallel(...).review(...).build()`
- **统一协议**: MCP / A2A / AG-UI / A2UI 在单一 API 面使用

### 治理（BizCore Harness）

- **策略引擎**: 以声明方式配置执行前后的判定与约束
- **HITL**: 审批・中断・恢复（ApprovalManager / Checkpointer / interrupt）
- **Context Engineering**: 令牌预算、轮次压缩、RetrievalGate、KeyNotes
- **评估・审计**: Evaluation、Replay、Budget 管理

### 管理（BizCore Platform）

- **App 生命周期**: 创建 → 配置 → 执行 → 观测 → 分发
- **LLM 统一管理**: Provider / Model / Secret / Local Engine 由 Platform 统一管理
- **统一 API**: `/api/studios/*` 与 `/api/studios/framework/apps/*` 作为正规路径

### 业务应用（BizCore Studios）

- **Migration Studio**: 代码迁移・现代化改造
- **Enterprise FAQ Studio**: 基于 RAG 的 FAQ・知识管理
- **Computer Assistant Studio**: 通用 AI 助手・自动化

---

## LLM 推理基础设施（Gateway 统一）

BizCore AI 禁止直接调用 Provider SDK，所有 LLM 请求统一走 `LLM Orchestrator → LLM Gateway`。

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

## 仓库结构

### 7 个核心层

- `contracts/`: 协议・接口定义（Versioned）
- `infrastructure/`: 底层基础设施（LLM Provider / Storage / Cache / Queue）
- `shared/`: 共享服务（Gateway / RAG / Access / Trace / Audit）
- `kernel/`: Agent Runtime / Flow Engine / Orchestration / Protocol
- `harness/`: Governance / Policy / Approval / Budget / Evaluation
- `domain/`: 业务域的正本实现
- `control_plane/`: BizCore Control Plane 的正本实现

### 产品层

- `apps/`: BizCore Studios 与 custom apps，位于 7 个核心层之外

### 迁移说明

- 迁移期间仍可能保留部分旧 import 与配置命名，但正本实现统一收敛到 `contracts/`、`infrastructure/`、`shared/`、`kernel/`、`harness/`、`domain/`、`control_plane/`

### 开发・运维

- `plugins/`: 扩展功能（Blocks / Tools / Providers）
- `docs/`: 对外/对内文档、设计资料
- `tests/`: 自动化测试集（Unit / Integration / E2E）
- `code-rules/`: 统一编码规范与 Lint 规则
- `scripts/`: 开发・维护辅助脚本

---

## 技术栈

| 领域         | 技术                                                    |
| ------------ | ------------------------------------------------------- |
| **后端**     | Python 3.13+, FastAPI, Pydantic, Uvicorn                |
| **前端**     | React, Vite, TypeScript, ESLint, Prettier               |
| **AI 协议**  | MCP, A2A, AG-UI, A2UI                                   |
| **基础设施** | Supabase / PostgreSQL / Turso, Pinecone / Qdrant, Redis |
| **质量**     | Ruff, mypy, pytest（80%+ 覆盖率）, ESLint, tsc          |

---

## 快速开始

> 各应用（FAQ System、Decision Governance Engine 等）的 DB 构建・本地启动・Docker 部署步骤，请参考各应用的 README.md。
> 此处仅记载共享基础设施（auth_service + control_plane）的启动步骤。

### 前提条件

| 工具                    | 版本  | 用途                        |
| ----------------------- | ----- | --------------------------- |
| Python                  | 3.13+ | 后端运行                    |
| conda                   | 任意  | 推荐虚拟环境（`agentflow`） |
| Docker + Docker Compose | 最新  | DB・容器启动                |
| Node.js                 | 18+   | 前端开发                    |

### 1. 环境搭建

```bash
# 创建并激活 conda 环境（推荐）
conda create -n agentflow python=3.13 -y
conda activate agentflow

# 安装依赖
pip install -e ".[apps,dev]"
```

### 2. auth_service（认证基础设施）

支持 SQLite（本地开发）和 PostgreSQL（Docker/生产）。

#### 本地启动（SQLite 自动创建，无需 DB 步骤）

```bash
conda activate agentflow

# 启动 auth_service（端口 8010）
python -m shared.auth_service.main
# 健康检查: curl http://localhost:8010/health

# 启动前端（另开终端，端口 3000）
cd shared/auth_service/frontend && npm install && npm run dev
```

#### Docker 启动（PostgreSQL）

```bash
cd shared/auth_service
docker compose up --build -d
# 健康检查: curl http://localhost:8010/health
# 停止: docker compose down
```

### 3. control_plane（平台管理）

默认使用 SQLite，无需 DB 步骤。

#### 本地启动

```bash
conda activate agentflow

# 启动 control_plane（端口 8900）
python -m control_plane.main serve
# 健康检查: curl http://localhost:8900/health

# 启动前端（另开终端，端口 3200）
cd control_plane/frontend && npm install && npm run dev
```

### 4. dev_studio（开发支持）

代码生成・CI/CD 配置・质量分析等开发支持工具。

#### 本地启动

```bash
conda activate agentflow
python -m apps.dev_studio.main
# 健康检查: curl http://localhost:8011/health
```

#### Docker 启动

```bash
cd apps/dev_studio
docker compose up --build -d
# 健康检查: curl http://localhost:8011/health
# 停止: docker compose down
```

### 服务一览

| 服务 | 后端 | 前端 | 说明 |
|---|---|---|---|
| auth_service | http://localhost:8010 | http://localhost:3000 | 认证・用户管理 |
| control_plane | http://localhost:8900 | http://localhost:3200 | 平台管理 |

### 各应用 README

| 应用                       | README                                                                                 | 说明                      |
| -------------------------- | -------------------------------------------------------------------------------------- | ------------------------- |
| FAQ System                 | [apps/faq_system/README.md](apps/faq_system/README.md)                                 | 基于 RAG 的 FAQ・知识管理 |
| Decision Governance Engine | [apps/decision_governance_engine/README.md](apps/decision_governance_engine/README.md) | 决策支持系统              |
| Code Migration Assistant   | [apps/code_migration_assistant/README.md](apps/code_migration_assistant/README.md)     | 代码迁移支持              |

---

## 文档・链接

- **文档**: [docs/index.md](docs/index.md) | [对外](docs/external/README.md) | [对内](docs/internal/README.md) | [Studios](docs/studios.md)
- **仓库**: [GitHub](https://github.com/liushuang393/serverlessAIAgents) | [Issues](https://github.com/liushuang393/serverlessAIAgents/issues)
- **许可证**: [MIT License](LICENSE)

---

## 致谢 — 所用开源技术

BizCore AI 的开发离不开以下优秀开源项目、工具与设计思想的支撑。
谨向所有贡献者和社区表示衷心感谢。

### 🏗️ 框架层

| 项目 | 职责 | 许可证 |
| --- | --- | --- |
| [FastAPI](https://github.com/fastapi/fastapi) | 高性能异步 Web API 框架（Kernel / Control Plane 基础） | MIT |
| [Pydantic v2](https://github.com/pydantic/pydantic) | 类型安全的数据校验与 Schema 定义（Contracts 全域） | MIT |
| [SQLAlchemy](https://github.com/sqlalchemy/sqlalchemy) | ORM 与数据库抽象（Control Plane / Auth Service） | MIT |
| [Alembic](https://github.com/sqlalchemy/alembic) | 数据库迁移管理 | MIT |
| [Uvicorn](https://github.com/encode/uvicorn) | ASGI 服务器（FastAPI 运行时） | BSD-3-Clause |
| [React](https://github.com/facebook/react) | 前端 UI 框架 | MIT |
| [Vite](https://github.com/vitejs/vite) | 高速前端构建工具 | MIT |
| [TypeScript](https://github.com/microsoft/TypeScript) | 类型安全的前端开发语言 | Apache 2.0 |

### 🤖 AI 协议层

| 项目 | 职责 | 来源 |
| --- | --- | --- |
| [Model Context Protocol (MCP)](https://modelcontextprotocol.io/) | AI 工具集成协议（Layer 5 Integration 标准） | Anthropic |
| [Agent-to-Agent Protocol (A2A)](https://github.com/google-a2a/A2A) | Agent 间通信与委托协议 | Google |
| [AG-UI Protocol](https://github.com/ag-ui-protocol/ag-ui) | Agent → UI 实时流式传输协议 | 开放标准 |

### 🛠️ 工具与基础设施层

| 项目 | 职责 | 许可证 |
| --- | --- | --- |
| [Qdrant](https://github.com/qdrant/qdrant) | 高性能向量数据库（RAG Pipeline） | Apache 2.0 |
| [Redis](https://github.com/redis/redis) | 分布式缓存与消息队列 | BSD-3-Clause |
| [PostgreSQL](https://www.postgresql.org/) | 关系型数据库 | PostgreSQL License |
| [Supabase](https://github.com/supabase/supabase) | 托管 PostgreSQL + BaaS | Apache 2.0 |
| [Ruff](https://github.com/astral-sh/ruff) | 超高速 Python Linter 与格式化工具 | MIT |
| [mypy](https://github.com/python/mypy) | Python 静态类型检查器 | MIT |
| [pytest](https://github.com/pytest-dev/pytest) | Python 测试框架 | MIT |
| [ESLint](https://github.com/eslint/eslint) / [Prettier](https://github.com/prettier/prettier) | 前端代码质量与格式化工具 | MIT |
| [Docker](https://www.docker.com/) | 容器化与部署基础设施 | Apache 2.0 |

### 🧩 内置 Skill

BizCore Studios 内置的以下 Skill 基于开源工具与服务实现。

#### web-content-fetcher（网页正文提取）

| 项目 | 职责 | 许可证 |
| --- | --- | --- |
| [Jina Reader](https://github.com/jina-ai/reader) | 从 URL 提取干净 Markdown 正文的云服务（首选） | Apache 2.0 |
| [Scrapling](https://github.com/D4Vinci/Scrapling) | 具备反爬虫能力的 Python 网页抓取库（二级降级） | MIT |
| [html2text](https://github.com/Alir3z4/html2text) | HTML 转 Markdown 转换库 | GPL-3.0 |

#### design-skills（设计图像生成）

| 项目 | 职责 | 许可证 |
| --- | --- | --- |
| [ComfyUI](https://github.com/comfyanonymous/ComfyUI) | 本地 GPU 图像生成后端（首选） | GPL-3.0 |
| [Stable Diffusion XL (SDXL)](https://github.com/Stability-AI/generative-models) | 用于本地推理的高质量文生图模型 | CreativeML Open RAIL++-M |
| [OpenAI gpt-image-1](https://platform.openai.com/docs/guides/images) | ComfyUI 不可用时的云端兜底 | — (商业 API) |

#### minimalist-entrepreneur-skills（极简创业框架技能集）

基于 Sahil Lavingia（Gumroad 创始人）所著《The Minimalist Entrepreneur》的创业支持技能包，包含 10 个结构化技能，在 `apps/messaging_hub` 中使用。

| 项目 | 职责 | 许可证 |
| --- | --- | --- |
| [slavingia/skills](https://github.com/slavingia/skills) | 极简创业框架技能集（10 个技能）的原始仓库 | MIT |
| [《The Minimalist Entrepreneur》](https://www.minimalistentrepreneur.com/) — Sahil Lavingia | 技能的思想基础书籍 | — |

### 💡 设计思想与架构参考

| 思想 / 参考来源 | 概要 |
| --- | --- |
| [Microsoft Agent Lightning](https://github.com/microsoft/agent-lightning) | BizCore 执行/训练解耦模型与分布式轨迹设计的重要参考 |
| **Clean Architecture** — Robert C. Martin | 分层隔离与依赖方向原则，7 核心层架构的设计指南 |
| **ReAct Pattern** — Yao et al., 2022 | 融合 Reasoning + Acting 的 Agent 循环设计，Kernel 运行时的理论基础 |
| **RAG（检索增强生成）** — Lewis et al., 2020 | 通过检索增强实现知识集成，RAGEngine / RetrievalGate 的理论依据 |
| **HITL（人机协同）** | 将人类监督内嵌于 AI 治理的设计思想（Harness / ApprovalManager 的根基） |
| **Gateway Pattern** — Enterprise Integration Patterns | 集中式 LLM 访问、故障转移与可观测性设计，LLM Gateway 的架构依据 |
| **Contract-First Design** | 以跨层契约先行定义接口、解耦实现的设计原则，`contracts/` 层的核心哲学 |
| **12-Factor App** — Heroku | 云原生应用设计原则，应用于配置外部化、无状态化与日志标准化 |
| **微内核 Agent（百行原型）** — 内部自研 | `@agent` 装饰器最小化 Agent 定义模式的原型。起初以约 100 行实验代码实现，现已演进为 `kernel/agent_decorator.py`（Skills 集成、Pydantic 类型校验、AgentRegistry 与 A2AHub 注册） |
