# AgentFlow框架 v2.0 - 开放标准集成版

> 基于PocketFlow + 三大开放协议(MCP/A2A/AG-UI)的企业级AI Agent快速开发框架  
> 口号: **搭积木式开发,提示词驱动,开放标准互联**

---

## 🎯 核心设计理念

### 三层协议栈架构

```
┌─────────────────────────────────────────┐
│         AG-UI Layer (前端交互层)           │  用户 ↔ Agent
│  • 实时流式UI更新                          │
│  • 多模态交互(文本/语音/表单)               │
│  • 可视化Agent状态                        │
└─────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────┐
│       A2A Layer (Agent协作层)             │  Agent ↔ Agent
│  • 跨框架Agent通信                        │
│  • 任务委托与协调                          │
│  • 分布式多Agent编排                       │
└─────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────┐
│      PocketFlow Core (执行引擎)           │  
│  • Node-Flow工作流引擎                     │
│  • Shared Store数据管理                   │
│  • 重试/回退/并发控制                       │
└─────────────────────────────────────────┘
                    ↕
┌─────────────────────────────────────────┐
│        MCP Layer (工具连接层)             │  Agent ↔ Tools/Data
│  • 标准化工具调用                          │
│  • 数据源连接                             │
│  • 上下文管理                             │
└─────────────────────────────────────────┘
```

### 积木化设计四原则

#### 1️⃣ **标准接口,松耦合**
每个Agent都是独立的"积木块",通过标准协议互联:
```python
class AgentBlock:
    # A2A接口 - Agent间通信
    def get_agent_card(self) -> AgentCard: ...
    
    # AG-UI接口 - 用户交互
    def emit_ui_event(self, event: UIEvent): ...
    
    # MCP接口 - 工具使用
    def list_tools(self) -> List[Tool]: ...
    
    # PocketFlow接口 - 工作流执行
    def get_flow(self) -> Flow: ...
```

#### 2️⃣ **热插拔,可组合**
像乐高积木一样自由组合:
```yaml
# 产品配置 = Agent积木组合
product:
  name: "文档处理助手"
  agents:
    - id: pdf_analyzer
      source: "local://agents/pdf_analyzer"  # 本地Agent
    - id: translator
      source: "a2a://translation-service.com"  # 远程A2A Agent
    - id: email_sender
      source: "marketplace://email-pro"  # 市场Agent
  
  workflow:
    - when: user_upload_pdf
      call: pdf_analyzer
    - then: translator
      if: needs_translation
    - finally: email_sender
```

#### 3️⃣ **模板化,可复制**
参照PocketFlow的模板思想:
```
templates/
├── basic/              # 基础单Agent模板
├── rag/                # RAG模式模板  
├── mapreduce/          # MapReduce模式模板
├── agent-orchestrator/ # 多Agent编排模板
└── custom/            # 自定义模板
    └── pdf-to-report/  # 具体场景模板(可复用)
        ├── design.md
        ├── agent.yaml
        ├── nodes.py
        └── prompts/
```

#### 4️⃣ **可视化,零代码**
为未来的可视化平台预留接口:
```json
// agent.visual.json - 可视化元数据
{
  "ui": {
    "icon": "📄",
    "color": "#3B82F6",
    "category": "文档处理",
    "inputs": [
      {"name": "pdf_path", "type": "file", "accept": ".pdf"}
    ],
    "outputs": [
      {"name": "summary", "type": "text", "preview": true}
    ]
  },
  "a2a_card": {...},
  "ag_ui_events": [...]
}
```

---

## 1️⃣ 用户故事 (User Stories)

### US-001: 一键启动,三协议支持
**作为** 开发者  
**我想要** 创建的Agent自动支持MCP/A2A/AG-UI三个协议  
**以便** 无缝接入整个AI生态系统

**验收标准**:
```bash
agentflow init my-agent --protocols mcp,a2a,ag-ui
# 生成的Agent自动包含:
# - MCP工具定义(tools.mcp.json)
# - A2A AgentCard(agent.a2a.json)  
# - AG-UI事件映射(agent.agui.json)
```

### US-002: 积木市场,即搜即用
**作为** 开发者  
**我想要** 搜索并导入现成的Agent积木  
**以便** 快速组装产品原型

**验收标准**:
```bash
# 搜索Agent积木
agentflow search "PDF解析"
> 找到3个积木:
> 1. pdf-parser-basic (本地) - 基础PDF文本提取
> 2. pdf-analyzer-pro (A2A) - 高级PDF分析,支持表格
> 3. pdf-ocr-agent (A2A) - OCR识别手写PDF

# 安装并自动配置
agentflow install pdf-analyzer-pro
> ✅ 下载A2A AgentCard
> ✅ 配置认证信息
> ✅ 生成本地适配器
> ✅ 添加到项目依赖

# 在代码中直接使用
from agentflow import get_agent
pdf_agent = get_agent("pdf-analyzer-pro")
result = pdf_agent.run({"pdf_path": "test.pdf"})
```

### US-003: 可视化编排,零代码搭建
**作为** 产品经理  
**我想要** 通过拖拽界面设计Agent工作流  
**以便** 不需要写代码就能创建AI产品

**验收标准**:
- ✅ 打开Web编排器: `agentflow studio`
- ✅ 拖拽Agent积木到画布
- ✅ 连接Agent之间的数据流
- ✅ 预览AG-UI交互界面
- ✅ 一键生成可部署代码

### US-004: 模板复用,改提示词即可
**作为** 开发者  
**我想要** 使用成熟的场景模板  
**以便** 只需修改提示词和配置就能上线

**验收标准**:
```bash
# 从模板创建
agentflow create from-template invoice-processor
> 📋 使用模板: 发票处理系统
> 📝 请回答以下问题来定制你的Agent:
> 
> 1. 发票格式? [PDF/图片/Excel] PDF
> 2. 需要提取哪些字段? 发票号,金额,日期,供应商
> 3. 处理后动作? [保存数据库/发送邮件/生成报表] 保存数据库
>
> ✅ 生成完成!只需修改 prompts/extract.txt 中的提取规则
```

---

## 2️⃣ 框架需求 (Framework Requirements)

### 核心架构图

```mermaid
graph TB
    subgraph "用户界面层"
        WebUI[Web界面<br/>AG-UI协议]
        MobileUI[移动端<br/>AG-UI协议]
        VSCODE[VS Code插件<br/>AG-UI协议]
    end
    
    subgraph "AgentFlow核心"
        Studio[可视化编排器]
        Registry[Agent积木市场]
        CLI[命令行工具]
        
        subgraph "运行时引擎"
            A2AServer[A2A Server<br/>Agent协作]
            AGUIEmitter[AG-UI Emitter<br/>UI事件流]
            FlowEngine[PocketFlow Engine<br/>工作流执行]
            MCPClient[MCP Client<br/>工具调用]
        end
    end
    
    subgraph "Agent积木库"
        LocalAgent[本地Agent积木]
        RemoteAgent[远程A2A Agent]
        MarketAgent[市场Agent积木]
    end
    
    subgraph "工具与数据层"
        MCPTools[MCP工具服务器]
        Database[(数据库)]
        APIs[外部API]
    end
    
    WebUI --> AGUIEmitter
    MobileUI --> AGUIEmitter
    VSCODE --> AGUIEmitter
    
    Studio --> Registry
    CLI --> Registry
    Registry --> LocalAgent
    Registry --> RemoteAgent
    Registry --> MarketAgent
    
    AGUIEmitter --> FlowEngine
    A2AServer --> FlowEngine
    FlowEngine --> MCPClient
    
    RemoteAgent -.A2A协议.-> A2AServer
    FlowEngine --> LocalAgent
    MCPClient --> MCPTools
    MCPClient --> Database
    MCPClient --> APIs
```

### 技术栈选型

| 层级 | 技术选择 | 理由 |
|------|---------|------|
| **工作流引擎** | PocketFlow (100行) | 简单、易扩展、AI友好 |
| **A2A通信** | google-a2a SDK | 官方SDK,生态支持好 |
| **AG-UI实现** | AG-UI Python SDK | 支持SSE流式输出 |
| **MCP客户端** | mcp Python SDK | Anthropic官方 |
| **可视化** | React Flow + shadcn/ui | 拖拽式编排 |
| **CLI** | Click + Rich | 美观的命令行 |
| **配置管理** | YAML + Pydantic | 类型安全 |

### 项目结构 v2.0

```
agentflow-project/
├── .agentflow/                 # 框架配置目录
│   ├── config.yaml             # 项目全局配置
│   ├── protocols/              # 协议配置
│   │   ├── mcp.json            # MCP工具清单
│   │   ├── a2a.json            # A2A AgentCard
│   │   └── agui.json           # AG-UI事件定义
│   ├── prompts/                # AI助手提示词库
│   │   ├── design.prompt
│   │   ├── implement.prompt
│   │   └── optimize.prompt
│   └── registry.yaml           # 已安装Agent清单
│
├── agents/                     # Agent积木库
│   └── my_agent/               # 单个Agent积木
│       ├── agent.yaml  Yeah.         # Agent元数据(积木说明书)
│       ├── design.md           # 设计文档
│       ├── nodes.py            # PocketFlow Nodes
│       ├── flow.py             # PocketFlow Flow
│       ├── a2a_adapter.py      # A2A适配器
│       ├── agui_events.py      # AG-UI事件定义
│       └── visual.json         # 可视化元数据
│
├── tools/                      # MCP工具库
│   ├── mcp_server.py           # MCP服务器
│   ├── llm_tools.py
│   ├── file_tools.py
│   └── custom_tools.py
│
├── workflows/                  # 产品级工作流(积木组合)
│   └── document_processor/
│       ├── workflow.yaml       # 工作流定义
│       ├── orchestrator.py     # 编排逻辑
│       └── ui_config.yaml      # AG-UI界面配置
│
├── tests/                      # 测试
│   ├── test_agents.py
│   ├── test_tools.py
│   └── test_protocols.py       # 协议兼容性测试
│
├── main.py                     # 主入口
├── requirements.txt
└── README.md
```

### Agent积木规范 (agent.yaml)

```yaml
# agents/pdf_analyzer/agent.yaml
meta:
  id: pdf-analyzer-v1
  name: "PDF分析器"
  version: "1.0.0"
  author: "your-team"
  icon: "📄"
  category: "文档处理"
  description: "高级PDF分析,支持文本提取、表格识别、摘要生成"
  
# 积木接口定义
interfaces:
  # 输入接口(插座)
  inputs:
    - name: pdf_path
      type: file
      required: true
      accept: [".pdf"]
      description: "待分析的PDF文件路径"
    - name: analysis_type
      type: enum
      options: ["extract", "summarize", "qa"]
      default: "extract"
      
  # 输出接口(插头)
  outputs:
    - name: result
      type: object
      schema:
        text: string
        tables: array
        metadata: object
        
# 协议支持
protocols:
  mcp:
    tools: ["read_pdf", "extract_tables"]
    resources: ["pdf_content"]
    
  a2a:
    enabled: true
    card_path: "./a2a_card.json"
    skills: ["document_analysis", "table_extraction"]
    
  agui:
    enabled: true
    events:
      - type: "TEXT_MESSAGE_CONTENT"  # 流式输出分析进度
      - type: "TOOL_CALL_START"       # 显示工具调用
      - type: "STATE_DELTA"           # 更新分析状态
      
# 依赖关系
dependencies:
  agents: []  # 不依赖其他Agent
  tools:      # MCP工具依赖
    - mcp://file-tools/read_pdf
    - mcp://llm-tools/summarize
  packages:   # Python包依赖
    - PyPDF2>=3.0.0
    - pdfplumber>=0.9.0
    
# PocketFlow配置
pocketflow:
  entry: "flow.py:create_pdf_analysis_flow"
  shared_schema: "schemas.py:PDFAnalysisSchema"
  
# 可视化配置
visual:
  color: "#3B82F6"
  size: "medium"  # small/medium/large
  ports:
    input: ["left"]
    output: ["right"]
```

---

## 3️⃣ 详细机能设计

### 机能1: 三协议自动适配器

**设计目标**: Agent开发者只需关注业务逻辑,协议层自动生成

```python
# agentflow/adapters.py
from agentflow import AgentBlock, auto_adapt

@auto_adapt(protocols=["mcp", "a2a", "agui"])
class PDFAnalyzerAgent(AgentBlock):
    """只需实现核心业务逻辑,协议适配自动完成"""
    
    def __init__(self):
        self.metadata = self.load_metadata("agent.yaml")
        self.flow = self.create_flow()
    
    # 核心业务逻辑 - PocketFlow
    def create_flow(self) -> Flow:
        read_node = ReadPDFNode()
        analyze_node = AnalyzeNode()
        output_node = OutputNode()
        
        read_node >> analyze_node >> output_node
        return Flow(start=read_node)
    
    # 以下方法由@auto_adapt装饰器自动生成:
    
    # def get_mcp_tools(self) -> List[Tool]:
    #     """自动从agent.yaml的protocols.mcp生成"""
    #     ...
    
    # def get_a2a_card(self) -> AgentCard:
    #     """自动从agent.yaml的protocols.a2a生成"""
    #     ...
    
    # def emit_agui_events(self, flow_state):
    #     """自动将Flow执行状态转为AG-UI事件"""
    #     ...
```

**自动适配器实现逻辑**:

```python
# agentflow/adapters/auto_adapter.py
class ProtocolAdapter:
    @staticmethod
    def generate_mcp_tools(agent_meta: dict) -> List[Tool]:
        """从agent.yaml自动生成MCP工具定义"""
        tools = []
        for tool_uri in agent_meta["protocols"]["mcp"]["tools"]:
            # 解析 mcp://category/tool_name
            category, name = parse_mcp_uri(tool_uri)
            tool_def = load_tool_definition(category, name)
            tools.append(tool_def)
        return tools
    
    @staticmethod
    def generate_a2a_card(agent_meta: dict) -> AgentCard:
        """生成A2A AgentCard"""
        return AgentCard(
            name=agent_meta["meta"]["name"],
            description=agent_meta["meta"]["description"],
            version=agent_meta["meta"]["version"],
            skills=[
                Skill(
                    name=skill,
                    input_schema=agent_meta["interfaces"]["inputs"],
                    output_schema=agent_meta["interfaces"]["outputs"]
                )
                for skill in agent_meta["protocols"]["a2a"]["skills"]
            ],
            endpoint=f"http://localhost:8000/agents/{agent_meta['meta']['id']}"
        )
    
    @staticmethod
    def wrap_flow_with_agui(flow: Flow, agent_meta: dict):
        """将PocketFlow的执行包装为AG-UI事件流"""
        
        # 在Flow执行前
        def on_flow_start(shared):
            emit_agui_event({
                "type": "RUN_STARTED",
                "data": {"agent": agent_meta["meta"]["name"]}
            })
        
        # Node执行时
        def on_node_exec(node, input_data):
            emit_agui_event({
                "type": "TOOL_CALL_START",
                "data": {"tool": node.__class__.__name__, "input": input_data}
            })
        
        # 流式输出时
        def on_llm_stream(token):
            emit_agui_event({
                "type": "TEXT_MESSAGE_CONTENT",
                "data": {"delta": token}
            })
        
        # 状态变化时
        def on_state_change(old_state, new_state):
            emit_agui_event({
                "type": "STATE_DELTA",
                "data": {"patch": diff(old_state, new_state)}
            })
        
        # 注入钩子到Flow
        flow.register_hooks({
            "on_start": on_flow_start,
            "on_node_exec": on_node_exec,
            "on_llm_token": on_llm_stream,
            "on_state_change": on_state_change
        })
```

### 机能2: Agent积木市场

**本地市场** (`.agentflow/marketplace/`)
```
marketplace/
├── index.yaml              # 市场索引
├── agents/
│   ├── pdf-analyzer/
│   │   ├── agent.yaml
│   │   ├── package.tar.gz
│   │   └── README.md
│   └── email-sender/
└── templates/
    └── invoice-processor/  # 整套模板
```

**远程市场API**:
```python
# agentflow/marketplace.py
class AgentMarketplace:
    def search(self, query: str, filters: dict = None):
        """搜索Agent积木"""
        # 支持标签、分类、协议支持等过滤
        return [
            {
                "id": "pdf-analyzer-pro",
                "name": "PDF高级分析器",
                "author": "acme-corp",
                "version": "2.1.0",
                "protocols": ["mcp", "a2a", "agui"],
                "rating": 4.8,
                "downloads": 15234,
                "price": "free",  # or "premium"
                "source": "a2a://marketplace.agentflow.ai/pdf-analyzer-pro"
            }
        ]
    
    def install(self, agent_id: str):
        """安装Agent积木"""
        # 1. 下载Agent包或获取A2A endpoint
        # 2. 解析依赖关系
        # 3. 安装依赖的工具和其他Agent
        # 4. 生成本地适配器代码
        # 5. 更新registry.yaml
        pass
    
    def publish(self, agent_path: str):
        """发布自己的Agent到市场"""
        # 1. 验证agent.yaml完整性
        # 2. 运行协议兼容性测试
        # 3. 打包并上传
        # 4. 生成A2A endpoint(如果是远程Agent)
        pass
```

### 机能3: 可视化编排器 (AgentFlow Studio)

**启动方式**:
```bash
agentflow studio
> 🎨 AgentFlow Studio 已启动
> 🌐 访问: http://localhost:3000
```

**核心功能**:

1. **画布编排**
```
┌────────────────────────────────────────────────────┐
│  [File] [Edit] [View] [Agents]  [🔍Search Agents]  │
├────────────────────────────────────────────────────┤
│                                                    │
│   📤 User Upload                                   │
│        │                                           │
│        ↓                                           │
│   [📄 PDF Analyzer]  ←─ 从市场拖拽的Agent积木         │
│        │                                           │
│        ├─→ {text}                                  │
│        ├─→ {tables}                                │
│        │                                           │
│        ↓                                           │
│   [🌐 Translator]                                  │
│        │                                           │
│        ↓                                           │
│   [📧 Email Sender]                                │
│        │                                           │
│        ↓                                           │
│   ✅ Done                                          │
│                                                    │
├────────────────────────────────────────────────────┤
│  Properties:          Preview:                     │
│  Agent: PDF Analyzer  ┌─────────────────┐         │
│  ID: pdf-analyzer     │  [Test Run] 🔄  │         │
│  Inputs:              │                 │         │
│    • pdf_path: file   │  📄 分析中...   │         │
│  Outputs:             │  ▓▓▓▓░░░░ 50%   │         │
│    • text: string     │                 │         │
│    • tables: array    └─────────────────┘         │
└────────────────────────────────────────────────────┘
```

2. **实时预览**
   - 点击"Test Run"可以测试整个工作流
   - 通过AG-UI协议实时显示Agent状态
   - 支持断点调试

3. **一键生成代码**
```python
# studio自动生成的workflow代码
# workflows/my_workflow/orchestrator.py

from agentflow import get_agent, Workflow

class MyWorkflow(Workflow):
    def __init__(self):
        # 加载画布上的Agent积木
        self.pdf_analyzer = get_agent("pdf-analyzer")
        self.translator = get_agent("translator")
        self.email_sender = get_agent("email-sender")
    
    async def run(self, user_input):
        # 按画布连接顺序执行
        pdf_result = await self.pdf_analyzer.run({
            "pdf_path": user_input["file"]
        })
        
        translated = await self.translator.run({
            "text": pdf_result["text"],
            "target_lang": "en"
        })
        
        await self.email_sender.run({
            "to": user_input["email"],
            "content": translated["text"]
        })
        
        return {"status": "success"}
```

### 机能4: 模板系统增强

**场景模板库**:
```
templates/
├── scenarios/                  # 具体业务场景模板
│   ├── invoice-processor/      # 发票处理
│   │   ├── README.md
│   │   ├── template.yaml       # 模板元数据
│   │   ├── agents/             # 预配置的Agent
│   │   ├── prompts/            # 可定制的提示词
│   │   └── config.example.yaml # 配置示例
│   │
│   ├── customer-service-bot/   # 客服机器人
│   ├── data-analysis-pipeline/ # 数据分析管道
│   └── email-automation/       # 邮件自动化
│
└── patterns/                   # 设计模式模板
    ├── rag/
    ├── mapreduce/
    └── multi-agent-debate/
```

**模板元数据** (template.yaml):
```yaml
meta:
  id: invoice-processor-v1
  name: "发票处理系统"
  description: "自动处理PDF/图片发票,提取信息并保存到数据库"
  author: "agentflow-team"
  version: "1.0.0"
  difficulty: "beginner"  # beginner/intermediate/advanced
  
# 模板包含的积木
includes:
  agents:
    - pdf-extractor
    - invoice-parser
    - database-writer
  tools:
    - pdf-reader
    - ocr-tool
    - db-connector
    
# 可定制参数(向导式配置)
customization:
  questions:
    - id: invoice_format
      prompt: "发票格式?"
      type: select
      options: ["PDF", "图片", "Excel"]
      default: "PDF"
      
    - id: extract_fields
      prompt: "需要提取哪些字段?"
      type: multiselect
      options: ["发票号", "金额", "日期", "供应商", "税额"]
      default: ["发票号", "金额", "日期"]
      
    - id: output_action
      prompt: "处理后动作?"
      type: select
      options: ["保存数据库", "发送邮件", "生成报表"]
      default: "保存数据库"
      
  # 基于用户回答生成配置
  config_template: |
    invoice:
      format: {{ invoice_format }}
      fields: {{ extract_fields | join(', ') }}
    output:
      action: {{ output_action }}
      
# 需要修改的提示词文件
prompts_to_edit:
  - path: "prompts/extract.txt"
    description: "修改发票字段提取规则"
    example: |
      从发票中提取以下信息:
      - 发票号码: 通常在右上角
      - 金额: 查找"合计"或"总计"
      ...
      
# 部署配置
deployment:
  type: "standalone"  # standalone/cloud/edge
  dependencies:
    - PostgreSQL >= 12
  environment:
    - DATABASE_URL
    - OCR_API_KEY
```

**使用模板的命令**:
```bash
# 交互式创建
agentflow create from-template invoice-processor

# 输出:
📋 使用模板: 发票处理系统 v1.0.0
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📝 请回答以下问题来定制你的Agent:

❓ 发票格式? 
   1) PDF  2) 图片  3) Excel
👉 选择 [1]: 1

❓ 需要提取哪些字段? (多选,空格分隔)
   1) 发票号  2) 金额  3) 日期  4) 供应商  5) 税额
👉 选择 [1 2 3]: 1 2 3 4

❓ 处理后动作?
   1) 保存数据库  2) 发送邮件  3) 生成报表
👉 选择 [1]: 1

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
✅ 项目创建完成!

📁 项目结构:
   invoice-processor/
   ├── agents/
   │   ├── pdf-extractor/
   │   ├── invoice-parser/
   │   └── database-writer/
   ├── prompts/
   │   └── extract.txt  ⚠️  需要微调
   └── config.yaml

🎯 下一步:
   1. cd invoice-processor
   2. 编辑 prompts/extract.txt 调整提取规则
   3. 配置 .env 文件(DATABASE_URL等)
   4. agentflow run --test  # 测试运行
   5. agentflow deploy      # 部署上线

💡 AI助手已准备好协助你!在Cursor中打开项目,
   AI会自动理解这个模板并帮你快速定制。
```

---

## 4️⃣ 实现任务清单

### 阶段1: 核心框架 + 协议集成 (Week 1-3)

#### Task 1.1: PocketFlow核心包装
**优先级**: P0  
**工时**: 2天

```python
# agentflow/core/flow_engine.py
from pocketflow import Node, Flow as PocketFlow

class AgentFlowEngine:
    """PocketFlow包装器,增加协议钩子"""
    
    def __init__(self, flow: PocketFlow):
        self.flow = flow
        self.hooks = {
            "on_start": [],
            "on_node_exec": [],
            "on_node_complete": [],
            "on_state_change": [],
            "on_finish": []
        }
    
    def register_hook(self, event: str, callback):
        """注册事件钩子"""
        self.hooks[event].append(callback)
    
    def run(self, shared: dict):
        """执行Flow并触发钩子"""
        # 触发开始钩子
        for hook in self.hooks["on_start"]:
            hook(shared)
        
        # 执行PocketFlow (需要monkey patch Node)
        result = self.flow.run(shared)
        
        # 触发完成钩子
        for hook in self.hooks["on_finish"]:
            hook(shared, result)
        
        return result
```

#### Task 1.2: MCP客户端集成
**优先级**: P0  
**工时**: 3天

```python
# agentflow/protocols/mcp_client.py
from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client

class MCPToolManager:
    """MCP工具管理器"""
    
    def __init__(self, mcp_config_path: str):
        self.config = load_yaml(mcp_config_path)
        self.sessions = {}  # 每个MCP服务器一个session
    
    async def connect_all_servers(self):
        """连接所有MCP服务器"""
        for server in self.config["servers"]:
            session = await self._connect_server(server)
            self.sessions[server["name"]] = session
    
    async def _connect_server(self, server_config):
        """连接单个MCP服务器"""
        server_params = StdioServerParameters(
            command=server_config["command"],
            args=server_config.get("args", []),
            env=server_config.get("env", {})
        )
        
        async with stdio_client(server_params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                
                # 列出可用工具
                tools = await session.list_tools()
                print(f"✅ 连接MCP服务器: {server_config['name']}")
                print(f"   可用工具: {[t.name for t in tools]}")
                
                return session
    
    async def call_tool(self, tool_uri: str, arguments: dict):
        """调用MCP工具
        
        Args:
            tool_uri: 格式 mcp://server_name/tool_name
            arguments: 工具参数
        """
        server_name, tool_name = self._parse_uri(tool_uri)
        session = self.sessions[server_name]
        
        result = await session.call_tool(tool_name, arguments)
        return result
    
    def generate_tool_definitions(self):
        """生成工具定义供LLM使用"""
        tools = []
        for server_name, session in self.sessions.items():
            for tool in session.list_tools():
                tools.append({
                    "name": f"{server_name}/{tool.name}",
                    "description": tool.description,
                    "parameters": tool.inputSchema
                })
        return tools
```

**MCP配置示例** (.agentflow/protocols/mcp.yaml):
```yaml
servers:
  - name: file-tools
    command: npx
    args: ["-y", "@modelcontextprotocol/server-filesystem", "/path/to/data"]
    
  - name: database-tools
    command: npx
    args: ["-y", "@modelcontextprotocol/server-postgres"]
    env:
      DATABASE_URL: "postgresql://user:pass@localhost/db"
      
  - name: custom-tools
    command: python
    args: ["tools/mcp_server.py"]
```

#### Task 1.3: A2A Server/Client实现
**优先级**: P0  
**工时**: 4天

```python
# agentflow/protocols/a2a_server.py
from google_a2a import A2AServer, AgentCard, Skill

class AgentFlowA2AServer:
    """A2A服务器,暴露本地Agent给远程调用"""
    
    def __init__(self, port: int = 8000):
        self.port = port
        self.agents = {}  # 注册的Agent
        self.server = A2AServer()
    
    def register_agent(self, agent_block):
        """注册Agent到A2A服务器"""
        # 从agent.yaml生成AgentCard
        card = self._generate_card(agent_block)
        
        # 注册处理函数
        @self.server.task_handler(card.name)
        async def handle_task(task):
            # 调用Agent的PocketFlow执行
            result = await agent_block.run(task.input)
            return result
        
        self.agents[card.name] = agent_block
        print(f"✅ Agent已注册到A2A: {card.name}")
    
    def _generate_card(self, agent_block) -> AgentCard:
        """从agent.yaml生成AgentCard"""
        meta = agent_block.metadata
        
        return AgentCard(
            name=meta["meta"]["name"],
            description=meta["meta"]["description"],
            version=meta["meta"]["version"],
            endpoint=f"http://localhost:{self.port}/agents/{meta['meta']['id']}",
            skills=[
                Skill(
                    name=skill,
                    description=f"{meta['meta']['name']} - {skill}",
                    input_schema=meta["interfaces"]["inputs"],
                    output_schema=meta["interfaces"]["outputs"]
                )
                for skill in meta["protocols"]["a2a"]["skills"]
            ]
        )
    
    def start(self):
        """启动A2A服务器"""
        print(f"🚀 A2A Server启动: http://localhost:{self.port}")
        self.server.run(port=self.port)


# agentflow/protocols/a2a_client.py
class AgentFlowA2AClient:
    """A2A客户端,调用远程Agent"""
    
    def __init__(self):
        self.remote_agents = {}  # 缓存远程Agent的AgentCard
    
    async def discover_agent(self, endpoint: str):
        """发现远程Agent"""
        # 获取AgentCard
        card = await fetch_agent_card(endpoint)
        self.remote_agents[card.name] = {
            "card": card,
            "endpoint": endpoint
        }
        return card
    
    async def call_remote_agent(self, agent_name: str, input_data: dict):
        """调用远程Agent"""
        agent_info = self.remote_agents[agent_name]
        
        # 发送A2A任务
        result = await send_a2a_task(
            endpoint=agent_info["endpoint"],
            skill=agent_info["card"].skills[0].name,  # 简化:使用第一个skill
            input=input_data
        )
        
        return result
```

#### Task 1.4: AG-UI事件流实现
**优先级**: P1  
**工时**: 3天

```python
# agentflow/protocols/agui_emitter.py
from ag_ui import AGUIServer, Event

class AGUIEventEmitter:
    """AG-UI事件发射器"""
    
    def __init__(self):
        self.server = AGUIServer()
        self.current_run_id = None
    
    def attach_to_flow(self, flow_engine: AgentFlowEngine):
        """将AG-UI事件绑定到Flow执行"""
        
        # 注册钩子
        flow_engine.register_hook("on_start", self._on_flow_start)
        flow_engine.register_hook("on_node_exec", self._on_node_exec)
        flow_engine.register_hook("on_state_change", self._on_state_change)
        flow_engine.register_hook("on_finish", self._on_flow_finish)
    
    def _on_flow_start(self, shared):
        """Flow开始"""
        self.current_run_id = generate_id()
        self.emit({
            "type": "RUN_STARTED",
            "run_id": self.current_run_id,
            "timestamp": now()
        })
    
    def _on_node_exec(self, node, input_data):
        """Node执行"""
        self.emit({
            "type": "TOOL_CALL_START",
            "run_id": self.current_run_id,
            "data": {
                "tool_name": node.__class__.__name__,
                "arguments": input_data
            }
        })
    
    def _on_state_change(self, old_state, new_state):
        """状态变化"""
        delta = diff_dict(old_state, new_state)
        self.emit({
            "type": "STATE_DELTA",
            "run_id": self.current_run_id,
            "data": {"patch": delta}
        })
    
    def _on_flow_finish(self, shared, result):
        """Flow完成"""
        self.emit({
            "type": "RUN_FINISHED",
            "run_id": self.current_run_id,
            "data": {"result": result}
        })
    
    def emit(self, event: dict):
        """发送AG-UI事件(SSE)"""
        self.server.send_event(Event(**event))
    
    def stream_llm_tokens(self, token_generator):
        """流式输出LLM token"""
        for token in token_generator:
            self.emit({
                "type": "TEXT_MESSAGE_CONTENT",
                "run_id": self.current_run_id,
                "data": {"delta": token}
            })
```

### 阶段2: CLI工具 + 市场 (Week 4-5)

#### Task 2.1: CLI命令实现
```bash
agentflow --help

Commands:
  init              创建新项目
  create            创建Agent/Tool/Workflow
  install           从市场安装Agent
  search            搜索Agent市场
  list              列出本地资源
  run               运行Agent/Workflow
  test              测试Agent
  deploy            部署到生产环境
  studio            启动可视化编排器
  protocols         协议管理(test/validate)
```

#### Task 2.2: Agent市场API
```python
# 搜索
agentflow search "PDF处理" --filter protocols=a2a,mcp

# 安装
agentflow install pdf-analyzer-pro
> 📦 正在安装: pdf-analyzer-pro@2.1.0
> ⬇️  下载AgentCard...
> 🔍 解析依赖: PyPDF2, pdfplumber
> ✅ 安装完成!
> 
> 使用方法:
>   from agentflow import get_agent
>   agent = get_agent("pdf-analyzer-pro")

# 发布
agentflow publish ./agents/my-agent --price free
```

### 阶段3: 可视化Studio (Week 6-8)

#### Task 3.1: React Flow画布
- 拖拽Agent积木
- 连接数据流
- 属性编辑器
- 实时预览

#### Task 3.2: AG-UI前端集成
```typescript
// studio前端通过AG-UI协议连接后端
import { useAGUI } from '@ag-ui/react'

function AgentPreview({ agentId }) {
  const { events, sendMessage } = useAGUI(`http://localhost:8000/agents/${agentId}`)
  
  return (
    <div>
      {events.map(event => {
        switch(event.type) {
          case 'TEXT_MESSAGE_CONTENT':
            return <StreamingText delta={event.data.delta} />
          case 'TOOL_CALL_START':
            return <ToolIndicator tool={event.data.tool_name} />
          case 'STATE_DELTA':
            return <StateViewer patch={event.data.patch} />
        }
      })}
    </div>
  )
}
```

### 阶段4: 模板和文档 (Week 9-10)

#### Task 4.1: 创建10个场景模板
1. 发票处理系统 ✓
2. 客服聊天机器人
3. 数据分析管道
4. 邮件自动化
5. 文档翻译服务
6. 代码审查助手
7. 会议纪要生成
8. 社交媒体内容创作
9. 简历筛选系统
10. 智能问答系统

#### Task 4.2: 完整文档
- 快速开始指南
- 协议集成教程
- Agent开发指南
- 积木市场使用
- 可视化编排教程
- 最佳实践
- API参考

---

## 5️⃣ Cursor/Windsurf AI助手提示词

### 提示词模板库

#### 1. 项目初始化提示词
```
我要使用AgentFlow框架创建一个新的AI Agent项目。

项目需求:
- 项目名称: [项目名]
- 主要功能: [描述功能]
- 协议支持: MCP, A2A, AG-UI 全部启用
- 设计模式: [RAG/MapReduce/Agent/Workflow]

请帮我:
1. 运行 agentflow init 命令创建项目结构
2. 生成 agent.yaml 配置文件
3. 创建符合三协议标准的适配器代码
4. 生成 design.md 设计文档

AgentFlow框架规范:
- 项目结构参考 AgentFlow v2.0 文档
- agent.yaml必须包含 protocols 配置
- 自动生成MCP/A2A/AG-UI适配器

请逐步执行,等待我确认每一步。
```

#### 2. Agent开发提示词
```
基于AgentFlow框架,开发一个[Agent名称]。

功能需求:
{详细描述功能}

技术约束:
- 必须支持MCP/A2A/AG-UI三协议
- 使用PocketFlow作为工作流引擎
- 输入:[描述输入]
- 输出:[描述输出]

开发步骤:
1. 先完成 design.md 设计文档
   - 选择合适的设计模式
   - 设计Node和Flow结构
   - 定义Shared Store Schema
   - 识别需要的MCP工具

2. 实现 agent.yaml 元数据
   - 定义inputs/outputs接口
   - 配置protocols支持
   - 声明dependencies

3. 实现PocketFlow代码
   - nodes.py: 实现所有Node
   - flow.py: 连接Flow
   - schemas.py: Pydantic数据模型

4. 协议适配
   - 使用 @auto_adapt 装饰器
   - 确保MCP工具正确注册
   - 生成A2A AgentCard
   - 配置AG-UI事件映射

5. 测试
   - 单元测试
   - 协议兼容性测试
   - 端到端测试

请从设计文档开始,每完成一步暂停等我确认。
```

#### 3. 积木组合提示词
```
我要组合多个Agent积木创建一个产品级工作流。

产品名称: [产品名]
业务场景: [描述场景]

需要的Agent积木:
1. [Agent1] - 功能: [描述]
2. [Agent2] - 功能: [描述]  
3. [Agent3] - 功能: [描述]

数据流:
User Input → Agent1 → Agent2 → Agent3 → Output

请帮我:
1. 检查这些Agent是否在市场存在
   agentflow search [关键词]
   
2. 安装需要的Agent
   agentflow install [agent-id]
   
3. 创建工作流编排代码 workflows/[product-name]/orchestrator.py
   - 使用A2A协议连接远程Agent
   - 处理Agent间数据传递
   - 实现错误处理和重试
   
4. 配置AG-UI界面 workflows/[product-name]/ui_config.yaml
   - 设计用户输入表单
   - 配置实时状态显示
   
5. 编写测试用例

AgentFlow规范:
- 工作流使用 AgentFlow.Workflow 基类
- 优先使用A2A协议调用远程Agent
- 数据传递使用标准化的Schema

开始实现,遇到问题及时询问我。
```

#### 4. 模板定制提示词
```
使用AgentFlow模板快速创建项目。

模板名称: invoice-processor
定制需求:
- 发票格式: PDF
- 提取字段: [字段列表]
- 输出方式: 保存到PostgreSQL

步骤:
1. agentflow create from-template invoice-processor
   
2. 根据定制需求修改配置:
   - config.yaml: 更新数据库连接
   - prompts/extract.txt: 调整字段提取规则
   
3. 修改agent.yaml适应我的需求:
   - 更新inputs/outputs定义
   - 调整dependencies
   
4. 测试运行:
   agentflow run --test
   
5. 如有问题,使用AG-UI调试界面查看实时日志

请帮我执行这些步骤,并解释每个配置的作用。
```

---

## 6️⃣ 总结与路线图

### 核心价值

```
┌─────────────────────────────────────────────────────────┐
│         AgentFlow = 简单 + 开放 + 积木化                  │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  🎯 PocketFlow内核      → 100行,AI友好                  │
│  🌐 三协议支持          → MCP/A2A/AG-UI全覆盖            │
│  🧩 积木化设计          → 搭积木式开发                   │
│  📦 模板系统            → 改提示词即可用                  │
│  🎨 可视化编排          → 零代码搭建                     │
│  🤖 AI助手友好          → Cursor/Windsurf原生支持        │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

### 与其他框架对比

| 特性 | AgentFlow | LangChain | CrewAI | AutoGen |
|------|-----------|-----------|--------|---------|
| **核心代码行数** | ~500行 | ~50K行 | ~10K行 | ~20K行 |
| **MCP支持** | ✅ 原生 | ⚠️ 需插件 | ❌ | ❌ |
| **A2A支持** | ✅ 原生 | ❌ | ⚠️ 实验性 | ❌ |
| **AG-UI支持** | ✅ 原生 | ❌ | ✅ | ❌ |
| **积木化设计** | ✅ | ⚠️ 复杂 | ⚠️ 部分 | ❌ |
| **可视化编排** | ✅ | ❌ | ❌ | ❌ |
| **模板系统** | ✅ | ⚠️ 简单 | ⚠️ 简单 | ❌ |
| **AI助手友好** | ✅ 极佳 | ⚠️ 一般 | ✅ | ⚠️ |

### 开发路线图

**Q1 2026: MVP版本**
- ✅ PocketFlow集成
- ✅ MCP/A2A/AG-UI基础支持
- ✅ CLI工具
- ✅ 3个场景模板
- ✅ 基础文档

**Q2 2026: 市场版本**
- 🔄 Agent积木市场
- 🔄 可视化Studio (Beta)
- 🔄 10个场景模板
- 🔄 社区贡献机制

**Q3 2026: 企业版本**
- 📅 企业级安全认证
- 📅 多租户支持
- 📅 性能监控
- 📅 SaaS云服务

**Q4 2026: 生态版本**
- 📅 第三方集成市场
- 📅 AI助手深度集成
- 📅 移动端支持
- 📅 国际化

---

## 附录: 快速参考

### 常用命令速查

```bash
# 项目管理
agentflow init <name>                 # 创建项目
agentflow create agent <name>         # 创建Agent
agentflow create from-template <tpl>  # 从模板创建

# 市场操作
agentflow search <query>              # 搜索Agent
agentflow install <agent-id>          # 安装Agent
agentflow publish <path>              # 发布Agent

# 运行和测试
agentflow run <agent-name>            # 运行Agent
agentflow test                        # 测试
agentflow studio                      # 启动可视化

# 协议管理
agentflow protocols test              # 测试协议兼容性
agentflow protocols validate          # 验证配置
```

### agent.yaml 快速模板

```yaml
meta: {id, name, version, author, icon, category, description}
interfaces:
  inputs: [{name, type, required, description}]
  outputs: [{name, type, schema}]
protocols:
  mcp: {tools, resources}
  a2a: {enabled, skills}
  agui: {enabled, events}
dependencies: {agents, tools, packages}
pocketflow: {entry, shared_schema}
visual: {color, size, ports}
```

### 协议文档链接

- **MCP**: https://modelcontextprotocol.io
- **A2A**: https://github.com/google-a2a/A2A
- **AG-UI**: https://docs.ag-ui.com

---

**AgentFlow: 让AI Agent开发像搭积木一样简单!** 🚀
