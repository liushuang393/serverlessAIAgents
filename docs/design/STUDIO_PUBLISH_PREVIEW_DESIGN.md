# Studio Preview & 发布功能设计

> **目的**: 支持在 Studio 中预览工作流执行结果，并一键发布到各种平台

---

## 📋 目录

1. [概述](#概述)
2. [Preview 功能设计](#preview-功能设计)
3. [发布功能设计](#发布功能设计)
4. [代码生成器设计](#代码生成器设计)
5. [API 设计](#api-设计)
6. [UI 设计](#ui-设计)
7. [实现计划](#实现计划)

---

## 概述

### 目标

1. **Preview**: 在 Studio 中实时运行和调试工作流
2. **发布**: 将工作流导出为可部署的代码/服务
3. **多平台支持**: Vercel、AWS Lambda、Docker、REST API 等

### 架构图

```
┌─────────────────────────────────────────────────────────────────────┐
│                         AgentFlow Studio                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌────────────────────────────────────────────────────────────────┐ │
│  │  Canvas (Workflow Editor)                                       │ │
│  │  ┌─────────┐    ┌─────────┐    ┌─────────┐                     │ │
│  │  │ Agent A │ ─→ │ Agent B │ ─→ │ Agent C │                     │ │
│  │  └─────────┘    └─────────┘    └─────────┘                     │ │
│  └────────────────────────────────────────────────────────────────┘ │
│                              ↓                                       │
│  ┌──────────────────┐  ┌────────────────────────────────────────┐  │
│  │  Preview Panel   │  │  Publish Panel                          │  │
│  │  ---------------  │  │  ----------------                       │  │
│  │  [▶ Run]         │  │  Target: [Vercel ▼]                     │  │
│  │  Input: {...}    │  │  Name: my-workflow                       │  │
│  │  Output: {...}   │  │  [📦 Export Code] [🚀 Deploy]           │  │
│  │  Logs: [...]     │  │                                          │  │
│  └──────────────────┘  └────────────────────────────────────────┘  │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────────┐
│                      Code Generator                                  │
├─────────────────────────────────────────────────────────────────────┤
│  Workflow JSON → Python Code / Dockerfile / Serverless Config       │
└─────────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────────┐
│                    Deployment Manager                                │
├─────────────────────────────────────────────────────────────────────┤
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐           │
│  │  Vercel  │  │  AWS     │  │  Docker  │  │  K8s     │           │
│  │          │  │  Lambda  │  │  Hub     │  │          │           │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘           │
└─────────────────────────────────────────────────────────────────────┘
```

---

## Preview 功能设计

### 功能列表

| 功能 | 说明 | 优先级 |
|------|------|--------|
| **Run Workflow** | 在编辑器中执行工作流 | P0 |
| **Input Editor** | JSON 输入编辑器 | P0 |
| **Output Viewer** | 结果展示（支持 JSON/Markdown） | P0 |
| **Debug Mode** | 分步执行，查看中间结果 | P1 |
| **Log Viewer** | 实时日志流 | P1 |
| **Breakpoints** | 断点调试 | P2 |

### 数据流

```
User Input (JSON)
       ↓
┌──────────────────┐
│  Preview API     │  POST /api/preview/run
└──────────────────┘
       ↓
┌──────────────────┐
│  Workflow Runner │  agentflow.core.runner
└──────────────────┘
       ↓ (WebSocket/SSE)
┌──────────────────┐
│  Real-time       │  - Progress updates
│  Updates         │  - Intermediate results
└──────────────────┘  - Logs
       ↓
Final Output (JSON)
```

### API 端点

```yaml
# Preview API
POST /api/preview/run:
  description: 运行工作流预览
  request:
    workflow: Workflow  # 工作流定义
    input: object       # 输入数据
    debug: boolean      # 是否启用调试模式
  response:
    stream: true        # SSE 流式响应
    events:
      - type: progress
        data: { node_id, status, progress }
      - type: log
        data: { level, message, timestamp }
      - type: result
        data: { node_id, output }
      - type: complete
        data: { final_output, duration }
      - type: error
        data: { node_id, error }
```

---

## 发布功能设计

### 支持的发布目标

| 目标 | 说明 | 生成物 |
|------|------|--------|
| **Vercel** | Serverless Functions | `api/`, `vercel.json` |
| **AWS Lambda** | Lambda Functions | `handler.py`, `serverless.yml` |
| **Docker** | 容器化部署 | `Dockerfile`, `docker-compose.yml` |
| **FastAPI** | REST API 服务 | `app.py`, `requirements.txt` |
| **CLI** | 命令行工具 | `cli.py`, `pyproject.toml` |
| **Python Package** | 可导入模块 | `__init__.py`, `setup.py` |

### 发布流程

```
1. Export Code (代码导出)
   ┌─────────────────────────────────────────┐
   │  Workflow JSON                          │
   │  ↓                                      │
   │  Code Generator                         │
   │  ↓                                      │
   │  Generated Code (Python/Config files)   │
   │  ↓                                      │
   │  Download as ZIP                        │
   └─────────────────────────────────────────┘

2. Direct Deploy (直接部署)
   ┌─────────────────────────────────────────┐
   │  Workflow JSON                          │
   │  ↓                                      │
   │  Code Generator                         │
   │  ↓                                      │
   │  Deployment Manager                     │
   │  ↓                                      │
   │  Platform API (Vercel/AWS/etc)          │
   │  ↓                                      │
   │  Deployed URL                           │
   └─────────────────────────────────────────┘
```

---

## 代码生成器设计

### 输入格式（Workflow JSON）

```json
{
  "id": "my-workflow",
  "name": "我的工作流",
  "description": "示例工作流",
  "nodes": [
    {
      "id": "agent-1",
      "type": "agent",
      "data": {
        "agentType": "QAAgent",
        "config": {
          "system_prompt": "你是一个助手",
          "model": "gpt-4"
        }
      },
      "position": { "x": 100, "y": 100 }
    },
    {
      "id": "agent-2",
      "type": "agent",
      "data": {
        "agentType": "SummaryAgent",
        "config": {}
      },
      "position": { "x": 300, "y": 100 }
    }
  ],
  "edges": [
    {
      "id": "edge-1",
      "source": "agent-1",
      "target": "agent-2"
    }
  ]
}
```

### 输出示例

#### Vercel Function

```python
# api/workflow.py
from agentflow import create_flow

flow = create_flow("my-workflow") \
    .then(QAAgent, config={"system_prompt": "你是一个助手", "model": "gpt-4"}) \
    .then(SummaryAgent) \
    .build()

async def handler(request):
    """Vercel Serverless Function Handler"""
    data = await request.json()
    result = await flow.run(data)
    return Response(json.dumps(result), content_type="application/json")
```

#### Dockerfile

```dockerfile
FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 8000

CMD ["uvicorn", "app:app", "--host", "0.0.0.0", "--port", "8000"]
```

#### FastAPI App

```python
# app.py
from fastapi import FastAPI
from pydantic import BaseModel
from agentflow import create_flow

app = FastAPI(title="My Workflow API")

# 生成的工作流
flow = create_flow("my-workflow") \
    .then(QAAgent, config={"system_prompt": "你是一个助手"}) \
    .then(SummaryAgent) \
    .build()

class WorkflowInput(BaseModel):
    question: str

class WorkflowOutput(BaseModel):
    answer: str
    summary: str

@app.post("/run", response_model=WorkflowOutput)
async def run_workflow(input_data: WorkflowInput):
    """运行工作流"""
    result = await flow.run(input_data.model_dump())
    return result

@app.get("/health")
async def health():
    return {"status": "healthy"}
```

---

## API 设计

### 后端 API

```yaml
# /api/publish
POST /api/publish/export:
  description: 导出工作流代码
  request:
    workflow_id: string
    target: enum[vercel, lambda, docker, fastapi, cli, package]
    options:
      name: string
      version: string
      include_tests: boolean
  response:
    type: file
    content_type: application/zip
    filename: {workflow_name}-{target}.zip

POST /api/publish/deploy:
  description: 部署工作流
  request:
    workflow_id: string
    target: enum[vercel, lambda, docker]
    credentials:
      token: string  # 平台 API Token
      project_id: string
  response:
    deployment_id: string
    url: string
    status: enum[deploying, deployed, failed]
    logs: string[]

GET /api/publish/status/{deployment_id}:
  description: 获取部署状态
  response:
    status: enum[deploying, deployed, failed]
    url: string
    logs: string[]
```

### WebSocket API

```yaml
# 实时预览
WS /ws/preview/{session_id}:
  # 客户端 → 服务器
  client_messages:
    - type: run
      data: { workflow, input }
    - type: stop
    - type: step  # 单步执行
  
  # 服务器 → 客户端
  server_messages:
    - type: progress
      data: { node_id, status, percentage }
    - type: log
      data: { level, message }
    - type: node_result
      data: { node_id, output }
    - type: complete
      data: { output, duration }
    - type: error
      data: { node_id, error }
```

---

## UI 设计

### Preview Panel 组件

```tsx
// components/PreviewPanel.tsx
interface PreviewPanelProps {
  workflow: Workflow;
  onRun: (input: object) => void;
}

const PreviewPanel: React.FC<PreviewPanelProps> = ({ workflow, onRun }) => {
  const [input, setInput] = useState({});
  const [output, setOutput] = useState(null);
  const [logs, setLogs] = useState([]);
  const [status, setStatus] = useState<'idle' | 'running' | 'complete' | 'error'>('idle');
  
  return (
    <div className="preview-panel">
      <div className="preview-header">
        <h3>Preview</h3>
        <Button onClick={() => onRun(input)} disabled={status === 'running'}>
          {status === 'running' ? <Spinner /> : <PlayIcon />}
          Run
        </Button>
      </div>
      
      <Tabs>
        <Tab title="Input">
          <JsonEditor value={input} onChange={setInput} />
        </Tab>
        <Tab title="Output">
          <JsonViewer data={output} />
        </Tab>
        <Tab title="Logs">
          <LogViewer logs={logs} />
        </Tab>
      </Tabs>
    </div>
  );
};
```

### Publish Dialog 组件

```tsx
// components/PublishDialog.tsx
interface PublishDialogProps {
  workflow: Workflow;
  open: boolean;
  onClose: () => void;
}

const PublishDialog: React.FC<PublishDialogProps> = ({ workflow, open, onClose }) => {
  const [target, setTarget] = useState<PublishTarget>('vercel');
  const [options, setOptions] = useState({});
  
  const handleExport = async () => {
    const blob = await api.exportWorkflow(workflow.id, target, options);
    downloadBlob(blob, `${workflow.name}-${target}.zip`);
  };
  
  const handleDeploy = async () => {
    const result = await api.deployWorkflow(workflow.id, target, options);
    // Show deployment status
  };
  
  return (
    <Dialog open={open} onClose={onClose}>
      <DialogTitle>发布工作流</DialogTitle>
      <DialogContent>
        <Select value={target} onChange={setTarget}>
          <MenuItem value="vercel">Vercel (Serverless)</MenuItem>
          <MenuItem value="lambda">AWS Lambda</MenuItem>
          <MenuItem value="docker">Docker Container</MenuItem>
          <MenuItem value="fastapi">FastAPI Service</MenuItem>
          <MenuItem value="cli">CLI Tool</MenuItem>
        </Select>
        
        <TextField label="项目名称" />
        <TextField label="版本" defaultValue="1.0.0" />
        
        <FormGroup>
          <FormControlLabel control={<Checkbox />} label="包含测试代码" />
          <FormControlLabel control={<Checkbox />} label="生成 README" />
        </FormGroup>
      </DialogContent>
      <DialogActions>
        <Button onClick={handleExport}>📦 导出代码</Button>
        <Button onClick={handleDeploy} variant="contained">🚀 直接部署</Button>
      </DialogActions>
    </Dialog>
  );
};
```

---

## 实现计划

### Phase 1: Preview 功能 (2周)

| 任务 | 说明 | 时间 |
|------|------|------|
| Preview API | 后端执行 API | 3天 |
| WebSocket 支持 | 实时日志流 | 2天 |
| Preview Panel UI | 前端组件 | 3天 |
| Input/Output Editor | JSON 编辑器 | 2天 |
| 测试 | 单元 + E2E | 2天 |

### Phase 2: 代码生成器 (2周)

| 任务 | 说明 | 时间 |
|------|------|------|
| 生成器框架 | 模板引擎 | 2天 |
| FastAPI 生成 | REST API 模板 | 2天 |
| Docker 生成 | Dockerfile 模板 | 2天 |
| Vercel 生成 | Serverless 模板 | 2天 |
| Lambda 生成 | AWS 模板 | 2天 |
| 测试 | 生成代码验证 | 2天 |

### Phase 3: 发布功能 (2周)

| 任务 | 说明 | 时间 |
|------|------|------|
| Publish API | 导出/部署接口 | 2天 |
| Vercel 集成 | API 调用 | 2天 |
| Docker Hub 集成 | 镜像推送 | 2天 |
| Publish Dialog UI | 前端组件 | 3天 |
| 测试 | 部署流程测试 | 3天 |

---

## 技术选型

| 组件 | 技术 | 说明 |
|------|------|------|
| **代码模板** | Jinja2 | Python 模板引擎 |
| **ZIP 打包** | zipfile | Python 标准库 |
| **WebSocket** | FastAPI WebSocket | 实时通信 |
| **JSON Editor** | Monaco Editor | VS Code 同款 |
| **部署 API** | httpx | 异步 HTTP 客户端 |

---

## 安全考虑

1. **凭证管理**: 平台 Token 不存储，仅在部署时使用
2. **代码审计**: 生成的代码需经过安全检查
3. **沙箱执行**: Preview 在隔离环境中运行
4. **Rate Limiting**: 部署 API 限流防止滥用

---

## 相关文档

- [Studio UI 操作ガイド](../guide-studio-ui.md)
- [API リファレンス](../api.md)
- [内蔵 Skills ガイド](../guide-builtin-skills.md)
