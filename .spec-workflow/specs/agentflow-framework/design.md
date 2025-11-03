# Design Document: AgentFlow Framework

## 1. Architecture Overview

### 1.1 System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    User Interface Layer                      │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐   │
│  │ Web UI   │  │ Mobile   │  │ VS Code  │  │   CLI    │   │
│  │ (AG-UI)  │  │ (AG-UI)  │  │ (AG-UI)  │  │          │   │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘   │
└───────┼─────────────┼─────────────┼─────────────┼──────────┘
        │             │             │             │
        └─────────────┴─────────────┴─────────────┘
                          │
        ┌─────────────────┴─────────────────┐
        │      AgentFlow Core Runtime       │
        │  ┌─────────────────────────────┐  │
        │  │   Protocol Adapter Layer    │  │
        │  │  ┌────┐  ┌────┐  ┌──────┐  │  │
        │  │  │MCP │  │A2A │  │AG-UI │  │  │
        │  │  └────┘  └────┘  └──────┘  │  │
        │  └─────────────────────────────┘  │
        │  ┌─────────────────────────────┐  │
        │  │   AgentFlow Engine          │  │
        │  │  (PocketFlow + Hooks)       │  │
        │  └─────────────────────────────┘  │
        │  ┌─────────────────────────────┐  │
        │  │   Agent Block Manager       │  │
        │  │  • Registry                 │  │
        │  │  • Loader                   │  │
        │  │  • Validator                │  │
        │  └─────────────────────────────┘  │
        └─────────────────┬─────────────────┘
                          │
        ┌─────────────────┴─────────────────┐
        │                                   │
   ┌────┴────┐                      ┌──────┴──────┐
   │ Local   │                      │   Remote    │
   │ Agents  │                      │   Agents    │
   │         │                      │   (A2A)     │
   └────┬────┘                      └──────┬──────┘
        │                                  │
   ┌────┴────────────────────────────┬────┘
   │                                 │
┌──┴──────┐                    ┌────┴─────┐
│   MCP   │                    │ External │
│ Servers │                    │   APIs   │
└─────────┘                    └──────────┘
```

### 1.2 Core Components

| Component | Responsibility | Technology |
|-----------|---------------|------------|
| **AgentFlow Engine** | Execute PocketFlow graphs with protocol hooks | Python, PocketFlow |
| **Protocol Adapters** | MCP/A2A/AG-UI integration | Official SDKs |
| **Agent Block Manager** | Load, validate, and manage agent blocks | Python, Pydantic |
| **CLI Tool** | Command-line interface | Click, Rich |
| **Marketplace** | Agent discovery and distribution | YAML index, HTTP |
| **Visual Studio** | Web-based workflow editor | React, React Flow |

---

## 2. Detailed Component Design

### 2.1 AgentFlow Engine

**Purpose**: Wrap PocketFlow with protocol event hooks

**Class Diagram**:
```python
class AgentFlowEngine:
    """
    Core execution engine that wraps PocketFlow and provides
    lifecycle hooks for protocol integration.
    """
    
    # Attributes
    flow: PocketFlow                    # The underlying PocketFlow graph
    hooks: Dict[str, List[Callable]]    # Event hooks registry
    shared_store: SharedStore           # Shared state across nodes
    
    # Methods
    def __init__(flow: PocketFlow)
    def register_hook(event: str, callback: Callable) -> None
    def run(shared: dict) -> Any
    def _trigger_hooks(event: str, *args) -> None
    def _wrap_node_execution(node: Node) -> Node
```

**Hook Events**:
- `on_start`: Triggered before flow execution
- `on_node_exec`: Triggered before each node execution
- `on_node_complete`: Triggered after each node execution
- `on_state_change`: Triggered when shared state changes
- `on_llm_token`: Triggered for each LLM token (streaming)
- `on_finish`: Triggered after flow completion
- `on_error`: Triggered on execution errors

**Implementation Strategy**:
```python
# Monkey-patch PocketFlow Node to inject hooks
def _wrap_node_execution(self, node: Node) -> Node:
    original_run = node.run
    
    def wrapped_run(shared):
        # Pre-execution hook
        self._trigger_hooks("on_node_exec", node, shared)
        
        # Execute original node
        result = original_run(shared)
        
        # Post-execution hook
        self._trigger_hooks("on_node_complete", node, shared, result)
        
        return result
    
    node.run = wrapped_run
    return node
```

---

### 2.2 Protocol Adapter Layer

#### 2.2.1 MCP Client Adapter

**Purpose**: Connect to MCP servers and expose tools to agents

**Class Diagram**:
```python
class MCPToolManager:
    """Manages connections to multiple MCP servers"""
    
    # Attributes
    config: Dict[str, Any]              # MCP server configurations
    sessions: Dict[str, ClientSession]  # Active MCP sessions
    tool_registry: Dict[str, Tool]      # Available tools
    
    # Methods
    async def connect_all_servers() -> None
    async def connect_server(server_config: dict) -> ClientSession
    async def call_tool(tool_uri: str, arguments: dict) -> Any
    def get_tool_definitions() -> List[ToolDefinition]
    def parse_tool_uri(uri: str) -> Tuple[str, str]
```

**Configuration Format** (`.agentflow/protocols/mcp.yaml`):
```yaml
servers:
  - name: file-tools
    command: npx
    args: ["-y", "@modelcontextprotocol/server-filesystem", "/data"]
    
  - name: database-tools
    command: npx
    args: ["-y", "@modelcontextprotocol/server-postgres"]
    env:
      DATABASE_URL: "postgresql://localhost/db"
```

**Tool URI Format**: `mcp://server-name/tool-name`

---

#### 2.2.2 A2A Server/Client Adapter

**Purpose**: Expose local agents and call remote agents via A2A protocol

**Class Diagram**:
```python
class AgentFlowA2AServer:
    """Exposes local agents as A2A endpoints"""
    
    # Attributes
    port: int
    agents: Dict[str, AgentBlock]
    server: A2AServer
    
    # Methods
    def register_agent(agent_block: AgentBlock) -> None
    def generate_agent_card(agent_meta: dict) -> AgentCard
    def start() -> None

class AgentFlowA2AClient:
    """Calls remote A2A agents"""
    
    # Attributes
    remote_agents: Dict[str, AgentInfo]
    
    # Methods
    async def discover_agent(endpoint: str) -> AgentCard
    async def call_remote_agent(agent_name: str, input_data: dict) -> Any
```

**AgentCard Generation**:
```python
def generate_agent_card(agent_meta: dict) -> AgentCard:
    return AgentCard(
        name=agent_meta["meta"]["name"],
        description=agent_meta["meta"]["description"],
        version=agent_meta["meta"]["version"],
        endpoint=f"http://localhost:{port}/agents/{agent_meta['meta']['id']}",
        skills=[
            Skill(
                name=skill,
                input_schema=agent_meta["interfaces"]["inputs"],
                output_schema=agent_meta["interfaces"]["outputs"]
            )
            for skill in agent_meta["protocols"]["a2a"]["skills"]
        ]
    )
```

---

#### 2.2.3 AG-UI Event Emitter

**Purpose**: Convert flow execution events to AG-UI SSE streams

**Class Diagram**:
```python
class AGUIEventEmitter:
    """Emits AG-UI events during flow execution"""
    
    # Attributes
    server: AGUIServer
    current_run_id: str
    
    # Methods
    def attach_to_flow(flow_engine: AgentFlowEngine) -> None
    def emit(event: dict) -> None
    def stream_llm_tokens(token_generator: Iterator[str]) -> None
    
    # Hook Handlers
    def _on_flow_start(shared: dict) -> None
    def _on_node_exec(node: Node, input_data: dict) -> None
    def _on_state_change(old_state: dict, new_state: dict) -> None
    def _on_flow_finish(shared: dict, result: Any) -> None
```

**Event Mapping**:
| Flow Event | AG-UI Event Type | Data |
|------------|------------------|------|
| Flow Start | `RUN_STARTED` | `{run_id, timestamp}` |
| Node Exec | `TOOL_CALL_START` | `{tool_name, arguments}` |
| Node Complete | `TOOL_CALL_END` | `{tool_name, result}` |
| LLM Token | `TEXT_MESSAGE_CONTENT` | `{delta}` |
| State Change | `STATE_DELTA` | `{patch}` |
| Flow Finish | `RUN_FINISHED` | `{result}` |
| Error | `ERROR` | `{message, stack_trace}` |

---

### 2.3 Agent Block System

#### 2.3.1 Agent Metadata Schema

**File**: `agent.yaml`

**Schema** (Pydantic):
```python
class AgentMetadata(BaseModel):
    meta: MetaInfo
    interfaces: InterfaceDefinition
    protocols: ProtocolConfig
    dependencies: DependencySpec
    pocketflow: PocketFlowConfig
    visual: VisualConfig

class MetaInfo(BaseModel):
    id: str                    # Unique identifier (kebab-case)
    name: str                  # Display name
    version: str               # Semantic version
    author: str
    icon: str                  # Emoji or icon name
    category: str              # Category for marketplace
    description: str

class InterfaceDefinition(BaseModel):
    inputs: List[InputField]
    outputs: List[OutputField]

class InputField(BaseModel):
    name: str
    type: str                  # file, string, number, enum, etc.
    required: bool = True
    description: str
    accept: Optional[List[str]] = None  # For file types
    options: Optional[List[str]] = None # For enum types
    default: Optional[Any] = None

class OutputField(BaseModel):
    name: str
    type: str
    schema: Optional[dict] = None  # JSON schema for complex types

class ProtocolConfig(BaseModel):
    mcp: Optional[MCPConfig] = None
    a2a: Optional[A2AConfig] = None
    agui: Optional[AGUIConfig] = None

class MCPConfig(BaseModel):
    tools: List[str]           # Tool URIs: mcp://server/tool
    resources: List[str]       # Resource URIs

class A2AConfig(BaseModel):
    enabled: bool = True
    skills: List[str]          # Skill names
    card_path: Optional[str] = None

class AGUIConfig(BaseModel):
    enabled: bool = True
    events: List[str]          # Event types to emit

class DependencySpec(BaseModel):
    agents: List[str] = []     # Other agent IDs
    tools: List[str] = []      # MCP tool URIs
    packages: List[str] = []   # Python packages

class PocketFlowConfig(BaseModel):
    entry: str                 # Module path: "flow.py:create_flow"
    shared_schema: str         # Schema path: "schemas.py:MySchema"

class VisualConfig(BaseModel):
    color: str                 # Hex color
    size: str = "medium"       # small/medium/large
    ports: dict                # Input/output port positions
```

---

#### 2.3.2 Agent Block Manager

**Purpose**: Load, validate, and manage agent blocks

**Class Diagram**:
```python
class AgentBlockManager:
    """Manages agent block lifecycle"""
    
    # Attributes
    registry: Dict[str, AgentBlock]
    registry_path: Path
    
    # Methods
    def load_agent(agent_path: Path) -> AgentBlock
    def validate_agent(agent_meta: AgentMetadata) -> ValidationResult
    def register_agent(agent_id: str, agent_block: AgentBlock) -> None
    def get_agent(agent_id: str) -> AgentBlock
    def list_agents(filters: dict = None) -> List[AgentInfo]
    def resolve_dependencies(agent_id: str) -> List[str]
```

**Agent Loading Process**:
1. Read `agent.yaml` and parse with Pydantic
2. Validate metadata schema
3. Check dependencies availability
4. Load PocketFlow entry point
5. Apply protocol adapters
6. Register in local registry

---

#### 2.3.3 Auto-Adapter Decorator

**Purpose**: Automatically generate protocol adapters from metadata

**Implementation**:
```python
def auto_adapt(protocols: List[str]):
    """
    Decorator that auto-generates protocol adapters
    
    Usage:
        @auto_adapt(protocols=["mcp", "a2a", "agui"])
        class MyAgent(AgentBlock):
            ...
    """
    def decorator(cls):
        original_init = cls.__init__
        
        def new_init(self, *args, **kwargs):
            original_init(self, *args, **kwargs)
            
            # Load metadata
            self.metadata = self.load_metadata("agent.yaml")
            
            # Generate protocol adapters
            if "mcp" in protocols:
                self._mcp_tools = ProtocolAdapter.generate_mcp_tools(self.metadata)
            
            if "a2a" in protocols:
                self._a2a_card = ProtocolAdapter.generate_a2a_card(self.metadata)
            
            if "agui" in protocols:
                ProtocolAdapter.wrap_flow_with_agui(self.flow, self.metadata)
        
        cls.__init__ = new_init
        
        # Add protocol interface methods
        if "mcp" in protocols:
            cls.get_mcp_tools = lambda self: self._mcp_tools
        
        if "a2a" in protocols:
            cls.get_a2a_card = lambda self: self._a2a_card
        
        return cls
    
    return decorator
```

---

### 2.4 CLI Tool

**Purpose**: Command-line interface for all framework operations

**Command Structure**:
```
agentflow
├── init <name>                    # Create new project
├── create
│   ├── agent <name>               # Create agent from scratch
│   ├── from-template <template>   # Create from template
│   └── tool <name>                # Create MCP tool
├── install <agent-id>             # Install from marketplace
├── search <query>                 # Search marketplace
├── list
│   ├── agents                     # List local agents
│   ├── tools                      # List MCP tools
│   └── templates                  # List templates
├── run <agent-name>               # Run agent
├── test                           # Run tests
├── studio                         # Launch visual editor
├── protocols
│   ├── test                       # Test protocol compatibility
│   └── validate                   # Validate configurations
├── publish <path>                 # Publish to marketplace
└── deploy                         # Deploy to production
```

**Implementation** (Click framework):
```python
import click
from rich.console import Console
from rich.table import Table

@click.group()
def cli():
    """AgentFlow - Build AI Agents Like LEGO"""
    pass

@cli.command()
@click.argument('name')
@click.option('--protocols', default='mcp,a2a,agui', help='Protocols to enable')
def init(name, protocols):
    """Create a new AgentFlow project"""
    console = Console()
    
    with console.status(f"[bold green]Creating project {name}..."):
        # Create project structure
        create_project_structure(name, protocols.split(','))
    
    console.print(f"✅ Project {name} created successfully!", style="bold green")
    console.print(f"\nNext steps:")
    console.print(f"  cd {name}")
    console.print(f"  agentflow create agent my-agent")
```

---

### 2.5 Agent Marketplace

**Purpose**: Registry and distribution system for agent blocks

**Architecture**:
```
Marketplace
├── Local Registry (.agentflow/registry.yaml)
├── Remote Index (marketplace.agentflow.ai/index.json)
└── Agent Sources
    ├── Local Agents (file://)
    ├── Remote A2A Agents (a2a://)
    └── Marketplace Packages (marketplace://)
```

**Registry Schema** (`.agentflow/registry.yaml`):
```yaml
agents:
  - id: pdf-analyzer
    name: "PDF Analyzer"
    version: "1.0.0"
    source: "local"
    path: "./agents/pdf-analyzer"
    protocols: [mcp, a2a, agui]
    
  - id: translator-pro
    name: "Translator Pro"
    version: "2.1.0"
    source: "a2a"
    endpoint: "https://api.example.com/translator"
    protocols: [a2a]
    
  - id: email-sender
    name: "Email Sender"
    version: "1.5.0"
    source: "marketplace"
    package_url: "https://marketplace.agentflow.ai/packages/email-sender-1.5.0.tar.gz"
    protocols: [mcp, a2a, agui]
```

**Marketplace API**:
```python
class AgentMarketplace:
    def search(query: str, filters: dict = None) -> List[AgentInfo]
    def install(agent_id: str, version: str = "latest") -> None
    def publish(agent_path: Path, metadata: dict) -> str
    def get_agent_info(agent_id: str) -> AgentInfo
    def check_updates() -> List[UpdateInfo]
```

---

## 3. Data Models

### 3.1 Core Data Structures

```python
@dataclass
class AgentBlock:
    """Represents a loaded agent block"""
    id: str
    metadata: AgentMetadata
    flow: PocketFlow
    mcp_tools: Optional[List[Tool]]
    a2a_card: Optional[AgentCard]
    agui_emitter: Optional[AGUIEventEmitter]

@dataclass
class AgentInfo:
    """Agent information for marketplace"""
    id: str
    name: str
    description: str
    version: str
    author: str
    category: str
    protocols: List[str]
    rating: float
    downloads: int
    source_type: str  # local/remote/marketplace
    source_url: str

@dataclass
class ValidationResult:
    """Agent validation result"""
    valid: bool
    errors: List[str]
    warnings: List[str]
```

---

## 4. API Design

### 4.1 Python API

```python
# Initialize framework
from agentflow import AgentFlow, get_agent

app = AgentFlow(config_path=".agentflow/config.yaml")

# Load and run agent
agent = get_agent("pdf-analyzer")
result = await agent.run({
    "pdf_path": "document.pdf",
    "analysis_type": "summarize"
})

# Compose multiple agents
from agentflow import Workflow

class DocumentProcessor(Workflow):
    def __init__(self):
        self.pdf_agent = get_agent("pdf-analyzer")
        self.translator = get_agent("translator")
        self.email_sender = get_agent("email-sender")
    
    async def run(self, input_data):
        # Sequential execution
        pdf_result = await self.pdf_agent.run({"pdf_path": input_data["file"]})
        translated = await self.translator.run({"text": pdf_result["text"]})
        await self.email_sender.run({"to": input_data["email"], "content": translated})
        
        return {"status": "success"}
```

### 4.2 REST API (for Studio)

```
GET    /api/agents                    # List all agents
GET    /api/agents/:id                # Get agent details
POST   /api/agents/:id/run            # Execute agent
GET    /api/agents/:id/events         # SSE stream (AG-UI)

GET    /api/marketplace/search        # Search marketplace
POST   /api/marketplace/install       # Install agent

GET    /api/workflows                 # List workflows
POST   /api/workflows                 # Create workflow
PUT    /api/workflows/:id             # Update workflow
POST   /api/workflows/:id/run         # Execute workflow
```

---

## 5. Security Considerations

### 5.1 Agent Isolation
- Each agent runs in its own execution context
- Shared state is immutable between agents
- MCP tools have explicit permission declarations

### 5.2 Marketplace Security
- Agent packages are validated before installation
- Code signing for published agents (future)
- Dependency vulnerability scanning

### 5.3 Protocol Security
- A2A endpoints require authentication tokens
- MCP servers run with limited file system access
- AG-UI events don't expose sensitive data

---

## 6. Performance Optimization

### 6.1 Lazy Loading
- Agents are loaded on-demand
- MCP connections are pooled and reused
- Protocol adapters are cached

### 6.2 Async Execution
- All I/O operations are async
- Parallel agent execution where possible
- Streaming responses for LLM outputs

### 6.3 Caching Strategy
- Agent metadata cached in memory
- MCP tool definitions cached
- A2A AgentCards cached with TTL

---

## 7. Error Handling

### 7.1 Error Categories
- **Configuration Errors**: Invalid YAML, missing files
- **Validation Errors**: Schema violations, type mismatches
- **Runtime Errors**: Node execution failures, timeouts
- **Protocol Errors**: MCP/A2A/AG-UI communication failures

### 7.2 Error Recovery
- Automatic retry with exponential backoff
- Fallback to alternative agents (if configured)
- Graceful degradation (skip optional steps)
- Detailed error logging with context

---

## 8. Testing Strategy

### 8.1 Unit Tests
- Each component tested in isolation
- Mock protocol adapters for testing
- 80%+ code coverage target

### 8.2 Integration Tests
- End-to-end agent execution
- Protocol compatibility tests
- Marketplace operations

### 8.3 Performance Tests
- Load testing with 50+ concurrent agents
- Latency measurements for protocol overhead
- Memory usage profiling

---

## 9. Deployment Architecture

### 9.1 Development Mode
```
Local Machine
├── AgentFlow CLI
├── Local Agents
├── MCP Servers (local)
└── Studio (localhost:3000)
```

### 9.2 Production Mode
```
Cloud Environment
├── AgentFlow Runtime (Docker)
├── Agent Registry (S3/GCS)
├── MCP Servers (Kubernetes)
├── A2A Gateway (Load Balanced)
└── Monitoring (Prometheus/Grafana)
```

---

## 10. Technology Stack Summary

| Layer | Technology | Version | Justification |
|-------|-----------|---------|---------------|
| **Core** | Python | 3.13+ | AG-UI requires 3.13+; JIT performance boost (10-20%); LTS until 2029 |
| **Workflow** | PocketFlow | Latest | Lightweight, AI-friendly |
| **MCP** | mcp Python SDK | Latest | Official Anthropic SDK (supports 3.10+) |
| **A2A** | google-a2a | Latest | Official Google SDK (supports 3.9+) |
| **AG-UI** | ag-ui Python | Latest | Official AG-UI SDK (requires 3.13+) |
| **CLI** | Click + Rich | Latest | Best Python CLI experience |
| **Validation** | Pydantic | 2.x | Type safety, validation |
| **Config** | PyYAML | Latest | Human-readable configs |
| **Frontend** | React + React Flow | Latest | Visual workflow editor |
| **Testing** | pytest | Latest | Industry standard |

---

## 11. Migration Path

### 11.1 From LangChain
- Provide LangChain adapter for existing chains
- Migration guide with examples
- Tool to convert LangChain agents to AgentFlow

### 11.2 From CrewAI
- Similar agent-based architecture
- Map CrewAI roles to AgentFlow blocks
- Preserve multi-agent orchestration patterns

---

## 12. Future Enhancements (Post-V1.0)

- **Enterprise Features**: SSO, RBAC, audit logs
- **Cloud Marketplace**: Hosted agent registry
- **Mobile SDK**: React Native components
- **Real-time Collaboration**: Multi-user studio editing
- **Advanced Monitoring**: Distributed tracing, metrics
- **Custom Protocols**: Plugin system for new protocols

---

**Document Version**: 1.0  
**Created**: 2025-11-03  
**Status**: Draft

*This design document provides implementation-ready specifications for all AgentFlow components.*

