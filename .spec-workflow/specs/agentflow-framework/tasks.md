# Tasks: AgentFlow Framework Implementation

## Task Organization

**Total Tasks**: 24  
**Estimated Duration**: 10 weeks  
**Priority Levels**: P0 (Must Have), P1 (Should Have), P2 (Nice to Have)

---

## Phase 1: Core Framework & Protocol Integration (Week 1-3)

### Task 1.1: Project Structure Setup
- [x] **Priority**: P0
- [x] **Effort**: 1 day
- [x] **Dependencies**: None
- [ ] **Files**: 
  - `agentflow/__init__.py`
  - `agentflow/core/__init__.py`
  - `agentflow/protocols/__init__.py`
  - `agentflow/cli/__init__.py`
  - `setup.py`
  - `pyproject.toml`

**Description**: Create the basic project structure and package configuration.

**Requirements**: 
- FR-004 (CLI Tool)

**Acceptance Criteria**:
- Project can be installed with `pip install -e .`
- All core modules are importable
- Package metadata is complete (name, version, dependencies)

**_Prompt**:
```
Role: Python Package Engineer
Task: Set up the AgentFlow framework project structure
Context: 
- Reference design.md Section 1.2 for component structure
- Use modern Python packaging (pyproject.toml)
- Include all dependencies from design.md Section 10

Restrictions:
- Must support Python 3.13+ (AG-UI protocol requirement)
- Keep dependencies minimal
- Use semantic versioning

_Leverage:
- pyproject.toml template from Python packaging guide
- Standard src layout pattern

_Requirements: FR-004

Success:
- `pip install -e .` works without errors
- `import agentflow` succeeds
- All core submodules are importable
- Dependencies are correctly specified

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md (change [ ] to [-])
3. Implement the task
4. Mark as complete in tasks.md (change [-] to [x])
```

---

### Task 1.2: PocketFlow Engine Wrapper
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 1.1
- [ ] **Files**:
  - `agentflow/core/engine.py`
  - `agentflow/core/hooks.py`
  - `tests/test_engine.py`

**Description**: Implement AgentFlowEngine that wraps PocketFlow with event hooks.

**Requirements**: 
- FR-003 (Workflow Engine)
- NFR-001.1 (Performance)

**Acceptance Criteria**:
- Engine can execute PocketFlow graphs
- All 6 hook events are triggered correctly
- Hooks can be registered and unregistered
- Shared state is properly managed
- Unit tests achieve 90%+ coverage

**_Prompt**:
```
Role: Workflow Engine Developer
Task: Implement AgentFlowEngine wrapper for PocketFlow with event hooks
Context:
- Reference design.md Section 2.1 for detailed specifications
- PocketFlow is a lightweight workflow engine (~100 lines)
- Need to inject hooks without modifying PocketFlow core

Restrictions:
- Must not modify PocketFlow source code
- Hook overhead must be <10ms per event
- Thread-safe hook execution

_Leverage:
- Python decorators for hook registration
- Monkey patching for Node.run interception
- Context managers for hook lifecycle

_Requirements: FR-003, NFR-001.1

Success:
- All 6 hook events (on_start, on_node_exec, on_node_complete, on_state_change, on_llm_token, on_finish) work
- Multiple hooks can be registered for same event
- Hooks receive correct arguments
- Tests verify hook execution order
- Performance overhead <10ms per hook

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Study PocketFlow Node and Flow classes
4. Implement AgentFlowEngine with hook system
5. Write comprehensive unit tests
6. Mark as complete in tasks.md
```

---

### Task 1.3: MCP Client Integration
- [x] **Priority**: P0
- [x] **Effort**: 3 days
- [x] **Dependencies**: Task 1.1
- [ ] **Files**:
  - `agentflow/protocols/mcp_client.py`
  - `agentflow/protocols/mcp_config.py`
  - `.agentflow/protocols/mcp.yaml` (example)
  - `tests/test_mcp_client.py`

**Description**: Implement MCP client that connects to multiple MCP servers and exposes tools.

**Requirements**: 
- FR-001.1, FR-001.2 (MCP Integration)
- NFR-001.2 (MCP overhead <100ms)

**Acceptance Criteria**:
- Can connect to multiple MCP servers from YAML config
- Tools are callable via URI format `mcp://server/tool`
- Tool definitions are generated for LLM use
- Connection pooling and reuse
- Graceful error handling for server failures
- Integration tests with mock MCP servers

**_Prompt**:
```
Role: Protocol Integration Engineer
Task: Implement MCP client for connecting to multiple MCP servers
Context:
- Reference design.md Section 2.2.1 for specifications
- Use official mcp Python SDK from Anthropic
- Support stdio-based MCP servers

Restrictions:
- Must handle server crashes gracefully
- Tool call overhead <100ms
- Support async operations only

_Leverage:
- mcp.client.stdio for server connections
- asyncio for concurrent server management
- PyYAML for configuration parsing

_Requirements: FR-001.1, FR-001.2, NFR-001.2

Success:
- Can connect to 3+ MCP servers simultaneously
- Tool URI parsing works: mcp://server/tool
- get_tool_definitions() returns LLM-compatible format
- call_tool() executes and returns results
- Connection errors don't crash the system
- Tests cover all error scenarios

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement MCPToolManager class
4. Create example mcp.yaml configuration
5. Write integration tests with mock servers
6. Mark as complete in tasks.md
```

---

### Task 1.4: A2A Server Implementation
- [x] **Priority**: P0
- [x] **Effort**: 3 days
- [x] **Dependencies**: Task 1.1, Task 1.2
- [ ] **Files**:
  - `agentflow/protocols/a2a_server.py`
  - `agentflow/protocols/a2a_card.py`
  - `tests/test_a2a_server.py`

**Description**: Implement A2A server that exposes local agents as A2A endpoints.

**Requirements**: 
- FR-001.3 (A2A Server)
- US-001 (Protocol Support)

**Acceptance Criteria**:
- Agents can be registered to A2A server
- AgentCard is auto-generated from agent.yaml
- A2A endpoints respond to task requests
- Skills are properly mapped to agent capabilities
- Server can handle concurrent requests
- Integration tests with google-a2a SDK

**_Prompt**:
```
Role: Distributed Systems Engineer
Task: Implement A2A server to expose local agents as A2A endpoints
Context:
- Reference design.md Section 2.2.2 for specifications
- Use official google-a2a SDK
- AgentCard must be generated from agent.yaml metadata

Restrictions:
- Must support async task handling
- Timeout for long-running tasks (30s default)
- Proper error responses per A2A spec

_Leverage:
- google_a2a.A2AServer for server implementation
- Pydantic for AgentCard validation
- FastAPI for HTTP endpoints (if needed)

_Requirements: FR-001.3, US-001

Success:
- register_agent() adds agent to server
- generate_agent_card() creates valid AgentCard
- A2A task requests execute agent flows
- Multiple agents can be registered
- Concurrent requests are handled
- Tests verify A2A protocol compliance

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement AgentFlowA2AServer class
4. Implement AgentCard generation logic
5. Write integration tests
6. Mark as complete in tasks.md
```

---

### Task 1.5: A2A Client Implementation
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 1.4
- [ ] **Files**:
  - `agentflow/protocols/a2a_client.py`
  - `tests/test_a2a_client.py`

**Description**: Implement A2A client for discovering and calling remote agents.

**Requirements**: 
- FR-001.4 (A2A Client)
- US-002 (Auto Protocol Adapters)

**Acceptance Criteria**:
- Can discover remote agents via endpoint
- AgentCard is fetched and cached
- Remote agent calls work with proper error handling
- Timeout and retry logic implemented
- Integration tests with mock A2A servers

**_Prompt**:
```
Role: API Client Developer
Task: Implement A2A client for calling remote agents
Context:
- Reference design.md Section 2.2.2 for specifications
- Must work with A2A server from Task 1.4
- Support agent discovery and invocation

Restrictions:
- Timeout after 30s (configurable)
- Retry failed requests (3 attempts)
- Cache AgentCards with TTL

_Leverage:
- google_a2a SDK for client operations
- httpx for async HTTP requests
- cachetools for AgentCard caching

_Requirements: FR-001.4, US-002

Success:
- discover_agent() fetches and parses AgentCard
- call_remote_agent() executes remote tasks
- Timeout and retry logic works
- AgentCard caching reduces redundant requests
- Tests cover network failures

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement AgentFlowA2AClient class
4. Add caching and retry logic
5. Write integration tests
6. Mark as complete in tasks.md
```

---

### Task 1.6: AG-UI Event Emitter
- [x] **Priority**: P1
- [x] **Effort**: 3 days
- [x] **Dependencies**: Task 1.2
- [ ] **Files**:
  - `agentflow/protocols/agui_emitter.py`
  - `agentflow/protocols/agui_events.py`
  - `tests/test_agui_emitter.py`

**Description**: Implement AG-UI event emitter that converts flow events to SSE streams.

**Requirements**: 
- FR-001.5 (AG-UI Integration)
- US-006 (Real-time Preview)

**Acceptance Criteria**:
- Emitter attaches to AgentFlowEngine hooks
- All flow events are converted to AG-UI events
- SSE streaming works correctly
- Event types match AG-UI specification
- Integration tests verify event sequences

**_Prompt**:
```
Role: Real-time Systems Developer
Task: Implement AG-UI event emitter for flow execution streaming
Context:
- Reference design.md Section 2.2.3 for specifications
- Use official ag-ui Python SDK
- Events must follow AG-UI protocol spec

Restrictions:
- Event latency <50ms
- Must handle backpressure
- No event loss during streaming

_Leverage:
- ag_ui.AGUIServer for SSE streaming
- AgentFlowEngine hooks from Task 1.2
- asyncio.Queue for event buffering

_Requirements: FR-001.5, US-006

Success:
- attach_to_flow() registers all necessary hooks
- Flow events map to correct AG-UI event types
- SSE stream delivers events in real-time
- Multiple clients can subscribe
- Tests verify event ordering and content

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement AGUIEventEmitter class
4. Map all flow events to AG-UI events
5. Write integration tests with mock clients
6. Mark as complete in tasks.md
```

---

### Task 1.7: Agent Metadata Schema
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 1.1
- [ ] **Files**:
  - `agentflow/core/metadata.py`
  - `agentflow/core/schemas.py`
  - `tests/test_metadata.py`

**Description**: Implement Pydantic models for agent.yaml metadata schema.

**Requirements**: 
- FR-002 (Agent Block System)
- NFR-003.2 (Type Hints)

**Acceptance Criteria**:
- All metadata fields have Pydantic models
- Validation catches schema violations
- YAML parsing and serialization works
- Example agent.yaml provided
- Comprehensive validation tests

**_Prompt**:
```
Role: Data Modeling Engineer
Task: Implement Pydantic models for agent.yaml metadata schema
Context:
- Reference design.md Section 2.3.1 for complete schema
- Use Pydantic v2 for validation
- Support YAML serialization

Restrictions:
- All fields must have type hints
- Validation errors must be clear
- Support optional fields with defaults

_Leverage:
- Pydantic BaseModel for schema definition
- PyYAML for YAML parsing
- JSON Schema generation for documentation

_Requirements: FR-002, NFR-003.2

Success:
- AgentMetadata model covers all fields
- Nested models (MetaInfo, InterfaceDefinition, etc.) work
- YAML files can be loaded and validated
- Validation errors are actionable
- Example agent.yaml passes validation

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement all Pydantic models from design.md
4. Create example agent.yaml
5. Write validation tests
6. Mark as complete in tasks.md
```

---

### Task 1.8: Agent Block Manager
- [ ] **Priority**: P0
- [ ] **Effort**: 3 days
- [ ] **Dependencies**: Task 1.7
- [ ] **Files**:
  - `agentflow/core/manager.py`
  - `agentflow/core/loader.py`
  - `agentflow/core/validator.py`
  - `tests/test_manager.py`

**Description**: Implement agent block manager for loading, validating, and managing agents.

**Requirements**: 
- FR-002 (Agent Block System)
- US-004 (Install Agent Blocks)

**Acceptance Criteria**:
- Agents can be loaded from directories
- Metadata validation works
- Dependency resolution implemented
- Registry management (add/remove/list)
- Integration tests with sample agents

**_Prompt**:
```
Role: Component Manager Developer
Task: Implement agent block manager for lifecycle management
Context:
- Reference design.md Section 2.3.2 for specifications
- Must handle agent loading, validation, and registry
- Support dependency resolution

Restrictions:
- Circular dependencies must be detected
- Invalid agents must not crash the system
- Registry must be thread-safe

_Leverage:
- AgentMetadata from Task 1.7
- pathlib for file operations
- YAML for registry persistence

_Requirements: FR-002, US-004

Success:
- load_agent() reads and validates agent.yaml
- validate_agent() catches all schema violations
- register_agent() adds to local registry
- resolve_dependencies() returns dependency tree
- list_agents() supports filtering
- Tests cover all error cases

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement AgentBlockManager class
4. Add dependency resolution logic
5. Write comprehensive tests
6. Mark as complete in tasks.md
```

---

## Phase 2: CLI & Marketplace (Week 4-5)

### Task 2.1: CLI Framework Setup
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 1.1
- [ ] **Files**:
  - `agentflow/cli/main.py`
  - `agentflow/cli/commands/__init__.py`
  - `tests/test_cli.py`

**Description**: Set up Click-based CLI framework with Rich formatting.

**Requirements**: 
- FR-004 (CLI Tool)
- NFR-004.2 (Clear Error Messages)

**Acceptance Criteria**:
- CLI entry point works: `agentflow --help`
- Command groups are organized
- Rich formatting for output
- Error handling with helpful messages
- Tests for CLI invocation

**_Prompt**:
```
Role: CLI Developer
Task: Set up AgentFlow CLI framework with Click and Rich
Context:
- Reference design.md Section 2.4 for command structure
- Use Click for command framework
- Use Rich for beautiful terminal output

Restrictions:
- All commands must have --help
- Error messages must be actionable
- Support --verbose flag globally

_Leverage:
- Click decorators for command definition
- Rich Console for formatted output
- Click testing utilities

_Requirements: FR-004, NFR-004.2

Success:
- `agentflow --help` shows all commands
- Command groups (create, list, protocols) work
- Rich formatting displays correctly
- Error messages are clear and helpful
- Tests verify command structure

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement CLI entry point and command groups
4. Add Rich formatting
5. Write CLI tests
6. Mark as complete in tasks.md
```

---

### Task 2.2: CLI Init Command
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 2.1
- [ ] **Files**:
  - `agentflow/cli/commands/init.py`
  - `agentflow/templates/project_template/`
  - `tests/test_cli_init.py`

**Description**: Implement `agentflow init` command for project creation.

**Requirements**: 
- US-001 (One-Command Init)
- FR-004.1 (Project Initialization)

**Acceptance Criteria**:
- Creates complete project structure
- Generates protocol configuration files
- Supports --protocols flag
- Creates example agent
- Tests verify all files are created

**_Prompt**:
```
Role: Project Scaffolding Developer
Task: Implement agentflow init command for project creation
Context:
- Reference requirements.md US-001 for acceptance criteria
- Must create all necessary files and directories
- Support protocol selection via --protocols flag

Restrictions:
- Must not overwrite existing projects
- All generated files must be valid
- Support dry-run mode (--dry-run)

_Leverage:
- Jinja2 for file templates
- pathlib for directory creation
- Click for command implementation

_Requirements: US-001, FR-004.1

Success:
- `agentflow init my-project` creates full structure
- --protocols flag filters protocol configs
- Generated files are syntactically valid
- Example agent is runnable
- Tests verify all scenarios

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Create project templates
4. Implement init command
5. Write tests
6. Mark as complete in tasks.md
```

---

### Task 2.3: CLI Create Commands
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 2.1, Task 1.8
- [ ] **Files**:
  - `agentflow/cli/commands/create.py`
  - `agentflow/templates/agent_template/`
  - `tests/test_cli_create.py`

**Description**: Implement `agentflow create` commands (agent, tool, from-template).

**Requirements**: 
- FR-004.2 (Agent Creation)
- US-007 (Create from Template)

**Acceptance Criteria**:
- `create agent` generates agent structure
- `create from-template` uses template system
- Interactive prompts for configuration
- Validates agent names (kebab-case)
- Tests cover all create variants

**_Prompt**:
```
Role: Code Generator Developer
Task: Implement agentflow create commands for agent/tool/template creation
Context:
- Reference design.md Section 2.4 for command structure
- Must generate valid agent structures
- Support template-based creation

Restrictions:
- Agent names must be kebab-case
- Generated code must pass validation
- Templates must be customizable

_Leverage:
- Click prompts for interactive input
- Jinja2 for code generation
- AgentBlockManager for validation

_Requirements: FR-004.2, US-007

Success:
- `create agent my-agent` generates valid structure
- `create from-template invoice-processor` works
- Interactive prompts collect necessary info
- Generated agents pass validation
- Tests verify all creation modes

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement create command group
4. Add agent and template creation
5. Write tests
6. Mark as complete in tasks.md
```

---

### Task 2.4: Marketplace Search & Install
- [x] **Priority**: P1
- [x] **Effort**: 3 days
- [x] **Dependencies**: Task 1.8, Task 2.1
- [ ] **Files**:
  - `agentflow/marketplace/client.py`
  - `agentflow/marketplace/registry.py`
  - `agentflow/cli/commands/marketplace.py`
  - `tests/test_marketplace.py`

**Description**: Implement marketplace search and install functionality.

**Requirements**: 
- US-003 (Search Agent Blocks)
- US-004 (Install Agent Blocks)
- FR-005 (Marketplace)

**Acceptance Criteria**:
- Search returns relevant results
- Filtering by protocols, category works
- Install downloads and configures agents
- Dependency resolution during install
- Tests with mock marketplace API

**_Prompt**:
```
Role: Package Manager Developer
Task: Implement marketplace search and install commands
Context:
- Reference requirements.md US-003, US-004 for specifications
- Support local and remote marketplaces
- Handle dependency resolution

Restrictions:
- Must validate agents before installation
- Network failures must be handled gracefully
- Support version pinning

_Leverage:
- httpx for API requests
- AgentBlockManager for installation
- YAML for registry management

_Requirements: US-003, US-004, FR-005

Success:
- `agentflow search "PDF"` returns results
- Filtering by --protocols works
- `agentflow install agent-id` downloads and installs
- Dependencies are resolved automatically
- Tests cover network failures

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement marketplace client
4. Add search and install commands
5. Write integration tests
6. Mark as complete in tasks.md
```

---

### Task 2.5: CLI Run & Test Commands
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 1.2, Task 1.8, Task 2.1
- [ ] **Files**:
  - `agentflow/cli/commands/run.py`
  - `agentflow/cli/commands/test.py`
  - `tests/test_cli_run.py`

**Description**: Implement commands to run and test agents.

**Requirements**: 
- FR-004.4 (Testing Commands)
- NFR-004.3 (Helpful CLI)

**Acceptance Criteria**:
- `run` command executes agents
- Input can be provided via CLI or file
- Output is formatted nicely
- `test` command runs agent tests
- Progress indicators for long operations

**_Prompt**:
```
Role: CLI Execution Developer
Task: Implement agentflow run and test commands
Context:
- Reference design.md Section 4.1 for Python API
- Must execute agents and display results
- Support different input formats

Restrictions:
- Must handle agent errors gracefully
- Long operations need progress indicators
- Output must be parseable (JSON mode)

_Leverage:
- AgentBlockManager to load agents
- AgentFlowEngine to execute
- Rich progress bars and spinners

_Requirements: FR-004.4, NFR-004.3

Success:
- `agentflow run my-agent --input data.json` works
- Progress is shown during execution
- Output is formatted with Rich
- `agentflow test` runs all agent tests
- Tests verify execution and error handling

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement run command
4. Implement test command
5. Write tests
6. Mark as complete in tasks.md
```

---

## Phase 3: Auto-Adapter & Integration (Week 6-7) [COMPLETE]

### Task 3.1: Protocol Adapter Generator
- [x] **Priority**: P0
- [x] **Effort**: 3 days
- [x] **Dependencies**: Task 1.3, Task 1.4, Task 1.6, Task 1.7
- [ ] **Files**:
  - `agentflow/adapters/protocol_adapter.py`
  - `agentflow/adapters/mcp_adapter.py`
  - `agentflow/adapters/a2a_adapter.py`
  - `agentflow/adapters/agui_adapter.py`
  - `tests/test_adapters.py`

**Description**: Implement protocol adapter generator that creates MCP/A2A/AG-UI adapters from metadata.

**Requirements**: 
- US-002 (Auto Protocol Adapters)
- FR-001 (Protocol Integration)

**Acceptance Criteria**:
- MCP tools generated from agent.yaml
- A2A AgentCard generated from metadata
- AG-UI events mapped to flow hooks
- All adapters are testable independently
- Integration tests verify protocol compliance

**_Prompt**:
```
Role: Code Generation Engineer
Task: Implement protocol adapter generator for automatic MCP/A2A/AG-UI support
Context:
- Reference design.md Section 2.3.3 for auto-adapter design
- Must generate adapters from AgentMetadata
- Each protocol has different requirements

Restrictions:
- Generated code must be valid
- Must handle missing protocol configs gracefully
- No runtime code generation (use factories)

_Leverage:
- AgentMetadata from Task 1.7
- Protocol clients from Tasks 1.3-1.6
- Factory pattern for adapter creation

_Requirements: US-002, FR-001

Success:
- generate_mcp_tools() creates tool definitions
- generate_a2a_card() creates valid AgentCard
- wrap_flow_with_agui() attaches event emitters
- Each adapter can be tested independently
- Tests verify protocol compliance

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement ProtocolAdapter class
4. Add generator methods for each protocol
5. Write comprehensive tests
6. Mark as complete in tasks.md
```

---

### Task 3.2: Auto-Adapt Decorator
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 3.1
- [ ] **Files**:
  - `agentflow/decorators.py`
  - `tests/test_decorators.py`

**Description**: Implement @auto_adapt decorator that applies protocol adapters to agent classes.

**Requirements**: 
- US-002 (Auto Protocol Adapters)
- NFR-004.1 (Quick Start)

**Acceptance Criteria**:
- Decorator can be applied to agent classes
- Protocol methods are injected automatically
- Supports selective protocol enabling
- Works with inheritance
- Tests verify decorator behavior

**_Prompt**:
```
Role: Python Metaprogramming Expert
Task: Implement @auto_adapt decorator for automatic protocol support
Context:
- Reference design.md Section 2.3.3 for decorator design
- Must inject protocol methods into agent classes
- Support selective protocol enabling

Restrictions:
- Must not break existing class methods
- Should work with inheritance
- Type hints must be preserved

_Leverage:
- Python decorators and __init__ wrapping
- ProtocolAdapter from Task 3.1
- functools.wraps for metadata preservation

_Requirements: US-002, NFR-004.1

Success:
- @auto_adapt(protocols=["mcp", "a2a"]) works
- Protocol methods are accessible on instances
- Decorator can be stacked with others
- Type hints are preserved
- Tests verify all protocol combinations

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement auto_adapt decorator
4. Handle protocol injection logic
5. Write decorator tests
6. Mark as complete in tasks.md
```

---

### Task 3.3: Agent Block Base Class
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: Task 1.8, Task 3.2
- [ ] **Files**:
  - `agentflow/core/agent_block.py`
  - `tests/test_agent_block.py`

**Description**: Implement AgentBlock base class that all agents inherit from.

**Requirements**: 
- FR-002 (Agent Block System)
- US-002 (Auto Protocol Adapters)

**Acceptance Criteria**:
- Base class provides common functionality
- Metadata loading is automatic
- Protocol adapters are applied
- Lifecycle methods (init, run, cleanup)
- Example agent using base class

**_Prompt**:
```
Role: Framework API Designer
Task: Implement AgentBlock base class for all agents
Context:
- Reference design.md Section 2.3 for agent system
- Must provide common functionality for all agents
- Integrate with auto-adapt decorator

Restrictions:
- Must be simple to subclass
- No magic behavior that's hard to debug
- Clear lifecycle methods

_Leverage:
- @auto_adapt decorator from Task 3.2
- AgentMetadata from Task 1.7
- AgentFlowEngine from Task 1.2

_Requirements: FR-002, US-002

Success:
- AgentBlock base class is defined
- Subclasses automatically get protocol support
- load_metadata() works correctly
- run() method is abstract
- Example agent demonstrates usage

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement AgentBlock base class
4. Create example agent
5. Write tests
6. Mark as complete in tasks.md
```

---

### Task 3.4: End-to-End Integration Tests
- [x] **Priority**: P0
- [x] **Effort**: 3 days
- [x] **Dependencies**: All Phase 1-3 tasks
- [ ] **Files**:
  - `tests/integration/test_full_workflow.py`
  - `tests/integration/test_protocols.py`
  - `tests/integration/fixtures/`

**Description**: Create comprehensive integration tests for the entire framework.

**Requirements**: 
- NFR-003.3 (80% Coverage)
- All functional requirements

**Acceptance Criteria**:
- Full agent lifecycle tested (create, load, run)
- All three protocols tested together
- Multi-agent workflows tested
- Error scenarios covered
- Performance benchmarks included

**_Prompt**:
```
Role: QA Engineer
Task: Create comprehensive integration tests for AgentFlow framework
Context:
- Must test entire framework end-to-end
- Cover all three protocols (MCP/A2A/AG-UI)
- Include performance benchmarks

Restrictions:
- Tests must be deterministic
- Use fixtures for test data
- Mock external services

_Leverage:
- pytest fixtures for setup/teardown
- pytest-asyncio for async tests
- pytest-benchmark for performance

_Requirements: NFR-003.3, All FRs

Success:
- Full workflow test passes (init → create → run)
- Protocol integration tests pass
- Multi-agent orchestration works
- Error handling is verified
- Performance meets NFR targets

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Create integration test suite
4. Add fixtures and test data
5. Run and verify all tests pass
6. Mark as complete in tasks.md
```

---

## Phase 4: Visual Studio (Week 8-9) [P2 - Optional]

### Task 4.1: Studio Backend API
- [x] **Priority**: P2
- [x] **Effort**: 3 days
- [x] **Dependencies**: Task 1.2, Task 1.8
- [ ] **Files**:
  - `agentflow/studio/api.py`
  - `agentflow/studio/server.py`
  - `tests/test_studio_api.py`

**Description**: Implement REST API for visual studio backend.

**Requirements**: 
- FR-006 (Visual Studio)
- Design Section 4.2 (REST API)

**Acceptance Criteria**:
- All REST endpoints implemented
- CORS configured for frontend
- WebSocket support for AG-UI events
- API documentation generated
- Integration tests for all endpoints

**_Prompt**:
```
Role: Backend API Developer
Task: Implement REST API for AgentFlow Studio
Context:
- Reference design.md Section 4.2 for API specification
- Use FastAPI for REST endpoints
- Support WebSocket for AG-UI streaming

Restrictions:
- Must be async throughout
- Proper error responses (4xx, 5xx)
- API versioning (/api/v1/)

_Leverage:
- FastAPI for REST API
- WebSockets for AG-UI events
- AgentBlockManager for agent operations

_Requirements: FR-006, Design Section 4.2

Success:
- All endpoints from design.md work
- CORS allows frontend access
- WebSocket streaming works
- OpenAPI docs are generated
- Tests cover all endpoints

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement FastAPI application
4. Add all REST endpoints
5. Write API tests
6. Mark as complete in tasks.md
```

---

### Task 4.2: Studio Frontend - Canvas
- [x] **Priority**: P2
- [x] **Effort**: 5 days
- [x] **Dependencies**: Task 4.1
- [ ] **Files**:
  - `studio/src/components/Canvas.tsx`
  - `studio/src/components/AgentNode.tsx`
  - `studio/src/hooks/useWorkflow.ts`

**Description**: Implement React Flow-based visual workflow canvas.

**Requirements**: 
- US-005 (Drag-and-Drop Composition)
- FR-006.1, FR-006.2 (Canvas & Properties)

**Acceptance Criteria**:
- Drag-and-drop agent blocks
- Connect nodes with edges
- Properties panel for selected nodes
- Save/load workflows
- Undo/redo support

**_Prompt**:
```
Role: Frontend Developer
Task: Implement visual workflow canvas using React Flow
Context:
- Reference requirements.md US-005 for specifications
- Use React Flow for canvas
- Use shadcn/ui for UI components

Restrictions:
- Must be responsive
- Support keyboard shortcuts
- Accessible (WCAG 2.1)

_Leverage:
- React Flow library
- shadcn/ui components
- Zustand for state management

_Requirements: US-005, FR-006.1, FR-006.2

Success:
- Agents can be dragged onto canvas
- Nodes can be connected
- Properties panel shows node config
- Workflows can be saved/loaded
- Undo/redo works

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Set up React Flow canvas
4. Implement agent nodes
5. Add properties panel
6. Mark as complete in tasks.md
```

---

## Phase 5: Templates & Documentation (Week 10)

### Task 5.1: Template System Implementation
- [x] **Priority**: P1
- [x] **Effort**: 3 days
- [x] **Dependencies**: Task 2.3
- [ ] **Files**:
  - `agentflow/templates/template_manager.py`
  - `agentflow/templates/scenarios/`
  - `tests/test_templates.py`

**Description**: Implement template system with scenario templates.

**Requirements**: 
- US-007 (Create from Template)
- FR-007 (Template System)

**Acceptance Criteria**:
- Template metadata schema defined
- Wizard-based customization works
- Parameter substitution implemented
- 3+ scenario templates included
- Tests verify template generation

**_Prompt**:
```
Role: Template System Developer
Task: Implement template system with scenario templates
Context:
- Reference design.md Section 2.4 for template design
- Must support wizard-based customization
- Include invoice-processor template

Restrictions:
- Templates must be self-contained
- Parameter validation required
- Support nested templates

_Leverage:
- Jinja2 for template rendering
- Click prompts for wizard
- YAML for template metadata

_Requirements: US-007, FR-007

Success:
- Template metadata schema works
- Wizard collects customization parameters
- Templates generate valid projects
- 3+ scenario templates included
- Tests verify all templates

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Implement template manager
4. Create 3 scenario templates
5. Write template tests
6. Mark as complete in tasks.md
```

---

### Task 5.2: Documentation & Examples
- [x] **Priority**: P1
- [x] **Effort**: 3 days
- [x] **Dependencies**: All previous tasks
- [x] **Files**:
  - `docs/`
  - `examples/`
  - `README.md`
  - `CONTRIBUTING.md`

**Description**: Create comprehensive documentation and examples.

**Requirements**: 
- NFR-003.4 (Documentation)
- NFR-004.1 (Quick Start)

**Acceptance Criteria**:
- Quick start guide (10 min to first agent)
- API reference documentation
- 5+ example agents
- Architecture documentation
- Contributing guide

**_Prompt**:
```
Role: Technical Writer
Task: Create comprehensive documentation for AgentFlow
Context:
- Reference all requirements and design documents
- Must enable users to get started in 10 minutes
- Include API reference and examples

Restrictions:
- Use MkDocs for documentation
- Code examples must be tested
- Keep language simple and clear

_Leverage:
- MkDocs Material theme
- Auto-generated API docs
- Example agents from tests

_Requirements: NFR-003.4, NFR-004.1

Success:
- Quick start guide exists
- API reference is complete
- 5+ working examples included
- Architecture diagrams present
- Contributing guide available

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Set up MkDocs
4. Write all documentation sections
5. Create example agents
6. Mark as complete in tasks.md
```

---

### Task 5.3: Release Preparation
- [x] **Priority**: P0
- [x] **Effort**: 2 days
- [x] **Dependencies**: All previous tasks
- [x] **Files**:
  - `CHANGELOG.md`
  - `LICENSE`
  - `.github/workflows/`
  - `pyproject.toml` (version bump)

**Description**: Prepare for v1.0 release.

**Requirements**: 
- All functional requirements
- All non-functional requirements

**Acceptance Criteria**:
- Version bumped to 1.0.0
- CHANGELOG complete
- CI/CD pipeline configured
- PyPI package ready
- GitHub release created

**_Prompt**:
```
Role: Release Engineer
Task: Prepare AgentFlow v1.0 release
Context:
- All features are complete
- Tests are passing
- Documentation is ready

Restrictions:
- Follow semantic versioning
- Include migration guide
- Test PyPI upload first

_Leverage:
- GitHub Actions for CI/CD
- twine for PyPI upload
- Keep a Changelog format

_Requirements: All FRs and NFRs

Success:
- Version is 1.0.0
- CHANGELOG lists all features
- CI/CD runs all tests
- Package uploads to PyPI
- GitHub release is created

Instructions:
1. First run spec-workflow-guide to get the workflow guide
2. Mark this task as in-progress in tasks.md
3. Update version and changelog
4. Configure CI/CD
5. Create release
6. Mark as complete in tasks.md
```

---

## Summary

**Total Tasks**: 24
- **Phase 1** (Core): 8 tasks, 19 days
- **Phase 2** (CLI): 5 tasks, 11 days
- **Phase 3** (Integration): 4 tasks, 10 days
- **Phase 4** (Studio): 2 tasks, 8 days [Optional]
- **Phase 5** (Polish): 3 tasks, 8 days

**Critical Path**: Phase 1 → Phase 2 → Phase 3 → Phase 5 (48 days / ~10 weeks)

**Priority Breakdown**:
- P0 (Must Have): 18 tasks
- P1 (Should Have): 4 tasks
- P2 (Nice to Have): 2 tasks

---

## Task Dependencies Graph

```
Phase 1: Core Framework
1.1 (Project Setup)
  ├─→ 1.2 (Engine)
  ├─→ 1.3 (MCP Client)
  ├─→ 1.7 (Metadata)
  └─→ 2.1 (CLI Framework)

1.2 (Engine)
  ├─→ 1.4 (A2A Server)
  ├─→ 1.6 (AG-UI Emitter)
  └─→ 2.5 (Run Command)

1.3 (MCP Client) → 3.1 (Adapter Generator)
1.4 (A2A Server) → 1.5 (A2A Client) → 3.1
1.6 (AG-UI Emitter) → 3.1
1.7 (Metadata) → 1.8 (Manager) → 2.3, 2.4, 3.1

Phase 2: CLI & Marketplace
2.1 (CLI Framework)
  ├─→ 2.2 (Init Command)
  ├─→ 2.3 (Create Commands)
  ├─→ 2.4 (Marketplace)
  └─→ 2.5 (Run/Test Commands)

Phase 3: Integration
3.1 (Adapter Generator) → 3.2 (Decorator)
3.2 (Decorator) → 3.3 (Base Class)
3.3 (Base Class) → 3.4 (Integration Tests)

Phase 4: Studio [Optional]
4.1 (Backend API) → 4.2 (Frontend Canvas)

Phase 5: Polish
5.1 (Templates) → 5.2 (Documentation) → 5.3 (Release)
```

---

## Implementation Guidelines

### Code Quality Standards
- **Type Hints**: All functions must have type hints
- **Docstrings**: Google-style docstrings for all public APIs
- **Testing**: Minimum 80% code coverage
- **Formatting**: Use black and isort
- **Linting**: Pass mypy, flake8, pylint

### Git Workflow
- **Branch Naming**: `task/{task-number}-{short-description}`
  - Example: `task/1.2-engine-wrapper`
- **Commit Messages**: Follow Conventional Commits
  - `feat:`, `fix:`, `docs:`, `test:`, `refactor:`
- **PR Requirements**:
  - All tests passing
  - Code review approved
  - Documentation updated

### Testing Strategy
- **Unit Tests**: Test each component in isolation
- **Integration Tests**: Test component interactions
- **E2E Tests**: Test full user workflows
- **Performance Tests**: Verify NFR requirements

### Documentation Requirements
- **Code Comments**: Explain "why", not "what"
- **API Docs**: Auto-generated from docstrings
- **Examples**: Working code examples for each feature
- **Tutorials**: Step-by-step guides for common tasks

---

## Risk Mitigation

### Technical Risks
1. **PocketFlow Limitations**
   - **Risk**: PocketFlow may not support all needed features
   - **Mitigation**: Fork and extend if necessary (Task 1.2)
   - **Owner**: Task 1.2 implementer

2. **Protocol SDK Changes**
   - **Risk**: MCP/A2A/AG-UI SDKs may have breaking changes
   - **Mitigation**: Version lock dependencies, create adapters
   - **Owner**: Task 3.1 implementer

3. **Performance Bottlenecks**
   - **Risk**: Protocol overhead may exceed targets
   - **Mitigation**: Profile early, optimize critical paths
   - **Owner**: Task 3.4 implementer

### Schedule Risks
1. **Dependency Delays**
   - **Risk**: Blocked tasks waiting for dependencies
   - **Mitigation**: Parallel work where possible, mock interfaces
   - **Owner**: Project manager

2. **Scope Creep**
   - **Risk**: Additional features requested during development
   - **Mitigation**: Strict prioritization, defer to v1.1
   - **Owner**: Product owner

---

## Success Metrics

### Development Metrics
- [ ] All P0 tasks completed
- [ ] 80%+ test coverage achieved
- [ ] All integration tests passing
- [ ] Performance benchmarks met
- [ ] Documentation complete

### Quality Metrics
- [ ] Zero critical bugs
- [ ] <5 high-priority bugs
- [ ] All NFRs validated
- [ ] Code review approval rate >90%

### User Metrics (Post-Release)
- [ ] 100+ GitHub stars in 3 months
- [ ] 10+ community contributions
- [ ] <10 minutes to first agent
- [ ] 90%+ user satisfaction

---

## Next Steps After Task Completion

1. **Review & Approval**: Each task requires code review
2. **Integration**: Merge to main branch after approval
3. **Testing**: Run full test suite on main
4. **Documentation**: Update docs for new features
5. **Release**: Follow release checklist (Task 5.3)

---

*This task breakdown provides implementation-ready work items with clear acceptance criteria and AI-friendly prompts for each task.*

