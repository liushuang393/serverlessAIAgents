# agentflow/protocols/

## Overview
- Protocol layer: MCP (tools), A2A (agent-to-agent), AG-UI (SSE events), A2UI (declarative UI over AG-UI), plus UCP.

## Structure
```
agentflow/protocols/
├── __init__.py        # protocol export surface
├── agui_events.py     # AG-UI event models
├── agui_emitter.py    # converts runtime events -> AG-UI
├── mcp_client.py      # MCP client
├── mcp_tool.py        # MCP tool types
├── a2a_client.py      # A2A client
├── a2a_server.py      # A2A server
└── a2ui/              # A2UI components/renderer/emitter
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Protocol exports | `agentflow/protocols/__init__.py` | Single import surface for protocol types.
| AG-UI event schemas | `agentflow/protocols/agui_events.py` | Canonical event models + `.to_dict()` / `.to_sse()`.
| AG-UI emitter | `agentflow/protocols/agui_emitter.py` | Converts runtime events to AG-UI events.
| A2UI components/renderer | `agentflow/protocols/a2ui/` | Components, renderer, emitter, rich content.
| MCP client/tool types | `agentflow/protocols/mcp_client.py`, `agentflow/protocols/mcp_tool.py` | Client and tool request/response.
| A2A client/server | `agentflow/protocols/a2a_client.py`, `agentflow/protocols/a2a_server.py` | Agent discovery/communication.
| UCP | `agentflow/protocols/ucp/` | UCP messages + client/adapter.

## Anti-Patterns
- Do not craft custom events as `{type: "...", data: {...}}`. Use `AGUIEvent` subclasses.
- Progress events must include `current`, `total`, `percentage` (avoid ambiguous progress).
