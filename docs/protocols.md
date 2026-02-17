# Protocols and Adapters (Internal)

Last Updated: 2026-02-17
Audience: Developer / Operator

## 1. Scope

This document is for internal implementation. Business users should not be exposed to protocol-level details.

- Business surface: only Studio workflow and artifacts
- Developer/operator surface: protocol adapters, channel wiring, policy controls

## 2. Protocol Position in Architecture

```mermaid
flowchart LR
    classDef surface fill:#f1f8e9,stroke:#7cb342,color:#33691e,stroke-width:1.5px
    classDef kernel fill:#e8f5e9,stroke:#43a047,color:#1b5e20,stroke-width:1.5px
    classDef adapter fill:#fff8e1,stroke:#fb8c00,color:#e65100,stroke-width:1.5px
    classDef gov fill:#ffebee,stroke:#e53935,color:#b71c1c,stroke-width:1.5px

    B["Business Surface"]
    D["Developer/Operator Surface"]
    K["Kernel Runner"]
    A["Protocol Adapters\nMCP / A2A / AG-UI / A2UI / WS"]
    G["Policy + Audit"]

    B --> K
    D --> K
    K --> A
    A --> G
    K --> G

    class B,D surface
    class K kernel
    class A adapter
    class G gov
```

## 3. Protocol Usage Policy

1. Protocols are implementation detail, not product entry.
2. Side-effect operations must go through policy and audit chain.
3. Protocol adapters are pluginized; do not hardcode them into product logic.
4. Business profile audit does not enforce protocol-surface checks (A2A/MCP/stream).
5. Developer profile audit keeps protocol checks enabled.

## 4. Adapter Categories

- MCP adapter: tool/resource integration
- A2A adapter: agent-to-agent coordination
- AG-UI/A2UI adapter: UI event and component transport
- WebSocket adapter: bidirectional session interaction

## 5. Operational Checklist

- Declare adapter capabilities and risk tier in plugin manifest.
- Bind required permissions explicitly.
- Validate compatibility against kernel and product line versions.
- Capture audit records for adapter execution paths.
