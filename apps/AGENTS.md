# apps/

## Overview
- Sample applications demonstrating the framework.

## Structure
```
apps/
├── code_migration_assistant
├── decision_governance_engine/
├── design_skills_engine
├── platform/
├── market_trend_monitor/
├── faq_system/
└── messaging_hub/
```

## Apps
| App | Location | Notes |
|-----|----------|-------|
| Decision Governance Engine | `apps/decision_governance_engine/` | PipelineEngine-based multi-agent decision support.
| Market Trend Monitor | `apps/market_trend_monitor/` | Multi-agent data collection + analysis example.
| Code Migration Assistant | `apps/code_migration_assistant/` | MCP-tool-style migration workflow.
| FAQ System | `apps/faq_system/` | Demo app; framework contains most business logic.
| Messaging Hub | `apps/messaging_hub/` | Multi-platform chat gateway (Telegram/Slack/Discord).

## Notes
- Each app has its own entrypoints and README; prefer `apps/*/AGENTS.md` for per-app specifics.

## Anti-Patterns
- Reusing app code as framework code: move reusable pieces into `agentflow/`.
