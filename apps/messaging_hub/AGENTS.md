# apps/messaging_hub/

## Overview
- Multi-platform chatbot gateway (Telegram/Slack/Discord) with unified session management.

## Structure
```
apps/messaging_hub/
├── main.py
├── agents/
└── admin_ui/  # optional
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| FastAPI app + runtime entry | `apps/messaging_hub/main.py` | Contains FastAPI app + `__main__` runner.
| Agents | `apps/messaging_hub/agents/` | App-level agents.
| Env config | `apps/messaging_hub/.env.example` | Platform tokens + LLM provider.

## Run (Typical)
```bash
python apps/messaging_hub/main.py
# or
uvicorn apps.messaging_hub.main:app --reload --port 8000
```

## Notes
- Requires at least one LLM provider key and one platform token (see `.env.example`).
