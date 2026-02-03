# agentflow/deploy/

## Overview
- Deployment/config generation + deploy target execution wiring.

## Structure
```
agentflow/deploy/
├── __init__.py         # public deploy functions
├── config/             # DeployTarget -> ConfigTemplate
├── targets/            # per-platform deploy targets
├── templates/          # static templates (Dockerfile)
└── workflow_generator.py # workflow -> code/zip
```

## Where To Look
| Task | Location | Notes |
|------|----------|-------|
| Public deploy API | `agentflow/deploy/__init__.py` | Functions like `generate_all`, `generate_workflow_code`.
| Target implementations | `agentflow/deploy/targets/` | Docker, Vercel, AWS Lambda, GitHub Actions, etc.
| Config templates (UI fields) | `agentflow/deploy/config/manager.py` | `DeployTarget` -> `ConfigTemplate` mapping.
| Static deploy templates | `agentflow/deploy/templates/` | Dockerfile templates.
| Workflow -> code path | `agentflow/deploy/workflow_generator.py` | Studio workflow to generated code/zip.

## Notes
- CI/CD reusable workflows live in `.github/workflows/` and mirror deploy targets.

## Anti-Patterns
- Committing `.env` or credentials; secrets scanning is enforced by pre-commit.
