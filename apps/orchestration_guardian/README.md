# Orchestration Guardian

Lightweight app for validating orchestration readiness and protocol contract coverage.

`product_line`: `framework` / `surface_profile`: `operator`

## Run

```bash
python -m apps.orchestration_guardian.main
```

## Endpoints

- `GET /api/health`
- `GET /api/checklist`
- `POST /api/verify`
