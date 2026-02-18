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

## Test Env Bootstrap

```bash
conda run -n agentflow python scripts/bootstrap_test_env.py --env-file .env
```

- 共有テスト時の env は手動作成せず、上記コマンドで自動補完する。

## Production / Tenant Mail Policy

- 本番シークレットは Secret Manager 注入を使用する。
- 多租户招待メールは「通知」と「ログイン URL」を別メールに分離する。
- 詳細手順: `docs/internal/env-bootstrap-and-tenant-invite-security.md`
