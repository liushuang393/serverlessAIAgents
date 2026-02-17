# AgentFlow Platform

Platform は 3 Studio 製品線と Framework 管理面を提供します。

## 1. 役割

- 顧客向け: 3 Studio の実行導線（テンプレート → 設定 → 実行 → 成果物）
- 開発向け: Framework API と Plugin 拡張
- 運用向け: 監査、ポリシー、実行管理

## 2. 正規 API Prefix

- Studio API: `/api/studios/*`
- Framework API: `/api/studios/framework/*`

旧 `/api/agents` などの経路は廃止済みです。

## 3. 主要エンドポイント

### Studio

- `GET /api/studios`
- `GET /api/studios/{studio}/templates`
- `POST /api/studios/{studio}/runs`
- `GET /api/studios/{studio}/runs/{id}/artifacts`

### Framework

- `GET /api/studios/framework/apps`
- `GET /api/studios/framework/agents`
- `GET /api/studios/framework/skills`
- `GET /api/studios/framework/rag/overview`
- `GET /api/studios/framework/mcp/config`
- `GET /api/studios/framework/gallery/featured`
- `GET /api/studios/framework/components`
- `GET /api/studios/framework/dashboard/{tenant_id}`
- `POST /api/studios/framework/publish/deploy`

## 4. 開発起動

1. `conda activate agentflow`
2. `python -m apps.platform.main serve --port 8000`
3. `cd apps/platform/frontend && npm run dev`

## 5. 参照ドキュメント

- 外部向け: `docs/external/README.md`
- 内部向け: `docs/internal/README.md`
- アーキテクチャ: `docs/architecture.md`
- app_config 契約: `apps/platform/docs/app-config-schema.md`
