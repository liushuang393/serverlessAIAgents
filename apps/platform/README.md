# AgentFlow Platform


<!-- README_REQUIRED_SECTIONS_START -->
## 機能概要
- 3 Studio 製品線と Framework 管理 API を単一 Control Plane で提供。
- App/Agent/Skill/RAG/MCP の発見・管理・実行・監査を統合。
- Publish/Deploy 導線を標準化し、app ライフサイクル運用を一元化。

## 優位性
- `/api/studios/*` と `/api/studios/framework/*` の正規導線で契約を統一。
- business/developer/operator の surface 分離で利用者体験を最適化。
- framework 監査機能により、manifest と実装の整合性を継続検証。

## 技術アーキテクチャ
- Backend: FastAPI Routers + Discovery/Lifecycle/Audit Services。
- Frontend: React + Zustand による運用コンソール。
- App Manifest と taxonomy（business_base / pattern）で構成情報を標準化。

## アプリケーション階層
- Studio Surface: 顧客向け実行導線。
- Framework Control: App/Agent/Skill/RAG/MCP 管理。
- Governance Layer: 監査・ポリシー・可視性制御。
- Delivery Layer: 発布、起動、停止、運用検証。
<!-- README_REQUIRED_SECTIONS_END -->

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

### 4.1 開発環境セットアップ（統一手順）

Platform 単体ではなく、リポジトリ全体の開発環境をセットアップします。

```bash
cd <repo-root>
bash setup_dev.sh
```

手動で行う場合:

```bash
conda activate agentflow
pip install -e ".[dev,apps]"
```

### 4.2 起動（Backend / Frontend）

Backend（FastAPI）:

```bash
conda activate agentflow
python -m apps.platform.main serve
```

Frontend:

```bash
cd apps/platform/frontend
npm install
npm run dev
```

ポートは `apps/platform/app_config.json` が単一定義元です（`ports.api` / `ports.frontend`）。

## 5. テスト/静的チェック（統一スクリプト）

```bash
cd <repo-root>
./check.sh format
./check.sh lint
./check.sh type-check
./check.sh test
```

または:

```bash
cd <repo-root>
make check-all
```

## 6. 本番ビルド/発布（Platform に統一）

### 6.1 Publish（推奨）

Platform CLI から publish/deploy を実行します（生成・検証・発布の導線を統一）。

```bash
conda activate agentflow
python -m apps.platform.main publish ./apps/<app_dir> --target docker
```

### 6.2 API 経由（自動化向け）

- `POST /api/studios/framework/publish/deploy`

### 6.3 Frontend ビルド

```bash
cd apps/platform/frontend
npm install
npm run build
```

## 7. 参照ドキュメント

- 外部向け: `docs/external/README.md`
- 内部向け: `docs/internal/README.md`
- アーキテクチャ: `docs/architecture.md`
- app_config 契約: `apps/platform/docs/app-config-schema.md`
