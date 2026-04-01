# Decision Governance Engine

企業級意思決定支援システム - PipelineEngine パターンによる Multi-Agent システム

`product_line`: `framework` / `surface_profile`: `developer`

---

<!-- README_REQUIRED_SECTIONS_START -->

## 機能概要

- 企業意思決定を多段 Agent で評価し、提案・リスク・実行計画を統合出力。
- PASS/COACH モードで改善提案を反復し、却下一辺倒にならない運用を実現。
- REST / SSE / WebSocket で同一判断フローを提供し、UI と API を整合。

## 優位性

- PipelineEngine による責務分離で、判断過程の透明性と再現性を担保。
- COACH ループにより、意思決定品質を実運用で継続改善できる。
- レポート出力（PDF/API）まで一貫し、現場導入時の説明コストを下げられる。

## 技術アーキテクチャ

- Backend: FastAPI + AgentFlow PipelineEngine + AgentRegistry。
- Frontend: React/TypeScript + SSE/WebSocket 進捗表示。
- Core Flow: CognitiveGate → Gatekeeper → Dao/Fa/Shu/Qi → Review。

## アプリケーション階層

- Interface Layer: HTTP API / Stream / WebSocket。
- Orchestration Layer: Pipeline 実行制御。
- Agent Layer: 認知・分析・計画・レビュー。
- Output Layer: レポート生成・可視化・エクスポート。
<!-- README_REQUIRED_SECTIONS_END -->

## 1. システム概要

Decision Governance Engine は、企業の重要な意思決定を支援するマルチエージェントシステムです。
**BizCore の PipelineEngine パターン** を使用して実装されています。

### アーキテクチャ概要

```
┌─────────────────────────────────────────────────────────────┐
│  PipelineEngine (kernel/engines/pipeline_engine.py)         │
│  ┌─────────────────────────────────────────────────────────┐│
│  │ 前処理層: CognitiveGate（認知分析）→ Gatekeeper（門番） ││
│  │ 診断層: Clarification（問題精緻化）                     ││
│  │ 分析層: Dao → Fa → Shu → Qi（順次実行、依存チェーン）   ││
│  │ 検証層: ReviewAgent (COACH改善指導 / REVISE回退対応)    ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

**実行フロー（コーチング型インタラクティブ検証）**:

```
CognitiveGate → Gatekeeper → Clarification → Dao → Fa → Shu → Qi → Review
     ↓              ↓                                              ↓
   分析のみ      拦截判断                                    PASS / COACH
   (通過)      (不適格拒否)                                     ↓
                                                          ┌─────┴─────┐
                                                          │  PASS     │
                                                          │  → 承認   │
                                                          └───────────┘
                                                          ┌─────┴─────┐
                                                          │  COACH    │
                                                          │  課題あり │
                                                          └─────┬─────┘
                                                                ↓
                                              ┌─────────────────────────────────┐
                                              │  改善提案フェーズ                │
                                              │  各指摘に改善提案 + OK条件を提示│
                                              └─────────┬───────────────────────┘
                                                        ↓
                                              ┌─────────────────────┐
                                              │ ユーザーが認可/訂正  │◄──┐
                                              └─────────┬───────────┘   │
                                                        ↓               │
                                              ┌─────────────────────┐   │
                                              │ Agent個別再評価      │   │
                                              │ (該当指摘のみ再判定) │───┘
                                              └─────────┬───────────┘ まだNG
                                                        ↓ 全件OK
                                              ┌─────────────────────┐
                                              │ 再スコアリング       │
                                              │ → 最終 PASS         │
                                              └─────────────────────┘
```

> **設計思想**: REJECT（即却下）は廃止。課題が見つかっても「改善指導（COACH）」
> モードに入り、ユーザーと対話しながら提案を磨き上げる。
> これにより「認知を高めて提案を助ける」という本来のミッションを達成する。

### 前後端アーキテクチャ

```
┌─────────────────────────────────────────────────────────────┐
│  Frontend (React + TypeScript)                              │
│  - UI表示、進捗表示、ユーザー入力                            │
└─────────────────────────────────┬───────────────────────────┘
                                  │ HTTP / SSE
┌─────────────────────────────────▼───────────────────────────┐
│  Backend API (FastAPI)                                      │
│  - api.py → routers/ → DecisionEngine                       │
└─────────────────────────────────┬───────────────────────────┘
                                  │
┌─────────────────────────────────▼───────────────────────────┐
│  DecisionEngine (engine.py)                                 │
│  - PipelineEngine を継承                                    │
│  - run() / run_stream() API                                 │
└─────────────────────────────────┬───────────────────────────┘
                                  │
┌─────────────────────────────────▼───────────────────────────┐
│  各Agent (agents/*.py) + AgentRegistry                      │
└─────────────────────────────────────────────────────────────┘
```

### コア概念

| 層      | Agent              | 役割                                               |
| ------- | ------------------ | -------------------------------------------------- |
| 🧠 認知 | CognitiveGateAgent | 認知前処理（評価対象・意図・基準・不可逆性の特定） |
| 🚪 門番 | GatekeeperAgent    | 不適格な質問を門前払い（Gate判断）                 |
| 🔍 診断 | ClarificationAgent | 論理的穴・暗黙の仮定・認知バイアス特定             |
| 🎯 道   | DaoAgent           | 問題の本質抽出、因果齿轮分析                       |
| ⚖️ 法   | FaAgent            | 戦略パス評価（稳健型 vs 激进型）                   |
| 📋 術   | ShuAgent           | 具体的実行計画策定                                 |
| 🔧 器   | QiAgent            | 技術実装方針策定                                   |
| ✅ 検証 | ReviewAgent        | 全層検証・コーチング型改善指導（PASS/COACH）       |

### 技術スタック

**バックエンド:**

- Python 3.13+
- FastAPI（Web API）
- Pydantic v2（バリデーション）
- asyncio（非同期処理）

**フロントエンド:**

- React 18 + TypeScript
- Vite（ビルドツール）
- Tailwind CSS
- Zustand（状態管理）

---

## 2. 機能一覧

### 2.1 API エンドポイント

| エンドポイント                      | メソッド  | 説明                   |
| ----------------------------------- | --------- | ---------------------- |
| `/api/health`                       | GET       | ヘルスチェック         |
| `/api/agents`                       | GET       | Agent定義取得          |
| `/api/decision`                     | POST      | 同期的意思決定処理     |
| `/api/decision/stream`              | GET       | SSEストリーム付き処理  |
| `/ws/decision`                      | WebSocket | リアルタイム進捗通知   |
| `/api/report/{id}/pdf`              | GET       | PDFエクスポート        |
| `/api/report/{id}/components`       | GET       | A2UIコンポーネント取得 |
| `/api/report/{id}/agent/{agent_id}` | GET       | 個別Agent出力取得      |
| `/api/workflow/config`              | GET       | Studio UI設定取得      |

**PDFエクスポートの注意（重要）**

- `/api/report/{id}/pdf` の `{id}` は **UUID（request_id）** を推奨（履歴DBから復元できるため）
- 画面表示用の `report_id`（`PROP-...`）は提案書の案件IDであり、環境によっては永続化されない場合があります
- フロントエンドは `request_id` が取得できる場合、PDF出力に `request_id` を使用します

### 2.2 入力スキーマ

```python
# DecisionRequest（必須フィールド）
{
    "question": "意思決定の質問（10〜2000字）",
    "constraints": {
        "budget": {"amount": 500, "currency": "JPY"},
        "timeline": {"months": 6},
        "technical": ["Python", "AWS"],
        "regulatory": ["GDPR"]
    }
}
```

### 2.3 出力レポート

- エグゼクティブサマリー（30字以内の一文結論）
- 道（本質）分析結果
- 法（戦略）パス評価
- 術（計画）フェーズ定義
- 器（技術）実装方針
- 検証結果・署名欄

---

## 3. 使用方法

本システムは **Web UI（推奨）** と **CLI** の2つの利用方法を提供します。

| 方法      | 用途                                 | 対象ユーザー          |
| --------- | ------------------------------------ | --------------------- |
| 🖥️ Web UI | メイン利用方法、リアルタイム進捗表示 | CEO/CFO/CTO等の経営層 |
| ⌨️ CLI    | バッチ処理、スクリプト連携           | 開発者・自動化用途    |

---

## 4. 環境構築（新規セットアップ）

**詳細な手順は [インストールガイド](../../INSTALLATION_GUIDE_JA.md) を参照してください。**

### 4.1 前提条件

| 項目                 | 要件                                                                                                           |
| -------------------- | -------------------------------------------------------------------------------------------------------------- |
| Python               | 3.13以上                                                                                                       |
| Node.js              | 18以上                                                                                                         |
| パッケージマネージャ | pip, npm                                                                                                       |
| LLM Gateway          | `control_plane` の LLM Management（必要時のみ `.bizcore/llm_gateway.yaml` fallback、旧レガシー設定パスも互換） |
| API Key              | Platform 未設定時のみ fallback として利用                                                                      |

### 4.2 クイックセットアップ

```bash
# 1. プロジェクトルートで BizCore をインストール
pip install -e .

# 2. auth_service を先に起動（共有認証基盤）
cd shared/auth_service
conda run -n agentflow python scripts/compose.py publish

# 3. フロントエンド依存関係をインストール
cd ../../apps/decision_governance_engine/frontend
npm install

# 4. 環境変数を設定
cd ../../..
cat <<'EOF' > .env
PLATFORM_SECRET_MASTER_KEY=
AUTH_SERVICE_URL=http://localhost:8010
EOF

# 5. Gateway 設定を初期化（未作成時）
python -c "from infrastructure.llm.gateway import get_gateway_router; print(get_gateway_router())"
```

> 注意:
>
> - login を使う場合は `auth_service` を先に起動してください。
> - この app の正本は `app_config.json` の `contracts.llm` です。
> - Provider / model / API Key は `control_plane` の `LLM Management` で管理してください。
> - app 層は Provider 名を直接扱わず、契約から解決された model を利用します。

### DB マイグレーション（必須）

```bash
# DB コンテナ起動
cd apps/decision_governance_engine
docker-compose up -d postgres-main redis

# スキーマを最新化（新規DB作成 / 既存DB差分適用 を自動判定）
alembic -c apps/decision_governance_engine/alembic.ini upgrade head

# 現在の適用済みリビジョンを確認
alembic -c apps/decision_governance_engine/alembic.ini current

# マイグレーション履歴を一覧表示
alembic -c apps/decision_governance_engine/alembic.ini history --verbose

# 1つ前のリビジョンにロールバック
alembic -c apps/decision_governance_engine/alembic.ini downgrade -1

# 特定リビジョンまでロールバック
alembic -c apps/decision_governance_engine/alembic.ini downgrade <revision_id>

# 新しいマイグレーションファイルを自動生成（モデル変更後）
alembic -c apps/decision_governance_engine/alembic.ini revision --autogenerate -m "変更内容の説明"

# 空のマイグレーションファイルを手動作成
alembic -c apps/decision_governance_engine/alembic.ini revision -m "変更内容の説明"
```

> **補足:** 既存DBに `alembic_version` が無い場合（`create_all()` で作成済み等）、
> `upgrade head` 実行時に初期リビジョンが自動 stamp されるため、手動操作は不要です。

---

## 4. 開発環境（インストール: 統一手順）

この app 単体ではなく、リポジトリ全体の開発環境をセットアップします。

```bash
cd <repo-root>
bash setup_dev.sh
```

手動で行う場合:

```bash
conda activate agentflow
pip install -e ".[dev,apps]"
```

## 5. 起動手順

### 5.1 Web UI モード（推奨）

#### 方法1: Docker Compose（推奨）

```bash
# 共有 auth_service を先に起動
cd shared/auth_service
conda run -n agentflow python scripts/compose.py publish

# DGE を起動
cd ../../apps/decision_governance_engine
conda run -n agentflow python scripts/compose.py publish

# 開発モードで再起動
conda run -n agentflow python scripts/compose.py start

# ログ確認
docker compose -f docker-compose.yml -f docker-compose.dev.yml logs -f backend

# 停止
conda run -n agentflow python scripts/compose.py stop
```

**サービス構成:**

- `postgres-main`: PostgreSQL データベース（ポート 5432）
- `redis`: Redis キャッシュ（Docker 内部依存、host port 非公開）
- `backend`: FastAPI バックエンド（ポート 8001、ホットリロード有効）
- `frontend`: Vite 開発サーバー（ポート 5175）

**ブラウザでアクセス**

```
http://localhost:5175
```

login は `auth_service` の demo user を使います。

#### 方法2: ローカル環境（手動起動）

**前提条件:**

- auth_service が起動していること
- PostgreSQL が起動していること
- 環境変数 `DATABASE_URL` が設定されていること
- Redis は任意（未接続時はキャッシュ無効で継続）

**ターミナル1: バックエンドAPI起動**

```bash
# プロジェクトルートで実行

# 依存関係インストール
pip install -e ".[dev]"

# ローカル開発（ホットリロード有効）
# scripts/dev.py が 明示指定 > app_config.json の順で host / port を解決し、
# 共通 launcher 経由で uvicorn を起動する
AUTH_SERVICE_URL=http://localhost:8010 \
conda run -n agentflow python apps/decision_governance_engine/scripts/dev.py --reload

# 本番相当のローカル起動（リロードなし）
AUTH_SERVICE_URL=http://localhost:8010 \
conda run -n agentflow python apps/decision_governance_engine/scripts/dev.py --no-reload --workers 2
```

**ターミナル2: フロントエンド起動**

```bash
cd apps/decision_governance_engine/frontend

# 依存関係インストール
npm install

# 開発サーバー起動（ポート 5175）
npm run dev
```

**ブラウザでアクセス**

```
http://localhost:5175
```

### 5.2 動作確認

```bash
# APIヘルスチェック（ポート 8001）
curl http://localhost:8001/api/health
# 期待出力: {"status":"ok","version":"1.0.0"}

# Swagger UI（API仕様書）
open http://localhost:8001/docs
```

### 5.2.1 Login 動作確認

```bash
# auth_service の demo user を利用
curl -X POST http://localhost:8001/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"admin123"}'
```

期待値:

- `http://localhost:5175` の login 画面が表示される
- `/api/auth/login` が成功する
- `/api/auth/me` が認証済みユーザーを返す

### 5.3 CLI モード（開発者向け）

```bash
# 基本実行
python -m apps.decision_governance_engine.main "新規事業AとBのどちらに投資すべきか判断したい"

# 制約条件付き（予算500万円、期間6ヶ月）
python -m apps.decision_governance_engine.main "新規事業への投資判断をしたい" --budget 500 --timeline 6

# インタラクティブモード
python -m apps.decision_governance_engine.main --interactive
```

---

## 6. テスト手順

### 6.1 ユニットテスト実行

```bash
# Decision Governance Engine専用テスト
pytest tests/unit/test_decision_governance_engine.py -v

# カバレッジ付き
pytest tests/unit/test_decision_governance_engine.py -v \
  --cov=apps.decision_governance_engine \
  --cov-report=term-missing

# 特定テストクラスのみ
pytest tests/unit/test_decision_governance_engine.py::TestDecisionEngine -v
```

### 6.2 統合テスト

```bash
# APIサーバー起動後
pytest tests/integration/ -v -m "decision"
```

### 6.3 フロントエンドテスト

```bash
cd apps/decision_governance_engine/frontend
npm run test
npm run lint
npm run type-check
```

### 6.4 Lint/型チェック

```bash
# Ruff（リンター）
ruff check apps/decision_governance_engine/

# Mypy（型チェック）
mypy apps/decision_governance_engine/
```

---

## 7. 本番デプロイ手順

### 7.0 Platform から発布（推奨）

Platform（Control Plane）に publish/deploy を統一する場合:

```bash
conda activate agentflow
python -m control_plane.main publish ./apps/decision_governance_engine --target docker
```

（この app は `apps/decision_governance_engine/app_config.json` の `runtime.commands.publish` に compose wrapper の発布手順を保持しています）

### 7.1 ビルド

```bash
# フロントエンドビルド
cd apps/decision_governance_engine/frontend
npm run build

# 静的ファイルは dist/ に出力される
```

### 7.2 本番相当の単体起動

```bash
conda run -n agentflow python apps/decision_governance_engine/scripts/dev.py --no-reload --workers 4
```

### 7.3 Docker（推奨）

```bash
# app_config.json の host / port を使って compose wrapper から起動
cd apps/decision_governance_engine
conda run -n agentflow python scripts/compose.py publish
```

### 7.4 本番チェックリスト

- [ ] 環境変数（APIキー等）が正しく設定されている
- [ ] CORS設定が本番ドメインに限定されている
- [ ] ログレベルがINFO以上に設定されている
- [ ] ヘルスチェックエンドポイントが応答する
- [ ] SSL/TLS証明書が設定されている（HTTPS）

---

## 8. ディレクトリ構成

```
apps/decision_governance_engine/
├── __init__.py              # パッケージ初期化（DecisionEngine エクスポート）
├── engine.py                # DecisionEngine（PipelineEngine 継承）★メインエントリーポイント
├── main.py                  # CLIエントリーポイント
├── api.py                   # FastAPI REST API（engine.py を使用）
├── flow_config.py           # 起動時設定（後方互換用）
├── startup.py               # 起動情報ログ
├── agents/                  # 各Agentの実装
│   ├── agent_definitions.yaml   # Agent定義（YAML）
│   ├── cognitive_gate_agent.py  # 認知前処理
│   ├── gatekeeper_agent.py      # 門番
│   ├── clarification_agent.py   # 診断
│   ├── dao_agent.py             # 道（本質分析）
│   ├── fa_agent.py              # 法（戦略評価）
│   ├── shu_agent.py             # 術（計画策定）
│   ├── qi_agent.py              # 器（技術実装）
│   └── review_agent.py          # 検証（PASS/REVISE/REJECT）
├── schemas/                 # Pydanticスキーマ
├── services/                # ビジネスロジック
│   ├── agent_registry.py        # Agent一元管理
│   ├── report_generator.py      # レポート生成
│   └── ui_components.py         # A2UIコンポーネント生成
├── routers/                 # FastAPIルーター
├── prompts/                 # プロンプトテンプレート
├── skills/                  # SKILL.md定義
├── knowledge/               # 知識ベース（RAG用）
└── frontend/                # React フロントエンド
```

**重要**:

- **メインエントリーポイント**: `engine.py` の `DecisionEngine` クラス
- **API エントリーポイント**: `api.py`（内部で `engine.py` を使用）
- **CLI エントリーポイント**: `main.py`（内部で `engine.py` を使用）

---

## 9. トラブルシューティング

### よくある問題

**Q: `ModuleNotFoundError: No module named 'apps'`**

```bash
# プロジェクトルートから実行するか、PYTHONPATHを設定
export PYTHONPATH="${PYTHONPATH}:$(pwd)"
```

**Q: APIキーエラー**

```bash
# Platform 未設定時の fallback key を確認
echo $OPENAI_API_KEY
# .envファイルを確認
cat .env
```

**Q: ポート8000が使用中**

```bash
# app_config.json の値を使って起動する
conda run -n agentflow python apps/decision_governance_engine/scripts/dev.py --reload
```

---

## 10. 関連ドキュメント

- [インストールガイド](../../INSTALLATION_GUIDE_JA.md) - 詳細なセットアップ手順
- [設計仕様書](design/decision-agent-spec.md)
- [変更履歴](CHANGELOG_v2.0.md) / [CHANGELOG_v3.0.md](CHANGELOG_v3.0.md)
- [AgentFlow メイン README](../../README.md) - フレームワーク全体の説明

## 11. 共有テスト env 自動生成

```bash
conda run -n bizcore python scripts/bootstrap_test_env.py --env-file .env
```

- `POSTGRES_PASSWORD` と `SESSION_SECRET` を含むテスト用値を自動補完します。
- 手動で共通テスト鍵を作成せず、空値補完のみで運用してください。

## 12. 本番運用と多租户招待メール

- 本番では `.env` を配布せず、Secret Manager 注入を使用してください。
- DB 初期化時に tenant 分離情報を先に作成し、監査ログを有効化してください。
- 招待メールは通知と URL を分離送信してください。
- 詳細手順: `docs/internal/env-bootstrap-and-tenant-invite-security.md`
