# Legacy-to-Agent™ Enterprise Modernization Platform

> AI Agent がレガシーシステムを **理解 → 運用 → 改修** する新世代エンタープライズプラットフォーム
> **Framework Compliance**: AgentFlow 1.0.0 (AG-UI / A2A / Skills)

[![AgentFlow](https://img.shields.io/badge/Framework-AgentFlow-blue)](#)
[![Python](https://img.shields.io/badge/Python-3.11+-green)](#)
[![License](https://img.shields.io/badge/License-Apache%202.0-lightgrey)](#)

---

<!-- README_REQUIRED_SECTIONS_START -->
## 機能概要
- レガシー資産の解析、依存関係の可視化、移行候補の抽出を一体で実行。
- 段階移行（Strangler）前提で、変換コードと検証レポートをパッケージ化。
- HITL 承認フローと監査ログを通し、業務判断と技術判断を接続。

## 優位性
- 単純変換ではなく「業務語義 + コード変換」を同時に扱える。
- 移行後も Agent が運用継続を支援し、改善サイクルを維持できる。
- 証跡付き報告書を標準出力し、監査・説明責任にそのまま利用できる。

## 技術アーキテクチャ
- FastAPI 統合サーバー上で AgentFlow のオーケストレーションを実行。
- M1〜M5（摂取/語義/編排/生成/ガバナンス）の多層構成で責務分離。
- AG-UI / A2A / Skills 準拠で外部連携と拡張を標準化。

## アプリケーション階層
- Presentation: API / CLI / Dashboard。
- Orchestration: Migration Coordinator / HITL / Kill Switch。
- Domain Services: 解析・変換・成果物生成。
- Governance: 監査、ポリシー適合、証跡管理。
<!-- README_REQUIRED_SECTIONS_END -->

## Product Position

- `product_line`: `migration`
- `surface_profile`: `business`
- 顧客導線: `テンプレート選択 → データ/権限設定 → 実行 → 成果物確認`

## 🎯 なぜ Legacy-to-Agent™ か

従来のマイグレーションツールは「コードの変換」に留まります。
Legacy-to-Agent™ は AI Agent が**業務を理解**し、人間と協力して移行を実行します。

| 従来のアプローチ | Legacy-to-Agent™ |
|----------------|-------------------|
| ルールベースの機械的変換 | AI Agent による業務理解 + 変換 |
| 一括変換のみ | 段階的移行（Strangler 対応） |
| ブラックボックス | HITL 承認 + 監査ログ + 報告書 |
| 変換後は終了 | Agent が継続運用をサポート |
| 独自プロトコル | AgentFlow 標準 (AG-UI/A2A) |

---

## 🏗️ 5層アーキテクチャ

```
┌─────────────────────────────────────────────────────────┐
│  P: Presentation (AG-UI / CLI / API)                    │
├─────────────────────────────────────────────────────────┤
│  M3: Agent Orchestration                                │
│      ├─ CodeMigrationOrchestrator                       │
│      ├─ HITL ApprovalFlow                               │
│      └─ Kill Switch                                     │
├──────────────────┬──────────────────┬───────────────────┤
│  M1: Ingestion   │  M2: Semantics   │  M4: Generation   │
│  旧システム摂取   │  業務語義モデル    │  現代化コード生成  │
├──────────────────┴──────────────────┴───────────────────┤
│  M5: Governance & Compliance                            │
│      ├─ GovernanceEngine (監査)                         │
│      └─ compliance-reporter (報告書)                    │
└─────────────────────────────────────────────────────────┘
```

---

## 🛠️ 開発環境（インストール: 統一手順）

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

## 🚀 起動方法

本アプリケーションは自己完結型パッケージとして設計されています。
以下のコマンドでサーバーを起動してください。

```bash
cd apps/code_migration_assistant
./run_server.sh
```

ダッシュボードは `http://localhost:8003` でアクセス可能です。

### 起動スクリプト詳細

`run_server.sh` は以下の処理を実行します：

```bash
#!/bin/bash
# プロジェクトルートを自動検出
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"

export PYTHONPATH=$PYTHONPATH:$PROJECT_ROOT

# FastAPI サーバーを起動（ポート 8003）
cd "$PROJECT_ROOT"
exec uvicorn apps.code_migration_assistant.backend.app:app --host 0.0.0.0 --port 8003 --reload
```

**重要**: このスクリプトは **1つの統合サーバー** を起動します。前台と後台は分離していません。

---

## 🧪 テスト

```bash
cd <repo-root>
pytest apps/code_migration_assistant/tests -q
```

統一チェック（lint/type/test）をまとめて実行する場合:

```bash
cd <repo-root>
./check.sh all
```

## 📦 本番ビルド/発布（Platform に統一）

推奨手順（Platform CLI）:

```bash
conda activate agentflow
python -m apps.platform.main publish ./apps/code_migration_assistant --target docker
```

自動化する場合は Platform API（`POST /api/studios/framework/publish/deploy`）を使用します。

## 🏛️ 前後台アーキテクチャ（重要）

### 統合サーバー構成

このアプリケーションは **1つの FastAPI サーバー** で前台と後台を統合しています：

```
┌─────────────────────────────────────────────────────────┐
│  FastAPI Server (Port 8003)                             │
├─────────────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────────────────┐│
│  │ Frontend Layer (Static Files)                       ││
│  │ ├─ index.html (UI)                                  ││
│  │ ├─ app.js (Vanilla JavaScript)                      ││
│  │ └─ style.css (Styling)                              ││
│  │ [ブラウザで実行される]                                ││
│  └─────────────────────────────────────────────────────┘│
│                                                         │
│  ┌─────────────────────────────────────────────────────┐│
│  │ Backend API Layer (FastAPI)                         ││
│  │ ├─ REST Endpoints: /api/migration/execute           ││
│  │ ├─ WebSocket: /api/ws/{task_id}                     ││
│  │ └─ Static Files Server: /                           ││
│  │ [サーバーで実行される]                                ││
│  └─────────────────────────────────────────────────────┘│
│                                                         │
│  ┌─────────────────────────────────────────────────────┐│
│  │ Business Logic Layer (Background)                   ││
│  │ ├─ CodeMigrationEngine                              ││
│  │ ├─ 7-Stage Pipeline                                 ││
│  │ └─ Event Streaming via WebSocket                    ││
│  │ [バックグラウンドタスクで実行される]                  ││
│  └─────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────┘
```

### 通信フロー

```
1. ユーザーがブラウザで http://localhost:8003 にアクセス
   ↓
2. FastAPI が frontend/index.html を提供
   ↓
3. ブラウザが app.js を実行（Vanilla JavaScript）
   ↓
4. ユーザーが「Start Migration」をクリック
   ↓
5. app.js が POST /api/migration/execute を呼び出し
   ↓
6. backend/app.py が CodeMigrationEngine を初期化
   ↓
7. BackgroundTasks で engine._execute_stream() を実行
   ↓
8. app.js が WebSocket で /api/ws/{task_id} に接続
   ↓
9. エンジンがイベントを発行 → WebSocket で app.js に送信
   ↓
10. app.js がイベントを処理して UI を更新
```

### 重要な誤解を解く

**Q: 「真の後台？」**

**A: 以下の通りです：**

- **前台（Frontend）**: `frontend/app.js` - ブラウザで実行される Vanilla JavaScript
- **後台（Backend API）**: `backend/app.py` - FastAPI サーバー、REST API と WebSocket を提供
- **真の後台（Business Logic）**: `engine.py` の `CodeMigrationEngine` - バックグラウンドタスクで実行される

**起動スクリプト `run_server.sh` は後台（FastAPI）を起動します。**
前台はこの後台から静的ファイルとして提供されます。

つまり：
- ❌ 「後台が前台を起動する」ではなく
- ✅ 「後台が前台のファイルを提供し、前台が後台の API を呼び出す」

---

## 📁 ディレクトリ構造

AgentFlow の 8層アーキテクチャに基づき、Frontend/Backend を分離しています。

```
apps/code_migration_assistant/
├── backend/                 # Service Layer: API Server (FastAPI)
│   └── app.py               # Application Entry Point
├── frontend/                # UI Layer: Static Assets (HTML/JS/CSS)
│   ├── index.html
│   └── app.js
├── agents/                  # Agent Layer: Business Logic (A2A Ready)
├── adapters/                # Adapter Layer: Language Skills (COBOL/Java)
├── engine.py                # Engine Layer: Pipeline Orchestration
├── orchestrator.py          # Platform Layer: High-level Interface
├── docs/                    # Architecture Documentation
└── run_server.sh            # Portable Startup Script
```

---

## 📦 製品パッケージ

### Package A: Assessment（診断評価）

旧システムを診断し、移行計画と報告書を出力。コード変換は行わない。

```python
orchestrator = CodeMigrationOrchestrator()
result = await orchestrator.assess({"source_code": cobol_code})
# → 分析結果 + 業務モデル + 影響分析 + 報告書
```

### Package B: Modernization（完全移行）

7工程パイプラインで完全移行。HITL 承認 + 監査ログ付き。

```python
result = await orchestrator.modernize({
    "source_code": cobol_code,
    "run_tests": True,
})
# → 移行済みコード + テスト + 品質判定 + 報告書
```

### Package C: Agent Platform（持続運用）

移行後も AI Agent がシステム運用を継続支援。
(AgentCard による A2A 連携が可能)

```python
result = await orchestrator.platform_mode({
    "operation": "document",  # or "analyze", "status"
})
```

---

## 🔧 Skill 体系

| Skill | レイヤー | 役割 | depends_on |
|-------|---------|------|-----------|
| `code-analysis` | M1 | 静的解析・複雑度・依存 | ─ |
| `legacy-ingestion` | M1 | 旧システム摂取・AST | code-analysis |
| `business-semantics` | M2 | 業務フロー・ルール抽出 | legacy-ingestion |
| `cobol-migration` | M4 | COBOL→Java 変換 | ─ |
| `modernization-generator` | M4 | Spring Boot/REST/JPA 生成 | business-semantics, cobol-migration |
| `compliance-reporter` | M5 | 設計書・監査報告書 | business-semantics |

### Skill-First Capability Contract

```
analysis            -> legacy-ingestion
business_semantics  -> business-semantics
transform           -> modernization-generator
report              -> compliance-reporter

実行モード:
- skill_mode=skill_first: Skill 実行を優先し、失敗時は native fallback
- skill_mode=native_only: 既存 Agent 実装のみを使用
```

---

## 🛡️ ガバナンス & HITL

### HITL 承認ポイント

設計工程後に人間による承認を要求（HIGH リスク）:

```
分析 → 設計 → [HITL 承認] → 変換 → テスト → 検証 → 裁定 → 修正
```

### Kill Switch

```python
engine = CodeMigrationEngine()
engine.kill()  # 即時停止（次のチェックポイントで停止）
```

### 監査ログ

全操作が GovernanceEngine を経由して記録。
compliance-reporter Skill で日本語の監査報告書を生成。

---

##  対応言語

| ソース言語 | ターゲット言語 | ステータス |
|-----------|-------------|----------|
| COBOL | Java/Spring | ✅ 第一波本実装 |
| RPG (AS/400) | Java | 🔌 拡張インターフェースのみ |
| PL/I | Java | 🔌 拡張インターフェースのみ |
| Fortran | Java | 🔌 拡張インターフェースのみ |

---

## 🗓️ ロードマップ

- [x] 7工程固定パイプライン
- [x] 4言語ソースアダプター
- [x] MCP ツール体系
- [x] Skill-first capability + native fallback
- [x] HITL 承認ポイント
- [x] Kill Switch
- [x] GovernanceEngine 統合
- [x] 4新規 Skill (M1/M2/M4/M5)
- [x] 3製品パッケージ入口
- [x] **AgentFlow Framework Compliance (AG-UI/A2A)**
- [x] **Architecture Restructuring (Frontend/Backend)**
- [x] **Spring Boot 完全生成 (Controller/Service/Repository)**
- [x] **PDF 報告書出力 (reportlab 集約)**
- [x] **多言語対応 (日本語/英語/中国語 i18n)**
- [x] **Agent 工場モード完全実装 (platform_mode)**
- [ ] 継続的なセキュリティ脆弱性スキャン
- [ ] クラウドネイティブ移行最適化 (Terraform 生成)

---

## 🛠️ 設計の妥当性と拡張性

### 1. 変換の確実性 (Process Validation)
本プラットフォームは、単なる LLM による一括変換ではなく、**「7工程パイプライン」**によって各ステップでの品質を担保しています。
- **事実抽出 (LegacyAnalysis)**: LLM ではなく、専用のパーサー（PLV / AST）により100%正確な変数・ロジック構造を抽出します。
- **等価性検証 (Differential)**: 移行前後のコードを実際に実行（差分テスト）し、その挙動が一致するかを機械的に判定します。
- **HITL (Human-In-The-Loop)**: 設計段階で人間が介在し、AI の提案を修正・承認するため、最終成果物のコントロールが可能です。

### 2. 新規パターンの追加手順 (Extension Guide)
新しい言語ペア（例: RPG to C# や Struts to Spring Boot）を追加する場合、コアコード（Engine）を修正することなく、以下の配置のみで対応可能です。

#### ステップ 1: アダプターの実装
`adapters/source/` または `adapters/target/` に新しいアダプタークラスを作成します。
- **SourceAdapter**: 解析（AST生成）を担当。
- **TargetAdapter**: スケルトン生成とコンパイル・実行を担当。

#### ステップ 2: 設定の追加
`config/migration_types.yaml` に新しい移行タイプを登録します。
```yaml
- name: new-pattern
  source: { language: SOURCE, adapter: ... }
  target: { language: TARGET, adapter: ... }
  prompts: { transform: ... }
```

#### ステップ 3: プロンプトと型の定義
`config/prompts/` および `config/type_mappings/` に言語固有のルールを配置します。

---

## 🧠 学習連携（Agent Lightning backend）

既定では収集/学習は無効です。必要時にのみ opt-in してください。

```python
from apps.code_migration_assistant.lightning import create_lightning_engine_config
from apps.code_migration_assistant.engine import CodeMigrationEngine

config = create_lightning_engine_config(
    enable_collection=False,       # 既定: 収集しない
    enable_training=False,         # 既定: 学習しない
    enable_api_optimization=False, # 既定: 最適化しない
    backend="auto",  # auto|builtin|microsoft
)
engine = CodeMigrationEngine(config=config)
```

必要案件では「収集」と「訓練」を分けて実行します。

1. 実行（通常）

```python
result = await engine.run({"source_code": source_code})
```

2. 収集（必要時のみ）

```python
config = create_lightning_engine_config(
    enable_collection=True,
    enable_training=False,
    backend="auto",
)
engine = CodeMigrationEngine(config=config)
```

3. 訓練（別ジョブ）

```python
result = await engine.train_latest_run(apply_optimized_profile=True)
profile = engine.get_optimized_llm_profile()
```

Note:
- `backend="microsoft"` 指定時、ライブラリ未導入なら `strict_backend=False` で builtin fallback
- 外部I/F（Engine 入出力）は変更しません
- 実行フローから自動訓練を起動しないでください（運用分離）

---

## 🙏 謝辞

本アプリの実行/改善分離、およびトレース・報酬の設計改善は  
[Microsoft Agent Lightning](https://github.com/microsoft/agent-lightning) の思想とアーキテクチャを参考にしています。

## 共有テスト env 自動生成

```bash
conda run -n agentflow python scripts/bootstrap_test_env.py --env-file .env
```

- `CODE_MIGRATION_API_KEY_ENV` / `CODE_MIGRATION_API_KEY` / `CODE_MIGRATION_CORS_ORIGINS` を自動補完します。
- テスト用途では手動で鍵を発行する必要はありません。

## 本番運用と多租户招待メール

- 本番では API キーを Secret Manager 注入に切り替えてください（`.env` 配布禁止）。
- テナント招待時は、通知メールとログイン URL メールを分離送信してください。
- 詳細手順: `docs/internal/env-bootstrap-and-tenant-invite-security.md`
