# AgentFlow

**AI エージェント開発のための基盤・プラットフォーム** — MCP / A2A / AG-UI / A2UI を統一インターフェースで扱う軽量フレームワーク。

**言語**: [English](README_EN.md) | [简体中文](README_ZH.md) | 日本語

---

## 1. 概要・特徴

AgentFlow は、複数プロトコルとエージェント協調を**一つの API 面**で扱える基盤です。業務向けには **3 Studio 製品線**（Migration Studio / Enterprise FAQ Studio / Computer Assistant Studio）で提供し、開発向けには Kernel（`agentflow`）と Plugin による拡張を前提としています。

| 特徴                  | 説明                                                                          |
| --------------------- | ----------------------------------------------------------------------------- |
| **8層アーキテクチャ** | アプリ・UI・フロー・Agent・ツール・Provider・プロトコル・インフラの責任分離   |
| **4プロトコル統一**   | MCP / A2A / AG-UI / A2UI を同一コードベースで利用                             |
| **3 Studio 製品線**   | 顧客導線を「テンプレート → 設定 → 実行 → 成果物」に統一                       |
| **開発方式の選択肢**  | `@agent` デコレータ / `create_flow` / AgentCoordinator で簡単〜高度まで対応   |
| **Engine パターン**   | SimpleEngine / PipelineEngine / GateEngine / RAGEngine / PEVEngine を配置即用 |
| **型安全・非同期**    | 100% 型アノテーション、async/await 前提の I/O                                 |
| **Skills 自動進化**   | 利用に応じて能力を拡張するプラグイン機構                                      |

---

## 2. 主な機能

- **Engine 実行**: `SimpleEngine`（単一 Agent）、`PipelineEngine`（多段・Review ループ）、`GateEngine`（入口審査）、`RAGEngine`（検索拡張）、`PEVEngine`（Plan-Execute-Verify）
- **Agent 定義**: `@agent` デコレータ、`AgentBlock` 継承、`AgentClient.get("名前").invoke(...)` による呼び出し
- **フロー構築**: `create_flow(...).gate(...).then(...).parallel(...).review(...).build()`
- **松結合 Provider**: `get_llm()` / `get_vectordb()` / `get_db()` / `get_embedding()` で環境に応じた実装を取得
- **チャネル**: 多プラットフォームメッセージ統合（MessageGateway / MessageChannelAdapter）
- **HITL**: 承認・中断・再開（ApprovalManager / Checkpointer / interrupt）
- **Context Engineering**: トークン予算、ターン圧縮、RetrievalGate、KeyNotes 等
- **組み込み Skills**: database-manager / stripe-payment / deployment-manager / auth-provider 等（オプション）

---

## 3. 技術アーキテクチャ

**8層構成**（上から下）: アプリケーション → UI → フロー → Agent → ツール → Provider → プロトコル → インフラ。上位は下位のみに依存し、契約は `agentflow/__init__.py` の公開 API を経由します。

**技術スタック**: Python 3.13+ / FastAPI / Pydantic / Uvicorn（バックエンド）、React・Vite・TypeScript（Studio / apps フロント）、MCP・A2A・AG-UI・A2UI（プロトコル）、PocketFlow 等（ワークフロー基盤）。品質は Ruff / mypy / pytest を標準としています。

---

## 4. 基盤・Platform・App の役割

| 層                            | 役割                                                                                                                                | 例                                                                                                |
| ----------------------------- | ----------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- |
| **Kernel（agentflow）**       | 安定した API・Engine・Provider・プロトコル抽象。拡張は Plugin First。副作用操作はポリシー・監査を経由。                             | `agentflow` パッケージ、公開 API                                                                  |
| **Platform（apps/platform）** | 3 Studio の実行導線（テンプレート→設定→実行→成果物）と Framework 管理 API。正規 API: `/api/studios/*`、`/api/studios/framework/*`。 | バックエンド `apps.platform.main`、フロント `apps/platform/frontend`                              |
| **Apps（apps/\*）**           | 製品・サンプルアプリ。Migration / FAQ / Assistant 等の Studio に対応する app や、オーケストレーション・メッセージング等の横断 app。 | `code_migration_assistant`、`faq_system`、`decision_governance_engine`、`market_trend_monitor` 等 |

対外説明は 3 Studio に揃え、プロトコル名や内部レイヤーは業務画面に露出しません。

---

## 5. クイックスタート・ドキュメント・ライセンス

**実行前**: 既定環境は `conda activate agentflow`。コマンド実行前に `code-rules/CLAUDE.md` および対象 app の README を確認してください。

```bash
conda activate agentflow
pip install -e ".[apps,dev]"
python -m apps.platform.main serve --port 8000
# 別ターミナル: cd apps/platform/frontend && npm install && npm run dev
```

- **ドキュメント**: 目次 [docs/index.md](docs/index.md)、対外 [docs/external/README.md](docs/external/README.md)、対内 [docs/internal/README.md](docs/internal/README.md)、3 Studio [docs/studios.md](docs/studios.md)
- **リポジトリ**: [GitHub](https://github.com/liushuang393/serverlessAIAgents) | [Issues](https://github.com/liushuang393/serverlessAIAgents/issues)
- **ライセンス**: [MIT License](LICENSE)

実行/訓練の分離やトレース設計の一部は [Microsoft Agent Lightning](https://github.com/microsoft/agent-lightning) の思想を参考にしています。
