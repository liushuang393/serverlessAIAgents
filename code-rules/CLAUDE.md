# 📌 AgentFlow Code Rules インデックス

> **プロジェクト**: AgentFlow - MCP/A2A/AG-UI/A2UI 統一インターフェース AI エージェントフレームワーク
> **バージョン**: 1.0.0
> **最終更新**: 2026-01-19
> **適用範囲**: AgentFlow 全 Python コードベース

## 🧠 全体方針（Global）

- [開発原則](global/principles.md)
- [命名規則](global/naming-guidelines.md)
- [スタイル & フォーマット](global/style-formatting.md)
- [例外/エラー処理](global/error-handling.md)
- [AI弱点補強](global/ai-weakness.md)

## 🧠 言語別ルール

- [Pythonルール](languages/python-rules.md)

## 🧠 プロジェクト固有

- [アーキテクチャ設計](project/architecture.md)
- [Agent Lightning 統合規約](project/agent-lightning-integration.md)
- [リポジトリ構造](project/repo-structure.md)
- [CI/CDガイドライン](project/ci-cd-guidelines.md)
- [ファイル管理規約](project/file-management.md)

## 🧠 AgentFlow固有指針

- [AgentFlow Python](company-specific/agentflow-python.md)

## 🧠 使用ツール

- [リンター & フォーマッター](tools/lint-format.md)
- [テスト方針](tools/testing.md)

---

## 🎯 AgentFlow プロジェクト概要

**AgentFlow** は MCP / A2A / AG-UI / A2UI の 4 プロトコルを統一インターフェースで扱う軽量 AI エージェント開発フレームワークです。

### 外部提供優先ルール（2026-02）

- 対外説明は `Migration Studio / Enterprise FAQ Studio / Computer Assistant Studio` の 3 主線を優先する。
- `business` surface ではプロトコル・内部層詳細を露出しない。
- 副作用を伴う操作は必ずポリシーと監査を経由する。
- ドキュメントは `docs/external/`（対外）と `docs/internal/`（対内）に分離して管理する。
- Platform API は `/api/studios/*` と `/api/studios/framework/apps/*` を正規経路とし、`/api/apps/*` を再導入しない。

### 主要特徴
- **8層アーキテクチャ**: アプリ・UI・フロー・Agent・ツール・Provider・プロトコル・インフラの明確分離
- **統一プロトコル**: 4つの標準プロトコルを1つのAPIで利用
- **非同期優先**: すべてのI/Oをasync/await前提で設計
- **型安全**: 100%型アノテーション、mypyとRuffを標準化
- **Skills自動進化**: 使えば使うほど賢くなる自動学習システム

### 開発方式
- **@agentデコレータ**: 1行でAgent定義、設定ゼロ
- **create_flow**: 宣言的チェーンAPI、Gate/Review/並行実行
- **AgentCoordinator**: 完全制御、高度な協調パターン
- **Engine Pattern**: 配置即用、4種類の予定義パターン

### 技術スタック
- **言語**: Python 3.13+
- **アーキテクチャ**: 8層クリーンアーキテクチャ
- **プロトコル**: MCP, A2A, AG-UI, A2UI
- **インフラ**: Supabase/PostgreSQL/Turso, Pinecone/Qdrant, Redis
- **品質**: Ruff, mypy, pytest, 92% カバレッジ

### 品質基準
- **型安全**: 100% 型アノテーション必須
- **テスト**: pytest + カバレッジ 80% 以上（目標 90%）
- **フォーマット**: Ruff 統一（format + lint）
- **ドキュメント**: Google スタイル Docstring

---

## 📋 ルール適用ガイド

### 優先順位
1. **プロジェクト固有ルール** - AgentFlow のアーキテクチャ特性を反映
2. **言語別ルール** - Python 生態系のベストプラクティス
3. **グローバルルール** - 普遍的な開発原則

### 自動化チェック
```bash
# コミット前必須チェック
ruff format .                    # フォーマット
ruff check .                     # リント
mypy agentflow                   # 型チェック
pytest --cov=agentflow --cov-fail-under=80  # テスト
```

### AI生成コードの品質保証
- **仕様明文化テンプレート**強制
- **100%テストカバレッジ**要求
- **命名意図検証**と**エラー処理完全性確認**

---

## 🔍 コンプライアンス確認

### 自動化チェックリスト

#### コミット前チェック (Pre-commit)
- [ ] `ruff format .` - コードフォーマット
- [ ] `ruff check .` - リントチェック
- [ ] `mypy agentflow` - 型チェック
- [ ] `pytest --cov=agentflow --cov-fail-under=80` - テスト実行
- [ ] `pre-commit run --all-files` - 全自動チェック

#### マージ前チェック (Pre-merge)
- [ ] すべての自動化チェック通過
- [ ] 最低 1 名のレビュー承認
- [ ] 関連ドキュメント更新済み
- [ ] 破壊的変更は移行ガイドを添付

#### リリース前チェック (Pre-release)
- [ ] すべての統合テスト通過
- [ ] セキュリティスキャン完了
- [ ] パフォーマンス/ストリーミング動作検証
- [ ] ロールバック手順確認

### 品質ゲート

| 段階 | 品質基準 | 責任者 |
|------|---------|--------|
| **コードコミット** | Ruff/Mypy/Tests 通過 | 開発者 |
| **機能完了** | 統合テスト、ドキュメント更新 | 機能責任者 |
| **リリース準備** | CI/CD 全通過、セキュリティスキャン | リリース担当 |

各ルールファイルには具体的なチェック項目と自動化スクリプトが記載されています。

# グローバル設定（AI 共通ルール）

## 基本原則
- AI は補助的意思決定者であり、最終判断は行わない
- 不確実な場合は必ず明示する
- 推測・捏造は禁止する

## 言語設定
- 回答は日本語で行う
- コードコメントは日本語を使用する
- 顧客向け文書・既定AIプロンプトは日本語を標準とする
- 中国語は `README_ZH.md` と `**/locales/zh*.json`、および多言語機能維持に必須な判定語彙/正規表現/テストのみ許可する（`i18n-zh-keep`）

## 出力規則
- 原則として構造化された形式で出力する
- 複雑な内容は段階的に説明する
- 出力の最後に次のアクションを示す

## 開発環境（必須）

| 項目 | バージョン | 備考 |
|------|-----------|------|
| **Python 環境** | conda activate agentflow | **必ず conda 環境を使用** |
| Python | 3.13+ | conda agentflow 内 |
| Node.js | 22+ LTS | フロントエンド用 |
| npm | 11+ | フロントエンド用 |
| Docker | 29+ | App 発布/起動/停止用 |
| Docker Compose | v5+ | App ライフサイクル管理 |
| FastAPI | 0.123+ | バックエンド |
| Pydantic | 2.12+ | スキーマ定義 |
| Uvicorn | 0.40+ | ASGI サーバー |

### サーバー起動コマンド

```bash
# バックエンド（ローカル開発 ポート 8001）
conda activate agentflow
python -m apps.platform.main serve --port 8001

# フロントエンド（別ターミナル）
cd apps/platform/frontend
npm run dev
```

> **注意**: ユーザーの実行中プロセス（サーバー等）を勝手に kill しないこと。

## 環境設定管理（必須）
- **host / port / API URL 等の環境依存値はソースコードにハードコード禁止**
- 環境変数（`.env`）または設定ファイルで一元管理する
- ローカル開発・テスト・本番は環境別ファイル（`.env` / `.env.test` / `.env.production`）で分離する
- 本番環境はテスト環境と可能な限り同一構成にする
- `.env.example` を変更したら必ず `.env` も同期する
- LLM プロバイダーは `LLM_PROVIDER` で明示指定する（`auto` 常用を避ける）
- `app_config.json` を持つ App のポートは `app_config.json` を単一定義元とし、`<APP>_HOST/PORT` は一時上書き用途に限定する
- 詳細: [開発原則 - 環境設定管理](global/principles.md#環境設定管理environment-configuration)

## モジュラー設計ルール
## 基本原則

**すべてのソースファイルは1000行未満に保つこと。**

## ファイルサイズ制限

| ファイル種別 | 推奨行数 | 最大行数 |
|-------------|---------|---------|
| コンポーネント/クラス | 200-300行 | 500行 |
| ユーティリティ/ヘルパー | 100-200行 | 300行 |
| 設定ファイル | 50-100行 | 200行 |
| テストファイル | 300-500行 | 800行 |
| **絶対上限** | - | **1000行** |

## 分割基準

ファイルが以下の条件に該当する場合、分割を検討すること：

1. **行数超過**: 500行を超えた時点で分割を計画
2. **責務過多**: 1ファイルに3つ以上の責務が混在
3. **依存過多**: インポート文が20行を超える
4. **関数過多**: 1ファイルに10個以上の公開関数/メソッド
## 実装時の指示

### コード生成時
- 新規ファイル作成時は300行以内を目標とする
- 500行を超える場合は分割案を提示してから実装
- 1000行を超えるファイルは絶対に作成しない

### リファクタリング時
- 既存の大規模ファイルを発見したら分割を提案
- 分割時は既存の動作を壊さないよう段階的に実施
- テストを先に書いてから分割作業を行う

## 安全・責任
- 違法・侵害行為に関わる出力は禁止
- 承認・署名・契約行為を代行しない


---

# AgentFlow プロジェクト開発規約・設計ガイド（code-rules統合版）

# AgentFlow プロジェクト開発規約・設計ガイド（外部公開インターフェース版）

最終更新: 2026-02-11  
対象: `agentflow/` を利用して Agent / Workflow / Frontend 連携を設計・実装する開発者

---

## 1. 目的と適用範囲

本ガイドは、`agentflow` プロジェクトを実装基準として再整理し、次を統一する。

- プロジェクト開発規約（設計・実装・運用）
- 対外公開する抽象インターフェース（Public API）
- シナリオ別の Engine / Workflow 組み合わせ指針
- フロントエンドとバックエンド Agent の連携方法
- 進捗追跡、HITL（人間参加）、Agent 間連携パターン

このガイドの API 契約上の正本は `agentflow/__init__.py` とし、各サブモジュールは実装詳細とみなす。

---

## 2. アーキテクチャ規約

### 2.1 レイヤ分離

- 上位層は下位層にのみ依存する。
- `apps/` の業務ロジックを `agentflow/` の再利用可能層へ昇格させる場合、公開契約は `agentflow/__init__.py` に再エクスポートする。
- UI 層から直接内部実装（例: `agentflow.flow.executor`）を参照しない。公開 API を経由する。

### 2.2 非同期・I/O

- Agent/Engine の I/O 経路は async-first を徹底する。
- 長時間処理は `run_stream()` を標準入口とし、SSE/WS で中間状態を返す。

### 2.3 契約優先

- イベントは ad-hoc dict ではなく `agentflow/protocols/agui_events.py` のモデルを優先する。
- 進捗イベントは `current` `total` `percentage` を必須とする。

---

## 3. 対外抽象インターフェース（Public API）

## 3.1 Agent 定義

1. デコレータ方式（最短）
   - `@agent`
   - 呼び出し: `AgentClient.get("AgentName").invoke(...)`
2. 基底クラス方式（明示）
   - `AgentBlock` 継承
   - `run(self, input_data: dict[str, Any]) -> dict[str, Any]`
3. 高信頼方式（型安全・回復性）
   - `ResilientAgent[...]` 継承

```python
from agentflow import agent, AgentClient

@agent
class QAAgent:
    system_prompt = "あなたは簡潔に回答するアシスタントです"

    async def process(self, input_data: dict) -> dict:
        return {"answer": "ok"}

result = await AgentClient.get("QAAgent").invoke({"question": "..."})
```

## 3.2 Engine 実行契約

共通インターフェース:

- `await engine.run(inputs, thread_id=...) -> dict`
- `async for event in engine.run_stream(inputs): ...`

主要 Engine:

- `SimpleEngine`: 単一 Agent
- `GateEngine`: 事前判定 + 本処理
- `PipelineEngine`: 多段 + Review ループ
- `RAGEngine`: 検索拡張
- `PEVEngine`: Plan-Execute-Verify

設定契約:

- `EngineConfig`
- `HITLEngineConfig`（`enabled` `checkpointer` `interrupt_before/after` など）

## 3.3 Flow DSL（低レベル協調）

- `create_flow("flow-id")`
- `.gate(...)`
- `.then(...)`
- `.parallel(...)`
- `.review(...)`
- `.build()`

Flow を使う場合も最終的な API 提供面は Engine で包むことを推奨する。

## 3.4 通信・プロトコル

- AG-UI（SSEイベント）: `FlowStartEvent` `NodeStartEvent` `ProgressEvent` `FlowCompleteEvent`
- A2A（Agent間HTTP）: `A2AClient` / `A2AServer` / `AgentCard`
- MCP（ツール接続）: `MCPClient` またはツールレジストリ経由
- API層: `APIResponse` `SSEEmitter` `WebSocketHub` `create_agent_router`

## 3.5 HITL

- 中断: `interrupt(...)`（`InterruptSignal` を発火）
- 再開指示: `Command` / `CommandType`
- 承認管理: `ApprovalManager`
- 永続化: `Checkpointer`（`Memory` / `Redis` / `Postgres`）

---

## 4. Agent 設計規約

### 4.1 入出力設計

- 入力は必須項目を明確化し、Pydantic で境界バリデーションする。
- 出力は次段 Agent が参照しやすい key 名で返す（例: `{stage}_result` 互換）。
- レポート系は最終段に集約し、途中段では生データと根拠を保持する。

### 4.2 ライフサイクル

- `initialize()` で外部接続を作成。
- `cleanup()` で確実に破棄。
- `run()` 内で重い初期化を毎回行わない。

### 4.3 失敗戦略

- 例外を握りつぶさず、境界で意味のあるエラーへ変換する。
- Gate 失敗・Review COACH（旧 REJECT）・REVISE を仕様として分離する。

### 4.4 返回値バリデーション（必須）

- **裸の `Enum(value)` 変換は禁止**。必ず `safe_enum()` を経由する。
- Agent 出力の Enum フィールドは `safe_enum(EnumCls, value, default, aliases=...)` で変換する。
- `aliases` で後方互換マッピングを明示する（例: `{"REJECT": "COACH"}`）。
- 未知の値はクラッシュせず、フォールバックデフォルトを返しログに警告を出力する。
- LLM 出力は揺れるため、`.strip().upper()` で正規化してからパースする。

```python
# ❌ 禁止: 裸の Enum 変換（未知値でクラッシュ）
verdict = ReviewVerdict(verdict_str)

# ✅ 必須: safe_enum 経由（フォールバック + エイリアス付き）
from agentflow.core.type_safe import safe_enum
verdict = safe_enum(
    ReviewVerdict, verdict_str, ReviewVerdict.REVISE,
    aliases={"REJECT": "COACH"},
)
```

---

## 5. Workflow / Engine 設定指針

## 5.1 Engine 選定マトリクス

| 要件 | 推奨 |
|---|---|
| 単一質問応答 | `SimpleEngine` |
| 入口審査が必要 | `GateEngine` |
| 多段分析・差戻し | `PipelineEngine` |
| 知識検索前提 | `RAGEngine` |
| 計画/実行/検証分離 | `PEVEngine` |

## 5.2 Pipeline ステージ規約

- `name` は固定 ID とし、UI 表示名と分離する。
- `gate: true` は入口制御に限定する。
- `review: true` は最終品質判定に限定する。
- `retry_from` は再実行開始点を明示する。
- 依存チェーンがない場合のみ `parallel: true` を使う。

## 5.3 推奨 EngineConfig（目安）

- 開発: `timeout_seconds=300`, `enable_events=True`
- 本番API: `enable_events=True`, `hitl.enabled` をユースケースで有効化
- 長時間バッチ: `timeout_seconds` を業務SLOに合わせ増加

---

## 6. フロントエンドとバックエンド Agent 連携

## 6.1 推奨通信分担

- REST (`run`): 即時応答が必要な同期処理
- SSE (`run_stream`): 進捗・思考ログ・最終結果
- WebSocket: 双方向操作（承認、購読、リアルタイムコマンド）

## 6.2 SSE イベントライフサイクル

標準遷移:

1. `connection.established`（任意）
2. `flow.start`
3. `node.start` / `progress` / `log`
4. `node.complete`（または `node.error`）
5. `flow.complete` または `flow.error`

互換対応:

- 既存実装に `type` ベースイベントがあるため、フロントで `event_type` へ正規化する。

## 6.3 画面状態管理の要点

- `isConnected` `isComplete` `error` `retryCount` を分離管理する。
- `request_id` / `result_id` を保持し、PDF出力や履歴取得に再利用する。
- 接続タイムアウトと指数バックオフ再接続を実装する。

---

## 7. 進捗追跡の実装規約

- 進捗UIは `ProgressEvent` の `current/total/percentage` を一次情報とする。
- ノード単位の状態は `node.start` / `node.complete` / `node.error` で更新する。
- `log` イベントは「判断理由」と「次アクション」を含める。
- Gate拒否やReview判定は専用イベント（例: `early_return`, `review_verdict`）で明示する。

---

## 8. 人間参加（HITL）設計

## 8.1 基本原則

- `interrupt()` 前に非冪等操作を実行しない。
- 割り込み時の状態は Checkpointer に保存する。
- 承認期限を必ず設定し、失効時のデフォルト動作を定義する。

## 8.2 API 設計

- 承認一覧: `GET /hitl/requests`
- 承認: `POST /hitl/requests/{id}/approve`
- 拒否: `POST /hitl/requests/{id}/reject`

## 8.3 監査

- 承認要求、承認/拒否、タイムアウト、再開を全て監査ログ化する。
- リスク操作（削除、外部公開、金額確定）は強制承認にする。

---

## 9. Agent 間連携パターン

## 9.1 直列受け渡し（最頻出）

- `PipelineEngine` で各段結果を次段にマージ。
- 段ごとに責務を1つに限定。

## 9.2 並列集約

- 同一入力から複数 Agent を同時実行し、統合段でマージ。
- 並列段後は統合ルール（優先順、重複解消）を明示する。

## 9.3 評価ループ（REVISE）

- `Review` で `PASS/REVISE/COACH` を返す。
- `REVISE` は `retry_from` を起点に再分析。

## 9.4 リモート委譲（A2A）

- 自 Agent にない能力は `A2AClient` で外部 Agent へ委譲。
- `AgentCard` をキャッシュし、失敗時は指数バックオフで再試行。

---

## 10. シナリオ別の組み合わせ例

### 10.1 FAQ/チャット

- 構成: `SimpleEngine` + `@agent`
- 追加: 必要時のみ `RAGEngine` へ昇格

### 10.2 入力審査付き業務処理

- 構成: `GateEngine`
- 入口不適合は早期返却し、本処理コストを節約

### 10.3 意思決定支援（多段 + 人間確認）

- 構成: `PipelineEngine` + `Review` + `HITL`
- UI: SSEで進捗、RESTで承認/履歴/PDF

### 10.4 生成デザインパイプライン

- 構成: `PipelineEngine`（Intent分析 → Prompt計画 → 実行）
- 外部連携: ComfyUI/OpenAI画像生成クライアント

### 10.5 市場監視・通知

- 構成: 収集 → 分析 → レポート → 通知 の Pipeline
- 追加: RedTeam/SignalScorer を中段に挿入

---

## 11. ベストプラクティス

1. Public API 以外を直接外部公開しない。  
2. ステージ名は固定、表示名はUIで管理する。  
3. SSEイベントは「機械可読」と「人間可読」の両方を満たす。  
4. `request_id` を全レイヤで引き回し、再現性を確保する。  
5. HITL は「承認理由」と「影響範囲」を必須入力にする。  
6. Agent 間I/Oは小さく保ち、巨大データはストア参照に切り替える。  

---

## 12. アンチパターン

- `{"type": "...", "data": ...}` のみで独自イベントを乱立する。
- 1 Agent に複数責務（収集 + 分析 + 出力）を同居させる。
- Review 判定を曖昧文字列で返し、分岐規則を持たない。
- **裸の `Enum(value)` を使い、未知値でクラッシュする**（必ず `safe_enum()` を使う）。
- SSE だけで承認コマンドまで処理しようとする（双方向はWS/RESTを使う）。
- `interrupt()` 前にDB更新を行い、再開時に二重実行を起こす。

---

## 13. 新規実装の完了条件（DoD）

1. Engine選定理由が明文化されている。  
2. Agent I/O スキーマと失敗時契約が定義済み。  
3. SSEイベントとフロント状態遷移の対応表がある。  
4. HITL必要箇所に承認経路とタイムアウト方針がある。  
5. ユニットテスト（正常/拒否/例外/再試行）が揃っている。  
6. 公開対象は `agentflow/__init__.py` 経由で参照可能。  
