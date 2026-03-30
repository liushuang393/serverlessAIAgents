# 📌 AgentFlow Code Rules — Layer 1 詳細参照版

> **⚠️ このファイルは「詳細参照版」です。**
> 常時ロードされる Layer 0 ルールは **ルート `AGENTS.md` / `CLAUDE.md`** に記載されています。
> ルート `AGENTS.md` と `CLAUDE.md` は同期ミラーです。このファイルはそれらの索引に従い、該当タスクが発生したときに読んでください。

> **プロジェクト**: AgentFlow - MCP/A2A/AG-UI/A2UI 統一インターフェース AI エージェントフレームワーク
> **バージョン**: 1.3.0
> **最終更新**: 2026-02-28
> **適用範囲**: AgentFlow 全 Python コードベース

---

## 🚨 CRITICAL: Python 型エラー 最重要警告（全 AI エージェント必読）

> **AIへの最優先指示**: Python の型エラーはこのプロジェクトで最も深刻かつ頻発する問題です。
> コードを生成・編集するたびに以下の表を照合し、違反パターンがゼロであることを確認してください。

### AI が最も犯しやすい Python 型エラー（絶対禁止）

| #   | ❌ 違反パターン                          | ✅ 正解                                    | 理由                     |
| --- | ---------------------------------------- | ------------------------------------------ | ------------------------ |
| 1   | 型アノテーションなし `def fn(x):`        | `def fn(x: str) -> None:`                  | mypy エラー直行          |
| 2   | `Optional[X]` を None チェックなしで参照 | `if value is not None:` ガード後にアクセス | AttributeError 原因      |
| 3   | `Any` をデフォルト型として使う           | 具体型を明示（理由コメント必須）           | 型安全を無効化           |
| 4   | 戻り値型と実際の `return` 値が不一致     | 宣言型と一致する値を返す                   | mypy エラー & 実行時バグ |
| 5   | `dict["key"]` で KeyError リスク         | `.get("key")` または存在確認必須           | 実行時クラッシュ         |
| 6   | `cast()` で型を誤魔化す                  | 実際に正しい型の値を生成する               | 型チェック回避           |
| 7   | `# type: ignore` を理由なく使う          | 根本原因を修正する                         | 問題先送り厳禁           |
| 8   | Pydantic フィールドに `Any` / 型なし     | `field: str`, `field: int \| None` 等      | バリデーション無効化     |
| 9   | `list[str]` に異なる型を append          | `list[str \| int]` 等で型を拡張            | TypeError 原因           |
| 10  | `async def` の戻り値型を省略             | `async def fn() -> dict[str, Any]:`        | 非同期でも型必須         |

### コード生成・編集時の必須チェックリスト（毎回実施）

```
□ 全引数・戻り値に型アノテーション付与済みか？
□ None を返す可能性がある場合 `T | None` を宣言済みか？
□ None の可能性がある変数を None チェックなしに参照していないか？
□ mypy --strict 相当でエラー 0 になるか（頭の中でシミュレート）？
□ Pydantic モデルの全フィールドに型が正確に定義されているか？
□ dict アクセスは KeyError リスクを回避しているか？
□ cast() / # type: ignore を使っていないか？
```

詳細パターン集 → [AI弱点補強 - Python型エラー](global/ai-weakness.md#python型エラー最重要最頻出)
型アノテーション規約 → [Pythonルール - 型エラー頻出パターン](languages/python-rules.md#python型エラー頻出パターン厳重注意)

---


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
- [依存関係管理・Dependabot](project/dependency-management.md)
- [ファイル管理規約](project/file-management.md)

## 🧠 AgentFlow固有指針

- [AgentFlow Python](company-specific/agentflow-python.md)
- **Agent 登録標準（app_config.json agents[]）** → 本ファイル セクション 15（Agent 追加時は必読）

## 🧠 使用ツール

- [リンター & フォーマッター](tools/lint-format.md)
- [テスト方針](tools/testing.md)
- [アーキテクチャ検証ツール](tools/architecture-validation.md)
- [check-all エラー分類と対処](tools/check-errors-analysis.md)（型エラー解消時の参照）
- [Mypy 回避パターン（AI 向け）](global/mypy-avoid-patterns.md)（コード生成時に必須）

---

## ✅ コミット前チェック（必須）

```bash
./check.sh all
./check.sh all --no-type-check
conda run -n agentflow python scripts/check_layer_boundaries.py
conda run -n agentflow python scripts/check_no_direct_provider_calls.py
conda run -n agentflow python scripts/check_app_compliance.py
conda run -n agentflow python scripts/check_rules_compliance.py
```

- まずは `./check.sh all` を正本とする。
- 型エラー解消中のみ `./check.sh all --no-type-check` を使う。
- 境界や規約だけを個別確認したい場合は下のスクリプトを `conda run -n agentflow` 経由で実行する。

---

## 🎯 AgentFlow フレームワーク概要

- **プロトコル**: MCP / A2A / AG-UI / A2UI を統一インターフェースで提供
- **依存方向**: `apps → control_plane → domain → harness → kernel → shared → infrastructure → contracts`（逆依存禁止）
- **非同期優先**: すべての I/O を `async/await` で設計。長時間処理は `run_stream()` + SSE/WS
- **API 正本**: `contracts/`（型定義）+ `kernel/__init__.py`（re-export）
- **外部 API 正規経路**: `/api/studios/*` と `/api/studios/framework/apps/*`（`/api/apps/*` 再導入禁止）

### Engine 選定マトリクス

| 要件                   | 推奨 Engine        |
| ---------------------- | ------------------ |
| 単一質問応答           | `SimpleEngine`     |
| 入口審査が必要         | `GateEngine`       |
| 多段分析・差戻し       | `PipelineEngine`   |
| 知識検索前提           | `RAGEngine`        |
| 計画/実行/検証分離     | `PEVEngine`        |

### Agent 定義方式

- **デコレータ**: `@agent` → `AgentClient.get("Name").invoke(...)`
- **基底クラス**: `AgentBlock` 継承 → `run(input_data: dict[str, Any]) -> dict[str, Any]`
- **高信頼**: `ResilientAgent[...]` 継承

### 重要実装ルール（AI 必読）

- **Enum 変換**: 裸の `Enum(value)` 禁止 → 必ず `safe_enum(Cls, val, default, aliases=...)` を使う
- **HITL**: `interrupt()` 前に非冪等操作禁止。状態は `Checkpointer` に保存する
- **SSE**: `result` イベントを必ずハンドリング。各行を `try/catch` で個別保護する
- **非同期 DB**: `await session.commit()` が必須（非同期モードは自動コミットしない）
- **環境変数**: `host/port/URL` のハードコード禁止 → `.env` または `app_config.json` で管理
- **ファイルサイズ**: 全ソースファイル 1000行未満（推奨 300行）
- **文字コード**: UTF-8 (BOM なし)。コメント・ドキュメントは日本語

### アンチパターン

- 1 Agent に複数責務（収集 + 分析 + 出力）を同居させる
- `interrupt()` 前に DB 更新 → 再開時に二重実行が発生する
- SSE だけで承認コマンドを処理する（双方向は WS/REST を使う）
- 裸の `Enum(value)` で未知値クラッシュ
- UI 層から `kernel` 内部実装を直接 import する（Public API 経由にする）

### 新規実装 DoD

1. Engine 選定理由が明文化されている
2. Agent I/O スキーマと失敗時契約が定義済み
3. HITL 必要箇所に承認経路とタイムアウト方針がある
4. ユニットテスト（正常/拒否/例外/再試行）が揃っている
5. **`app_config.json` の `agents[]` に該当 Agent を登録済み**（→ セクション 15 参照）

詳細設計ガイド → [AgentFlow Python](company-specific/agentflow-python.md) / [アーキテクチャ設計](project/architecture.md)

---


## 15. Agent 登録標準（app_config.json）— AI 必読

> Platform が全 Agent を漏れなく表示できるのは、`apps/*/app_config.json` の `agents[]` を
> 起動時にグローバルスキャンするためです。**Python 実装を書いても manifest に登録しなければ
> Platform には表示されません。**

### 15.1 検出メカニズム

```
apps/*/app_config.json
  ↓ (glob scan on startup)
AppDiscoveryService.scan()   ← apps/**/app_config.json を全件捕捉
  ↓
AgentAggregatorService.list_all()  ← 全 App の agents[] を展開・正規化
  ↓
GET /api/studios/framework/agents  ← Platform UI へ配信
```

サーバー起動後に App を追加した場合は `POST /api/studios/framework/apps/rescan` で再スキャンが必要。

### 15.2 agents[] エントリ必須フォーマット

```json
{
  "agents": [
    {
      "name": "MyAgent",
      "module": "apps.my_app.agents.my_agent",
      "capabilities": ["analysis", "trend_detection"],
      "business_base": "reasoning",
      "pattern": "analyzer"
    }
  ]
}
```

| フィールド      | 必須    | 説明                                                           | 省略時の動作                              |
| --------------- | ------- | -------------------------------------------------------------- | ----------------------------------------- |
| `name`          | ✅ 必須 | Agent 名（App 内一意、snake_case 推奨）                        | バリデーションエラー                      |
| `module`        | 推奨    | Python モジュールパス（例: `apps.faq_system.agents.qa_agent`） | 省略可だが診断・監査機能が低下            |
| `capabilities`  | 推奨    | 能力タグ一覧（下表参照）                                       | 空リスト→検索にヒットしなくなる           |
| `business_base` | 推奨    | 業務基盤分類（下表参照）                                       | capabilities・App名から自動推論           |
| `pattern`       | 推奨    | Agent パターン（下表参照）                                     | name・module・engine_pattern から自動推論 |

### 15.3 business_base 有効値一覧

| 値            | 意味                 | 典型的な用途                                          |
| ------------- | -------------------- | ----------------------------------------------------- |
| `platform`    | プラットフォーム管理 | Gallery, Publish, Analytics 等の Platform 自身のAgent |
| `knowledge`   | 知識・RAG            | FAQ, 検索, ドキュメント回答                           |
| `reasoning`   | 推論・分析           | 分析, スコアリング, トレンド検出, 予測                |
| `interaction` | 対話・通知           | チャット, 会議, メッセージング, 通知                  |
| `integration` | 外部統合             | SQL接続, Webhook, MCP, API連携                        |
| `operations`  | 運用・移行           | マイグレーション, デプロイ, ワークフロー              |
| `governance`  | ガバナンス・監査     | Gate判定, コンプライアンス, レビュー, 監査            |
| `media`       | メディア生成         | 画像生成, デザイン, ComfyUI                           |
| `custom`      | その他               | 上記に該当しない場合（最後の手段）                    |

### 15.4 pattern（Agent パターン）有効値一覧

| 値               | 意味                            | 典型的な Agent 名キーワード         |
| ---------------- | ------------------------------- | ----------------------------------- |
| `specialist`     | 単一責務の専門 Agent            | FaqAgent, FileOrganizerAgent        |
| `coordinator`    | 複数 Agent の調整・ルーティング | Coordinator, Orchestrator, Hub      |
| `pipeline_stage` | Pipeline の中間ステージ         | PromptPlanner, Transformer          |
| `gatekeeper`     | 入口判定・ポリシーガード        | GateAgent, PolicyAgent, GuardAgent  |
| `reviewer`       | 品質検証・レビュー              | ReviewAgent, Checker, Validator     |
| `analyzer`       | 分析・診断・スコアリング        | AnalyzerAgent, DiagnosAgent, Scorer |
| `executor`       | 実行・収集・変換                | CollectorAgent, WorkflowExecutor    |
| `router`         | 意図分類・ディスパッチ          | RouterAgent, IntentClassifier       |
| `reporter`       | 集計・レポート・要約            | ReporterAgent, SummaryAgent         |
| `custom`         | その他                          | 上記に該当しない場合（最後の手段）  |

### 15.5 engine_pattern（App レベル）有効値

App の `blueprint.engine_pattern` に設定する値。

| 値            | 対応 Engine        | 使用目的                                    |
| ------------- | ------------------ | ------------------------------------------- |
| `simple`      | `SimpleEngine`     | 単一 Agent の直接応答                       |
| `flow`        | Flow DSL           | 宣言的チェーン（gate/then/parallel/review） |
| `pipeline`    | `PipelineEngine`   | 多段処理＋Reviewループ                      |
| `coordinator` | `AgentCoordinator` | 動的ルーティング、複数 Agent 協調           |
| `deep_agent`  | `PEVEngine` 等     | 計画/実行/検証の深い自律ループ              |
| `custom`      | 独自実装           | 上記に該当しない場合                        |

### 15.6 新規 Agent 追加の DoD チェックリスト

```
□ Python 実装（@agent / AgentBlock / ResilientAgent）を作成済みか？
□ app_config.json の agents[] に name / module / capabilities / business_base / pattern を記載したか？
□ business_base は 15.3 の有効値から選択したか？（省略時は自動推論、精度は低い）
□ pattern は 15.4 の有効値から選択したか？（省略時は自動推論、精度は低い）
□ capabilities は具体的なタグ（例: "analysis", "rag", "notification"）を列挙したか？
□ App 内で Agent 名が重複していないか？（重複するとバリデーションエラーで App 全体が登録失敗）
□ 追加後に Platform を再起動 or /api/studios/framework/apps/rescan を実行したか？
□ GET /api/studios/framework/agents で新 Agent が表示されることを確認したか？
```

### 15.7 自動推論の限界と明示記載の重要性

`business_base` と `pattern` は省略時にコードで自動推論されますが、推論精度には限界があります。

- `business_base` 推論: capabilities の domain → tags → App名の順で推論
- `pattern` 推論: Agent名・moduleパスのキーワードマッチ → engine_pattern フォールバック → `specialist`

**AI へ**: 推論に頼ると意図と異なる分類になる場合があります。必ず明示的に記載してください。

---

## 14. Apps 品質改善の教訓（FAQ Studio 2026-02）

> FAQ Studio 品質向上プロジェクトで発見された頻出バグパターン。
> AI が同じ過ちを繰り返さないよう、以下を必ず参照すること。

### 14.1 非同期 DB コミット（最重要）

| ❌ バグ                                       | ✅ 正解                                    | 理由                                                  |
| --------------------------------------------- | ------------------------------------------ | ----------------------------------------------------- |
| `session.add(obj)` のみで `commit()` を忘れる | `session.add(obj); await session.commit()` | 非同期モード（aiosqlite/asyncpg）は自動コミットしない |
| `session.commit()` を `await` なしで呼ぶ      | `await session.commit()`                   | 非同期セッションは coroutine を返す                   |

- SQLite 同期モードは自動コミットするが、**非同期モード・PostgreSQL では必ず明示 commit が必要**。
- `IntegrityError` のハンドリングも忘れないこと（ユニーク制約違反等）。

### 14.2 SSE イベントハンドリング（フロントエンド）

バックエンドが送信する SSE イベントタイプ: `progress`, `result`, `error`

| ❌ バグ                             | ✅ 正解                                       | 理由                                         |
| ----------------------------------- | --------------------------------------------- | -------------------------------------------- |
| `result` イベントを無視する         | `result` イベントから回答本文を取得し表示する | チャットの回答が画面に表示されない致命的バグ |
| SSE パースを `try/catch` なしで行う | 各行を個別に `try/catch` で囲む               | 1行の不正データで全ストリームが停止          |
| イベント型を `any` で扱う           | 型安全な union 型で定義する                   | TypeScript の型チェックが無効化される        |

### 14.3 Pydantic バリデーション vs サービス層バリデーション

- Pydantic の `min_length`, `max_length`, `ge`, `le` 等はリクエスト解析段階（422 エラー）で適用される。
- サービス層のバリデーション（例: パスワード強度チェック）は 200 + `success: false` で返る。
- **テストではどちらの層で拒否されるか正確に区別すること。**

### 14.4 Pydantic BaseSettings と環境変数

- `AgentFlowSettings` は `.env` ファイルと環境変数の両方から値を読み込む。
- **テストでデフォルト値を検証する場合**: `monkeypatch.delenv()` だけでは不十分。`.env` からも読まれるため `AgentFlowSettings(_env_file=None)` を使う。
- FAQ アプリの `.env`（`override=True`）はルート `.env` より優先される。

### 14.5 JWT ブラックリスト

- 本プロジェクトの JWT 実装はログアウト時にトークンをブラックリスト化する。
- **ログアウト後のトークンは拒否される**（ステートレス JWT の一般的な想定と異なる）。
- テストでは `success is False` で検証すること。

### 14.6 フロントエンド禁止事項（再確認）

- `console.log` / `console.error` はプロダクションコードで**絶対禁止**。
- 代替: エラーはユーザー向け UI 表示（トースト等）に変換する。
- `logout` 等の非同期関数は `Promise<void>` を正しく型宣言する。

### 14.7 Ollama / ローカル LLM 設定

- `settings.py` のデフォルトモデルと `.env` の `OLLAMA_MODEL` を一致させること。
- `router.py` でモデル名をハードコードせず、`os.environ.get("OLLAMA_MODEL", default)` を使う。
- Ollama は OpenAI 互換 API（`/v1/chat/completions`）をサポートする。ネイティブ API は fallback として使用。
