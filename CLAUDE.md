# AgentFlow — AI ルール Layer 0（常時ロード）

> **プロジェクト**: AgentFlow — MCP/A2A/AG-UI/A2UI 統一インターフェース AI エージェントフレームワーク
> **このファイルは常時ロードされる最小必須ルールセット（Layer 0）です。**
> ルート `AGENTS.md` はこのファイルへのリダイレクトのみ（重複ロード防止）。
> 詳細な規約は `code-rules/AGENTS.md`（Layer 1・詳細参照版）に集約されています。

---

## 🚨 CRITICAL: Python 型エラー（コード編集のたびに必ずチェック）

| #   | ❌ 違反パターン                          | ✅ 正解                                    |
| --- | ---------------------------------------- | ------------------------------------------ |
| 1   | `def fn(x):` 型なし                      | `def fn(x: str) -> None:`                  |
| 2   | `Optional[X]` を None チェックなしで参照 | `if value is not None:` ガード後にアクセス |
| 3   | `Any` をデフォルト型として使う           | 具体型を明示（理由コメント必須）           |
| 4   | 戻り値型と `return` 値が不一致           | 宣言型と一致する値を返す                   |
| 5   | `dict["key"]` で KeyError リスク         | `.get("key")` または存在確認必須           |
| 6   | `cast()` で型を誤魔化す                  | 実際に正しい型の値を生成する               |
| 7   | `# type: ignore` を理由なく使う          | 根本原因を修正する                         |
| 8   | Pydantic フィールドに `Any` / 型なし     | `field: str`, `field: int \| None` 等      |
| 9   | `async def` の戻り値型を省略             | `async def fn() -> dict[str, Any]:`        |

**毎回の自己チェック**: 全引数・戻り値に型付与済みか？ None ガードは漏れていないか？ mypy --strict でエラー 0 か？

---

## 🚨 Git 操作安全規約

- `git checkout` / `git reset` / `git restore` / `git stash` はユーザーの明示的な許可なしに実行禁止
- 変更前に必ず `git diff` をチャットに提示してユーザー確認を仰ぐ
- `--force` / `-f` を伴う操作は独断で実行禁止
- **worktree 禁止**: `git worktree` を使わない。単独開発のため、常に `main` ブランチ上で直接作業する

---

## 🌐 グローバル設定

- **言語**: 回答は日本語、コードコメントも日本語
- **Python 実行環境**: `conda run -n agentflow <command>` を使用（`conda activate` ではなく）
- **環境依存値ハードコード禁止**: host / port / API URL は、App が `app_config.json` を持つ場合は `app_config.json` を正本にし、環境変数は一時 override のみに使う
- **ファイルサイズ上限**: 1000行（推奨 300行、500行超で分割計画必須）
- **AI の役割**: 補助的意思決定者。不確実な場合は明示し、推測・捏造禁止

---

## 📚 サブルール索引（タスク別参照ガイド）

> **以下のタスクに該当する場合、作業前に対応ファイルを必ず読むこと。**
> パスはすべてリポジトリルート基準。詳細規約全体 → [`code-rules/AGENTS.md`](code-rules/AGENTS.md)

### 🐍 Python 言語・型安全

| ファイル                                                                               | 概要                                                                   | 利用場面                                |
| -------------------------------------------------------------------------------------- | ---------------------------------------------------------------------- | --------------------------------------- |
| [`code-rules/languages/python-rules.md`](code-rules/languages/python-rules.md)         | 型アノテーション・非同期・Pydantic・命名規則など Python 全般の実装規約 | Python コードを新規作成・編集するとき   |
| [`code-rules/global/mypy-avoid-patterns.md`](code-rules/global/mypy-avoid-patterns.md) | AI が犯しやすい mypy エラーパターンと安全な修正方法の一覧              | 型エラー (mypy) を修正・解消するとき    |
| [`code-rules/global/ai-weakness.md`](code-rules/global/ai-weakness.md)                 | AI が頻繁に犯すミスパターン（型・非同期・DB・SSE 等）と対策集          | AI 生成コードのレビューや再発防止の確認 |

### 🏗️ アーキテクチャ・Agent 設計

| ファイル                                                                   | 概要                                                                  | 利用場面                                     |
| -------------------------------------------------------------------------- | --------------------------------------------------------------------- | -------------------------------------------- |
| [`code-rules/project/architecture.md`](code-rules/project/architecture.md) | 7コア層+Apps の依存方向・レイヤ分離・設計原則                         | 層構造・依存関係・設計判断に関わるとき       |
| [`code-rules/AGENTS.md` § フレームワーク概要](code-rules/AGENTS.md#-agentflow-フレームワーク概要) | Agent/Engine/Flow/HITL の Public API 設計規約とシナリオ別構成例       | AgentFlow の Public API を設計・実装するとき |
| [`code-rules/AGENTS.md` § 15](code-rules/AGENTS.md#15-agent-登録標準app_configjson--ai-必読) | `app_config.json` の `agents[]` 登録フォーマットと DoD チェックリスト | 新規 Agent を追加・登録するとき              |
| [`code-rules/global/principles.md`](code-rules/global/principles.md)       | 品質優先・インクリメンタル改善・設計原則などの開発方針                | 設計・実装方針を確認したいとき               |

### 🧪 品質・テスト・CI

| ファイル                                                                                     | 概要                                                                           | 利用場面                                             |
| -------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ | ---------------------------------------------------- |
| [`code-rules/tools/testing.md`](code-rules/tools/testing.md)                                 | pytest 方針・カバレッジ・モック・非同期テストの規約と手順                      | テストを書く・修正するとき                           |
| [`code-rules/tools/lint-format.md`](code-rules/tools/lint-format.md)                         | Ruff・ESLint・Prettier の設定方法と利用ルール                                  | リント・フォーマット設定を変更するとき               |
| [`code-rules/tools/architecture-validation.md`](code-rules/tools/architecture-validation.md) | レイヤー境界・プロバイダ隔離・App コンプライアンス・ルール遵守の自動検証ツール | アーキテクチャ違反の確認・CI/CD 検証ツールを使うとき |
| [`code-rules/tools/check-errors-analysis.md`](code-rules/tools/check-errors-analysis.md)     | `./check.sh` の出力エラーを種別に分類し対処する手順ガイド                      | `check.sh` でエラーが出たとき                        |
| [`code-rules/project/ci-cd-guidelines.md`](code-rules/project/ci-cd-guidelines.md)           | GitHub Actions・pre-commit フックの設定規約と変更手順                          | CI/CD・pre-commit を変更するとき                     |

### 🎨 コードスタイル・命名・ドキュメント

| ファイル                                                                                       | 概要                                        | 利用場面                             |
| ---------------------------------------------------------------------------------------------- | ------------------------------------------- | ------------------------------------ |
| [`code-rules/global/naming-guidelines.md`](code-rules/global/naming-guidelines.md)             | 変数・関数・クラス・ファイル等の命名規約    | 新規コード作成・リネームするとき     |
| [`code-rules/global/style-formatting.md`](code-rules/global/style-formatting.md)               | コードスタイル・インデント・フォーマット規約 | フォーマットの判断に迷ったとき       |
| [`code-rules/global/documentation-standards.md`](code-rules/global/documentation-standards.md) | docstring・README・コメントの書き方規約     | ドキュメント・docstring を書くとき   |

### 🔒 エラー処理・セキュリティ

| ファイル                                                                             | 概要                                                 | 利用場面                                 |
| ------------------------------------------------------------------------------------ | ---------------------------------------------------- | ---------------------------------------- |
| [`code-rules/global/error-handling.md`](code-rules/global/error-handling.md)         | 例外処理・エラーハンドリングの設計規約               | try/except・エラー処理を設計するとき     |
| [`code-rules/global/security-standards.md`](code-rules/global/security-standards.md) | 認証・認可・入力検証・秘密情報管理のセキュリティ規約 | セキュリティ関連の実装・レビューするとき |

### ⚡ AgentFlow 固有・統合

| ファイル                                                                                                 | 概要                                       | 利用場面                                              |
| -------------------------------------------------------------------------------------------------------- | ------------------------------------------ | ----------------------------------------------------- |
| [`code-rules/company-specific/agentflow-python.md`](code-rules/company-specific/agentflow-python.md)     | AgentFlow 固有の Python 実装パターン・規約 | AgentFlow コア層のコードを書くとき                    |
| [`code-rules/project/agent-lightning-integration.md`](code-rules/project/agent-lightning-integration.md) | Agent Lightning 学習連携の統合規約         | `agentflow/run/`・`agentflow/engines/` を変更するとき |

### 📦 プロジェクト管理・構造

| ファイル                                                                                                 | 概要                                                         | 利用場面                                 |
| -------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------ | ---------------------------------------- |
| [`code-rules/project/dependency-management.md`](code-rules/project/dependency-management.md)             | Dependabot・pip/npm 依存更新の方針・グループ化・マージルール | 依存関係を更新・追加するとき             |
| [`code-rules/project/repo-structure.md`](code-rules/project/repo-structure.md)                           | ディレクトリ構成・ファイル配置・命名の規約                   | ファイル・ディレクトリ構造を変更するとき |
| [`code-rules/project/file-management.md`](code-rules/project/file-management.md)                         | ファイル作成・削除・移動のルールとサイズ管理                 | ファイル操作・整理するとき               |
| [`code-rules/project/contribution-guidelines.md`](code-rules/project/contribution-guidelines.md)         | PR・コミット・レビューのコントリビューション規約             | PR 作成・コードレビューするとき          |

---

## ✅ コミット前チェック（必須）

```bash
./check.sh all          # 推奨: Python + フロントエンド + テスト 一括実行
./check.sh all --no-type-check  # 型エラー解消中は他チェックのみ
```

> 詳細なチェック項目・コマンド一覧 → [`code-rules/AGENTS.md`](code-rules/AGENTS.md)
