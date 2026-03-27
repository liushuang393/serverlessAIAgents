# AgentFlow — AI ルール Layer 0（常時ロード）

> **プロジェクト**: AgentFlow — MCP/A2A/AG-UI/A2UI 統一インターフェース AI エージェントフレームワーク
> **このファイルは常時ロードされる最小必須ルールセットです。**
> 詳細な規約は `code-rules/CLAUDE.md`（詳細参照版）に集約されています。

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

---

## 🌐 グローバル設定

- **言語**: 回答は日本語、コードコメントも日本語
- **Python 実行環境**: `conda run -n agentflow <command>` を使用（`conda activate` ではなく）
- **環境依存値ハードコード禁止**: host / port / API URL は `.env` で管理
- **ファイルサイズ上限**: 1000行（推奨 300行、500行超で分割計画必須）
- **AI の役割**: 補助的意思決定者。不確実な場合は明示し、推測・捏造禁止

---

## 📂 タスク別ルール参照トリガー

> **以下の条件に合致するタスクを実行する前に、対応するファイルを必ず読むこと。**

| タスク条件                               | 読むべきファイル                              |
| ---------------------------------------- | --------------------------------------------- |
| Python コードを新規作成・編集する        | `code-rules/languages/python-rules.md`        |
| 型エラー (mypy) を修正する               | `code-rules/global/mypy-avoid-patterns.md`    |
| 新規 Agent を追加する                    | `code-rules/CLAUDE.md` セクション 15          |
| アーキテクチャ・層構造に関わる変更       | `code-rules/project/architecture.md`          |
| テストを書く・修正する                   | `code-rules/tools/testing.md`                 |
| リント・フォーマット設定を変更する       | `code-rules/tools/lint-format.md`             |
| CI/CD・pre-commit を変更する             | `code-rules/project/ci-cd-guidelines.md`      |
| 依存関係を更新・追加する                 | `code-rules/project/dependency-management.md` |
| `check.sh` エラーを解析する              | `code-rules/tools/check-errors-analysis.md`   |
| ファイル・ディレクトリ構造を変更する     | `code-rules/project/repo-structure.md`        |
| AgentFlow Public API を設計・実装する    | `code-rules/CLAUDE.md` セクション 3〜12       |
| 発生頻度の高い AI 弱点パターンを確認する | `code-rules/global/ai-weakness.md`            |

**全体的な詳細規約** → `code-rules/CLAUDE.md`（Layer 1 詳細参照版）

---

## ✅ コミット前チェック（必須）

```bash
# 推奨: 一括実行
./check.sh all

# 型エラー解消中の場合
./check.sh all --no-type-check
```

| 領域                        | コマンド                                                                              |
| --------------------------- | ------------------------------------------------------------------------------------- |
| Python フォーマット・リント | `ruff format .` / `ruff check .`                                                      |
| Python 型チェック           | `mypy contracts infrastructure shared kernel harness control_plane domain apps tests` |
| Python テスト               | `pytest --cov=control_plane --cov-fail-under=80`                                      |
| フロントエンド              | `cd control_plane/frontend && npm run lint && npm run type-check && npm run build`    |
| 共通フック                  | `pre-commit run --all-files`                                                          |
