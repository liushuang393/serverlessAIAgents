# Mypy 回避パターン（AI 向け）

> **目的**: AI がコードを書く・修正する際に、本リポジトリで多発している MyPy エラーを出さないためのルール。
> **適用**: AgentFlow の Python コード全般。`mypy agentflow --strict --ignore-missing-imports` で検証。

## 0. エラー件数別の優先ルール（本リポジトリの実態）

以下は `python scripts/mypy_error_summary.py` で集計したコード別件数に基づく。**AI は新規・修正コードでこれらを絶対に発生させないこと。**

| コード              | 件数目安 | 禁止（やってはいけないこと）                                                                           | すること                                                                                                  |
| ------------------- | -------- | ------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------- |
| **call-arg**        | 138      | モデル/関数の必須引数を省略する。存在しないキーワードを渡す。                                          | 呼び出し前にモデル定義を確認し、必須引数をすべて渡す。Pydantic モデル変更時は grep で全呼び出し元を更新。 |
| **no-any-return**   | 120      | 戻り型を `-> dict[str, Any]` 等にしておきながら `return response.json()` のように Any をそのまま返す。 | `return cast(宣言型, 式)` を使うか、実装で具体型を返す。                                                  |
| **attr-defined**    | 71       | オブジェクトに存在しない属性を書く。Optional を None チェックせず `.attr` する。                       | 正しい属性名を使う。`x: T \| None` の場合は `if x is not None:` のブロック内でのみ `x.attr`。             |
| **assignment**      | 62       | 変数を `A` 型で宣言したあとに `変数 = B型の値` と代入する（A ≠ B）。                                   | 複数型を代入する場合は `変数: A \| B` またはプロトコルで宣言する。                                        |
| **union-attr**      | 58       | `x: Foo \| None` に対して `x.method()` や `x.attr` をいきなり書く。                                    | 必ず `if x is not None:` で分岐してからアクセスする。                                                     |
| **type-arg**        | 46       | `dict` / `list` / `Callable` に型パラメータを付けない。型として `callable` を使う。                    | `dict[str, Any]`、`list[T]`、`Callable[[Arg], Ret]`（typing.Callable）を明示する。                        |
| **unreachable**     | 39       | 型推論で既に分岐が決まっている後の `elif` を書く。                                                     | 不要な分岐を削除する。または型の絞り込みを見直す。                                                        |
| **arg-type**        | 36       | 引数が `str` のところに `str \| None` を渡す。                                                         | 呼び出し前に `x or ""` や `if x is not None:` で型を合わせる。                                            |
| **no-untyped-call** | 36       | 型アノテーションのない関数を呼ぶ（その関数が未型付け）。                                               | 被呼び出し側の関数に引数・戻り値の型を付ける。                                                            |
| **no-untyped-def**  | 34       | 公開関数・メソッドに型アノテーションを付けない。                                                       | すべての public な `def` に引数型と `-> 戻り型` を付ける。                                                |
| **valid-type**      | 7        | 型注釈で `callable` を使う。                                                                           | `from typing import Callable` し、`Callable[[Arg], Ret]` を使う。                                         |
| **override**        | 4        | サブクラスでメソッドの戻り型を基底と異なる型にする。                                                   | オーバーライドの戻り型は基底と同一または共変にする。                                                      |
| **unused-ignore**   | 9        | 修正後も不要な `# type: ignore` を残す。                                                               | 不要になったら削除。`python scripts/fix_mypy_safe.py` で一括削除可。                                      |

---

## 1. 必須ルール（守らないと型エラーになる）

### 1.1 戻り値の型

- **no-any-return を出さない**  
  宣言が `-> dict[str, Any]` や `-> str` なのに、実装で `return response.json()` のように Any を返さない。
  - **すること**: 返す値が確実にその型なら `cast(宣言型, 式)` を使う。または実装を具体型を返すように変える。
  - **禁止**: 宣言を `-> Any` にして誤魔化す（strict では不十分な場合あり）。

### 1.2 関数・メソッドの型アノテーション

- **no-untyped-def を出さない**  
  すべての public な関数・メソッドに、引数と戻り値の型アノテーションを付ける。
  - 引数: `def f(self, x: str, count: int = 0) -> list[str]:`
  - 戻り値: 必ず `-> ...` を書く。`__iter__` などは `-> Iterator[T]` など適切な型を付ける。

### 1.3 ジェネリックの型パラメータ

- **type-arg を出さない**
  - `dict` → `dict[str, Any]` または必要なキー・値の型
  - `list` → `list[str]` や `list[SomeModel]` など
  - `Callable` → `Callable[[Arg1, Arg2], Ret]`（`typing.Callable` を使う。**`builtins.callable` は型として使わない**）
  - `Task` → `asyncio.Task[T]`
  - `list[dict]` → `list[dict[str, Any]]` など

### 1.4 Optional / Union の扱い

- **union-attr を出さない**  
  `x: Foo | None` に対して `x.method()` や `x.attr` をする前に、`if x is not None:` で分岐する。
- **arg-type を出さない**  
  引数が `str` のところに `str | None` を渡さない。None の場合はデフォルト値にフォールバックするか、呼び出し前に分岐する。

### 1.5 Pydantic・FastAPI のモデル呼び出し

- **call-arg を出さない**  
  モデルやレスポンス型に必須フィールドが追加されたら、**すべての呼び出し元**でその引数を渡す。  
  キーワード引数名を間違えない（`rejection_reason` / `comment` / `approver` など）。
- 新しくモデルを追加・変更するときは、`ApprovalRequest` / `ApprovalResponse` / `HITLConfig` / `APIResponse` など、既存の利用箇所を grep して一括で合わせる。

### 1.6 継承・オーバーライド

- **override を出さない**  
  サブクラスでメソッドをオーバーライドするとき、戻り値の型はスーパークラスと**同一または共変**にする。
  - 例: 基底が `async def deploy(...) -> Coroutine[..., AsyncIterator[DeployEvent]]` なら、サブでは `async def deploy(...)` の戻り型をそれに合わせる。  
    「async generator を直接返したい」場合は、基底の定義を `AsyncIterator` を返す形に揃えるか、サブで `await` してから yield する形にする。

### 1.7 型として使ってはいけないもの

- **valid-type**  
  `callable` を型として使わない。必ず `from typing import Callable` して `Callable[[Arg], Ret]` を使う。

### 1.8 変数宣言と代入

- **assignment を出さない**  
  変数に最初に代入する型と、後の代入の型を一致させる。  
  例: `provider: FAISSProvider` と宣言したあとに `provider = QdrantProvider(...)` は不可。  
  複数実装を切り替える場合は `Union` やプロトコルで宣言する。
- **var-annotated**  
  空リストなどで型が推論されないときは `issues: list[str] = []` のように明示する。

### 1.9 type: ignore

- **unused-ignore を出さない**  
  修正の結果、もう不要になった `# type: ignore` は削除する。mypy は `--warn-unused-ignores` で警告する。

---

## 2. 推奨パターン（コピペ用）

```python
# Optional の安全なアクセス
def use_agent(agent: AgentProtocol | None) -> dict[str, Any]:
    if agent is None:
        return {}
    return await agent.run(inputs)  # ここでは agent は AgentProtocol

# ジェネリックの明示
def merge(a: dict[str, Any], b: dict[str, Any]) -> dict[str, Any]:
    return {**a, **b}

# コールバックの型
from typing import Callable
Middleware = Callable[[str, WSMessage, Callable[..., Awaitable[None]]], Awaitable[None]]

# 戻り値のキャスト（やむを得ない場合）
from typing import cast
def load_json(path: Path) -> dict[str, Any]:
    return cast(dict[str, Any], json.loads(path.read_text()))
```

---

## 3. 本リポジトリで特に注意するモジュール（エラー多発ファイル）

以下は mypy エラー件数が特に多いモジュール。**これらのファイルを触る際は上記ルールを厳守し、既存の call-arg / no-any-return / attr-defined を増やさないこと。**

- **agentflow/skills/builtin/bi_analytics/analyzer.py** — type-var, arg-type（`sum`/`min`/`max` に `list[Any|None]` を渡さない。数値リストは `list[float]` に絞る）
- **agentflow/code_intelligence/parsers/modern/python_parser.py**, **java_parser.py**, **legacy/cobol_parser.py** — ASTNode / ParseResult / SymbolInfo のシグネチャに合わせる。`errors=[str]` ではなく `list[ParseError]`。
- **agentflow/agents/sales_agent.py**, **data_analytics_agent.py**, **faq_agent.py** — 型注釈と戻り値の型を揃える。
- **agentflow/services/rag_service.py**, **text2sql_service.py** — 公開メソッドの型と call-arg。
- **agentflow/hitl/** — `ApprovalRequest` / `ApprovalResponse` / `HITLConfig` / `ApprovalFlowConfig` は必須引数が多い。新規追加時は既存の `ApprovalRequest(...)` 等を grep して揃える。
- **agentflow/deploy/targets/** — `BaseDeployTarget.deploy` の戻り型と各サブクラスの `async def deploy` の戻り型を一致させる。
- **agentflow/protocols/a2ui/renderer.py** — 型注釈は `Callable[..., Any]` を使用（`callable` 禁止）。
- **agentflow/providers/vectordb_provider.py** — 複数プロバイダを同一変数に代入する場合は、型を `Union` または共通プロトコルで宣言する。
- **agentflow/flow/nodes.py**, **pev/pev_engine.py** — `agent: AgentProtocol | None` / `_planner: HierarchicalPlanner | None` 等は、使用前に `if x is not None:` を必ず入れる。
- **agentflow/state/selectors.py** — no-any-return, unreachable。戻り値は cast、分岐は整理。

---

## 4. 自動修正ツール

- **unused-ignore の一括削除**: `python scripts/fix_mypy_safe.py`（`--dry-run` で事前確認）
- **エラー件数サマリー**: `python scripts/mypy_error_summary.py`
- 詳細: [check-errors-analysis.md](../tools/check-errors-analysis.md)

## 5. 参照

- エラー分類と自動可/不可: [check-errors-analysis.md](../tools/check-errors-analysis.md)
- 品質ゲート・コミット前チェック: [CLAUDE.md コンプライアンス確認](../CLAUDE.md#-コンプライアンス確認)
