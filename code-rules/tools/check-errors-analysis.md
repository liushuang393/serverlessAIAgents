# check-all エラー分類と対処方針

> **目的**: `make check-all` / `./check.sh all` で発生するエラーを分類し、自動修正可能なものと手動・ルール化で防ぐものを整理する。
> **最終更新**: 2026-02-19

## 1. 結論：何が失敗しているか

| フェーズ       | 結果   | 備考 |
|----------------|--------|------|
| **Format**     | ✅ 通過 | Ruff format + Prettier（Python と JS/TS は別ツールで混在なし） |
| **Lint**       | ✅ 通過 | Ruff check + ESLint |
| **Type-check** | ❌ 失敗 | **MyPy で 829 件のエラー（207 ファイル）** |
| **Test**       | 未実行 | type-check 失敗のため未到達 |
| **Build**      | 未実行 | 同上 |

**フォーマットのファイル種別の混同は発生していない**（Python は Ruff、JS/TS は Prettier/ESLint で正しく分離されている）。問題は **コードの型不整合（MyPy strict）** のみ。

---

## 2. MyPy エラーの分類

以下は `mypy agentflow --strict --ignore-missing-imports` の出力をコード別に集計した分類である。

### 2.1 自動修正しづらい（ルールで再発防止）

| コード | 意味 | 件数目安 | 対処 |
|--------|------|----------|------|
| **no-any-return** | 戻り値が Any なのに宣言が具体型 | 多数 | 戻り値を `cast(宣言型, 式)` するか、実装で具体型を返す。AI は「戻り型を明示し、Any を返さない」ルールに従う。 |
| **no-untyped-def** | 関数・メソッドに型アノテーションがない | 多数 | 引数・戻り値に型を付与。AI は「全ての public 関数に型アノテーション必須」ルールに従う。 |
| **type-arg** | ジェネリックに型パラメータがない（`dict` → `dict[str, Any]` 等） | 多数 | `dict` → `dict[str, Any]`、`list` → `list[T]`、`Callable` → `Callable[[Arg], Ret]` に修正。AI ルールで禁止。 |
| **arg-type** | 引数の型が宣言と不整合 | 多数 | 呼び出し側で型を合わせる、または `cast`。Nullable の場合は `assert x is not None` や分岐で絞り込む。 |
| **assignment** | 代入の左辺と右辺の型が不整合 | 多数 | 変数の宣言型を見直す、または右辺を正しい型に変換。 |
| **call-arg** | 必須引数不足・予期しないキーワード引数 | 多数 | モデル・関数のシグネチャ変更に合わせて呼び出しを更新。AI は「Pydantic モデル・API 変更時は呼び出し元を一括確認」ルールに従う。 |
| **attr-defined** | オブジェクトにその属性がない | あり | 正しい属性名・型に修正。Optional の場合は None チェックを入れる。 |
| **union-attr** | `X | None` に対して属性アクセスしている | 多数 | アクセス前に `if x is not None:` で分岐する。AI ルールで必須。 |
| **override** | サブクラスのメソッドの戻り型がスーパーと不整合 | あり | 基底の `async def deploy()` が Coroutine を返すなら、サブクラスは `async def` のまま `await` してから yield するなど、型を揃える。 |
| **unreachable** | 到達不能な文（型推論で分岐が決まっている等） | あり | 不要な elif を削除する、または型の絞り込みを見直す。 |
| **valid-type** | `callable` を型として使っている | あり | `typing.Callable[[Arg], Ret]` を使う。AI ルールで禁止。 |
| **unused-ignore** | `# type: ignore` が不要になっている | あり | 該当行の `# type: ignore` を削除。 |

### 2.2 設計・API の不整合（手動修正が必要）

- **Missing named argument "xxx" for "YYY"**  
  Pydantic モデルや FastAPI のレスポンス型に必須フィールドが追加されたが、呼び出し側が未対応。  
  → 該当モデルの定義と全呼び出し元を照合し、必須引数を追加する。
- **Unexpected keyword argument "zzz" for "WWW"**  
  → モデルから削除された引数を呼び出しから外す、またはモデル側を合わせる。
- **Return type "AsyncIterator[...]" incompatible with "Coroutine[..., AsyncIterator[...]]"**  
  → 基底が `async def` で Coroutine を返す定義なのに、サブクラスが async generator を直接返している。  
  → 基底のシグネチャを `async def ... -> AsyncIterator` に揃えるか、サブで `await` してからイテレートする形に変更。

### 2.3 よくあるファイル・モジュール

- `agentflow/state/selectors.py` — no-any-return, unreachable
- `agentflow/code_intelligence/ast/nodes.py` — no-untyped-def, type-arg
- `agentflow/skills/builtin/bi_analytics/*` — sum/min/max に `list[Any|None]` を渡している (arg-type, type-var)
- `agentflow/deploy/targets/*.py` — override (deploy の戻り型)
- `agentflow/hitl/*.py` — call-arg (ApprovalRequest, ApprovalResponse, HITLConfig の必須引数)
- `agentflow/protocols/a2ui/renderer.py` — valid-type (callable → Callable)
- `agentflow/providers/vectordb_provider.py` — assignment (Union 型の変数に別実装を代入)
- `agentflow/flow/*.py` — union-attr (AgentProtocol | None), NodeType の属性不足
- `agentflow/studio/routes/*.py` — call-arg, attr-defined (RAGService 等)

---

## 3. 自動修正できる MyPy エラー（ツールで対応）

| エラーコード | 件数目安 | 対処方法 |
|--------------|----------|----------|
| **unused-ignore** | 数件 | `python scripts/fix_mypy_safe.py` で不要な `# type: ignore` を削除。`--dry-run` で事前確認可。 |
| **valid-type** | 数件 | 型注釈で `callable` を使わず `from typing import Callable` の `Callable[[Arg], Ret]` を使う。1 ファイルずつ手動修正。 |

上記以外の MyPy エラーには **mypy 公式の --fix はない**。手動修正または AI が [mypy-avoid-patterns](../global/mypy-avoid-patterns.md) に従って新規・修正コードを書くことで再発防止する。

```bash
# 安全な自動修正の実行例
conda activate agentflow
python scripts/fix_mypy_safe.py --dry-run   # 削除対象のみ表示
python scripts/fix_mypy_safe.py             # 実行
mypy agentflow --strict --ignore-missing-imports  # 確認
```

## 4. 自動修正できない MyPy エラー（AI ルールで再発防止）

以下はツールでは一括修正できないため、**AI がコードを書く際に [Mypy 回避パターン（AI 向け）](../global/mypy-avoid-patterns.md) に必ず従う**こと。

| コード | 件数目安 | 概要 |
|--------|----------|------|
| call-arg | 138 | 必須引数不足・余計なキーワード → モデル/関数のシグネチャに合わせる |
| no-any-return | 120 | 宣言型と異なる Any を返す → cast または実装を具体型に |
| attr-defined | 71 | 存在しない属性アクセス → 正しい属性名・None チェック |
| assignment | 62 | 代入の型不整合 → 変数型を Union 等に合わせる |
| union-attr | 58 | Optional に属性アクセス → 事前に `if x is not None:` |
| type-arg | 46 | ジェネリックに型パラメータなし → dict[str, Any] 等を明示 |
| unreachable | 39 | 到達不能コード → 削除または分岐を見直す |
| arg-type | 36 | 引数型不整合 → 呼び出し側を合わせる |
| no-untyped-call | 36 | 未型付け関数の呼び出し → 被呼び出し側に型を付ける |
| no-untyped-def | 34 | 関数に型アノテーションなし → 引数・戻り値に型を付ける |
| valid-type | 7 | 型として callable 使用 → typing.Callable を使う |
| override | 4 | オーバーライドの戻り型不整合 → 基底と一致させる |

その他（operator, misc, index, list-item, var-annotated, return-value 等）も [mypy-avoid-patterns](../global/mypy-avoid-patterns.md) を参照。

## 5. その他の自動化（フォーマット・リント）

| 項目 | ツール | コマンド |
|------|--------|----------|
| Python フォーマット | Ruff | `ruff format .` |
| Python リント＋自動修正 | Ruff | `ruff check . --fix` |
| JS/TS フォーマット | Prettier | `cd studio && npx prettier --write "src/**/*.{ts,tsx,js,jsx,json,css}"` |
| JS/TS リント＋自動修正 | ESLint | `cd studio && npx eslint "src/**/*.{ts,tsx,js,jsx}" --fix` |

---

## 6. 運用上のチェックの段階化

型チェックが未解消の間も、フォーマット・リント・テスト・ビルドまで通したい場合は次を使う。

- **全チェック（型含む）**: `make check-all` または `./check.sh all`
- **型チェックをスキップ**: `make check-nomypy` または `./check.sh all --no-type-check`

コミット前は原則 `check-all` を推奨し、型エラー解消中のみ `check-nomypy` で他を確認する。

---

## 7. エラー件数の集計（任意）

型エラー解消の進捗を確認するときは、以下でコード別・ファイル別のサマリーを出せる。

```bash
conda activate agentflow
python scripts/mypy_error_summary.py
```

## 8. 参照

- 型エラーを減らすための AI 向けルール: [Mypy 回避パターン（AI 向け）](../global/mypy-avoid-patterns.md)
- リンター・フォーマット: [lint-format.md](lint-format.md)
- コミット前チェック一覧: [CLAUDE.md コンプライアンス確認](../CLAUDE.md#-コンプライアンス確認)
