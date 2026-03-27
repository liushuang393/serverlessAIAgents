# AI弱点補強ルール

> **バージョン**: 1.1.0
> **適用範囲**: AgentFlow AI生成コード全般
> **最終更新**: 2026-02-21

## 📋 目次

0. [🚨 Python型エラー（最重要・最頻出）](#python型エラー最重要最頻出)
1. [AI生成コードの脆弱性](#ai生成コードの脆弱性)
2. [仕様明文化テンプレート](#仕様明文化テンプレート)
3. [自動テスト要求ルール](#自動テスト要求ルール)
4. [コードレビューポイント](#コードレビューポイント)
5. [品質ゲート](#品質ゲート)
6. [自動化スクリプト](#自動化スクリプト)

---

## 🚨 Python型エラー（最重要・最頻出）

> **2026-02-21 追加**: Python の型エラーはこのプロジェクトで最も頻発する問題です。
> 以下のパターンを必ず頭に入れ、コードを出力するたびに自己チェックしてください。

### ❌ 頻出違反パターン と ✅ 正解

#### P-1: 型アノテーションの欠落

```python
# ❌ 禁止: 引数・戻り値の型なし
def get_user(user_id):
    return db.find(user_id)

# ✅ 必須: 全引数・戻り値に型アノテーション
def get_user(user_id: str) -> dict[str, Any] | None:
    return db.find(user_id)
```

#### P-2: None 安全性の欠落（最頻出クラッシュ原因）

```python
# ❌ 禁止: None チェックなしで属性・メソッドにアクセス
user = get_user("id")
name = user["name"]          # user が None なら KeyError / TypeError

# ✅ 必須: None ガードを挟む
user = get_user("id")
if user is None:
    raise ValueError("ユーザーが見つかりません")
name = user["name"]
```

#### P-3: `Any` の安易な使用

```python
# ❌ 禁止: Any でごまかす
def process(data: Any) -> Any:
    ...

# ✅ 必須: 具体型を使う（本当に Any が必要な場合はコメントで理由を明記）
def process(data: dict[str, str]) -> list[str]:
    ...
```

#### P-4: 戻り値型と実際の return 値が不一致

```python
# ❌ 禁止: 宣言型と実際の値が違う
def get_ids() -> list[str]:
    return {"a": 1, "b": 2}   # dict を返しているのに list[str] と宣言

# ✅ 正解: 宣言と実装を一致させる
def get_ids() -> dict[str, int]:
    return {"a": 1, "b": 2}
```

#### P-5: dict アクセスの KeyError リスク

```python
# ❌ 禁止: キー存在を保証せずに直接アクセス
value = config["timeout"]     # キーがなければ KeyError

# ✅ 推奨: .get() またはデフォルト値を使う
value = config.get("timeout", 30)
```

#### P-6: `cast()` / `# type: ignore` で問題を隠蔽

```python
# ❌ 禁止: 型の問題を隠す
result: str = cast(str, some_func())    # 実際に str でない場合バグ
x = some_func()  # type: ignore         # 根本原因を放置

# ✅ 正解: 正しい型を返す関数にするか、実際の型を使う
result: str = str(some_func())          # 明示的に変換
```

#### P-7: Pydantic モデルの型誤定義

```python
# ❌ 禁止: 型なし / Any 使用
class UserModel(BaseModel):
    name: Any
    age = None   # フィールド型なし

# ✅ 必須: 全フィールドに正確な型
class UserModel(BaseModel):
    name: str
    age: int | None = None
```

#### P-8: async 関数の戻り値型省略

```python
# ❌ 禁止: 非同期でも型を省略
async def fetch_data(url):
    ...

# ✅ 必須: async も同様に型を付ける
async def fetch_data(url: str) -> dict[str, Any]:
    ...
```

#### P-9: ジェネリック型の旧スタイル（Python 3.9+以降は不要）

```python
# ❌ 旧スタイル（非推奨）
from typing import List, Dict, Optional
def fn(x: List[str]) -> Dict[str, int]: ...

# ✅ 新スタイル（Python 3.13 推奨）
def fn(x: list[str]) -> dict[str, int]: ...
def fn2(x: str | None = None) -> str: ...
```

#### P-10: TypedDict / dataclass での型ミス

```python
# ❌ 禁止: TypedDict キーに型宣言なし
class Config(TypedDict):
    host: str
    port    # ← 型なし、エラー

# ✅ 正解
class Config(TypedDict):
    host: str
    port: int
    debug: bool
```

### 📋 Python 型エラー セルフチェックリスト

コードを書いたら必ず以下を確認すること：

```
□ 全引数・戻り値に型アノテーションを付けたか？
□ None を返す可能性がある場合 T | None を宣言したか？
□ None の可能性がある値を .attr / ["key"] でアクセスする前に None チェックを入れたか？
□ 戻り値の宣言型と実際に return している値の型が一致しているか？
□ dict アクセスに .get() またはキー存在確認を使っているか？
□ Any / cast() / # type: ignore を使っていないか？
□ Pydantic フィールドに全て明示的な型を付けたか？
□ async def の戻り値型を省略していないか？
□ list / dict のジェネリックは Python 3.9+ スタイルか？
```

---

## 🤖 AI生成コードの脆弱性

### 主要な弱点

#### 1. 曖昧な意図のまま書かれる傾向

**問題**: AIはユーザーの曖昧な指示を「それらしい」コードに変換してしまう

```python
# ❌ AI生成の脆弱例: 意図が不明瞭
def process_data(data):
    """データを処理する"""
    # 何を処理するのか？ どんな形式か？ 結果は？
    return data.upper()  # たまたま動いただけ

# ✅ 明確な仕様化
def normalize_user_name(name: str) -> str:
    """ユーザー名を正規化する.

    以下の処理を行う:
    1. 前後の空白を削除
    2. 全て小文字に変換
    3. 特殊文字を除去

    Args:
        name: 正規化対象のユーザー名

    Returns:
        正規化されたユーザー名

    Raises:
        ValueError: nameがNoneまたは空文字の場合
    """
    if not name or not isinstance(name, str):
        raise ValueError("有効なユーザー名を指定してください")
    return name.strip().lower()
```

#### 2. エラー処理の欠如

**問題**: AIは「正常系」しか考えない

```python
# ❌ AI生成の脆弱例: エラー処理なし
async def fetch_user_data(user_id):
    response = await http.get(f"/api/users/{user_id}")
    return response.json()

# ✅ 包括的なエラー処理
async def fetch_user_data(user_id: str) -> dict[str, Any] | None:
    """ユーザー情報を取得する.

    Args:
        user_id: ユーザーID

    Returns:
        ユーザー情報、存在しない場合はNone

    Raises:
        ValueError: user_idが無効な場合
        HTTPError: APIエラーの場合
        TimeoutError: タイムアウトの場合
    """
    if not user_id or not isinstance(user_id, str):
        raise ValueError("有効なuser_idを指定してください")

    try:
        async with aiohttp.ClientSession(timeout=aiohttp.ClientTimeout(total=10)) as session:
            async with session.get(f"{API_BASE_URL}/users/{user_id}") as response:
                if response.status == 404:
                    return None
                response.raise_for_status()
                return await response.json()
    except asyncio.TimeoutError:
        logger.error("user_data_fetch_timeout", user_id=user_id)
        raise TimeoutError(f"ユーザー情報取得タイムアウト: {user_id}")
    except aiohttp.ClientError as e:
        logger.error("user_data_fetch_error", user_id=user_id, error=str(e))
        raise HTTPError(f"ユーザー情報取得失敗: {user_id}") from e
```

#### 3. セキュリティ考慮の欠如

**問題**: AIはセキュリティ脅威を想定しない

```python
# ❌ AI生成の脆弱例: SQLインジェクション危険
def get_user_by_email(email):
    query = f"SELECT * FROM users WHERE email = '{email}'"
    return db.execute(query)

# ✅ セキュアな実装
def get_user_by_email(email: str) -> dict[str, Any] | None:
    """メールアドレスでユーザーを検索.

    Args:
        email: 検索対象のメールアドレス

    Returns:
        ユーザー情報、存在しない場合はNone

    Raises:
        ValueError: emailが無効な場合
    """
    if not email or "@" not in email:
        raise ValueError("有効なメールアドレスを指定してください")

    # パラメータ化クエリを使用
    query = "SELECT id, name, email, created_at FROM users WHERE email = ?"
    result = db.execute(query, (email,)).fetchone()
    return dict(result) if result else None
```

#### 4. パフォーマンス考慮の欠如

**問題**: AIは効率より「動くこと」を優先

```python
# ❌ AI生成の脆弱例: 非効率
def find_active_users(users):
    active = []
    for user in users:
        if user.get("status") == "active":
            active.append(user)
    return active

# ✅ 効率的な実装
def find_active_users(users: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """アクティブユーザーのみを抽出.

    Args:
        users: ユーザー情報のリスト

    Returns:
        アクティブユーザーのリスト
    """
    return [user for user in users if user.get("status") == "active"]
```

---

## 📝 仕様明文化テンプレート

### 関数仕様テンプレート

````python
def [関数名](
    [パラメータ]: [型],
    *,
    [キーワード専用パラメータ]: [型] = [デフォルト値],
) -> [戻り値型]:
    """[関数概要（1行）].

    [詳細説明：何をするのか、なぜ必要なのか]

    Args:
        [パラメータ名]: [説明（型情報含む）]
        [キーワード専用パラメータ名]: [説明（デフォルト値含む）]

    Returns:
        [戻り値の説明と型情報]

    Raises:
        [例外クラス]: [発生条件と意味]
        [例外クラス]: [発生条件と意味]

    Example:
        ```python
        # 使用例を具体的に記載
        result = [関数名]([引数例])
        ```

    Note:
        [重要な注意事項、実装上の制約など]
    """
````

### クラス仕様テンプレート

````python
class [クラス名]:
    """[クラス概要（1行）].

    [詳細説明：責務、ライフサイクル、使用場面]

    Attributes:
        [属性名] ([型]): [説明]
        [属性名] ([型]): [説明]

    Example:
        ```python
        # インスタンス化と使用例
        instance = [クラス名]([コンストラクタ引数])
        result = await instance.[メソッド名]([引数])
        ```

    Note:
        [スレッドセーフティ、ライフサイクルなどの重要事項]
    """
````

### API仕様テンプレート

````python
@app.[method]("/api/[エンドポイント]")
async def [関数名](
    [パラメータ]: [型],
    request: Request,
) -> [レスポンス型]:
    """[API概要（1行）].

    [詳細説明：何をするAPIか、認証要件など]

    Args:
        [パラメータ]: [説明]
        request: FastAPI Request オブジェクト

    Returns:
        [レスポンスの構造と意味]

    Raises:
        HTTPException: [HTTPステータスコードと発生条件]
            - 400: [バリデーションエラー]
            - 401: [認証エラー]
            - 403: [権限エラー]
            - 404: [リソース未発見]
            - 500: [内部エラー]

    Example:
        ```bash
        # curl コマンド例
        curl -X [METHOD] "http://localhost:8000/api/[エンドポイント]" \
             -H "Content-Type: application/json" \
             -d '[リクエストボディ例]'
        ```
    """
````

---

## 🧪 自動テスト要求ルール

### テストカバレッジ 100% 必須

**AI生成コードは必ずユニットテストを作成し、100% カバレッジを達成する**

```python
# ✅ テスト必須の関数
def calculate_discount(price: float, discount_rate: float) -> float:
    """割引額を計算する.

    Args:
        price: 原価
        discount_rate: 割引率（0.0-1.0）

    Returns:
        割引後の価格
    """
    if not isinstance(price, (int, float)) or price < 0:
        raise ValueError("価格は0以上の数値を指定してください")
    if not isinstance(discount_rate, (int, float)) or not (0 <= discount_rate <= 1):
        raise ValueError("割引率は0.0-1.0の範囲で指定してください")

    return price * (1 - discount_rate)

# 🧪 対応するテスト（100% カバレッジ）
@pytest.mark.parametrize("price,discount_rate,expected", [
    (100, 0.1, 90),
    (200, 0.2, 160),
    (50, 0, 50),
    (75, 1, 0),
])
def test_calculate_discount_valid(price, discount_rate, expected):
    """正常系のテスト."""
    assert calculate_discount(price, discount_rate) == expected

@pytest.mark.parametrize("price,discount_rate", [
    (-10, 0.1),      # 負の価格
    (100, -0.1),     # 負の割引率
    (100, 1.1),      # 1を超える割引率
    ("100", 0.1),    # 文字列価格
    (100, "0.1"),    # 文字列割引率
])
def test_calculate_discount_invalid(price, discount_rate):
    """異常系のテスト."""
    with pytest.raises(ValueError):
        calculate_discount(price, discount_rate)
```

### テスト設計原則

1. **境界値テスト**: 最小値・最大値・境界値
2. **異常系テスト**: エラー条件・例外ケース
3. **パラメータ化テスト**: 複数条件の一括テスト
4. **モック・スタブ**: 外部依存の隔離

```python
# ✅ 包括的なテスト例
class TestUserService:
    @pytest.fixture
    async def user_service(self):
        """テスト用のUserServiceインスタンス."""
        return UserService(db=MockDatabase())

    @pytest.mark.asyncio
    async def test_create_user_success(self, user_service):
        """ユーザー作成成功ケース."""
        user_data = {"name": "Test User", "email": "test@example.com"}

        result = await user_service.create_user(user_data)

        assert result["id"] is not None
        assert result["name"] == user_data["name"]
        assert result["email"] == user_data["email"]

    @pytest.mark.asyncio
    async def test_create_user_duplicate_email(self, user_service):
        """重複メールアドレスエラー."""
        user_data = {"name": "Test User", "email": "existing@example.com"}

        with pytest.raises(UserAlreadyExistsError):
            await user_service.create_user(user_data)

    @pytest.mark.asyncio
    async def test_get_user_not_found(self, user_service):
        """ユーザー未発見ケース."""
        with pytest.raises(UserNotFoundError):
            await user_service.get_user("nonexistent-id")
```

---

## 🔍 コードレビューポイント

### AI生成コード特有のチェック項目

#### 1. 意図の明確性

- [ ] 関数/メソッドの目的が明確か
- [ ] 変数名が何を表しているか明確か
- [ ] ビジネスロジックが仕様と一致するか

#### 2. エラー処理の完全性

- [ ] 全ての例外ケースを考慮しているか
- [ ] エラーメッセージが具体的か
- [ ] 適切な例外クラスを使用しているか

#### 3. セキュリティ

- [ ] 入力検証が適切か
- [ ] SQLインジェクション対策済みか
- [ ] XSS対策済みか
- [ ] 機密情報がログに記録されていないか

#### 4. パフォーマンス

- [ ] アルゴリズムの時間計算量が適切か
- [ ] メモリ使用量が効率的か
- [ ] 非同期処理が適切に使用されているか

#### 5. 保守性

- [ ] 関数が単一責務か
- [ ] 循環複雑度が10以下か
- [ ] ドキュメントが完全か

### レビュアーチェックリスト

```markdown
## AI生成コードレビュー チェックリスト

### 仕様確認

- [ ] 仕様書と実装が一致している
- [ ] 要件定義を満たしている
- [ ] エッジケースを考慮している

### コード品質

- [ ] 型アノテーションが100%付与されている
- [ ] エラー処理が包括的である
- [ ] セキュリティ対策が施されている
- [ ] パフォーマンスが最適化されている

### テスト品質

- [ ] ユニットテストが100%カバレッジ
- [ ] 異常系テストが網羅的
- [ ] モック/スタブが適切に使用されている

### ドキュメント

- [ ] Docstringが完全（Googleスタイル）
- [ ] 型情報が明確
- [ ] 使用例が記載されている
```

---

## 🚪 品質ゲート

### コミット前ゲート

```bash
#!/bin/bash
# pre-commit.sh - AI生成コード品質チェック

# 1. 型チェック
echo "🔍 型チェック実行..."
mypy agentflow --ignore-missing-imports
if [ $? -ne 0 ]; then
    echo "❌ 型チェック失敗"
    exit 1
fi

# 2. リントチェック
echo "🔍 リントチェック実行..."
ruff check .
if [ $? -ne 0 ]; then
    echo "❌ リントチェック失敗"
    exit 1
fi

# 3. テスト実行
echo "🧪 テスト実行..."
pytest --cov=agentflow --cov-fail-under=80 --tb=short
if [ $? -ne 0 ]; then
    echo "❌ テスト失敗"
    exit 1
fi

echo "✅ 全ての品質チェック通過"
```

### マージ前ゲート

```bash
#!/bin/bash
# pre-merge.sh - 包括的品質チェック

# 1. AI生成コードの仕様確認
echo "🤖 AI生成コード仕様確認..."
python scripts/verify_ai_code_specs.py
if [ $? -ne 0 ]; then
    echo "❌ 仕様確認失敗"
    exit 1
fi

# 2. セキュリティスキャン
echo "🔒 セキュリティスキャン..."
bandit -r agentflow
if [ $? -ne 0 ]; then
    echo "❌ セキュリティ問題検出"
    exit 1
fi

# 3. パフォーマンスチェック
echo "⚡ パフォーマンスチェック..."
python scripts/performance_check.py
if [ $? -ne 0 ]; then
    echo "❌ パフォーマンス問題検出"
    exit 1
fi

echo "✅ マージ前品質チェック通過"
```

---

## 🤖 自動化スクリプト

### AIコード品質検証スクリプト

```python
#!/usr/bin/env python3
# scripts/verify_ai_code_specs.py

import ast
import re
from pathlib import Path
from typing import List, Dict, Any

class AICodeVerifier:
    """AI生成コードの品質を検証する."""

    def __init__(self, source_dir: str = "agentflow"):
        self.source_dir = Path(source_dir)
        self.errors: List[str] = []

    def verify_all(self) -> bool:
        """全てのAI生成コードを検証."""
        python_files = self.source_dir.rglob("*.py")

        for file_path in python_files:
            if self._is_ai_generated(file_path):
                self._verify_file(file_path)

        if self.errors:
            print("❌ AIコード品質エラー:")
            for error in self.errors:
                print(f"  - {error}")
            return False

        print("✅ AIコード品質チェック通過")
        return True

    def _is_ai_generated(self, file_path: Path) -> bool:
        """ファイルがAI生成かどうかを判定."""
        content = file_path.read_text()

        # AI生成の兆候をチェック
        ai_indicators = [
            "# Generated by AI",
            "# AI-generated code",
            "# Created with AI assistance",
            "@ai_generated"  # カスタムデコレータ
        ]

        return any(indicator in content for indicator in ai_indicators)

    def _verify_file(self, file_path: Path):
        """単一ファイルを検証."""
        try:
            tree = ast.parse(file_path.read_text())
            self._verify_functions(tree, file_path)
            self._verify_classes(tree, file_path)
            self._verify_error_handling(tree, file_path)
        except SyntaxError as e:
            self.errors.append(f"{file_path}: 構文エラー - {e}")

    def _verify_functions(self, tree: ast.AST, file_path: Path):
        """関数定義を検証."""
        for node in ast.walk(tree):
            if isinstance(node, ast.FunctionDef):
                self._verify_function_docstring(node, file_path)
                self._verify_function_annotations(node, file_path)
                self._verify_function_error_handling(node, file_path)

    def _verify_function_docstring(self, node: ast.FunctionDef, file_path: Path):
        """Docstringを検証."""
        if not ast.get_docstring(node):
            self.errors.append(f"{file_path}:{node.lineno}: {node.name} - Docstringなし")

        docstring = ast.get_docstring(node)
        if docstring and not self._has_required_docstring_sections(docstring):
            self.errors.append(f"{file_path}:{node.lineno}: {node.name} - Docstring不完全")

    def _verify_function_annotations(self, node: ast.FunctionDef, file_path: Path):
        """型アノテーションを検証."""
        # 戻り値アノテーション
        if not node.returns:
            self.errors.append(f"{file_path}:{node.lineno}: {node.name} - 戻り値型アノテーションなし")

        # パラメータアノテーション
        for arg in node.args.args:
            if not arg.annotation:
                self.errors.append(f"{file_path}:{node.lineno}: {node.name} - パラメータ {arg.arg} の型アノテーションなし")

    def _verify_function_error_handling(self, node: ast.FunctionDef, file_path: Path):
        """エラー処理を検証."""
        has_try_except = any(isinstance(child, ast.Try) for child in ast.walk(node))

        # 外部API呼び出し等のリスク操作がある場合
        has_risky_operations = any(
            isinstance(child, (ast.Call, ast.Await))
            for child in ast.walk(node)
        )

        if has_risky_operations and not has_try_except:
            self.errors.append(f"{file_path}:{node.lineno}: {node.name} - リスク操作にエラー処理なし")

    def _has_required_docstring_sections(self, docstring: str) -> bool:
        """必要なDocstringセクションがあるか検証."""
        required_sections = ["Args:", "Returns:"]
        return all(section in docstring for section in required_sections)

if __name__ == "__main__":
    verifier = AICodeVerifier()
    success = verifier.verify_all()
    exit(0 if success else 1)
```

### テストカバレッジ検証スクリプト

```python
#!/usr/bin/env python3
# scripts/verify_test_coverage.py

import subprocess
import json
import sys
from pathlib import Path

def verify_ai_code_test_coverage():
    """AI生成コードのテストカバレッジを検証."""

    # カバレッジ実行
    result = subprocess.run([
        "pytest",
        "--cov=agentflow",
        "--cov-report=json:coverage.json",
        "--cov-report=term-missing"
    ], capture_output=True, text=True)

    if result.returncode != 0:
        print("❌ テスト実行失敗")
        print(result.stdout)
        print(result.stderr)
        return False

    # カバレッジレポート読み込み
    coverage_file = Path("coverage.json")
    if not coverage_file.exists():
        print("❌ カバレッジレポートが見つかりません")
        return False

    with open(coverage_file) as f:
        coverage_data = json.load(f)

    # AI生成ファイルの100%カバレッジを検証
    ai_files_missing_coverage = []

    for file_path, file_data in coverage_data["files"].items():
        if _is_ai_generated_file(file_path):
            missing_lines = file_data.get("missing_lines", [])
            if missing_lines:
                ai_files_missing_coverage.append((file_path, missing_lines))

    if ai_files_missing_coverage:
        print("❌ AI生成ファイルのテストカバレッジ不足:")
        for file_path, missing_lines in ai_files_missing_coverage:
            print(f"  📁 {file_path}")
            print(f"    未カバー行: {missing_lines}")
        return False

    print("✅ AI生成コードのテストカバレッジ100%達成")
    return True

def _is_ai_generated_file(file_path: str) -> bool:
    """ファイルがAI生成かどうかを判定."""
    try:
        content = Path(file_path).read_text()
        ai_indicators = [
            "# Generated by AI",
            "# AI-generated code",
            "@ai_generated"
        ]
        return any(indicator in content for indicator in ai_indicators)
    except:
        return False

if __name__ == "__main__":
    success = verify_ai_code_test_coverage()
    sys.exit(0 if success else 1)
```

---

## 📋 AI弱点補強 チートシート

| 弱点                   | 補強策                 | 自動化                       |
| ---------------------- | ---------------------- | ---------------------------- |
| **意図曖昧**           | 仕様明文化テンプレート | Docstring検証スクリプト      |
| **エラー処理欠如**     | 包括的例外処理         | エラー処理検証スクリプト     |
| **セキュリティ無視**   | 入力検証・サニタイズ   | セキュリティスキャナー       |
| **パフォーマンス無視** | アルゴリズム見直し     | パフォーマンスプロファイラー |
| **テストなし**         | 100%カバレッジ必須     | カバレッジ検証スクリプト     |

_最終更新: 2026-01-19 | AgentFlow AI生成コード品質保証_
