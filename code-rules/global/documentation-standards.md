# ドキュメント規約

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Framework 全コードベース
> **最終更新**: 2025-01-19

## 📋 目次

1. [ドキュメント原則](#ドキュメント原則)
2. [Docstring 規約](#docstring-規約)
3. [コメント規約](#コメント規約)
4. [ドキュメント生成](#ドキュメント生成)
5. [例とテンプレート](#例とテンプレート)

---

## 🎯 ドキュメント原則

### 必須原則
- **100% カバレッジ**: すべての公開 API にドキュメント必須
- **継続的更新**: コード変更時にドキュメントを同期
- **実用的価値**: ドキュメントが開発者の役に立つ

### 品質基準
```python
# ✅ 良いドキュメント
def process_user_data(user_id: str, data: dict) -> dict:
    """
    指定されたユーザーのデータを処理する。

    この関数はユーザーデータの検証、変換、保存を実行する。
    エラーが発生した場合は適切な例外を発生させる。

    Args:
        user_id: 処理対象のユーザーID。UUID形式である必要がある。
        data: 処理するユーザーデータ。以下のキーを含む:
            - name: ユーザー名（文字列）
            - email: メールアドレス（文字列）
            - age: 年齢（整数、オプション）

    Returns:
        処理結果を含む辞書:
        {
            "status": "success|error",
            "user_id": "処理されたユーザーID",
            "processed_at": "処理時刻のISOフォーマット文字列"
        }

    Raises:
        ValueError: user_id が無効な場合、または data が不完全な場合
        DatabaseError: データベース操作に失敗した場合

    Example:
        >>> result = process_user_data(
        ...     "123e4567-e89b-12d3-a456-426614174000",
        ...     {"name": "John Doe", "email": "john@example.com", "age": 30}
        ... )
        >>> result["status"]
        'success'
    """
```

---

## 📝 Docstring 規約

### Google スタイル（必須）
```python
def function_name(
    param1: Type,
    param2: Type,
    *,
    keyword_param: Type = default
) -> ReturnType:
    """一文で関数を説明する。

    より詳細な説明があればここに記載。
    複数行にわたる場合もある。

    Args:
        param1: パラメータ1の説明。
        param2: パラメータ2の説明。
        keyword_param: キーワード専用パラメータの説明。
                      デフォルト値: default

    Returns:
        戻り値の説明。型と意味を明確に。

    Raises:
        ExceptionType: 発生条件の説明。
        AnotherException: 別の例外の説明。

    Example:
        >>> result = function_name("arg1", "arg2", keyword_param="value")
        >>> print(result)
        expected_output

    Note:
        重要な注意事項があればここに記載。
    """
```

### クラス Docstring
```python
class UserManager:
    """
    ユーザー管理を担当するクラス。

    このクラスはユーザーの作成、更新、削除、検索機能を提供する。
    すべての操作はトランザクション内で実行され、
    データの一貫性を保証する。

    Attributes:
        database_url: データベース接続URL
        connection_pool: 接続プールインスタンス

    Example:
        >>> manager = UserManager("postgresql://localhost/db")
        >>> user = manager.create_user({"name": "John", "email": "john@example.com"})
        >>> user.id
        '123e4567-e89b-12d3-a456-426614174000'
    """

    def __init__(self, database_url: str) -> None:
        """UserManager を初期化する。

        Args:
            database_url: データベース接続URL。
                         postgresql://user:pass@host:port/db 形式。
        """
```

### モジュール Docstring
```python
"""
ユーザー管理モジュール。

このモジュールはユーザー関連のすべての機能を提供する。
主な機能:
- ユーザー CRUD 操作
- 認証・認可
- プロフィール管理
- パスワードリセット

Classes:
    UserManager: ユーザー管理メインクラス
    UserValidator: ユーザー検証クラス
    AuthenticationService: 認証サービス

Functions:
    create_user: 新規ユーザー作成
    authenticate_user: ユーザー認証
    reset_password: パスワードリセット

Examples:
    基本的な使用例:

    >>> from user_management import UserManager
    >>> manager = UserManager()
    >>> user = manager.create_user({"name": "John", "email": "john@example.com"})
"""

# モジュールレベル定数
DEFAULT_USER_ROLE = "user"
MAX_LOGIN_ATTEMPTS = 5
```

---

## 💬 コメント規約

### インラインコメント
```python
# ✅ 良いコメント: なぜそうするのかを説明
user_count = len(users)  # アクティブユーザーのみをカウントするため len() を使用

# ✅ 複雑なロジックを説明
if user.status == "active" and user.last_login > threshold:
    # アクティブユーザーかつ最近ログインしている場合のみ処理
    # これにより無効化されたアカウントを除外
    process_user(user)

# ❌ 悪いコメント: 何をしているかはコードから明らか
user_count = len(users)  # ユーザー数を数える

# ❌ 悪いコメント: コードと矛盾
if user.is_active:  # 非アクティブユーザーのみ処理 ❌
    process_user(user)
```

### ブロックコメント
```python
# 複数行のロジックを説明する場合
#
# このセクションではユーザーデータのマイグレーションを実行する。
# 以下のステップで処理:
# 1. 古いフォーマットのデータを特定
# 2. 新しいスキーマに変換
# 3. 参照整合性を検証
# 4. トランザクション内で更新

def migrate_user_data():
    # ステップ1: 移行対象データの特定
    old_format_users = query_old_format_users()

    for user in old_format_users:
        # ステップ2: データ変換
        new_data = convert_user_format(user)

        # ステップ3: 検証
        validate_migrated_data(new_data)

        # ステップ4: 更新
        update_user_in_transaction(user.id, new_data)
```

### TODO コメント
```python
# TODO(#123): パフォーマンス最適化 - バッチ処理を実装
# この関数は現在 O(n) の複雑度だが、データ量増加により
# パフォーマンス問題が発生する可能性がある。
# バッチサイズをパラメータ化し、並行処理を検討。
def process_large_dataset(data: list[dict]) -> list[dict]:
    return [process_item(item) for item in data]

# FIXME(#456): メモリリーク修正
# この実装では一時ファイルが適切にクリーンアップされない。
# コンテキストマネージャーを使用して確実にクローズする。
def create_temp_file() -> str:
    temp_file = tempfile.NamedTemporaryFile(delete=False)
    return temp_file.name  # リークの原因

# NOTE: 将来の拡張ポイント
# このクラスは将来的にプラグインアーキテクチャをサポート予定。
# PluginManager クラスを追加する際はここを起点に設計。
class BaseProcessor:
    pass
```

---

## 📚 ドキュメント生成

### Sphinx ドキュメント
```python
# conf.py 設定
extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.napoleon',  # Google/NumPy スタイル対応
    'sphinx.ext.viewcode',
    'sphinx.ext.coverage',
]

# autodoc 設定
autodoc_default_options = {
    'members': True,
    'undoc-members': True,
    'show-inheritance': True,
    'member-order': 'bysource',
}

# Napoleon 設定 (Google スタイル)
napoleon_google_docstring = True
napoleon_numpy_docstring = False
napoleon_include_init_with_doc = False
napoleon_include_private_with_doc = False
```

### MkDocs 設定（README ドキュメント）
```yaml
# mkdocs.yml
site_name: AgentFlow Documentation
theme:
  name: material
  palette:
    primary: blue
    accent: light-blue

plugins:
  - search
  - mkdocstrings:
      handlers:
        python:
          options:
            show_source: true
            show_root_heading: true
            heading_level: 2

nav:
  - Home: index.md
  - API Reference: api.md
  - User Guide: guide/
  - Developer Guide: development/
```

---

## 📝 例とテンプレート

### 関数ドキュメントテンプレート
```python
async def execute_workflow(
    workflow_id: str,
    inputs: dict[str, Any],
    *,
    timeout: float = 30.0,
    retry_count: int = 0,
) -> dict[str, Any]:
    """
    指定されたワークフローを実行する。

    ワークフロー定義に基づいてエージェントを調整・実行し、
    結果を収集して返す。タイムアウトやリトライ処理を含む。

    Args:
        workflow_id: 実行対象のワークフローID。
                    WorkflowDefinition.id と一致する必要がある。
        inputs: ワークフロー実行時の入力パラメータ。
               ワークフロー定義の input_schema に準拠する必要がある。
        timeout: 実行タイムアウト（秒）。
                デフォルト: 30.0
        retry_count: リトライ回数。
                    デフォルト: 0（リトライなし）

    Returns:
        実行結果を含む辞書:
        {
            "status": "success|failed|timeout",
            "result": {...},  # ワークフロー出力
            "execution_time": 15.2,  # 実行時間（秒）
            "agent_count": 3,  # 使用されたエージェント数
            "error": "..."  # エラー発生時のエラーメッセージ
        }

    Raises:
        WorkflowNotFoundError: 指定されたワークフローが存在しない場合
        WorkflowExecutionError: ワークフロー実行中に予期しないエラーが発生した場合
        TimeoutError: 指定されたタイムアウト時間を超えた場合

    Example:
        基本的なワークフロー実行:

        >>> result = await execute_workflow(
        ...     "user-onboarding-workflow",
        ...     {"user_id": "123", "department": "engineering"}
        ... )
        >>> result["status"]
        'success'

    Example:
        リトライ付き実行:

        >>> result = await execute_workflow(
        ...     "data-processing-workflow",
        ...     {"data": large_dataset},
        ...     timeout=60.0,
        ...     retry_count=2
        ... )

    Note:
        - 大量データを処理する場合、timeout を適切に設定すること
        - retry_count を設定すると、失敗時に自動リトライされる
        - 実行時間はネットワーク遅延により変動する可能性がある
    """
```

### クラスドキュメントテンプレート
```python
class AgentCoordinator:
    """
    複数エージェントの調整・実行を管理するクラス。

    このクラスはワークフロー定義に基づいて適切なエージェントを選択し、
    並行または順次実行を管理する。エラーハンドリングとリトライロジックを含む。

    Attributes:
        workflow_config: ワークフロー設定辞書
        agent_registry: 使用可能なエージェントレジストリ
        execution_context: 現在の実行コンテキスト
        max_concurrent_agents: 最大並行実行エージェント数（デフォルト: 5）

    Example:
        基本的な使用例:

        >>> coordinator = AgentCoordinator({
        ...     "max_concurrent": 3,
        ...     "timeout": 30.0
        ... })
        >>> await coordinator.initialize()
        >>> result = await coordinator.execute_workflow(workflow_def, inputs)

    Note:
        AgentCoordinator はスレッドセーフではない。
        複数のワークフローを並行実行する場合は、インスタンスを分離すること。
    """

    def __init__(
        self,
        workflow_config: dict[str, Any],
        *,
        max_concurrent_agents: int = 5,
    ) -> None:
        """
        AgentCoordinator を初期化する。

        Args:
            workflow_config: ワークフロー実行設定。
                           timeout, retry_policy 等を含む。
            max_concurrent_agents: 最大並行実行エージェント数。
                                 デフォルト: 5
        """
```

### モジュールドキュメントテンプレート
```python
"""
ワークフロー実行エンジンモジュール。

このモジュールは AgentFlow のコア実行エンジンを提供する。
ワークフロー定義の解析、エージェントの調整、実行監視、結果収集を行う。

主な機能:
- ワークフロー定義の検証と実行
- エージェントの動的選択と調整
- 実行進捗のリアルタイム監視
- エラーハンドリングとリカバリー
- パフォーマンス指標の収集

アーキテクチャ:
- WorkflowEngine: メイン実行エンジン
- AgentCoordinator: エージェント調整クラス
- ExecutionMonitor: 実行監視クラス
- ResultCollector: 結果収集クラス

使用例:
    基本的なワークフロー実行:

    >>> from agentflow.core import WorkflowEngine
    >>>
    >>> engine = WorkflowEngine()
    >>> workflow = engine.load_workflow("user-onboarding.yaml")
    >>> result = await engine.execute(workflow, {"user_id": "123"})

    カスタム設定での実行:

    >>> engine = WorkflowEngine({
    ...     "max_concurrent_agents": 10,
    ...     "execution_timeout": 60.0,
    ...     "enable_monitoring": True
    ... })
    >>> result = await engine.execute(workflow, inputs)

注意事項:
- ワークフロー定義は事前に validate_workflow() で検証すること
- 大規模実行時はリソース使用量に注意
- エラーハンドリングは呼び出し元で適切に行うこと

関連モジュール:
- agentflow.core.config: 設定管理
- agentflow.core.exceptions: 例外定義
- agentflow.agents: エージェント実装
"""

# モジュールレベル定数
DEFAULT_EXECUTION_TIMEOUT = 30.0
MAX_CONCURRENT_AGENTS = 5
SUPPORTED_WORKFLOW_VERSIONS = ["1.0", "1.1"]
```

---

## 🔍 ドキュメント品質チェック

### 必須チェックリスト
- [ ] すべての公開関数/クラス/メソッドに Docstring がある
- [ ] Docstring が Google スタイルに従っている
- [ ] Args, Returns, Raises が適切に記述されている
- [ ] 実用的価値のある Example が含まれている
- [ ] 複雑なロジックに説明コメントがある
- [ ] TODO/FIXME コメントに Issue 番号が付いている
- [ ] コード変更時にドキュメントが同期されている

### 自動化チェック
```bash
# Docstring カバレッジチェック
interrogate -v --fail-under=100 agentflow/

# ドキュメント生成テスト
sphinx-build -b html docs/ _build/html

# リンク切れチェック
sphinx-build -b linkcheck docs/ _build/linkcheck
```

---

## 📚 関連ドキュメント

- [**コーディング規約**](coding-standards.md) - コード品質基準
- [**命名規約**](naming-conventions.md) - 命名規則
- [**アーキテクチャ設計**](architecture-guidelines.md) - 設計原則
- [**APIドキュメント**](../../../docs/api.md) - API リファレンス

---

**優れたドキュメントはコードの価値を最大化します。** 📚

*最終更新: 2025-01-19 | バージョン: 1.0.0*