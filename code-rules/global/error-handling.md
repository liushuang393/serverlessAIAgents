# 例外/エラー処理

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow 全 Python コード
> **最終更新**: 2026-01-19

## 📋 目次

1. [基本原則](#基本原則)
2. [例外クラス設計](#例外クラス設計)
3. [エラーハンドリングパターン](#エラーハンドリングパターン)
4. [ログ記録](#ログ記録)
5. [リカバリ戦略](#リカバリ戦略)
6. [非同期エラー処理](#非同期エラー処理)
7. [テスト時のエラー検証](#テスト時のエラー検証)
8. [自動化チェック](#自動化チェック)

---

## 🎯 基本原則

### Fail Fast
- **早期失敗**: 問題を早期に検知・報告
- **明確なエラーメッセージ**: 何が問題でどう対処すべきか
- **例外の再送出**: 握りつぶし禁止

```python
# ✅ Fail Fast: 問題を即座に報告
async def validate_workflow(workflow: WorkflowDefinition) -> None:
    if not workflow.nodes:
        raise WorkflowValidationError(
            f"ワークフローにノードがありません: {workflow.workflow_id}",
            workflow_id=workflow.workflow_id,
        )

    if workflow.workflow_id == "":
        raise WorkflowValidationError(
            "ワークフローIDが空です",
            workflow_id=workflow.workflow_id,
        )
```

### 具体的な例外
- **bare `except` 禁止**: 具体的な例外クラスを使用
- **適切な例外階層**: 業務ロジックに適した例外

```python
# ✅ 具体的な例外
try:
    result = await self.llm.chat(messages)
except LLMAPIError as e:
    logger.error("llm_api_failed", error=str(e), model=self.model_name)
    raise
except asyncio.TimeoutError:
    logger.error("llm_timeout", timeout=self.timeout)
    raise LLMTimeoutError(f"LLM 応答タイムアウト: {self.timeout}s")

# ❌ 禁止: bare except
try:
    result = await risky_operation()
except:  # 何が起きても握りつぶす
    pass
```

---

## 🏗️ 例外クラス設計

### 例外階層
```
AgentFlowError (基底)
├── WorkflowError (ワークフロー関連)
│   ├── WorkflowNotFoundError
│   ├── WorkflowValidationError
│   └── WorkflowExecutionError
├── DeployError (デプロイ関連)
│   ├── DeployConfigError
│   ├── DeployTargetError
│   └── DeployTimeoutError
├── ProtocolError (プロトコル関連)
│   ├── MCPError
│   ├── A2AError
│   └── AGUIError
└── InfrastructureError (インフラ関連)
    ├── LLMError
    │   ├── LLMAPIError
    │   ├── LLMTimeoutError
    │   └── LLMQuotaError
    ├── DatabaseError
    └── CacheError
```

### 例外クラス実装
- **データ属性**: エラーコンテキストを保持
- **メッセージ**: 明確で実用的

```python
# ✅ 適切な例外クラス
class WorkflowNotFoundError(AgentFlowError):
    """ワークフローが見つからない場合の例外."""

    def __init__(self, workflow_id: str, message: str | None = None):
        self.workflow_id = workflow_id
        super().__init__(
            message or f"ワークフローが見つかりません: {workflow_id}"
        )

class LLMAPIError(AgentFlowError):
    """LLM API エラーの例外."""

    def __init__(
        self,
        message: str,
        provider: str,
        model: str,
        status_code: int | None = None,
    ):
        self.provider = provider
        self.model = model
        self.status_code = status_code
        super().__init__(message)
```

---

## 🎛️ エラーハンドリングパターン

### サービス層ハンドリング
- **ログ記録**: エラーをログに記録
- **変換**: 下位層例外を業務例外に変換
- **再送出**: 呼び出し元に適切な例外を投げる

```python
# ✅ サービス層でのエラーハンドリング
class PublishService:
    async def generate_code(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
    ) -> GeneratedCode:
        try:
            # ワークフロー検証
            await self._validate_workflow(workflow)

            # コード生成
            return await self.code_generator.generate(workflow, output_type)

        except WorkflowValidationError:
            # 業務例外はそのまま再送出
            raise
        except Exception as e:
            # 予期せぬエラーはログ記録後、業務例外に変換
            logger.error(
                "code_generation_failed",
                workflow_id=workflow.workflow_id,
                output_type=output_type.value,
                error=str(e),
                exc_info=True,
            )
            raise CodeGenerationError(
                f"コード生成に失敗しました: {workflow.workflow_id}"
            ) from e
```

### API層ハンドリング
- **HTTP ステータス**: 適切なHTTPステータスコード
- **エラーレスポンス**: 構造化されたエラー情報

```python
# ✅ API層でのエラーハンドリング
@app.exception_handler(WorkflowNotFoundError)
async def handle_workflow_not_found(
    request: Request,
    exc: WorkflowNotFoundError,
) -> JSONResponse:
    return JSONResponse(
        status_code=404,
        content={
            "error": {
                "type": "workflow_not_found",
                "message": exc.message,
                "workflow_id": exc.workflow_id,
            }
        }
    )

@app.exception_handler(AgentFlowError)
async def handle_agentflow_error(
    request: Request,
    exc: AgentFlowError,
) -> JSONResponse:
    logger.error(
        "api_error",
        error_type=type(exc).__name__,
        error_message=str(exc),
        path=request.url.path,
        exc_info=True,
    )

    return JSONResponse(
        status_code=500,
        content={
            "error": {
                "type": "internal_error",
                "message": "内部エラーが発生しました",
            }
        }
    )
```

---

## 📊 ログ記録

### structlog の使用
- **構造化ログ**: JSON 形式のログ
- **コンテキスト情報**: エラーに関連する情報を含む

```python
import structlog

logger = structlog.get_logger()

# ✅ 構造化ログ
try:
    result = await deploy_to_target(code, config)
except DeployError as e:
    logger.error(
        "deployment_failed",
        workflow_id=workflow.workflow_id,
        target=config.target.value,
        error=str(e),
        duration=time.time() - start_time,
        exc_info=True,
    )
    raise
```

### ログレベル
- **ERROR**: システムエラー、業務例外
- **WARNING**: 警告、再試行可能なエラー
- **INFO**: 正常処理の重要な情報
- **DEBUG**: 開発時の詳細情報

```python
# ✅ 適切なログレベル
logger.info("workflow_execution_started", workflow_id=workflow.workflow_id)
logger.debug("llm_request", messages=messages, model=model)

try:
    result = await llm.chat(messages)
except LLMAPIError as e:
    logger.warning("llm_retry_attempt", attempt=attempt, error=str(e))
    if attempt >= max_retries:
        logger.error("llm_max_retries_exceeded", max_retries=max_retries)
        raise
```

---

## 🔄 リカバリ戦略

### リトライパターン
- **指数バックオフ**: 負荷を分散
- **最大リトライ回数**: 無限ループ防止
- **再試行条件**: 再試行可能なエラーのみ

```python
# ✅ リトライパターン
async def execute_with_retry(
    operation: Callable[[], Awaitable[T]],
    max_retries: int = 3,
    base_delay: float = 1.0,
) -> T:
    last_exception = None

    for attempt in range(max_retries + 1):
        try:
            return await operation()
        except RETRYABLE_EXCEPTIONS as e:
            last_exception = e

            if attempt < max_retries:
                delay = base_delay * (2 ** attempt)
                logger.warning(
                    "operation_retry",
                    attempt=attempt + 1,
                    max_retries=max_retries,
                    delay=delay,
                    error=str(e),
                )
                await asyncio.sleep(delay)
            else:
                logger.error(
                    "operation_failed_after_retries",
                    max_retries=max_retries,
                    error=str(e),
                )
                raise last_exception from e
```

### フォールバック
- **代替実装**: 主要機能が失敗した場合の代替
- **部分的成功**: 完全失敗より部分成功を優先

```python
# ✅ フォールバックパターン
async def get_embedding_with_fallback(
    text: str,
    preferred_provider: str = "openai",
) -> list[float]:
    """埋め込み取得（フォールバック対応）."""

    providers = [preferred_provider] + ["sentence-transformer", "mock"]

    for provider in providers:
        try:
            return await get_embedding(text, provider=provider)
        except EmbeddingError as e:
            logger.warning(
                "embedding_provider_failed",
                provider=provider,
                text_length=len(text),
                error=str(e),
            )
            continue

    raise EmbeddingError(f"全ての埋め込みプロバイダーが失敗しました")
```

---

## ⚡ 非同期エラー処理

### Task 例外処理
- **gather の例外**: 個別例外を処理
- **キャンセル**: 適切なクリーンアップ

```python
# ✅ 非同期タスクの例外処理
async def run_parallel_agents(
    agents: list[AgentBlock],
    inputs: dict[str, Any],
) -> list[Result]:
    """複数のAgentを並行実行."""

    async def run_single_agent(agent: AgentBlock) -> Result:
        try:
            return await agent.run(inputs)
        except Exception as e:
            logger.error(
                "agent_execution_failed",
                agent_name=agent.__class__.__name__,
                error=str(e),
                exc_info=True,
            )
            raise AgentExecutionError(
                f"Agent実行失敗: {agent.__class__.__name__}"
            ) from e

    tasks = [run_single_agent(agent) for agent in agents]

    try:
        return await asyncio.gather(*tasks)
    except Exception as e:
        # 最初の例外で全てのタスクをキャンセル
        for task in tasks:
            if not task.done():
                task.cancel()

        try:
            await asyncio.gather(*tasks, return_exceptions=True)
        except asyncio.CancelledError:
            pass

        raise e
```

### タイムアウト処理
- **asyncio.wait_for**: タイムアウト設定
- **TimeoutError**: 適切な例外変換

```python
# ✅ タイムアウト処理
async def execute_with_timeout(
    operation: Callable[[], Awaitable[T]],
    timeout: float = 30.0,
) -> T:
    try:
        return await asyncio.wait_for(operation(), timeout=timeout)
    except asyncio.TimeoutError:
        logger.error("operation_timeout", timeout=timeout)
        raise OperationTimeoutError(f"操作がタイムアウトしました: {timeout}s")
```

---

## 🧪 テスト時のエラー検証

### 例外テスト
- **pytest.raises**: 期待される例外を検証
- **例外属性**: エラーメッセージと属性を検証

```python
# ✅ 例外テスト
import pytest

def test_workflow_not_found_error():
    """ワークフロー未発見エラーのテスト."""

    with pytest.raises(WorkflowNotFoundError) as exc_info:
        raise WorkflowNotFoundError("wf-123")

    error = exc_info.value
    assert error.workflow_id == "wf-123"
    assert "wf-123" in str(error)

async def test_llm_timeout_error():
    """LLMタイムアウトエラーのテスト."""

    with pytest.raises(LLMTimeoutError) as exc_info:
        async def slow_operation():
            await asyncio.sleep(10)

        await asyncio.wait_for(slow_operation(), timeout=1)

    error = exc_info.value
    assert "タイムアウト" in str(error)
```

### エラーハンドリングテスト
- **モック**: 例外発生をシミュレート
- **アサーション**: エラーが適切に処理されるか

```python
# ✅ エラーハンドリングテスト
async def test_service_error_handling(mocker):
    """サービス層のエラーハンドリングテスト."""

    # LLM が例外を投げるようにモック
    mock_llm = mocker.patch("agentflow.llm.get_llm")
    mock_llm.return_value.chat.side_effect = LLMAPIError("API Error")

    service = PreviewService()

    with pytest.raises(CodeGenerationError) as exc_info:
        await service.generate_code(mock_workflow, CodeOutputType.BACKEND)

    error = exc_info.value
    assert "コード生成に失敗" in str(error)
    # ログが記録されていることを検証
    # （実際のテストでは caplog フィクスチャを使用）
```

---

## ✅ 自動化チェック

### 静的解析設定
```toml
[tool.ruff.lint]
select = [
    "BLE",  # blind-except (bare except)
    "TRY",  # try-except patterns
]

[tool.ruff.lint.flake8-blind-except]
extend-ignore = ["BLE001"]  # 必要に応じて除外
```

### テスト設定
```toml
[tool.pytest.ini_options]
filterwarnings = [
    "error",  # 警告をエラーとして扱う
    "ignore::DeprecationWarning",
]
```

### CI/CD エラーチェック
```yaml
- name: Run tests with error checking
  run: |
    pytest --tb=short --strict-markers
    # エラーハンドリングが適切か検証
    python -m scripts.check_error_handling
```

---

## 📋 エラーハンドリングチートシート

| パターン | 使用例 | 説明 |
|----------|--------|------|
| **Fail Fast** | 入力検証で即例外 | 問題を早期発見 |
| **具体例外** | `LLMAPIError` | 何が問題か明確 |
| **ログ記録** | structlog 使用 | 構造化されたログ |
| **リトライ** | 指数バックオフ | 一時的エラーの回復 |
| **フォールバック** | 代替プロバイダー | 堅牢性の確保 |
| **タイムアウト** | `asyncio.wait_for` | 無限待機防止 |

*最終更新: 2026-01-19 | BizCore 7コア層 + Apps外層対応*