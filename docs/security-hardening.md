# AgentFlow セキュリティ強化ガイド

## 概要

このドキュメントは、Agentic AI システムのベストプラクティスに基づいた AgentFlow のセキュリティ強化方針を説明します。

参考: [90%的团队都在用的 Agentic AI 设计模式解析](https://mp.weixin.qq.com/s/xxx)

---

## 🚨 高優先度: 工具使用のセキュリティ

### 問題点

- 工具調用が実システムに影響を与える可能性
- ユーザー意図が曖昧な場合の誤った工具調用
- 工具のタイムアウトや失敗

### 実装必須の対策

#### 1. 工具ホワイトリスト機制

```python
# agentflow/core/tool_security.py
class ToolWhitelist:
    """工具ホワイトリスト管理."""
    
    ALLOWED_TOOLS = {
        "mcp://filesystem/read_file",
        "mcp://filesystem/write_file",
        "mcp://github/create_issue",
        # ... 許可された工具のみ
    }
    
    @classmethod
    def is_allowed(cls, tool_uri: str) -> bool:
        """工具が許可されているか確認."""
        return tool_uri in cls.ALLOWED_TOOLS
```

#### 2. 審計ログ記録

```python
# agentflow/core/audit_log.py
import logging
from datetime import datetime
from typing import Any

class AuditLogger:
    """工具調用の審計ログ."""
    
    def log_tool_call(
        self,
        user_id: str,
        tool_uri: str,
        parameters: dict[str, Any],
        result: Any,
        success: bool,
    ) -> None:
        """工具調用をログに記録."""
        log_entry = {
            "timestamp": datetime.utcnow().isoformat(),
            "user_id": user_id,
            "tool_uri": tool_uri,
            "parameters": parameters,
            "result": result,
            "success": success,
        }
        logging.info(f"AUDIT: {log_entry}")
```

#### 3. Schema 検証

```python
from pydantic import BaseModel, ValidationError

def validate_tool_parameters(tool_schema: dict, parameters: dict) -> bool:
    """工具パラメータを検証."""
    try:
        # Pydantic で動的にモデルを作成して検証
        model = create_model_from_schema(tool_schema)
        model(**parameters)
        return True
    except ValidationError as e:
        logging.error(f"Parameter validation failed: {e}")
        return False
```

#### 4. タイムアウト制御とリトライ

```python
import asyncio
from tenacity import retry, stop_after_attempt, wait_exponential

@retry(
    stop=stop_after_attempt(3),
    wait=wait_exponential(multiplier=1, min=2, max=10)
)
async def call_tool_with_timeout(
    tool_uri: str,
    parameters: dict,
    timeout: float = 30.0
) -> Any:
    """タイムアウト付きで工具を調用."""
    try:
        return await asyncio.wait_for(
            call_tool(tool_uri, parameters),
            timeout=timeout
        )
    except asyncio.TimeoutError:
        raise ToolTimeoutError(f"Tool {tool_uri} timed out after {timeout}s")
```

---

## 🔒 高優先度: API セキュリティ

### 問題点

- 21個のAPIルートでCSRF保護が不完全
- JWT が localStorage に保存され XSS 攻撃に脆弱
- CORS 設定が環境変数に依存

### 実装必須の対策

#### 1. CSRF 保護

```python
from fastapi import Depends, HTTPException, Header
from secrets import token_urlsafe

class CSRFProtection:
    """CSRF トークン検証."""
    
    def __init__(self):
        self._tokens: set[str] = set()
    
    def generate_token(self) -> str:
        """CSRF トークンを生成."""
        token = token_urlsafe(32)
        self._tokens.add(token)
        return token
    
    def verify_token(self, token: str) -> bool:
        """CSRF トークンを検証."""
        if token in self._tokens:
            self._tokens.remove(token)  # 一度だけ使用可能
            return True
        return False

csrf = CSRFProtection()

async def verify_csrf_token(
    x_csrf_token: str = Header(...)
) -> None:
    """CSRF トークンを検証する依存関数."""
    if not csrf.verify_token(x_csrf_token):
        raise HTTPException(status_code=403, detail="Invalid CSRF token")

# 使用例
@app.post("/api/workflows", dependencies=[Depends(verify_csrf_token)])
async def create_workflow(request: WorkflowCreateRequest):
    ...
```

#### 2. JWT セキュア保存（HttpOnly Cookie）

```python
from fastapi import Response
from jose import jwt
from datetime import datetime, timedelta

def set_auth_cookie(response: Response, user_id: str) -> None:
    """HttpOnly Cookie に JWT を設定."""
    token = jwt.encode(
        {
            "sub": user_id,
            "exp": datetime.utcnow() + timedelta(hours=24)
        },
        SECRET_KEY,
        algorithm="HS256"
    )
    response.set_cookie(
        key="auth_token",
        value=token,
        httponly=True,  # JavaScript からアクセス不可
        secure=True,    # HTTPS のみ
        samesite="strict",  # CSRF 保護
        max_age=86400   # 24時間
    )
```

---

## 📊 中優先度: 可観測性と審計追跡

### 実装推奨の対策

#### 1. リクエスト ID とトレース

```python
import uuid
from contextvars import ContextVar

request_id_var: ContextVar[str] = ContextVar("request_id")

@app.middleware("http")
async def add_request_id(request: Request, call_next):
    """すべてのリクエストに一意の ID を付与."""
    request_id = str(uuid.uuid4())
    request_id_var.set(request_id)
    response = await call_next(request)
    response.headers["X-Request-ID"] = request_id
    return response
```

---

## 実装チェックリスト

### 必須（高優先度）

- [ ] 工具ホワイトリスト機制
- [ ] 審計ログ記録
- [ ] Schema 検証
- [ ] タイムアウト制御
- [ ] CSRF 保護（全 POST/PUT/DELETE ルート）
- [ ] JWT HttpOnly Cookie
- [ ] CORS ホワイトリスト

### 推奨（中優先度）

- [ ] リクエスト ID トレース
- [ ] レート制限
- [ ] 構造化ログ
- [ ] メトリクス収集

### オプション（低優先度）

- [ ] 工具実行のサンドボックス化
- [ ] 異常検知
- [ ] セキュリティスキャン自動化

---

## 参考資料

- [Agentic AI 設計模式](https://mp.weixin.qq.com/s/xxx)
- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [FastAPI Security](https://fastapi.tiangolo.com/tutorial/security/)

