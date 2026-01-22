# セキュリティ規約

> **バージョン**: 1.0.0
> **適用範囲**: AgentFlow Framework 全セキュリティ関連実装
> **最終更新**: 2025-01-19

## 📋 目次

1. [セキュリティ原則](#セキュリティ原則)
2. [入力検証](#入力検証)
3. [認証・認可](#認証認可)
4. [データ保護](#データ保護)
5. [API セキュリティ](#api-セキュリティ)
6. [ログと監査](#ログと監査)
7. [脆弱性管理](#脆弱性管理)

---

## 🛡️ セキュリティ原則

### ゼロトラストアーキテクチャ
```python
# ✅ 正しい: すべてのリクエストを検証
class APISecurityMiddleware:
    """API リクエストのセキュリティ検証ミドルウェア。"""

    async def __call__(self, request: Request, call_next):
        # 1. 認証トークンの検証
        if not await self._validate_auth_token(request):
            raise HTTPException(status_code=401, detail="Invalid token")

        # 2. 権限チェック
        if not await self._check_permissions(request):
            raise HTTPException(status_code=403, detail="Insufficient permissions")

        # 3. レート制限チェック
        if await self._is_rate_limited(request):
            raise HTTPException(status_code=429, detail="Rate limit exceeded")

        # 4. 入力検証
        await self._validate_input(request)

        return await call_next(request)
```

### 最小権限原則
```python
# ✅ 正しい: 最小限の権限のみ付与
class UserPermissions:
    """ユーザー権限管理。"""

    PERMISSIONS = {
        "user": ["read_own_data", "update_own_profile"],
        "moderator": ["user", "moderate_content", "view_reports"],
        "admin": ["moderator", "manage_users", "system_config"],
    }

    @classmethod
    def get_permissions(cls, role: str) -> set[str]:
        """ロールに基づく権限を取得。"""
        if role not in cls.PERMISSIONS:
            return set()
        return set(cls.PERMISSIONS[role])

    @classmethod
    def has_permission(cls, user_role: str, required_permission: str) -> bool:
        """指定された権限を持っているかチェック。"""
        user_permissions = cls.get_permissions(user_role)
        return required_permission in user_permissions
```

### フェイルセーフ設計
```python
# ✅ 正しい: エラー時は安全側に倒れる
class SecureWorkflowEngine:
    """セキュリティを考慮したワークフローエンジン。"""

    async def execute_workflow(self, workflow_id: str, user_id: str) -> dict:
        try:
            # 権限チェック
            if not await self._check_workflow_access(workflow_id, user_id):
                return {"status": "denied", "error": "Access denied"}

            # リソース制限チェック
            if not await self._check_resource_limits(user_id):
                return {"status": "limited", "error": "Resource limit exceeded"}

            # ワークフロー実行
            result = await self._execute_securely(workflow_id, user_id)
            return {"status": "success", "result": result}

        except Exception as e:
            # エラー時は詳細情報をログに記録するが、ユーザーには最小限の情報のみ返す
            logger.error(f"Workflow execution failed: {e}", exc_info=True)
            return {"status": "error", "error": "Internal server error"}
```

---

## 🔍 入力検証

### 包括的な入力検証
```python
# ✅ 正しい: 多層的な入力検証
from pydantic import BaseModel, Field, field_validator
import re

class UserInput(BaseModel):
    """ユーザー入力モデル。"""

    email: str = Field(..., min_length=5, max_length=254)
    name: str = Field(..., min_length=1, max_length=100)
    age: int = Field(..., ge=0, le=150)

    @field_validator("email")
    @classmethod
    def validate_email(cls, v: str) -> str:
        """メールアドレス形式の検証。"""
        if not re.match(r"^[^@]+@[^@]+\.[^@]+$", v):
            raise ValueError("Invalid email format")
        return v.lower().strip()

    @field_validator("name")
    @classmethod
    def validate_name(cls, v: str) -> str:
        """名前のサニタイズ。"""
        # 危険な文字の除去
        sanitized = re.sub(r"[<>]", "", v)
        return sanitized.strip()

class InputValidator:
    """包括的な入力検証クラス。"""

    @staticmethod
    def validate_workflow_input(data: dict) -> dict:
        """ワークフロー入力の検証。"""
        # 1. 構造検証
        required_fields = ["workflow_id", "inputs"]
        for field in required_fields:
            if field not in data:
                raise ValueError(f"Missing required field: {field}")

        # 2. 型検証
        if not isinstance(data["inputs"], dict):
            raise ValueError("inputs must be a dictionary")

        # 3. サイズ制限
        if len(str(data)) > 1024 * 1024:  # 1MB 制限
            raise ValueError("Input too large")

        # 4. コンテンツ検証
        for key, value in data["inputs"].items():
            if not isinstance(key, str) or len(key) > 100:
                raise ValueError(f"Invalid input key: {key}")
            if len(str(value)) > 10 * 1024:  # 10KB per value
                raise ValueError(f"Input value too large: {key}")

        return data
```

### SQL インジェクション対策
```python
# ✅ 正しい: パラメータ化クエリを使用
class SecureDatabaseService:
    """SQL インジェクション対策済みデータベースサービス。"""

    async def get_user_by_email(self, email: str) -> dict | None:
        """メールアドレスによるユーザー検索。"""
        # パラメータ化クエリを使用（推奨）
        query = "SELECT * FROM users WHERE email = $1"
        async with self.pool.acquire() as conn:
            result = await conn.fetchrow(query, email)
            return dict(result) if result else None

    async def search_users(self, search_term: str, limit: int = 10) -> list[dict]:
        """安全なユーザー検索。"""
        # ワイルドカード検索もパラメータ化
        query = """
            SELECT * FROM users
            WHERE name ILIKE $1
            ORDER BY name
            LIMIT $2
        """
        # 検索語を適切にエスケープ
        safe_term = f"%{search_term.replace('%', '%%').replace('_', '__')}%"

        async with self.pool.acquire() as conn:
            results = await conn.fetch(query, safe_term, min(limit, 100))
            return [dict(row) for row in results]
```

### XSS 対策
```python
# ✅ 正しい: 出力時のエスケープ
import html

class SecureTemplateRenderer:
    """XSS 対策済みテンプレートレンダラー。"""

    @staticmethod
    def render_user_profile(user: dict) -> str:
        """ユーザープロフィールの安全なレンダリング。"""
        name = html.escape(user.get("name", ""))
        email = html.escape(user.get("email", ""))

        return f"""
        <div class="user-profile">
            <h2>{name}</h2>
            <p>Email: {email}</p>
        </div>
        """

    @staticmethod
    def sanitize_html_input(input_html: str) -> str:
        """HTML 入力のサニタイズ。"""
        from bleach import clean

        # 許可するタグと属性を明示的に指定
        allowed_tags = ['p', 'br', 'strong', 'em']
        allowed_attrs = {}

        return clean(input_html, tags=allowed_tags, attributes=allowed_attrs, strip=True)
```

---

## 🔐 認証・認可

### JWT ベース認証
```python
# ✅ 正しい: 安全な JWT 実装
import jwt
from datetime import datetime, timedelta
from typing import Optional

class JWTAuthService:
    """JWT ベース認証サービス。"""

    SECRET_KEY = "your-256-bit-secret"  # 環境変数から取得すべき
    ALGORITHM = "HS256"
    ACCESS_TOKEN_EXPIRE_MINUTES = 30

    @classmethod
    def create_access_token(cls, data: dict) -> str:
        """アクセストークン作成。"""
        to_encode = data.copy()
        expire = datetime.utcnow() + timedelta(minutes=cls.ACCESS_TOKEN_EXPIRE_MINUTES)
        to_encode.update({"exp": expire, "iat": datetime.utcnow()})

        encoded_jwt = jwt.encode(to_encode, cls.SECRET_KEY, algorithm=cls.ALGORITHM)
        return encoded_jwt

    @classmethod
    def verify_token(cls, token: str) -> Optional[dict]:
        """トークン検証。"""
        try:
            payload = jwt.decode(token, cls.SECRET_KEY, algorithms=[cls.ALGORITHM])

            # トークンの有効期限チェック
            exp = payload.get("exp")
            if exp and datetime.utcfromtimestamp(exp) < datetime.utcnow():
                return None

            return payload
        except jwt.PyJWTError:
            return None

class AuthMiddleware:
    """認証ミドルウェア。"""

    async def authenticate_request(self, request) -> Optional[dict]:
        """リクエスト認証。"""
        auth_header = request.headers.get("Authorization")
        if not auth_header or not auth_header.startswith("Bearer "):
            return None

        token = auth_header.split(" ")[1]
        return JWTAuthService.verify_token(token)
```

### ロールベースアクセス制御 (RBAC)
```python
# ✅ 正しい: RBAC 実装
from enum import Enum
from typing import Set

class UserRole(Enum):
    """ユーザーロール定義。"""
    GUEST = "guest"
    USER = "user"
    MODERATOR = "moderator"
    ADMIN = "admin"

class Permission(Enum):
    """権限定義。"""
    READ_PUBLIC = "read_public"
    READ_PRIVATE = "read_private"
    WRITE_PUBLIC = "write_public"
    WRITE_PRIVATE = "write_private"
    DELETE_PUBLIC = "delete_public"
    DELETE_PRIVATE = "delete_private"
    MANAGE_USERS = "manage_users"
    SYSTEM_CONFIG = "system_config"

class RBACService:
    """ロールベースアクセス制御サービス。"""

    # ロールごとの権限マッピング
    ROLE_PERMISSIONS = {
        UserRole.GUEST: {Permission.READ_PUBLIC},
        UserRole.USER: {
            Permission.READ_PUBLIC, Permission.READ_PRIVATE,
            Permission.WRITE_PUBLIC, Permission.WRITE_PRIVATE,
            Permission.DELETE_PUBLIC,
        },
        UserRole.MODERATOR: {
            Permission.READ_PUBLIC, Permission.READ_PRIVATE,
            Permission.WRITE_PUBLIC, Permission.WRITE_PRIVATE,
            Permission.DELETE_PUBLIC, Permission.DELETE_PRIVATE,
        },
        UserRole.ADMIN: set(Permission),  # すべての権限
    }

    @classmethod
    def has_permission(
        cls,
        user_role: UserRole,
        required_permission: Permission
    ) -> bool:
        """指定された権限を持っているかチェック。"""
        user_permissions = cls.ROLE_PERMISSIONS.get(user_role, set())
        return required_permission in user_permissions

    @classmethod
    def get_user_permissions(cls, user_role: UserRole) -> Set[Permission]:
        """ユーザーの全権限を取得。"""
        return cls.ROLE_PERMISSIONS.get(user_role, set())
```

### パスワードセキュリティ
```python
# ✅ 正しい: 安全なパスワードハンドリング
import hashlib
import secrets
import bcrypt
from typing import Tuple

class PasswordService:
    """パスワードセキュリティサービス。"""

    @staticmethod
    def hash_password(password: str) -> str:
        """パスワードのハッシュ化（bcrypt 推奨）。"""
        # bcrypt を使用した安全なハッシュ化
        salt = bcrypt.gensalt(rounds=12)  # 十分なラウンド数
        return bcrypt.hashpw(password.encode(), salt).decode()

    @staticmethod
    def verify_password(password: str, hashed: str) -> bool:
        """パスワード検証。"""
        return bcrypt.checkpw(password.encode(), hashed.encode())

    @staticmethod
    def generate_secure_token(length: int = 32) -> str:
        """安全なランダムトークン生成。"""
        return secrets.token_urlsafe(length)

    @staticmethod
    def hash_sensitive_data(data: str, salt: str = None) -> str:
        """機密データのハッシュ化（パスワード以外）。"""
        if salt is None:
            salt = secrets.token_hex(16)

        # PBKDF2 を使用した安全なハッシュ化
        hash_obj = hashlib.pbkdf2_hmac(
            'sha256',
            data.encode(),
            salt.encode(),
            100000  # 十分なイテレーション数
        )
        return f"{salt}:{hash_obj.hex()}"
```

---

## 🔒 データ保護

### データ暗号化
```python
# ✅ 正しい: AES 暗号化の実装
from cryptography.fernet import Fernet
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC
import base64
import os

class DataEncryptionService:
    """データ暗号化サービス。"""

    @staticmethod
    def generate_key(password: str, salt: bytes = None) -> bytes:
        """パスワードベースの暗号化キー生成。"""
        if salt is None:
            salt = os.urandom(16)

        kdf = PBKDF2HMAC(
            algorithm=hashes.SHA256(),
            length=32,
            salt=salt,
            iterations=100000,
        )
        return base64.urlsafe_b64encode(kdf.derive(password.encode()))

    def __init__(self, key: bytes):
        self.fernet = Fernet(key)

    def encrypt_data(self, data: str) -> str:
        """データの暗号化。"""
        encrypted = self.fernet.encrypt(data.encode())
        return encrypted.decode()

    def decrypt_data(self, encrypted_data: str) -> str:
        """データの復号化。"""
        decrypted = self.fernet.decrypt(encrypted_data.encode())
        return decrypted.decode()

# 使用例
encryption = DataEncryptionService(key=b"your-32-byte-key")
encrypted = encryption.encrypt_data("sensitive data")
decrypted = encryption.decrypt_data(encrypted)
```

### 機密情報管理
```python
# ✅ 正しい: 環境変数ベースの機密情報管理
import os
from typing import Optional

class ConfigurationManager:
    """安全な設定管理。"""

    @staticmethod
    def get_secret(key: str, default: str = None) -> Optional[str]:
        """環境変数からの機密情報取得。"""
        return os.getenv(key, default)

    @classmethod
    def get_database_config(cls) -> dict:
        """データベース設定の安全な取得。"""
        return {
            "host": cls.get_secret("DB_HOST", "localhost"),
            "port": int(cls.get_secret("DB_PORT", "5432")),
            "user": cls.get_secret("DB_USER"),
            "password": cls.get_secret("DB_PASSWORD"),
            "database": cls.get_secret("DB_NAME"),
        }

    @classmethod
    def get_jwt_config(cls) -> dict:
        """JWT 設定の安全な取得。"""
        secret = cls.get_secret("JWT_SECRET")
        if not secret:
            raise ValueError("JWT_SECRET environment variable is required")

        return {
            "secret": secret,
            "algorithm": cls.get_secret("JWT_ALGORITHM", "HS256"),
            "expire_minutes": int(cls.get_secret("JWT_EXPIRE_MINUTES", "30")),
        }

# 設定ファイルに機密情報をハードコードしない
# .env ファイルを使用（バージョン管理から除外）
"""
# .env (バージョン管理対象外)
DB_HOST=localhost
DB_USER=myuser
DB_PASSWORD=mysecretpassword
JWT_SECRET=your-super-secret-jwt-key
"""
```

---

## 🌐 API セキュリティ

### HTTPS 強制
```python
# ✅ 正しい: HTTPS リダイレクトミドルウェア
from fastapi import Request, HTTPException
from starlette.responses import RedirectResponse
from starlette.middleware.base import BaseHTTPMiddleware

class HTTPSRedirectMiddleware(BaseHTTPMiddleware):
    """HTTP を HTTPS にリダイレクトするミドルウェア。"""

    async def dispatch(self, request: Request, call_next):
        # 開発環境以外では HTTPS を強制
        if not request.url.scheme == "https" and os.getenv("ENVIRONMENT") != "development":
            https_url = request.url.replace(scheme="https")
            return RedirectResponse(https_url, status_code=301)

        response = await call_next(request)
        return response
```

### CORS 設定
```python
# ✅ 正しい: 制限的な CORS 設定
from fastapi.middleware.cors import CORSMiddleware

# 許可するオリジンのホワイトリスト
ALLOWED_ORIGINS = [
    "https://yourapp.com",
    "https://app.yourapp.com",
    # 開発環境用
    "http://localhost:3000",
    "http://localhost:5173",
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=ALLOWED_ORIGINS,  # ワイルドカード (*) 禁止
    allow_credentials=True,
    allow_methods=["GET", "POST", "PUT", "DELETE"],  # 必要なメソッドのみ
    allow_headers=["Authorization", "Content-Type"],  # 必要なヘッダーのみ
    max_age=86400,  # プリフライトリクエストのキャッシュ時間
)
```

### レート制限
```python
# ✅ 正しい: トークンバケットアルゴリズムによるレート制限
import time
from collections import defaultdict
from typing import Dict, Tuple

class RateLimiter:
    """レート制限実装。"""

    def __init__(self, requests_per_minute: int = 60):
        self.requests_per_minute = requests_per_minute
        self.requests: Dict[str, list] = defaultdict(list)

    def is_allowed(self, client_id: str) -> bool:
        """リクエストが許可されるかチェック。"""
        now = time.time()
        client_requests = self.requests[client_id]

        # 1分以上前のリクエストを削除
        client_requests[:] = [
            req_time for req_time in client_requests
            if now - req_time < 60
        ]

        if len(client_requests) >= self.requests_per_minute:
            return False

        client_requests.append(now)
        return True

class RateLimitMiddleware(BaseHTTPMiddleware):
    """レート制限ミドルウェア。"""

    def __init__(self, app, limiter: RateLimiter):
        super().__init__(app)
        self.limiter = limiter

    async def dispatch(self, request: Request, call_next):
        # クライアント識別（IP アドレスなど）
        client_id = self._get_client_id(request)

        if not self.limiter.is_allowed(client_id):
            raise HTTPException(
                status_code=429,
                detail="Too many requests",
                headers={"Retry-After": "60"}
            )

        return await call_next(request)

    def _get_client_id(self, request: Request) -> str:
        """クライアントIDの取得（X-Forwarded-For ヘッダーを考慮）。"""
        forwarded = request.headers.get("X-Forwarded-For")
        if forwarded:
            # 最初のIPアドレスを使用
            return forwarded.split(",")[0].strip()
        return request.client.host
```

---

## 📝 ログと監査

### セキュリティログ
```python
# ✅ 正しい: セキュリティイベントのログ記録
import logging
import json
from datetime import datetime

class SecurityLogger:
    """セキュリティイベントロガー。"""

    def __init__(self):
        self.logger = logging.getLogger("security")
        self.logger.setLevel(logging.INFO)

        # セキュリティログ専用ハンドラー
        handler = logging.FileHandler("security.log")
        formatter = logging.Formatter(
            '%(asctime)s - %(levelname)s - %(message)s'
        )
        handler.setFormatter(formatter)
        self.logger.addHandler(handler)

    def log_auth_attempt(self, user_id: str, success: bool, ip_address: str):
        """認証試行のログ記録。"""
        event = {
            "event_type": "auth_attempt",
            "user_id": user_id,
            "success": success,
            "ip_address": ip_address,
            "timestamp": datetime.utcnow().isoformat(),
        }
        self.logger.info(json.dumps(event))

    def log_unauthorized_access(self, user_id: str, resource: str, ip_address: str):
        """不正アクセスのログ記録。"""
        event = {
            "event_type": "unauthorized_access",
            "user_id": user_id,
            "resource": resource,
            "ip_address": ip_address,
            "timestamp": datetime.utcnow().isoformat(),
        }
        self.logger.warning(json.dumps(event))

    def log_suspicious_activity(self, user_id: str, activity: str, details: dict):
        """不審な活動のログ記録。"""
        event = {
            "event_type": "suspicious_activity",
            "user_id": user_id,
            "activity": activity,
            "details": details,
            "timestamp": datetime.utcnow().isoformat(),
        }
        self.logger.error(json.dumps(event))

# 使用例
security_logger = SecurityLogger()

# 認証成功
security_logger.log_auth_attempt("user123", True, "192.168.1.1")

# 不正アクセス
security_logger.log_unauthorized_access("user123", "/admin/users", "192.168.1.1")

# 不審な活動
security_logger.log_suspicious_activity(
    "user123",
    "multiple_failed_logins",
    {"attempts": 5, "time_window": "5_minutes"}
)
```

### 監査トレイル
```python
# ✅ 正しい: 操作の監査トレイル
from typing import Dict, Any
import hashlib

class AuditService:
    """監査サービス。"""

    def __init__(self, logger: SecurityLogger):
        self.logger = logger

    async def log_operation(
        self,
        user_id: str,
        operation: str,
        resource: str,
        details: Dict[str, Any],
        ip_address: str
    ) -> str:
        """操作の監査ログ記録。"""
        audit_id = self._generate_audit_id()

        audit_entry = {
            "audit_id": audit_id,
            "user_id": user_id,
            "operation": operation,
            "resource": resource,
            "details": details,
            "ip_address": ip_address,
            "timestamp": datetime.utcnow().isoformat(),
            "checksum": self._calculate_checksum(details),
        }

        # 構造化ログとして記録
        self.logger.logger.info(f"AUDIT: {json.dumps(audit_entry)}")

        # データベースにも保存（オプション）
        await self._save_to_database(audit_entry)

        return audit_id

    def _generate_audit_id(self) -> str:
        """監査IDの生成。"""
        return f"audit_{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}_{secrets.token_hex(4)}"

    def _calculate_checksum(self, data: Dict[str, Any]) -> str:
        """データのチェックサム計算。"""
        data_str = json.dumps(data, sort_keys=True)
        return hashlib.sha256(data_str.encode()).hexdigest()[:16]

    async def _save_to_database(self, audit_entry: Dict[str, Any]):
        """監査エントリをデータベースに保存。"""
        # 監査ログを永続化
        pass

# 使用例
audit_service = AuditService(security_logger)

# 重要な操作の監査
audit_id = await audit_service.log_operation(
    user_id="admin",
    operation="user_delete",
    resource="user:123",
    details={"reason": "account_closure", "requested_by": "user123"},
    ip_address="192.168.1.100"
)
```

---

## 🛠️ 脆弱性管理

### 依存関係セキュリティスキャン
```bash
# requirements.txt または pyproject.toml の脆弱性チェック
pip install safety
safety check

# 定期的なセキュリティ更新チェック
pip install pip-audit
pip-audit

# CI/CD での自動チェック
- name: Run Safety Check
  run: safety check --output json | tee safety-report.json

- name: Run Pip Audit
  run: pip-audit --format json | tee audit-report.json
```

### セキュリティヘッダー
```python
# ✅ 正しい: セキュリティヘッダーの設定
from fastapi import FastAPI
from fastapi.middleware.httpsredirect import HTTPSRedirectMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware

app = FastAPI()

# HTTPS 強制
app.add_middleware(HTTPSRedirectMiddleware)

# 信頼できるホスト制限
app.add_middleware(
    TrustedHostMiddleware,
    allowed_hosts=["yourdomain.com", "*.yourdomain.com"]
)

# セキュリティヘッダーミドルウェア
class SecurityHeadersMiddleware(BaseHTTPMiddleware):
    """セキュリティヘッダー追加ミドルウェア。"""

    async def dispatch(self, request: Request, call_next):
        response = await call_next(request)

        # セキュリティヘッダーの追加
        response.headers["X-Content-Type-Options"] = "nosniff"
        response.headers["X-Frame-Options"] = "DENY"
        response.headers["X-XSS-Protection"] = "1; mode=block"
        response.headers["Strict-Transport-Security"] = "max-age=31536000; includeSubDomains"
        response.headers["Content-Security-Policy"] = "default-src 'self'"
        response.headers["Referrer-Policy"] = "strict-origin-when-cross-origin"

        return response

app.add_middleware(SecurityHeadersMiddleware)
```

### 定期的なセキュリティレビュー
```python
# .github/workflows/security-audit.yml
name: Security Audit
on:
  schedule:
    - cron: '0 0 * * 1'  # 毎週月曜日実行
  push:
    branches: [main]

jobs:
  security:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Set up Python
        uses: actions/setup-python@v4
        with:
          python-version: '3.13'

      - name: Install dependencies
        run: pip install -e .[dev]

      - name: Run Bandit (Security Linter)
        run: |
          pip install bandit
          bandit -r agentflow -f json -o bandit-report.json

      - name: Run Safety Check
        run: |
          pip install safety
          safety check --output json | tee safety-report.json

      - name: Upload reports
        uses: actions/upload-artifact@v3
        with:
          name: security-reports
          path: |
            bandit-report.json
            safety-report.json
```

---

## ✅ セキュリティチェックリスト

### 開発時チェックリスト
- [ ] すべてのユーザー入力が検証されている
- [ ] SQL インジェクション対策が実装されている
- [ ] XSS 対策が実装されている
- [ ] 機密情報がハードコードされていない
- [ ] パスワードが適切にハッシュ化されている
- [ ] JWT トークンが安全に管理されている
- [ ] HTTPS が強制されている
- [ ] CORS が適切に設定されている
- [ ] レート制限が実装されている

### デプロイ時チェックリスト
- [ ] セキュリティヘッダーが設定されている
- [ ] 環境変数で機密情報が管理されている
- [ ] データベース接続が暗号化されている
- [ ] ログに機密情報が含まれていない
- [ ] 監査トレイルが有効になっている
- [ ] 定期的なセキュリティスキャンが設定されている

---

## 📚 関連ドキュメント

- [**テスト規約**](testing-standards.md) - セキュリティテストのベストプラクティス
- [**API セキュリティ**](api-security.md) - REST API のセキュリティ実装
- [**データ保護**](data-protection.md) - データ暗号化とプライバシー保護
- [**コンプライアンス**](compliance.md) - 法的・規制遵守

---

**セキュリティはすべての開発段階で最優先事項です。** 🛡️

*最終更新: 2025-01-19 | バージョン: 1.0.0*