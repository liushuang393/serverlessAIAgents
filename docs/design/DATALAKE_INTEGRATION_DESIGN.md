# DataLake Integration Module 設計ドキュメント

> バージョン: 1.0.0
> 日付: 2026-01-13
> ステータス: **実装完了**

## 1. 概要

### 1.1 目標

企業のデータ孤島を打破し、Agent が多様なデータソースにシームレスにアクセス可能にする統一データレイク接続層。

### 1.2 サポートデータソース

| カテゴリ | データソース | 優先度 |
|----------|-------------|--------|
| **ローカル** | ファイルシステム | P0 |
| **クラウドストレージ** | AWS S3, MinIO | P0 |
| **API** | REST, GraphQL | P0 |
| **ネットワークドライブ** | OneDrive, Google Drive | P1 |
| **データベース** | PostgreSQL, MySQL | P1 |
| **データウェアハウス** | Snowflake, BigQuery | P2 |
| **SaaS** | Salesforce, HubSpot | P2 |

### 1.3 設計原則

| 原則 | 説明 |
|------|------|
| **統一インターフェース** | 全データソースに共通のAPI |
| **プラグイン式** | コネクタの追加が容易 |
| **認証分離** | 認証ロジックは外部注入可能 |
| **ストリーミング** | 大容量データの段階読込 |
| **型安全** | Pydantic モデル、型ヒント完備 |

## 2. アーキテクチャ

### 2.1 レイヤー構成

```
┌─────────────────────────────────────────────────────────────────┐
│                        DataLake API                              │
│   list() / read() / write() / query() / stream() / exists()     │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                     DataLake Manager                             │
│   - コネクタ登録・管理                                           │
│   - 認証プロバイダ統合                                           │
│   - キャッシュ管理                                               │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                    Connector Layer                               │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐           │
│  │  Local   │ │   S3     │ │  REST    │ │ OneDrive │  ...      │
│  │Connector │ │Connector │ │Connector │ │Connector │           │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘           │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                    Format Handler Layer                          │
│  CSV | JSON | Excel | Parquet | PDF | Image | Audio             │
└─────────────────────────────────────────────────────────────────┘
                              │
┌─────────────────────────────────────────────────────────────────┐
│                    Auth Provider (外部注入)                      │
│  OAuth2 | API Key | IAM Role | Service Account                  │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 URI スキーム

統一リソース識別子でデータソースを指定：

```
{scheme}://{authority}/{path}?{query}

例:
- file:///data/reports/q4.csv
- s3://my-bucket/data/customers.parquet
- onedrive://Documents/Reports/2024/
- gdrive://shared/Analytics/
- rest://api.example.com/v1/users
- postgres://db.example.com/mydb/users
```

## 3. コアインターフェース

### 3.1 DataConnector Protocol

```python
from abc import ABC, abstractmethod
from typing import AsyncIterator, Any
from pydantic import BaseModel

class DataItem(BaseModel):
    """データアイテム."""
    uri: str
    name: str
    size: int | None = None
    modified_at: datetime | None = None
    content_type: str | None = None
    metadata: dict[str, Any] = Field(default_factory=dict)
    is_directory: bool = False

class ReadResult(BaseModel):
    """読み取り結果."""
    uri: str
    content: bytes | str | dict | list
    content_type: str
    size: int
    metadata: dict[str, Any] = Field(default_factory=dict)

class DataConnector(ABC):
    """データコネクタ抽象基底クラス."""

    @property
    @abstractmethod
    def scheme(self) -> str:
        """URIスキーム (例: 's3', 'file', 'onedrive')."""
        ...

    @abstractmethod
    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
    ) -> list[DataItem]:
        """ディレクトリ/バケット内のアイテム一覧."""
        ...

    @abstractmethod
    async def read(self, path: str) -> ReadResult:
        """ファイル/オブジェクトを読み取り."""
        ...

    @abstractmethod
    async def write(
        self,
        path: str,
        content: bytes | str,
        content_type: str | None = None,
    ) -> DataItem:
        """ファイル/オブジェクトを書き込み."""
        ...

    @abstractmethod
    async def exists(self, path: str) -> bool:
        """存在確認."""
        ...

    @abstractmethod
    async def delete(self, path: str) -> bool:
        """削除."""
        ...

    async def stream(
        self,
        path: str,
        chunk_size: int = 8192,
    ) -> AsyncIterator[bytes]:
        """ストリーミング読み取り（大容量ファイル用）."""
        result = await self.read(path)
        content = result.content if isinstance(result.content, bytes) else str(result.content).encode()
        for i in range(0, len(content), chunk_size):
            yield content[i:i + chunk_size]
```

### 3.2 AuthProvider Protocol（外部注入）

```python
class AuthProvider(Protocol):
    """認証プロバイダ（外部システムから注入）."""

    async def get_credentials(
        self,
        connector_type: str,
        resource_id: str | None = None,
    ) -> dict[str, Any]:
        """認証情報を取得."""
        ...

    async def refresh_token(
        self,
        connector_type: str,
        refresh_token: str,
    ) -> dict[str, Any]:
        """トークンをリフレッシュ."""
        ...
```

### 3.3 FormatHandler Protocol

```python
class FormatHandler(ABC):
    """フォーマットハンドラ抽象基底クラス."""

    @property
    @abstractmethod
    def supported_extensions(self) -> set[str]:
        """サポートする拡張子."""
        ...

    @property
    @abstractmethod
    def supported_content_types(self) -> set[str]:
        """サポートするContent-Type."""
        ...

    @abstractmethod
    async def parse(self, content: bytes, **kwargs) -> Any:
        """バイナリをパース."""
        ...

    @abstractmethod
    async def serialize(self, data: Any, **kwargs) -> bytes:
        """データをシリアライズ."""
        ...
```

## 4. DataLake Manager

### 4.1 統一アクセスAPI

```python
class DataLake:
    """データレイク統一アクセス."""

    def __init__(
        self,
        auth_provider: AuthProvider | None = None,
        cache_ttl: int = 300,
    ):
        self._connectors: dict[str, DataConnector] = {}
        self._format_handlers: dict[str, FormatHandler] = {}
        self._auth_provider = auth_provider
        self._cache: dict[str, tuple[Any, float]] = {}
        self._cache_ttl = cache_ttl

    def register_connector(self, connector: DataConnector) -> None:
        """コネクタを登録."""
        self._connectors[connector.scheme] = connector

    def register_format_handler(self, handler: FormatHandler) -> None:
        """フォーマットハンドラを登録."""
        for ext in handler.supported_extensions:
            self._format_handlers[ext] = handler

    async def list(self, uri: str, **kwargs) -> list[DataItem]:
        """URIのアイテム一覧."""
        connector, path = self._resolve(uri)
        return await connector.list(path, **kwargs)

    async def read(
        self,
        uri: str,
        parse: bool = True,
        **kwargs,
    ) -> ReadResult | Any:
        """URIを読み取り."""
        connector, path = self._resolve(uri)
        result = await connector.read(path)

        if parse:
            ext = Path(path).suffix.lower()
            if handler := self._format_handlers.get(ext):
                result.content = await handler.parse(
                    result.content if isinstance(result.content, bytes)
                    else str(result.content).encode(),
                    **kwargs,
                )
        return result

    async def write(self, uri: str, content: Any, **kwargs) -> DataItem:
        """URIに書き込み."""
        connector, path = self._resolve(uri)

        # フォーマットハンドラでシリアライズ
        ext = Path(path).suffix.lower()
        if handler := self._format_handlers.get(ext):
            if not isinstance(content, (bytes, str)):
                content = await handler.serialize(content, **kwargs)

        return await connector.write(path, content, **kwargs)

    async def exists(self, uri: str) -> bool:
        """存在確認."""
        connector, path = self._resolve(uri)
        return await connector.exists(path)

    async def delete(self, uri: str) -> bool:
        """削除."""
        connector, path = self._resolve(uri)
        return await connector.delete(path)

    async def query(
        self,
        uri: str,
        query: str,
        **kwargs,
    ) -> list[dict[str, Any]]:
        """クエリ実行（DB/API向け）."""
        connector, path = self._resolve(uri)
        if hasattr(connector, 'query'):
            return await connector.query(path, query, **kwargs)
        raise NotImplementedError(f"{connector.scheme} does not support query")

    def _resolve(self, uri: str) -> tuple[DataConnector, str]:
        """URIからコネクタとパスを解決."""
        parsed = urlparse(uri)
        scheme = parsed.scheme or 'file'

        if scheme not in self._connectors:
            raise ValueError(f"Unknown scheme: {scheme}")

        path = parsed.path
        if parsed.netloc:
            path = f"{parsed.netloc}{path}"

        return self._connectors[scheme], path
```

## 5. 組み込みコネクタ（P0）

### 5.1 LocalFileConnector

```python
class LocalFileConnector(DataConnector):
    """ローカルファイルシステムコネクタ."""

    @property
    def scheme(self) -> str:
        return "file"

    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
    ) -> list[DataItem]:
        """ディレクトリ内のファイル一覧."""
        ...

    async def read(self, path: str) -> ReadResult:
        """ファイル読み取り."""
        ...

    async def write(
        self,
        path: str,
        content: bytes | str,
        content_type: str | None = None,
    ) -> DataItem:
        """ファイル書き込み."""
        ...
```

### 5.2 S3Connector

```python
class S3Connector(DataConnector):
    """AWS S3 / MinIO 互換コネクタ."""

    def __init__(
        self,
        endpoint_url: str | None = None,  # MinIO用
        region: str = "us-east-1",
        auth_provider: AuthProvider | None = None,
    ):
        ...

    @property
    def scheme(self) -> str:
        return "s3"
```

### 5.3 RestAPIConnector

```python
class RestAPIConnector(DataConnector):
    """REST API コネクタ."""

    def __init__(
        self,
        base_url: str | None = None,
        auth_provider: AuthProvider | None = None,
        default_headers: dict[str, str] | None = None,
    ):
        ...

    @property
    def scheme(self) -> str:
        return "rest"

    async def query(
        self,
        endpoint: str,
        query: str | dict,
        method: str = "GET",
        **kwargs,
    ) -> list[dict[str, Any]]:
        """APIクエリ実行."""
        ...
```

## 6. Agent統合

### 6.1 DataLakeSkill

```python
# agentflow/skills/builtin/datalake/SKILL.md
class DataLakeSkill:
    """データレイクアクセスSkill."""

    def __init__(self, datalake: DataLake):
        self._datalake = datalake

    async def list_files(self, uri: str, pattern: str = "*") -> list[dict]:
        """ファイル一覧を取得."""
        items = await self._datalake.list(uri, pattern=pattern)
        return [item.model_dump() for item in items]

    async def read_file(self, uri: str) -> dict:
        """ファイルを読み取り."""
        result = await self._datalake.read(uri)
        return {"uri": uri, "content": result.content, "type": result.content_type}

    async def search_data(
        self,
        uri: str,
        query: str,
    ) -> list[dict]:
        """データを検索."""
        return await self._datalake.query(uri, query)
```

### 6.2 DeepAgentCoordinator統合

```python
# DeepAgentCoordinator での使用
coordinator = DeepAgentCoordinator(
    llm_client=llm,
    tools={
        "datalake": DataLakeSkill(datalake),
    },
)

# Agent が自動的にデータソースにアクセス
result = await coordinator.execute(
    "S3バケットの売上データを分析し、四半期レポートを作成"
)
```

## 7. セキュリティ考慮事項

### 7.1 認証分離

- 認証ロジックは `AuthProvider` として外部注入
- 各コネクタは認証情報を保持しない
- トークンリフレッシュは自動処理

### 7.2 アクセス制御

```python
class AccessControl(Protocol):
    """アクセス制御（外部注入）."""

    async def check_permission(
        self,
        user_id: str,
        uri: str,
        action: Literal["read", "write", "delete", "list"],
    ) -> bool:
        """権限チェック."""
        ...
```

## 8. ファイル構成

```
agentflow/
├── datalake/
│   ├── __init__.py           # 統一エクスポート
│   ├── core.py               # DataLake, DataItem, ReadResult
│   ├── connector.py          # DataConnector Protocol
│   ├── auth.py               # AuthProvider Protocol
│   ├── format_handlers.py    # FormatHandler + 組み込みハンドラ
│   └── connectors/
│       ├── __init__.py
│       ├── local.py          # LocalFileConnector
│       ├── s3.py             # S3Connector
│       ├── rest.py           # RestAPIConnector
│       ├── onedrive.py       # OneDriveConnector (P1)
│       ├── gdrive.py         # GoogleDriveConnector (P1)
│       └── database.py       # DatabaseConnector (P1)
└── skills/builtin/datalake/
    └── SKILL.md              # DataLake Skill定義
```

## 9. 実装状況

| 優先度 | 項目 | ステータス |
|--------|------|------------|
| **P0** | コアインターフェース | ✅ 完了 |
| **P0** | LocalFileConnector | ✅ 完了 |
| **P0** | S3Connector | ✅ 完了 |
| **P0** | RestAPIConnector | ✅ 完了 |
| **P0** | FormatHandlers (CSV/JSON/Excel/Parquet) | ✅ 完了 |
| **P1** | OneDriveConnector | ✅ 完了 |
| **P1** | GoogleDriveConnector | ✅ 完了 |
| **P1** | DatabaseConnector | ✅ 完了 |
| **P2** | Snowflake/BigQuery | 📋 設計予留 |
| **P2** | Salesforce/HubSpot | 📋 設計予留 |

## 10. 使用方法

### 10.1 基本的な使用

```python
from agentflow.datalake import DataLake, get_datalake
from agentflow.datalake.connectors import (
    LocalFileConnector,
    S3Connector,
    OneDriveConnector,
    GoogleDriveConnector,
    DatabaseConnector,
)

# DataLake初期化
datalake = DataLake()

# コネクタ登録
datalake.register_connector(LocalFileConnector())
datalake.register_connector(S3Connector())

# ファイル読み取り
result = await datalake.read("file:///data/report.csv")
print(result.content)

# S3からデータ取得
items = await datalake.list("s3://my-bucket/data/")
for item in items:
    print(f"{item.name}: {item.size} bytes")
```

### 10.2 クラウドストレージ

```python
# OneDrive
from agentflow.datalake.connectors.onedrive import OneDriveConnector, OneDriveConfig

connector = OneDriveConnector(OneDriveConfig(
    client_id="your-client-id",
    client_secret="your-secret",
    tenant_id="your-tenant",
))
datalake.register_connector(connector)

# Google Drive
from agentflow.datalake.connectors.gdrive import GoogleDriveConnector, GoogleDriveConfig

connector = GoogleDriveConnector(GoogleDriveConfig(
    service_account_file="/path/to/service-account.json",
))
datalake.register_connector(connector)
```

### 10.3 データベース

```python
from agentflow.datalake.connectors.database import DatabaseConnector, DatabaseConfig

connector = DatabaseConnector(DatabaseConfig(
    connection_string="postgresql+asyncpg://user:pass@localhost/db",
))
datalake.register_connector(connector)

# テーブル一覧
tables = await datalake.list("db://")

# クエリ実行
results = await datalake.query("db://users", "age > 18")
```

### 10.4 フォーマットハンドラ

```python
from agentflow.datalake import get_format_handler, CSVHandler, JSONHandler

# 自動フォーマット検出
handler = get_format_handler(".csv")
data = await handler.parse(csv_bytes)

# カスタムオプション
csv_handler = CSVHandler()
data = await csv_handler.parse(csv_bytes, delimiter=";", encoding="shift_jis")
```

## 11. 環境変数

| 変数名 | 説明 | 必須 |
|--------|------|------|
| `AWS_ACCESS_KEY_ID` | AWS アクセスキー | S3使用時 |
| `AWS_SECRET_ACCESS_KEY` | AWS シークレットキー | S3使用時 |
| `S3_ENDPOINT_URL` | S3エンドポイント（MinIO用） | MinIO使用時 |
| `MICROSOFT_CLIENT_ID` | Microsoft アプリID | OneDrive使用時 |
| `MICROSOFT_CLIENT_SECRET` | Microsoft シークレット | OneDrive使用時 |
| `MICROSOFT_TENANT_ID` | Microsoft テナントID | OneDrive使用時 |
| `GOOGLE_CLIENT_ID` | Google クライアントID | GDrive使用時 |
| `GOOGLE_CLIENT_SECRET` | Google シークレット | GDrive使用時 |
| `GOOGLE_REFRESH_TOKEN` | Google リフレッシュトークン | GDrive使用時 |
| `GOOGLE_APPLICATION_CREDENTIALS` | サービスアカウントJSON | GDrive使用時 |
| `DATABASE_URL` | データベース接続文字列 | DB使用時 |

## 12. 注意事項

### 12.1 セキュリティ

- 認証情報は環境変数または外部AuthProviderで管理
- 接続文字列にパスワードを直接記載しない
- 本番環境ではサービスアカウント認証を推奨

### 12.2 パフォーマンス

- 大容量ファイルは `stream()` メソッドを使用
- データベースクエリは適切なインデックスを設定
- S3アクセスはリージョンを適切に設定

### 12.3 エラーハンドリング

```python
from agentflow.datalake import DataLake

try:
    result = await datalake.read("s3://bucket/file.csv")
except FileNotFoundError:
    print("ファイルが見つかりません")
except PermissionError:
    print("アクセス権限がありません")
except ValueError as e:
    print(f"設定エラー: {e}")
```

---

*ドキュメント更新日: 2026-01-13*

