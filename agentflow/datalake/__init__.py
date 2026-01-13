# -*- coding: utf-8 -*-
"""DataLake Integration Module - 統一データアクセス層.

企業のデータ孤島を打破し、Agent が多様なデータソースに
シームレスにアクセス可能にする統一データレイク接続層。

サポートデータソース:
    - ローカルファイルシステム (file://)
    - AWS S3 / MinIO (s3://)
    - REST API (rest://, http://, https://)
    - OneDrive (onedrive://) - P1
    - Google Drive (gdrive://) - P1
    - Database (postgres://, mysql://) - P1

使用例:
    >>> from agentflow.datalake import DataLake, get_datalake
    >>>
    >>> # シンプルな使用
    >>> dl = get_datalake()
    >>> items = await dl.list("s3://my-bucket/data/")
    >>> content = await dl.read("file:///data/report.csv")
    >>>
    >>> # カスタム設定
    >>> dl = DataLake(auth_provider=my_auth)
    >>> dl.register_connector(S3Connector(endpoint_url="http://minio:9000"))
"""

from agentflow.datalake.core import (
    DataItem,
    DataLake,
    ReadResult,
    WriteResult,
    get_datalake,
    reset_datalake,
)
from agentflow.datalake.connector import (
    DataConnector,
    ConnectorConfig,
)
from agentflow.datalake.auth import (
    AuthProvider,
    AuthCredentials,
    SimpleAuthProvider,
)
from agentflow.datalake.format_handlers import (
    FormatHandler,
    CSVHandler,
    JSONHandler,
    ExcelHandler,
    ParquetHandler,
    get_format_handler,
    register_format_handler,
)

__all__ = [
    # Core
    "DataLake",
    "DataItem",
    "ReadResult",
    "WriteResult",
    "get_datalake",
    "reset_datalake",
    # Connector
    "DataConnector",
    "ConnectorConfig",
    # Auth
    "AuthProvider",
    "AuthCredentials",
    "SimpleAuthProvider",
    # Format Handlers
    "FormatHandler",
    "CSVHandler",
    "JSONHandler",
    "ExcelHandler",
    "ParquetHandler",
    "get_format_handler",
    "register_format_handler",
]

