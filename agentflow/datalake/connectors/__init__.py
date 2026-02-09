"""DataLake Connectors - 各種データソースコネクタ.

サポートコネクタ:
    - LocalFileConnector: ローカルファイルシステム (file://)
    - S3Connector: AWS S3 / MinIO (s3://)
    - RestAPIConnector: REST API (rest://, http://, https://)
    - OneDriveConnector: Microsoft OneDrive (onedrive://)
    - GoogleDriveConnector: Google Drive (gdrive://)
    - DatabaseConnector: PostgreSQL/MySQL/SQLite (db://)
"""

from agentflow.datalake.connectors.local import LocalFileConnector


__all__ = [
    "LocalFileConnector",
]

# 条件付きインポート（依存関係がない場合はスキップ）
try:
    from agentflow.datalake.connectors.s3 import S3Connector
    __all__.append("S3Connector")
except ImportError:
    pass

try:
    from agentflow.datalake.connectors.rest import RestAPIConnector
    __all__.append("RestAPIConnector")
except ImportError:
    pass

try:
    from agentflow.datalake.connectors.onedrive import OneDriveConnector
    __all__.append("OneDriveConnector")
except ImportError:
    pass

try:
    from agentflow.datalake.connectors.gdrive import GoogleDriveConnector
    __all__.append("GoogleDriveConnector")
except ImportError:
    pass

try:
    from agentflow.datalake.connectors.database import DatabaseConnector
    __all__.append("DatabaseConnector")
except ImportError:
    pass
