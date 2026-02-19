"""DataLake モジュールテスト."""

import tempfile
from pathlib import Path

import pytest


class TestDataLakeImports:
    """インポートテスト."""

    def test_core_imports(self):
        """コアモジュールのインポート."""
        from agentflow.datalake import (
            DataItem,
            DataLake,
        )

        assert DataLake is not None
        assert DataItem is not None

    def test_connector_imports(self):
        """コネクタのインポート."""
        from agentflow.datalake import DataConnector
        from agentflow.datalake.connectors import LocalFileConnector

        assert DataConnector is not None
        assert LocalFileConnector is not None

    def test_cloud_connector_imports(self):
        """クラウドコネクタのインポート."""
        from agentflow.datalake.connectors import (
            DatabaseConnector,
            GoogleDriveConnector,
            OneDriveConnector,
        )

        assert OneDriveConnector is not None
        assert GoogleDriveConnector is not None
        assert DatabaseConnector is not None

    def test_auth_imports(self):
        """認証モジュールのインポート."""
        from agentflow.datalake import (
            AuthCredentials,
            AuthProvider,
        )

        assert AuthProvider is not None
        assert AuthCredentials is not None

    def test_format_handler_imports(self):
        """フォーマットハンドラのインポート."""
        from agentflow.datalake import (
            CSVHandler,
            JSONHandler,
        )

        assert CSVHandler is not None
        assert JSONHandler is not None


class TestLocalFileConnector:
    """LocalFileConnector テスト."""

    @pytest.fixture
    def connector(self):
        """コネクタインスタンス."""
        from agentflow.datalake.connectors import LocalFileConnector

        return LocalFileConnector()

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ."""
        with tempfile.TemporaryDirectory() as td:
            yield Path(td)

    def test_scheme(self, connector):
        """スキーム確認."""
        assert connector.scheme == "file"

    @pytest.mark.asyncio
    async def test_write_and_read(self, connector, temp_dir):
        """書き込みと読み取り."""
        test_file = temp_dir / "test.txt"
        content = "Hello, DataLake!"

        # 書き込み
        item = await connector.write(str(test_file), content)
        assert item.name == "test.txt"
        assert item.size == len(content)

        # 読み取り
        result = await connector.read(str(test_file))
        assert result.content == content.encode()

    @pytest.mark.asyncio
    async def test_list(self, connector, temp_dir):
        """ディレクトリ一覧."""
        # テストファイル作成
        (temp_dir / "file1.txt").write_text("test1")
        (temp_dir / "file2.txt").write_text("test2")
        (temp_dir / "subdir").mkdir()

        items = await connector.list(str(temp_dir))
        names = [item.name for item in items]

        assert "file1.txt" in names
        assert "file2.txt" in names
        assert "subdir" in names

    @pytest.mark.asyncio
    async def test_exists(self, connector, temp_dir):
        """存在確認."""
        test_file = temp_dir / "exists.txt"
        test_file.write_text("test")

        assert await connector.exists(str(test_file)) is True
        assert await connector.exists(str(temp_dir / "notexists.txt")) is False

    @pytest.mark.asyncio
    async def test_delete(self, connector, temp_dir):
        """削除."""
        test_file = temp_dir / "delete.txt"
        test_file.write_text("test")

        assert await connector.delete(str(test_file)) is True
        assert not test_file.exists()


class TestFormatHandlers:
    """フォーマットハンドラテスト."""

    @pytest.mark.asyncio
    async def test_csv_handler(self):
        """CSVハンドラ."""
        from agentflow.datalake import CSVHandler

        handler = CSVHandler()
        data = [{"name": "Alice", "age": "30"}, {"name": "Bob", "age": "25"}]

        # シリアライズ
        csv_bytes = await handler.serialize(data)
        assert b"name,age" in csv_bytes

        # パース
        parsed = await handler.parse(csv_bytes)
        assert len(parsed) == 2
        assert parsed[0]["name"] == "Alice"

    @pytest.mark.asyncio
    async def test_json_handler(self):
        """JSONハンドラ."""
        from agentflow.datalake import JSONHandler

        handler = JSONHandler()
        data = {"key": "value", "number": 42}

        # シリアライズ
        json_bytes = await handler.serialize(data)
        assert b'"key"' in json_bytes

        # パース
        parsed = await handler.parse(json_bytes)
        assert parsed["key"] == "value"
        assert parsed["number"] == 42


class TestDataLake:
    """DataLake 統合テスト."""

    @pytest.fixture
    def datalake(self):
        """DataLakeインスタンス."""
        from agentflow.datalake import DataLake, reset_datalake
        from agentflow.datalake.connectors import LocalFileConnector

        reset_datalake()
        dl = DataLake()
        dl.register_connector(LocalFileConnector())
        return dl

    def test_registered_schemes(self, datalake):
        """登録スキーム確認."""
        assert "file" in datalake.registered_schemes

    @pytest.mark.asyncio
    async def test_read_write_with_uri(self, datalake):
        """URIでの読み書き."""
        with tempfile.TemporaryDirectory() as td:
            uri = f"file://{td}/test.json"
            data = {"message": "Hello"}

            # 書き込み
            result = await datalake.write(uri, data)
            assert result.size > 0

            # 読み取り
            read_result = await datalake.read(uri)
            assert read_result.content == data


class TestConnectorConfigs:
    """コネクタ設定テスト."""

    def test_onedrive_config(self):
        """OneDrive設定."""
        from agentflow.datalake.connectors.onedrive import OneDriveConfig

        config = OneDriveConfig(
            client_id="test-client-id",
            tenant_id="test-tenant",
        )
        assert config.client_id == "test-client-id"
        assert config.tenant_id == "test-tenant"

    def test_gdrive_config(self):
        """Google Drive設定."""
        from agentflow.datalake.connectors.gdrive import GoogleDriveConfig

        config = GoogleDriveConfig(
            client_id="test-client-id",
            service_account_file="/path/to/sa.json",
        )
        assert config.client_id == "test-client-id"
        assert config.service_account_file == "/path/to/sa.json"

    def test_database_config(self):
        """データベース設定."""
        from agentflow.datalake.connectors.database import DatabaseConfig

        config = DatabaseConfig(
            connection_string="postgresql+asyncpg://user:pass@localhost/db",
            pool_size=10,
        )
        assert "postgresql" in config.connection_string
        assert config.pool_size == 10


class TestConnectorSchemes:
    """コネクタスキームテスト."""

    def test_local_scheme(self):
        """ローカルファイルスキーム."""
        from agentflow.datalake.connectors import LocalFileConnector

        connector = LocalFileConnector()
        assert connector.scheme == "file"

    def test_onedrive_scheme(self):
        """OneDriveスキーム."""
        from agentflow.datalake.connectors import OneDriveConnector

        connector = OneDriveConnector()
        assert connector.scheme == "onedrive"

    def test_gdrive_scheme(self):
        """Google Driveスキーム."""
        from agentflow.datalake.connectors import GoogleDriveConnector

        connector = GoogleDriveConnector()
        assert connector.scheme == "gdrive"

    def test_database_scheme(self):
        """データベーススキーム."""
        from agentflow.datalake.connectors import DatabaseConnector

        connector = DatabaseConnector()
        assert connector.scheme == "db"
