"""エージェントメタデータスキーマのユーティリティ.

このモジュールは agent.yaml の読み込み、検証、シリアライズ機能を提供します。
"""

import logging
from pathlib import Path
from typing import Any

import yaml
from pydantic import ValidationError

from agentflow.core.exceptions import AgentFlowError
from agentflow.core.metadata import AgentMetadata


class SchemaValidationError(AgentFlowError):
    """スキーマ検証エラー.

    agent.yaml のスキーマ検証に失敗した場合に発生します。
    """

    def __init__(self, errors: list[dict[str, Any]]) -> None:
        """スキーマ検証エラーを初期化.

        Args:
            errors: Pydantic バリデーションエラーリスト
        """
        self.errors = errors
        error_messages = "\n".join(
            f"  - {err['loc']}: {err['msg']}" for err in errors
        )
        super().__init__(f"Schema validation failed:\n{error_messages}")


class SchemaLoader:
    """エージェントメタデータスキーマローダー.

    agent.yaml ファイルの読み込みと検証を行います。
    """

    def __init__(self, *, logger: logging.Logger | None = None) -> None:
        """スキーマローダーを初期化.

        Args:
            logger: ロガーインスタンス
        """
        self._logger = logger or logging.getLogger(__name__)

    def load_from_file(self, path: Path) -> AgentMetadata:
        """YAML ファイルからメタデータを読み込み.

        Args:
            path: agent.yaml ファイルパス

        Returns:
            検証済みの AgentMetadata インスタンス

        Raises:
            FileNotFoundError: ファイルが存在しない場合
            SchemaValidationError: スキーマ検証に失敗した場合
            yaml.YAMLError: YAML パースに失敗した場合
        """
        if not path.exists():
            msg = f"Agent metadata file not found: {path}"
            raise FileNotFoundError(msg)

        self._logger.debug(f"Loading agent metadata from: {path}")

        try:
            with path.open("r", encoding="utf-8") as f:
                data = yaml.safe_load(f)
        except yaml.YAMLError as e:
            self._logger.error(f"Failed to parse YAML: {e}")
            raise

        return self.validate(data)

    def load_from_dict(self, data: dict[str, Any]) -> AgentMetadata:
        """辞書からメタデータを読み込み.

        Args:
            data: メタデータ辞書

        Returns:
            検証済みの AgentMetadata インスタンス

        Raises:
            SchemaValidationError: スキーマ検証に失敗した場合
        """
        return self.validate(data)

    def validate(self, data: dict[str, Any]) -> AgentMetadata:
        """メタデータを検証.

        Args:
            data: 検証するメタデータ辞書

        Returns:
            検証済みの AgentMetadata インスタンス

        Raises:
            SchemaValidationError: スキーマ検証に失敗した場合
        """
        try:
            metadata = AgentMetadata.model_validate(data)
            self._logger.debug(f"Validated agent metadata: {metadata.meta.id}")
            return metadata
        except ValidationError as e:
            self._logger.error(f"Schema validation failed: {e}")
            raise SchemaValidationError(e.errors()) from e

    def save_to_file(self, metadata: AgentMetadata, path: Path) -> None:
        """メタデータを YAML ファイルに保存.

        Args:
            metadata: 保存する AgentMetadata インスタンス
            path: 保存先ファイルパス
        """
        self._logger.debug(f"Saving agent metadata to: {path}")

        # Pydantic モデルを辞書に変換
        data = metadata.model_dump(mode="python", exclude_none=True)

        # YAML に書き込み
        with path.open("w", encoding="utf-8") as f:
            yaml.safe_dump(
                data,
                f,
                default_flow_style=False,
                allow_unicode=True,
                sort_keys=False,
            )

        self._logger.info(f"Saved agent metadata: {metadata.meta.id}")

    def to_dict(self, metadata: AgentMetadata) -> dict[str, Any]:
        """メタデータを辞書に変換.

        Args:
            metadata: 変換する AgentMetadata インスタンス

        Returns:
            メタデータ辞書
        """
        return metadata.model_dump(mode="python", exclude_none=True)

    def to_json_schema(self) -> dict[str, Any]:
        """JSON スキーマを生成.

        Returns:
            AgentMetadata の JSON スキーマ
        """
        return AgentMetadata.model_json_schema()

