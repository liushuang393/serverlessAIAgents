"""BaseDeployTarget - デプロイターゲット基底クラス."""

from __future__ import annotations

from abc import ABC, abstractmethod
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from pathlib import Path

    from agentflow.core.interfaces import (
        ConfigField,
        DeployConfig,
        DeployEvent,
        ValidationResult,
    )


class BaseDeployTarget(ABC):
    """デプロイターゲット基底クラス.

    全てのデプロイターゲットはこのクラスを継承します。
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """ターゲット名."""
        ...

    @property
    @abstractmethod
    def description(self) -> str:
        """説明."""
        ...

    @abstractmethod
    def deploy(
        self,
        source_path: Path,
        config: DeployConfig,
    ) -> AsyncIterator[DeployEvent]:
        """デプロイを実行.

        Args:
            source_path: ソースディレクトリ
            config: デプロイ設定

        Yields:
            デプロイイベント
        """
        ...

    @abstractmethod
    def get_config_fields(self) -> list[ConfigField]:
        """必要な設定フィールドを取得.

        Returns:
            設定フィールドのリスト
        """
        ...

    @abstractmethod
    def validate_config(self, config: dict[str, Any]) -> ValidationResult:
        """設定を検証.

        Args:
            config: 検証する設定

        Returns:
            検証結果
        """
        ...


__all__ = ["BaseDeployTarget"]
