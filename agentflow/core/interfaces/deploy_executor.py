# -*- coding: utf-8 -*-
"""IDeployExecutor - デプロイ執行器インターフェース.

生成されたコードをターゲットプラットフォームにデプロイするインターフェース。

このインターフェースは安定しており、変更は慎重に行う必要があります。
"""

from __future__ import annotations

from collections.abc import AsyncIterator
from pathlib import Path
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

if TYPE_CHECKING:
    from agentflow.core.interfaces.types import (
        ConfigField,
        DeployConfig,
        DeployEvent,
        DeployResult,
        DeployTarget,
        GeneratedCode,
        ValidationResult,
    )


@runtime_checkable
class IDeployExecutor(Protocol):
    """デプロイ執行器インターフェース.

    生成されたコードを編譯してターゲットプラットフォームにデプロイします。

    責務:
    - コードのビルド/編譯
    - プラットフォーム API を使用したデプロイ
    - 進捗のストリーミング報告
    - 設定の検証

    実装要件:
    - deploy(): 非同期ストリーム方式でデプロイ
    - get_required_config(): プラットフォームに必要な設定を取得
    - validate_config(): 設定の検証

    使用例:
        >>> executor = DeployExecutor()
        >>> async for event in executor.deploy(
        ...     source=generated_code,
        ...     target=DeployTarget.VERCEL,
        ...     config=deploy_config,
        ... ):
        ...     print(f"{event.progress}%: {event.message}")
    """

    async def deploy(
        self,
        source: "GeneratedCode | Path",
        target: "DeployTarget",
        config: "DeployConfig",
    ) -> AsyncIterator["DeployEvent"]:
        """デプロイを実行.

        ストリーム方式で進捗を報告しながらデプロイを実行します。

        Args:
            source: 生成されたコード、またはソースディレクトリパス
            target: デプロイターゲット
            config: デプロイ設定

        Yields:
            デプロイイベント（進捗、ログ、成功、エラー）

        Raises:
            ValueError: 無効な設定
            DeployError: デプロイ失敗
        """
        ...

    async def deploy_sync(
        self,
        source: "GeneratedCode | Path",
        target: "DeployTarget",
        config: "DeployConfig",
    ) -> "DeployResult":
        """デプロイを同期実行.

        Args:
            source: 生成されたコード、またはソースディレクトリパス
            target: デプロイターゲット
            config: デプロイ設定

        Returns:
            デプロイ結果
        """
        ...

    async def get_required_config(
        self,
        target: "DeployTarget",
    ) -> list["ConfigField"]:
        """ターゲットプラットフォームに必要な設定フィールドを取得.

        UI でフォームを動的に生成するために使用します。

        Args:
            target: デプロイターゲット

        Returns:
            設定フィールドのリスト
        """
        ...

    async def validate_config(
        self,
        target: "DeployTarget",
        config: dict[str, Any],
    ) -> "ValidationResult":
        """設定を検証.

        Args:
            target: デプロイターゲット
            config: 検証する設定

        Returns:
            検証結果
        """
        ...

    def get_supported_targets(self) -> list["DeployTarget"]:
        """サポートされているデプロイターゲットを取得.

        Returns:
            デプロイターゲットのリスト
        """
        ...


__all__ = ["IDeployExecutor"]
