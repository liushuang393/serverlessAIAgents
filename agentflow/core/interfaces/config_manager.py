# -*- coding: utf-8 -*-
"""IConfigManager - 設定管理器インターフェース.

デプロイ設定テンプレートとユーザー設定を管理するインターフェース。

このインターフェースは安定しており、変更は慎重に行う必要があります。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

if TYPE_CHECKING:
    from agentflow.core.interfaces.types import (
        ConfigField,
        ConfigTemplate,
        DeployTarget,
        ValidationResult,
    )


@runtime_checkable
class IConfigManager(Protocol):
    """設定管理器インターフェース.

    デプロイ設定のテンプレート管理と、ユーザー設定の処理を行います。

    責務:
    - プラットフォーム別の設定テンプレートを提供
    - 必須フィールドの特定（UI フォーム生成用）
    - テンプレートとユーザー設定のマージ
    - 設定の永続化と読み込み

    使用例:
        >>> manager = ConfigManager()
        >>> # テンプレート取得
        >>> template = await manager.get_template(DeployTarget.VERCEL)
        >>> # 必須フィールド取得（UI 用）
        >>> fields = await manager.get_required_fields(
        ...     DeployTarget.VERCEL,
        ...     current_config={"token": "xxx"},
        ... )
        >>> # マージ
        >>> final_config = await manager.merge_config(template, user_config)
    """

    async def get_template(
        self,
        target: "DeployTarget",
    ) -> "ConfigTemplate":
        """プラットフォーム設定テンプレートを取得.

        Args:
            target: デプロイターゲット

        Returns:
            設定テンプレート

        Raises:
            ValueError: サポートされていないターゲット
        """
        ...

    async def get_required_fields(
        self,
        target: "DeployTarget",
        current_config: dict[str, Any] | None = None,
    ) -> list["ConfigField"]:
        """未入力の必須フィールドを取得.

        現在の設定を考慮して、まだ入力されていない必須フィールドを返します。
        UI でフォームを動的に表示するために使用します。

        Args:
            target: デプロイターゲット
            current_config: 現在の設定（省略時は全フィールドを返す）

        Returns:
            入力が必要なフィールドのリスト
        """
        ...

    async def merge_config(
        self,
        template: "ConfigTemplate",
        user_config: dict[str, Any],
    ) -> dict[str, Any]:
        """テンプレートとユーザー設定をマージ.

        Args:
            template: 設定テンプレート
            user_config: ユーザー入力の設定

        Returns:
            マージされた設定
        """
        ...

    async def validate(
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

    async def save_config(
        self,
        name: str,
        target: "DeployTarget",
        config: dict[str, Any],
    ) -> None:
        """設定を保存.

        Args:
            name: 設定名
            target: デプロイターゲット
            config: 保存する設定
        """
        ...

    async def load_config(
        self,
        name: str,
    ) -> dict[str, Any] | None:
        """設定を読み込み.

        Args:
            name: 設定名

        Returns:
            設定、または None（存在しない場合）
        """
        ...

    async def list_configs(
        self,
        target: "DeployTarget | None" = None,
    ) -> list[str]:
        """保存された設定一覧を取得.

        Args:
            target: フィルタするターゲット（省略時は全て）

        Returns:
            設定名のリスト
        """
        ...


__all__ = ["IConfigManager"]
