# -*- coding: utf-8 -*-
"""ブラウザスキル基底クラス.

全てのブラウザスキルが継承する共通基盤。
URL検証、セキュリティチェック、ログ出力を提供。

Example:
    >>> class MyBrowserSkill(BrowserSkillBase):
    ...     async def my_operation(self, url: str) -> str:
    ...         self._validate_url(url)
    ...         return await self._do_something()
"""

from __future__ import annotations

import logging
from abc import ABC
from typing import Any

from agentflow.skills.browser.config import BrowserSkillConfig


class BrowserSkillError(Exception):
    """ブラウザスキルエラー基底クラス."""

    def __init__(
        self,
        message: str,
        skill_name: str = "",
        details: dict[str, Any] | None = None,
    ) -> None:
        """初期化."""
        super().__init__(message)
        self.skill_name = skill_name
        self.details = details or {}


class DomainSecurityError(BrowserSkillError):
    """ドメインセキュリティ違反."""

    pass


class BrowserStateError(BrowserSkillError):
    """ブラウザ状態エラー."""

    pass


class BrowserSkillBase(ABC):
    """ブラウザスキル基底クラス.

    セキュリティ検証とログ出力の共通処理を提供。
    """

    def __init__(self, config: BrowserSkillConfig | None = None) -> None:
        """初期化.

        Args:
            config: ブラウザスキル設定（None の場合はデフォルト）
        """
        self._config = config or BrowserSkillConfig()
        self._logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")

    @property
    def config(self) -> BrowserSkillConfig:
        """設定を取得."""
        return self._config

    def _validate_url(self, url: str) -> str:
        """URLを検証.

        Args:
            url: 検証するURL

        Returns:
            検証済みURL

        Raises:
            DomainSecurityError: ドメインが許可されていない場合
        """
        from urllib.parse import urlparse

        parsed = urlparse(url)

        # スキームチェック
        if parsed.scheme not in ("http", "https"):
            msg = f"無効なURLスキーム: {parsed.scheme}"
            raise DomainSecurityError(msg, skill_name=self.__class__.__name__)

        # ドメインチェック
        if not self._config.is_domain_allowed(url):
            domain = parsed.netloc.split(":")[0]
            msg = f"ドメイン '{domain}' は許可されていません"
            self._logger.warning(msg)
            raise DomainSecurityError(
                msg,
                skill_name=self.__class__.__name__,
                details={"domain": domain, "whitelist": self._config.domain_whitelist},
            )

        return url

    def _check_page_limit(self, current_pages: int) -> None:
        """ページ数制限をチェック.

        Args:
            current_pages: 現在のページ数

        Raises:
            BrowserStateError: ページ数制限に達した場合
        """
        if current_pages >= self._config.max_pages:
            msg = f"最大ページ数に達しました: {current_pages}/{self._config.max_pages}"
            raise BrowserStateError(msg, skill_name=self.__class__.__name__)

    def _audit_log(self, operation: str, details: dict[str, Any]) -> None:
        """監査ログを出力.

        Args:
            operation: 操作名
            details: 詳細情報
        """
        self._logger.info(
            "AUDIT: skill=%s, operation=%s, details=%s",
            self.__class__.__name__,
            operation,
            details,
        )

