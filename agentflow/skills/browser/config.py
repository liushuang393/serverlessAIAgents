# -*- coding: utf-8 -*-
"""ブラウザスキル設定モデル.

セキュリティ制約を定義する設定クラス。

Example:
    >>> config = BrowserSkillConfig(
    ...     domain_whitelist=["example.com", "api.example.com"],
    ...     headless=True,
    ... )
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Literal


BrowserType = Literal["chromium", "firefox", "webkit"]


@dataclass
class BrowserSkillConfig:
    """ブラウザスキル設定.

    セキュリティ制約とブラウザ設定を定義。

    Attributes:
        domain_whitelist: 許可ドメインリスト（空の場合は全て拒否）
        browser_type: ブラウザタイプ
        headless: ヘッドレスモード（デフォルトTrue、セキュリティ上推奨）
        disable_downloads: ダウンロード禁止
        disable_uploads: アップロード禁止
        disable_javascript: JavaScript無効化（セキュリティ強化）
        max_pages: 最大同時ページ数
        page_timeout_seconds: ページロードタイムアウト
        navigation_timeout_seconds: ナビゲーションタイムアウト
        viewport_width: ビューポート幅
        viewport_height: ビューポート高さ
        user_agent: カスタムUser-Agent
        require_human_confirmation: 危険操作時に人工確認必須
    """

    domain_whitelist: list[str] = field(default_factory=list)
    browser_type: BrowserType = "chromium"
    headless: bool = True
    disable_downloads: bool = True
    disable_uploads: bool = True
    disable_javascript: bool = False
    max_pages: int = 5
    page_timeout_seconds: int = 30
    navigation_timeout_seconds: int = 30
    viewport_width: int = 1280
    viewport_height: int = 720
    user_agent: str | None = None
    require_human_confirmation: bool = True

    def is_domain_allowed(self, url: str) -> bool:
        """URLのドメインが許可されているか判定.

        Args:
            url: チェックするURL

        Returns:
            許可されている場合True
        """
        from urllib.parse import urlparse

        if not self.domain_whitelist:
            return False  # ホワイトリスト空の場合は全て拒否

        try:
            parsed = urlparse(url)
            domain = parsed.netloc.split(":")[0]  # ポート番号を除去

            return any(
                domain == allowed or domain.endswith(f".{allowed}")
                for allowed in self.domain_whitelist
            )
        except Exception:
            return False

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "domain_whitelist": self.domain_whitelist,
            "browser_type": self.browser_type,
            "headless": self.headless,
            "disable_downloads": self.disable_downloads,
            "disable_uploads": self.disable_uploads,
            "disable_javascript": self.disable_javascript,
            "max_pages": self.max_pages,
            "page_timeout_seconds": self.page_timeout_seconds,
            "navigation_timeout_seconds": self.navigation_timeout_seconds,
            "viewport_width": self.viewport_width,
            "viewport_height": self.viewport_height,
        }

