"""ブラウザ制御スキル（メイン実装）.

Playwright ベースの安全なブラウザ操作を提供。

Example:
    >>> async with BrowserSkill(config) as browser:
    ...     page = await browser.navigate("https://example.com")
    ...     text = await browser.get_text("h1")
    ...     await browser.click("button.submit")
"""

from __future__ import annotations

import base64
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any, Literal

from agentflow.skills.browser.base import BrowserSkillBase, BrowserSkillError, BrowserStateError


if TYPE_CHECKING:
    from playwright.async_api import Browser, BrowserContext, Page

    from agentflow.skills.browser.config import BrowserSkillConfig


@dataclass
class PageInfo:
    """ページ情報."""

    url: str
    title: str
    content_type: str = "text/html"
    loaded_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "url": self.url,
            "title": self.title,
            "content_type": self.content_type,
            "loaded_at": self.loaded_at.isoformat(),
        }


@dataclass
class ElementInfo:
    """要素情報."""

    selector: str
    tag_name: str
    text: str = ""
    attributes: dict[str, str] = field(default_factory=dict)
    is_visible: bool = True

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "selector": self.selector,
            "tag_name": self.tag_name,
            "text": self.text[:500] if len(self.text) > 500 else self.text,
            "attributes": self.attributes,
            "is_visible": self.is_visible,
        }


@dataclass
class ScreenshotResult:
    """スクリーンショット結果."""

    data_base64: str
    width: int
    height: int
    format: str = "png"
    taken_at: datetime = field(default_factory=datetime.now)


class BrowserSkill(BrowserSkillBase):
    """ブラウザ制御スキル.

    Playwright を使用した安全なブラウザ操作を提供。
    ドメインホワイトリストによるアクセス制限付き。
    """

    def __init__(self, config: BrowserSkillConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)
        self._playwright: Any = None
        self._browser: Browser | None = None
        self._context: BrowserContext | None = None
        self._page: Page | None = None
        self._pages: list[Page] = []

    async def start(self) -> None:
        """ブラウザを起動."""
        if self._browser is not None:
            return

        try:
            from playwright.async_api import async_playwright
        except ImportError:
            msg = "playwright がインストールされていません: pip install playwright && playwright install"
            raise BrowserSkillError(msg, skill_name="BrowserSkill")

        self._audit_log("start_browser", {"browser_type": self._config.browser_type})

        self._playwright = await async_playwright().start()

        # ブラウザタイプ選択
        browser_type = getattr(self._playwright, self._config.browser_type)

        self._browser = await browser_type.launch(
            headless=self._config.headless,
        )

        # コンテキスト作成（セキュリティ設定付き）
        context_options: dict[str, Any] = {
            "viewport": {
                "width": self._config.viewport_width,
                "height": self._config.viewport_height,
            },
            "java_script_enabled": not self._config.disable_javascript,
        }

        if self._config.user_agent:
            context_options["user_agent"] = self._config.user_agent

        self._context = await self._browser.new_context(**context_options)

        # ダウンロード/アップロード制御
        if self._config.disable_downloads:
            self._context.set_default_timeout(self._config.page_timeout_seconds * 1000)

        self._logger.info("ブラウザを起動しました")

    async def stop(self) -> None:
        """ブラウザを停止."""
        if self._context:
            await self._context.close()
            self._context = None

        if self._browser:
            await self._browser.close()
            self._browser = None

        if self._playwright:
            await self._playwright.stop()
            self._playwright = None

        self._pages = []
        self._page = None
        self._logger.info("ブラウザを停止しました")

    async def navigate(
        self,
        url: str,
        wait_until: Literal["commit", "domcontentloaded", "load", "networkidle"] = "load",
    ) -> PageInfo:
        """URLに移動.

        Args:
            url: 移動先URL
            wait_until: 待機条件 (load/domcontentloaded/networkidle)

        Returns:
            ページ情報
        """
        await self._ensure_browser()
        validated_url = self._validate_url(url)

        self._audit_log("navigate", {"url": validated_url})

        # 新しいページ作成（必要な場合）
        if self._page is None:
            self._check_page_limit(len(self._pages))
            self._page = await self._context.new_page()  # type: ignore
            self._pages.append(self._page)

        await self._page.goto(
            validated_url,
            wait_until=wait_until,
            timeout=self._config.navigation_timeout_seconds * 1000,
        )

        return PageInfo(
            url=self._page.url,
            title=await self._page.title(),
        )

    async def click(self, selector: str) -> bool:
        """要素をクリック."""
        await self._ensure_page()
        self._audit_log("click", {"selector": selector})

        await self._page.click(selector, timeout=self._config.page_timeout_seconds * 1000)  # type: ignore
        return True

    async def type_text(self, selector: str, text: str, clear: bool = True) -> bool:
        """テキストを入力."""
        await self._ensure_page()
        self._audit_log("type_text", {"selector": selector, "text_length": len(text)})

        if clear:
            await self._page.fill(selector, text)  # type: ignore
        else:
            await self._page.type(selector, text)  # type: ignore
        return True

    async def get_text(self, selector: str) -> str:
        """要素のテキストを取得."""
        await self._ensure_page()
        return await self._page.text_content(selector) or ""  # type: ignore

    async def get_attribute(self, selector: str, attribute: str) -> str | None:
        """要素の属性を取得."""
        await self._ensure_page()
        return await self._page.get_attribute(selector, attribute)  # type: ignore

    async def take_screenshot(self, full_page: bool = False) -> ScreenshotResult:
        """スクリーンショットを取得."""
        await self._ensure_page()
        self._audit_log("screenshot", {"full_page": full_page})

        screenshot_bytes = await self._page.screenshot(full_page=full_page)  # type: ignore
        return ScreenshotResult(
            data_base64=base64.b64encode(screenshot_bytes).decode("utf-8"),
            width=self._config.viewport_width,
            height=self._config.viewport_height,
        )

    async def _ensure_browser(self) -> None:
        """ブラウザが起動していることを確認."""
        if self._browser is None:
            await self.start()

    async def _ensure_page(self) -> None:
        """ページが存在することを確認."""
        await self._ensure_browser()
        if self._page is None:
            msg = "ページが開かれていません。先に navigate() を呼び出してください"
            raise BrowserStateError(msg, skill_name="BrowserSkill")

    async def __aenter__(self) -> BrowserSkill:
        """async with サポート."""
        await self.start()
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """クリーンアップ."""
        await self.stop()
