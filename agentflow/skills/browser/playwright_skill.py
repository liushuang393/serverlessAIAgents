"""Playwright SkillGateway 統合モジュール.

BrowserSkill を SkillGateway に登録するためのファクトリ関数と
SkillDefinition を提供。

設計原則:
- app 層はこのモジュールを経由して browser スキルを登録する
- per-user isolation は user_data_base_dir / user_id でディレクトリを分離
- ドメインホワイトリストは SkillGateway の GatewayConfig で一元管理

使用例:
    >>> from agentflow.skills.browser.playwright_skill import create_browser_skill_definitions
    >>> from agentflow.skills.gateway import SkillGateway
    >>>
    >>> gateway = SkillGateway()
    >>> for skill_def in create_browser_skill_definitions(
    ...     domain_whitelist=["example.com"],
    ...     user_data_base_dir="/tmp/browser_profiles",
    ...     user_id="user1",
    ... ):
    ...     gateway.register_skill(skill_def)
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from agentflow.skills.browser.browser_skill import BrowserSkill
from agentflow.skills.browser.config import BrowserSkillConfig
from agentflow.skills.gateway import (
    RiskLevel,
    SkillCategory,
    SkillDefinition,
)


def _get_or_create_browser(
    browser_instances: dict[str, BrowserSkill],
    user_id: str,
    config: BrowserSkillConfig,
) -> BrowserSkill:
    """ユーザー単位のブラウザインスタンスを取得または作成."""
    if user_id not in browser_instances:
        browser_instances[user_id] = BrowserSkill(config)
    return browser_instances[user_id]


def create_browser_skill_definitions(
    domain_whitelist: list[str],
    *,
    user_data_base_dir: str | None = None,
    user_id: str = "default",
    headless: bool = True,
) -> list[SkillDefinition]:
    """ブラウザスキルの SkillDefinition リストを作成.

    Args:
        domain_whitelist: アクセス許可ドメインリスト
        user_data_base_dir: per-user ブラウザプロファイルのベースディレクトリ
        user_id: ユーザー識別子（per-user isolation に使用）
        headless: ヘッドレスモード（デフォルト True）

    Returns:
        SkillGateway に登録可能な SkillDefinition のリスト
    """
    # per-user 独立プロファイルディレクトリ
    user_data_dir: str | None = None
    if user_data_base_dir:
        user_data_dir = str(Path(user_data_base_dir) / user_id)

    config = BrowserSkillConfig(
        domain_whitelist=domain_whitelist,
        headless=headless,
        user_data_dir=user_data_dir,
    )

    # ユーザー単位ブラウザインスタンスキャッシュ
    browser_instances: dict[str, BrowserSkill] = {}

    async def navigate_handler(url: str, **_: Any) -> dict[str, Any]:
        """URL に移動してページ情報を返す."""
        browser = _get_or_create_browser(browser_instances, user_id, config)
        page_info = await browser.navigate(url)
        return page_info.to_dict()

    async def screenshot_handler(full_page: bool = False, **_: Any) -> dict[str, Any]:
        """スクリーンショットを取得."""
        browser = _get_or_create_browser(browser_instances, user_id, config)
        result = await browser.take_screenshot(full_page=full_page)
        return {
            "data_base64": result.data_base64,
            "width": result.width,
            "height": result.height,
            "format": result.format,
            "taken_at": result.taken_at.isoformat(),
        }

    async def get_text_handler(selector: str = "body", **_: Any) -> dict[str, Any]:
        """要素のテキストを取得."""
        browser = _get_or_create_browser(browser_instances, user_id, config)
        text = await browser.get_text(selector)
        return {"text": text, "selector": selector}

    async def fill_form_handler(fields: dict[str, str], **_: Any) -> dict[str, Any]:
        """フォームを入力."""
        browser = _get_or_create_browser(browser_instances, user_id, config)
        success = await browser.fill_form(fields)
        return {"success": success, "fields_filled": list(fields.keys())}

    async def click_handler(selector: str, **_: Any) -> dict[str, Any]:
        """要素をクリック."""
        browser = _get_or_create_browser(browser_instances, user_id, config)
        success = await browser.click(selector)
        return {"success": success, "selector": selector}

    async def generate_pdf_handler(**_: Any) -> dict[str, Any]:
        """現在のページを PDF に変換."""
        browser = _get_or_create_browser(browser_instances, user_id, config)
        pdf_base64 = await browser.generate_pdf()
        return {"pdf_base64": pdf_base64}

    return [
        SkillDefinition(
            name="browser_navigate",
            description="指定URLに移動してページ情報を返す",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.MEDIUM,
            handler=navigate_handler,
            parameters={"url": "string"},
            requires_confirmation=False,
            allowed_in_isolated=False,
            allowed_in_real_machine=True,
        ),
        SkillDefinition(
            name="browser_screenshot",
            description="現在のページのスクリーンショットを取得",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.LOW,
            handler=screenshot_handler,
            parameters={"full_page": "boolean"},
            requires_confirmation=False,
            allowed_in_isolated=False,
            allowed_in_real_machine=True,
        ),
        SkillDefinition(
            name="browser_get_text",
            description="指定セレクタの要素テキストを取得",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.LOW,
            handler=get_text_handler,
            parameters={"selector": "string"},
            requires_confirmation=False,
            allowed_in_isolated=False,
            allowed_in_real_machine=True,
        ),
        SkillDefinition(
            name="browser_fill_form",
            description="フォームフィールドを一括入力",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.HIGH,
            handler=fill_form_handler,
            parameters={"fields": "object"},
            requires_confirmation=True,
            allowed_in_isolated=False,
            allowed_in_real_machine=True,
        ),
        SkillDefinition(
            name="browser_click",
            description="指定セレクタの要素をクリック",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.MEDIUM,
            handler=click_handler,
            parameters={"selector": "string"},
            requires_confirmation=False,
            allowed_in_isolated=False,
            allowed_in_real_machine=True,
        ),
        SkillDefinition(
            name="browser_generate_pdf",
            description="現在のページを PDF に変換して base64 を返す",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.LOW,
            handler=generate_pdf_handler,
            parameters={},
            requires_confirmation=False,
            allowed_in_isolated=False,
            allowed_in_real_machine=True,
        ),
    ]
