# -*- coding: utf-8 -*-
"""ブラウザ制御スキルパッケージ.

安全なブラウザ操作API群を提供。Playwright ベースの実装。

設計原則:
- Agent は直接ブラウザを操作できない
- 全てのセキュリティはコード制御
- ドメインホワイトリスト制限
- ヘッドレスモード強制（デフォルト）

使用例:
    >>> from agentflow.skills.browser import BrowserSkillConfig, BrowserSkill
    >>> config = BrowserSkillConfig(domain_whitelist=["example.com"])
    >>> browser = BrowserSkill(config)
    >>> await browser.start()
    >>> page = await browser.navigate("https://example.com")
    >>> await browser.stop()
"""

from agentflow.skills.browser.config import BrowserSkillConfig
from agentflow.skills.browser.base import BrowserSkillBase, BrowserSkillError
from agentflow.skills.browser.browser_skill import BrowserSkill

__all__ = [
    # 設定
    "BrowserSkillConfig",
    # 基底クラス
    "BrowserSkillBase",
    "BrowserSkillError",
    # スキル
    "BrowserSkill",
]

