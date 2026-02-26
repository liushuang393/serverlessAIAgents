"""SkillBuilder - SkillsContractConfig から SkillGateway を構築するファクトリ.

contracts.skills 設定を読み込み、SkillGateway インスタンスを生成する。
auto_install=false で default_skills が空の場合は None を返す。

使用例:
    >>> from agentflow.bootstrap.skill_builder import build_skill_gateway
    >>> gateway = await build_skill_gateway(skills_config_dict)
"""

from __future__ import annotations

import logging
from typing import Any

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class SkillsBootstrapConfig(BaseModel):
    """SkillsBuilder 用設定（contracts.skills から読み込み）.

    Attributes:
        auto_install: 起動時に自動インストール
        hot_reload: ホットリロード有効化
        allowed_sources: 許可するスキルソース
        default_skills: デフォルトで有効にするスキル名リスト
    """

    auto_install: bool = Field(default=False)
    hot_reload: bool = Field(default=True)
    allowed_sources: list[str] = Field(default_factory=list)
    default_skills: list[str] = Field(default_factory=list)


async def build_skill_gateway(skills_config: dict[str, Any] | None) -> Any | None:
    """SkillsContractConfig 辞書から SkillGateway を構築.

    Args:
        skills_config: contracts.skills 設定辞書

    Returns:
        SkillGateway インスタンス、または None（設定なし / スキルなし）

    Graceful Degradation:
        - skills_config が None → None を返す
        - default_skills が空 かつ auto_install=false → None を返す
        - SkillGateway 初期化エラー → ログ警告して None を返す
    """
    if skills_config is None:
        logger.debug("Skills設定なし: SkillGateway をスキップ")
        return None

    config = SkillsBootstrapConfig.model_validate(skills_config)

    # スキルが設定されていない場合はスキップ
    if not config.default_skills and not config.auto_install:
        logger.debug("Skills未設定（default_skills空 かつ auto_install=false）: SkillGateway をスキップ")
        return None

    try:
        from agentflow.skills.factory import create_skill_gateway

        gateway = create_skill_gateway()

        logger.info(
            "SkillGateway 構築完了: default_skills=%s",
            config.default_skills,
        )
        return gateway

    except ImportError:
        logger.debug("SkillGateway が利用不可: スキップ")
        return None
    except Exception as exc:
        logger.warning(
            "SkillGateway 構築失敗（Graceful Degradation）: %s",
            exc,
            exc_info=True,
        )
        return None


__all__ = ["SkillsBootstrapConfig", "build_skill_gateway"]
