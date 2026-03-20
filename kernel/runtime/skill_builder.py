"""Runtime skill gateway builder from canonical contracts.skills payloads."""

from __future__ import annotations

import asyncio
import logging
from typing import TYPE_CHECKING

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from kernel.skills.gateway import SkillGateway


logger = logging.getLogger(__name__)

SkillsPayload = dict[str, object]


class SkillsBootstrapConfig(BaseModel):
    """Normalized runtime skills bootstrap configuration."""

    auto_install: bool = Field(default=False)
    hot_reload: bool = Field(default=True)
    allowed_sources: list[str] = Field(default_factory=list)
    default_skills: list[str] = Field(default_factory=list)


async def build_skill_gateway(skills_config: SkillsPayload | None) -> SkillGateway | None:
    """Build a runtime skill gateway from contracts.skills payloads."""
    if skills_config is None:
        logger.debug("Skills settings not configured; runtime skills disabled")
        return None

    config = SkillsBootstrapConfig.model_validate(skills_config)
    if not config.default_skills and not config.auto_install:
        logger.debug("Skill runtime disabled by manifest")
        return None

    try:
        from kernel.skills.factory import create_skill_gateway

        gateway = await asyncio.to_thread(create_skill_gateway)
        logger.info("Runtime skill gateway ready: default_skills=%s", config.default_skills)
        return gateway
    except ImportError:
        logger.debug("SkillGateway unavailable in current environment")
        return None
    except Exception as exc:
        logger.warning("Runtime skill bootstrap failed: %s", exc, exc_info=True)
        return None


__all__ = ["SkillsBootstrapConfig", "build_skill_gateway"]
