"""強化版 FAQ Agent - Thin Wrapper around Kernel FAQAgent."""

from __future__ import annotations

from typing import Any

from kernel.agents.specialized.faq_agent import (
    FAQAgent as KernelFAQAgent,
)
from kernel.agents.specialized.faq_agent import (
    FAQAgentConfig,
    FAQResponse,
)


# Alias for backward compatibility if needed, but we'll use the kernel one
EnhancedFAQConfig = FAQAgentConfig


class EnhancedFAQAgent(KernelFAQAgent):
    """強化版 FAQ Agent (Apps Wrapper).

    実装の大部分は kernel.agents.specialized.faq_agent.FAQAgent に移動しました。
    このクラスは互換性のために残されています。
    """

    def __init__(
        self,
        config: EnhancedFAQConfig | None = None,
        llm_client: Any | None = None,
        bundle: Any | None = None,
    ) -> None:
        super().__init__(config=config, llm_client=llm_client, bundle=bundle)


__all__ = [
    "EnhancedFAQAgent",
    "EnhancedFAQConfig",
    "FAQResponse",
]
