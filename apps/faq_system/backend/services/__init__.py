# -*- coding: utf-8 -*-
"""FAQ System Services.

⚠️ 注意: このモジュールはフレームワーク層のサービスを再エクスポートします。
業務ロジックは agentflow/services/ に実装されています。
"""

# フレームワーク層のサービスを再エクスポート
from agentflow.agents import FAQAgent as FAQService
from agentflow.agents import FAQAgentConfig as FAQConfig

__all__ = [
    "FAQService",
    "FAQConfig",
]
