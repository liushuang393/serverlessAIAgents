# -*- coding: utf-8 -*-
"""製造業界Agentテンプレート.

製造業界向けの専門Agentテンプレートを提供。

テンプレート一覧:
- QualityControlTemplate: 品質管理Agent
- SupplyChainTemplate: サプライチェーン最適化Agent
- ProductionPlanningTemplate: 生産計画Agent
"""

from agentflow.templates.manufacturing.production_planning_template import (
    ProductionPlanningTemplate,
)
from agentflow.templates.manufacturing.quality_control_template import (
    QualityControlTemplate,
)
from agentflow.templates.manufacturing.supply_chain_template import SupplyChainTemplate

__all__ = [
    "QualityControlTemplate",
    "SupplyChainTemplate",
    "ProductionPlanningTemplate",
]


def register_all() -> None:
    """全製造テンプレートを登録."""
    from agentflow.templates import register_template

    register_template(QualityControlTemplate())
    register_template(SupplyChainTemplate())
    register_template(ProductionPlanningTemplate())

