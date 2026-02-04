# -*- coding: utf-8 -*-
"""AgentWizard - Agent 自動生成・進化システム.

自然言語から Agent を自動生成し、テスト、検証、発布を行います。

主要コンポーネント:
- AgentWizard: Agent 自動生成メインクラス
- CapabilityGapDetector: 能力缺口検出
- SystemSynthesizer: システム自動合成
- SkillForge: 技能锻造
- SelfImprovementLoop: 自己改進循環

使用例:
    >>> from agentflow.wizard import AgentWizard
    >>>
    >>> wizard = AgentWizard()
    >>> result = await wizard.create_from_description(
    ...     "PDFを解析して要約を生成するAgent"
    ... )
    >>> print(result.agent_spec)
"""

from agentflow.wizard.models import (
    AgentSpec,
    CapabilityGap,
    GapType,
    SynthesisResult,
    TestResult,
    ValidationResult,
    WizardConfig,
)
from agentflow.wizard.agent_wizard import AgentWizard
from agentflow.wizard.gap_detector import CapabilityGapDetector
from agentflow.wizard.system_synthesizer import SystemSynthesizer
from agentflow.wizard.skill_forge import SkillForge
from agentflow.wizard.improvement_loop import SelfImprovementLoop
from agentflow.wizard.test_synthesizer import TestSynthesizer

__version__ = "1.0.0"
__author__ = "AgentFlow Team"

__all__ = [
    # Models
    "AgentSpec",
    "CapabilityGap",
    "GapType",
    "SynthesisResult",
    "TestResult",
    "ValidationResult",
    "WizardConfig",
    # Main classes
    "AgentWizard",
    "CapabilityGapDetector",
    "SystemSynthesizer",
    "SkillForge",
    "SelfImprovementLoop",
    "TestSynthesizer",
]
