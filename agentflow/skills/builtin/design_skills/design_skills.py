"""Design Skills スキル実装.

DesignSkillsEngine を AgentFlow スキルとして公開。

使用例:
    >>> from agentflow.skills.builtin.design_skills.design_skills import run
    >>> result = await run({"brief": "商品画像を生成", "num_images": 4})
"""

from typing import Any

from agentflow.skills.builtin.design_skills.engine import DesignSkillsEngine


async def run(input_data: dict[str, Any]) -> dict[str, Any]:
    """デザインスキルパイプラインを実行.

    Args:
        input_data: デザインブリーフパラメータ

    Returns:
        生成画像とメタデータ
    """
    engine = DesignSkillsEngine()
    result = await engine.run(input_data)
    return result if isinstance(result, dict) else {"result": result}
