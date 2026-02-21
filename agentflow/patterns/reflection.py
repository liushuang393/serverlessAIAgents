"""Reflection Pattern - 自己評価と改善の反復.

このモジュールは Reflection Pattern を実装します：
- Generate → Reflect → Iterate ループ
- 自己評価による品質向上
- 反復制限による無限ループ防止

設計原則：
- 簡単：AgentBlock ベース、理解しやすい
- 柔軟：評価基準をカスタマイズ可能
- 健壮：最大反復回数で無限ループ防止
- 独立：外部フレームワーク不要

参考（思想のみ吸収）：
- Analytics Vidhya: Reflection Pattern
- Anthropic: Self-Correction with Chain-of-Thought

技術スタック：
- AgentBlock: Agent基底クラス
- Pydantic: データ検証
- PocketFlow: ワークフロー実行
"""

import logging
from typing import Any

from pydantic import BaseModel, Field

from agentflow.core.agent_block import AgentBlock
from agentflow.core.types import WorkflowConfig


class ReflectionResult(BaseModel):
    """評価結果."""

    is_acceptable: bool = Field(..., description="合格判定")
    score: float = Field(..., ge=0, le=100, description="スコア (0-100)")
    feedback: str = Field(default="", description="フィードバック")
    suggestions: list[str] = Field(default_factory=list, description="改善提案")


class ReflectorAgent(AgentBlock):
    """自己評価Agent - 生成結果を評価.

    責務：
    - 生成結果を評価基準に基づいて判定
    - スコアリング
    - 具体的なフィードバック生成

    Example:
        >>> reflector = ReflectorAgent(
        ...     llm_client=my_llm,
        ...     evaluation_criteria={
        ...         "clarity": "内容が明確か",
        ...         "accuracy": "情報が正確か"
        ...     }
        ... )
        >>> result = await reflector.run({"output": "生成結果"})
        >>> is_acceptable = result["is_acceptable"]
    """

    def __init__(
        self,
        llm_client: Any = None,
        evaluation_criteria: dict[str, str] | None = None,
        acceptance_threshold: float = 70.0,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント
            evaluation_criteria: 評価基準 {"項目": "説明"}
            acceptance_threshold: 合格スコア閾値 (0-100)
            **kwargs: AgentBlock への引数
        """
        super().__init__(**kwargs)
        self._llm = llm_client
        self._criteria = evaluation_criteria or {
            "quality": "出力の品質が十分か",
            "completeness": "必要な情報が全て含まれているか",
        }
        self._threshold = acceptance_threshold
        self._logger = logging.getLogger(__name__)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """生成結果を評価.

        Args:
            input_data: {
                "output": 評価対象の出力,
                "task": 元のタスク (オプション)
            }

        Returns:
            {
                "is_acceptable": bool,
                "score": float,
                "feedback": str,
                "suggestions": list[str],
                "output": 元の出力
            }
        """
        output = input_data.get("output", "")
        task = input_data.get("task", "")

        self._logger.info("評価開始")

        if self._llm:
            # LLM を使って評価
            prompt = self._build_evaluation_prompt(output, task)
            response = await self._llm.generate(prompt)
            reflection = self._parse_reflection(response)
        else:
            # Fallback: 簡易評価
            reflection = ReflectionResult(
                is_acceptable=len(output) > 10,
                score=50.0,
                feedback="LLM が設定されていないため簡易評価",
                suggestions=["LLM を設定してください"],
            )

        self._logger.info(f"評価完了: スコア={reflection.score}, 合格={reflection.is_acceptable}")

        return {
            "is_acceptable": reflection.is_acceptable,
            "score": reflection.score,
            "feedback": reflection.feedback,
            "suggestions": reflection.suggestions,
            "output": output,
            "task": task,
        }

    def _build_evaluation_prompt(self, output: str, task: str) -> str:
        """評価 Prompt を構築."""
        criteria_text = "\n".join([f"- {key}: {value}" for key, value in self._criteria.items()])

        return f"""あなたは品質評価の専門家です。以下の出力を評価してください。

【元のタスク】
{task if task else "（指定なし）"}

【生成された出力】
{output}

【評価基準】
{criteria_text}

以下の形式で評価してください：
1. 合格判定：Yes または No
2. スコア：0-100 の数値
3. フィードバック：具体的な問題点や良い点
4. 改善提案：どう改善すべきか（箇条書き）

回答形式：
判定: [Yes/No]
スコア: [数値]
フィードバック: [説明]
改善提案:
- [提案1]
- [提案2]
"""

    def _parse_reflection(self, response: str) -> ReflectionResult:
        """LLM レスポンスをパース."""
        # 簡易パース実装
        is_acceptable = "yes" in response.lower()[:100]
        score = self._threshold if is_acceptable else self._threshold - 10

        return ReflectionResult(
            is_acceptable=is_acceptable,
            score=score,
            feedback=response[:200],
            suggestions=["改善を続けてください"],
        )


class ImproverAgent(AgentBlock):
    """改善Agent - フィードバックに基づいて改善.

    責務：
    - フィードバックを分析
    - 改善提案を適用
    - 改善後の出力を生成

    Example:
        >>> improver = ImproverAgent(llm_client=my_llm)
        >>> result = await improver.run({
        ...     "output": "元の出力",
        ...     "feedback": "もっと詳しく",
        ...     "suggestions": ["具体例を追加"]
        ... })
        >>> improved = result["improved_output"]
    """

    def __init__(
        self,
        llm_client: Any = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント
            **kwargs: AgentBlock への引数
        """
        super().__init__(**kwargs)
        self._llm = llm_client
        self._logger = logging.getLogger(__name__)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """フィードバックに基づいて改善.

        Args:
            input_data: {
                "output": 元の出力,
                "feedback": フィードバック,
                "suggestions": 改善提案リスト,
                "task": 元のタスク (オプション)
            }

        Returns:
            {
                "improved_output": 改善後の出力,
                "task": 元のタスク
            }
        """
        output = input_data.get("output", "")
        feedback = input_data.get("feedback", "")
        suggestions = input_data.get("suggestions", [])
        task = input_data.get("task", "")

        self._logger.info("改善開始")

        if self._llm:
            # LLM を使って改善
            prompt = self._build_improvement_prompt(output, feedback, suggestions, task)
            improved = await self._llm.generate(prompt)
        else:
            # Fallback: 元の出力を返す
            improved = output + "\n[改善: LLM が設定されていません]"

        self._logger.info("改善完了")

        return {
            "improved_output": improved,
            "task": task,
        }

    def _build_improvement_prompt(
        self,
        output: str,
        feedback: str,
        suggestions: list[str],
        task: str,
    ) -> str:
        """改善 Prompt を構築."""
        suggestions_text = "\n".join([f"- {s}" for s in suggestions])

        return f"""あなたはコンテンツ改善の専門家です。以下の出力を改善してください。

【元のタスク】
{task if task else "（指定なし）"}

【現在の出力】
{output}

【フィードバック】
{feedback}

【改善提案】
{suggestions_text}

上記のフィードバックと提案に基づいて、出力を改善してください。
改善後の出力のみを返してください（説明不要）。
"""


class ReflectionLoop:
    """Reflectionループ制御.

    責務：
    - Generate → Reflect → Improveの反復
    - 最大反復回数の制限
    - 改善履歴の記録

    Example:
        >>> loop = ReflectionLoop(
        ...     generator=my_generator,
        ...     reflector=my_reflector,
        ...     improver=my_improver,
        ...     max_iterations=3
        ... )
        >>> result = await loop.execute("タスク内容")
    """

    def __init__(
        self,
        generator: AgentBlock,
        reflector: ReflectorAgent,
        improver: ImproverAgent,
        max_iterations: int = 3,
    ) -> None:
        """初期化.

        Args:
            generator: 初期生成 Agent
            reflector: 評価 Agent
            improver: 改善 Agent
            max_iterations: 最大反復回数
        """
        self._generator = generator
        self._reflector = reflector
        self._improver = improver
        self._max_iterations = max_iterations
        self._logger = logging.getLogger(__name__)

    async def execute(self, task: str) -> dict[str, Any]:
        """Reflection ループを実行.

        Args:
            task: タスク内容

        Returns:
            {
                "final_output": 最終出力,
                "iterations": 反復回数,
                "history": 改善履歴,
                "final_score": 最終スコア
            }
        """
        self._logger.info(f"Reflection ループ開始: max_iterations={self._max_iterations}")

        # 1. 初期生成
        gen_result = await self._generator.run({"task": task})
        current_output = gen_result.get("output", "")

        history = []
        iteration = 0

        # 2. 評価・改善ループ
        for iteration in range(self._max_iterations):
            self._logger.info(f"反復 {iteration + 1}/{self._max_iterations}")

            # 評価
            reflection = await self._reflector.run(
                {
                    "output": current_output,
                    "task": task,
                }
            )

            history.append(
                {
                    "iteration": iteration + 1,
                    "output": current_output,
                    "score": reflection["score"],
                    "feedback": reflection["feedback"],
                    "is_acceptable": reflection["is_acceptable"],
                }
            )

            # 合格判定
            if reflection["is_acceptable"]:
                self._logger.info(f"合格！反復 {iteration + 1} で終了")
                break

            # 改善
            if iteration < self._max_iterations - 1:
                improvement = await self._improver.run(
                    {
                        "output": current_output,
                        "feedback": reflection["feedback"],
                        "suggestions": reflection["suggestions"],
                        "task": task,
                    }
                )
                current_output = improvement["improved_output"]

        self._logger.info(f"Reflection ループ完了: 反復回数={iteration + 1}")

        return {
            "final_output": current_output,
            "iterations": iteration + 1,
            "history": history,
            "final_score": history[-1]["score"] if history else 0.0,
        }


class ReflectionWorkflow:
    """Reflection Workflowファクトリー.

    責務：
    - WorkflowConfigを生成
    - Generator + Reflector + Improverを組み合わせ
    - AgentFlowEngineと統合

    Example:
        >>> workflow = ReflectionWorkflow.create(
        ...     workflow_id="content-reflection",
        ...     generator=my_generator,
        ...     llm_client=my_llm,
        ...     evaluation_criteria={"quality": "品質が高いか"},
        ...     max_iterations=3
        ... )
        >>> engine = AgentFlowEngine()
        >>> engine.register_workflow(workflow)
        >>> result = await engine.execute("content-reflection", {"task": "記事を書く"})
    """

    @staticmethod
    def create(
        workflow_id: str,
        generator: AgentBlock,
        llm_client: Any,
        evaluation_criteria: dict[str, str] | None = None,
        max_iterations: int = 3,
        acceptance_threshold: float = 70.0,
    ) -> WorkflowConfig:
        """Reflection Workflow を作成.

        Args:
            workflow_id: ワークフロー ID
            generator: 初期生成 Agent
            llm_client: LLM クライアント
            evaluation_criteria: 評価基準
            max_iterations: 最大反復回数
            acceptance_threshold: 合格スコア閾値

        Returns:
            WorkflowConfig
        """
        # Reflector と Improver を作成
        reflector = ReflectorAgent(
            llm_client=llm_client,
            evaluation_criteria=evaluation_criteria,
            acceptance_threshold=acceptance_threshold,
        )

        improver = ImproverAgent(llm_client=llm_client)

        # ReflectionLoop を作成
        loop = ReflectionLoop(
            generator=generator,
            reflector=reflector,
            improver=improver,
            max_iterations=max_iterations,
        )

        # WorkflowConfig を構築
        # 注意：実際の実装では PocketFlow のノード定義が必要
        # ここでは簡略化のため、メタデータのみ設定
        return WorkflowConfig(
            workflow_id=workflow_id,
            name="Reflection Workflow",
            description=f"Generate-Reflect-Iterate ループ (最大{max_iterations}回)",
            nodes=[
                {
                    "id": "generator",
                    "type": "agent",
                    "agent": generator,
                },
                {
                    "id": "reflection_loop",
                    "type": "loop",
                    "loop": loop,
                },
            ],
            edges=[
                {"from": "generator", "to": "reflection_loop"},
            ],
            config={
                "max_iterations": max_iterations,
                "evaluation_criteria": evaluation_criteria,
                "acceptance_threshold": acceptance_threshold,
            },
        )


__all__ = [
    "ImproverAgent",
    "ReflectionLoop",
    "ReflectionResult",
    "ReflectionWorkflow",
    "ReflectorAgent",
]
