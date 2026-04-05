"""PlannerAgent - タスク分解・実行計画生成エージェント.

ユーザー要求を受け取り、利用可能エージェントと制約条件を考慮して
ExecutionPlan（ステップ分解 + リスク付与）を生成する。
再計画（replan）にも対応し、失敗ステップ以降の計画を再生成する。
"""

from __future__ import annotations

import json
import logging
import uuid
from typing import Any

from harness.orchestration.models import (
    ExecutionPlan,
    PlannerInput,
    PlannerOutput,
    PlanStep,
    ReplanRequest,
)
from harness.risk.service import RiskLevel
from kernel.agents.resilient_agent import ResilientAgent


_logger = logging.getLogger(__name__)

# リスクが高いと推定されるキーワード
_HIGH_RISK_KEYWORDS: frozenset[str] = frozenset(
    {
        "delete",
        "drop",
        "remove",
        "destroy",
        "truncate",
        "削除",
        "破棄",
        "消去",
        "リセット",
    }
)

_MEDIUM_RISK_KEYWORDS: frozenset[str] = frozenset(
    {
        "update",
        "modify",
        "write",
        "send",
        "publish",
        "deploy",
        "更新",
        "変更",
        "書き込み",
        "送信",
        "公開",
        "デプロイ",
    }
)


# === LLM プロンプト構築 ===

_SYSTEM_PROMPT = """\
あなたはタスク分解の専門家です。
ユーザーの要求を分析し、実行可能なステップに分解してください。

出力は以下の JSON 形式で返してください:
{
  "steps": [
    {
      "step_id": "step-1",
      "agent_id": "利用可能エージェントのID",
      "description": "このステップの説明",
      "input_spec": {},
      "dependencies": [],
      "risk_level": "low"
    }
  ],
  "reasoning": "分解の理由"
}

ルール:
- 各ステップは1つのエージェントに対応させる
- dependencies には前提となるステップの step_id を指定する
- risk_level は "low", "medium", "high", "critical" のいずれか
- 並列実行可能なステップは dependencies を空にする
- JSON 以外のテキストは出力しない
"""


def _build_user_prompt(input_data: PlannerInput) -> str:
    """ユーザープロンプトを構築."""
    parts = [f"## ユーザー要求\n{input_data.user_request}"]

    if input_data.available_agents:
        agents_text = ", ".join(input_data.available_agents)
        parts.append(f"\n## 利用可能エージェント\n{agents_text}")

    if input_data.constraints:
        constraints_text = "\n".join(f"- {c}" for c in input_data.constraints)
        parts.append(f"\n## 制約条件\n{constraints_text}")

    if input_data.context:
        parts.append(f"\n## 追加コンテキスト\n{json.dumps(input_data.context, ensure_ascii=False)}")

    return "\n".join(parts)


def _build_replan_prompt(request: ReplanRequest) -> str:
    """再計画用プロンプトを構築."""
    completed_ids = list(request.completed_results.keys())
    return (
        f"## 再計画リクエスト\n"
        f"元の目標: {request.original_plan.goal}\n"
        f"失敗ステップ: {request.failed_step_id}\n"
        f"失敗理由: {request.failure_reason}\n"
        f"完了済みステップ: {', '.join(completed_ids) if completed_ids else 'なし'}\n\n"
        f"失敗ステップ以降を再計画してください。完了済みステップはそのまま活用します。"
    )


# === リスクレベル推定 ===


def _estimate_step_risk(step: PlanStep) -> RiskLevel:
    """ステップの説明からリスクレベルをヒューリスティックに推定.

    LLM が risk_level を指定済みの場合はそのまま使う。
    未指定または LOW の場合のみキーワードベースで上書きする。
    """
    if step.risk_level != RiskLevel.LOW:
        return step.risk_level

    description_lower = step.description.lower()

    for keyword in _HIGH_RISK_KEYWORDS:
        if keyword in description_lower:
            return RiskLevel.HIGH

    for keyword in _MEDIUM_RISK_KEYWORDS:
        if keyword in description_lower:
            return RiskLevel.MEDIUM

    return RiskLevel.LOW


def _parse_plan_json(raw: str, goal: str) -> ExecutionPlan:
    """LLM 出力の JSON を ExecutionPlan にパース.

    Args:
        raw: LLM の出力テキスト（JSON 想定）
        goal: 元のユーザー要求

    Returns:
        ExecutionPlan

    Raises:
        ValueError: JSON パース失敗時
    """
    # JSON ブロックの抽出（```json ... ``` 形式対応）
    text = raw.strip()
    if "```json" in text:
        start = text.index("```json") + 7
        end = text.index("```", start)
        text = text[start:end].strip()
    elif "```" in text:
        start = text.index("```") + 3
        end = text.index("```", start)
        text = text[start:end].strip()

    try:
        data = json.loads(text)
    except json.JSONDecodeError as e:
        msg = f"LLM 出力の JSON パースに失敗: {e}"
        raise ValueError(msg) from e

    steps_raw = data.get("steps", [])
    if not isinstance(steps_raw, list):
        msg = "steps フィールドがリストではありません"
        raise ValueError(msg)

    steps: list[PlanStep] = []
    for i, s in enumerate(steps_raw):
        step = PlanStep(
            step_id=s.get("step_id", f"step-{i + 1}"),
            agent_id=s.get("agent_id", "unknown"),
            description=s.get("description", ""),
            input_spec=s.get("input_spec", {}),
            dependencies=s.get("dependencies", []),
            risk_level=RiskLevel(s.get("risk_level", "low")),
            metadata=s.get("metadata", {}),
        )
        # ヒューリスティックでリスクを上書き
        step = step.model_copy(update={"risk_level": _estimate_step_risk(step)})
        steps.append(step)

    plan = ExecutionPlan(
        goal=goal,
        steps=steps,
        metadata={"reasoning": data.get("reasoning", "")},
    )
    return plan.model_copy(update={"overall_risk": plan.compute_overall_risk()})


# === PlannerAgent ===


class PlannerAgent(ResilientAgent[PlannerInput, PlannerOutput]):
    """タスク分解・実行計画生成エージェント.

    ユーザー要求を LLM で分析し、ExecutionPlan を生成する。
    再計画（replan）にも対応。
    """

    name: str = "PlannerAgent"
    timeout_seconds: int = 120
    max_retries: int = 2
    temperature: float = 0.3  # 計画は決定的であるべき

    def _parse_input(self, input_data: dict[str, Any]) -> PlannerInput:
        """入力データを PlannerInput に変換."""
        return PlannerInput.model_validate(input_data)

    async def process(self, input_data: PlannerInput) -> PlannerOutput:
        """タスク分解を実行し ExecutionPlan を生成.

        Args:
            input_data: ユーザー要求・コンテキスト・制約条件

        Returns:
            PlannerOutput（計画 + 推論過程 + 警告）
        """
        user_prompt = _build_user_prompt(input_data)

        response = await self._call_llm(
            prompt=f"{_SYSTEM_PROMPT}\n\n{user_prompt}",
        )

        plan = _parse_plan_json(response, goal=input_data.user_request)

        warnings = self._validate_plan(plan, input_data.available_agents)

        return PlannerOutput(
            plan=plan,
            reasoning=plan.metadata.get("reasoning", ""),
            warnings=warnings,
        )

    async def replan(self, request: ReplanRequest) -> PlannerOutput:
        """失敗ステップ以降を再計画.

        Args:
            request: 再計画リクエスト

        Returns:
            PlannerOutput（新しい計画）
        """
        user_prompt = _build_replan_prompt(request)

        # 元の計画から利用可能エージェントを抽出
        available_agents = list({s.agent_id for s in request.original_plan.steps})

        response = await self._call_llm(
            prompt=f"{_SYSTEM_PROMPT}\n\n{user_prompt}",
        )

        new_plan = _parse_plan_json(response, goal=request.original_plan.goal)

        # 完了済みステップを保持し、新ステップを追加
        completed_step_ids = set(request.completed_results.keys())
        kept_steps = [s for s in request.original_plan.steps if s.step_id in completed_step_ids]
        # 新ステップの step_id 衝突を回避
        existing_ids = {s.step_id for s in kept_steps}
        for step in new_plan.steps:
            if step.step_id in existing_ids:
                step = step.model_copy(
                    update={"step_id": f"{step.step_id}-r{uuid.uuid4().hex[:4]}"},
                )
            kept_steps.append(step)

        merged_plan = new_plan.model_copy(
            update={
                "plan_id": request.original_plan.plan_id,
                "steps": kept_steps,
            }
        )
        merged_plan = merged_plan.model_copy(
            update={"overall_risk": merged_plan.compute_overall_risk()},
        )

        warnings = self._validate_plan(merged_plan, available_agents)

        return PlannerOutput(
            plan=merged_plan,
            reasoning=new_plan.metadata.get("reasoning", ""),
            warnings=warnings,
        )

    def _validate_plan(
        self,
        plan: ExecutionPlan,
        available_agents: list[str],
    ) -> list[str]:
        """計画の妥当性を検証し、警告リストを返す."""
        warnings: list[str] = []

        if not plan.steps:
            warnings.append("計画にステップが含まれていません")

        # 依存関係の整合性チェック
        step_ids = {s.step_id for s in plan.steps}
        for step in plan.steps:
            for dep in step.dependencies:
                if dep not in step_ids:
                    warnings.append(f"ステップ '{step.step_id}' の依存 '{dep}' が計画内に存在しません")

        # エージェントID の存在チェック
        if available_agents:
            available_set = set(available_agents)
            for step in plan.steps:
                if step.agent_id not in available_set:
                    warnings.append(
                        f"ステップ '{step.step_id}' のエージェント '{step.agent_id}' は利用可能リストに含まれません"
                    )

        # 循環依存の簡易チェック
        if self._has_cycle(plan.steps):
            warnings.append("ステップ間に循環依存が検出されました")

        return warnings

    @staticmethod
    def _has_cycle(steps: list[PlanStep]) -> bool:
        """トポロジカルソートで循環依存を検出."""
        graph: dict[str, list[str]] = {s.step_id: list(s.dependencies) for s in steps}
        visited: set[str] = set()
        in_stack: set[str] = set()

        def _dfs(node: str) -> bool:
            if node in in_stack:
                return True
            if node in visited:
                return False
            visited.add(node)
            in_stack.add(node)
            for dep in graph.get(node, []):
                if _dfs(dep):
                    return True
            in_stack.discard(node)
            return False

        return any(_dfs(s.step_id) for s in steps if s.step_id not in visited)


__all__ = ["PlannerAgent"]
