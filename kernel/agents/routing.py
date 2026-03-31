"""Agent routing policy and ranking utilities."""

from __future__ import annotations

from dataclasses import dataclass, field

from kernel.agents.contracts import AgentDescriptor, AgentFeedbackSummary


@dataclass(slots=True)
class AgentRoutingPolicy:
    """静的ルーティングポリシー.

    決定順序は `explicit > policy > capability > feedback > fallback` を想定する。
    """

    preferred_agents: list[str] = field(default_factory=list)
    agent_weights: dict[str, float] = field(default_factory=dict)
    fallback_agents: list[str] = field(default_factory=list)
    use_feedback: bool = True


@dataclass(slots=True)
class AgentRouteCandidate:
    """route 候補."""

    agent_id: str
    score: float
    reasons: list[str] = field(default_factory=list)


def rank_agent_candidates(
    *,
    descriptors: list[AgentDescriptor],
    feedback_by_agent: dict[str, AgentFeedbackSummary],
    required_capability: str | None = None,
    policy: AgentRoutingPolicy | None = None,
    explicit_agent_id: str | None = None,
) -> list[AgentRouteCandidate]:
    """候補 agent をスコア順に返す."""
    if explicit_agent_id:
        return [AgentRouteCandidate(agent_id=explicit_agent_id, score=10_000.0, reasons=["explicit"])]

    policy = policy or AgentRoutingPolicy()
    preferred_index = {agent_id: index for index, agent_id in enumerate(policy.preferred_agents)}
    fallback_set = set(policy.fallback_agents)
    candidates: list[AgentRouteCandidate] = []

    for descriptor in descriptors:
        score = 0.0
        reasons: list[str] = []

        if descriptor.agent_id in preferred_index:
            bonus = max(0, 100.0 - preferred_index[descriptor.agent_id])
            score += bonus
            reasons.append("policy")

        if required_capability and required_capability in descriptor.capabilities:
            score += 10.0
            reasons.append("capability")

        policy_weight = policy.agent_weights.get(descriptor.agent_id)
        if policy_weight is not None:
            score += policy_weight
            reasons.append("policy_weight")

        if policy.use_feedback:
            feedback = feedback_by_agent.get(descriptor.agent_id)
            if feedback is not None:
                score += feedback.success_rate * 5.0
                if feedback.total_runs > 0:
                    reasons.append("feedback")

        if descriptor.agent_id in fallback_set:
            score -= 100.0
            reasons.append("fallback")

        candidates.append(
            AgentRouteCandidate(
                agent_id=descriptor.agent_id,
                score=score,
                reasons=reasons,
            )
        )

    candidates.sort(key=lambda item: item.score, reverse=True)
    return candidates


__all__ = [
    "AgentRouteCandidate",
    "AgentRoutingPolicy",
    "rank_agent_candidates",
]
