# -*- coding: utf-8 -*-
"""Transformation reflection loop helpers."""

from __future__ import annotations

from collections.abc import Awaitable, Callable
from typing import Any

from apps.code_migration_assistant.workflow.models import TransformationIterationRecord


async def run_reflection_loop(
    *,
    generate: Callable[[list[str]], dict[str, Any]],
    evaluate: Callable[[dict[str, Any]], Awaitable[dict[str, Any]]],
    acceptance_threshold: float,
    max_iterations: int,
) -> tuple[dict[str, Any], list[TransformationIterationRecord], bool, float | None]:
    """Generate->Evaluate 反復ループを実行."""
    feedback: list[str] = []
    records: list[TransformationIterationRecord] = []
    current_payload: dict[str, Any] = {}
    accepted = False
    final_score: float | None = None
    previous_code: str | None = None
    previous_score: float | None = None

    for iteration in range(1, max_iterations + 1):
        current_payload = generate(feedback)
        evaluation = await evaluate(current_payload)

        score_raw = evaluation.get("score")
        score = float(score_raw) if isinstance(score_raw, (int, float)) else None
        accepted = bool(evaluation.get("is_acceptable", False))
        final_score = score

        record = TransformationIterationRecord(
            iteration=iteration,
            score=score,
            accepted=accepted,
            feedback=list(evaluation.get("feedback", [])),
            suggestions=list(evaluation.get("suggestions", [])),
        )
        records.append(record)

        if accepted:
            break

        next_feedback = []
        for key in ("feedback", "suggestions"):
            values = evaluation.get(key, [])
            if isinstance(values, list):
                next_feedback.extend(str(item) for item in values)
        feedback = next_feedback

        target_code = current_payload.get("target_code")
        if isinstance(target_code, str) and previous_code == target_code:
            if previous_score is not None and score is not None and score <= previous_score:
                break
        previous_code = target_code if isinstance(target_code, str) else None
        previous_score = score

    if final_score is not None and final_score >= acceptance_threshold:
        accepted = True
    return current_payload, records, accepted, final_score
