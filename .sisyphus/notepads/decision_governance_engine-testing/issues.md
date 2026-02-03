## 2026-02-03 Issues

- pytest decision_governance_engine unit test failing:
  - tests/unit/test_decision_governance_engine.py::TestDecisionEngine::test_reject_invalid_question
  - expected status in ['rejected','cognitive_gate_blocked'] but got 'success' with is_acceptable=False

- KnowledgePage uses dynamic Tailwind class names (bg-${agentInfo.color}-600, text-${agentInfo.color}-300, etc.).
  - Risk: Tailwind JIT may not include these styles -> UI may degrade.
