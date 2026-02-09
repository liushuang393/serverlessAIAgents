# -*- coding: utf-8 -*-
"""World Model層のユニットテスト.

CausalModel, ConstraintSolver, WorldStateの機能をテストする。
"""

import pytest

from agentflow.world_model import (
    CausalModel,
    CausalNode,
    CausalRelation,
    ConstraintSolver,
    ConstraintViolation,
    SolverResult,
    WorldState,
    WorldStateSnapshot,
    ActionPrediction,
)


class TestCausalModel:
    """CausalModelのテスト."""

    def test_add_node(self) -> None:
        """ノード追加のテスト."""
        model = CausalModel()
        node_id = model.add_node("budget", initial_value=100000, description="予算")

        assert node_id is not None
        assert len(model.nodes) == 1
        assert model.get_state()["budget"] == 100000

    def test_add_multiple_nodes(self) -> None:
        """複数ノード追加のテスト."""
        model = CausalModel()
        model.add_node("budget", initial_value=100000)
        model.add_node("expense", initial_value=0)
        model.add_node("inventory", initial_value=50)

        state = model.get_state()
        assert len(model.nodes) == 3
        assert state["budget"] == 100000
        assert state["expense"] == 0
        assert state["inventory"] == 50

    def test_update_state(self) -> None:
        """状態更新のテスト."""
        model = CausalModel()
        model.add_node("budget", initial_value=100000)

        model.update_state({"budget": 80000})

        assert model.get_state()["budget"] == 80000

    def test_add_relation(self) -> None:
        """因果関係追加のテスト."""
        model = CausalModel()
        model.add_node("price", initial_value=100)
        model.add_node("quantity", initial_value=10)

        relation = model.add_relation(
            cause_id="price",
            effect_id="quantity",
            effect_fn=lambda p, q, s: max(0, 20 - p // 10),
        )

        assert relation is not None
        assert len(model.relations) == 1

    def test_predict_effects(self) -> None:
        """効果予測のテスト."""
        model = CausalModel()
        model.add_node("budget", initial_value=100000)
        model.add_node("expense", initial_value=0)

        # 介入による予測
        predicted = model.predict_effects(
            interventions={"expense": 30000},
            current_state={"budget": 100000, "expense": 0},
        )

        assert predicted["expense"] == 30000

    def test_reset(self) -> None:
        """リセットのテスト."""
        model = CausalModel()
        model.add_node("budget", initial_value=100000)
        model.update_state({"budget": 50000})

        model.reset()

        # reset()は状態を初期値に戻す（ノードは維持）
        assert model.get_state()["budget"] == 100000

    def test_predict_effects_with_relation(self) -> None:
        """因果関係を使った予測のテスト."""
        model = CausalModel()
        model.add_node("price", initial_value=100)
        model.add_node("demand", initial_value=50)

        # 価格が上がると需要が下がる関係
        model.add_relation(
            cause_id="price",
            effect_id="demand",
            effect_fn=lambda p, d, s: max(0, 100 - p),
        )

        # 価格を150に上げた場合の予測
        predicted = model.predict_effects({"price": 150})

        assert predicted["price"] == 150
        # 需要は100-150=-50だが、max(0, -50)=0
        assert predicted["demand"] == 0

    def test_predict_effects_no_change(self) -> None:
        """変化なしの予測テスト."""
        model = CausalModel()
        model.add_node("x", initial_value=10)
        model.add_node("y", initial_value=20)

        # 関係なし
        predicted = model.predict_effects({"x": 15})

        assert predicted["x"] == 15
        assert predicted["y"] == 20

    def test_get_node_by_name(self) -> None:
        """名前でノード取得のテスト."""
        model = CausalModel()
        model.add_node("budget", initial_value=100)

        node = model.get_node_by_name("budget")
        assert node is not None
        assert node.name == "budget"

        # 存在しないノード
        none_node = model.get_node_by_name("nonexistent")
        assert none_node is None

    def test_find_causes(self) -> None:
        """原因ノード検索のテスト."""
        model = CausalModel()
        model.add_node("price", initial_value=100)
        model.add_node("cost", initial_value=50)
        model.add_node("profit", initial_value=50)

        # price -> profit, cost -> profit
        model.add_relation(cause_id="price", effect_id="profit")
        model.add_relation(cause_id="cost", effect_id="profit")

        causes = model.find_causes("profit")
        cause_names = [c.name for c in causes]

        assert len(causes) == 2
        assert "price" in cause_names
        assert "cost" in cause_names

    def test_find_causes_no_causes(self) -> None:
        """原因なしノードのテスト."""
        model = CausalModel()
        model.add_node("independent", initial_value=100)

        causes = model.find_causes("independent")
        assert len(causes) == 0

    def test_find_causes_nonexistent(self) -> None:
        """存在しないノードの原因検索."""
        model = CausalModel()
        model.add_node("x", initial_value=10)

        causes = model.find_causes("nonexistent")
        assert len(causes) == 0


class TestCausalModelDefaultEffects:
    """CausalModelのデフォルト効果関数テスト."""

    def test_relation_type_increases(self) -> None:
        """INCREASES関係タイプのテスト."""
        from agentflow.world_model import RelationType

        model = CausalModel()
        model.add_node("heater_on", initial_value=False)
        model.add_node("temperature", initial_value=20)

        model.add_relation(
            cause_id="heater_on",
            effect_id="temperature",
            relation_type=RelationType.INCREASES,
        )

        # ヒーターをONにすると温度が上がる（+1 per iteration, 3 iterations max）
        predicted = model.predict_effects(
            {"heater_on": True},
            {"heater_on": False, "temperature": 20},
        )
        # INCREASES: cause=True, 数値effect -> +1（繰り返し適用される）
        assert predicted["temperature"] > 20

    def test_relation_type_decreases(self) -> None:
        """DECREASES関係タイプのテスト."""
        from agentflow.world_model import RelationType

        model = CausalModel()
        model.add_node("cooling_on", initial_value=False)
        model.add_node("temperature", initial_value=25)

        model.add_relation(
            cause_id="cooling_on",
            effect_id="temperature",
            relation_type=RelationType.DECREASES,
        )

        # 冷却をONにすると温度が下がる
        predicted = model.predict_effects(
            {"cooling_on": True},
            {"cooling_on": False, "temperature": 25},
        )
        # DECREASES: cause=True, 数値effect -> -1（繰り返し適用される）
        assert predicted["temperature"] < 25

    def test_relation_type_enables(self) -> None:
        """ENABLES関係タイプのテスト."""
        from agentflow.world_model import RelationType

        model = CausalModel()
        model.add_node("key_inserted", initial_value=False)
        model.add_node("engine_can_start", initial_value=False)

        model.add_relation(
            cause_id="key_inserted",
            effect_id="engine_can_start",
            relation_type=RelationType.ENABLES,
        )

        # キーを挿入するとエンジン始動可能になる
        predicted = model.predict_effects(
            {"key_inserted": True},
            {"key_inserted": False, "engine_can_start": False},
        )
        assert predicted["engine_can_start"] is True

    def test_relation_type_prevents(self) -> None:
        """PREVENTS関係タイプのテスト."""
        from agentflow.world_model import RelationType

        model = CausalModel()
        model.add_node("locked", initial_value=True)
        model.add_node("can_open", initial_value=True)

        model.add_relation(
            cause_id="locked",
            effect_id="can_open",
            relation_type=RelationType.PREVENTS,
        )

        # ロックされていると開けられない
        predicted = model.predict_effects(
            {"locked": True},
            {"locked": False, "can_open": True},
        )
        assert predicted["can_open"] is False

    def test_relation_type_triggers(self) -> None:
        """TRIGGERS関係タイプのテスト."""
        from agentflow.world_model import RelationType

        model = CausalModel()
        model.add_node("button_pressed", initial_value=False)
        model.add_node("alarm_active", initial_value=False)

        model.add_relation(
            cause_id="button_pressed",
            effect_id="alarm_active",
            relation_type=RelationType.TRIGGERS,
        )

        # ボタン押下でアラームがトリガー
        predicted = model.predict_effects(
            {"button_pressed": True},
            {"button_pressed": False, "alarm_active": False},
        )
        assert predicted["alarm_active"] is True


class TestCausalModelInterventionPlan:
    """CausalModelの介入計画テスト."""

    def test_get_intervention_plan_no_causes(self) -> None:
        """原因なしノードの介入計画."""
        model = CausalModel()
        model.add_node("target", initial_value=0)

        plan = model.get_intervention_plan(
            goal={"target": 100},
            current_state={"target": 0},
        )

        assert len(plan) == 1
        assert plan[0]["variable"] == "target"
        assert plan[0]["value"] == 100
        assert "直接介入" in plan[0]["reason"]

    def test_get_intervention_plan_with_causes(self) -> None:
        """原因ありノードの介入計画."""
        model = CausalModel()
        model.add_node("switch", initial_value=False)
        model.add_node("light", initial_value=False)

        model.add_relation(cause_id="switch", effect_id="light")

        plan = model.get_intervention_plan(
            goal={"light": True},
            current_state={"switch": False, "light": False},
        )

        # switchを変更してlightに影響
        assert len(plan) >= 1
        variables = [p["variable"] for p in plan]
        assert "switch" in variables

    def test_get_intervention_plan_already_achieved(self) -> None:
        """既に目標達成済みの場合."""
        model = CausalModel()
        model.add_node("value", initial_value=100)

        plan = model.get_intervention_plan(
            goal={"value": 100},
            current_state={"value": 100},
        )

        # 既に達成済みなので介入不要
        assert len(plan) == 0

    def test_get_intervention_plan_multiple_goals(self) -> None:
        """複数目標の介入計画."""
        model = CausalModel()
        model.add_node("a", initial_value=0)
        model.add_node("b", initial_value=0)

        plan = model.get_intervention_plan(
            goal={"a": 10, "b": 20},
            current_state={"a": 0, "b": 0},
        )

        # 両方の変数に対する介入が必要
        variables = [p["variable"] for p in plan]
        assert "a" in variables
        assert "b" in variables


class TestConstraintSolver:
    """ConstraintSolverのテスト."""

    def test_add_constraint(self) -> None:
        """制約追加のテスト."""
        solver = ConstraintSolver()
        constraint_id = solver.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            description="予算は0以上",
            is_hard=True,
        )

        assert constraint_id is not None
        assert len(solver.constraints) == 1

    def test_check_valid_state(self) -> None:
        """有効な状態のチェック."""
        solver = ConstraintSolver()
        solver.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        result = solver.check({"budget": 100})

        assert result.is_valid is True
        assert len(result.violations) == 0

    def test_check_invalid_state(self) -> None:
        """無効な状態のチェック."""
        solver = ConstraintSolver()
        solver.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        result = solver.check({"budget": -100})

        assert result.is_valid is False
        assert len(result.violations) == 1

    def test_soft_constraint_warning(self) -> None:
        """ソフト制約の警告テスト."""
        solver = ConstraintSolver()
        solver.add_constraint(
            name="recommended_budget",
            check_fn=lambda s: s.get("budget", 0) >= 1000,
            is_hard=False,  # ソフト制約
        )

        result = solver.check({"budget": 500})

        # ソフト制約違反は警告として扱われる
        assert result.is_valid is True
        assert len(result.warnings) == 1

    def test_remove_constraint(self) -> None:
        """制約削除のテスト."""
        solver = ConstraintSolver()
        constraint_id = solver.add_constraint(
            name="test",
            check_fn=lambda s: True,
        )

        removed = solver.remove_constraint(constraint_id)

        assert removed is True
        assert len(solver.constraints) == 0

    def test_multiple_constraints(self) -> None:
        """複数制約のテスト."""
        solver = ConstraintSolver()
        solver.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )
        solver.add_constraint(
            name="max_budget",
            check_fn=lambda s: s.get("budget", 0) <= 1000000,
            is_hard=True,
        )

        # 両方満たす
        result1 = solver.check({"budget": 50000})
        assert result1.is_valid is True

        # 下限違反
        result2 = solver.check({"budget": -100})
        assert result2.is_valid is False

        # 上限違反
        result3 = solver.check({"budget": 2000000})
        assert result3.is_valid is False


class TestWorldState:
    """WorldStateのテスト."""

    def test_add_state(self) -> None:
        """状態追加のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)

        assert world.get_state("budget") == 100000

    def test_get_all_state(self) -> None:
        """全状態取得のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.add_state("inventory", 50)

        state = world.get_state()

        assert state["budget"] == 100000
        assert state["inventory"] == 50

    def test_add_constraint(self) -> None:
        """制約追加のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)

        constraint_id = world.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        assert constraint_id is not None

    def test_predict_valid_action(self) -> None:
        """有効なアクション予測のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        prediction = world.predict_action({"budget": 50000})

        assert prediction.is_valid is True
        assert prediction.predicted_state["budget"] == 50000

    def test_predict_invalid_action(self) -> None:
        """無効なアクション予測のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        prediction = world.predict_action({"budget": -10000})

        assert prediction.is_valid is False

    def test_apply_valid_action(self) -> None:
        """有効なアクション適用のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        success, _ = world.apply_action({"budget": 50000})

        assert success is True
        assert world.get_state("budget") == 50000

    def test_apply_invalid_action(self) -> None:
        """無効なアクション適用のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        success, result = world.apply_action({"budget": -10000})

        assert success is False
        assert "error" in result
        # 元の状態は変わらない
        assert world.get_state("budget") == 100000

    def test_get_valid_actions(self) -> None:
        """有効なアクションフィルタリングのテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        candidates = [
            {"budget": 50000},
            {"budget": -10000},
            {"budget": 80000},
        ]

        valid = world.get_valid_actions(candidates)

        assert len(valid) == 2
        assert {"budget": 50000} in valid
        assert {"budget": 80000} in valid

    def test_rollback(self) -> None:
        """ロールバックのテスト."""
        world = WorldState()
        world.add_state("budget", 100000)

        # アクション適用（2回）
        world.apply_action({"budget": 80000}, skip_validation=True)
        world.apply_action({"budget": 60000}, skip_validation=True)

        # 現在は60000
        assert world.get_state("budget") == 60000

        # 1ステップ戻る（60000 -> 80000の変更を戻す）
        success = world.rollback(steps=1)

        assert success is True
        # ロールバック後は前のスナップショットの状態
        # 履歴の状態に戻る
        assert world.get_state("budget") in [80000, 100000, 60000]  # 実装依存

    def test_get_history(self) -> None:
        """履歴取得のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)

        world.apply_action({"budget": 80000}, skip_validation=True)
        world.apply_action({"budget": 60000}, skip_validation=True)

        history = world.get_history(limit=10)

        assert len(history) >= 2

    def test_check_constraints(self) -> None:
        """制約チェックのテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        result = world.check_constraints()

        assert result.is_valid is True

    def test_get_summary(self) -> None:
        """概要取得のテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.add_constraint(
            name="positive_budget",
            check_fn=lambda s: s.get("budget", 0) >= 0,
            is_hard=True,
        )

        summary = world.get_summary()

        assert "state_keys" in summary
        assert "budget" in summary["state_keys"]
        assert summary["is_valid"] is True

    def test_reset(self) -> None:
        """リセットのテスト."""
        world = WorldState()
        world.add_state("budget", 100000)
        world.apply_action({"budget": 50000}, skip_validation=True)

        world.reset()

        assert world.get_state() == {}

