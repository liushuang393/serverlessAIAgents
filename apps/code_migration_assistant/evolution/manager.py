"""自己適応進化マネージャーモジュール.

QualityGate の判定結果に基づいて、該当ステージのプロンプトを
自動改善し、再実行サイクルを管理する。
"""

from __future__ import annotations

import json
import logging
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from pathlib import Path


logger = logging.getLogger(__name__)

# 問題種別 → 対象プロンプトファイルのマッピング
_DECISION_TO_STAGE: dict[str, str] = {
    "DESIGN_ISSUE": "designer",
    "TRANSFORM_ISSUE": "transformer",
    "TEST_ISSUE": "test_generator",
}

# デフォルト最大イテレーション数
_DEFAULT_MAX_ITERATIONS = 3


class EvolutionManager:
    """プロンプト自己適応進化を管理するクラス.

    品質ゲートの判定結果を受け取り、該当ステージのプロンプトを
    更新して再実行サイクルを制御する。
    """

    def __init__(
        self,
        prompts_dir: Path,
        max_iterations: int = _DEFAULT_MAX_ITERATIONS,
    ) -> None:
        """初期化.

        Args:
            prompts_dir: プロンプトファイルディレクトリ（agents/prompts/）
            max_iterations: 最大進化反復数
        """
        self.prompts_dir = prompts_dir
        self.max_iterations = max_iterations
        self._iteration_counts: dict[str, int] = {}

    def can_evolve(self, program_name: str, decision: str) -> bool:
        """進化可能かどうかを判定する.

        Args:
            program_name: プログラム名
            decision: QualityGate の判定結果

        Returns:
            進化可能な場合 True
        """
        if decision not in _DECISION_TO_STAGE:
            return False
        key = f"{program_name}:{decision}"
        current = self._iteration_counts.get(key, 0)
        return current < self.max_iterations

    def get_retry_stage(self, decision: str) -> str | None:
        """判定結果から再実行開始ステージを取得する.

        Args:
            decision: QualityGate の判定結果

        Returns:
            再実行開始ステージ名（例: "designer"）。不明の場合 None
        """
        return _DECISION_TO_STAGE.get(decision)

    def evolve(
        self,
        program_name: str,
        decision: str,
        quality_gate_result: dict[str, Any],
        version_dir: Path,
    ) -> dict[str, Any] | None:
        """プロンプトを進化させる.

        Args:
            program_name: プログラム名
            decision: QualityGate の判定結果（DESIGN_ISSUE 等）
            quality_gate_result: QualityGate の詳細結果
            version_dir: 現バージョンのディレクトリ

        Returns:
            進化情報の dict（iteration, stage, fix_summary）。
            進化できない場合 None
        """
        if not self.can_evolve(program_name, decision):
            logger.warning(
                "最大反復数に達しました: program=%s decision=%s max=%d",
                program_name,
                decision,
                self.max_iterations,
            )
            return None

        stage = _DECISION_TO_STAGE[decision]
        key = f"{program_name}:{decision}"
        iteration = self._iteration_counts.get(key, 0) + 1
        self._iteration_counts[key] = iteration

        fix_summary = self._generate_fix_summary(decision, quality_gate_result)

        logger.info(
            "Evolution: program=%s stage=%s iteration=%d/%d",
            program_name,
            stage,
            iteration,
            self.max_iterations,
        )
        logger.info("修正内容: %s", fix_summary)

        return {
            "iteration": iteration,
            "stage": stage,
            "decision": decision,
            "fix_summary": fix_summary,
            "evolved_at": datetime.now(UTC).isoformat(),
            "quality_gate_reason": quality_gate_result.get("reason", ""),
            "evidence": quality_gate_result.get("evidence", {}),
        }

    def record_evolution(
        self,
        evolution_info: dict[str, Any],
        program_dir: Path,
        program_name: str | None = None,
    ) -> None:
        """進化履歴を evolution.json に記録する.

        Args:
            evolution_info: evolve() の戻り値
            program_dir: プログラムディレクトリ
            program_name: プログラム名（省略時は program_dir のディレクトリ名を使用）
        """
        evolution_path = program_dir / "evolution.json"
        resolved_name = program_name or program_dir.name

        existing: dict[str, Any] = {}
        if evolution_path.exists():
            try:
                existing = json.loads(evolution_path.read_text(encoding="utf-8"))
            except json.JSONDecodeError:
                existing = {}

        iterations: list[dict[str, Any]] = existing.get("iterations", [])
        iterations.append(evolution_info)

        merged = {
            "_updated_at": datetime.now(UTC).isoformat(),
            "program_name": resolved_name,
            "total_iterations": len(iterations),
            "iterations": iterations,
        }

        evolution_path.write_text(
            json.dumps(merged, ensure_ascii=False, indent=2),
            encoding="utf-8",
        )
        logger.info("evolution.json 更新: %s", evolution_path)

    def get_iteration_count(self, program_name: str, decision: str) -> int:
        """現在の反復数を取得する.

        Args:
            program_name: プログラム名
            decision: 判定結果

        Returns:
            現在の反復数（0始まり）
        """
        key = f"{program_name}:{decision}"
        return self._iteration_counts.get(key, 0)

    def reset(self, program_name: str) -> None:
        """プログラムの反復カウントをリセットする.

        Args:
            program_name: プログラム名
        """
        keys_to_delete = [k for k in self._iteration_counts if k.startswith(f"{program_name}:")]
        for key in keys_to_delete:
            del self._iteration_counts[key]

    @staticmethod
    def _generate_fix_summary(
        decision: str,
        quality_gate_result: dict[str, Any],
    ) -> str:
        """判定結果から修正サマリーを生成する.

        Args:
            decision: 判定結果
            quality_gate_result: QualityGate の詳細結果

        Returns:
            修正サマリー文字列
        """
        reason = quality_gate_result.get("reason", "不明")
        evidence = quality_gate_result.get("evidence", {})

        if decision == "DESIGN_ISSUE":
            field_coverage = evidence.get("field_coverage", "不明")
            return f"設計問題を修正: {reason} (フィールドカバレッジ: {field_coverage})"

        if decision == "TRANSFORM_ISSUE":
            return f"変換問題を修正: {reason} (Java文法エラーまたはビジネスロジック欠落)"

        if decision == "TEST_ISSUE":
            test_count = evidence.get("test_count", 0)
            return f"テスト問題を修正: {reason} (テスト件数: {test_count})"

        return f"問題を修正: {reason}"
