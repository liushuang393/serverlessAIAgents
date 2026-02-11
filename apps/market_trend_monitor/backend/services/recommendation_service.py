"""アクション推奨エンジン.

Phase 13: Signal グレードに基づくコンテキスト固有の行動推奨。
"""

from __future__ import annotations

import logging
from typing import Any


# グレード別テンプレート
GRADE_TEMPLATES: dict[str, dict[str, str]] = {
    "A": {
        "priority": "high",
        "action_type": "immediate_action",
        "template": (
            "【即時対応推奨】{topic}は強力なシグナル(グレードA)を示しています。"
            "信頼性の高い複数ソースから裏付けられており、"
            "早期の戦略的意思決定が推奨されます。"
        ),
    },
    "B": {
        "priority": "medium",
        "action_type": "monitor_closely",
        "template": (
            "【注視推奨】{topic}は中程度のシグナル(グレードB)を示しています。"
            "追加情報の収集と定期的なモニタリングを推奨します。"
            "成長率や証拠の蓄積に応じて対応を検討してください。"
        ),
    },
    "C": {
        "priority": "low",
        "action_type": "background_tracking",
        "template": (
            "【参考情報】{topic}は弱いシグナル(グレードC)を示しています。"
            "バックグラウンドでの追跡を継続し、"
            "シグナル強度が上昇した場合に再評価してください。"
        ),
    },
    "D": {
        "priority": "none",
        "action_type": "no_action",
        "template": (
            "【ノイズ】{topic}は現時点では有意なシグナルを示していません(グレードD)。"
            "定期レポートで確認する程度で十分です。"
        ),
    },
}


class RecommendationService:
    """アクション推奨サービス."""

    def __init__(self) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)

    def generate_recommendation(
        self,
        topic: str,
        grade: str,
        score: float,
        growth_rate: float = 0.0,
        metadata: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """シグナルグレードに基づくアクション推奨を生成.

        Args:
            topic: トレンドトピック
            grade: シグナルグレード (A/B/C/D)
            score: 合計スコア
            growth_rate: 成長率
            metadata: 追加メタデータ

        Returns:
            推奨アクション辞書
        """
        template_data = GRADE_TEMPLATES.get(grade, GRADE_TEMPLATES["D"])

        recommendation = template_data["template"].format(topic=topic)

        # 成長率に基づく追加推奨
        growth_note = ""
        if growth_rate > 0.5:
            growth_note = "急速な成長を検知しています。早めの対応を検討してください。"
        elif growth_rate < -0.3:
            growth_note = "減退傾向にあります。既存投資の見直しを検討してください。"

        return {
            "topic": topic,
            "grade": grade,
            "score": round(score, 2),
            "priority": template_data["priority"],
            "action_type": template_data["action_type"],
            "recommendation": recommendation,
            "growth_note": growth_note,
            "metadata": metadata or {},
        }

    def generate_batch_recommendations(
        self,
        signals: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """複数シグナルの一括推奨生成."""
        results = []
        for signal in signals:
            rec = self.generate_recommendation(
                topic=signal.get("metadata", {}).get("trend_topic", "Unknown"),
                grade=signal.get("grade", "D"),
                score=signal.get("score", {}).get("total", 0.0)
                if isinstance(signal.get("score"), dict)
                else 0.0,
                growth_rate=signal.get("metadata", {}).get("growth_rate", 0.0),
                metadata=signal.get("metadata", {}),
            )
            results.append(rec)
        return sorted(results, key=lambda r: r["score"], reverse=True)
