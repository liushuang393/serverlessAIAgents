"""ビジネスワークフローテンプレート.

Minimalist Entrepreneur スキルを連鎖実行するプリセットワークフローを定義する。
各ワークフローは SkillsManager.create_workflow() で登録可能な辞書形式。

使用例:
    >>> from apps.messaging_hub.skills.entrepreneurship.workflows import (
    ...     get_default_workflows,
    ... )
    >>> for wf_def in get_default_workflows():
    ...     await skills_manager.create_workflow(wf_def)
"""

from __future__ import annotations

from typing import Any


def _step(
    step_id: str,
    skill_name: str,
    *,
    on_success: str | None = None,
    on_failure: str | None = None,
    timeout: int = 120,
) -> dict[str, Any]:
    """ワークフローステップを構築するヘルパー.

    Args:
        step_id: ステップID
        skill_name: biz_ プレフィックス付きスキル名
        on_success: 成功時の次ステップID
        on_failure: 失敗時の次ステップID
        timeout: タイムアウト秒数

    Returns:
        ステップ定義辞書
    """
    return {
        "id": step_id,
        "skill_name": skill_name,
        "params": {
            "user_input": "${params.user_input}",
            "context": "${results." + step_id + "_prev.advice}" if on_success else "${params.context}",
        },
        "on_success": on_success,
        "on_failure": on_failure,
        "timeout_seconds": timeout,
    }


def get_default_workflows() -> list[dict[str, Any]]:
    """デフォルトのビジネスワークフロー定義を返す.

    Returns:
        ワークフロー定義辞書のリスト
    """
    return [
        # 1. 製品発見ジャーニー
        {
            "name": "製品発見ジャーニー",
            "description": (
                "コミュニティ発見 → アイデア検証 → プロセス化 → MVP定義。どの製品を作るべきかを段階的に明確化する。"
            ),
            "steps": [
                {
                    "id": "find_community",
                    "skill_name": "biz_find_community",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${params.context}",
                    },
                    "on_success": "validate_idea",
                    "timeout_seconds": 120,
                },
                {
                    "id": "validate_idea",
                    "skill_name": "biz_validate_idea",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.find_community.advice}",
                    },
                    "on_success": "processize",
                    "timeout_seconds": 120,
                },
                {
                    "id": "processize",
                    "skill_name": "biz_processize",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.validate_idea.advice}",
                    },
                    "on_success": "mvp",
                    "timeout_seconds": 120,
                },
                {
                    "id": "mvp",
                    "skill_name": "biz_mvp",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.processize.advice}",
                    },
                    "timeout_seconds": 120,
                },
            ],
            "entry_step_id": "find_community",
            "status": "active",
            "metadata": {
                "category": "entrepreneurship",
                "task_type": "product_discovery",
            },
        },
        # 2. 製品発売ジャーニー
        {
            "name": "製品発売ジャーニー",
            "description": ("MVP → 価格設定 → 顧客獲得 → マーケティング。製品を市場に投入するための完全プロセス。"),
            "steps": [
                {
                    "id": "mvp",
                    "skill_name": "biz_mvp",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${params.context}",
                    },
                    "on_success": "pricing",
                    "timeout_seconds": 120,
                },
                {
                    "id": "pricing",
                    "skill_name": "biz_pricing",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.mvp.advice}",
                    },
                    "on_success": "first_customers",
                    "timeout_seconds": 120,
                },
                {
                    "id": "first_customers",
                    "skill_name": "biz_first_customers",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.pricing.advice}",
                    },
                    "on_success": "marketing_plan",
                    "timeout_seconds": 120,
                },
                {
                    "id": "marketing_plan",
                    "skill_name": "biz_marketing_plan",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.first_customers.advice}",
                    },
                    "timeout_seconds": 120,
                },
            ],
            "entry_step_id": "mvp",
            "status": "active",
            "metadata": {
                "category": "entrepreneurship",
                "task_type": "product_launch",
            },
        },
        # 3. 収益性レビュー
        {
            "name": "収益性レビュー",
            "description": (
                "価格見直し → 持続可能成長 → ミニマリストレビュー。収益性を維持・向上させるための定期チェック。"
            ),
            "steps": [
                {
                    "id": "pricing",
                    "skill_name": "biz_pricing",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${params.context}",
                    },
                    "on_success": "grow_sustainably",
                    "timeout_seconds": 120,
                },
                {
                    "id": "grow_sustainably",
                    "skill_name": "biz_grow_sustainably",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.pricing.advice}",
                    },
                    "on_success": "minimalist_review",
                    "timeout_seconds": 120,
                },
                {
                    "id": "minimalist_review",
                    "skill_name": "biz_minimalist_review",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.grow_sustainably.advice}",
                    },
                    "timeout_seconds": 120,
                },
            ],
            "entry_step_id": "pricing",
            "status": "active",
            "metadata": {
                "category": "entrepreneurship",
                "task_type": "profitability",
            },
        },
        # 4. 製品改善サイクル
        {
            "name": "製品改善サイクル",
            "description": (
                "ミニマリストレビュー → アイデア再検証 → 次期MVP。製品改善を監視し、次の改善サイクルを回す。"
            ),
            "steps": [
                {
                    "id": "minimalist_review",
                    "skill_name": "biz_minimalist_review",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${params.context}",
                    },
                    "on_success": "validate_idea",
                    "timeout_seconds": 120,
                },
                {
                    "id": "validate_idea",
                    "skill_name": "biz_validate_idea",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.minimalist_review.advice}",
                    },
                    "on_success": "mvp",
                    "timeout_seconds": 120,
                },
                {
                    "id": "mvp",
                    "skill_name": "biz_mvp",
                    "params": {
                        "user_input": "${params.user_input}",
                        "context": "${results.validate_idea.advice}",
                    },
                    "timeout_seconds": 120,
                },
            ],
            "entry_step_id": "minimalist_review",
            "status": "active",
            "metadata": {
                "category": "entrepreneurship",
                "task_type": "product_improvement",
            },
        },
    ]
