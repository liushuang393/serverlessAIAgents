"""Messaging Hub - プロンプトレイヤー定義（6層プロンプトレイヤー対応版）.

kernel/prompts の 6 層モデルに準拠し、L1(CoreSystem) を共通化。
MULTI_TURN パターン: Intent分類→specialist dispatch→summary の会話。

使用例:
    >>> from apps.messaging_hub.prompts import CORE_SYSTEM, COORDINATOR_TASK
    >>> from kernel.prompts import (
    ...     PromptLayerSet, PromptPattern, RuntimeContextLayer,
    ...     ConversationStateLayer,
    ... )
    >>> layers = PromptLayerSet(
    ...     core_system=CORE_SYSTEM,
    ...     runtime_context=RuntimeContextLayer(user_request=message),
    ...     conversation_state=ConversationStateLayer(
    ...         decisions=prior_decisions,
    ...         turn_count=turn,
    ...     ),
    ... )
"""

from kernel.prompts.models import CoreSystemLayer, TaskSystemLayer


# ---------------------------------------------------------------------------
# L1: 全 Agent 共通 CoreSystem
# ---------------------------------------------------------------------------
CORE_SYSTEM = CoreSystemLayer(
    role=(
        "あなたは主管向けパーソナルアシスタントである。"
        "自然言語メッセージを解析し、適切なタスクを実行して簡潔なサマリーを返す。"
    ),
    success_criteria=[
        "ユーザーの意図を正確に把握する",
        "実行結果を主管向けに簡潔に要約する",
    ],
    prohibitions=[
        "話題を勝手に変えない",
        "不明な操作を確認なしに実行しない",
    ],
    output_principles=[
        "サマリーは簡潔に（1-3行）",
        "要対応項目は明確に区別する",
    ],
)

# ---------------------------------------------------------------------------
# L2: Specialist ごとの TaskSystem
# ---------------------------------------------------------------------------

COORDINATOR_TASK = TaskSystemLayer(
    goal="ユーザーメッセージの意図を分類し、適切なspecialistにルーティングする",
    deliverables=["意図分類結果", "specialist指定"],
)

FILE_ORGANIZER_TASK = TaskSystemLayer(
    goal="ファイルの整理・分類・リネームを実行する",
    constraints=["削除操作は確認後のみ実行"],
)

MEETING_TASK = TaskSystemLayer(
    goal="会議スケジュールの管理・リマインダー設定を行う",
    deliverables=["スケジュール更新結果"],
)

BUSINESS_ADVISOR_TASK = TaskSystemLayer(
    goal="ビジネスに関する助言・分析を提供する",
    constraints=["具体的な数値・根拠を示す"],
)

FLIGHT_WATCH_TASK = TaskSystemLayer(
    goal="フライト情報の監視・通知を行う",
    deliverables=["フライトステータス"],
)
