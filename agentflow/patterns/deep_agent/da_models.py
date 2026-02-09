"""DeepAgent データモデル.

このモジュールはDeepAgentCoordinatorで使用するデータモデルを定義します。

モジュール構成:
- da_models.py: データモデル（本ファイル）
- da_stores.py: ストレージ抽象
- da_compressor.py: コンテキスト圧縮
- da_pool.py: Agent池
- da_progress.py: 進捗管理
- da_evolver.py: 自己進化
- da_coordinator.py: メイン協調器
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class TaskStatus(str, Enum):
    """タスク状態."""

    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    BLOCKED = "blocked"


class AgentType(str, Enum):
    """通用Agent種別（5-6個の基本Agent）.

    DeepAgentsフレームワークで定義された基本的なAgent役割:
    - RESEARCH: 調査・検索を担当
    - ANALYSIS: 分析・推論を担当
    - PLANNING: 計画・設計を担当
    - EXECUTION: 実行・操作を担当
    - REVIEW: 審査・検証を担当
    - REPORT: 報告・総括を担当
    """

    RESEARCH = "research"
    ANALYSIS = "analysis"
    PLANNING = "planning"
    EXECUTION = "execution"
    REVIEW = "review"
    REPORT = "report"


class TodoItem(BaseModel):
    """計画項目（DeepAgentsのTodoList思想）.

    タスク分解後の各ステップを表現するデータモデル。
    依存関係管理と優先度に基づく実行順序制御を提供。

    Attributes:
        id: 一意識別子（自動生成）
        task: タスク説明
        agent_type: 実行Agent種別
        status: タスク状態
        priority: 優先度（0-10、大きいほど優先）
        dependencies: 依存タスクID
        tools: 使用ツール
        skills: 使用スキル
        metadata: 追加メタデータ
        result: 実行結果
        error: エラー情報
        retry_count: リトライ回数
    """

    model_config = {"arbitrary_types_allowed": True}

    id: str = Field(default_factory=lambda: f"todo-{uuid.uuid4().hex[:8]}")
    task: str = Field(default="", description="タスク説明")
    agent_type: str = Field(default=AgentType.EXECUTION.value, description="実行Agent種別")
    status: TaskStatus = Field(default=TaskStatus.PENDING, description="タスク状態")
    priority: int = Field(default=0, ge=0, le=10, description="優先度（大きいほど優先）")
    dependencies: list[str] = Field(default_factory=list, description="依存タスクID")
    tools: list[str] = Field(default_factory=list, description="使用ツール")
    skills: list[str] = Field(default_factory=list, description="使用スキル")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")
    result: dict[str, Any] | None = Field(default=None, description="実行結果")
    error: str | None = Field(default=None, description="エラー情報")
    retry_count: int = Field(default=0, ge=0, description="リトライ回数")
    created_at: datetime = Field(default_factory=datetime.now)
    completed_at: datetime | None = Field(default=None)

    def is_ready(self, completed_ids: set[str]) -> bool:
        """実行準備完了かどうか."""
        if self.status != TaskStatus.PENDING:
            return False
        return all(dep in completed_ids for dep in self.dependencies)

    def mark_completed(self) -> None:
        """完了マーク."""
        self.status = TaskStatus.COMPLETED
        self.completed_at = datetime.now()

    def mark_failed(self, error_msg: str) -> None:
        """失敗マーク."""
        self.status = TaskStatus.FAILED
        self.error = error_msg
        self.retry_count += 1


class CognitiveAnalysis(BaseModel):
    """認知分析結果.

    タスクの意図・複雑度を分析した結果を格納。
    LLMによる認知分析フェーズの出力。
    """

    intent: str = Field(default="", description="ユーザー意図")
    is_clear: bool = Field(default=False, description="意図が明確か")
    clarification_needed: list[str] = Field(default_factory=list, description="追加質問")
    complexity: str = Field(default="medium", description="複雑度: low/medium/high")
    domains: list[str] = Field(default_factory=list, description="関連領域")
    suggested_agents: list[str] = Field(default_factory=list, description="推薦Agent")


class QualityDimension(str, Enum):
    """品質評審次元.

    多次元品質評価の各軸を定義:
    - COMPLETENESS: タスク要件の充足度
    - ACCURACY: 結果の正確さ
    - CONSISTENCY: 全体の整合性
    - EFFICIENCY: リソース使用効率
    - CLARITY: 出力の理解しやすさ
    """

    COMPLETENESS = "completeness"
    ACCURACY = "accuracy"
    CONSISTENCY = "consistency"
    EFFICIENCY = "efficiency"
    CLARITY = "clarity"


class QualityReview(BaseModel):
    """品質評審結果.

    多次元の品質評価結果を格納。
    verdict で最終判定（pass/revise/reject）を示す。
    """

    is_acceptable: bool = Field(default=False, description="合格判定")
    score: float = Field(default=0.0, ge=0, le=100, description="総合スコア")
    issues: list[str] = Field(default_factory=list, description="問題点")
    suggestions: list[str] = Field(default_factory=list, description="改善提案")
    retry_tasks: list[str] = Field(default_factory=list, description="再実行タスクID")
    dimension_scores: dict[str, float] = Field(
        default_factory=dict,
        description="次元別スコア（completeness, accuracy, etc.）",
    )
    verdict: str = Field(default="pending", description="判定（pass/revise/reject/pending）")
    confidence: float = Field(default=0.5, ge=0, le=1, description="評価信頼度")


class EvolutionRecord(BaseModel):
    """進化記録.

    成功パターン学習やフィードバック処理の記録。
    """

    timestamp: datetime = Field(default_factory=datetime.now)
    event_type: str = Field(default="", description="success/feedback/education")
    pattern: str = Field(default="", description="学習パターン")
    confidence: float = Field(default=0.5, description="信頼度")
    applied: bool = Field(default=False, description="適用済みか")


class MessageType(str, Enum):
    """Agent間メッセージ種別."""

    RESULT = "result"
    REQUEST = "request"
    NOTIFY = "notify"
    ERROR = "error"
    SYSTEM = "system"


class AgentMessage(BaseModel):
    """Agent間通信メッセージ（標準化）.

    Agent間の統一通信プロトコル。
    """

    model_config = {"arbitrary_types_allowed": True}

    id: str = Field(default_factory=lambda: f"msg-{uuid.uuid4().hex[:8]}")
    from_agent: str = Field(..., description="送信Agent")
    to_agent: str = Field(default="*", description="受信Agent（*=ブロードキャスト）")
    msg_type: MessageType = Field(default=MessageType.NOTIFY)
    content: Any = Field(default=None, description="メッセージ内容（任意型）")
    timestamp: datetime = Field(default_factory=datetime.now)
    correlation_id: str | None = Field(default=None, description="関連メッセージID")


class ParallelGroup(BaseModel):
    """並行実行グループ.

    依存関係のないタスクをグループ化。
    """

    group_id: str = Field(default_factory=lambda: f"pg-{uuid.uuid4().hex[:8]}")
    todo_ids: list[str] = Field(default_factory=list)
    status: TaskStatus = Field(default=TaskStatus.PENDING)


class CompactionStrategy(str, Enum):
    """圧縮戦略."""

    SELECTIVE = "selective"
    SUMMARIZE = "summarize"
    HIERARCHICAL = "hierarchical"
    HYBRID = "hybrid"


@dataclass
class MemoryTier:
    """メモリ層定義（MemGPT/Letta風）."""

    name: str
    max_tokens: int
    priority: int = 0
    content: list[dict[str, Any]] = field(default_factory=list)

    def token_count(self) -> int:
        """概算トークン数を返す（1トークン≈4文字）."""
        total_chars = sum(len(str(item)) for item in self.content)
        return total_chars // 4


@dataclass
class CompactionResult:
    """圧縮結果.

    コンテキスト圧縮操作の結果を記録。
    圧縮率と保持された重要情報を追跡。
    """

    original_tokens: int
    compressed_tokens: int
    compression_ratio: float
    preserved_keys: list[str]
    summary: str | None = None


# =============================================================================
# エクスポート
# =============================================================================
__all__ = [
    "AgentMessage",
    "AgentType",
    "CognitiveAnalysis",
    "CompactionResult",
    "CompactionStrategy",
    "EvolutionRecord",
    "MemoryTier",
    "MessageType",
    "ParallelGroup",
    "QualityDimension",
    "QualityReview",
    "TaskStatus",
    "TodoItem",
]

