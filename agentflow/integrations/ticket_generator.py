"""工単生成器（Ticket Generator）.

FAQ システムで回答できない質問を自動的に工単化するモジュール。
Jira/ServiceNow/Redmine 等の外部システムと連携。

設計原則:
- 不確定時の優雅な降格
- 自動工単生成による人工フォローアップ
- 複数チケットシステム対応

使用例:
    >>> from agentflow.integrations.ticket_generator import TicketGenerator
    >>>
    >>> generator = TicketGenerator(
    ...     provider="jira",
    ...     config=JiraConfig(project_key="FAQ"),
    ... )
    >>>
    >>> ticket = await generator.create_from_question(
    ...     question="来年の昇給率は？",
    ...     context={"user_id": "123", "department": "営業"},
    ...     reason="回答の信頼度が低い",
    ... )
    >>> print(ticket.ticket_id)  # FAQ-123
"""

from __future__ import annotations

import logging
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class TicketPriority(str, Enum):
    """工単優先度."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"
    URGENT = "urgent"


class TicketStatus(str, Enum):
    """工単ステータス."""

    OPEN = "open"
    IN_PROGRESS = "in_progress"
    PENDING = "pending"
    RESOLVED = "resolved"
    CLOSED = "closed"


class TicketType(str, Enum):
    """工単タイプ."""

    INQUIRY = "inquiry"  # 問合せ
    KNOWLEDGE_GAP = "knowledge_gap"  # 知識ギャップ
    FEEDBACK = "feedback"  # フィードバック
    BUG = "bug"  # バグ報告
    IMPROVEMENT = "improvement"  # 改善提案


@dataclass
class Ticket:
    """工単.

    Attributes:
        ticket_id: 工単ID
        title: タイトル
        description: 説明
        ticket_type: 工単タイプ
        priority: 優先度
        status: ステータス
        assignee: 担当者
        reporter: 報告者
        labels: ラベル
        created_at: 作成日時
        metadata: メタデータ
    """

    ticket_id: str
    title: str
    description: str
    ticket_type: TicketType = TicketType.INQUIRY
    priority: TicketPriority = TicketPriority.MEDIUM
    status: TicketStatus = TicketStatus.OPEN
    assignee: str = ""
    reporter: str = ""
    labels: list[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    due_date: datetime | None = None
    original_question: str = ""
    ai_context: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)
    external_id: str = ""  # 外部システムのID
    external_url: str = ""  # 外部システムのURL

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "ticket_id": self.ticket_id,
            "title": self.title,
            "description": self.description,
            "ticket_type": self.ticket_type.value,
            "priority": self.priority.value,
            "status": self.status.value,
            "assignee": self.assignee,
            "reporter": self.reporter,
            "labels": self.labels,
            "created_at": self.created_at.isoformat(),
            "external_id": self.external_id,
            "external_url": self.external_url,
        }


@dataclass
class TicketGeneratorConfig:
    """工単生成器設定."""

    # デフォルト設定
    default_priority: TicketPriority = TicketPriority.MEDIUM
    default_ticket_type: TicketType = TicketType.INQUIRY
    default_labels: list[str] = field(default_factory=lambda: ["faq-system", "auto-generated"])

    # 自動割り当て
    auto_assign: bool = True
    assignment_rules: dict[str, str] = field(default_factory=dict)

    # テンプレート
    title_template: str = "[FAQ問合せ] {question_summary}"
    description_template: str = """## 問合せ内容

{question}

## 発生経緯

- **ユーザー**: {user_id}
- **部門**: {department}
- **生成理由**: {reason}
- **信頼度**: {confidence}

## AI回答（参考）

{partial_answer}

## 関連情報

{context}
"""

    # SLA設定
    sla_hours: dict[TicketPriority, int] = field(
        default_factory=lambda: {
            TicketPriority.LOW: 72,
            TicketPriority.MEDIUM: 24,
            TicketPriority.HIGH: 8,
            TicketPriority.URGENT: 2,
        }
    )


class TicketProviderBase(ABC):
    """工単プロバイダー基底クラス."""

    @abstractmethod
    async def create_ticket(self, ticket: Ticket) -> Ticket:
        """工単を作成.

        Args:
            ticket: 工単

        Returns:
            作成された工単（外部IDを含む）
        """

    @abstractmethod
    async def update_ticket(self, ticket_id: str, updates: dict[str, Any]) -> Ticket:
        """工単を更新.

        Args:
            ticket_id: 工単ID
            updates: 更新内容

        Returns:
            更新された工単
        """

    @abstractmethod
    async def get_ticket(self, ticket_id: str) -> Ticket | None:
        """工単を取得.

        Args:
            ticket_id: 工単ID

        Returns:
            工単、または None
        """

    @abstractmethod
    async def search_tickets(self, query: str, **kwargs: Any) -> list[Ticket]:
        """工単を検索.

        Args:
            query: 検索クエリ
            **kwargs: 追加パラメータ

        Returns:
            工単リスト
        """


class InMemoryTicketProvider(TicketProviderBase):
    """インメモリ工単プロバイダー（テスト用）."""

    def __init__(self) -> None:
        """初期化."""
        self._tickets: dict[str, Ticket] = {}
        self._counter = 0

    async def create_ticket(self, ticket: Ticket) -> Ticket:
        """工単を作成."""
        self._counter += 1
        ticket.external_id = f"TKT-{self._counter:06d}"
        ticket.external_url = f"http://tickets.example.com/{ticket.external_id}"
        self._tickets[ticket.ticket_id] = ticket
        return ticket

    async def update_ticket(self, ticket_id: str, updates: dict[str, Any]) -> Ticket:
        """工単を更新."""
        ticket = self._tickets.get(ticket_id)
        if not ticket:
            msg = f"Ticket not found: {ticket_id}"
            raise ValueError(msg)

        for key, value in updates.items():
            if hasattr(ticket, key):
                setattr(ticket, key, value)

        ticket.updated_at = datetime.now()
        return ticket

    async def get_ticket(self, ticket_id: str) -> Ticket | None:
        """工単を取得."""
        return self._tickets.get(ticket_id)

    async def search_tickets(self, query: str, **kwargs: Any) -> list[Ticket]:
        """工単を検索."""
        results = []
        query_lower = query.lower()

        for ticket in self._tickets.values():
            if query_lower in ticket.title.lower() or query_lower in ticket.description.lower():
                results.append(ticket)

        return results


class JiraTicketProvider(TicketProviderBase):
    """Jira 工単プロバイダー."""

    def __init__(
        self,
        server_url: str,
        username: str,
        api_token: str,
        project_key: str,
    ) -> None:
        """初期化.

        Args:
            server_url: Jira サーバーURL
            username: ユーザー名
            api_token: API トークン
            project_key: プロジェクトキー
        """
        self._server_url = server_url
        self._username = username
        self._api_token = api_token
        self._project_key = project_key
        self._logger = logging.getLogger(__name__)

    async def create_ticket(self, ticket: Ticket) -> Ticket:
        """工単を作成."""
        # Jira API を呼び出す実装
        # 実際の実装では jira-python ライブラリを使用
        self._logger.info(
            "Creating Jira ticket: %s in project %s",
            ticket.title,
            self._project_key,
        )

        # プレースホルダー実装
        ticket.external_id = f"{self._project_key}-{uuid.uuid4().hex[:6].upper()}"
        ticket.external_url = f"{self._server_url}/browse/{ticket.external_id}"

        return ticket

    async def update_ticket(self, ticket_id: str, updates: dict[str, Any]) -> Ticket:
        """工単を更新."""
        self._logger.info("Updating Jira ticket: %s", ticket_id)
        # 実装は省略
        msg = "Jira update not implemented"
        raise NotImplementedError(msg)

    async def get_ticket(self, ticket_id: str) -> Ticket | None:
        """工単を取得."""
        self._logger.info("Getting Jira ticket: %s", ticket_id)
        # 実装は省略
        return None

    async def search_tickets(self, query: str, **kwargs: Any) -> list[Ticket]:
        """工単を検索."""
        self._logger.info("Searching Jira tickets: %s", query)
        # 実装は省略
        return []


class ServiceNowTicketProvider(TicketProviderBase):
    """ServiceNow 工単プロバイダー."""

    def __init__(
        self,
        instance_url: str,
        username: str,
        password: str,
        table_name: str = "incident",
    ) -> None:
        """初期化."""
        self._instance_url = instance_url
        self._username = username
        self._password = password
        self._table_name = table_name
        self._logger = logging.getLogger(__name__)

    async def create_ticket(self, ticket: Ticket) -> Ticket:
        """工単を作成."""
        self._logger.info("Creating ServiceNow incident: %s", ticket.title)

        # プレースホルダー実装
        ticket.external_id = f"INC{uuid.uuid4().hex[:10].upper()}"
        ticket.external_url = f"{self._instance_url}/nav_to.do?uri=incident.do?sys_id={ticket.external_id}"

        return ticket

    async def update_ticket(self, ticket_id: str, updates: dict[str, Any]) -> Ticket:
        """工単を更新."""
        msg = "ServiceNow update not implemented"
        raise NotImplementedError(msg)

    async def get_ticket(self, ticket_id: str) -> Ticket | None:
        """工単を取得."""
        return None

    async def search_tickets(self, query: str, **kwargs: Any) -> list[Ticket]:
        """工単を検索."""
        return []


class TicketGenerator:
    """工単生成器.

    FAQ システムで回答できない質問を自動的に工単化。

    Example:
        >>> generator = TicketGenerator()
        >>>
        >>> ticket = await generator.create_from_question(
        ...     question="来年の昇給率は？",
        ...     context={"user_id": "123"},
        ...     reason="信頼度が低い",
        ... )
    """

    def __init__(
        self,
        config: TicketGeneratorConfig | None = None,
        provider: TicketProviderBase | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
            provider: 工単プロバイダー
        """
        self._config = config or TicketGeneratorConfig()
        self._provider = provider or InMemoryTicketProvider()
        self._logger = logging.getLogger(__name__)

    async def create_from_question(
        self,
        question: str,
        context: dict[str, Any] | None = None,
        reason: str = "",
        partial_answer: str = "",
        confidence: float = 0.0,
        priority: TicketPriority | None = None,
    ) -> Ticket:
        """質問から工単を作成.

        Args:
            question: 質問
            context: コンテキスト
            reason: 生成理由
            partial_answer: 部分回答
            confidence: 信頼度
            priority: 優先度

        Returns:
            作成された工単
        """
        context = context or {}

        # タイトルを生成
        question_summary = question[:50] + "..." if len(question) > 50 else question
        title = self._config.title_template.format(
            question_summary=question_summary,
        )

        # 説明を生成
        description = self._config.description_template.format(
            question=question,
            user_id=context.get("user_id", "不明"),
            department=context.get("department", "不明"),
            reason=reason or "回答の信頼度が低い",
            confidence=f"{confidence:.0%}",
            partial_answer=partial_answer or "なし",
            context=self._format_context(context),
        )

        # 優先度を決定
        if priority is None:
            priority = self._determine_priority(question, context, confidence)

        # 担当者を決定
        assignee = ""
        if self._config.auto_assign:
            assignee = self._determine_assignee(question, context)

        # 工単を作成
        ticket_id = f"FAQ-{datetime.now().strftime('%Y%m%d')}-{uuid.uuid4().hex[:6].upper()}"

        ticket = Ticket(
            ticket_id=ticket_id,
            title=title,
            description=description,
            ticket_type=self._config.default_ticket_type,
            priority=priority,
            status=TicketStatus.OPEN,
            assignee=assignee,
            reporter=context.get("user_id", ""),
            labels=self._config.default_labels.copy(),
            original_question=question,
            ai_context={
                "confidence": confidence,
                "partial_answer": partial_answer,
                "reason": reason,
            },
            metadata=context,
        )

        # SLA に基づく期限を設定
        sla_hours = self._config.sla_hours.get(priority, 24)
        ticket.due_date = datetime.now() + timedelta(hours=sla_hours)

        # プロバイダーで作成
        created_ticket = await self._provider.create_ticket(ticket)

        self._logger.info(
            "Created ticket: %s (external: %s)",
            created_ticket.ticket_id,
            created_ticket.external_id,
        )

        return created_ticket

    async def create_knowledge_gap_ticket(
        self,
        topic: str,
        query_count: int,
        hit_rate: float,
        sample_questions: list[str],
    ) -> Ticket:
        """知識ギャップ工単を作成.

        Args:
            topic: トピック
            query_count: 照会数
            hit_rate: 命中率
            sample_questions: サンプル質問

        Returns:
            作成された工単
        """
        title = f"[知識ギャップ] {topic}"

        description = f"""## 知識ギャップ検出

### 概要
- **トピック**: {topic}
- **照会数**: {query_count}件（過去30日）
- **命中率**: {hit_rate:.0%}

### サンプル質問
{chr(10).join(f"- {q}" for q in sample_questions[:5])}

### 対応依頼
このトピックに関するドキュメントの追加・更新をお願いします。
"""

        ticket_id = f"GAP-{datetime.now().strftime('%Y%m%d')}-{uuid.uuid4().hex[:6].upper()}"

        ticket = Ticket(
            ticket_id=ticket_id,
            title=title,
            description=description,
            ticket_type=TicketType.KNOWLEDGE_GAP,
            priority=TicketPriority.MEDIUM if hit_rate < 0.5 else TicketPriority.LOW,
            labels=["knowledge-gap", "auto-detected"],
            metadata={
                "topic": topic,
                "query_count": query_count,
                "hit_rate": hit_rate,
            },
        )

        return await self._provider.create_ticket(ticket)

    async def update_ticket_status(
        self,
        ticket_id: str,
        status: TicketStatus,
        comment: str = "",
    ) -> Ticket:
        """工単ステータスを更新.

        Args:
            ticket_id: 工単ID
            status: 新ステータス
            comment: コメント

        Returns:
            更新された工単
        """
        updates: dict[str, Any] = {"status": status}
        if comment:
            updates["metadata"] = {"last_comment": comment}

        return await self._provider.update_ticket(ticket_id, updates)

    async def get_ticket(self, ticket_id: str) -> Ticket | None:
        """工単を取得.

        Args:
            ticket_id: 工単ID

        Returns:
            工単、または None
        """
        return await self._provider.get_ticket(ticket_id)

    async def search_tickets(
        self,
        query: str | None = None,
        status: TicketStatus | None = None,
        priority: TicketPriority | None = None,
        reporter: str | None = None,
    ) -> list[Ticket]:
        """工単を検索.

        Args:
            query: 検索クエリ
            status: ステータス
            priority: 優先度
            reporter: 報告者

        Returns:
            工単リスト
        """
        results = await self._provider.search_tickets(query or "")

        # フィルタリング
        if status:
            results = [t for t in results if t.status == status]
        if priority:
            results = [t for t in results if t.priority == priority]
        if reporter:
            results = [t for t in results if t.reporter == reporter]

        return results

    def _determine_priority(
        self,
        question: str,
        context: dict[str, Any],
        confidence: float,
    ) -> TicketPriority:
        """優先度を決定.

        Args:
            question: 質問
            context: コンテキスト
            confidence: 信頼度

        Returns:
            優先度
        """
        # 緊急キーワード
        urgent_keywords = ["緊急", "至急", "本日中", "今すぐ", "urgent"]
        if any(k in question.lower() for k in urgent_keywords):
            return TicketPriority.URGENT

        # 信頼度が非常に低い場合
        if confidence < 0.3:
            return TicketPriority.HIGH

        # VIPユーザー
        if context.get("role") in ["manager", "executive"]:
            return TicketPriority.HIGH

        return self._config.default_priority

    def _determine_assignee(
        self,
        question: str,
        context: dict[str, Any],
    ) -> str:
        """担当者を決定.

        Args:
            question: 質問
            context: コンテキスト

        Returns:
            担当者ID
        """
        question_lower = question.lower()

        for keyword, assignee in self._config.assignment_rules.items():
            if keyword.lower() in question_lower:
                return assignee

        return ""

    def _format_context(self, context: dict[str, Any]) -> str:
        """コンテキストをフォーマット.

        Args:
            context: コンテキスト

        Returns:
            フォーマット済み文字列
        """
        lines = []
        for key, value in context.items():
            if key not in ["user_id", "department"]:
                lines.append(f"- **{key}**: {value}")

        return "\n".join(lines) if lines else "なし"


# timedelta import
from datetime import timedelta


__all__ = [
    "InMemoryTicketProvider",
    "JiraTicketProvider",
    "ServiceNowTicketProvider",
    "Ticket",
    "TicketGenerator",
    "TicketGeneratorConfig",
    "TicketPriority",
    "TicketProviderBase",
    "TicketStatus",
    "TicketType",
]
