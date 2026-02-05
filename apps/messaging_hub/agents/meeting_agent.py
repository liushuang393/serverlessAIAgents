"""会議エージェント.

会議の準備、議事録生成、フォローアップスケジュールを担当。

使用例:
    >>> agent = MeetingAgent(calendar_skill)
    >>> brief = await agent.prepare_meeting_brief(event_id)
    >>> notes = await agent.generate_meeting_notes(transcript)
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import TYPE_CHECKING, Any

from agentflow.providers import get_llm


if TYPE_CHECKING:
    from agentflow.skills import CalendarEvent, CalendarSkill


@dataclass
class MeetingBrief:
    """会議ブリーフ.

    Attributes:
        event_id: イベントID
        title: 会議タイトル
        attendees: 参加者リスト
        agenda: アジェンダ
        background: 背景情報
        discussion_points: 議論ポイント
        prepared_questions: 準備された質問
        related_documents: 関連ドキュメント
    """

    event_id: str
    title: str
    attendees: list[str] = field(default_factory=list)
    agenda: list[str] = field(default_factory=list)
    background: str = ""
    discussion_points: list[str] = field(default_factory=list)
    prepared_questions: list[str] = field(default_factory=list)
    related_documents: list[str] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "event_id": self.event_id,
            "title": self.title,
            "attendees": self.attendees,
            "agenda": self.agenda,
            "background": self.background,
            "discussion_points": self.discussion_points,
            "prepared_questions": self.prepared_questions,
            "related_documents": self.related_documents,
            "created_at": self.created_at.isoformat(),
        }


@dataclass
class MeetingNotes:
    """議事録.

    Attributes:
        event_id: イベントID
        title: 会議タイトル
        date: 会議日時
        attendees: 参加者リスト
        summary: 要約
        key_discussions: 主要議論
        decisions: 決定事項
        action_items: アクションアイテム
        next_steps: 次のステップ
        raw_transcript: 元のトランスクリプト
    """

    event_id: str
    title: str
    date: datetime
    attendees: list[str] = field(default_factory=list)
    summary: str = ""
    key_discussions: list[dict[str, Any]] = field(default_factory=list)
    decisions: list[str] = field(default_factory=list)
    action_items: list[dict[str, Any]] = field(default_factory=list)
    next_steps: list[str] = field(default_factory=list)
    raw_transcript: str = ""
    created_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "event_id": self.event_id,
            "title": self.title,
            "date": self.date.isoformat(),
            "attendees": self.attendees,
            "summary": self.summary,
            "key_discussions": self.key_discussions,
            "decisions": self.decisions,
            "action_items": self.action_items,
            "next_steps": self.next_steps,
            "created_at": self.created_at.isoformat(),
        }

    def to_markdown(self) -> str:
        """Markdown形式に変換."""
        lines = [
            f"# {self.title}",
            "",
            f"**日時**: {self.date.strftime('%Y-%m-%d %H:%M')}",
            f"**参加者**: {', '.join(self.attendees)}",
            "",
            "## 要約",
            self.summary,
            "",
        ]

        if self.key_discussions:
            lines.append("## 主要議論")
            for i, disc in enumerate(self.key_discussions, 1):
                lines.append(f"### {i}. {disc.get('topic', '')}")
                lines.append(disc.get("summary", ""))
                lines.append("")

        if self.decisions:
            lines.append("## 決定事項")
            for dec in self.decisions:
                lines.append(f"- {dec}")
            lines.append("")

        if self.action_items:
            lines.append("## アクションアイテム")
            lines.append("| 担当者 | タスク | 期限 |")
            lines.append("|--------|--------|------|")
            for item in self.action_items:
                assignee = item.get("assignee", "未定")
                task = item.get("task", "")
                due = item.get("due_date", "未定")
                lines.append(f"| {assignee} | {task} | {due} |")
            lines.append("")

        if self.next_steps:
            lines.append("## 次のステップ")
            for step in self.next_steps:
                lines.append(f"- {step}")

        return "\n".join(lines)


class MeetingAgent:
    """会議エージェント.

    会議の準備、議事録生成、フォローアップを担当。
    """

    def __init__(
        self,
        calendar_skill: CalendarSkill | None = None,
    ) -> None:
        """初期化.

        Args:
            calendar_skill: カレンダースキル
        """
        self._calendar = calendar_skill
        self._logger = logging.getLogger(__name__)

    async def prepare_meeting_brief(
        self,
        event_id: str | None = None,
        event: CalendarEvent | None = None,
        context: str | None = None,
    ) -> MeetingBrief:
        """会議ブリーフを準備.

        Args:
            event_id: イベントID
            event: イベント（直接指定）
            context: 追加コンテキスト

        Returns:
            会議ブリーフ
        """
        # イベント情報取得
        if event is None and self._calendar and event_id:
            # カレンダーからイベントを取得
            # TODO: get_event_by_id の実装が必要
            pass

        if event is None:
            # ダミーイベントを作成
            event_data = {
                "id": event_id or "unknown",
                "title": "会議",
                "start": datetime.now(),
                "end": datetime.now() + timedelta(hours=1),
            }
        else:
            event_data = event.to_dict() if hasattr(event, "to_dict") else {}

        # LLMでブリーフを生成
        llm = get_llm(temperature=0.5)

        system_prompt = """あなたは優秀な会議準備アシスタントです。
会議の情報に基づいて、参加者が効果的に準備できるブリーフを作成してください。

以下の項目を含めてください：
1. アジェンダ（推測される議題）
2. 背景情報
3. 議論ポイント
4. 準備すべき質問

JSON形式で出力してください：
{
    "agenda": ["議題1", "議題2"],
    "background": "背景説明",
    "discussion_points": ["ポイント1", "ポイント2"],
    "prepared_questions": ["質問1", "質問2"]
}"""

        user_prompt = f"""会議情報:
タイトル: {event_data.get("title", "会議")}
参加者: {", ".join(event_data.get("attendees", []))}
説明: {event_data.get("description", "")}
{f"追加コンテキスト: {context}" if context else ""}

上記の会議に向けたブリーフを作成してください。"""

        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ]
        )

        content = response.get("content", "")

        # JSONをパース
        import json

        try:
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            data = json.loads(content.strip())
        except json.JSONDecodeError:
            data = {}

        brief = MeetingBrief(
            event_id=event_data.get("id", "unknown"),
            title=event_data.get("title", "会議"),
            attendees=event_data.get("attendees", []),
            agenda=data.get("agenda", []),
            background=data.get("background", ""),
            discussion_points=data.get("discussion_points", []),
            prepared_questions=data.get("prepared_questions", []),
        )

        self._logger.info("会議ブリーフ作成: title=%s", brief.title)

        return brief

    async def generate_meeting_notes(
        self,
        transcript: str,
        event: CalendarEvent | None = None,
        title: str = "会議",
        attendees: list[str] | None = None,
    ) -> MeetingNotes:
        """議事録を生成.

        Args:
            transcript: 会議のトランスクリプト
            event: 関連イベント
            title: 会議タイトル
            attendees: 参加者リスト

        Returns:
            議事録
        """
        event_id = event.id if event else "unknown"
        event_title = event.title if event else title
        event_date = event.start if event else datetime.now()
        event_attendees = event.attendees if event else (attendees or [])

        # LLMで議事録を生成
        llm = get_llm(temperature=0.3)

        system_prompt = """あなたは優秀な議事録作成者です。
会議のトランスクリプトから構造化された議事録を作成してください。

以下の項目を含めてください：
1. 要約（2-3文）
2. 主要議論（トピックごと）
3. 決定事項
4. アクションアイテム（担当者、タスク、期限）
5. 次のステップ

JSON形式で出力してください：
{
    "summary": "会議の要約",
    "key_discussions": [
        {"topic": "議題", "summary": "議論内容"}
    ],
    "decisions": ["決定事項1", "決定事項2"],
    "action_items": [
        {"assignee": "担当者", "task": "タスク内容", "due_date": "期限"}
    ],
    "next_steps": ["次のステップ1"]
}"""

        user_prompt = f"""会議: {event_title}
参加者: {", ".join(event_attendees) if event_attendees else "不明"}

トランスクリプト:
{transcript}

上記から議事録を作成してください。"""

        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ]
        )

        content = response.get("content", "")

        # JSONをパース
        import json

        try:
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            data = json.loads(content.strip())
        except json.JSONDecodeError:
            data = {
                "summary": content,
                "key_discussions": [],
                "decisions": [],
                "action_items": [],
                "next_steps": [],
            }

        notes = MeetingNotes(
            event_id=event_id,
            title=event_title,
            date=event_date,
            attendees=event_attendees,
            summary=data.get("summary", ""),
            key_discussions=data.get("key_discussions", []),
            decisions=data.get("decisions", []),
            action_items=data.get("action_items", []),
            next_steps=data.get("next_steps", []),
            raw_transcript=transcript,
        )

        self._logger.info("議事録生成: title=%s, actions=%d", notes.title, len(notes.action_items))

        return notes

    async def schedule_follow_up(
        self,
        meeting_notes: MeetingNotes,
        days_later: int = 7,
    ) -> CalendarEvent | None:
        """フォローアップ会議をスケジュール.

        Args:
            meeting_notes: 議事録
            days_later: 何日後にスケジュールするか

        Returns:
            作成されたイベント
        """
        if not self._calendar:
            self._logger.warning("カレンダースキルが設定されていません")
            return None

        # 空き時間を検索
        from_date = datetime.now() + timedelta(days=days_later)
        to_date = from_date + timedelta(days=7)

        free_slots = await self._calendar.find_free_slots(
            duration_minutes=60,
            start=from_date,
            end=to_date,
            attendees=meeting_notes.attendees,
        )

        if not free_slots:
            self._logger.warning("空き時間が見つかりません")
            return None

        # 最初の空き時間でイベント作成
        slot = free_slots[0]

        # フォローアップ内容を生成
        description_lines = [
            f"前回の会議「{meeting_notes.title}」のフォローアップ",
            "",
            "## 前回の決定事項",
        ]
        for dec in meeting_notes.decisions:
            description_lines.append(f"- {dec}")

        description_lines.append("")
        description_lines.append("## 確認事項（アクションアイテム）")
        for item in meeting_notes.action_items:
            description_lines.append(f"- {item.get('assignee', '未定')}: {item.get('task', '')}")

        event = await self._calendar.create_event(
            title=f"フォローアップ: {meeting_notes.title}",
            start=slot.start,
            end=slot.end,
            description="\n".join(description_lines),
            attendees=meeting_notes.attendees,
            reminder_minutes=30,
        )

        self._logger.info(
            "フォローアップ会議スケジュール: title=%s, date=%s",
            event.title,
            event.start.isoformat(),
        )

        return event

    async def extract_action_items(
        self,
        text: str,
    ) -> list[dict[str, Any]]:
        """テキストからアクションアイテムを抽出.

        Args:
            text: 入力テキスト

        Returns:
            アクションアイテムリスト
        """
        llm = get_llm(temperature=0.3)

        system_prompt = """テキストからアクションアイテム（タスク、担当者、期限）を抽出してください。

JSON配列で出力してください：
[
    {"task": "タスク内容", "assignee": "担当者名", "due_date": "期限"}
]

担当者や期限が不明な場合は「未定」としてください。"""

        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": text},
            ]
        )

        content = response.get("content", "")

        import json

        try:
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            items = json.loads(content.strip())
            return items if isinstance(items, list) else []
        except json.JSONDecodeError:
            return []

    async def summarize_meeting_history(
        self,
        event_ids: list[str] | None = None,
        days: int = 30,
    ) -> dict[str, Any]:
        """会議履歴を要約.

        Args:
            event_ids: 対象イベントIDリスト
            days: 過去何日間

        Returns:
            要約結果
        """
        # TODO: 過去の会議を取得して傾向を分析
        return {
            "period_days": days,
            "total_meetings": 0,
            "total_hours": 0,
            "top_topics": [],
            "pending_action_items": [],
        }
