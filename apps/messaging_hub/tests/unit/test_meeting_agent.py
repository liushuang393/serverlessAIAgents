"""Unit tests for MeetingAgent."""

from datetime import datetime

from apps.messaging_hub.agents.meeting_agent import (
    MeetingAgent,
    MeetingBrief,
    MeetingNotes,
)


class TestMeetingBrief:
    """Test cases for MeetingBrief dataclass."""

    def test_meeting_brief_creation(self) -> None:
        """Test creating a MeetingBrief instance."""
        brief = MeetingBrief(
            event_id="event-123",
            title="Weekly Standup",
            attendees=["Alice", "Bob"],
            agenda=["Review tasks", "Plan sprint"],
        )
        assert brief.event_id == "event-123"
        assert brief.title == "Weekly Standup"
        assert len(brief.attendees) == 2
        assert len(brief.agenda) == 2

    def test_meeting_brief_to_dict(self) -> None:
        """Test MeetingBrief.to_dict method."""
        brief = MeetingBrief(
            event_id="event-123",
            title="Test Meeting",
        )
        data = brief.to_dict()

        assert data["event_id"] == "event-123"
        assert data["title"] == "Test Meeting"
        assert "created_at" in data


class TestMeetingNotes:
    """Test cases for MeetingNotes dataclass."""

    def test_meeting_notes_creation(self) -> None:
        """Test creating a MeetingNotes instance."""
        notes = MeetingNotes(
            event_id="event-456",
            title="Project Review",
            date=datetime(2024, 1, 15, 10, 0),
            attendees=["Alice", "Bob", "Charlie"],
            summary="Discussed project progress",
            decisions=["Approve budget"],
        )
        assert notes.event_id == "event-456"
        assert notes.title == "Project Review"
        assert len(notes.attendees) == 3
        assert len(notes.decisions) == 1

    def test_meeting_notes_to_dict(self) -> None:
        """Test MeetingNotes.to_dict method."""
        notes = MeetingNotes(
            event_id="event-456",
            title="Test Meeting",
            date=datetime(2024, 1, 15, 10, 0),
        )
        data = notes.to_dict()

        assert data["event_id"] == "event-456"
        assert data["title"] == "Test Meeting"
        assert "date" in data
        assert "created_at" in data

    def test_meeting_notes_to_markdown(self) -> None:
        """Test MeetingNotes.to_markdown method."""
        notes = MeetingNotes(
            event_id="event-789",
            title="Sprint Planning",
            date=datetime(2024, 1, 20, 14, 30),
            attendees=["Alice", "Bob"],
            summary="Planned next sprint tasks",
            decisions=["Start feature X"],
            action_items=[{"assignee": "Alice", "task": "Design review", "due_date": "2024-01-25"}],
        )
        markdown = notes.to_markdown()

        assert "# Sprint Planning" in markdown
        assert "Alice" in markdown
        assert "Bob" in markdown
        assert "Planned next sprint tasks" in markdown
        assert "Start feature X" in markdown
        assert "Design review" in markdown


class TestMeetingAgentInit:
    """Test cases for MeetingAgent initialization."""

    def test_agent_creation_without_calendar(self) -> None:
        """Test creating MeetingAgent without calendar skill."""
        agent = MeetingAgent()
        assert agent._calendar is None

    def test_agent_creation_with_calendar(self) -> None:
        """Test creating MeetingAgent with calendar skill."""

        # Mock calendar skill
        class MockCalendarSkill:
            pass

        mock_calendar = MockCalendarSkill()
        agent = MeetingAgent(calendar_skill=mock_calendar)
        assert agent._calendar is mock_calendar
