"""Structured constraints system for Agent OS.

Constraints define what an agent can do (permissions),
must do (goals), must not do (prohibitions), and
invariants that must always hold.
"""

import uuid
from enum import Enum

from pydantic import BaseModel, Field


class ConstraintType(str, Enum):
    """Types of constraints.

    These represent different categories of constraints
    that can be applied to agent behavior.
    """

    PERMISSION = "permission"
    """What the agent is allowed to do."""

    GOAL = "goal"
    """What the agent should try to achieve."""

    PROHIBITION = "prohibition"
    """What the agent must never do."""

    INVARIANT = "invariant"
    """Conditions that must always hold."""

    PREFERENCE = "preference"
    """Soft constraints expressing preferences."""


class StructuredConstraint(BaseModel):
    """A single constraint on agent behavior.

    Constraints are rules that govern what an agent can
    and cannot do. They can be hard (must be followed)
    or soft (preferences).

    Attributes:
        constraint_id: Unique identifier for this constraint
        constraint_type: Type of constraint
        name: Short name for the constraint
        description: Human-readable description
        is_hard: Whether this is a hard constraint (must be followed)
        source: Where this constraint came from
    """

    constraint_id: str = Field(default_factory=lambda: f"const-{uuid.uuid4().hex[:8]}")
    constraint_type: ConstraintType
    name: str
    description: str
    is_hard: bool = True
    source: str = "system"

    model_config = {"frozen": False}


class StructuredConstraints(BaseModel):
    """Collection of structured constraints.

    Organizes constraints by type for easy access and
    prompt generation.

    Attributes:
        permissions: What the agent is allowed to do
        goals: What the agent should achieve
        prohibitions: What the agent must never do
        invariants: Conditions that must always hold
        preferences: Soft constraints expressing preferences
    """

    permissions: list[StructuredConstraint] = Field(default_factory=list)
    goals: list[StructuredConstraint] = Field(default_factory=list)
    prohibitions: list[StructuredConstraint] = Field(default_factory=list)
    invariants: list[StructuredConstraint] = Field(default_factory=list)
    preferences: list[StructuredConstraint] = Field(default_factory=list)

    model_config = {"frozen": False}

    def to_prompt_section(self) -> str:
        """Convert constraints to a prompt section.

        Generates a human-readable section that can be
        included in LLM prompts to communicate constraints.

        Returns:
            Formatted string with constraint sections
        """
        sections = []

        if self.prohibitions:
            sections.append("### NEVER DO (Hard Prohibitions)")
            for c in self.prohibitions:
                sections.append(f"- {c.description}")

        if self.permissions:
            sections.append("### ALLOWED ACTIONS")
            for c in self.permissions:
                sections.append(f"- {c.description}")

        if self.goals:
            sections.append("### GOALS")
            for c in self.goals:
                sections.append(f"- {c.description}")

        if self.invariants:
            sections.append("### INVARIANTS (Must Always Hold)")
            for c in self.invariants:
                sections.append(f"- {c.description}")

        if self.preferences:
            sections.append("### PREFERENCES (Soft Constraints)")
            for c in self.preferences:
                sections.append(f"- {c.description}")

        return "\n".join(sections)

    def add_permission(self, name: str, description: str) -> None:
        """Add a permission constraint.

        Args:
            name: Short name for the permission
            description: What is permitted
        """
        self.permissions.append(
            StructuredConstraint(
                constraint_type=ConstraintType.PERMISSION,
                name=name,
                description=description,
            )
        )

    def add_prohibition(self, name: str, description: str, is_hard: bool = True) -> None:
        """Add a prohibition constraint.

        Args:
            name: Short name for the prohibition
            description: What is prohibited
            is_hard: Whether this is a hard constraint
        """
        self.prohibitions.append(
            StructuredConstraint(
                constraint_type=ConstraintType.PROHIBITION,
                name=name,
                description=description,
                is_hard=is_hard,
            )
        )

    def add_goal(self, name: str, description: str) -> None:
        """Add a goal constraint.

        Args:
            name: Short name for the goal
            description: What should be achieved
        """
        self.goals.append(
            StructuredConstraint(
                constraint_type=ConstraintType.GOAL,
                name=name,
                description=description,
            )
        )

    def get_hard_prohibitions(self) -> list[StructuredConstraint]:
        """Get all hard prohibition constraints.

        Returns:
            List of hard prohibition constraints
        """
        return [p for p in self.prohibitions if p.is_hard]
