from agentflow.reasoner.constraints import (
    ConstraintType,
    StructuredConstraint,
    StructuredConstraints,
)


def test_constraint_creation():
    constraint = StructuredConstraint(
        constraint_type=ConstraintType.PROHIBITION,
        name="no_delete",
        description="Cannot delete files",
        is_hard=True,
    )
    assert constraint.constraint_type == ConstraintType.PROHIBITION
    assert constraint.name == "no_delete"
    assert constraint.is_hard is True


def test_constraint_default_values():
    constraint = StructuredConstraint(
        constraint_type=ConstraintType.PERMISSION,
        name="can_read",
        description="Can read files",
    )
    assert constraint.constraint_id.startswith("const-")
    assert constraint.is_hard is True
    assert constraint.source == "system"


def test_constraint_types():
    assert ConstraintType.PERMISSION
    assert ConstraintType.GOAL
    assert ConstraintType.PROHIBITION
    assert ConstraintType.INVARIANT
    assert ConstraintType.PREFERENCE


def test_structured_constraints_to_prompt_section():
    constraints = StructuredConstraints(
        prohibitions=[
            StructuredConstraint(
                constraint_type=ConstraintType.PROHIBITION,
                name="no_delete",
                description="Cannot delete files",
                is_hard=True,
            )
        ]
    )

    prompt_section = constraints.to_prompt_section()
    assert "NEVER DO" in prompt_section
    assert "Cannot delete files" in prompt_section


def test_structured_constraints_with_permissions():
    constraints = StructuredConstraints(
        permissions=[
            StructuredConstraint(
                constraint_type=ConstraintType.PERMISSION,
                name="can_read",
                description="Can read any file",
            ),
            StructuredConstraint(
                constraint_type=ConstraintType.PERMISSION,
                name="can_write",
                description="Can write to temp directory",
            ),
        ]
    )

    prompt_section = constraints.to_prompt_section()
    assert "ALLOWED ACTIONS" in prompt_section
    assert "Can read any file" in prompt_section
    assert "Can write to temp directory" in prompt_section


def test_structured_constraints_with_goals():
    constraints = StructuredConstraints(
        goals=[
            StructuredConstraint(
                constraint_type=ConstraintType.GOAL,
                name="complete_task",
                description="Complete the assigned task efficiently",
            )
        ]
    )

    prompt_section = constraints.to_prompt_section()
    assert "GOALS" in prompt_section
    assert "Complete the assigned task efficiently" in prompt_section


def test_structured_constraints_combined():
    constraints = StructuredConstraints(
        permissions=[
            StructuredConstraint(
                constraint_type=ConstraintType.PERMISSION,
                name="can_search",
                description="Can search the web",
            )
        ],
        prohibitions=[
            StructuredConstraint(
                constraint_type=ConstraintType.PROHIBITION,
                name="no_external",
                description="Cannot access external APIs",
            )
        ],
        goals=[
            StructuredConstraint(
                constraint_type=ConstraintType.GOAL,
                name="help_user",
                description="Help the user achieve their goal",
            )
        ],
    )

    prompt_section = constraints.to_prompt_section()
    assert "NEVER DO" in prompt_section
    assert "ALLOWED ACTIONS" in prompt_section
    assert "GOALS" in prompt_section


def test_empty_constraints():
    constraints = StructuredConstraints()
    prompt_section = constraints.to_prompt_section()
    assert prompt_section == ""
