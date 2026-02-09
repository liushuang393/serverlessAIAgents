# -*- coding: utf-8 -*-
"""Unit tests for RLM workspace module."""

import pytest

from agentflow.context.rlm.workspace import (
    VariableType,
    Workspace,
    WorkspaceVariable,
)


class TestVariableType:
    """Tests for VariableType enum."""

    def test_high_priority_types(self) -> None:
        """Test high priority type values."""
        assert VariableType.FINAL.value == "final"
        assert VariableType.EVIDENCE.value == "evidence"
        assert VariableType.KEY_FACT.value == "key_fact"

    def test_medium_priority_types(self) -> None:
        """Test medium priority type values."""
        assert VariableType.SUMMARY.value == "summary"
        assert VariableType.EXTRACT.value == "extract"
        assert VariableType.SEARCH_RESULT.value == "search_result"

    def test_low_priority_types(self) -> None:
        """Test low priority type values."""
        assert VariableType.TEMP.value == "temp"
        assert VariableType.DEBUG.value == "debug"


class TestWorkspaceVariable:
    """Tests for WorkspaceVariable."""

    def test_creation(self) -> None:
        """Test variable creation."""
        var = WorkspaceVariable(
            name="test_var",
            value="test_value",
            var_type=VariableType.SUMMARY,
            tokens=100,
        )

        assert var.name == "test_var"
        assert var.value == "test_value"
        assert var.var_type == VariableType.SUMMARY
        assert var.tokens == 100

    def test_priority(self) -> None:
        """Test variable priority."""
        final_var = WorkspaceVariable(name="f", value="", var_type=VariableType.FINAL)
        temp_var = WorkspaceVariable(name="t", value="", var_type=VariableType.TEMP)

        assert final_var.priority > temp_var.priority

    def test_eviction_score(self) -> None:
        """Test eviction score calculation."""
        import time

        var = WorkspaceVariable(
            name="test",
            value="value",
            var_type=VariableType.FINAL,
            access_count=5,
        )

        score = var.eviction_score(time.time())
        assert score > 0  # Should have positive score
        assert score <= 100  # Should not exceed 100


class TestWorkspace:
    """Tests for Workspace."""

    @pytest.fixture
    def workspace(self) -> Workspace:
        """Create a fresh workspace for each test."""
        return Workspace(capacity=10, token_budget=1000)

    def test_set_and_get(self, workspace: Workspace) -> None:
        """Test setting and getting variables."""
        workspace.set("key1", "value1")
        assert workspace.get("key1") == "value1"

    def test_get_default(self, workspace: Workspace) -> None:
        """Test getting non-existent variable with default."""
        assert workspace.get("nonexistent", "default") == "default"

    def test_get_none_default(self, workspace: Workspace) -> None:
        """Test getting non-existent variable returns None."""
        assert workspace.get("nonexistent") is None

    def test_set_with_type(self, workspace: Workspace) -> None:
        """Test setting variable with type."""
        workspace.set("summary", "content", var_type=VariableType.SUMMARY)
        var = workspace.get_variable("summary")

        assert var is not None
        assert var.var_type == VariableType.SUMMARY

    def test_set_with_string_type(self, workspace: Workspace) -> None:
        """Test setting variable with string type."""
        workspace.set("evidence", "proof", var_type="evidence")
        var = workspace.get_variable("evidence")

        assert var is not None
        assert var.var_type == VariableType.EVIDENCE

    def test_has(self, workspace: Workspace) -> None:
        """Test checking variable existence."""
        workspace.set("exists", "value")

        assert workspace.has("exists") is True
        assert workspace.has("not_exists") is False

    def test_delete(self, workspace: Workspace) -> None:
        """Test deleting variable."""
        workspace.set("to_delete", "value")
        assert workspace.delete("to_delete") is True
        assert workspace.has("to_delete") is False

    def test_delete_not_found(self, workspace: Workspace) -> None:
        """Test deleting non-existent variable."""
        assert workspace.delete("nonexistent") is False

    def test_list_variables(self, workspace: Workspace) -> None:
        """Test listing all variables."""
        workspace.set("var1", "value1")
        workspace.set("var2", "value2", var_type=VariableType.SUMMARY)

        all_vars = workspace.list_variables()
        assert len(all_vars) == 2

    def test_list_variables_by_type(self, workspace: Workspace) -> None:
        """Test listing variables by type."""
        workspace.set("temp1", "value1", var_type=VariableType.TEMP)
        workspace.set("temp2", "value2", var_type=VariableType.TEMP)
        workspace.set("summary1", "value3", var_type=VariableType.SUMMARY)

        temp_vars = workspace.list_variables(VariableType.TEMP)
        assert len(temp_vars) == 2

    def test_list_names(self, workspace: Workspace) -> None:
        """Test listing variable names."""
        workspace.set("name1", "value1")
        workspace.set("name2", "value2")

        names = workspace.list_names()
        assert "name1" in names
        assert "name2" in names

    def test_eviction_by_capacity(self) -> None:
        """Test automatic eviction when capacity exceeded."""
        ws = Workspace(capacity=3, token_budget=10000)

        ws.set("var1", "a", var_type=VariableType.TEMP)
        ws.set("var2", "b", var_type=VariableType.TEMP)
        ws.set("var3", "c", var_type=VariableType.TEMP)
        evicted = ws.set("var4", "d", var_type=VariableType.TEMP)

        # One variable should be evicted
        assert len(evicted) >= 1
        assert len(ws.list_variables()) <= 3

    def test_eviction_preserves_final(self) -> None:
        """Test that FINAL type is not evicted."""
        ws = Workspace(capacity=2, token_budget=10000)

        ws.set("final_var", "important", var_type=VariableType.FINAL)
        ws.set("temp1", "a", var_type=VariableType.TEMP)
        ws.set("temp2", "b", var_type=VariableType.TEMP)  # Should trigger eviction

        # FINAL should still exist
        assert ws.has("final_var")

    def test_to_context_string(self, workspace: Workspace) -> None:
        """Test context string generation."""
        workspace.set("summary1", "First summary", var_type=VariableType.SUMMARY)
        workspace.set("temp1", "Temp data", var_type=VariableType.TEMP)

        context = workspace.to_context_string()
        assert "summary1" in context
        assert "temp1" in context

    def test_to_context_string_with_include_types(self, workspace: Workspace) -> None:
        """Test context string with type filtering."""
        workspace.set("summary1", "Summary", var_type=VariableType.SUMMARY)
        workspace.set("temp1", "Temp", var_type=VariableType.TEMP)

        context = workspace.to_context_string(include_types=[VariableType.SUMMARY])
        assert "summary1" in context
        assert "temp1" not in context

    def test_to_context_string_with_exclude_types(self, workspace: Workspace) -> None:
        """Test context string with type exclusion."""
        workspace.set("summary1", "Summary", var_type=VariableType.SUMMARY)
        workspace.set("debug1", "Debug", var_type=VariableType.DEBUG)

        context = workspace.to_context_string(exclude_types=[VariableType.DEBUG])
        assert "summary1" in context
        assert "debug1" not in context

    def test_get_final_answer(self, workspace: Workspace) -> None:
        """Test getting final answer."""
        workspace.set("final_answer", "The answer is 42", var_type=VariableType.FINAL)

        answer = workspace.get_final_answer()
        assert answer == "The answer is 42"

    def test_get_final_answer_none(self, workspace: Workspace) -> None:
        """Test getting final answer when none exists."""
        answer = workspace.get_final_answer()
        assert answer is None

    def test_get_evidence(self, workspace: Workspace) -> None:
        """Test getting evidence."""
        workspace.set("ev1", "Evidence 1", var_type=VariableType.EVIDENCE)
        workspace.set("ev2", "Evidence 2", var_type=VariableType.EVIDENCE)

        evidence = workspace.get_evidence()
        assert len(evidence) == 2
        assert "Evidence 1" in evidence
        assert "Evidence 2" in evidence

    def test_clear(self, workspace: Workspace) -> None:
        """Test clearing workspace."""
        workspace.set("var1", "value1")
        workspace.set("var2", "value2")

        workspace.clear()
        assert len(workspace.list_variables()) == 0

    def test_get_stats(self, workspace: Workspace) -> None:
        """Test getting statistics."""
        workspace.set("var1", "value1", var_type=VariableType.TEMP)
        workspace.set("var2", "value2", var_type=VariableType.SUMMARY)

        stats = workspace.get_stats()
        assert stats["variable_count"] == 2
        assert stats["capacity"] == 10
        assert stats["token_budget"] == 1000
        assert "by_type" in stats

    def test_to_dict(self, workspace: Workspace) -> None:
        """Test dictionary conversion."""
        workspace.set("var1", "value1")

        d = workspace.to_dict()
        assert "variables" in d
        assert "stats" in d
        assert "var1" in d["variables"]

    def test_update_variable(self, workspace: Workspace) -> None:
        """Test updating existing variable."""
        workspace.set("var1", "old_value")
        workspace.set("var1", "new_value")

        assert workspace.get("var1") == "new_value"
        assert len(workspace.list_variables()) == 1

    def test_access_count_increment(self, workspace: Workspace) -> None:
        """Test that access count increments."""
        workspace.set("var1", "value")

        workspace.get("var1")
        workspace.get("var1")
        workspace.get("var1")

        var = workspace.get_variable("var1")
        assert var is not None
        assert var.access_count == 3
