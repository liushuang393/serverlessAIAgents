"""harness/governance/engine.py の単体テスト.

GovernanceEngine のインポートと基本操作をテスト。
"""

from __future__ import annotations


class TestGovernanceEngineImport:
    """GovernanceEngine のインポートテスト."""

    def test_import_governance_engine(self) -> None:
        """GovernanceEngine がインポートできる."""
        from harness.governance.engine import GovernanceEngine

        engine = GovernanceEngine()
        assert engine is not None
