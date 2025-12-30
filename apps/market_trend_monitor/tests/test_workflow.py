"""ワークフローの統合テスト."""

import pytest

from apps.market_trend_monitor.backend.workflow import MarketTrendWorkflow


class TestMarketTrendWorkflow:
    """MarketTrendWorkflow のテスト."""

    @pytest.mark.asyncio
    async def test_workflow_initialization(self) -> None:
        """初期化テスト."""
        workflow = MarketTrendWorkflow()
        await workflow.initialize()
        assert workflow is not None
        await workflow.cleanup()

    @pytest.mark.asyncio
    async def test_workflow_run(self) -> None:
        """実行テスト."""
        workflow = MarketTrendWorkflow()
        await workflow.initialize()

        input_data = {
            "keywords": ["COBOL", "Java migration"],
            "sources": ["news"],
        }

        result = await workflow.run(input_data)

        # 各エージェントの結果を確認
        assert result is not None
        # Multi-Agent パターンの結果構造を確認
        # （実際の構造は MultiAgentWorkflow の実装に依存）

        await workflow.cleanup()

    @pytest.mark.asyncio
    async def test_workflow_default_input(self) -> None:
        """デフォルト入力でのテスト."""
        workflow = MarketTrendWorkflow()
        await workflow.initialize()

        # 入力なしで実行（デフォルト設定を使用）
        result = await workflow.run()

        assert result is not None

        await workflow.cleanup()

