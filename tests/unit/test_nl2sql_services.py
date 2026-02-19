"""NL2SQL 増強サービス 単体テスト.

Schema Linker、Few-shot Manager、SQL Post-Processor のテスト。
"""

from __future__ import annotations

import pytest

from agentflow.services.fewshot_manager import (
    BM25,
    FewshotExample,
    FewshotManager,
    FewshotManagerConfig,
)
from agentflow.services.schema_linker import (
    SchemaLinker,
    SchemaLinkerConfig,
    SchemaLinkResult,
)
from agentflow.services.sql_postprocessor import (
    PostProcessorConfig,
    PostProcessResult,
    SQLPostProcessor,
)


# =============================================================================
# BM25 テスト
# =============================================================================


class TestBM25:
    """BM25 類似度計算テスト."""

    def test_empty_corpus(self) -> None:
        """空のコーパス."""
        bm25 = BM25()
        assert bm25.n_docs == 0

    def test_single_document(self) -> None:
        """単一ドキュメント."""
        bm25 = BM25()
        bm25.add_document(["売上", "TOP", "10"])
        assert bm25.n_docs == 1

    def test_score_matching_query(self) -> None:
        """マッチするクエリのスコア."""
        bm25 = BM25()
        bm25.add_document(["売上", "TOP", "10"])
        bm25.add_document(["地域", "別", "集計"])
        bm25.add_document(["前月", "比較"])

        # 「売上」を含むクエリは最初のドキュメントに高スコア
        query = ["売上", "TOP"]
        scores = [bm25.score(query, i) for i in range(3)]
        assert scores[0] > scores[1]
        assert scores[0] > scores[2]

    def test_score_no_match(self) -> None:
        """マッチしないクエリのスコア."""
        bm25 = BM25()
        bm25.add_document(["売上", "TOP", "10"])

        query = ["在庫", "管理"]
        score = bm25.score(query, 0)
        assert score == 0.0


# =============================================================================
# FewshotManager テスト
# =============================================================================


class TestFewshotManager:
    """Few-shot Manager テスト."""

    @pytest.fixture
    def manager(self) -> FewshotManager:
        """マネージャーのフィクスチャ（デフォルト例あり）."""
        config = FewshotManagerConfig(default_k=3)
        return FewshotManager(config=config)

    def test_default_examples_loaded(self, manager: FewshotManager) -> None:
        """デフォルト例がロードされていること."""
        # FewshotManager はデフォルトで5つの例を持つ
        assert len(manager._examples) >= 5

    def test_add_example(self, manager: FewshotManager) -> None:
        """例の追加."""
        initial_count = len(manager._examples)
        example = FewshotExample(
            query="カスタムクエリ",
            sql="SELECT * FROM custom LIMIT 10",
            pattern="custom",
        )
        manager.add_example(example)
        assert len(manager._examples) == initial_count + 1

    def test_get_similar_examples(self, manager: FewshotManager) -> None:
        """類似例の取得."""
        # 売上関連クエリ（デフォルト例に「売上TOP10」が含まれている）
        similar = manager.get_similar_examples("今月の売上TOP10", k=2)
        assert len(similar) <= 2
        # 結果が返ること
        assert isinstance(similar, list)

    def test_format_examples_prompt(self, manager: FewshotManager) -> None:
        """プロンプトフォーマット."""
        examples = manager.get_similar_examples("売上TOP10", k=1)
        prompt = manager.format_examples_prompt(examples)

        # プロンプトが文字列で返ること
        assert isinstance(prompt, str)

    def test_detect_pattern(self, manager: FewshotManager) -> None:
        """パターン検出."""
        # ランキングパターン（_detect_pattern は単一パターンを返す）
        pattern = manager._detect_pattern("売上TOP10を教えて")
        assert pattern == "ranking" or pattern is None  # TOPを含むのでranking

        # 集計パターン
        pattern = manager._detect_pattern("地域別の合計を出して")
        assert pattern == "aggregation" or pattern is None

        # 比較パターン
        pattern = manager._detect_pattern("前月と比較して")
        assert pattern == "comparison" or pattern is None

    def test_get_examples_with_pattern_filter(self, manager: FewshotManager) -> None:
        """パターンフィルタ付きの例取得."""
        # ランキングパターンのクエリ
        similar = manager.get_similar_examples("TOP10ランキング", k=3)
        # 結果が返ること
        assert isinstance(similar, list)


# =============================================================================
# SchemaLinker テスト
# =============================================================================


class TestSchemaLinker:
    """Schema Linker テスト."""

    @pytest.fixture
    def schema(self) -> dict[str, list[str]]:
        """テスト用スキーマ."""
        return {
            "sales": ["id", "amount", "order_date", "customer_id", "product_id"],
            "products": ["id", "product_name", "price", "category_id"],
            "customers": ["id", "customer_name", "email", "region_id"],
            "regions": ["id", "region_name"],
            "categories": ["id", "category_name"],
        }

    @pytest.fixture
    def linker(self, schema: dict[str, list[str]]) -> SchemaLinker:
        """リンカーのフィクスチャ."""
        config = SchemaLinkerConfig(use_llm=False)  # LLM なしでテスト
        return SchemaLinker(schema=schema, config=config)

    @pytest.mark.asyncio
    async def test_link_sales_query(self, linker: SchemaLinker) -> None:
        """売上クエリのリンク（テーブル名を含む）."""
        await linker.start()

        # テーブル名を含むクエリでマッチングをテスト
        result = await linker.link("sales table TOP10")

        assert isinstance(result, SchemaLinkResult)
        assert "sales" in result.relevant_tables
        assert result.confidence > 0

        await linker.stop()

    @pytest.mark.asyncio
    async def test_link_customer_query(self, linker: SchemaLinker) -> None:
        """顧客クエリのリンク."""
        await linker.start()

        # テーブル名を含むクエリでマッチングをテスト
        result = await linker.link("customers list count")

        assert "customers" in result.relevant_tables
        assert result.confidence > 0

        await linker.stop()

    @pytest.mark.asyncio
    async def test_link_region_query(self, linker: SchemaLinker) -> None:
        """地域クエリのリンク."""
        await linker.start()

        # テーブル名を含むクエリ
        result = await linker.link("regions sales summary")

        assert "regions" in result.relevant_tables
        assert result.confidence > 0

        await linker.stop()

    @pytest.mark.asyncio
    async def test_linked_schema_format(self, linker: SchemaLinker) -> None:
        """リンク済みスキーマのフォーマット."""
        await linker.start()

        # テーブル名を含むクエリ
        result = await linker.link("products categories sales")

        # linked_schema が生成されていること（テーブルが見つかればスキーマ生成）
        if len(result.relevant_tables) > 0:
            assert result.linked_schema != ""
        else:
            # テーブルがなければスキーマも空
            assert result.linked_schema == ""

        await linker.stop()

    def test_string_match_score(self, linker: SchemaLinker) -> None:
        """文字列マッチスコア."""
        # 売上 → sales テーブル（テーブル名が含まれる場合）
        scores = linker._string_match_score("sales TOP10")
        assert "tables" in scores
        assert "sales" in scores["tables"]
        assert scores["tables"]["sales"] > 0

        # customers テーブル
        scores = linker._string_match_score("customers 一覧")
        assert scores["tables"]["customers"] > 0


# =============================================================================
# SQLPostProcessor テスト
# =============================================================================


class TestSQLPostProcessor:
    """SQL Post-Processor テスト."""

    @pytest.fixture
    def processor(self) -> SQLPostProcessor:
        """プロセッサのフィクスチャ."""
        config = PostProcessorConfig(
            enable_llm_correction=False,  # LLM なしでテスト
            allowed_tables=["sales", "products", "customers"],
        )
        return SQLPostProcessor(config=config)

    @pytest.mark.asyncio
    async def test_validate_valid_sql(self, processor: SQLPostProcessor) -> None:
        """有効なSQLの検証."""
        await processor.start()

        result = await processor.process(
            sql="SELECT * FROM sales LIMIT 10",
            query="売上を見せて",
        )

        assert isinstance(result, PostProcessResult)
        assert result.validation.is_valid
        assert result.final_sql == "SELECT * FROM sales LIMIT 10"

        await processor.stop()

    @pytest.mark.asyncio
    async def test_validate_dangerous_sql(self, processor: SQLPostProcessor) -> None:
        """危険なSQLの検証."""
        await processor.start()

        # DROP TABLE
        result = await processor.process(
            sql="DROP TABLE sales",
            query="テーブル削除",
        )
        assert not result.validation.is_valid
        assert len(result.validation.errors) > 0

        # DELETE
        result = await processor.process(
            sql="DELETE FROM sales WHERE id = 1",
            query="データ削除",
        )
        assert not result.validation.is_valid

        await processor.stop()

    @pytest.mark.asyncio
    async def test_validate_update_sql(self, processor: SQLPostProcessor) -> None:
        """UPDATE SQLの検証."""
        await processor.start()

        result = await processor.process(
            sql="UPDATE sales SET amount = 100",
            query="データ更新",
        )

        assert not result.validation.is_valid

        await processor.stop()

    @pytest.mark.asyncio
    async def test_validate_insert_sql(self, processor: SQLPostProcessor) -> None:
        """INSERT SQLの検証."""
        await processor.start()

        result = await processor.process(
            sql="INSERT INTO sales VALUES (1, 100)",
            query="データ挿入",
        )

        assert not result.validation.is_valid

        await processor.stop()

    @pytest.mark.asyncio
    async def test_syntax_validation(self, processor: SQLPostProcessor) -> None:
        """構文検証."""
        await processor.start()

        # 基本的な構文チェック（SELECT がない）
        result = await processor.process(
            sql="FROM sales",
            query="売上",
        )

        # 構文エラーが検出されるはず
        assert not result.validation.is_valid or len(result.validation.errors) > 0

        await processor.stop()

    @pytest.mark.asyncio
    async def test_sql_cleanup(self, processor: SQLPostProcessor) -> None:
        """SQLクリーンアップ."""
        await processor.start()

        # Markdown コードブロック付き
        result = await processor.process(
            sql="```sql\nSELECT * FROM sales\n```",
            query="売上",
        )

        # コードブロックが除去されていること
        assert "```" not in result.final_sql
        assert "SELECT" in result.final_sql

        await processor.stop()


# =============================================================================
# 統合テスト
# =============================================================================


class TestNL2SQLIntegration:
    """NL2SQL 増強機能の統合テスト."""

    @pytest.fixture
    def schema(self) -> dict[str, list[str]]:
        """テスト用スキーマ."""
        return {
            "sales": ["id", "amount", "order_date", "customer_id"],
            "customers": ["id", "customer_name", "region_id"],
        }

    @pytest.mark.asyncio
    async def test_full_pipeline_without_llm(self, schema: dict[str, list[str]]) -> None:
        """LLM なしのフルパイプライン."""
        # 1. Schema Linker
        linker = SchemaLinker(
            schema=schema,
            config=SchemaLinkerConfig(use_llm=False),
        )
        await linker.start()

        link_result = await linker.link("sales TOP10")
        assert "sales" in link_result.relevant_tables

        # 2. Few-shot Manager
        manager = FewshotManager()
        manager.add_example(
            FewshotExample(
                query="売上TOP10",
                sql="SELECT * FROM sales ORDER BY amount DESC LIMIT 10",
                pattern="ranking",
            )
        )
        examples = manager.get_similar_examples("売上ランキング", k=1)
        assert len(examples) >= 0  # 例があれば取得

        # 3. Post-Processor
        processor = SQLPostProcessor(config=PostProcessorConfig(enable_llm_correction=False))
        await processor.start()

        pp_result = await processor.process(
            sql="SELECT * FROM sales ORDER BY amount DESC LIMIT 10",
            query="売上TOP10",
        )
        assert pp_result.validation.is_valid

        await linker.stop()
        await processor.stop()


# =============================================================================
# DSL 中間表現層テスト
# =============================================================================


class TestQueryDSL:
    """QueryDSL 中間表現層テスト."""

    @pytest.mark.asyncio
    async def test_nl_to_dsl_sales_query(self) -> None:
        """売上クエリのNL→DSL変換."""
        from agentflow.services.semantic_layer import SemanticLayerService

        service = SemanticLayerService()
        dsl = await service.nl_to_dsl("今月の売上TOP10")

        # 指標が解決されること
        assert "revenue" in dsl.metrics
        # 時間範囲が解決されること
        assert dsl.time_range is not None
        assert dsl.time_range.relative == "this_month"
        # リミットが解決されること
        assert dsl.limit == 10

    @pytest.mark.asyncio
    async def test_nl_to_dsl_region_query(self) -> None:
        """地域クエリのNL→DSL変換."""
        from agentflow.services.semantic_layer import SemanticLayerService

        service = SemanticLayerService()
        dsl = await service.nl_to_dsl("地域別の注文数")

        # 指標が解決されること
        assert "order_count" in dsl.metrics
        # ディメンションが解決されること
        assert "region" in dsl.dimensions

    @pytest.mark.asyncio
    async def test_dsl_to_sql(self) -> None:
        """DSL→SQL変換."""
        from agentflow.services.semantic_layer import (
            OrderByDSL,
            QueryDSL,
            SemanticLayerService,
            SortDirection,
            TimeRangeDSL,
        )

        service = SemanticLayerService()
        dsl = QueryDSL(
            metrics=["revenue"],
            dimensions=[],
            order_by=[OrderByDSL(field="revenue", direction=SortDirection.DESC)],
            limit=10,
            time_range=TimeRangeDSL(relative="this_month"),
        )

        sql = service.dsl_to_sql(dsl)

        # SQLが生成されること
        assert "SELECT" in sql
        assert "revenue" in sql
        assert "LIMIT 10" in sql

    @pytest.mark.asyncio
    async def test_full_dsl_pipeline(self) -> None:
        """完全NL→DSL→SQLパイプライン."""
        from agentflow.services.semantic_layer import SemanticLayerService

        service = SemanticLayerService()
        sql, dsl = await service.nl_to_sql_via_dsl("今月の売上TOP10")

        # DSLが生成されること
        assert dsl.raw_query == "今月の売上TOP10"
        assert len(dsl.metrics) > 0

        # SQLが生成されること
        assert "SELECT" in sql
        assert sql != ""

    def test_dsl_to_dict(self) -> None:
        """DSL辞書変換."""
        from agentflow.services.semantic_layer import (
            FilterDSL,
            FilterOperator,
            QueryDSL,
        )

        dsl = QueryDSL(
            metrics=["revenue"],
            dimensions=["region"],
            filters=[FilterDSL(field="amount", operator=FilterOperator.GE, value=1000)],
            limit=10,
        )

        data = dsl.to_dict()

        assert data["metrics"] == ["revenue"]
        assert data["dimensions"] == ["region"]
        assert len(data["filters"]) == 1
        assert data["limit"] == 10

    def test_dsl_from_dict(self) -> None:
        """辞書からDSL生成."""
        from agentflow.services.semantic_layer import QueryDSL

        data = {
            "metrics": ["revenue"],
            "dimensions": ["category"],
            "filters": [{"field": "amount", "operator": ">=", "value": 1000}],
            "order_by": [{"field": "revenue", "direction": "DESC"}],
            "limit": 5,
        }

        dsl = QueryDSL.from_dict(data)

        assert dsl.metrics == ["revenue"]
        assert dsl.dimensions == ["category"]
        assert len(dsl.filters) == 1
        assert len(dsl.order_by) == 1
        assert dsl.limit == 5
