"""エンティティ抽出ユニットテスト.

EntityExtractionService のテスト。
Mock LLM を使用（外部依存なし）。
"""

from __future__ import annotations

import json
from unittest.mock import AsyncMock

import pytest

from apps.market_trend_monitor.backend.services.entity_extraction_service import (
    Entity,
    EntityExtractionService,
    EntityRelation,
    EntityType,
)


# ============================================================
# Entity Model Tests
# ============================================================


class TestEntityModel:
    """Entity データモデルのテスト."""

    def test_entity_creation(self) -> None:
        """エンティティ生成テスト."""
        entity = Entity(name="IBM", entity_type=EntityType.COMPANY, confidence=0.9)
        assert entity.name == "IBM"
        assert entity.entity_type == EntityType.COMPANY
        assert entity.mentions == 1
        assert entity.confidence == 0.9

    def test_entity_to_dict(self) -> None:
        """to_dict 変換テスト."""
        entity = Entity(name="Java", entity_type=EntityType.TECHNOLOGY)
        d = entity.to_dict()
        assert d["name"] == "Java"
        assert d["entity_type"] == "technology"
        assert d["mentions"] == 1

    def test_entity_type_values(self) -> None:
        """EntityType全値テスト."""
        assert EntityType.COMPANY.value == "company"
        assert EntityType.TECHNOLOGY.value == "technology"
        assert EntityType.PRODUCT.value == "product"
        assert EntityType.PERSON.value == "person"
        assert EntityType.EVENT.value == "event"

    def test_entity_default_metadata(self) -> None:
        """デフォルトメタデータテスト."""
        entity = Entity(name="Test", entity_type=EntityType.TECHNOLOGY)
        assert entity.metadata == {}


class TestEntityRelationModel:
    """EntityRelation データモデルのテスト."""

    def test_relation_creation(self) -> None:
        """関係生成テスト."""
        rel = EntityRelation(
            source="IBM", target="COBOL",
            relation_type="develops", confidence=0.8,
        )
        assert rel.source == "IBM"
        assert rel.target == "COBOL"

    def test_relation_to_dict(self) -> None:
        """to_dict 変換テスト."""
        rel = EntityRelation(
            source="Java", target="COBOL",
            relation_type="migrates_from",
        )
        d = rel.to_dict()
        assert d["source"] == "Java"
        assert d["relation_type"] == "migrates_from"


# ============================================================
# Service Tests
# ============================================================


class TestEntityExtractionService:
    """EntityExtractionService のテスト."""

    async def test_extract_entities(self) -> None:
        """エンティティ抽出テスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(return_value=json.dumps([
            {"name": "IBM", "type": "company", "confidence": 0.95},
            {"name": "Java", "type": "technology", "confidence": 0.9},
        ]))
        service = EntityExtractionService(llm=mock_llm)

        entities = await service.extract_entities("IBM migrates COBOL to Java")
        assert len(entities) >= 2
        names = [e.name for e in entities]
        assert "IBM" in names
        assert "Java" in names

    async def test_extract_entities_empty_text(self) -> None:
        """空テキストのエンティティ抽出テスト."""
        service = EntityExtractionService(llm=AsyncMock())
        entities = await service.extract_entities("")
        assert entities == []

    async def test_extract_entities_llm_failure(self) -> None:
        """LLM失敗時のフォールバックテスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(side_effect=Exception("LLM error"))
        service = EntityExtractionService(llm=mock_llm)

        entities = await service.extract_entities("some text")
        assert entities == []

    async def test_extract_relations(self) -> None:
        """関係抽出テスト."""
        mock_llm = AsyncMock()
        mock_llm.chat = AsyncMock(return_value=json.dumps([
            {
                "source": "IBM", "target": "COBOL",
                "relation_type": "develops", "confidence": 0.8,
            },
        ]))
        service = EntityExtractionService(llm=mock_llm)

        entities = [
            Entity(name="IBM", entity_type=EntityType.COMPANY),
            Entity(name="COBOL", entity_type=EntityType.TECHNOLOGY),
        ]
        relations = await service.extract_relations(entities, "IBM develops COBOL tools")
        assert len(relations) == 1
        assert relations[0].source == "IBM"

    async def test_extract_relations_insufficient_entities(self) -> None:
        """エンティティ不足時の関係抽出テスト."""
        service = EntityExtractionService(llm=AsyncMock())
        entities = [Entity(name="IBM", entity_type=EntityType.COMPANY)]
        relations = await service.extract_relations(entities, "text")
        assert relations == []

    async def test_extract_relations_empty_text(self) -> None:
        """空テキスト時の関係抽出テスト."""
        service = EntityExtractionService(llm=AsyncMock())
        entities = [
            Entity(name="A", entity_type=EntityType.COMPANY),
            Entity(name="B", entity_type=EntityType.COMPANY),
        ]
        relations = await service.extract_relations(entities, "")
        assert relations == []

    def test_merge_entities_basic(self) -> None:
        """基本的な名寄せテスト."""
        service = EntityExtractionService()
        entities = [
            Entity(name="ibm", entity_type=EntityType.COMPANY, mentions=2),
            Entity(name="IBM", entity_type=EntityType.COMPANY, mentions=3),
        ]
        merged = service.merge_entities(entities)
        assert len(merged) == 1
        assert merged[0].name == "IBM"
        assert merged[0].mentions == 5

    def test_merge_entities_tcs_variants(self) -> None:
        """TCS表記揺れ統合テスト."""
        service = EntityExtractionService()
        entities = [
            Entity(name="tcs", entity_type=EntityType.COMPANY),
            Entity(name="Tata Consultancy Services", entity_type=EntityType.COMPANY),
        ]
        merged = service.merge_entities(entities)
        assert len(merged) == 1
        assert merged[0].name == "TCS"
        assert merged[0].mentions == 2

    def test_merge_entities_spring_variants(self) -> None:
        """Spring表記揺れ統合テスト."""
        service = EntityExtractionService()
        entities = [
            Entity(name="Spring", entity_type=EntityType.TECHNOLOGY),
            Entity(name="Spring Boot", entity_type=EntityType.TECHNOLOGY),
            Entity(name="Spring Framework", entity_type=EntityType.TECHNOLOGY),
        ]
        merged = service.merge_entities(entities)
        assert len(merged) == 1
        assert merged[0].name == "Spring Framework"

    def test_merge_entities_no_duplicates(self) -> None:
        """重複なしの名寄せテスト."""
        service = EntityExtractionService()
        entities = [
            Entity(name="IBM", entity_type=EntityType.COMPANY),
            Entity(name="Accenture", entity_type=EntityType.COMPANY),
        ]
        merged = service.merge_entities(entities)
        assert len(merged) == 2

    def test_merge_entities_sorted_by_mentions(self) -> None:
        """名寄せ結果がmentions降順テスト."""
        service = EntityExtractionService()
        entities = [
            Entity(name="IBM", entity_type=EntityType.COMPANY, mentions=1),
            Entity(name="Java", entity_type=EntityType.TECHNOLOGY, mentions=5),
        ]
        merged = service.merge_entities(entities)
        assert merged[0].name == "Java"

    def test_merge_entities_confidence_max(self) -> None:
        """名寄せ時にconfidenceが最大値テスト."""
        service = EntityExtractionService()
        entities = [
            Entity(name="IBM", entity_type=EntityType.COMPANY, confidence=0.7),
            Entity(name="ibm", entity_type=EntityType.COMPANY, confidence=0.9),
        ]
        merged = service.merge_entities(entities)
        assert merged[0].confidence == 0.9

    def test_parse_entities_response_valid(self) -> None:
        """正常なJSONレスポンスのパーステスト."""
        service = EntityExtractionService()
        raw = '[{"name": "IBM", "type": "company", "confidence": 0.9}]'
        entities = service._parse_entities_response(raw)
        assert len(entities) == 1
        assert entities[0].name == "IBM"

    def test_parse_entities_response_with_noise(self) -> None:
        """ノイズ付きJSONレスポンスのパーステスト."""
        service = EntityExtractionService()
        raw = 'Here are the entities:\n[{"name": "Java", "type": "technology"}]\nDone.'
        entities = service._parse_entities_response(raw)
        assert len(entities) == 1

    def test_parse_entities_response_invalid_json(self) -> None:
        """不正JSONのパーステスト."""
        service = EntityExtractionService()
        entities = service._parse_entities_response("not json at all")
        assert entities == []

    def test_parse_entities_response_invalid_type(self) -> None:
        """不正エンティティタイプのパーステスト."""
        service = EntityExtractionService()
        raw = '[{"name": "Test", "type": "unknown_type", "confidence": 0.5}]'
        entities = service._parse_entities_response(raw)
        assert len(entities) == 1
        assert entities[0].entity_type == EntityType.TECHNOLOGY  # フォールバック

    def test_extract_json_array(self) -> None:
        """JSON配列抽出テスト."""
        assert EntityExtractionService._extract_json_array("prefix [1,2] suffix") == "[1,2]"
        assert EntityExtractionService._extract_json_array("no array") == "[]"
        assert EntityExtractionService._extract_json_array("[1,2,3]") == "[1,2,3]"

    def test_merge_rules_coverage(self) -> None:
        """マージルールのカバレッジテスト."""
        rules = EntityExtractionService.MERGE_RULES
        assert "ibm" in rules
        assert "ntt data" in rules
        assert "spring boot" in rules
        assert rules["cobol"] == "COBOL"
