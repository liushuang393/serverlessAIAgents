"""エンティティ抽出サービス.

LLMベースの名前付きエンティティ認識（NER）を提供します。
Phase 12: LLM失敗時のルールベースfallbackを追加。
"""

from __future__ import annotations

import json
import logging
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from agentflow import get_llm


class EntityType(str, Enum):
    """エンティティタイプ."""

    COMPANY = "company"
    TECHNOLOGY = "technology"
    PRODUCT = "product"
    PERSON = "person"
    EVENT = "event"


@dataclass
class Entity:
    """エンティティデータモデル."""

    name: str
    entity_type: EntityType
    mentions: int = 1
    confidence: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "name": self.name,
            "entity_type": self.entity_type.value,
            "mentions": self.mentions,
            "confidence": self.confidence,
            "metadata": self.metadata,
        }


@dataclass
class EntityRelation:
    """エンティティ間関係データモデル."""

    source: str
    target: str
    relation_type: str
    confidence: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "source": self.source,
            "target": self.target,
            "relation_type": self.relation_type,
            "confidence": self.confidence,
            "metadata": self.metadata,
        }


class EntityExtractionService:
    """エンティティ抽出サービス.

    - LLMベースのNER
    - エンティティ間関係抽出
    - 表記揺れ統合（名寄せ）
    """

    MERGE_RULES: dict[str, str] = {
        # 企業名 (50件)
        "ibm": "IBM",
        "international business machines": "IBM",
        "accenture": "Accenture",
        "tcs": "TCS",
        "tata consultancy services": "TCS",
        "infosys": "Infosys",
        "ntt data": "NTT DATA",
        "nttdata": "NTT DATA",
        "nttデータ": "NTT DATA",
        "wipro": "Wipro",
        "cognizant": "Cognizant",
        "capgemini": "Capgemini",
        "dxc technology": "DXC Technology",
        "dxc": "DXC Technology",
        "micro focus": "Micro Focus",
        "opentext": "OpenText",
        "broadcom": "Broadcom",
        "oracle": "Oracle",
        "microsoft": "Microsoft",
        "google": "Google",
        "amazon": "Amazon",
        "aws": "AWS",
        "amazon web services": "AWS",
        "azure": "Microsoft Azure",
        "gcp": "Google Cloud",
        "google cloud": "Google Cloud",
        "fujitsu": "Fujitsu",
        "富士通": "Fujitsu",
        "hitachi": "Hitachi",
        "日立": "Hitachi",
        "nec": "NEC",
        "deloitte": "Deloitte",
        "kpmg": "KPMG",
        "mckinsey": "McKinsey",
        "gartner": "Gartner",
        "forrester": "Forrester",
        "red hat": "Red Hat",
        "vmware": "VMware",
        "salesforce": "Salesforce",
        "sap": "SAP",
        "atos": "Atos",
        "unisys": "Unisys",
        "lzlabs": "LzLabs",
        "raincode": "Raincode",
        "hcl": "HCL Technologies",
        "hcl technologies": "HCL Technologies",
        "tech mahindra": "Tech Mahindra",
        "persistent systems": "Persistent Systems",
        "mphasis": "Mphasis",
        "openai": "OpenAI",
        "anthropic": "Anthropic",
        # 技術名 (30件)
        "java": "Java",
        "jvm": "Java",
        "cobol": "COBOL",
        "spring": "Spring Framework",
        "spring boot": "Spring Framework",
        "spring framework": "Spring Framework",
        "kubernetes": "Kubernetes",
        "k8s": "Kubernetes",
        "docker": "Docker",
        "terraform": "Terraform",
        "microservices": "Microservices",
        "マイクロサービス": "Microservices",
        "legacy modernization": "Legacy Modernization",
        "レガシーモダナイゼーション": "Legacy Modernization",
        "mainframe": "Mainframe",
        "メインフレーム": "Mainframe",
        "devops": "DevOps",
        "ci/cd": "CI/CD",
        "api gateway": "API Gateway",
        "graphql": "GraphQL",
        "rest api": "REST API",
        "postgresql": "PostgreSQL",
        "mongodb": "MongoDB",
        "kafka": "Apache Kafka",
        "apache kafka": "Apache Kafka",
        "jenkins": "Jenkins",
        "github actions": "GitHub Actions",
        "copilot": "GitHub Copilot",
        "github copilot": "GitHub Copilot",
        "chatgpt": "ChatGPT",
        "gpt-4": "GPT-4",
        "claude": "Claude",
    }

    # Phase 12: ルールベースパターン辞書
    RULE_PATTERNS: dict[str, list[tuple[str, EntityType]]] = {
        "companies": [
            (r"\bIBM\b", EntityType.COMPANY),
            (r"\bAccenture\b", EntityType.COMPANY),
            (r"\bTCS\b", EntityType.COMPANY),
            (r"\bInfosys\b", EntityType.COMPANY),
            (r"\bNTT\s*DATA\b", EntityType.COMPANY),
            (r"\bCognizant\b", EntityType.COMPANY),
            (r"\bCapgemini\b", EntityType.COMPANY),
            (r"\bDXC\b", EntityType.COMPANY),
            (r"\bOracle\b", EntityType.COMPANY),
            (r"\bMicrosoft\b", EntityType.COMPANY),
            (r"\bGoogle\b", EntityType.COMPANY),
            (r"\bAWS\b", EntityType.COMPANY),
            (r"\bDeloitte\b", EntityType.COMPANY),
            (r"\bGartner\b", EntityType.COMPANY),
            (r"\bOpenAI\b", EntityType.COMPANY),
            (r"\bAnthropic\b", EntityType.COMPANY),
        ],
        "technologies": [
            (r"\bCOBOL\b", EntityType.TECHNOLOGY),
            (r"\bJava\b", EntityType.TECHNOLOGY),
            (r"\bSpring\s*(Boot|Framework)?\b", EntityType.TECHNOLOGY),
            (r"\bKubernetes\b", EntityType.TECHNOLOGY),
            (r"\bDocker\b", EntityType.TECHNOLOGY),
            (r"\bMicroservices?\b", EntityType.TECHNOLOGY),
            (r"\bDevOps\b", EntityType.TECHNOLOGY),
            (r"\bCI/CD\b", EntityType.TECHNOLOGY),
            (r"\bMainframe\b", EntityType.TECHNOLOGY),
            (r"\bLLM\b", EntityType.TECHNOLOGY),
            (r"\bGPT-\d\b", EntityType.TECHNOLOGY),
            (r"\bTerraform\b", EntityType.TECHNOLOGY),
            (r"\bGraphQL\b", EntityType.TECHNOLOGY),
            (r"\bPostgreSQL\b", EntityType.TECHNOLOGY),
            (r"\bKafka\b", EntityType.TECHNOLOGY),
        ],
    }

    def __init__(self, *, llm: Any | None = None) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._llm = llm

    def _get_llm(self) -> Any:
        """LLMインスタンスを取得."""
        if self._llm is None:
            self._llm = get_llm(temperature=0.2)
        return self._llm

    async def extract_entities(self, text: str) -> list[Entity]:
        """テキストからエンティティを抽出.

        Args:
            text: 抽出対象テキスト

        Returns:
            抽出されたエンティティリスト
        """
        if not text or not text.strip():
            return []

        try:
            llm = self._get_llm()
            prompt = (
                "Extract named entities from the following text.\n"
                "Entity types: COMPANY, TECHNOLOGY, PRODUCT, PERSON, EVENT\n"
                "Return JSON array: "
                '[{"name": "...", "type": "...", "confidence": 0.0-1.0}]\n\n'
                f"Text: {text[:2000]}\n\nJSON:"
            )
            response = await llm.chat([{"role": "user", "content": prompt}])
            raw = response if isinstance(response, str) else str(response)

            entities = self._parse_entities_response(raw)
            return self.merge_entities(entities)

        except Exception as e:
            self._logger.warning("LLMエンティティ抽出失敗、ルールベースにフォールバック: %s", e)
            return self._rule_based_extract(text)

    async def extract_relations(
        self,
        entities: list[Entity],
        text: str,
    ) -> list[EntityRelation]:
        """エンティティ間の関係を抽出.

        Args:
            entities: エンティティリスト
            text: 元テキスト

        Returns:
            関係リスト
        """
        if len(entities) < 2 or not text:
            return []

        try:
            llm = self._get_llm()
            entity_names = [e.name for e in entities[:10]]
            prompt = (
                "Identify relationships between these entities:\n"
                f"Entities: {', '.join(entity_names)}\n"
                "Return JSON array: "
                '[{"source": "...", "target": "...", '
                '"relation_type": "...", "confidence": 0.0-1.0}]\n'
                "Relation types: uses, competes_with, partners_with, "
                "develops, migrates_from, migrates_to\n\n"
                f"Context: {text[:1500]}\n\nJSON:"
            )
            response = await llm.chat([{"role": "user", "content": prompt}])
            raw = response if isinstance(response, str) else str(response)

            return self._parse_relations_response(raw)

        except Exception as e:
            self._logger.warning("関係抽出失敗: %s", e)
            return []

    def merge_entities(self, entities: list[Entity]) -> list[Entity]:
        """表記揺れを統合してエンティティを名寄せ.

        Args:
            entities: 統合対象エンティティリスト

        Returns:
            名寄せ後のエンティティリスト
        """
        merged: dict[str, Entity] = {}

        for entity in entities:
            canonical = self.MERGE_RULES.get(entity.name.lower(), entity.name)

            if canonical in merged:
                existing = merged[canonical]
                existing.mentions += entity.mentions
                existing.confidence = max(existing.confidence, entity.confidence)
            else:
                merged[canonical] = Entity(
                    name=canonical,
                    entity_type=entity.entity_type,
                    mentions=entity.mentions,
                    confidence=entity.confidence,
                    metadata=entity.metadata.copy(),
                )

        return sorted(merged.values(), key=lambda e: e.mentions, reverse=True)

    def _rule_based_extract(self, text: str) -> list[Entity]:
        """ルールベースのエンティティ抽出（LLM失敗時のfallback）.

        Phase 12: 正規表現パターンマッチングによるエンティティ検出。
        COBOL/Java移行ドメインに特化したパターン辞書を使用。
        """
        entities: list[Entity] = []
        seen: set[str] = set()

        for _category, patterns in self.RULE_PATTERNS.items():
            for pattern, entity_type in patterns:
                matches = re.findall(pattern, text, re.IGNORECASE)
                if matches:
                    # パターンから正規化名を取得
                    canonical = re.sub(r"\\b|\\s\*|\(.*?\)\??", "", pattern).strip()
                    # MERGE_RULESで正規化を試みる
                    first_match = matches[0] if isinstance(matches[0], str) else matches[0]
                    canonical = self.MERGE_RULES.get(first_match.lower(), first_match)

                    if canonical not in seen:
                        entities.append(Entity(
                            name=canonical,
                            entity_type=entity_type,
                            mentions=len(matches),
                            confidence=0.7,
                            metadata={"extraction_method": "rule_based"},
                        ))
                        seen.add(canonical)

        return self.merge_entities(entities)

    def _parse_entities_response(self, raw: str) -> list[Entity]:
        """LLMレスポンスをパースしてEntityリストに変換."""
        try:
            json_str = self._extract_json_array(raw)
            data = json.loads(json_str)
            if not isinstance(data, list):
                return []

            entities = []
            for item in data:
                if not isinstance(item, dict):
                    continue
                name = item.get("name", "").strip()
                if not name:
                    continue
                entity_type_str = item.get("type", "technology").lower()
                try:
                    entity_type = EntityType(entity_type_str)
                except ValueError:
                    entity_type = EntityType.TECHNOLOGY

                entities.append(Entity(
                    name=name,
                    entity_type=entity_type,
                    confidence=float(item.get("confidence", 0.5)),
                ))
            return entities

        except (json.JSONDecodeError, ValueError) as e:
            self._logger.debug("エンティティパース失敗: %s", e)
            return []

    def _parse_relations_response(self, raw: str) -> list[EntityRelation]:
        """LLMレスポンスをパースしてEntityRelationリストに変換."""
        try:
            json_str = self._extract_json_array(raw)
            data = json.loads(json_str)
            if not isinstance(data, list):
                return []

            relations = []
            for item in data:
                if not isinstance(item, dict):
                    continue
                source = item.get("source", "").strip()
                target = item.get("target", "").strip()
                if not source or not target:
                    continue
                relations.append(EntityRelation(
                    source=source,
                    target=target,
                    relation_type=item.get("relation_type", "related"),
                    confidence=float(item.get("confidence", 0.5)),
                ))
            return relations

        except (json.JSONDecodeError, ValueError) as e:
            self._logger.debug("関係パース失敗: %s", e)
            return []

    @staticmethod
    def _extract_json_array(text: str) -> str:
        """テキストからJSON配列を抽出."""
        start = text.find("[")
        end = text.rfind("]")
        if start == -1 or end == -1 or end <= start:
            return "[]"
        return text[start:end + 1]
