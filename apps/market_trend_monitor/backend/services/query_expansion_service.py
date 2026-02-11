"""クエリ拡張サービス.

LLMを使用してクエリを意味的に拡張し、検索精度を向上させます。
COBOL/Java移行ドメインの同義語マッピングと多言語クエリ生成を提供します。
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow import get_llm


class QueryExpansionService:
    """クエリ拡張サービス.

    - ドメイン同義語マッピングによるクエリ拡張
    - LLMベースの意味的クエリ拡張
    - 日英多言語クエリ生成
    """

    DOMAIN_SYNONYMS: dict[str, list[str]] = {
        "COBOL": ["mainframe", "legacy system", "レガシー", "メインフレーム", "汎用機"],
        "Java": ["JVM", "Spring", "Jakarta EE", "Java EE", "マイクロサービス"],
        "migration": [
            "modernization", "transformation", "移行", "刷新",
            "リプレース", "マイグレーション",
        ],
        "AI": [
            "artificial intelligence", "machine learning", "deep learning",
            "人工知能", "機械学習", "LLM", "生成AI",
        ],
        "LLM": [
            "large language model", "GPT", "Claude", "大規模言語モデル",
            "生成AI", "Generative AI",
        ],
        "modernization": [
            "DX", "digital transformation", "デジタル変革",
            "レガシーモダナイゼーション", "システム刷新",
        ],
        "legacy": [
            "レガシー", "旧システム", "old system", "technical debt",
            "技術的負債",
        ],
        "refactoring": [
            "リファクタリング", "code transformation", "コード変換",
            "自動変換",
        ],
    }

    def __init__(self, *, llm: Any | None = None) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._llm = llm

    def _get_llm(self) -> Any:
        """LLMインスタンスを取得（遅延初期化）."""
        if self._llm is None:
            self._llm = get_llm(temperature=0.3)
        return self._llm

    def expand_with_synonyms(self, query: str) -> list[str]:
        """ドメイン同義語を使用してクエリを拡張.

        Args:
            query: 元のクエリ

        Returns:
            拡張されたクエリリスト（元クエリを含む）
        """
        expanded = [query]
        query_lower = query.lower()

        for term, synonyms in self.DOMAIN_SYNONYMS.items():
            if term.lower() in query_lower:
                for synonym in synonyms:
                    expanded_query = query_lower.replace(term.lower(), synonym.lower())
                    if expanded_query != query_lower:
                        expanded.append(expanded_query)

        return list(dict.fromkeys(expanded))

    async def expand_query(self, query: str, max_expansions: int = 5) -> list[str]:
        """LLMを使用してクエリを意味的に拡張.

        Args:
            query: 元のクエリ
            max_expansions: 最大拡張数

        Returns:
            拡張されたクエリリスト
        """
        synonym_expanded = self.expand_with_synonyms(query)

        try:
            llm = self._get_llm()
            prompt = (
                f'Generate {max_expansions} alternative search queries for: "{query}"\n'
                "Focus on COBOL to Java migration domain.\n"
                "Return one query per line, no numbering.\n"
            )
            response = await llm.chat([{"role": "user", "content": prompt}])
            text = response if isinstance(response, str) else str(response)

            llm_queries = [
                line.strip() for line in text.strip().split("\n")
                if line.strip() and len(line.strip()) > 3
            ][:max_expansions]

            all_queries = synonym_expanded + llm_queries
            return list(dict.fromkeys(all_queries))

        except Exception as e:
            self._logger.warning("LLMクエリ拡張失敗: %s", e)
            return synonym_expanded

    async def generate_multilingual_queries(
        self,
        query: str,
    ) -> dict[str, list[str]]:
        """多言語クエリを生成.

        Args:
            query: 元のクエリ

        Returns:
            言語コードをキーとするクエリリスト辞書
        """
        result: dict[str, list[str]] = {"en": [query], "ja": []}

        try:
            llm = self._get_llm()

            is_japanese = any(
                "\u3040" <= ch <= "\u9fff" or "\uff00" <= ch <= "\uffef"
                for ch in query
            )

            if is_japanese:
                result["ja"].append(query)
                prompt = (
                    f"Translate the following Japanese query to English. "
                    f"Return only the translation.\n\n{query}"
                )
                response = await llm.chat([{"role": "user", "content": prompt}])
                en_query = (response if isinstance(response, str) else str(response)).strip()
                if en_query:
                    result["en"] = [en_query]
            else:
                prompt = (
                    f"Translate the following English query to Japanese. "
                    f"Return only the translation.\n\n{query}"
                )
                response = await llm.chat([{"role": "user", "content": prompt}])
                ja_query = (response if isinstance(response, str) else str(response)).strip()
                if ja_query:
                    result["ja"].append(ja_query)

        except Exception as e:
            self._logger.warning("多言語クエリ生成失敗: %s", e)

        for lang in result:
            result[lang] = list(dict.fromkeys(result[lang]))

        return result
