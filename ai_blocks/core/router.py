"""
Router（ルーター）コンポーネント

このモジュールは、インテント判定とAgent/Tool振り分けのための抽象インターフェースと
具体的な実装を提供します。
"""

import re
from abc import ABC, abstractmethod
from enum import Enum
from typing import Any, Dict, List, Optional, Pattern

from ..utils.logging import get_logger
from .models import RouteDefinition, RouteResult

logger = get_logger(__name__)


class RouteType(str, Enum):
    """ルートタイプ"""

    EXACT = "exact"  # 完全一致
    REGEX = "regex"  # 正規表現
    KEYWORD = "keyword"  # キーワード
    SEMANTIC = "semantic"  # 意味的類似性
    FUNCTION = "function"  # カスタム関数


class RouterInterface(ABC):
    """インテント判定とAgent/Tool振り分けのための抽象インターフェース"""

    @abstractmethod
    async def route(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> RouteResult:
        """
        入力に基づいて適切なAgent/Toolに振り分ける

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            RouteResult: ルーティング結果
        """
        pass

    @abstractmethod
    def register_route(self, route: RouteDefinition) -> None:
        """
        新しいルートを登録する

        Args:
            route: 登録するルート定義
        """
        pass

    @abstractmethod
    def unregister_route(self, pattern: str) -> bool:
        """
        ルートの登録を解除する

        Args:
            pattern: 解除するルートのパターン

        Returns:
            bool: 解除が成功したかどうか
        """
        pass

    @abstractmethod
    def get_routes(self) -> List[RouteDefinition]:
        """
        登録されているルートの一覧を取得する

        Returns:
            List[RouteDefinition]: ルート定義のリスト
        """
        pass


class RuleBasedRouter(RouterInterface):
    """ルールベースのルーター"""

    def __init__(self, default_target: str = "default"):
        """
        ルールベースのルーターを初期化する

        Args:
            default_target: デフォルトのターゲット
        """
        self.default_target = default_target
        self._routes: List[RouteDefinition] = []
        self._compiled_patterns: Dict[str, Pattern] = {}

        # デフォルトルートを追加
        self._add_default_routes()

        logger.info(f"ルールベースのルーターを初期化しました（デフォルトターゲット: {default_target}）")

    async def route(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> RouteResult:
        """
        入力に基づいて適切なAgent/Toolに振り分ける

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            RouteResult: ルーティング結果
        """
        if not input_text.strip():
            return RouteResult(
                target=self.default_target,
                confidence=0.0,
                reasoning="空の入力のためデフォルトターゲットを選択",
            )

        # 優先度順にルートをチェック
        sorted_routes = sorted(self._routes, key=lambda r: r.priority, reverse=True)

        for route in sorted_routes:
            match_result = await self._check_route_match(
                route, input_text, context or {}
            )
            if match_result:
                confidence, parameters, reasoning = match_result

                logger.debug(f"ルート '{route.pattern}' にマッチしました（信頼度: {confidence:.2f}）")

                return RouteResult(
                    target=route.target,
                    confidence=confidence,
                    parameters=parameters,
                    reasoning=reasoning,
                )

        # マッチするルートがない場合はデフォルト
        logger.debug(f"マッチするルートがないためデフォルトターゲット '{self.default_target}' を選択")

        return RouteResult(
            target=self.default_target,
            confidence=0.1,
            reasoning="マッチするルートがないためデフォルトを選択",
        )

    def register_route(self, route: RouteDefinition) -> None:
        """
        新しいルートを登録する

        Args:
            route: 登録するルート定義
        """
        # 既存のルートを削除（同じパターンの場合）
        self.unregister_route(route.pattern)

        # 新しいルートを追加
        self._routes.append(route)

        # 正規表現パターンをコンパイル
        if route.conditions.get("type") == RouteType.REGEX:
            try:
                self._compiled_patterns[route.pattern] = re.compile(
                    route.pattern, re.IGNORECASE
                )
            except re.error as e:
                logger.warning(f"正規表現パターンのコンパイルに失敗しました: {route.pattern} - {e}")

        logger.info(f"ルート '{route.pattern}' を登録しました（ターゲット: {route.target}）")

    def unregister_route(self, pattern: str) -> bool:
        """
        ルートの登録を解除する

        Args:
            pattern: 解除するルートのパターン

        Returns:
            bool: 解除が成功したかどうか
        """
        original_count = len(self._routes)
        self._routes = [r for r in self._routes if r.pattern != pattern]

        # コンパイル済みパターンも削除
        if pattern in self._compiled_patterns:
            del self._compiled_patterns[pattern]

        removed = len(self._routes) < original_count
        if removed:
            logger.info(f"ルート '{pattern}' の登録を解除しました")

        return removed

    def get_routes(self) -> List[RouteDefinition]:
        """
        登録されているルートの一覧を取得する

        Returns:
            List[RouteDefinition]: ルート定義のリスト
        """
        return self._routes.copy()

    async def _check_route_match(
        self, route: RouteDefinition, input_text: str, context: Dict[str, Any]
    ) -> Optional[tuple]:
        """
        ルートがマッチするかチェックする

        Args:
            route: チェックするルート
            input_text: 入力テキスト
            context: コンテキスト

        Returns:
            Optional[tuple]: マッチした場合は(confidence, parameters, reasoning)のタプル
        """
        route_type = route.conditions.get("type", RouteType.KEYWORD)

        if route_type == RouteType.EXACT:
            return await self._check_exact_match(route, input_text, context)
        elif route_type == RouteType.REGEX:
            return await self._check_regex_match(route, input_text, context)
        elif route_type == RouteType.KEYWORD:
            return await self._check_keyword_match(route, input_text, context)
        elif route_type == RouteType.FUNCTION:
            return await self._check_function_match(route, input_text, context)
        else:
            logger.warning(f"サポートされていないルートタイプです: {route_type}")
            return None

    async def _check_exact_match(
        self, route: RouteDefinition, input_text: str, context: Dict[str, Any]
    ) -> Optional[tuple]:
        """完全一致チェック"""
        if input_text.strip().lower() == route.pattern.lower():
            return (1.0, {}, f"完全一致: '{route.pattern}'")
        return None

    async def _check_regex_match(
        self, route: RouteDefinition, input_text: str, context: Dict[str, Any]
    ) -> Optional[tuple]:
        """正規表現マッチチェック"""
        pattern = self._compiled_patterns.get(route.pattern)
        if not pattern:
            return None

        match = pattern.search(input_text)
        if match:
            parameters = match.groupdict()
            confidence = 0.9  # 正規表現マッチは高い信頼度
            reasoning = f"正規表現マッチ: '{route.pattern}'"
            return (confidence, parameters, reasoning)

        return None

    async def _check_keyword_match(
        self, route: RouteDefinition, input_text: str, context: Dict[str, Any]
    ) -> Optional[tuple]:
        """キーワードマッチチェック"""
        keywords = route.conditions.get("keywords", [route.pattern])
        if isinstance(keywords, str):
            keywords = [keywords]

        input_lower = input_text.lower()
        matched_keywords = []

        for keyword in keywords:
            if keyword.lower() in input_lower:
                matched_keywords.append(keyword)

        if matched_keywords:
            confidence = min(0.8, len(matched_keywords) / len(keywords))
            reasoning = f"キーワードマッチ: {', '.join(matched_keywords)}"
            return (confidence, {"matched_keywords": matched_keywords}, reasoning)

        return None

    async def _check_function_match(
        self, route: RouteDefinition, input_text: str, context: Dict[str, Any]
    ) -> Optional[tuple]:
        """カスタム関数マッチチェック"""
        match_function = route.conditions.get("function")
        if not match_function or not callable(match_function):
            return None

        try:
            result = match_function(input_text, context)
            if isinstance(result, bool):
                if result:
                    return (0.7, {}, f"カスタム関数マッチ: {match_function.__name__}")
            elif isinstance(result, (int, float)):
                if result > 0:
                    return (
                        min(1.0, result),
                        {},
                        f"カスタム関数マッチ: {match_function.__name__}",
                    )
            elif isinstance(result, tuple) and len(result) >= 2:
                confidence, parameters = result[:2]
                reasoning = (
                    result[2]
                    if len(result) > 2
                    else f"カスタム関数マッチ: {match_function.__name__}"
                )
                return (confidence, parameters, reasoning)
        except Exception as e:
            logger.warning(f"カスタム関数の実行中にエラーが発生しました: {e}")

        return None

    def _add_default_routes(self) -> None:
        """デフォルトルートを追加する"""
        # ヘルプ関連
        help_route = RouteDefinition(
            pattern="help",
            target="help",
            priority=100,
            conditions={
                "type": RouteType.KEYWORD,
                "keywords": ["help", "ヘルプ", "助けて", "使い方", "how to"],
            },
            description="ヘルプ関連のクエリ",
        )
        self._routes.append(help_route)

        # 計算関連
        calc_route = RouteDefinition(
            pattern=r"(\d+)\s*[+\-*/]\s*(\d+)",
            target="calculator",
            priority=80,
            conditions={"type": RouteType.REGEX},
            description="数学計算関連のクエリ",
        )
        self._routes.append(calc_route)
        self._compiled_patterns[calc_route.pattern] = re.compile(calc_route.pattern)

        # 質問関連
        question_route = RouteDefinition(
            pattern="question",
            target="qa",
            priority=60,
            conditions={
                "type": RouteType.KEYWORD,
                "keywords": [
                    "?",
                    "？",
                    "what",
                    "how",
                    "why",
                    "when",
                    "where",
                    "何",
                    "どう",
                    "なぜ",
                    "いつ",
                    "どこ",
                ],
            },
            description="質問関連のクエリ",
        )
        self._routes.append(question_route)


class LLMBasedRouter(RouterInterface):
    """LLMベースのルーター"""

    def __init__(self, llm_provider: Any = None, default_target: str = "default"):
        """
        LLMベースのルーターを初期化する

        Args:
            llm_provider: LLMプロバイダー
            default_target: デフォルトのターゲット
        """
        self.llm_provider = llm_provider
        self.default_target = default_target
        self._routes: List[RouteDefinition] = []

        logger.info(f"LLMベースのルーターを初期化しました（デフォルトターゲット: {default_target}）")

    async def route(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> RouteResult:
        """
        入力に基づいて適切なAgent/Toolに振り分ける

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            RouteResult: ルーティング結果
        """
        if not self.llm_provider:
            logger.warning("LLMプロバイダーが設定されていません。デフォルトターゲットを返します。")
            return RouteResult(
                target=self.default_target,
                confidence=0.1,
                reasoning="LLMプロバイダーが設定されていない",
            )

        if not input_text.strip():
            return RouteResult(
                target=self.default_target,
                confidence=0.0,
                reasoning="空の入力のためデフォルトターゲットを選択",
            )

        try:
            # LLMにルーティング判定を依頼
            prompt = self._create_routing_prompt(input_text, context or {})
            response = await self.llm_provider.generate(prompt)

            # レスポンスを解析
            result = self._parse_llm_response(response)

            logger.debug(f"LLMルーティング結果: {result.target}（信頼度: {result.confidence:.2f}）")
            return result

        except Exception as e:
            logger.error(f"LLMルーティング中にエラーが発生しました: {e}")
            return RouteResult(
                target=self.default_target,
                confidence=0.1,
                reasoning=f"LLMエラーのためデフォルトを選択: {str(e)}",
            )

    def register_route(self, route: RouteDefinition) -> None:
        """
        新しいルートを登録する

        Args:
            route: 登録するルート定義
        """
        # 既存のルートを削除（同じパターンの場合）
        self.unregister_route(route.pattern)

        # 新しいルートを追加
        self._routes.append(route)

        logger.info(f"ルート '{route.pattern}' を登録しました（ターゲット: {route.target}）")

    def unregister_route(self, pattern: str) -> bool:
        """
        ルートの登録を解除する

        Args:
            pattern: 解除するルートのパターン

        Returns:
            bool: 解除が成功したかどうか
        """
        original_count = len(self._routes)
        self._routes = [r for r in self._routes if r.pattern != pattern]

        removed = len(self._routes) < original_count
        if removed:
            logger.info(f"ルート '{pattern}' の登録を解除しました")

        return removed

    def get_routes(self) -> List[RouteDefinition]:
        """
        登録されているルートの一覧を取得する

        Returns:
            List[RouteDefinition]: ルート定義のリスト
        """
        return self._routes.copy()

    def _create_routing_prompt(self, input_text: str, context: Dict[str, Any]) -> str:
        """ルーティング用のプロンプトを作成する"""
        available_targets = [route.target for route in self._routes]
        if self.default_target not in available_targets:
            available_targets.append(self.default_target)

        prompt = f"""
以下の入力テキストを分析し、最適なターゲットに振り分けてください。

入力テキスト: "{input_text}"

利用可能なターゲット:
{chr(10).join(f"- {target}" for target in available_targets)}

ルート定義:
{chr(10).join(f"- {route.pattern} -> {route.target}" for route in self._routes)}

以下の形式でJSONレスポンスを返してください:
{{
    "target": "選択されたターゲット",
    "confidence": 0.0-1.0の信頼度,
    "reasoning": "判定理由"
}}
"""

        return prompt

    def _parse_llm_response(self, response: str) -> RouteResult:
        """LLMレスポンスを解析する"""
        try:
            import json

            data = json.loads(response.strip())

            return RouteResult(
                target=data.get("target", self.default_target),
                confidence=float(data.get("confidence", 0.5)),
                reasoning=data.get("reasoning", "LLM判定"),
            )

        except (json.JSONDecodeError, ValueError, KeyError) as e:
            logger.warning(f"LLMレスポンスの解析に失敗しました: {e}")
            return RouteResult(
                target=self.default_target, confidence=0.1, reasoning="レスポンス解析エラー"
            )
