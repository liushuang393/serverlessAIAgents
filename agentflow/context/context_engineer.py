"""Context Engineer - 統合コンテキストエンジニアリング.

Context Engineering の全コンポーネントを統合し、
単一インターフェースで上下文管理を提供する。

設計原則:
- 統合API: 全コンポーネントを一つのインターフェースで利用
- 自動最適化: 予算・圧縮・選択を自動で最適化
- 設定可能: 細かい挙動はConfigで調整可能
- 疎結合: 各コンポーネントは独立して使用可能

使用例:
    >>> engineer = ContextEngineer()
    >>> await engineer.start()
    >>>
    >>> # メッセージ追加（自動圧縮）
    >>> engineer.add_message("user", "こんにちは")
    >>>
    >>> # コンテキスト構築
    >>> context = await engineer.build_context(
    ...     query="APIの仕様を教えて",
    ...     available_tools=all_tools,
    ... )
    >>>
    >>> # context.system_prompt  -> 予算内のプロンプト
    >>> # context.tools         -> 関連Top-7ツール
    >>> # context.rag_results   -> 必要時のみ検索結果
    >>> # context.messages      -> 圧縮済みメッセージ
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any

from agentflow.context.budget_manager import (
    BudgetConfig,
    TokenBudgetManager,
)
from agentflow.context.key_notes import (
    KeyNotesStore,
    NoteImportance,
)
from agentflow.context.key_notes import (
    StoreConfig as NotesConfig,
)
from agentflow.context.retrieval_gate import (
    GateConfig,
    RetrievalGate,
)
from agentflow.context.tool_selector import (
    SelectionConfig,
    ToolRelevanceSelector,
)
from agentflow.context.turn_compressor import (
    TurnBasedCompressor,
    TurnConfig,
)


if TYPE_CHECKING:
    from agentflow.providers.tool_provider import RegisteredTool

_logger = logging.getLogger(__name__)


@dataclass
class ContextConfig:
    """コンテキストエンジニア設定.

    Attributes:
        budget_config: Token予算設定
        tool_config: ツール選択設定
        gate_config: 検索判定設定
        notes_config: KeyNotes設定
        turn_config: ターン圧縮設定
        auto_compress: 自動圧縮有効化
        auto_extract_notes: 自動Note抽出有効化
    """

    budget_config: BudgetConfig = field(default_factory=BudgetConfig)
    tool_config: SelectionConfig = field(default_factory=SelectionConfig)
    gate_config: GateConfig = field(default_factory=GateConfig)
    notes_config: NotesConfig = field(default_factory=NotesConfig)
    turn_config: TurnConfig = field(default_factory=TurnConfig)
    auto_compress: bool = True
    auto_extract_notes: bool = True


@dataclass
class BuiltContext:
    """構築されたコンテキスト.

    Attributes:
        system_prompt: システムプロンプト（予算内）
        tools: 選択されたツール（関連Top-K）
        rag_results: RAG検索結果（必要時のみ）
        messages: メッセージ履歴（圧縮済み）
        key_notes: 重要Notes
        metadata: メタデータ
    """

    system_prompt: str
    tools: list[Any]  # RegisteredTool
    rag_results: list[dict[str, Any]] | None
    messages: list[dict[str, str]]
    key_notes: str
    metadata: dict[str, Any] = field(default_factory=dict)


class ContextEngineer:
    """統合コンテキストエンジニア.

    Context Engineering の全機能を統合し、
    上下文を注意力予算として最適に管理する。

    Example:
        >>> # 初期化
        >>> engineer = ContextEngineer(
        ...     config=ContextConfig(
        ...         budget_config=BudgetConfig(system_prompt_budget=500),
        ...         tool_config=SelectionConfig(max_tools=7),
        ...     ),
        ... )
        >>> await engineer.start()
        >>>
        >>> # メッセージ追加
        >>> engineer.add_message("user", "APIの仕様を教えて")
        >>>
        >>> # コンテキスト構築
        >>> context = await engineer.build_context(
        ...     query="APIの仕様を教えて",
        ...     base_prompt="あなたは親切なアシスタントです",
        ...     available_tools=tool_provider.list_tools(),
        ...     rag_search_func=rag_pipeline.search,
        ... )
        >>>
        >>> # LLM呼び出しに使用
        >>> response = await llm.chat(
        ...     messages=[
        ...         {"role": "system", "content": context.system_prompt},
        ...         *context.messages,
        ...     ],
        ...     tools=context.tools,
        ... )
    """

    def __init__(
        self,
        config: ContextConfig | None = None,
        embedding_provider: Any | None = None,
        llm_client: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            config: コンテキスト設定
            embedding_provider: 埋め込みプロバイダ（ツール選択用）
            llm_client: LLMクライアント（要約・抽出用）
        """
        self._config = config or ContextConfig()
        self._embedding = embedding_provider
        self._llm = llm_client

        # コンポーネント初期化
        self._budget_manager = TokenBudgetManager(
            config=self._config.budget_config,
        )
        self._tool_selector = ToolRelevanceSelector(
            config=self._config.tool_config,
            embedding_provider=embedding_provider,
        )
        self._retrieval_gate = RetrievalGate(
            config=self._config.gate_config,
        )
        self._key_notes = KeyNotesStore(
            config=self._config.notes_config,
        )
        self._compressor = TurnBasedCompressor(
            config=self._config.turn_config,
            key_notes_store=self._key_notes,
        )

        self._started = False
        self._logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """エンジニアを開始."""
        self._started = True
        self._logger.info("ContextEngineer started")

    async def stop(self) -> None:
        """エンジニアを停止."""
        self._started = False
        self._logger.info("ContextEngineer stopped")

    def add_message(
        self,
        role: str,
        content: str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """メッセージを追加.

        自動圧縮が有効な場合、閾値超過時に圧縮を実行。

        Args:
            role: メッセージロール
            content: メッセージ内容
            metadata: メタデータ
        """
        self._compressor.add_message(role, content, metadata)

    async def build_context(
        self,
        query: str,
        base_prompt: str = "",
        skills: list[Any] | None = None,
        available_tools: list[RegisteredTool] | None = None,
        rag_search_func: Any | None = None,
        existing_context: dict[str, Any] | None = None,
    ) -> BuiltContext:
        """最適化されたコンテキストを構築.

        Context Engineering 原則に基づき:
        1. システムプロンプトを予算内に収める
        2. 関連ツールをTop-Kに絞る
        3. RAG検索の必要性を判定
        4. メッセージを圧縮
        5. KeyNotesを注入

        Args:
            query: ユーザークエリ
            base_prompt: 基本システムプロンプト
            skills: スキルリスト
            available_tools: 利用可能ツール
            rag_search_func: RAG検索関数
            existing_context: 既存コンテキスト

        Returns:
            構築されたコンテキスト
        """
        # 予算管理器をリセット
        self._budget_manager.reset_usage()

        # 1. 自動圧縮チェック
        if self._config.auto_compress and self._compressor.should_compress():
            await self._compressor.compress()

        # 2. システムプロンプト構築
        system_prompt = await self._build_system_prompt(base_prompt, skills)

        # 3. ツール選択
        selected_tools: list[Any] = []
        if available_tools:
            selected_tools = await self._select_tools(query, available_tools)

        # 4. RAG検索判定・実行
        rag_results = None
        if rag_search_func:
            rag_results = await self._maybe_retrieve(query, rag_search_func, existing_context)

        # 5. RAG結果をシステムプロンプトに追加
        if rag_results:
            rag_context = self._format_rag_results(rag_results)
            system_prompt = f"{system_prompt}\n\n{rag_context}"

        # 6. KeyNotesをシステムプロンプトに追加
        notes_context = self._key_notes.to_context_string(
            max_tokens=self._config.budget_config.key_notes_budget,
            min_importance=NoteImportance.MEDIUM,
        )
        if notes_context:
            system_prompt = f"{system_prompt}\n\n{notes_context}"

        # 7. メッセージ取得
        messages = self._compressor.get_messages_as_dicts()

        # 8. メタデータ構築
        metadata = self._build_metadata(query, selected_tools, rag_results)

        return BuiltContext(
            system_prompt=system_prompt,
            tools=selected_tools,
            rag_results=rag_results,
            messages=messages,
            key_notes=notes_context,
            metadata=metadata,
        )

    async def _build_system_prompt(
        self,
        base_prompt: str,
        skills: list[Any] | None,
    ) -> str:
        """システムプロンプトを構築.

        Args:
            base_prompt: 基本プロンプト
            skills: スキルリスト

        Returns:
            予算内のシステムプロンプト
        """
        allocation = self._budget_manager.allocate_system_prompt(
            base_prompt=base_prompt,
            skills=skills,
        )

        if allocation.truncated:
            self._logger.debug(
                "システムプロンプト切り詰め: %d -> %d tokens",
                allocation.original_token_count,
                allocation.token_count,
            )

        return allocation.content

    async def _select_tools(
        self,
        query: str,
        all_tools: list[RegisteredTool],
    ) -> list[RegisteredTool]:
        """関連ツールを選択.

        Args:
            query: クエリ
            all_tools: 全ツール

        Returns:
            選択されたツール（Top-K）
        """
        selected = await self._tool_selector.select_relevant_tools(
            query=query,
            all_tools=all_tools,
            max_tools=self._config.tool_config.max_tools,
        )

        self._logger.debug(
            "ツール選択: %d -> %d",
            len(all_tools),
            len(selected),
        )

        return selected

    async def _maybe_retrieve(
        self,
        query: str,
        search_func: Any,
        existing_context: dict[str, Any] | None,
    ) -> list[dict[str, Any]] | None:
        """必要に応じてRAG検索を実行.

        Args:
            query: クエリ
            search_func: 検索関数
            existing_context: 既存コンテキスト

        Returns:
            検索結果、または None
        """
        # 検索必要性判定
        decision = await self._retrieval_gate.should_retrieve(
            query=query,
            context=existing_context or {},
        )

        if not decision.should_retrieve:
            self._logger.debug(
                "RAG検索スキップ: reason=%s, confidence=%.2f",
                decision.reason.value,
                decision.confidence,
            )
            return None

        # 検索実行
        search_query = decision.suggested_query or query
        try:
            results = await search_func(search_query, top_k=5)

            # Top 1-2 のみ使用
            limited_results = results[:2] if results else []

            self._logger.debug(
                "RAG検索実行: query='%s', results=%d -> %d",
                search_query[:30],
                len(results) if results else 0,
                len(limited_results),
            )

            return limited_results
        except Exception as e:
            self._logger.warning("RAG検索に失敗: %s", e)
            return None

    def _format_rag_results(self, results: list[dict[str, Any]]) -> str:
        """RAG結果をフォーマット.

        Args:
            results: 検索結果

        Returns:
            フォーマットされた文字列
        """
        if not results:
            return ""

        allocation = self._budget_manager.allocate_rag_context(
            chunks=[r.get("content", r.get("document", str(r))) for r in results],
            max_chunks=2,
        )

        if allocation.content:
            return f"# 参考情報\n{allocation.content}"
        return ""

    def _build_metadata(
        self,
        query: str,
        tools: list[Any],
        rag_results: list[dict[str, Any]] | None,
    ) -> dict[str, Any]:
        """メタデータを構築.

        Args:
            query: クエリ
            tools: 選択ツール
            rag_results: RAG結果

        Returns:
            メタデータ
        """
        return {
            "query": query,
            "tool_count": len(tools),
            "rag_used": rag_results is not None,
            "rag_result_count": len(rag_results) if rag_results else 0,
            "turn_count": self._compressor.get_turn_count(),
            "budget_usage": self._budget_manager.get_usage_summary(),
            "key_notes_count": len(self._key_notes.get_all_notes()),
        }

    # =========================================================================
    # 便利メソッド
    # =========================================================================

    def add_domain_keywords(self, keywords: list[str]) -> None:
        """ドメインキーワードを追加.

        Args:
            keywords: キーワードリスト
        """
        self._retrieval_gate.add_domain_keywords(keywords)

    async def remember(
        self,
        content: str,
        source: str = "",
    ) -> None:
        """重要情報を記憶.

        Args:
            content: 記憶する内容
            source: 情報源
        """
        await self._key_notes.extract_and_store(content, source)

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        return {
            "compressor": self._compressor.get_stats(),
            "budget": self._budget_manager.get_usage_summary(),
            "key_notes": len(self._key_notes.get_all_notes()),
        }

    def reset(self) -> None:
        """状態をリセット."""
        self._compressor.reset()
        self._key_notes.clear()
        self._budget_manager.reset_usage()
        self._tool_selector.clear_cache()

    # =========================================================================
    # コンポーネントアクセス（上級者向け）
    # =========================================================================

    @property
    def budget_manager(self) -> TokenBudgetManager:
        """予算管理器を取得."""
        return self._budget_manager

    @property
    def tool_selector(self) -> ToolRelevanceSelector:
        """ツール選択器を取得."""
        return self._tool_selector

    @property
    def retrieval_gate(self) -> RetrievalGate:
        """検索判定ゲートを取得."""
        return self._retrieval_gate

    @property
    def key_notes(self) -> KeyNotesStore:
        """KeyNotesストアを取得."""
        return self._key_notes

    @property
    def compressor(self) -> TurnBasedCompressor:
        """圧縮器を取得."""
        return self._compressor
