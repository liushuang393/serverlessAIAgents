"""
Memory-centric アーキテクチャパターン

このモジュールは、大量の知識ベースを活用したドキュメントQAや
知識検索に特化したエージェントパターンを実装します。
"""

import time
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, List, Optional

from ..core import ChunkerInterface, MemoryInterface, ParserInterface
from ..core.models import MemoryItem, Message, MessageRole, ParsedDocument
from ..utils.logging import get_logger
from .base import Agent

logger = get_logger(__name__)


class SearchStrategy(str, Enum):
    """検索戦略"""

    SEMANTIC = "semantic"  # セマンティック検索
    KEYWORD = "keyword"  # キーワード検索
    HYBRID = "hybrid"  # ハイブリッド検索
    MULTI_STEP = "multi_step"  # 多段階検索


@dataclass
class KnowledgeSource:
    """知識ソースのデータクラス"""

    id: str
    name: str
    description: str
    document_count: int = 0
    last_updated: Optional[float] = None
    metadata: Optional[Dict[str, Any]] = None


class MemoryCentricAgent(Agent):
    """記憶中心型エージェント"""

    def __init__(
        self,
        name: str = "MemoryCentricAgent",
        llm_provider: Any = None,
        memory: Optional[MemoryInterface] = None,
        chunker: Optional[ChunkerInterface] = None,
        parser: Optional[ParserInterface] = None,
        search_strategy: SearchStrategy = SearchStrategy.HYBRID,
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        Memory-centricエージェントを初期化する

        Args:
            name: エージェント名
            llm_provider: LLMプロバイダー
            memory: メモリコンポーネント
            chunker: チャンカーコンポーネント
            parser: パーサーコンポーネント
            search_strategy: 検索戦略
            config: 設定辞書
        """
        super().__init__(name, config)

        if not memory:
            raise ValueError("メモリコンポーネントが必要です")

        self.llm = llm_provider
        self.memory: MemoryInterface = memory  # 型ヒントを明確化
        self.chunker = chunker
        self.parser = parser
        self.search_strategy = search_strategy
        self.config = config if config is not None else {}
        # 設定値
        self.max_search_results = self.config.get("max_search_results", 10)
        self.similarity_threshold = self.config.get("similarity_threshold", 0.7)
        self.context_window_size = self.config.get("context_window_size", 4000)
        self.enable_source_citation = self.config.get("enable_source_citation", True)
        self.rerank_results = self.config.get("rerank_results", True)
        # 知識ソース管理
        self.knowledge_sources: Dict[str, KnowledgeSource] = {}

        logger.info(
            f"MemoryCentricAgent '{self.name}' を初期化しました (strategy: {search_strategy})"
        )

    async def add_knowledge_source(
        self,
        source_id: str,
        name: str,
        documents: List[Any],
        description: str = "",
        metadata: Optional[Dict[str, Any]] = None,
    ) -> None:
        """
        知識ソースを追加する

        Args:
            source_id: ソースの一意識別子
            name: ソース名
            documents: ドキュメントのリスト（ファイルパス、URL、テキスト等）
            description: ソースの説明
            metadata: 追加のメタデータ
        """
        logger.info(f"知識ソース '{name}' を追加中...")
        start_time = time.time()

        document_count = 0

        for doc in documents:
            try:
                # ドキュメントをパース
                if self.parser:
                    # docの型に応じてcontent_typeを推定
                    if isinstance(doc, bytes):
                        content = doc
                        content_type = "application/octet-stream"  # デフォルト
                    elif isinstance(doc, str):
                        # 文字列の場合はバイト列に変換
                        content = doc.encode("utf-8")
                        # 拡張子やコンテンツから推定
                        if doc.strip().startswith("<"):
                            content_type = "text/html"
                        elif doc.strip().startswith("{") or doc.strip().startswith("["):
                            content_type = "application/json"
                        else:
                            content_type = "text/plain"
                    else:
                        # その他の場合は文字列として扱う
                        content = str(doc).encode("utf-8")
                        content_type = "text/plain"

                    parsed_doc = await self.parser.parse(content, content_type)
                else:
                    # パーサーがない場合は文字列として扱う
                    parsed_doc = ParsedDocument(
                        text=str(doc),
                        metadata={"source_id": source_id},
                        source_type="text",
                        chunks=None,
                    )

                # テキストをチャンク化
                if self.chunker:
                    text_chunks = await self.chunker.chunk(parsed_doc.text)
                    chunk_texts = [chunk.text for chunk in text_chunks]
                else:
                    # チャンカーがない場合は全体を1つのチャンクとして扱う
                    chunk_texts = [parsed_doc.text]

                # 各チャンクをメモリに保存
                for i, chunk_content in enumerate(chunk_texts):
                    chunk_metadata = {
                        "source_id": source_id,
                        "source_name": name,
                        "document_index": document_count,
                        "chunk_index": i,
                        "total_chunks": len(chunk_texts),
                        **(metadata or {}),
                        **(parsed_doc.metadata or {}),
                    }

                    if self.memory is None:
                        raise ValueError("メモリコンポーネントが設定されていません")
                    await self.memory.store(chunk_content, chunk_metadata)

                document_count += 1

            except Exception as e:
                logger.error(f"ドキュメント処理エラー: {e}")
                continue

        # 知識ソースを登録
        self.knowledge_sources[source_id] = KnowledgeSource(
            id=source_id,
            name=name,
            description=description,
            document_count=document_count,
            last_updated=time.time(),
            metadata=metadata,
        )

        processing_time = time.time() - start_time
        logger.info(
            f"知識ソース '{name}' を追加完了 "
            f"({document_count} ドキュメント, {processing_time:.2f}秒)"
        )

    async def process(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        入力を処理して知識ベースを活用した応答を生成する

        Args:
            input_text: 入力テキスト（質問）
            context: 追加のコンテキスト情報

        Returns:
            str: 処理結果
        """
        context = context or {}
        start_time = time.time()

        try:
            # 1. 関連する知識を検索
            relevant_memories = await self._search_knowledge(input_text, context)

            if not relevant_memories:
                return "申し訳ありませんが、関連する情報が見つかりませんでした。"

            # 2. 検索結果を再ランク（必要に応じて）
            if self.rerank_results:
                relevant_memories = await self._rerank_results(
                    input_text, relevant_memories
                )

            # 3. コンテキストを構築
            context_text = self._build_context(relevant_memories)

            # 4. LLMで回答を生成
            response = await self._generate_answer(
                input_text, context_text, relevant_memories
            )

            processing_time = time.time() - start_time
            logger.info(
                f"知識ベース検索完了 "
                f"({len(relevant_memories)} 件の関連情報, {processing_time:.2f}秒)"
            )

            return response

        except Exception as e:
            logger.error(f"Memory-centric処理エラー: {e}")
            return f"申し訳ありませんが、処理中にエラーが発生しました: {str(e)}"

    async def _search_knowledge(
        self, query: str, context: Dict[str, Any]
    ) -> List[MemoryItem]:
        """
        知識ベースを検索する

        Args:
            query: 検索クエリ
            context: コンテキスト

        Returns:
            List[MemoryItem]: 関連する記憶アイテムのリスト
        """
        if self.search_strategy == SearchStrategy.SEMANTIC:
            return await self._semantic_search(query, context)
        elif self.search_strategy == SearchStrategy.KEYWORD:
            return await self._keyword_search(query, context)
        elif self.search_strategy == SearchStrategy.HYBRID:
            return await self._hybrid_search(query, context)
        elif self.search_strategy == SearchStrategy.MULTI_STEP:
            return await self._multi_step_search(query, context)
        else:
            raise ValueError(f"サポートされていない検索戦略: {self.search_strategy}")

    async def _semantic_search(
        self, query: str, context: Dict[str, Any]
    ) -> List[MemoryItem]:
        """セマンティック検索を実行する"""
        return await self.memory.search(
            query=query,
            limit=self.max_search_results,
            threshold=self.similarity_threshold,
        )

    async def _keyword_search(
        self, query: str, context: Dict[str, Any]
    ) -> List[MemoryItem]:
        """キーワード検索を実行する（簡易実装）"""
        # 全ての記憶を取得してキーワードマッチング
        # 実際の実装では、より効率的な全文検索エンジンを使用することを推奨
        all_memories = await self.memory.search(query="", limit=1000, threshold=0.0)

        keywords = query.lower().split()
        matched_memories = []

        for memory in all_memories:
            content_lower = memory.content.lower()
            score = sum(1 for keyword in keywords if keyword in content_lower)

            if score > 0:
                # キーワードマッチスコアを類似度として設定
                memory.similarity_score = score / len(keywords)
                matched_memories.append(memory)

        # スコア順でソート（Noneの場合は0.0として扱う）
        matched_memories.sort(
            key=lambda m: m.similarity_score if m.similarity_score is not None else 0.0,
            reverse=True,
        )

        return matched_memories[: self.max_search_results]

    async def _hybrid_search(
        self, query: str, context: Dict[str, Any]
    ) -> List[MemoryItem]:
        """ハイブリッド検索を実行する"""
        # セマンティック検索とキーワード検索の結果を組み合わせ
        semantic_results = await self._semantic_search(query, context)
        keyword_results = await self._keyword_search(query, context)

        # 結果をマージして重複を除去
        combined_results = {}

        # セマンティック検索結果（重み: 0.7）
        for memory in semantic_results:
            combined_results[memory.id] = memory
            # similarity_scoreがNoneの場合は0.0として扱う
            if memory.similarity_score is not None:
                memory.similarity_score = memory.similarity_score * 0.7
            else:
                memory.similarity_score = 0.0

        # キーワード検索結果（重み: 0.3）
        for memory in keyword_results:
            if memory.id in combined_results:
                # 既存の結果にスコアを加算
                # similarity_scoreがNoneの場合は0.0として扱う
                keyword_score = (
                    memory.similarity_score * 0.3
                    if memory.similarity_score is not None
                    else 0.0
                )
                existing_score = combined_results[memory.id].similarity_score
                if existing_score is not None:
                    combined_results[memory.id].similarity_score = (
                        existing_score + keyword_score
                    )
                else:
                    combined_results[memory.id].similarity_score = keyword_score
            else:
                # similarity_scoreがNoneの場合は0.0として扱う
                memory.similarity_score = (
                    memory.similarity_score * 0.3
                    if memory.similarity_score is not None
                    else 0.0
                )
                combined_results[memory.id] = memory

        # スコア順でソート（Noneの場合は0.0として扱う）
        final_results = list(combined_results.values())
        final_results.sort(
            key=lambda m: m.similarity_score if m.similarity_score is not None else 0.0,
            reverse=True,
        )

        return final_results[: self.max_search_results]

    async def _multi_step_search(
        self, query: str, context: Dict[str, Any]
    ) -> List[MemoryItem]:
        """多段階検索を実行する"""
        # 1段階目: 初期検索
        initial_results = await self._semantic_search(query, context)

        if not initial_results:
            return []

        # 2段階目: 初期結果から関連キーワードを抽出して再検索
        related_content = " ".join([memory.content for memory in initial_results[:3]])

        if self.llm:
            # LLMを使用して関連キーワードを生成
            messages = [
                Message(
                    role=MessageRole.SYSTEM,
                    content="以下のテキストから、検索に有用なキーワードを3-5個抽出してください。",
                ),
                Message(
                    role=MessageRole.USER,
                    content=f"元の質問: {query}\n\n関連テキスト: {related_content[:1000]}",
                ),
            ]

            response = await self.llm.generate(messages)
            expanded_query = f"{query} {response.content}"
        else:
            expanded_query = query

        # 拡張クエリで再検索
        expanded_results = await self._semantic_search(expanded_query, context)

        # 結果をマージ
        combined_results = {}
        for memory in initial_results + expanded_results:
            if memory.id not in combined_results:
                combined_results[memory.id] = memory

        final_results = list(combined_results.values())
        final_results.sort(
            key=lambda m: m.similarity_score if m.similarity_score is not None else 0.0,
            reverse=True,
        )

        return final_results[: self.max_search_results]

    async def _rerank_results(
        self, query: str, memories: List[MemoryItem]
    ) -> List[MemoryItem]:
        """検索結果を再ランクする"""
        if not self.llm or len(memories) <= 1:
            return memories

        # LLMを使用して関連度を再評価
        reranked_memories = []

        for memory in memories:
            messages = [
                Message(
                    role=MessageRole.SYSTEM,
                    content="以下の質問に対するテキストの関連度を0.0-1.0で評価してください。数値のみ回答してください。",
                ),
                Message(
                    role=MessageRole.USER,
                    content=f"質問: {query}\n\nテキスト: {memory.content[:500]}",
                ),
            ]

            try:
                response = await self.llm.generate(messages)
                relevance_score = float(response.content.strip())
                memory.similarity_score = relevance_score
                reranked_memories.append(memory)
            except (ValueError, Exception):
                # スコア解析に失敗した場合は元のスコアを維持
                reranked_memories.append(memory)

        # 再ランクされたスコアでソート（Noneの場合は0.0として扱う）
        reranked_memories.sort(
            key=lambda m: m.similarity_score if m.similarity_score is not None else 0.0,
            reverse=True,
        )

        return reranked_memories

    def _build_context(self, memories: List[MemoryItem]) -> str:
        """検索結果からコンテキストを構築する"""
        context_parts = []
        current_length = 0

        for i, memory in enumerate(memories):
            # ソース情報を含める
            source_info = ""
            if self.enable_source_citation and memory.metadata:
                source_name = memory.metadata.get("source_name", "不明")
                source_info = f" [出典: {source_name}]"

            content_with_source = f"{memory.content}{source_info}"

            # コンテキストウィンドウサイズを超えないようにチェック
            if current_length + len(content_with_source) > self.context_window_size:
                break

            context_parts.append(f"[参考情報 {i+1}]\n{content_with_source}")
            current_length += len(content_with_source)

        return "\n\n".join(context_parts)

    async def _generate_answer(
        self, question: str, context: str, memories: List[MemoryItem]
    ) -> str:
        """コンテキストを使用して回答を生成する"""
        if not self.llm:
            # LLMがない場合は検索結果をそのまま返す
            return context

        messages = [
            Message(
                role=MessageRole.SYSTEM,
                content="""あなたは知識ベースを活用して正確な回答を提供するアシスタントです。
以下の参考情報を基に、質問に対して正確で有用な回答を生成してください。

回答の際は：
1. 参考情報に基づいて回答してください
2. 情報が不足している場合はその旨を明記してください
3. 可能な限り具体的で詳細な回答を心がけてください
4. 出典がある場合は適切に引用してください""",
            ),
            Message(
                role=MessageRole.USER,
                content=f"""質問: {question}

参考情報:
{context}

上記の参考情報を基に、質問に回答してください。""",
            ),
        ]

        response = await self.llm.generate(messages)
        return str(response.content)

    def get_knowledge_sources(self) -> Dict[str, KnowledgeSource]:
        """登録されている知識ソースを取得する"""
        return self.knowledge_sources.copy()

    async def get_statistics(self) -> Dict[str, Any]:
        """統計情報を取得する"""
        total_memories = await self.memory.count()

        return {
            "total_memories": total_memories,
            "knowledge_sources": len(self.knowledge_sources),
            "search_strategy": self.search_strategy.value,
            "max_search_results": self.max_search_results,
            "similarity_threshold": self.similarity_threshold,
        }
