"""社内KB Agent - RBAC制御 + 保守モード.

社内向け知識ベース専用のAgent。
権限制御と規則類に対する保守的回答を実現。

設計原則:
- RBAC/ABAC による厳格な権限制御
- 規則類は保守モード（直接摘録優先）
- 必須引用（来源・バージョン・更新日時）
- 不確定時は確認を促す + 工単生成

使用例:
    >>> from apps.faq_system.backend.agents import InternalKBAgent
    >>>
    >>> agent = InternalKBAgent()
    >>> result = await agent.run({
    ...     "question": "年次有給休暇は何日もらえますか？",
    ...     "user_context": {"user_id": "123", "role": "employee"},
    ... })
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from agentflow.core import ResilientAgent
from agentflow.integrations.ticket_generator import TicketGenerator
from agentflow.knowledge.isolated_kb import IsolatedKBManager, KBType
from agentflow.security.policy_engine import AuthContext, AuthMode, PolicyEngine


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


@dataclass
class InternalKBConfig:
    """社内KB Agent 設定."""

    # KB設定
    collection: str = "internal_kb"
    top_k: int = 5
    min_similarity: float = 0.3

    # 保守モード（規則類）
    conservative_mode: bool = True
    conservative_keywords: list[str] = field(
        default_factory=lambda: [
            "規則",
            "規程",
            "ポリシー",
            "制度",
            "規定",
            "手続き",
            "申請",
            "就業",
            "休暇",
            "給与",
            "人事",
            "福利厚生",
            "セキュリティ",
        ]
    )

    # 引用設定
    require_citation: bool = True
    include_version: bool = True
    include_update_date: bool = True
    include_page_number: bool = True

    # 不確定時の動作
    uncertainty_threshold: float = 0.6
    auto_generate_ticket: bool = True

    # LLM設定
    temperature: float = 0.2


class Citation(BaseModel):
    """引用情報."""

    index: int = 0
    document_id: str = ""
    title: str = ""
    source: str = ""
    version: str = ""
    update_date: str = ""
    effective_date: str | None = None
    page_number: int | None = None
    section: str = ""
    snippet: str = ""
    relevance_score: float = 0.0
    owner_department: str = ""
    applicable_scope: str = ""


class InternalKBResponse(BaseModel):
    """社内KB Agent レスポンス."""

    question: str = ""
    answer: str = ""
    query_type: str = "faq"  # faq, rule, hybrid
    confidence: float = 0.0

    # 引用
    citations: list[Citation] = Field(default_factory=list)

    # 不確定時
    needs_confirmation: bool = False
    suggested_contacts: list[dict[str, str]] = Field(default_factory=list)
    ticket_id: str | None = None

    # メタデータ
    execution_time_ms: float = 0
    conservative_mode_used: bool = False
    error: str = ""


# =============================================================================
# 社内KB Agent
# =============================================================================


class InternalKBAgent(ResilientAgent):
    """社内KB Agent（RBAC制御 + 保守モード）.

    社内向け知識ベース専用のAgent。

    特徴:
    - RBAC/ABAC 権限制御
    - 規則類は保守モード（直接摘録優先）
    - 必須引用
    - 不確定時の工単自動生成
    """

    name = "InternalKBAgent"

    # システムプロンプト（通常モード）
    SYSTEM_PROMPT = """あなたは企業内部の知識ベース専門アシスタントです。

職責:
1. 社内規則・制度に関する質問に正確に回答する
2. 回答には必ずソースを引用する（[1]、[2]等）
3. 不明な点は正直に「確認が必要です」と伝える

回答ルール:
- 簡潔で正確な回答を心がける
- ソースを明示する
- 推測や憶測を避ける
- 追加の質問を促す提案を行う"""

    # システムプロンプト（保守モード）
    CONSERVATIVE_SYSTEM_PROMPT = """あなたは企業内部の規則・制度専門アシスタントです。

職責:
1. 規則・制度に関する質問に正確に回答する
2. 回答は必ず原文から直接摘録する
3. 自由な解釈や要約を避ける

回答ルール:
- 原文をそのまま引用する
- 引用元を明示する（[1]、[2]等）
- 解釈が必要な場合は「確認が必要です」と伝える
- 複数の解釈が可能な場合は全て提示する"""

    def __init__(
        self,
        config: InternalKBConfig | None = None,
        kb_manager: IsolatedKBManager | None = None,
        ticket_generator: TicketGenerator | None = None,
        policy_engine: PolicyEngine | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
            kb_manager: KB マネージャー
            ticket_generator: 工単生成器
            policy_engine: ポリシーエンジン
        """
        super().__init__()
        self._config = config or InternalKBConfig()
        self._kb_manager = kb_manager
        self._ticket_generator = ticket_generator or TicketGenerator()
        self._policy_engine = policy_engine or PolicyEngine()
        self._logger = logging.getLogger(self.name)
        self._initialized = False

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent 実行.

        Args:
            input_data: 入力データ

        Returns:
            InternalKBResponse の dict
        """
        start_time = datetime.now()
        question = input_data.get("question", "")
        user_context = input_data.get("user_context", {})

        if not question:
            return InternalKBResponse(error="質問が指定されていません").model_dump()

        await self._ensure_initialized()

        try:
            # 1. 権限チェック
            auth_result = await self._check_permission(user_context)
            if not auth_result["allowed"]:
                return InternalKBResponse(
                    question=question,
                    answer="申し訳ありません。このコンテンツへのアクセス権限がありません。",
                    error=auth_result.get("reason", "Permission denied"),
                ).model_dump()

            # 2. クエリ分類（規則類か判定）
            is_rule_query = self._is_rule_query(question)

            # 3. 検索実行
            search_results = await self._search(question, user_context)

            # 4. 回答生成
            if is_rule_query and self._config.conservative_mode:
                response = await self._generate_conservative_answer(question, search_results)
                response.conservative_mode_used = True
            else:
                response = await self._generate_answer(question, search_results)

            response.question = question
            response.query_type = "rule" if is_rule_query else "faq"

            # 5. 不確定判定
            if response.confidence < self._config.uncertainty_threshold:
                response = await self._handle_uncertainty(question, response, user_context)

            # 実行時間
            response.execution_time_ms = (datetime.now() - start_time).total_seconds() * 1000

            return response.model_dump()

        except Exception as e:
            self._logger.exception("InternalKBAgent エラー: %s", e)
            return InternalKBResponse(
                question=question,
                error=str(e),
                execution_time_ms=(datetime.now() - start_time).total_seconds() * 1000,
            ).model_dump()

    async def run_stream(self, input_data: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行.

        Args:
            input_data: 入力データ

        Yields:
            進捗イベント
        """
        question = input_data.get("question", "")

        yield {
            "type": "progress",
            "progress": 0,
            "message": f"「{question[:20]}」を検索中...",
        }

        await self._ensure_initialized()

        yield {
            "type": "progress",
            "progress": 20,
            "message": "権限を確認中...",
        }

        yield {
            "type": "progress",
            "progress": 40,
            "message": "知識ベースを検索中...",
        }

        yield {
            "type": "progress",
            "progress": 70,
            "message": "回答を生成中...",
        }

        result = await self.run(input_data)

        yield {
            "type": "progress",
            "progress": 100,
            "message": "完了",
        }

        yield {
            "type": "result",
            "data": result,
        }

    async def _check_permission(self, user_context: dict[str, Any]) -> dict[str, Any]:
        """権限チェック.

        Args:
            user_context: ユーザーコンテキスト

        Returns:
            {"allowed": bool, "reason": str}
        """
        if not user_context.get("user_id"):
            return {"allowed": False, "reason": "認証が必要です"}

        auth_context = AuthContext(
            subject={
                "user_id": user_context.get("user_id", ""),
                "role": user_context.get("role", "guest"),
                "department": user_context.get("department", ""),
            },
            resource={
                "type": "kb",
                "kb_type": "internal",
            },
            action="read",
        )

        result = await self._policy_engine.authorize(
            context=auth_context,
            mode=AuthMode.ABAC,
        )

        return {
            "allowed": result.allowed,
            "reason": result.reason,
        }

    def _is_rule_query(self, question: str) -> bool:
        """規則類クエリか判定.

        Args:
            question: 質問

        Returns:
            規則類の場合True
        """
        question_lower = question.lower()
        return any(keyword in question_lower for keyword in self._config.conservative_keywords)

    async def _search(
        self,
        question: str,
        user_context: dict[str, Any],
    ) -> list[dict[str, Any]]:
        """検索実行.

        Args:
            question: 質問
            user_context: ユーザーコンテキスト

        Returns:
            検索結果リスト
        """
        if self._kb_manager:
            results = await self._kb_manager.search(
                kb_type=KBType.INTERNAL,
                query=question,
                user_context=user_context,
                top_k=self._config.top_k,
            )
            return [
                {
                    "document_id": r.document_id,
                    "content": r.content,
                    "score": r.score,
                    "metadata": r.metadata,
                    "citation": r.citation,
                }
                for r in results
            ]

        # KB マネージャーがない場合のプレースホルダー
        return []

    async def _generate_answer(
        self,
        question: str,
        search_results: list[dict[str, Any]],
    ) -> InternalKBResponse:
        """通常モード回答生成.

        Args:
            question: 質問
            search_results: 検索結果

        Returns:
            レスポンス
        """
        if not search_results:
            return InternalKBResponse(
                answer="申し訳ありません。関連する情報が見つかりませんでした。",
                confidence=0.0,
            )

        # コンテキストを構築
        context_parts = []
        citations = []

        for i, result in enumerate(search_results[:5]):
            context_parts.append(f"[{i + 1}] {result['content']}")
            citations.append(
                Citation(
                    index=i + 1,
                    document_id=result.get("document_id", ""),
                    title=result.get("citation", {}).get("title", ""),
                    source=result.get("citation", {}).get("source", ""),
                    version=result.get("citation", {}).get("version", ""),
                    update_date=result.get("citation", {}).get("update_date", ""),
                    snippet=result.get("content", "")[:200],
                    relevance_score=result.get("score", 0.0),
                )
            )

        context = "\n\n".join(context_parts)

        # LLM で回答生成
        if self._llm is not None:
            prompt_messages = [
                {"role": "system", "content": self.SYSTEM_PROMPT},
                {"role": "user", "content": f"以下のコンテキストを参照して質問に回答してください。\n\nコンテキスト:\n{context}\n\n質問: {question}"},
            ]
            llm_response = await self._llm.chat(prompt_messages)
            answer = llm_response.get("content") or "回答を生成できませんでした。"
        else:
            answer = f"参考情報 [1] に基づくと、{search_results[0]['content'][:100]}..."

        return InternalKBResponse(
            answer=answer,
            confidence=min(
                search_results[0].get("score", 0.5) if search_results else 0.0,
                0.9,
            ),
            citations=citations,
        )

    async def _generate_conservative_answer(
        self,
        question: str,
        search_results: list[dict[str, Any]],
    ) -> InternalKBResponse:
        """保守モード回答生成（直接摘録優先）.

        Args:
            question: 質問
            search_results: 検索結果

        Returns:
            レスポンス
        """
        if not search_results:
            return InternalKBResponse(
                answer="申し訳ありません。関連する規則・制度情報が見つかりませんでした。担当部門にお問い合わせください。",
                confidence=0.0,
                needs_confirmation=True,
            )

        # 最も関連性の高い結果を直接引用
        top_result = search_results[0]
        content = top_result.get("content", "")

        citations = [
            Citation(
                index=1,
                document_id=top_result.get("document_id", ""),
                title=top_result.get("citation", {}).get("title", ""),
                source=top_result.get("citation", {}).get("source", ""),
                version=top_result.get("citation", {}).get("version", ""),
                update_date=top_result.get("citation", {}).get("update_date", ""),
                snippet=content[:300],
                relevance_score=top_result.get("score", 0.0),
            )
        ]

        # LLM で回答生成（CONSERVATIVE_SYSTEM_PROMPT 使用）
        if self._llm is not None:
            prompt_messages = [
                {"role": "system", "content": self.CONSERVATIVE_SYSTEM_PROMPT},
                {"role": "user", "content": f"以下の規則・制度情報を参照して質問に回答してください。\n\nコンテキスト:\n[1] {content}\n\n質問: {question}"},
            ]
            llm_response = await self._llm.chat(prompt_messages)
            answer = llm_response.get("content") or f"規則・制度に基づく回答:\n\n「{content}」\n\n[1] より引用"
        else:
            # 直接摘録フォールバック（LLM 不可時）
            answer = f"規則・制度に基づく回答:\n\n「{content}」\n\n[1] より引用"

        confidence = top_result.get("score", 0.5)

        # 信頼度が低い場合は注記を追加
        if confidence < 0.7:
            answer += "\n\n⚠️ この回答は参考情報です。正確な内容は担当部門にご確認ください。"

        return InternalKBResponse(
            answer=answer,
            confidence=confidence,
            citations=citations,
        )

    async def _handle_uncertainty(
        self,
        question: str,
        response: InternalKBResponse,
        user_context: dict[str, Any],
    ) -> InternalKBResponse:
        """不確定時の処理.

        Args:
            question: 質問
            response: 現在のレスポンス
            user_context: ユーザーコンテキスト

        Returns:
            更新されたレスポンス
        """
        response.needs_confirmation = True
        response.suggested_contacts = self._get_responsible_contacts(question)

        # 回答に注記を追加
        response.answer += "\n\n⚠️ この回答の信頼度は低めです。以下の担当者にご確認ください。"

        # 工単自動生成
        if self._config.auto_generate_ticket:
            ticket = await self._ticket_generator.create_from_question(
                question=question,
                context=user_context,
                reason="回答の信頼度が低い",
                partial_answer=response.answer,
                confidence=response.confidence,
            )
            response.ticket_id = ticket.ticket_id

        return response

    def _get_responsible_contacts(self, question: str) -> list[dict[str, str]]:
        """担当者連絡先を取得.

        Args:
            question: 質問

        Returns:
            連絡先リスト
        """
        # キーワードベースの担当者マッピング
        contact_mapping = {
            "休暇": {"department": "人事部", "email": "hr@example.com"},
            "給与": {"department": "人事部", "email": "hr@example.com"},
            "経費": {"department": "経理部", "email": "finance@example.com"},
            "セキュリティ": {"department": "情報セキュリティ部", "email": "security@example.com"},
            "システム": {"department": "IT部", "email": "it@example.com"},
        }

        contacts = []
        question_lower = question.lower()

        for keyword, contact in contact_mapping.items():
            if keyword in question_lower:
                contacts.append(contact)

        # デフォルト連絡先
        if not contacts:
            contacts.append(
                {
                    "department": "総務部",
                    "email": "general@example.com",
                }
            )

        return contacts

    async def _ensure_initialized(self) -> None:
        """初期化を確認."""
        if self._initialized:
            return

        if not self._kb_manager:
            self._kb_manager = IsolatedKBManager()
            await self._kb_manager.start()

        if self._llm is None:
            from agentflow.providers import get_llm
            self._llm = get_llm()

        self._initialized = True


__all__ = [
    "Citation",
    "InternalKBAgent",
    "InternalKBConfig",
    "InternalKBResponse",
]
