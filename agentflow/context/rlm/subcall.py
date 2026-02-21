"""SubCall Manager - ネストLLM呼び出し管理.

RLMコントローラーからのセマンティック操作（要約、抽出等）を
予算制限付きで実行する。

操作タイプ:
- semantic_search: 関連セクションを意味検索
- summarize_section: セクションを要約
- extract_information: 構造化情報を抽出
- assess_confidence: 回答の信頼度を評価

設計原則:
- 予算ファースト: 各呼び出しでToken消費を追跡
- 失敗安全: エラー時は空結果を返す
- 非同期: 全操作はasync

使用例:
    >>> manager = SubCallManager(llm_client, budget)
    >>> summary = await manager.summarize_section(content, focus="認証要件")
    >>> print(budget.remaining_tokens)  # 残り予算
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from typing import Any, Protocol

from agentflow.context.rlm.config import SubCallBudget


class LLMClientProtocol(Protocol):
    """LLMクライアントプロトコル（DI用）."""

    async def chat(
        self,
        messages: list[dict[str, str]],
        **kwargs: Any,
    ) -> dict[str, Any]:
        """チャット補完を実行.

        Args:
            messages: メッセージリスト
            **kwargs: 追加パラメータ

        Returns:
            レスポンス辞書（content, usage等）
        """
        ...


@dataclass
class SubCallResult:
    """サブコール結果.

    Attributes:
        success: 成功フラグ
        content: 結果コンテンツ
        tokens_used: 消費Token数
        error: エラーメッセージ（失敗時）
        metadata: 追加メタデータ
    """

    success: bool
    content: Any
    tokens_used: int = 0
    error: str = ""
    metadata: dict[str, Any] = field(default_factory=dict)


class SubCallManager:
    """サブコール（ネストLLM呼び出し）管理.

    RLMコントローラーからセマンティック操作を実行。
    予算を厳格に追跡し、超過時は実行を拒否。

    使用例:
        >>> budget = SubCallBudget(max_tokens=50000, max_calls=30)
        >>> manager = SubCallManager(llm_client, budget)
        >>>
        >>> # 要約
        >>> result = await manager.summarize_section(
        ...     content="長い文章...",
        ...     focus="認証要件",
        ... )
        >>> if result.success:
        ...     print(result.content)
        >>>
        >>> # 予算確認
        >>> print(f"Remaining: {budget.remaining_tokens} tokens")
    """

    def __init__(
        self,
        llm_client: LLMClientProtocol | None = None,
        budget: SubCallBudget | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            budget: サブコール予算
        """
        self._llm = llm_client
        self._budget = budget or SubCallBudget()
        self._logger = logging.getLogger(__name__)

    def set_llm_client(self, llm_client: LLMClientProtocol) -> None:
        """LLMクライアントを設定.

        Args:
            llm_client: LLMクライアント
        """
        self._llm = llm_client

    def set_budget(self, budget: SubCallBudget) -> None:
        """予算を設定.

        Args:
            budget: サブコール予算
        """
        self._budget = budget

    @property
    def budget(self) -> SubCallBudget:
        """現在の予算を取得."""
        return self._budget

    def _estimate_tokens(self, text: str) -> int:
        """Token数を推定.

        Args:
            text: 対象テキスト

        Returns:
            推定Token数
        """
        if not text:
            return 0

        # CJK文字をカウント
        cjk_chars = len(re.findall(r"[\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff]", text))
        ascii_chars = len(re.findall(r"[a-zA-Z0-9\s]", text))
        other_chars = len(text) - cjk_chars - ascii_chars

        return int(cjk_chars * 1.5 + ascii_chars * 0.25 + other_chars)

    async def _call_llm(
        self,
        system_prompt: str,
        user_prompt: str,
        max_tokens: int = 1000,
    ) -> SubCallResult:
        """LLMを呼び出し.

        Args:
            system_prompt: システムプロンプト
            user_prompt: ユーザープロンプト
            max_tokens: 最大出力Token数

        Returns:
            SubCallResult
        """
        if self._llm is None:
            return SubCallResult(
                success=False,
                content=None,
                error="LLM client not configured",
            )

        # 入力Token数を推定
        input_tokens = self._estimate_tokens(system_prompt) + self._estimate_tokens(user_prompt)
        estimated_total = input_tokens + max_tokens

        # 予算チェック
        if not self._budget.can_afford(estimated_total):
            return SubCallResult(
                success=False,
                content=None,
                error=f"Budget exhausted: need ~{estimated_total}, have {self._budget.remaining_tokens}",
            )

        try:
            messages = [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ]

            response = await self._llm.chat(
                messages=messages,
                max_tokens=max_tokens,
            )

            # Token使用量を取得
            usage = response.get("usage", {})
            tokens_used = usage.get("total_tokens", estimated_total)

            # 予算を消費
            self._budget.consume(tokens_used)

            # コンテンツを取得
            content = response.get("content", "")
            if not content and "choices" in response:
                choices = response["choices"]
                if choices:
                    content = choices[0].get("message", {}).get("content", "")

            return SubCallResult(
                success=True,
                content=content,
                tokens_used=tokens_used,
                metadata={"usage": usage},
            )

        except Exception as e:
            self._logger.warning("SubCall failed: %s", e)
            return SubCallResult(
                success=False,
                content=None,
                error=str(e),
            )

    async def semantic_search(
        self,
        content: str,
        query: str,
        top_k: int = 5,
    ) -> SubCallResult:
        """セマンティック検索.

        コンテンツからクエリに関連する部分を特定。

        Args:
            content: 検索対象コンテンツ
            query: 検索クエリ
            top_k: 返す結果数

        Returns:
            SubCallResult（content: 関連部分のリスト）
        """
        system_prompt = """You are a semantic search assistant.
Given content and a query, identify the most relevant sections.

Return a JSON array of objects with:
- "section": Brief description of the section
- "start_marker": A unique phrase at the start of the relevant section
- "relevance": Score 0-1

Only return the JSON array, no explanation."""

        user_prompt = f"""Query: {query}

Content:
{content[:15000]}  # Limit to avoid excessive tokens

Return top {top_k} most relevant sections."""

        result = await self._call_llm(system_prompt, user_prompt, max_tokens=500)

        if result.success:
            # JSONをパース試行
            try:
                import json

                sections = json.loads(result.content)
                result.content = sections
            except (json.JSONDecodeError, TypeError):
                # パース失敗時はそのまま返す
                pass

        return result

    async def summarize_section(
        self,
        content: str,
        focus: str = "",
        max_length: int = 500,
    ) -> SubCallResult:
        """セクションを要約.

        Args:
            content: 要約対象コンテンツ
            focus: 焦点を当てるトピック
            max_length: 最大文字数

        Returns:
            SubCallResult（content: 要約文字列）
        """
        system_prompt = """You are a technical summarization assistant.
Provide concise, accurate summaries that preserve key technical details.
Focus on facts, not opinions. Use bullet points for clarity."""

        focus_instruction = f"\nFocus especially on: {focus}" if focus else ""
        user_prompt = f"""Summarize the following content in at most {max_length} characters.{focus_instruction}

Content:
{content[:10000]}"""

        return await self._call_llm(system_prompt, user_prompt, max_tokens=max_length // 2)

    async def extract_information(
        self,
        content: str,
        extraction_spec: str,
    ) -> SubCallResult:
        """構造化情報を抽出.

        Args:
            content: 抽出元コンテンツ
            extraction_spec: 抽出仕様（何を抽出するか）

        Returns:
            SubCallResult（content: 抽出結果）
        """
        system_prompt = """You are an information extraction assistant.
Extract structured information according to the specification.
Return valid JSON. If information is not found, use null.
Only return the JSON, no explanation."""

        user_prompt = f"""Extraction specification:
{extraction_spec}

Content:
{content[:10000]}

Extract the specified information as JSON."""

        result = await self._call_llm(system_prompt, user_prompt, max_tokens=1000)

        if result.success:
            try:
                import json

                extracted = json.loads(result.content)
                result.content = extracted
            except (json.JSONDecodeError, TypeError):
                pass

        return result

    async def assess_confidence(
        self,
        query: str,
        current_answer: str,
        evidence: list[str],
    ) -> SubCallResult:
        """回答の信頼度を評価.

        Args:
            query: 元のクエリ
            current_answer: 現在の回答
            evidence: 根拠リスト

        Returns:
            SubCallResult（content: {"confidence": float, "reasoning": str}）
        """
        system_prompt = """You are a quality assessment assistant.
Evaluate how well an answer addresses a query based on evidence.

Return JSON with:
- "confidence": 0.0-1.0 (how confident the answer is correct and complete)
- "reasoning": Brief explanation
- "missing": What's missing or unclear (if any)
- "suggestions": Suggested next actions (if confidence < 0.9)

Only return JSON, no explanation."""

        evidence_text = "\n".join(f"- {e[:500]}" for e in evidence[:5])
        user_prompt = f"""Query: {query}

Current Answer:
{current_answer[:2000]}

Evidence:
{evidence_text}

Assess the answer quality."""

        result = await self._call_llm(system_prompt, user_prompt, max_tokens=500)

        if result.success:
            try:
                import json

                assessment = json.loads(result.content)
                result.content = assessment
            except (json.JSONDecodeError, TypeError):
                # パース失敗時はデフォルト値
                result.content = {
                    "confidence": 0.5,
                    "reasoning": "Failed to parse assessment",
                    "missing": "",
                    "suggestions": [],
                }

        return result

    async def plan_next_action(
        self,
        query: str,
        context_handles: list[dict[str, Any]],
        workspace_summary: str,
        available_actions: list[str],
    ) -> SubCallResult:
        """次のアクションを計画.

        Args:
            query: 元のクエリ
            context_handles: 利用可能なコンテキストハンドル
            workspace_summary: ワークスペースの現状
            available_actions: 利用可能なアクション

        Returns:
            SubCallResult（content: {"action": str, "parameters": dict, "reasoning": str}）
        """
        system_prompt = """You are a strategic planning assistant for document analysis.
Given a query and available contexts, decide the best next action.

Prioritize:
1. Use low-cost deterministic operations first (regex_find, keyword_find, peek)
2. Only use semantic operations when deterministic methods are insufficient
3. Stop when confident the answer is found

Return JSON with:
- "action": The action to take
- "parameters": Parameters for the action
- "reasoning": Why this action

Only return JSON."""

        handles_text = "\n".join(
            f"- {h['handle_id']}: {h.get('total_lines', '?')} lines, summary: {h.get('summary', 'N/A')[:100]}"
            for h in context_handles
        )

        actions_text = ", ".join(available_actions)

        user_prompt = f"""Query: {query}

Available Contexts:
{handles_text}

Current Workspace:
{workspace_summary}

Available Actions: {actions_text}

What should be the next action?"""

        result = await self._call_llm(system_prompt, user_prompt, max_tokens=300)

        if result.success:
            try:
                import json

                plan = json.loads(result.content)
                result.content = plan
            except (json.JSONDecodeError, TypeError):
                # パース失敗時はデフォルト
                result.content = {
                    "action": "peek",
                    "parameters": {},
                    "reasoning": "Failed to parse plan, defaulting to peek",
                }

        return result

    async def synthesize_answer(
        self,
        query: str,
        workspace_content: str,
        evidence: list[str],
    ) -> SubCallResult:
        """最終回答を合成.

        Args:
            query: 元のクエリ
            workspace_content: ワークスペースの内容
            evidence: 根拠リスト

        Returns:
            SubCallResult（content: 最終回答文字列）
        """
        system_prompt = """You are a synthesis assistant.
Combine gathered information to provide a complete, accurate answer.

Guidelines:
- Be comprehensive but concise
- Cite specific evidence when possible
- Acknowledge uncertainties
- Structure the answer clearly"""

        evidence_text = "\n".join(f"Evidence {i + 1}: {e[:1000]}" for i, e in enumerate(evidence[:5]))

        user_prompt = f"""Query: {query}

Gathered Information:
{workspace_content[:5000]}

Key Evidence:
{evidence_text}

Synthesize a complete answer to the query."""

        return await self._call_llm(system_prompt, user_prompt, max_tokens=2000)

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計辞書
        """
        return {
            "budget": self._budget.to_dict(),
            "has_llm": self._llm is not None,
        }
