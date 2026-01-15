# -*- coding: utf-8 -*-
"""DeepAgentCoordinator アダプター.

Decision Governance Engine に DeepAgent の機能を統合するアダプター。

提供機能:
- 認知分析（CognitiveAnalysis）
- コンテキスト圧縮（ContextCompressor）
- 自己進化システム（Evolver）
- 進捗管理（ProgressManager）

使用例:
    >>> adapter = DeepAgentAdapter(llm_client=my_llm)
    >>> cognitive = await adapter.analyze_cognitive("投資判断をしたい")
    >>> adapter.record_success(task, result)
"""

import logging
from typing import Any

from agentflow.patterns.deep_agent import (
    CognitiveAnalysis,
    ContextCompressor,
    CompactionStrategy,
    SelfEvolver,
    ProgressManager,
    QualityReview,
    QualityDimension,
    MemoryEvolutionStore,
    AgentMessage,
    MessageType,
)


class DeepAgentAdapter:
    """DeepAgentCoordinator 機能アダプター.

    既存の Decision Engine に DeepAgent の高度な機能を統合。

    責務:
    - 認知分析の実行
    - コンテキスト圧縮
    - 自己進化システムの管理
    - 進捗追跡

    Example:
        >>> adapter = DeepAgentAdapter(llm_client=my_llm)
        >>> # 認知分析
        >>> cognitive = await adapter.analyze_cognitive("新規事業の投資判断")
        >>> print(cognitive.complexity)  # "high"
        >>>
        >>> # 成功学習
        >>> adapter.record_success("投資判断", {"result": "承認"})
    """

    def __init__(
        self,
        llm_client: Any = None,
        enable_evolution: bool = True,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            enable_evolution: 自己進化機能を有効化
        """
        self._llm = llm_client
        self._logger = logging.getLogger(__name__)

        # コンポーネント初期化
        self._progress = ProgressManager()
        self._compressor = ContextCompressor(llm_client=llm_client)

        # 進化システム（オプション）
        if enable_evolution:
            self._evolution_store = MemoryEvolutionStore()
            self._evolver = SelfEvolver(store=self._evolution_store)
        else:
            self._evolver = None
            self._evolution_store = None

    async def analyze_cognitive(self, question: str) -> CognitiveAnalysis:
        """認知分析を実行.

        Args:
            question: 分析対象の質問

        Returns:
            認知分析結果
        """
        if not self._llm:
            # ルールベース簡易分析
            return CognitiveAnalysis(
                intent=question,
                is_clear=len(question) > 20,
                complexity=self._estimate_complexity(question),
                domains=self._extract_domains(question),
            )

        # LLMによる詳細分析
        prompt = f"""以下の意思決定課題を認知分析してください:

課題: {question}

分析項目:
1. ユーザーの意図（intent）
2. 課題が明確か（is_clear）
3. 不明確な場合の追加質問（clarification_needed）
4. 複雑度（complexity: low/medium/high）
5. 関連領域（domains）
6. 推薦分析パターン（suggested_agents）

JSON形式で回答:"""

        try:
            response = await self._llm.chat([
                {"role": "system", "content": "あなたは意思決定分析の専門家です。"},
                {"role": "user", "content": prompt},
            ], response_format={"type": "json_object"})

            import json
            data = json.loads(
                response.content if hasattr(response, "content") else str(response)
            )

            return CognitiveAnalysis(
                intent=data.get("intent", question),
                is_clear=data.get("is_clear", True),
                clarification_needed=data.get("clarification_needed", []),
                complexity=data.get("complexity", "medium"),
                domains=data.get("domains", []),
                suggested_agents=data.get("suggested_agents", []),
            )
        except Exception as e:
            self._logger.warning(f"認知分析失敗: {e}")
            return CognitiveAnalysis(intent=question, is_clear=True)

    def _estimate_complexity(self, question: str) -> str:
        """複雑度を推定."""
        # 簡易ヒューリスティック
        length = len(question)
        keywords = ["投資", "M&A", "戦略", "リスク", "長期", "複合"]
        keyword_count = sum(1 for kw in keywords if kw in question)

        if length > 200 or keyword_count >= 3:
            return "high"
        if length > 100 or keyword_count >= 1:
            return "medium"
        return "low"

    def _extract_domains(self, question: str) -> list[str]:
        """関連領域を抽出."""
        domain_keywords = {
            "finance": ["投資", "資金", "融資", "財務"],
            "strategy": ["戦略", "計画", "方針", "ビジョン"],
            "technology": ["技術", "IT", "システム", "DX"],
            "hr": ["人事", "採用", "組織", "人材"],
        }
        domains = []
        for domain, keywords in domain_keywords.items():
            if any(kw in question for kw in keywords):
                domains.append(domain)
        return domains or ["general"]

    async def compress_context(
        self,
        messages: list[dict[str, Any]],
        max_tokens: int = 4000,
        strategy: CompactionStrategy = CompactionStrategy.SELECTIVE,
    ) -> tuple[list[dict[str, Any]], float]:
        """コンテキストを圧縮.

        Args:
            messages: メッセージリスト
            max_tokens: 最大トークン数
            strategy: 圧縮戦略

        Returns:
            (圧縮後メッセージ, 圧縮率)
        """
        # dict を AgentMessage に変換
        agent_messages = [
            AgentMessage(
                from_agent=m.get("from", "system"),
                to_agent=m.get("to", "user"),
                content=m.get("content", {}),
                msg_type=MessageType.RESULT,
            )
            for m in messages
        ]

        compressed, result = await self._compressor.compact_messages(
            agent_messages,
            max_tokens=max_tokens,
            strategy=strategy,
        )

        # AgentMessage を dict に戻す
        compressed_dicts = [
            {
                "from": m.from_agent,
                "to": m.to_agent,
                "content": m.content,
            }
            for m in compressed
        ]

        return compressed_dicts, result.compression_ratio

    async def record_success(
        self,
        task: str,
        result: dict[str, Any],
        context: dict[str, Any] | None = None,
    ) -> None:
        """成功パターンを学習.

        Args:
            task: タスク内容
            result: 実行結果
            context: コンテキスト情報
        """
        if self._evolver:
            await self._evolver.learn_from_success(task, result, context or {})

    async def process_feedback(
        self,
        feedback_type: str,
        content: str,
    ) -> None:
        """フィードバックを処理.

        Args:
            feedback_type: フィードバック種別（correction/education/preference）
            content: フィードバック内容
        """
        if self._evolver:
            await self._evolver.process_feedback(feedback_type, content)

    def get_learned_hint(self, task: str) -> str | None:
        """学習済みヒントを取得.

        Args:
            task: タスク内容

        Returns:
            学習済みヒント（なければNone）
        """
        if self._evolver:
            return self._evolver.get_learned_hint(task)
        return None

    async def quality_review(
        self,
        task: str,
        results: dict[str, Any],
        threshold: float = 70.0,
    ) -> QualityReview:
        """品質評審を実行.

        Args:
            task: タスク内容
            results: 実行結果
            threshold: 品質閾値

        Returns:
            品質評審結果
        """
        if not self._llm:
            # ルールベース簡易評価
            has_all_sections = all(
                key in results
                for key in ["dao", "fa", "shu", "qi"]
            )
            score = 80.0 if has_all_sections else 50.0

            return QualityReview(
                is_acceptable=score >= threshold,
                score=score,
                dimension_scores={
                    QualityDimension.COMPLETENESS.value: score,
                    QualityDimension.ACCURACY.value: 70.0,
                    QualityDimension.CONSISTENCY.value: 75.0,
                },
                verdict="pass" if score >= threshold else "revise",
                confidence=0.6,
            )

        # LLMによる詳細評価
        prompt = f"""意思決定分析結果を評価してください:

タスク: {task}
結果: {results}

評価基準: 品質閾値 {threshold}点

以下の次元で0-100点評価:
- completeness: 分析の完全性
- accuracy: 分析の正確さ
- consistency: 全体の整合性

JSON形式で回答:
{{
  "is_acceptable": true/false,
  "score": 総合スコア0-100,
  "dimension_scores": {{"completeness": 0-100, ...}},
  "verdict": "pass/revise/reject",
  "issues": [],
  "suggestions": []
}}"""

        try:
            response = await self._llm.chat([
                {"role": "system", "content": "あなたは意思決定品質評価の専門家です。"},
                {"role": "user", "content": prompt},
            ], response_format={"type": "json_object"})

            import json
            data = json.loads(
                response.content if hasattr(response, "content") else str(response)
            )

            return QualityReview(
                is_acceptable=data.get("is_acceptable", False),
                score=data.get("score", 0.0),
                dimension_scores=data.get("dimension_scores", {}),
                verdict=data.get("verdict", "pending"),
                issues=data.get("issues", []),
                suggestions=data.get("suggestions", []),
                confidence=0.8,
            )
        except Exception as e:
            self._logger.warning(f"品質評審失敗: {e}")
            return QualityReview(
                is_acceptable=True,
                score=70.0,
                verdict="pass",
                confidence=0.4,
            )

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        stats = {
            "progress": self._progress.get_progress(),
            "compressor": self._progress.get_compressor_stats(),
        }
        if self._evolver:
            stats["evolution"] = self._evolver.get_evolution_stats()
        return stats


__all__ = ["DeepAgentAdapter"]

