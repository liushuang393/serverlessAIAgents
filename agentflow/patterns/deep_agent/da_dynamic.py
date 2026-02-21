"""DeepAgent 動的コンポーネント - DynamicAgent / Evolver / ConversationManager.

このモジュールは深度エージェントシステムの動的コンポーネントを提供します:
- DynamicAgent: AgentPool から動的に生成される Agent
- Evolver: 成功パターンから学習する自己進化システム
- ConversationManager: 対話管理・自動要約システム
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from agentflow.core.agent_block import AgentBlock
from agentflow.patterns.deep_agent.da_compressor import ContextCompressor
from agentflow.patterns.deep_agent.da_models import AgentMessage, EvolutionRecord


if TYPE_CHECKING:
    from agentflow.patterns.deep_agent.da_stores import EvolutionStore
    from agentflow.skills import SkillRegistry
    from agentflow.skills.engine import SkillEngine


def _default_skill_registry() -> SkillRegistry:
    """SkillRegistry をレイジーロードして返す."""
    from agentflow.skills import SkillRegistry

    return SkillRegistry()


# =============================================================================
# DynamicAgent - 動的生成 Agent
# =============================================================================


class DynamicAgent(AgentBlock):
    """動的生成Agent.

    AgentPoolから動的に生成されるAgent。
    Skills の instructions を自動的にシステムプロンプトに注入。
    """

    def __init__(
        self,
        name: str,
        system_prompt: str,
        llm_client: Any = None,
        tools: list[Any] | None = None,
        skills: list[str] | None = None,
        context: dict[str, Any] | None = None,
        skill_registry: Any = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            name: Agent 名
            system_prompt: システムプロンプト
            llm_client: LLM クライアント
            tools: ツールリスト
            skills: スキル名リスト
            context: コンテキスト
            skill_registry: スキルレジストリ（None の場合はグローバルを使用）
        """
        super().__init__(**kwargs)
        self._name = name
        self._system_prompt = system_prompt
        self._llm = llm_client
        self._tools = tools or []
        self._skills = skills or []
        self._context = context or {}
        self._skill_registry = skill_registry if skill_registry is not None else _default_skill_registry()
        self._logger = logging.getLogger(__name__)

    def _build_skill_instructions(self) -> str:
        """Skills の instructions を構築."""
        if not self._skills:
            return ""

        skill_prompts = []
        for skill_name in self._skills:
            skill = self._skill_registry.get(skill_name)
            if skill:
                skill_prompts.append(skill.to_prompt())
                self._logger.debug(f"Skill 指示を追加: {skill_name}")
            else:
                self._logger.warning(f"Skill が見つかりません: {skill_name}")

        if skill_prompts:
            return "\n\n---\n\n".join(skill_prompts)
        return ""

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent実行.

        Skills の instructions をシステムプロンプトに注入して LLM を呼び出す。
        """
        if not self._llm:
            return {"error": "LLM not configured", "agent": self._name}

        skill_instructions = self._build_skill_instructions()
        full_system_prompt = self._system_prompt
        if skill_instructions:
            full_system_prompt = f"{self._system_prompt}\n\n# Skills\n\n{skill_instructions}"

        messages = [
            {"role": "system", "content": full_system_prompt},
            {"role": "user", "content": str(input_data.get("task", input_data))},
        ]

        try:
            response = await self._llm.chat(messages)
            content = (
                response.get("content", "")
                if isinstance(response, dict)
                else (response.content if hasattr(response, "content") else str(response))
            )
            return {
                "agent": self._name,
                "output": content,
                "status": "success",
                "skills_used": self._skills,
            }
        except Exception as e:
            self._logger.exception(f"Agent {self._name} 実行失敗: {e}")
            return {"agent": self._name, "error": str(e), "status": "failed"}


# =============================================================================
# Evolver - 自己進化システム
# =============================================================================


class Evolver:
    """自己進化システム.

    成功パターンから学習し、フィードバックを処理して自己進化する。
    高信頼パターンは自動的に Skill として固化される。
    """

    SKILL_CONSOLIDATION_THRESHOLD = 0.85
    MIN_SUCCESS_COUNT_FOR_SKILL = 3

    def __init__(
        self,
        llm_client: Any = None,
        evolution_store: EvolutionStore | None = None,
        skill_engine: SkillEngine | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            evolution_store: 永続化ストア（L3層、オプション）
            skill_engine: SkillEngine（Skill 固化用、オプション）
        """
        self._llm = llm_client
        self._store = evolution_store
        self._skill_engine = skill_engine
        self._records: list[EvolutionRecord] = []
        self._learned_patterns: dict[str, str] = {}
        self._pattern_scores: dict[str, float] = {}
        self._pattern_success_count: dict[str, int] = {}
        self._consolidated_skills: set[str] = set()
        self._logger = logging.getLogger(__name__)

    async def learn_from_success(
        self,
        task: str,
        result: dict[str, Any],
        context: dict[str, Any],
    ) -> EvolutionRecord | None:
        """成功パターンから学習."""
        pattern_key = self._extract_pattern_key(task)

        self._pattern_success_count[pattern_key] = self._pattern_success_count.get(pattern_key, 0) + 1

        current_score = self._pattern_scores.get(pattern_key, 0.5)
        new_score = min(1.0, current_score + 0.1)
        self._pattern_scores[pattern_key] = new_score

        record = EvolutionRecord(
            event_type="success",
            pattern=f"task:{pattern_key}",
            confidence=new_score,
        )
        self._records.append(record)

        approach = result.get("approach", "")
        if approach:
            self._learned_patterns[pattern_key] = str(approach)

        if self._store:
            await self._store.save_pattern(
                pattern_key,
                {
                    "task": task[:200],
                    "approach": str(approach)[:500],
                    "confidence": new_score,
                },
            )
            await self._store.save_feedback(record)

        self._logger.info(f"成功パターン学習: {pattern_key} (信頼度: {new_score:.2f})")

        await self._try_consolidate_skill(task, pattern_key, result, context)

        return record

    async def _try_consolidate_skill(
        self,
        task: str,
        pattern_key: str,
        result: dict[str, Any],
        context: dict[str, Any],
    ) -> bool:
        """高信頼パターンを Skill として固化を試行."""
        if not self._skill_engine:
            return False

        if pattern_key in self._consolidated_skills:
            return False

        confidence = self._pattern_scores.get(pattern_key, 0.0)
        success_count = self._pattern_success_count.get(pattern_key, 0)

        if confidence < self.SKILL_CONSOLIDATION_THRESHOLD or success_count < self.MIN_SUCCESS_COUNT_FOR_SKILL:
            return False

        try:
            self._learned_patterns.get(pattern_key, "")
            context.get("agent_type", "unknown")
            context.get("skills_used", [])

            skill_result = await self._skill_engine.resolve(task)
            if skill_result.generated and skill_result.saved:
                self._consolidated_skills.add(pattern_key)
                self._logger.info(f"Skill 固化成功: {skill_result.skill.name} (パターン: {pattern_key})")
                return True

            return False

        except Exception as e:
            self._logger.warning(f"Skill 固化失敗: {e}")
            return False

    async def process_feedback(
        self,
        feedback_type: str,
        content: str,
        context: dict[str, Any] | None = None,
    ) -> EvolutionRecord:
        """客户反馈を処理."""
        confidence_map = {
            "education": 0.9,
            "correction": 0.8,
            "suggestion": 0.6,
        }
        confidence = confidence_map.get(feedback_type, 0.5)

        record = EvolutionRecord(
            event_type=f"feedback:{feedback_type}",
            pattern=content[:100],
            confidence=confidence,
        )
        self._records.append(record)

        if self._store:
            await self._store.save_feedback(record)

        self._logger.info(f"反馈処理: {feedback_type} (信頼度: {confidence:.2f})")
        return record

    def get_learned_hint(self, task: str) -> str | None:
        """学習済みヒントを取得."""
        pattern_key = self._extract_pattern_key(task)
        return self._learned_patterns.get(pattern_key)

    def get_pattern_confidence(self, task: str) -> float:
        """パターンの信頼度を取得."""
        pattern_key = self._extract_pattern_key(task)
        return self._pattern_scores.get(pattern_key, 0.0)

    def _extract_pattern_key(self, task: str) -> str:
        """タスクからパターンキーを抽出."""
        normalized = task[:30].strip().lower()
        return "".join(c if c.isalnum() else "_" for c in normalized)

    def get_evolution_stats(self) -> dict[str, Any]:
        """進化統計を取得."""
        return {
            "total_records": len(self._records),
            "learned_patterns": len(self._learned_patterns),
            "success_count": sum(1 for r in self._records if r.event_type == "success"),
            "feedback_count": sum(1 for r in self._records if "feedback" in r.event_type),
            "avg_confidence": (
                sum(self._pattern_scores.values()) / len(self._pattern_scores) if self._pattern_scores else 0.0
            ),
            "high_confidence_patterns": sum(1 for score in self._pattern_scores.values() if score >= 0.8),
        }

    def get_top_patterns(self, limit: int = 10) -> list[dict[str, Any]]:
        """高信頼度パターンを取得."""
        sorted_patterns = sorted(
            self._pattern_scores.items(),
            key=lambda x: -x[1],
        )[:limit]

        return [
            {
                "pattern": key,
                "confidence": score,
                "hint": self._learned_patterns.get(key, ""),
            }
            for key, score in sorted_patterns
        ]


# =============================================================================
# ConversationManager - 対話管理・自動要約
# =============================================================================


class ConversationManager:
    """対話管理・自動要約システム.

    トークン制限監視、インテリジェント要約、コンテキストウィンドウ最適化を提供する。

    Example:
        >>> manager = ConversationManager(max_tokens=4000)
        >>> manager.add_message(AgentMessage(...))
        >>> if manager.needs_summarization():
        ...     await manager.auto_summarize()
    """

    def __init__(
        self,
        llm_client: Any = None,
        max_tokens: int = 4000,
        warning_threshold: float = 0.8,
        recent_keep_count: int = 5,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（要約生成用）
            max_tokens: 最大トークン数
            warning_threshold: 警告閾値（0.0-1.0）
            recent_keep_count: 完全保持する直近メッセージ数
        """
        self._llm = llm_client
        self._max_tokens = max_tokens
        self._warning_threshold = warning_threshold
        self._recent_keep_count = recent_keep_count
        self._logger = logging.getLogger(__name__)

        self._messages: list[AgentMessage] = []
        self._summaries: list[str] = []
        self._compressor = ContextCompressor(llm_client=llm_client)

    def add_message(self, message: AgentMessage) -> None:
        """メッセージを追加."""
        self._messages.append(message)

    def add_messages(self, messages: list[AgentMessage]) -> None:
        """複数メッセージを追加."""
        self._messages.extend(messages)

    def current_token_count(self) -> int:
        """現在のトークン数を概算."""
        total = sum(len(str(m.content)) // 4 for m in self._messages)
        total += sum(len(s) // 4 for s in self._summaries)
        return total

    def utilization(self) -> float:
        """トークン使用率（0.0-1.0）."""
        return self.current_token_count() / self._max_tokens

    def needs_summarization(self) -> bool:
        """要約が必要かどうか."""
        return self.utilization() > self._warning_threshold

    def is_critical(self) -> bool:
        """クリティカル状態（即座に圧縮必要）."""
        return self.utilization() > 0.95

    async def auto_summarize(self) -> dict[str, Any]:
        """自動要約を実行."""
        if not self._messages:
            return {"status": "no_messages", "compressed": False}

        original_count = len(self._messages)
        original_tokens = self.current_token_count()

        preserve_messages = self._messages[-self._recent_keep_count :]
        to_summarize = (
            self._messages[: -self._recent_keep_count] if len(self._messages) > self._recent_keep_count else []
        )

        if not to_summarize:
            return {"status": "nothing_to_summarize", "compressed": False}

        summary = await self._generate_summary(to_summarize)
        if summary:
            self._summaries.append(summary)

        self._messages = preserve_messages

        return {
            "status": "success",
            "compressed": True,
            "original_messages": original_count,
            "preserved_messages": len(preserve_messages),
            "summarized_messages": len(to_summarize),
            "original_tokens": original_tokens,
            "new_tokens": self.current_token_count(),
            "compression_ratio": (self.current_token_count() / original_tokens if original_tokens > 0 else 1.0),
        }

    async def _generate_summary(self, messages: list[AgentMessage]) -> str | None:
        """メッセージ群から要約を生成."""
        if not messages:
            return None

        if not self._llm:
            return self._simple_summary(messages)

        content = "\n".join(
            [f"[{m.from_agent}→{m.to_agent}] {m.msg_type.value}: {str(m.content)[:200]}" for m in messages]
        )

        prompt = f"""以下の会話を簡潔に要約してください（重要な決定・結果・エラーを優先）:

{content}

要約（100-200文字）:"""

        try:
            response = await self._llm.chat(
                [
                    {"role": "system", "content": "あなたは会話を簡潔に要約する専門家です。"},
                    {"role": "user", "content": prompt},
                ]
            )
            return response.content if hasattr(response, "content") else str(response)
        except Exception as e:
            self._logger.warning(f"要約生成失敗: {e}")
            return self._simple_summary(messages)

    def _simple_summary(self, messages: list[AgentMessage]) -> str:
        """簡易要約（LLMなし）."""
        if not messages:
            return ""

        agents = {m.from_agent for m in messages}
        types = {m.msg_type.value for m in messages}

        key_points: list[str] = []
        for m in messages:
            content_str = str(m.content).lower()
            if "error" in content_str or "failed" in content_str:
                key_points.append(f"エラー: {str(m.content)[:50]}")
            elif "result" in content_str or "success" in content_str:
                key_points.append(f"結果: {str(m.content)[:50]}")

        summary = f"[要約] {len(messages)}メッセージ, Agents: {', '.join(agents)}, Types: {', '.join(types)}"
        if key_points:
            summary += f", キーポイント: {'; '.join(key_points[:3])}"

        return summary

    def get_context(self) -> dict[str, Any]:
        """現在のコンテキストを取得（LLMに渡す用）."""
        context: dict[str, Any] = {}

        if self._summaries:
            context["previous_summary"] = "\n".join(self._summaries)

        context["recent_messages"] = [
            {
                "from": m.from_agent,
                "to": m.to_agent,
                "type": m.msg_type.value,
                "content": m.content,
            }
            for m in self._messages
        ]

        return context

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "message_count": len(self._messages),
            "summary_count": len(self._summaries),
            "current_tokens": self.current_token_count(),
            "max_tokens": self._max_tokens,
            "utilization": self.utilization(),
            "needs_summarization": self.needs_summarization(),
            "is_critical": self.is_critical(),
        }

    def clear(self) -> None:
        """全データをクリア."""
        self._messages.clear()
        self._summaries.clear()


__all__ = [
    "ConversationManager",
    "DynamicAgent",
    "Evolver",
]
