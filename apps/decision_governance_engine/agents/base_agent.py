# -*- coding: utf-8 -*-
"""Decision Governance Engine - BaseDecisionAgent.

全てのDecision Agentが継承する基底クラス。
AgentFlowのAgentBlockを拡張し、LLM統合、リトライ、タイムアウト制御を追加。
"""

import asyncio
import logging
from abc import abstractmethod
from pathlib import Path
from typing import Any, Generic, TypeVar

from pydantic import BaseModel

from agentflow.core.agent_block import AgentBlock

InputT = TypeVar("InputT", bound=BaseModel)
OutputT = TypeVar("OutputT", bound=BaseModel)


class AgentExecutionError(Exception):
    """Agent実行エラー."""

    def __init__(self, agent_name: str, message: str, original_error: Exception | None = None) -> None:
        """初期化."""
        self.agent_name = agent_name
        self.original_error = original_error
        super().__init__(f"[{agent_name}] {message}")


class AgentTimeoutError(AgentExecutionError):
    """Agentタイムアウトエラー."""


class AgentRetryExhaustedError(AgentExecutionError):
    """Agentリトライ上限エラー."""


class BaseDecisionAgent(AgentBlock, Generic[InputT, OutputT]):
    """Decision Governance Engine用の基底Agentクラス.

    特徴:
    - Pydantic入出力の型安全性
    - LLMクライアント統合
    - Skills(SKILL.md)からのプロンプト読み込み
    - タイムアウト制御（asyncio.timeout）
    - リトライ機構（max_retry回）
    """

    # サブクラスで設定必須
    name: str = "BaseDecisionAgent"
    max_tokens: int = 1000
    temperature: float = 0.5
    timeout_seconds: int = 30
    max_retry: int = 2
    retry_delay: float = 1.0

    def __init__(
        self,
        llm_client: Any = None,
        prompts_dir: Path | None = None,
        skills_dir: Path | None = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアントインスタンス
            prompts_dir: プロンプトテンプレートディレクトリ（旧方式）
            skills_dir: Skillsディレクトリ（SKILL.md形式）
            **kwargs: AgentBlockへの引数
        """
        super().__init__(**kwargs)
        self._llm = llm_client
        self._prompts_dir = prompts_dir or Path(__file__).parent.parent / "prompts"
        self._skills_dir = skills_dir or Path(__file__).parent.parent / "skills"
        self._logger = logging.getLogger(f"decision_engine.{self.name}")
        self._cached_skill_prompt: str | None = None

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """エージェント実行（AgentBlock互換 + リトライ + タイムアウト）.

        Args:
            input_data: 入力データ（dict形式）

        Returns:
            出力データ（dict形式）

        Raises:
            AgentTimeoutError: タイムアウト時
            AgentRetryExhaustedError: リトライ上限到達時
        """
        last_error: Exception | None = None

        for attempt in range(self.max_retry + 1):
            try:
                return await self._run_with_timeout(input_data, attempt)
            except asyncio.TimeoutError:
                self._logger.warning(
                    f"{self.name} タイムアウト (attempt {attempt + 1}/{self.max_retry + 1})"
                )
                last_error = asyncio.TimeoutError(f"Timeout after {self.timeout_seconds}s")
            except Exception as e:
                self._logger.warning(
                    f"{self.name} エラー (attempt {attempt + 1}/{self.max_retry + 1}): {e}"
                )
                last_error = e

            # リトライ前にディレイ
            if attempt < self.max_retry:
                await asyncio.sleep(self.retry_delay)

        # 全リトライ失敗
        if isinstance(last_error, asyncio.TimeoutError):
            raise AgentTimeoutError(
                self.name,
                f"タイムアウト ({self.timeout_seconds}秒) - {self.max_retry + 1}回試行後",
                last_error,
            )
        raise AgentRetryExhaustedError(
            self.name,
            f"リトライ上限到達 ({self.max_retry + 1}回) - 最終エラー: {last_error}",
            last_error,
        )

    async def _run_with_timeout(self, input_data: dict[str, Any], attempt: int) -> dict[str, Any]:
        """タイムアウト付きで実行."""
        async with asyncio.timeout(self.timeout_seconds):
            # 入力をPydanticモデルに変換
            typed_input = self._parse_input(input_data)

            # メイン処理
            self._logger.info(f"{self.name} 実行開始 (attempt {attempt + 1})")
            output = await self.process(typed_input)
            self._logger.info(f"{self.name} 実行完了")

            # 出力検証
            if not self.validate_output(output):
                self._logger.warning(f"{self.name} 出力検証に警告あり")

            return output.model_dump()

    @abstractmethod
    async def process(self, input_data: InputT) -> OutputT:
        """メイン処理（サブクラスで実装）.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き出力データ
        """

    @abstractmethod
    def _parse_input(self, input_data: dict[str, Any]) -> InputT:
        """入力データをPydanticモデルに変換.

        Args:
            input_data: dict形式の入力

        Returns:
            型付き入力データ
        """

    def validate_output(self, output: OutputT) -> bool:
        """出力の自己検証.

        Args:
            output: 出力データ

        Returns:
            検証結果（True=有効）
        """
        # デフォルトは常にTrue、サブクラスでオーバーライド可能
        return True

    def _load_skill_prompt(self, skill_name: str | None = None) -> str:
        """SKILL.md形式のプロンプトを読み込み.

        Args:
            skill_name: Skill名（Noneの場合はAgent名から推定）

        Returns:
            プロンプト内容（YAML frontmatter除去済み）
        """
        if self._cached_skill_prompt is not None:
            return self._cached_skill_prompt

        # Skill名を決定
        if skill_name is None:
            # Agent名から推定: DaoAgent → dao, GatekeeperAgent → gatekeeper
            skill_name = self.name.replace("Agent", "").lower()

        skill_path = self._skills_dir / skill_name / "SKILL.md"

        if skill_path.exists():
            content = skill_path.read_text(encoding="utf-8")
            # YAML frontmatterを除去
            if content.startswith("---"):
                parts = content.split("---", 2)
                if len(parts) >= 3:
                    self._cached_skill_prompt = parts[2].strip()
                    return self._cached_skill_prompt
            self._cached_skill_prompt = content.strip()
            return self._cached_skill_prompt

        # フォールバック: 旧プロンプト形式
        return self._load_prompt(skill_name)

    def _load_prompt(self, prompt_name: str) -> str:
        """プロンプトテンプレートを読み込み（旧方式）.

        Args:
            prompt_name: プロンプトファイル名（拡張子なし）

        Returns:
            プロンプト内容
        """
        prompt_path = self._prompts_dir / f"{prompt_name}.txt"
        if prompt_path.exists():
            return prompt_path.read_text(encoding="utf-8")
        return ""

    async def _call_llm(self, prompt: str) -> str:
        """LLMを呼び出し.

        Args:
            prompt: 入力プロンプト

        Returns:
            LLM応答テキスト
        """
        if self._llm is None:
            self._logger.warning("LLMクライアント未設定、モック応答を返します")
            return ""

        response = await self._llm.generate(
            prompt,
            max_tokens=self.max_tokens,
            temperature=self.temperature,
        )
        return response

    def get_agent_info(self) -> dict[str, Any]:
        """Agent情報を取得（デバッグ/モニタリング用）."""
        return {
            "name": self.name,
            "max_tokens": self.max_tokens,
            "temperature": self.temperature,
            "timeout_seconds": self.timeout_seconds,
            "max_retry": self.max_retry,
        }

