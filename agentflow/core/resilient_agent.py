# -*- coding: utf-8 -*-
"""ResilientAgent - 健壮な Agent 基底クラス.

このモジュールは、リトライ、タイムアウト、LLM統合を備えた健壮な Agent 基底クラスを提供します。
decision_governance_engine の BaseDecisionAgent から汎用化したものです。

使用例:
    >>> from agentflow.core.resilient_agent import ResilientAgent
    >>> from pydantic import BaseModel
    >>>
    >>> class MyInput(BaseModel):
    ...     question: str
    >>>
    >>> class MyOutput(BaseModel):
    ...     answer: str
    >>>
    >>> class MyAgent(ResilientAgent[MyInput, MyOutput]):
    ...     name = "MyAgent"
    ...     timeout_seconds = 30
    ...     max_retries = 3
    ...
    ...     async def process(self, input_data: MyInput) -> MyOutput:
    ...         response = await self.llm.chat([...])
    ...         return MyOutput(answer=response.content)
    >>>
    >>> agent = MyAgent()
    >>> result = await agent.run({"question": "..."})
"""

import asyncio
import logging
from abc import abstractmethod
from pathlib import Path
from typing import Any, Generic, TypeVar

from pydantic import BaseModel

from agentflow.core.agent_block import AgentBlock
from agentflow.core.exceptions import (
    AgentRetryExhaustedError,
    AgentTimeoutError,
)

# 型変数（Pydantic モデルを制約）
InputT = TypeVar("InputT", bound=BaseModel)
OutputT = TypeVar("OutputT", bound=BaseModel)

__all__ = [
    "InputT",
    "OutputT",
    "ResilientAgent",
    "BaseDecisionAgent",
]


class ResilientAgent(AgentBlock, Generic[InputT, OutputT]):
    """健壮な Agent 基底クラス.

    特徴:
    - Pydantic 入出力の型安全性
    - LLM クライアント自動注入（松耦合）
    - Skills (SKILL.md) からのプロンプト読み込み
    - タイムアウト制御 (asyncio.timeout)
    - リトライ機構 (max_retries 回)

    クラス属性（サブクラスで設定）:
        name: Agent 名（必須）
        timeout_seconds: タイムアウト秒数（デフォルト: 30）
        max_retries: 最大リトライ回数（デフォルト: 2）
        retry_delay: リトライ間隔秒（デフォルト: 1.0）
        retry_backoff: リトライ退避方式 "fixed" | "exponential"（デフォルト: "exponential"）
        max_tokens: LLM 最大トークン数（デフォルト: 1000）
        temperature: LLM 温度パラメータ（デフォルト: 0.5）

    使用例:
        >>> class DaoAgent(ResilientAgent[DaoInput, DaoOutput]):
        ...     name = "DaoAgent"
        ...     timeout_seconds = 60
        ...     max_retries = 3
        ...
        ...     async def process(self, input_data: DaoInput) -> DaoOutput:
        ...         # 自動注入された self.llm を使用
        ...         prompt = self._load_skill_prompt("dao")
        ...         response = await self._call_llm(prompt)
        ...         return DaoOutput(essence=response)
    """

    # サブクラスで設定するクラス属性
    name: str = "ResilientAgent"
    timeout_seconds: int = 30
    max_retries: int = 2
    retry_delay: float = 1.0
    retry_backoff: str = "exponential"  # "fixed" | "exponential"
    max_tokens: int = 1000
    temperature: float = 0.5

    def __init__(
        self,
        llm_client: Any = None,
        prompts_dir: Path | None = None,
        skills_dir: Path | None = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアントインスタンス（None の場合は自動取得）
            prompts_dir: プロンプトテンプレートディレクトリ（旧方式）
            skills_dir: Skills ディレクトリ（SKILL.md 形式）
            **kwargs: AgentBlock への引数
        """
        super().__init__(**kwargs)
        self._llm = llm_client
        self._prompts_dir = prompts_dir
        self._skills_dir = skills_dir
        self._logger = logging.getLogger(f"agentflow.{self.name}")
        self._cached_skill_prompt: str | None = None

    async def initialize(self) -> None:
        """エージェント初期化.

        LLM クライアントの自動取得などを行います。
        """
        await super().initialize()

        # LLM クライアントを自動取得（松耦合）
        if self._llm is None:
            try:
                from agentflow.providers import get_llm

                self._llm = get_llm(temperature=self.temperature)
                self._logger.debug(f"{self.name}: LLM auto-injected")
            except Exception as e:
                self._logger.warning(f"{self.name}: Failed to auto-inject LLM: {e}")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """エージェント実行（リトライ + タイムアウト）.

        Args:
            input_data: 入力データ（dict 形式）

        Returns:
            出力データ（dict 形式）

        Raises:
            AgentTimeoutError: タイムアウト時
            AgentRetryExhaustedError: リトライ上限到達時
        """
        last_error: Exception | None = None

        for attempt in range(self.max_retries + 1):
            try:
                return await self._run_with_timeout(input_data, attempt)
            except asyncio.TimeoutError:
                self._logger.warning(
                    f"{self.name} タイムアウト (attempt {attempt + 1}/{self.max_retries + 1})"
                )
                last_error = asyncio.TimeoutError(
                    f"Timeout after {self.timeout_seconds}s"
                )
            except Exception as e:
                self._logger.warning(
                    f"{self.name} エラー (attempt {attempt + 1}/{self.max_retries + 1}): {e}"
                )
                last_error = e

            # リトライ前にディレイ
            if attempt < self.max_retries:
                delay = self._calculate_retry_delay(attempt)
                await asyncio.sleep(delay)

        # 全リトライ失敗
        if isinstance(last_error, asyncio.TimeoutError):
            raise AgentTimeoutError(
                self.name,
                self.timeout_seconds,
                self.max_retries + 1,
                last_error,
            )
        raise AgentRetryExhaustedError(
            self.name,
            self.max_retries + 1,
            last_error,
        )

    def _calculate_retry_delay(self, attempt: int) -> float:
        """リトライ遅延を計算.

        Args:
            attempt: 現在の試行回数（0-indexed）

        Returns:
            遅延秒数
        """
        if self.retry_backoff == "exponential":
            return self.retry_delay * (2**attempt)
        return self.retry_delay

    async def _run_with_timeout(
        self, input_data: dict[str, Any], attempt: int
    ) -> dict[str, Any]:
        """タイムアウト付きで実行.

        Args:
            input_data: 入力データ
            attempt: 試行回数

        Returns:
            出力データ
        """
        async with asyncio.timeout(self.timeout_seconds):
            # 入力を Pydantic モデルに変換
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
        """メイン処理（サブクラスで実装必須）.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き出力データ
        """

    @abstractmethod
    def _parse_input(self, input_data: dict[str, Any]) -> InputT:
        """入力データを Pydantic モデルに変換（サブクラスで実装必須）.

        Args:
            input_data: dict 形式の入力

        Returns:
            型付き入力データ
        """

    def validate_output(self, output: OutputT) -> bool:
        """出力の自己検証.

        Args:
            output: 出力データ

        Returns:
            検証結果（True = 有効）
        """
        # デフォルトは常に True、サブクラスでオーバーライド可能
        return True

    # ========================================
    # Skill / Prompt 読み込み
    # ========================================

    def _load_skill_prompt(self, skill_name: str | None = None) -> str:
        """SKILL.md 形式のプロンプトを読み込み.

        Args:
            skill_name: Skill 名（None の場合は Agent 名から推定）

        Returns:
            プロンプト内容（YAML frontmatter 除去済み）
        """
        if self._cached_skill_prompt is not None:
            return self._cached_skill_prompt

        # Skill 名を決定
        if skill_name is None:
            # Agent 名から推定: DaoAgent → dao, GatekeeperAgent → gatekeeper
            skill_name = self.name.replace("Agent", "").lower()

        # Skills ディレクトリを探索
        if self._skills_dir and self._skills_dir.exists():
            skill_path = self._skills_dir / skill_name / "SKILL.md"
            if skill_path.exists():
                return self._parse_skill_file(skill_path)

        # フォールバック: 旧プロンプト形式
        return self._load_prompt(skill_name)

    def _parse_skill_file(self, skill_path: Path) -> str:
        """SKILL.md ファイルをパース.

        Args:
            skill_path: Skill ファイルパス

        Returns:
            プロンプト内容
        """
        content = skill_path.read_text(encoding="utf-8")

        # YAML frontmatter を除去
        if content.startswith("---"):
            parts = content.split("---", 2)
            if len(parts) >= 3:
                self._cached_skill_prompt = parts[2].strip()
                return self._cached_skill_prompt

        self._cached_skill_prompt = content.strip()
        return self._cached_skill_prompt

    def _load_prompt(self, prompt_name: str) -> str:
        """プロンプトテンプレートを読み込み（旧方式）.

        Args:
            prompt_name: プロンプトファイル名（拡張子なし）

        Returns:
            プロンプト内容
        """
        if self._prompts_dir is None:
            return ""

        prompt_path = self._prompts_dir / f"{prompt_name}.txt"
        if prompt_path.exists():
            return prompt_path.read_text(encoding="utf-8")
        return ""

    # ========================================
    # LLM 呼び出し
    # ========================================

    async def _call_llm(self, prompt: str) -> str:
        """LLM を呼び出し.

        Args:
            prompt: 入力プロンプト

        Returns:
            LLM 応答テキスト
        """
        if self._llm is None:
            self._logger.warning("LLM クライアント未設定、モック応答を返します")
            return ""

        # LLMProvider.complete() または chat() を使用
        if hasattr(self._llm, "complete"):
            response = await self._llm.complete(
                prompt,
                max_tokens=self.max_tokens,
                temperature=self.temperature,
            )
            return response.get("content", "")
        elif hasattr(self._llm, "chat"):
            response = await self._llm.chat(
                [{"role": "user", "content": prompt}],
                max_tokens=self.max_tokens,
                temperature=self.temperature,
            )
            return response.get("content", "")
        else:
            self._logger.warning("LLM クライアントに有効なメソッドがありません")
            return ""

    @property
    def llm(self) -> Any:
        """LLM クライアントを取得.

        Returns:
            LLM クライアントインスタンス
        """
        return self._llm

    # ========================================
    # デバッグ / モニタリング
    # ========================================

    def get_agent_info(self) -> dict[str, Any]:
        """Agent 情報を取得（デバッグ/モニタリング用）.

        Returns:
            Agent 設定情報
        """
        return {
            "name": self.name,
            "timeout_seconds": self.timeout_seconds,
            "max_retries": self.max_retries,
            "retry_delay": self.retry_delay,
            "retry_backoff": self.retry_backoff,
            "max_tokens": self.max_tokens,
            "temperature": self.temperature,
            "has_llm": self._llm is not None,
        }


# ============================================================
# 後方互換エイリアス
# ============================================================

# BaseDecisionAgent として使用可能（後方互換）
BaseDecisionAgent = ResilientAgent

