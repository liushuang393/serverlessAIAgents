"""
Prompt Chaining アーキテクチャパターン

このモジュールは、複数の専用Agentを順次実行するパターンを実装します。
各Agentは特定のタスクに特化し、前のAgentの出力を次のAgentの入力として使用します。
"""

from typing import Any, Dict, List, Optional, Sequence

from ..core.models import ChainResult, Message, MessageRole
from ..utils.logging import get_logger
from .base import Agent, ChainExecutor

logger = get_logger(__name__)


class PromptChain:
    """複数の専用Agentを順次実行するパターン"""

    def __init__(
        self,
        agents: Optional[Sequence[Agent]] = None,
        name: str = "PromptChain",
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        Prompt Chainを初期化する

        Args:
            agents: 順次実行するAgentのリスト（Noneの場合は空リスト）
            name: チェーン名
            config: 設定辞書
        """
        self.name = name
        self.agents: List[Agent] = list(agents) if agents is not None else []
        self.config = config if config is not None else {}
        self.executor = ChainExecutor(self.agents)

        # 設定値
        self.enable_error_recovery = self.config.get("enable_error_recovery", True)
        self.max_retries = self.config.get("max_retries", 2)

        logger.info(f"Prompt Chain '{self.name}' を初期化しました（{len(self.agents)}個のAgent）")

    async def execute(
        self, initial_input: str, context: Optional[Dict[str, Any]] = None
    ) -> ChainResult:
        """
        チェーンを実行する

        Args:
            initial_input: 初期入力
            context: 実行コンテキスト

        Returns:
            ChainResult: チェーン実行結果
        """
        if not self.agents:
            return ChainResult(
                final_output="",
                intermediate_results=[],
                execution_time=0.0,
                success=False,
            )

        logger.info(f"Prompt Chain '{self.name}' の実行を開始します")

        # エラー回復機能が有効な場合は再試行ロジックを使用
        if self.enable_error_recovery:
            return await self._execute_with_retry(initial_input, context or {})
        else:
            return await self.executor.execute_sequential(initial_input, context or {})

    async def _execute_with_retry(
        self, initial_input: str, context: Dict[str, Any]
    ) -> ChainResult:
        """
        再試行機能付きでチェーンを実行する

        Args:
            initial_input: 初期入力
            context: 実行コンテキスト

        Returns:
            ChainResult: チェーン実行結果
        """
        last_result = None

        for attempt in range(self.max_retries + 1):
            try:
                result = await self.executor.execute_sequential(initial_input, context)

                if result.success:
                    logger.info(
                        f"Prompt Chain '{self.name}' が成功しました（試行回数: {attempt + 1}）"
                    )
                    return result
                else:
                    last_result = result
                    if attempt < self.max_retries:
                        logger.warning(
                            f"Prompt Chain '{self.name}' が失敗しました。"
                            f"再試行します（{attempt + 1}/{self.max_retries}）"
                        )

            except Exception as e:
                logger.error(f"Prompt Chain '{self.name}' の実行中にエラーが発生しました: {e}")
                if attempt == self.max_retries:
                    return ChainResult(
                        final_output="",
                        intermediate_results=[],
                        execution_time=0.0,
                        success=False,
                    )

        logger.error(f"Prompt Chain '{self.name}' が最大試行回数後も失敗しました")
        return last_result or ChainResult(
            final_output="", intermediate_results=[], execution_time=0.0, success=False
        )

    def add_agent(self, agent: Agent, position: Optional[int] = None) -> None:
        """
        Agentをチェーンに追加する

        Args:
            agent: 追加するAgent
            position: 挿入位置（Noneの場合は末尾に追加）
        """
        if position is None:
            self.agents.append(agent)
        else:
            self.agents.insert(position, agent)

        # Executorを更新
        self.executor = ChainExecutor(self.agents)

        logger.info(f"Agent '{agent.name}' をチェーンに追加しました")

    def remove_agent(self, agent_name: str) -> bool:
        """
        Agentをチェーンから削除する

        Args:
            agent_name: 削除するAgent名

        Returns:
            bool: 削除が成功したかどうか
        """
        original_count = len(self.agents)
        self.agents = [agent for agent in self.agents if agent.name != agent_name]

        if len(self.agents) < original_count:
            # Executorを更新
            self.executor = ChainExecutor(self.agents)
            logger.info(f"Agent '{agent_name}' をチェーンから削除しました")
            return True

        return False

    def get_agent_names(self) -> List[str]:
        """
        チェーン内のAgent名のリストを取得する

        Returns:
            List[str]: Agent名のリスト
        """
        return [agent.name for agent in self.agents]

    async def get_status(self) -> Dict[str, Any]:
        """
        チェーンの状態を取得する

        Returns:
            Dict[str, Any]: 状態情報
        """
        agent_statuses = []
        for agent in self.agents:
            if hasattr(agent, "get_status"):
                status = await agent.get_status()
            else:
                status = {
                    "name": agent.name,
                    "metrics": agent.get_metrics()
                    if hasattr(agent, "get_metrics")
                    else {},
                }
            agent_statuses.append(status)

        return {
            "name": self.name,
            "config": self.config,
            "agent_count": len(self.agents),
            "agent_names": self.get_agent_names(),
            "agents": agent_statuses,
        }


class SpecializedAgent(Agent):
    """特定のタスクに特化したAgent"""

    def __init__(
        self,
        name: str,
        task_description: str,
        llm_provider: Any = None,
        prompt_template: Optional[str] = None,
        config: Optional[Dict[str, Any]] = None,
    ):
        """
        特化Agentを初期化する

        Args:
            name: Agent名
            task_description: タスクの説明
            llm_provider: LLMプロバイダー
            prompt_template: プロンプトテンプレート
            config: 設定辞書
        """
        super().__init__(name, config)

        self.task_description = task_description
        self.llm = llm_provider
        self.prompt_template = prompt_template or self._default_prompt_template()

        logger.info(f"特化Agent '{self.name}' を初期化しました（タスク: {task_description}）")

    async def process(
        self, input_text: str, context: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        入力を処理して応答を生成する

        Args:
            input_text: 入力テキスト
            context: 追加のコンテキスト情報

        Returns:
            str: 処理結果
        """
        if not self.llm:
            # LLMがない場合は簡単な処理を行う
            return f"[{self.name}] {input_text}"

        try:
            # プロンプトを構築
            prompt = self._build_prompt(input_text, context or {})

            # LLMで処理
            messages = [Message(role=MessageRole.USER, content=prompt)]
            response = await self.llm.generate(messages)

            logger.debug(f"Agent '{self.name}' が処理を完了しました")
            return str(response.content).strip()

        except Exception as e:
            logger.error(f"Agent '{self.name}' の処理中にエラーが発生しました: {e}")
            return f"[{self.name}] エラーが発生しました: {str(e)}"

    def _build_prompt(self, input_text: str, context: Dict[str, Any]) -> str:
        """
        プロンプトを構築する

        Args:
            input_text: 入力テキスト
            context: コンテキスト

        Returns:
            str: 構築されたプロンプト
        """
        context = context or {}

        return self.prompt_template.format(
            task_description=self.task_description,
            input_text=input_text,
            agent_name=self.name,
            **context,
        )

    def _default_prompt_template(self) -> str:
        """
        デフォルトのプロンプトテンプレートを返す

        Returns:
            str: プロンプトテンプレート
        """
        return """
あなたは「{agent_name}」という名前の専門エージェントです。

あなたのタスク: {task_description}

入力: {input_text}

上記のタスクに従って、入力を処理してください。
簡潔で明確な出力を提供してください。
"""


# 事前定義された特化Agent


class SummarizerAgent(SpecializedAgent):
    """要約専門Agent"""

    def __init__(
        self, llm_provider: Any = None, config: Optional[Dict[str, Any]] = None
    ):
        super().__init__(
            name="Summarizer",
            task_description="テキストを簡潔に要約する",
            llm_provider=llm_provider,
            prompt_template="""
あなたは要約の専門家です。

以下のテキストを簡潔に要約してください：

{input_text}

要約は以下の点に注意してください：
- 主要なポイントを含める
- 簡潔で明確にする
- 元の意味を保持する
""",
            config=config,
        )


class TranslatorAgent(SpecializedAgent):
    """翻訳専門Agent"""

    def __init__(
        self,
        target_language: str = "English",
        llm_provider: Any = None,
        config: Optional[Dict[str, Any]] = None,
    ):
        self.target_language = target_language

        super().__init__(
            name="Translator",
            task_description=f"テキストを{target_language}に翻訳する",
            llm_provider=llm_provider,
            prompt_template=f"""
あなたは翻訳の専門家です。

以下のテキストを{target_language}に翻訳してください：

{{input_text}}

翻訳は以下の点に注意してください：
- 自然で流暢な表現にする
- 元の意味とニュアンスを保持する
- 文化的な文脈を考慮する
""",
            config=config,
        )


class AnalyzerAgent(SpecializedAgent):
    """分析専門Agent"""

    def __init__(
        self, llm_provider: Any = None, config: Optional[Dict[str, Any]] = None
    ):
        super().__init__(
            name="Analyzer",
            task_description="テキストを分析して洞察を提供する",
            llm_provider=llm_provider,
            prompt_template="""
あなたは分析の専門家です。

以下のテキストを分析してください：

{input_text}

分析は以下の観点から行ってください：
- 主要なテーマや概念
- 感情や論調
- 構造や論理の流れ
- 重要な洞察や示唆
""",
            config=config,
        )


# 便利な関数


def create_summarization_chain(llm_provider: Any = None) -> PromptChain:
    """要約チェーンを作成する"""
    agents = [AnalyzerAgent(llm_provider), SummarizerAgent(llm_provider)]

    return PromptChain(
        agents=agents,
        name="SummarizationChain",
        config={"enable_error_recovery": True},
    )


def create_translation_chain(
    target_language: str = "English", llm_provider: Any = None
) -> PromptChain:
    """翻訳チェーンを作成する"""
    agents = [
        AnalyzerAgent(llm_provider),
        TranslatorAgent(target_language, llm_provider),
    ]

    return PromptChain(
        agents=agents,
        name="TranslationChain",
        config={"enable_error_recovery": True},
    )
