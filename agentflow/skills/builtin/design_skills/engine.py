"""DesignSkillsEngine - デザイン生成パイプラインのメインエンジン.

PipelineEngine パターンを使用したデザイン生成エンジン。

アーキテクチャ(3ステージ順次実行):
    意図解析層: IntentAnalyzer(デザインブリーフ → 構造化意図)
    計画層:     PromptPlanner(構造化意図 → プロンプト計画)
    実行層:     WorkflowExecutor(プロンプト計画 → ComfyUI実行 → 画像群)

実行フロー:
    IntentAnalyzer → PromptPlanner → WorkflowExecutor
         ↓               ↓               ↓
    カテゴリ分類     グローバルスタイル    ComfyUI API
    役割配分        個別プロンプト        画像収集
    特徴抽出        一貫性シード          ローカル保存

使用例:
    >>> from agentflow.skills.builtin.design_skills.engine import DesignSkillsEngine
    >>>
    >>> # 基本使用
    >>> engine = DesignSkillsEngine()
    >>> result = await engine.run({
    ...     "brief": "アウトドアBluetoothスピーカーの商品画像、テクノ風、黒系",
    ...     "num_images": 8,
    ... })
    >>>
    >>> # SSEストリーム
    >>> async for event in engine.run_stream({"brief": "..."}):
    ...     print(event)
"""

import logging
from typing import Any

from agentflow.engines import EngineConfig, PipelineEngine
from agentflow.skills.builtin.design_skills.services.agent_registry import DesignAgentRegistry


class DesignSkillsEngine(PipelineEngine):
    """Design Skills Engine - Lovart風マルチ画像生成エンジン.

    PipelineEngine を継承し、3つの専門Agentを順次実行。
    自然言語のデザインブリーフから統一スタイルの画像セットを生成する。

    Attributes:
        _registry: DesignAgentRegistry インスタンス
        _output_directory: デフォルト出力ディレクトリ

    使用例:
        >>> engine = DesignSkillsEngine()
        >>> result = await engine.run({
        ...     "brief": "Bluetoothスピーカーの商品画像を生成",
        ...     "num_images": 8,
        ... })
    """

    def __init__(
        self,
        llm_client: Any = None,
        output_directory: str = "/tmp/design_output",
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント(省略時は各Agentで自動取得)
            output_directory: デフォルト画像出力ディレクトリ
        """
        self._registry = DesignAgentRegistry(llm_client=llm_client)
        self._output_directory = output_directory

        super().__init__(
            stages=[],  # _setup_stages で動的設定
            max_revisions=0,
            config=EngineConfig(
                name="design-skills-engine",
                enable_events=True,
                enable_memory=False,
                timeout_seconds=600,
            ),
        )
        self._logger = logging.getLogger("design_skills_engine")

    async def _setup_stages(self) -> None:
        """パイプラインステージを動的に設定.

        DesignAgentRegistry からAgentを取得し、stagesを構築。
        PipelineEngine の _setup_stages() フックをオーバーライド。
        """
        await self._registry.initialize()

        self._stage_configs = self._parse_stages(
            [
                {
                    "name": "intent_analyzer",
                    "agent": self._registry.get_agent("intent_analyzer"),
                },
                {
                    "name": "prompt_planner",
                    "agent": self._registry.get_agent("prompt_planner"),
                },
                {
                    "name": "workflow_executor",
                    "agent": self._registry.get_agent("workflow_executor"),
                },
            ]
        )

        # stage_instances を設定(Agent は既に _registry で初期化済み)
        for stage in self._stage_configs:
            instances = []
            if stage.agent:
                instances.append(stage.agent)
            if stage.agents:
                instances.extend(stage.agents)
            self._stage_instances[stage.name] = instances

        self._logger.info("DesignSkillsEngine ステージ設定完了")

    async def run(
        self,
        inputs: dict[str, Any],
        *,
        thread_id: str | None = None,
    ) -> dict[str, Any]:
        """デザインパイプラインを実行.

        Args:
            inputs: 必須: 'brief'。
                任意: 'num_images', 'style_preferences', 'target_platform',
                      'brand_colors', 'aspect_ratio', 'output_directory'

        Returns:
            パイプライン結果(生成画像情報を含む辞書)
        """
        payload = dict(inputs)

        # output_directory を正規化
        output_directory = payload.pop("output_directory", None)
        if isinstance(output_directory, str):
            self._output_directory = output_directory

        result = await super().run(payload, thread_id=thread_id)
        return result if isinstance(result, dict) else {"result": result}


__all__ = ["DesignSkillsEngine"]
