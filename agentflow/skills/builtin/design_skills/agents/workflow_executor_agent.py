"""WorkflowExecutorAgent - ComfyUIワークフロー実行Agent.

PromptPlanOutput を受け取り、各画像仕様をComfyUI経由で実行:
1. グローバルスタイル + 画像仕様からワークフローJSONを構築
2. ワークフローをキューに投入
3. 完了までポーリング
4. 生成画像を収集

使用例:
    >>> agent = WorkflowExecutorAgent()
    >>> result = await agent.run({
    ...     "prompt_plan": prompt_plan,
    ...     "output_directory": "/output",
    ... })
"""

import logging
import time
from pathlib import Path
from typing import Any

from agentflow import ResilientAgent
from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
    GeneratedImage,
    WorkflowExecutorInput,
    WorkflowResult,
)
from agentflow.skills.builtin.design_skills.tools.comfyui_client import ComfyUIClient


class WorkflowExecutorAgent(ResilientAgent[WorkflowExecutorInput, WorkflowResult]):
    """プロンプト計画をComfyUI経由で実行し、結果を収集.

    各画像を順次生成し、エラーが発生しても他の画像の生成を継続する。
    """

    name = "WorkflowExecutorAgent"
    timeout_seconds = 600  # バッチ生成のため10分

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)
        self._comfyui = ComfyUIClient()

    def _parse_input(self, input_data: dict[str, Any]) -> WorkflowExecutorInput:
        """入力辞書をWorkflowExecutorInputにパース."""
        return WorkflowExecutorInput(**input_data)

    async def process(self, input_data: WorkflowExecutorInput) -> WorkflowResult:
        """全画像仕様をComfyUI経由で実行."""
        plan = input_data.prompt_plan
        output_dir = input_data.output_directory

        # 出力ディレクトリを確保
        Path(output_dir).mkdir(parents=True, exist_ok=True)

        generated: list[GeneratedImage] = []
        errors: list[str] = []
        start_time = time.monotonic()

        for spec in plan.images:
            try:
                img_start = time.monotonic()

                # ワークフローJSONを構築
                workflow = self._comfyui.build_workflow_payload(
                    plan.global_style,
                    spec,
                )

                # キューに投入して完了を待機
                prompt_id = await self._comfyui.queue_prompt(workflow)
                history = await self._comfyui.poll_until_complete(prompt_id)

                # 出力画像パスを抽出
                file_path = self._extract_output_path(
                    history,
                    spec.image_id,
                    output_dir,
                )

                # ローカル保存が必要な場合にダウンロード
                if input_data.save_locally and file_path:
                    output_info = self._find_output_image(history)
                    if output_info:
                        image_bytes = await self._comfyui.get_image(
                            output_info["filename"],
                            output_info.get("subfolder", ""),
                            output_info.get("type", "output"),
                        )
                        local_path = Path(output_dir) / f"{spec.image_id}.png"
                        local_path.write_bytes(image_bytes)
                        file_path = str(local_path)

                img_time = time.monotonic() - img_start

                generated.append(
                    GeneratedImage(
                        image_id=spec.image_id,
                        role=spec.role,
                        file_path=file_path or f"{output_dir}/{spec.image_id}.png",
                        prompt_used=spec.prompt,
                        seed_used=spec.seed,
                        generation_time_seconds=round(img_time, 2),
                    )
                )
                self._logger.info(
                    f"生成完了: {spec.image_id} ({spec.role.value}) - {img_time:.1f}秒"
                )

            except Exception as e:
                self._logger.exception(f"生成失敗: {spec.image_id}")
                errors.append(f"{spec.image_id}: {e}")

        total_time = time.monotonic() - start_time

        return WorkflowResult(
            images=generated,
            output_directory=output_dir,
            total_generation_time_seconds=round(total_time, 2),
            errors=errors,
        )

    def _find_output_image(self, history: dict[str, Any]) -> dict[str, str] | None:
        """履歴エントリから出力画像情報を検索."""
        outputs = history.get("outputs", {})
        for node_output in outputs.values():
            images = node_output.get("images", [])
            if images:
                return images[0]
        return None

    def _extract_output_path(
        self,
        history: dict[str, Any],
        image_id: str,
        output_dir: str,
    ) -> str:
        """履歴から出力ファイルパスを抽出."""
        info = self._find_output_image(history)
        if info:
            return f"{output_dir}/{info['filename']}"
        return f"{output_dir}/{image_id}.png"
