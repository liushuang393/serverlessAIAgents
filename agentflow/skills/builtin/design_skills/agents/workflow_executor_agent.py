"""WorkflowExecutorAgent - 画像生成ワークフロー実行Agent.

PromptPlanOutput を受け取り、各画像仕様を実行:
1. ComfyUI(ローカル)のヘルスチェック
2. 利用可能ならComfyUI経由で生成、不可ならOpenAI(クラウド)にフォールバック
3. 生成画像を収集

フォールバック優先順位:
  ComfyUI(ローカルGPU) → OpenAI gpt-image-1(クラウド)

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
from agentflow.skills.builtin.design_skills.tools.openai_image_client import (
    OpenAIImageClient,
)


class WorkflowExecutorAgent(ResilientAgent[WorkflowExecutorInput, WorkflowResult]):
    """プロンプト計画を実行し、結果を収集.

    ComfyUIが利用可能ならローカルGPUで生成、
    利用不可ならOpenAI gpt-image-1にフォールバック。
    各画像を順次生成し、エラーが発生しても他の画像の生成を継続する。
    """

    name = "WorkflowExecutorAgent"
    timeout_seconds = 600  # バッチ生成のため10分

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)
        self._comfyui = ComfyUIClient()
        self._openai = OpenAIImageClient()
        self._use_openai = False  # ヘルスチェック後に決定

    def _parse_input(self, input_data: dict[str, Any]) -> WorkflowExecutorInput:
        """入力辞書をWorkflowExecutorInputにパース.
        
        PipelineEngine から渡される場合、prompt_planner_result フィールドに
        PromptPlanOutput が含まれているため、それを prompt_plan フィールドに変換する。
        """
        # PipelineEngine からの入力を処理
        if "prompt_planner_result" in input_data and "prompt_plan" not in input_data:
            prompt_result = input_data["prompt_planner_result"]
            # PromptPlanOutput オブジェクトまたは辞書を prompt_plan フィールドに設定
            if isinstance(prompt_result, dict):
                from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
                    PromptPlanOutput,
                )
                input_data["prompt_plan"] = PromptPlanOutput(**prompt_result)
            else:
                input_data["prompt_plan"] = prompt_result
        
        return WorkflowExecutorInput(**input_data)

    async def _detect_backend(self) -> str:
        """利用可能なバックエンドを検出.

        Returns:
            'comfyui' または 'openai'

        Raises:
            RuntimeError: 両方とも利用不可の場合
        """
        if await self._comfyui.health_check():
            self._use_openai = False
            self._logger.info("バックエンド: ComfyUI(ローカル)")
            return "comfyui"

        self._logger.info("ComfyUI利用不可、OpenAIフォールバックを確認中...")
        if await self._openai.health_check():
            self._use_openai = True
            self._logger.info("バックエンド: OpenAI gpt-image-1(クラウド)")
            return "openai"

        msg = (
            "画像生成バックエンドが利用不可です。"
            "ComfyUI(ローカル)を起動するか、OPENAI_API_KEY を設定してください。"
        )
        raise RuntimeError(msg)

    async def process(self, input_data: WorkflowExecutorInput) -> WorkflowResult:
        """全画像仕様を実行."""
        plan = input_data.prompt_plan
        output_dir = input_data.output_directory

        # 出力ディレクトリを確保
        Path(output_dir).mkdir(parents=True, exist_ok=True)

        # バックエンド検出
        backend = await self._detect_backend()

        generated: list[GeneratedImage] = []
        errors: list[str] = []
        start_time = time.monotonic()

        for spec in plan.images:
            try:
                img_start = time.monotonic()

                if backend == "comfyui":
                    file_path = await self._generate_via_comfyui(
                        plan.global_style, spec, output_dir, input_data.save_locally
                    )
                else:
                    file_path = await self._generate_via_openai(plan.global_style, spec, output_dir)

                img_time = time.monotonic() - img_start

                generated.append(
                    GeneratedImage(
                        image_id=spec.image_id,
                        role=spec.role,
                        file_path=file_path,
                        prompt_used=spec.prompt,
                        seed_used=spec.seed,
                        generation_time_seconds=round(img_time, 2),
                    )
                )
                self._logger.info(
                    f"生成完了: {spec.image_id} ({spec.role.value}) [{backend}] - {img_time:.1f}秒"
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

    # ------------------------------------------------------------------
    # ComfyUI バックエンド
    # ------------------------------------------------------------------

    async def _generate_via_comfyui(
        self,
        global_style: Any,
        spec: Any,
        output_dir: str,
        save_locally: bool,
    ) -> str:
        """ComfyUI経由で1枚生成."""
        workflow = self._comfyui.build_workflow_payload(global_style, spec)
        prompt_id = await self._comfyui.queue_prompt(workflow)
        history = await self._comfyui.poll_until_complete(prompt_id)

        file_path = self._extract_output_path(history, spec.image_id, output_dir)

        if save_locally:
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

        return file_path

    # ------------------------------------------------------------------
    # OpenAI バックエンド
    # ------------------------------------------------------------------

    async def _generate_via_openai(
        self,
        global_style: Any,
        spec: Any,
        output_dir: str,
    ) -> str:
        """OpenAI gpt-image-1経由で1枚生成."""
        # グローバルスタイルを含む完全プロンプトを構築
        style_context = (
            f"{', '.join(global_style.color_palette)} color scheme, "
            f"{global_style.lighting}, {global_style.camera_angle}, "
            f"{global_style.mood}"
        )
        full_prompt = f"{spec.prompt}, {style_context}"

        output_path = Path(output_dir) / f"{spec.image_id}.png"
        await self._openai.generate_and_save(
            full_prompt,
            output_path,
            width=spec.width,
            height=spec.height,
        )
        return str(output_path)

    # ------------------------------------------------------------------
    # ヘルパー
    # ------------------------------------------------------------------

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
