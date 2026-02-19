"""WorkflowExecutorAgent のテスト."""

from unittest.mock import AsyncMock, MagicMock


class TestWorkflowExecutorAgent:
    """WorkflowExecutorAgent ユニットテスト（ComfyUIモック使用）."""

    def _make_mock_comfyui(self):
        """モックComfyUIクライアントを生成."""
        client = MagicMock()
        client.health_check = AsyncMock(return_value=True)
        client.build_workflow_payload = MagicMock(return_value={"prompt": {}})
        client.queue_prompt = AsyncMock(return_value="prompt-001")
        client.poll_until_complete = AsyncMock(
            return_value={
                "outputs": {
                    "7": {
                        "images": [
                            {
                                "filename": "img_001_00001_.png",
                                "subfolder": "",
                                "type": "output",
                            }
                        ]
                    }
                }
            }
        )
        client.get_image = AsyncMock(return_value=b"\x89PNG fake")
        client.close = AsyncMock()
        return client

    def _make_agent(self, mock_comfyui):
        """テスト用Agentインスタンスを生成."""
        from agentflow.skills.builtin.design_skills.agents.workflow_executor_agent import (
            WorkflowExecutorAgent,
        )

        agent = WorkflowExecutorAgent(llm_client=None)
        agent._comfyui = mock_comfyui
        return agent

    def _make_sample_plan(self):
        """テスト用PromptPlanOutputを生成."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
            PromptPlanOutput,
        )

        return PromptPlanOutput(
            design_concept="テクノ風商品撮影",
            global_style=GlobalStyle(
                color_palette=["black"],
                lighting="studio",
                camera_angle="front",
                mood="professional",
                negative_prompt="blurry",
            ),
            images=[
                ImageSpec(
                    image_id="img_001",
                    role=ImageRole.HERO,
                    prompt="Speaker on black background",
                    seed=42,
                ),
            ],
            consistency_seed=42,
        )

    def test_agent_name(self) -> None:
        """Agent名が正しいこと."""
        mock = self._make_mock_comfyui()
        agent = self._make_agent(mock)
        assert agent.name == "WorkflowExecutorAgent"

    async def test_execute_single_image(self, tmp_path) -> None:
        """単一画像の生成が正しく実行されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            WorkflowExecutorInput,
        )

        mock = self._make_mock_comfyui()
        agent = self._make_agent(mock)
        plan = self._make_sample_plan()

        input_data = WorkflowExecutorInput(
            prompt_plan=plan,
            output_directory=str(tmp_path),
        )
        result = await agent.process(input_data)

        assert len(result.images) == 1
        assert result.images[0].image_id == "img_001"
        assert result.images[0].seed_used == 42
        assert result.output_directory == str(tmp_path)
        assert result.total_generation_time_seconds >= 0

    async def test_execute_multiple_images(self, tmp_path) -> None:
        """複数画像の生成が正しく実行されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
            PromptPlanOutput,
            WorkflowExecutorInput,
        )

        mock = self._make_mock_comfyui()
        agent = self._make_agent(mock)

        plan = PromptPlanOutput(
            design_concept="テスト",
            global_style=GlobalStyle(
                color_palette=["black"],
                lighting="studio",
                camera_angle="front",
                mood="pro",
                negative_prompt="blurry",
            ),
            images=[
                ImageSpec(image_id="img_001", role=ImageRole.HERO, prompt="hero", seed=42),
                ImageSpec(image_id="img_002", role=ImageRole.FEATURE, prompt="feature", seed=42),
                ImageSpec(image_id="img_003", role=ImageRole.DETAIL, prompt="detail", seed=42),
            ],
            consistency_seed=42,
        )

        input_data = WorkflowExecutorInput(
            prompt_plan=plan,
            output_directory=str(tmp_path),
        )
        result = await agent.process(input_data)

        assert len(result.images) == 3
        assert result.errors == []

    async def test_handles_comfyui_error_gracefully(self, tmp_path) -> None:
        """ComfyUIエラー時にグレースフルに処理すること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            WorkflowExecutorInput,
        )

        mock = self._make_mock_comfyui()
        mock.queue_prompt = AsyncMock(
            side_effect=Exception("Connection refused"),
        )

        agent = self._make_agent(mock)
        plan = self._make_sample_plan()

        input_data = WorkflowExecutorInput(
            prompt_plan=plan,
            output_directory=str(tmp_path),
        )
        result = await agent.process(input_data)

        # エラーが記録され、空の結果が返ること
        assert len(result.images) == 0
        assert len(result.errors) > 0
        assert "Connection refused" in result.errors[0]

    async def test_partial_failure(self, tmp_path) -> None:
        """一部の画像生成失敗時に他の画像は正常に生成されること."""
        from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
            PromptPlanOutput,
            WorkflowExecutorInput,
        )

        mock = self._make_mock_comfyui()
        # 2回目の呼び出しでエラー
        mock.queue_prompt = AsyncMock(
            side_effect=[
                "prompt-001",
                Exception("Timeout"),
                "prompt-003",
            ],
        )
        agent = self._make_agent(mock)

        plan = PromptPlanOutput(
            design_concept="テスト",
            global_style=GlobalStyle(
                color_palette=["black"],
                lighting="studio",
                camera_angle="front",
                mood="pro",
                negative_prompt="blurry",
            ),
            images=[
                ImageSpec(image_id="img_001", role=ImageRole.HERO, prompt="hero", seed=42),
                ImageSpec(image_id="img_002", role=ImageRole.FEATURE, prompt="feature", seed=42),
                ImageSpec(image_id="img_003", role=ImageRole.DETAIL, prompt="detail", seed=42),
            ],
            consistency_seed=42,
        )

        input_data = WorkflowExecutorInput(
            prompt_plan=plan,
            output_directory=str(tmp_path),
        )
        result = await agent.process(input_data)

        # 2枚成功、1枚失敗
        assert len(result.images) == 2
        assert len(result.errors) == 1
