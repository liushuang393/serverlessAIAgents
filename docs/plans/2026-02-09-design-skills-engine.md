# Design Skills Engine Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a Lovart-style "Design Skills" system that turns natural language design briefs into structured, multi-image generation plans executed via ComfyUI workflows — first as a standalone app, then integrated into the AgentFlow framework.

**Architecture:** A PipelineEngine-based app (`apps/design_skills_engine/`) following the same 8-layer architecture as DecisionGovernanceEngine. Three specialized agents form a pipeline: IntentAnalyzerAgent (understand the design brief) → PromptPlannerAgent (generate structured prompt plan with global style + per-image prompts) → WorkflowExecutorAgent (translate the plan into ComfyUI API calls). A ComfyUI client tool handles the actual HTTP communication with a running ComfyUI server.

**Tech Stack:** Python 3.13+, Pydantic v2 (schemas), AgentFlow framework (PipelineEngine, ResilientAgent, @tool), httpx (async ComfyUI API client), pytest + pytest-asyncio (testing)

---

## Phase 1: Schemas & Data Models

### Task 1: Create the app directory structure

**Files:**
- Create: `apps/design_skills_engine/__init__.py`
- Create: `apps/design_skills_engine/schemas/__init__.py`
- Create: `apps/design_skills_engine/schemas/design_schemas.py`
- Create: `apps/design_skills_engine/agents/__init__.py`
- Create: `apps/design_skills_engine/services/__init__.py`
- Create: `apps/design_skills_engine/tools/__init__.py`
- Test: `tests/unit/test_design_skills_engine.py`

**Step 1: Create directory structure**

```bash
mkdir -p apps/design_skills_engine/{schemas,agents,services,tools,prompts}
touch apps/design_skills_engine/__init__.py
touch apps/design_skills_engine/schemas/__init__.py
touch apps/design_skills_engine/agents/__init__.py
touch apps/design_skills_engine/services/__init__.py
touch apps/design_skills_engine/tools/__init__.py
```

**Step 2: Commit**

```bash
git add apps/design_skills_engine/
git commit -m "feat(dse): scaffold design_skills_engine app directory"
```

---

### Task 2: Define Pydantic schemas for design intent and prompt plan

**Files:**
- Create: `apps/design_skills_engine/schemas/design_schemas.py`
- Test: `tests/unit/test_design_skills_schemas.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_design_skills_schemas.py
"""Tests for design_skills_engine schemas."""
import pytest
from pydantic import ValidationError


class TestDesignBriefInput:
    """DesignBriefInput schema tests."""

    def test_minimal_input(self) -> None:
        from apps.design_skills_engine.schemas.design_schemas import DesignBriefInput

        brief = DesignBriefInput(brief="Create product images for a bluetooth speaker")
        assert brief.brief == "Create product images for a bluetooth speaker"
        assert brief.style_preferences == []
        assert brief.target_platform == ""
        assert brief.num_images == 8

    def test_full_input(self) -> None:
        from apps.design_skills_engine.schemas.design_schemas import DesignBriefInput

        brief = DesignBriefInput(
            brief="Create product images for a bluetooth speaker",
            style_preferences=["tech", "dark", "minimal"],
            target_platform="amazon",
            num_images=6,
            brand_colors=["#000000", "#1E90FF"],
            aspect_ratio="1:1",
        )
        assert brief.num_images == 6
        assert len(brief.brand_colors) == 2

    def test_num_images_bounds(self) -> None:
        from apps.design_skills_engine.schemas.design_schemas import DesignBriefInput

        with pytest.raises(ValidationError):
            DesignBriefInput(brief="test", num_images=0)
        with pytest.raises(ValidationError):
            DesignBriefInput(brief="test", num_images=25)


class TestGlobalStyle:
    """GlobalStyle schema tests."""

    def test_global_style_creation(self) -> None:
        from apps.design_skills_engine.schemas.design_schemas import GlobalStyle

        style = GlobalStyle(
            color_palette=["black", "dark blue", "neon accent"],
            lighting="studio lighting, high contrast",
            camera_angle="product photography, center frame",
            mood="professional, tech-forward",
            negative_prompt="blurry, low quality, text, watermark",
        )
        assert len(style.color_palette) == 3
        assert "studio" in style.lighting


class TestImageSpec:
    """ImageSpec schema tests."""

    def test_image_spec_creation(self) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            ImageRole,
            ImageSpec,
        )

        spec = ImageSpec(
            image_id="img_001",
            role=ImageRole.HERO,
            prompt="Outdoor bluetooth speaker on black background",
            seed=12345,
            weight=1.0,
        )
        assert spec.role == ImageRole.HERO
        assert spec.seed == 12345


class TestPromptPlan:
    """PromptPlan (PromptPlannerOutput) tests."""

    def test_prompt_plan_creation(self) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
            PromptPlanOutput,
        )

        plan = PromptPlanOutput(
            design_concept="Tech-forward product showcase",
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
        assert len(plan.images) == 1
        assert plan.consistency_seed == 42


class TestWorkflowResult:
    """WorkflowResult schema tests."""

    def test_workflow_result(self) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            GeneratedImage,
            ImageRole,
            WorkflowResult,
        )

        result = WorkflowResult(
            images=[
                GeneratedImage(
                    image_id="img_001",
                    role=ImageRole.HERO,
                    file_path="/output/hero_001.png",
                    prompt_used="Speaker on black background",
                    seed_used=42,
                ),
            ],
            output_directory="/output",
            total_generation_time_seconds=45.2,
        )
        assert len(result.images) == 1
        assert result.total_generation_time_seconds == 45.2
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_design_skills_schemas.py -v`
Expected: FAIL with `ModuleNotFoundError: No module named 'apps.design_skills_engine.schemas.design_schemas'`

**Step 3: Write minimal implementation**

```python
# apps/design_skills_engine/schemas/design_schemas.py
"""Design Skills Engine - Pydantic schemas.

Defines the data contracts for the design pipeline:
- DesignBriefInput: User's natural language design brief
- IntentAnalysis: Parsed design intent (IntentAnalyzerAgent output)
- GlobalStyle: Unified style parameters across all images
- ImageSpec: Per-image generation specification
- PromptPlanOutput: Complete prompt plan (PromptPlannerAgent output)
- GeneratedImage: Single generated image result
- WorkflowResult: Full workflow execution result
"""

from enum import Enum

from pydantic import BaseModel, Field


# =============================================================================
# Enums
# =============================================================================


class ImageRole(str, Enum):
    """Image role within the design set."""

    HERO = "HERO"              # Main visual / key visual
    FEATURE = "FEATURE"        # Feature highlight
    DETAIL = "DETAIL"          # Close-up / detail shot
    LIFESTYLE = "LIFESTYLE"    # In-context / lifestyle
    COMPARISON = "COMPARISON"  # Before/after or size comparison
    INFOGRAPHIC = "INFOGRAPHIC"  # Text overlay / spec sheet


class DesignCategory(str, Enum):
    """Design intent category."""

    PRODUCT_PHOTOGRAPHY = "PRODUCT_PHOTOGRAPHY"
    BRAND_IDENTITY = "BRAND_IDENTITY"
    SOCIAL_MEDIA = "SOCIAL_MEDIA"
    ADVERTISING = "ADVERTISING"
    PACKAGING = "PACKAGING"
    UI_MOCKUP = "UI_MOCKUP"


# =============================================================================
# Input schemas
# =============================================================================


class DesignBriefInput(BaseModel):
    """User's design brief - natural language input."""

    brief: str = Field(..., description="Natural language design description")
    style_preferences: list[str] = Field(
        default_factory=list,
        max_length=10,
        description="Style keywords (e.g. 'minimal', 'tech', 'warm')",
    )
    target_platform: str = Field(
        default="",
        description="Target platform (e.g. 'amazon', 'instagram', 'website')",
    )
    num_images: int = Field(
        default=8,
        ge=1,
        le=20,
        description="Number of images to generate (1-20)",
    )
    brand_colors: list[str] = Field(
        default_factory=list,
        max_length=5,
        description="Brand colors as hex codes",
    )
    aspect_ratio: str = Field(
        default="1:1",
        description="Aspect ratio (e.g. '1:1', '16:9', '4:3')",
    )
    reference_image_paths: list[str] = Field(
        default_factory=list,
        max_length=5,
        description="Paths to reference images for style consistency",
    )


# =============================================================================
# IntentAnalyzerAgent output
# =============================================================================


class IntentAnalysis(BaseModel):
    """Parsed design intent - output of IntentAnalyzerAgent."""

    category: DesignCategory = Field(..., description="Design category")
    subject: str = Field(
        ..., max_length=100, description="Main subject (e.g. 'bluetooth speaker')",
    )
    key_features: list[str] = Field(
        ..., max_length=5, description="Key features to highlight (max 5)",
    )
    target_audience: str = Field(
        default="", max_length=100, description="Target audience",
    )
    style_direction: str = Field(
        ..., max_length=200, description="Interpreted style direction",
    )
    image_roles: list[ImageRole] = Field(
        ..., description="Planned image roles for the set",
    )
    platform_constraints: dict[str, str] = Field(
        default_factory=dict,
        description="Platform-specific constraints (e.g. resolution, safe zones)",
    )


# =============================================================================
# PromptPlannerAgent output
# =============================================================================


class GlobalStyle(BaseModel):
    """Global style definition - shared across all images."""

    color_palette: list[str] = Field(
        ..., max_length=5, description="Color palette keywords",
    )
    lighting: str = Field(..., description="Lighting description")
    camera_angle: str = Field(..., description="Default camera angle")
    mood: str = Field(..., description="Overall mood/atmosphere")
    negative_prompt: str = Field(
        ..., description="Global negative prompt (applied to all images)",
    )
    lora_models: list[str] = Field(
        default_factory=list,
        max_length=3,
        description="LoRA model names for style consistency",
    )
    base_model: str = Field(
        default="sd_xl_base_1.0.safetensors",
        description="Base model checkpoint name",
    )


class ImageSpec(BaseModel):
    """Single image generation specification."""

    image_id: str = Field(..., description="Unique image identifier")
    role: ImageRole = Field(..., description="Image role in the set")
    prompt: str = Field(..., description="Image-specific positive prompt")
    seed: int = Field(default=-1, description="Seed (-1 for random)")
    weight: float = Field(
        default=1.0, ge=0.1, le=2.0, description="Prompt weight",
    )
    extra_negative: str = Field(
        default="", description="Additional negative prompt for this image",
    )
    controlnet_image_path: str = Field(
        default="", description="ControlNet reference image path",
    )
    ip_adapter_image_path: str = Field(
        default="", description="IP-Adapter reference image path",
    )
    width: int = Field(default=1024, ge=512, le=2048, description="Image width")
    height: int = Field(default=1024, ge=512, le=2048, description="Image height")
    cfg_scale: float = Field(
        default=7.0, ge=1.0, le=20.0, description="CFG scale",
    )
    steps: int = Field(default=30, ge=10, le=100, description="Sampling steps")
    sampler: str = Field(
        default="euler_ancestral", description="Sampler name",
    )


class PromptPlanOutput(BaseModel):
    """Complete prompt plan - output of PromptPlannerAgent."""

    design_concept: str = Field(
        ..., max_length=200, description="One-sentence design concept",
    )
    global_style: GlobalStyle = Field(..., description="Global style definition")
    images: list[ImageSpec] = Field(
        ..., min_length=1, max_length=20, description="Per-image specifications",
    )
    consistency_seed: int = Field(
        default=-1, description="Global seed for consistency (-1 for random)",
    )


# =============================================================================
# WorkflowExecutorAgent output
# =============================================================================


class GeneratedImage(BaseModel):
    """Single generated image result."""

    image_id: str = Field(..., description="Matches ImageSpec.image_id")
    role: ImageRole = Field(..., description="Image role")
    file_path: str = Field(..., description="Output file path")
    prompt_used: str = Field(..., description="Actual prompt used")
    seed_used: int = Field(..., description="Actual seed used")
    generation_time_seconds: float = Field(
        default=0.0, description="Generation time in seconds",
    )


class WorkflowResult(BaseModel):
    """Full workflow execution result."""

    images: list[GeneratedImage] = Field(..., description="Generated images")
    output_directory: str = Field(..., description="Output directory path")
    total_generation_time_seconds: float = Field(
        ..., description="Total generation time",
    )
    errors: list[str] = Field(
        default_factory=list, description="Non-fatal errors encountered",
    )
```

**Step 4: Update schemas __init__.py**

```python
# apps/design_skills_engine/schemas/__init__.py
"""Design Skills Engine schemas."""

from apps.design_skills_engine.schemas.design_schemas import (
    DesignBriefInput,
    DesignCategory,
    GeneratedImage,
    GlobalStyle,
    ImageRole,
    ImageSpec,
    IntentAnalysis,
    PromptPlanOutput,
    WorkflowResult,
)

__all__ = [
    "DesignBriefInput",
    "DesignCategory",
    "GeneratedImage",
    "GlobalStyle",
    "ImageRole",
    "ImageSpec",
    "IntentAnalysis",
    "PromptPlanOutput",
    "WorkflowResult",
]
```

**Step 5: Run test to verify it passes**

Run: `pytest tests/unit/test_design_skills_schemas.py -v`
Expected: All 6 tests PASS

**Step 6: Commit**

```bash
git add apps/design_skills_engine/schemas/ tests/unit/test_design_skills_schemas.py
git commit -m "feat(dse): add Pydantic schemas for design pipeline"
```

---

## Phase 2: ComfyUI Client Tool

### Task 3: Create the ComfyUI async HTTP client tool

**Files:**
- Create: `apps/design_skills_engine/tools/comfyui_client.py`
- Test: `tests/unit/test_comfyui_client.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_comfyui_client.py
"""Tests for ComfyUI client tool."""
import json
from unittest.mock import AsyncMock, MagicMock, patch

import pytest


class TestComfyUIClient:
    """ComfyUI client tests (mocked HTTP)."""

    @pytest.fixture
    def client(self):
        from apps.design_skills_engine.tools.comfyui_client import ComfyUIClient

        return ComfyUIClient(base_url="http://localhost:8188")

    @pytest.mark.asyncio
    async def test_build_workflow_payload(self, client) -> None:
        """Test workflow JSON construction from ImageSpec."""
        from apps.design_skills_engine.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
        )

        style = GlobalStyle(
            color_palette=["black"],
            lighting="studio lighting",
            camera_angle="front",
            mood="professional",
            negative_prompt="blurry, low quality",
        )
        spec = ImageSpec(
            image_id="img_001",
            role=ImageRole.HERO,
            prompt="Speaker on black background",
            seed=42,
            width=1024,
            height=1024,
            cfg_scale=7.0,
            steps=30,
            sampler="euler_ancestral",
        )
        payload = client.build_workflow_payload(style, spec)

        assert isinstance(payload, dict)
        assert "prompt" in payload
        # Verify positive prompt merges global style + image prompt
        ksampler = None
        for node in payload["prompt"].values():
            if node.get("class_type") == "KSampler":
                ksampler = node
                break
        assert ksampler is not None
        assert ksampler["inputs"]["seed"] == 42
        assert ksampler["inputs"]["steps"] == 30
        assert ksampler["inputs"]["cfg"] == 7.0

    @pytest.mark.asyncio
    async def test_queue_prompt_success(self, client) -> None:
        """Test queuing a prompt returns prompt_id."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"prompt_id": "abc-123"}

        with patch.object(client, "_http_client") as mock_http:
            mock_http.post = AsyncMock(return_value=mock_response)
            prompt_id = await client.queue_prompt({"prompt": {}})

        assert prompt_id == "abc-123"

    @pytest.mark.asyncio
    async def test_get_image_returns_bytes(self, client) -> None:
        """Test fetching generated image returns bytes."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.content = b"\x89PNG fake image data"

        with patch.object(client, "_http_client") as mock_http:
            mock_http.get = AsyncMock(return_value=mock_response)
            data = await client.get_image("test.png", "subfolder", "output")

        assert data == b"\x89PNG fake image data"

    @pytest.mark.asyncio
    async def test_health_check(self, client) -> None:
        """Test health check pings ComfyUI."""
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = {"exec_info": {"queue_remaining": 0}}

        with patch.object(client, "_http_client") as mock_http:
            mock_http.get = AsyncMock(return_value=mock_response)
            healthy = await client.health_check()

        assert healthy is True
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_comfyui_client.py -v`
Expected: FAIL with `ModuleNotFoundError`

**Step 3: Write minimal implementation**

```python
# apps/design_skills_engine/tools/comfyui_client.py
"""ComfyUI async HTTP client.

Communicates with a running ComfyUI server to:
- Queue image generation prompts
- Poll for completion
- Download generated images

Reference: ComfyUI API docs (POST /prompt, GET /history, GET /view)
"""

import asyncio
import logging
import os
import time
from typing import Any

import httpx

from apps.design_skills_engine.schemas.design_schemas import (
    GlobalStyle,
    ImageSpec,
)

logger = logging.getLogger(__name__)


class ComfyUIClient:
    """Async HTTP client for ComfyUI server.

    Args:
        base_url: ComfyUI server URL (default: from COMFYUI_URL env var)
        timeout: HTTP timeout in seconds
    """

    def __init__(
        self,
        base_url: str | None = None,
        timeout: float = 120.0,
    ) -> None:
        self._base_url = base_url or os.getenv("COMFYUI_URL", "http://localhost:8188")
        self._http_client = httpx.AsyncClient(
            base_url=self._base_url,
            timeout=timeout,
        )
        self._logger = logging.getLogger("design_skills.comfyui_client")

    async def close(self) -> None:
        """Close the HTTP client."""
        await self._http_client.aclose()

    def build_workflow_payload(
        self,
        style: GlobalStyle,
        spec: ImageSpec,
    ) -> dict[str, Any]:
        """Build ComfyUI API workflow JSON from style + image spec.

        Constructs a standard SDXL txt2img workflow with:
        - CheckpointLoaderSimple -> model
        - CLIPTextEncode (positive) -> merged global + image prompt
        - CLIPTextEncode (negative) -> global negative + image-specific negative
        - KSampler -> generation parameters
        - VAEDecode -> decode latent
        - SaveImage -> output

        Args:
            style: Global style definition
            spec: Per-image specification

        Returns:
            ComfyUI-compatible workflow dict
        """
        # Merge positive prompt: global context + image-specific
        style_context = (
            f"{', '.join(style.color_palette)} color scheme, "
            f"{style.lighting}, {style.camera_angle}, {style.mood}"
        )
        positive_prompt = f"{spec.prompt}, {style_context}"

        # Merge negative prompt
        negative_prompt = style.negative_prompt
        if spec.extra_negative:
            negative_prompt = f"{negative_prompt}, {spec.extra_negative}"

        workflow: dict[str, Any] = {
            "prompt": {
                "1": {
                    "class_type": "CheckpointLoaderSimple",
                    "inputs": {
                        "ckpt_name": style.base_model,
                    },
                },
                "2": {
                    "class_type": "CLIPTextEncode",
                    "inputs": {
                        "text": positive_prompt,
                        "clip": ["1", 1],
                    },
                },
                "3": {
                    "class_type": "CLIPTextEncode",
                    "inputs": {
                        "text": negative_prompt,
                        "clip": ["1", 1],
                    },
                },
                "4": {
                    "class_type": "EmptyLatentImage",
                    "inputs": {
                        "width": spec.width,
                        "height": spec.height,
                        "batch_size": 1,
                    },
                },
                "5": {
                    "class_type": "KSampler",
                    "inputs": {
                        "model": ["1", 0],
                        "positive": ["2", 0],
                        "negative": ["3", 0],
                        "latent_image": ["4", 0],
                        "seed": spec.seed,
                        "steps": spec.steps,
                        "cfg": spec.cfg_scale,
                        "sampler_name": spec.sampler,
                        "scheduler": "normal",
                        "denoise": 1.0,
                    },
                },
                "6": {
                    "class_type": "VAEDecode",
                    "inputs": {
                        "samples": ["5", 0],
                        "vae": ["1", 2],
                    },
                },
                "7": {
                    "class_type": "SaveImage",
                    "inputs": {
                        "images": ["6", 0],
                        "filename_prefix": spec.image_id,
                    },
                },
            },
        }
        return workflow

    async def queue_prompt(self, workflow: dict[str, Any]) -> str:
        """Queue a workflow for execution.

        Args:
            workflow: ComfyUI workflow dict (from build_workflow_payload)

        Returns:
            prompt_id from ComfyUI

        Raises:
            httpx.HTTPStatusError: If the server returns an error
        """
        response = await self._http_client.post("/prompt", json=workflow)
        response.raise_for_status()
        data = response.json()
        prompt_id: str = data["prompt_id"]
        self._logger.info(f"Queued prompt: {prompt_id}")
        return prompt_id

    async def poll_until_complete(
        self,
        prompt_id: str,
        poll_interval: float = 1.0,
        max_wait: float = 300.0,
    ) -> dict[str, Any]:
        """Poll ComfyUI history until the prompt completes.

        Args:
            prompt_id: The prompt ID to wait for
            poll_interval: Seconds between polls
            max_wait: Maximum wait time in seconds

        Returns:
            History entry for the completed prompt

        Raises:
            TimeoutError: If max_wait is exceeded
        """
        start = time.monotonic()
        while (time.monotonic() - start) < max_wait:
            response = await self._http_client.get(f"/history/{prompt_id}")
            if response.status_code == 200:
                history = response.json()
                if prompt_id in history:
                    return history[prompt_id]
            await asyncio.sleep(poll_interval)

        msg = f"ComfyUI prompt {prompt_id} did not complete within {max_wait}s"
        raise TimeoutError(msg)

    async def get_image(
        self,
        filename: str,
        subfolder: str = "",
        folder_type: str = "output",
    ) -> bytes:
        """Download a generated image from ComfyUI.

        Args:
            filename: Image filename
            subfolder: Subfolder within the output directory
            folder_type: Folder type ('output', 'input', 'temp')

        Returns:
            Image bytes
        """
        params = {
            "filename": filename,
            "subfolder": subfolder,
            "type": folder_type,
        }
        response = await self._http_client.get("/view", params=params)
        response.raise_for_status()
        return response.content

    async def health_check(self) -> bool:
        """Check if ComfyUI server is reachable.

        Returns:
            True if server responds, False otherwise
        """
        try:
            response = await self._http_client.get("/prompt")
            return response.status_code == 200
        except httpx.HTTPError:
            return False
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/test_comfyui_client.py -v`
Expected: All 4 tests PASS

**Step 5: Commit**

```bash
git add apps/design_skills_engine/tools/ tests/unit/test_comfyui_client.py
git commit -m "feat(dse): add async ComfyUI HTTP client with workflow builder"
```

---

## Phase 3: Agents

### Task 4: Create IntentAnalyzerAgent

**Files:**
- Create: `apps/design_skills_engine/agents/intent_analyzer_agent.py`
- Test: `tests/unit/test_intent_analyzer_agent.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_intent_analyzer_agent.py
"""Tests for IntentAnalyzerAgent."""
import pytest


class TestIntentAnalyzerAgent:
    """IntentAnalyzerAgent unit tests (rule-based, no LLM)."""

    @pytest.fixture
    def agent(self):
        from apps.design_skills_engine.agents.intent_analyzer_agent import (
            IntentAnalyzerAgent,
        )
        return IntentAnalyzerAgent(llm_client=None)

    def test_agent_name(self, agent) -> None:
        assert agent.name == "IntentAnalyzerAgent"

    @pytest.mark.asyncio
    async def test_process_product_brief(self, agent) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            DesignBriefInput,
            DesignCategory,
        )

        input_data = DesignBriefInput(
            brief="Create product images for an outdoor bluetooth speaker, "
                  "tech style, black and blue colors",
            style_preferences=["tech", "dark"],
            target_platform="amazon",
            num_images=6,
        )
        result = await agent.process(input_data)

        assert result.category == DesignCategory.PRODUCT_PHOTOGRAPHY
        assert result.subject  # non-empty
        assert len(result.image_roles) == 6
        assert len(result.key_features) >= 1

    @pytest.mark.asyncio
    async def test_process_social_media_brief(self, agent) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            DesignBriefInput,
            DesignCategory,
        )

        input_data = DesignBriefInput(
            brief="Design Instagram posts for a coffee brand campaign",
            target_platform="instagram",
            num_images=4,
        )
        result = await agent.process(input_data)

        assert result.category == DesignCategory.SOCIAL_MEDIA
        assert len(result.image_roles) == 4

    @pytest.mark.asyncio
    async def test_default_image_role_distribution(self, agent) -> None:
        """8 images should include 1 hero, features, details, lifestyle."""
        from apps.design_skills_engine.schemas.design_schemas import (
            DesignBriefInput,
            ImageRole,
        )

        input_data = DesignBriefInput(
            brief="Product images for a laptop stand",
            num_images=8,
        )
        result = await agent.process(input_data)

        roles = [r for r in result.image_roles]
        assert ImageRole.HERO in roles
        assert ImageRole.FEATURE in roles
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_intent_analyzer_agent.py -v`
Expected: FAIL with `ModuleNotFoundError`

**Step 3: Write minimal implementation**

```python
# apps/design_skills_engine/agents/intent_analyzer_agent.py
"""IntentAnalyzerAgent - Design intent understanding.

Analyzes a natural language design brief and extracts:
- Design category (product, brand, social media, etc.)
- Main subject and key features
- Target audience and style direction
- Image role distribution for the set
"""

import logging
from typing import Any

from apps.design_skills_engine.schemas.design_schemas import (
    DesignBriefInput,
    DesignCategory,
    ImageRole,
    IntentAnalysis,
)

from agentflow import ResilientAgent


class IntentAnalyzerAgent(ResilientAgent[DesignBriefInput, IntentAnalysis]):
    """Analyze design brief and extract structured intent.

    Rule-based fallback when LLM is unavailable.
    LLM-powered analysis when available.
    """

    name = "IntentAnalyzerAgent"
    temperature = 0.3

    # Category detection keywords
    CATEGORY_KEYWORDS: dict[DesignCategory, list[str]] = {
        DesignCategory.PRODUCT_PHOTOGRAPHY: [
            "product", "商品", "goods", "item", "speaker", "phone",
            "amazon", "shopify", "e-commerce", "ecommerce",
        ],
        DesignCategory.BRAND_IDENTITY: [
            "brand", "logo", "identity", "ブランド", "CI",
        ],
        DesignCategory.SOCIAL_MEDIA: [
            "instagram", "tiktok", "social", "post", "story",
            "SNS", "ソーシャル", "投稿",
        ],
        DesignCategory.ADVERTISING: [
            "ad", "advertisement", "campaign", "banner", "広告",
        ],
        DesignCategory.PACKAGING: [
            "package", "packaging", "box", "label", "パッケージ",
        ],
        DesignCategory.UI_MOCKUP: [
            "UI", "mockup", "wireframe", "app design", "画面",
        ],
    }

    # Default role distribution templates
    ROLE_DISTRIBUTIONS: dict[int, list[ImageRole]] = {
        1: [ImageRole.HERO],
        2: [ImageRole.HERO, ImageRole.FEATURE],
        3: [ImageRole.HERO, ImageRole.FEATURE, ImageRole.DETAIL],
        4: [ImageRole.HERO, ImageRole.FEATURE, ImageRole.FEATURE, ImageRole.LIFESTYLE],
    }

    def __init__(self, llm_client: Any = None) -> None:
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    def _parse_input(self, input_data: dict[str, Any]) -> DesignBriefInput:
        return DesignBriefInput(**input_data)

    async def process(self, input_data: DesignBriefInput) -> IntentAnalysis:
        """Analyze the design brief."""
        if self._llm:
            return await self._analyze_with_llm(input_data)
        return self._analyze_rule_based(input_data)

    def _infer_category(self, brief: str, platform: str) -> DesignCategory:
        """Infer design category from brief text and platform."""
        text = f"{brief} {platform}".lower()
        for category, keywords in self.CATEGORY_KEYWORDS.items():
            if any(kw.lower() in text for kw in keywords):
                return category
        return DesignCategory.PRODUCT_PHOTOGRAPHY

    def _distribute_roles(self, num_images: int) -> list[ImageRole]:
        """Distribute image roles based on count."""
        if num_images in self.ROLE_DISTRIBUTIONS:
            return self.ROLE_DISTRIBUTIONS[num_images]

        # For 5+ images: 1 hero + features + details + lifestyles
        roles: list[ImageRole] = [ImageRole.HERO]
        remaining = num_images - 1
        feature_count = min(remaining // 2, 4)
        detail_count = min((remaining - feature_count) // 2, 3)
        lifestyle_count = remaining - feature_count - detail_count

        roles.extend([ImageRole.FEATURE] * feature_count)
        roles.extend([ImageRole.DETAIL] * detail_count)
        roles.extend([ImageRole.LIFESTYLE] * lifestyle_count)
        return roles[:num_images]

    def _extract_subject(self, brief: str) -> str:
        """Extract the main subject from the brief."""
        # Simple heuristic: look for 'for a/an X' pattern
        lower = brief.lower()
        for marker in ["for a ", "for an ", "of a ", "of an ", "for "]:
            if marker in lower:
                idx = lower.index(marker) + len(marker)
                # Take next ~5 words
                words = brief[idx:].split()[:5]
                return " ".join(words).rstrip(",.")
        # Fallback: first few meaningful words
        return " ".join(brief.split()[:6])

    def _extract_features(self, brief: str) -> list[str]:
        """Extract key features from the brief."""
        features: list[str] = []
        # Look for feature-indicating words
        feature_indicators = [
            "emphasize", "highlight", "featuring", "with",
            "強調", "特徴",
        ]
        lower = brief.lower()
        for indicator in feature_indicators:
            if indicator in lower:
                idx = lower.index(indicator) + len(indicator)
                chunk = brief[idx:].split(",")[0].strip()
                if chunk:
                    features.append(chunk[:50])
        if not features:
            features.append("main product view")
        return features[:5]

    def _analyze_rule_based(self, input_data: DesignBriefInput) -> IntentAnalysis:
        """Rule-based analysis (no LLM)."""
        category = self._infer_category(input_data.brief, input_data.target_platform)
        subject = self._extract_subject(input_data.brief)
        features = self._extract_features(input_data.brief)
        roles = self._distribute_roles(input_data.num_images)

        style_parts: list[str] = []
        if input_data.style_preferences:
            style_parts.extend(input_data.style_preferences)
        style_parts.append(f"{category.value.lower().replace('_', ' ')} style")
        style_direction = ", ".join(style_parts)

        platform_constraints: dict[str, str] = {}
        if input_data.target_platform == "amazon":
            platform_constraints["background"] = "pure white recommended"
            platform_constraints["min_resolution"] = "1000x1000"
        elif input_data.target_platform == "instagram":
            platform_constraints["aspect_ratio"] = "1:1 or 4:5"
            platform_constraints["safe_zone"] = "avoid text in outer 10%"

        return IntentAnalysis(
            category=category,
            subject=subject,
            key_features=features,
            target_audience="",
            style_direction=style_direction,
            image_roles=roles,
            platform_constraints=platform_constraints,
        )

    async def _analyze_with_llm(self, input_data: DesignBriefInput) -> IntentAnalysis:
        """LLM-powered analysis."""
        from agentflow.utils import extract_json

        system_prompt = """You are a design intent analyzer. Given a design brief, extract:
1. category: PRODUCT_PHOTOGRAPHY, BRAND_IDENTITY, SOCIAL_MEDIA, ADVERTISING, PACKAGING, or UI_MOCKUP
2. subject: The main subject (max 100 chars)
3. key_features: Up to 5 features to highlight
4. target_audience: Who this is for
5. style_direction: Interpreted style (max 200 chars)
6. image_roles: Array of roles (HERO, FEATURE, DETAIL, LIFESTYLE, COMPARISON, INFOGRAPHIC)

Output JSON only."""

        user_prompt = f"""Brief: {input_data.brief}
Style preferences: {', '.join(input_data.style_preferences) if input_data.style_preferences else 'none'}
Platform: {input_data.target_platform or 'general'}
Number of images: {input_data.num_images}"""

        response = await self._call_llm(f"{system_prompt}\n\n{user_prompt}")
        data = extract_json(response)

        if data is None:
            return self._analyze_rule_based(input_data)

        try:
            return IntentAnalysis(
                category=DesignCategory(data.get("category", "PRODUCT_PHOTOGRAPHY")),
                subject=data.get("subject", "")[:100],
                key_features=data.get("key_features", ["main view"])[:5],
                target_audience=data.get("target_audience", "")[:100],
                style_direction=data.get("style_direction", "")[:200],
                image_roles=[
                    ImageRole(r) for r in data.get("image_roles", ["HERO"])
                ][:input_data.num_images],
                platform_constraints=data.get("platform_constraints", {}),
            )
        except (ValueError, KeyError):
            return self._analyze_rule_based(input_data)
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/test_intent_analyzer_agent.py -v`
Expected: All 4 tests PASS

**Step 5: Commit**

```bash
git add apps/design_skills_engine/agents/intent_analyzer_agent.py tests/unit/test_intent_analyzer_agent.py
git commit -m "feat(dse): add IntentAnalyzerAgent with rule-based + LLM analysis"
```

---

### Task 5: Create PromptPlannerAgent

**Files:**
- Create: `apps/design_skills_engine/agents/prompt_planner_agent.py`
- Test: `tests/unit/test_prompt_planner_agent.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_prompt_planner_agent.py
"""Tests for PromptPlannerAgent."""
import pytest


class TestPromptPlannerAgent:
    """PromptPlannerAgent unit tests (rule-based, no LLM)."""

    @pytest.fixture
    def agent(self):
        from apps.design_skills_engine.agents.prompt_planner_agent import (
            PromptPlannerAgent,
        )
        return PromptPlannerAgent(llm_client=None)

    @pytest.fixture
    def sample_intent(self):
        from apps.design_skills_engine.schemas.design_schemas import (
            DesignCategory,
            ImageRole,
            IntentAnalysis,
        )
        return IntentAnalysis(
            category=DesignCategory.PRODUCT_PHOTOGRAPHY,
            subject="outdoor bluetooth speaker",
            key_features=["waterproof", "high quality audio", "portable"],
            target_audience="outdoor enthusiasts",
            style_direction="tech, dark, minimal",
            image_roles=[
                ImageRole.HERO,
                ImageRole.FEATURE,
                ImageRole.FEATURE,
                ImageRole.DETAIL,
            ],
        )

    def test_agent_name(self, agent) -> None:
        assert agent.name == "PromptPlannerAgent"

    @pytest.mark.asyncio
    async def test_generates_prompt_plan(self, agent, sample_intent) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            PromptPlanInput,
        )

        input_data = PromptPlanInput(
            intent=sample_intent,
            brand_colors=["#000000", "#1E90FF"],
            aspect_ratio="1:1",
        )
        result = await agent.process(input_data)

        assert result.design_concept  # non-empty
        assert result.global_style is not None
        assert len(result.images) == 4  # matches intent.image_roles count
        assert result.global_style.negative_prompt  # non-empty

    @pytest.mark.asyncio
    async def test_all_images_share_seed(self, agent, sample_intent) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            PromptPlanInput,
        )

        input_data = PromptPlanInput(
            intent=sample_intent,
            brand_colors=[],
        )
        result = await agent.process(input_data)

        # Consistency seed should be set
        assert result.consistency_seed > 0
        # All images should use the consistency seed
        for img in result.images:
            assert img.seed == result.consistency_seed

    @pytest.mark.asyncio
    async def test_hero_image_prompt_distinct(self, agent, sample_intent) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            ImageRole,
            PromptPlanInput,
        )

        input_data = PromptPlanInput(intent=sample_intent)
        result = await agent.process(input_data)

        hero = [img for img in result.images if img.role == ImageRole.HERO]
        assert len(hero) == 1
        assert "center" in hero[0].prompt.lower() or "main" in hero[0].prompt.lower() or sample_intent.subject.lower() in hero[0].prompt.lower()
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_prompt_planner_agent.py -v`
Expected: FAIL with `ModuleNotFoundError`

**Step 3: Write minimal implementation**

First add `PromptPlanInput` to schemas:

```python
# Append to apps/design_skills_engine/schemas/design_schemas.py (after PromptPlanOutput)

class PromptPlanInput(BaseModel):
    """Input for PromptPlannerAgent."""

    intent: IntentAnalysis = Field(..., description="Analyzed design intent")
    brand_colors: list[str] = Field(
        default_factory=list, description="Brand colors as hex codes",
    )
    aspect_ratio: str = Field(default="1:1", description="Target aspect ratio")
    reference_image_paths: list[str] = Field(
        default_factory=list, description="Reference images for style matching",
    )
```

Then create the agent:

```python
# apps/design_skills_engine/agents/prompt_planner_agent.py
"""PromptPlannerAgent - Structured prompt plan generation.

Converts an IntentAnalysis into a complete PromptPlan with:
- Global style (unified across all images)
- Per-image prompts (role-specific)
- Consistency controls (shared seed, LoRA)
"""

import logging
import random
from typing import Any

from apps.design_skills_engine.schemas.design_schemas import (
    GlobalStyle,
    ImageRole,
    ImageSpec,
    PromptPlanInput,
    PromptPlanOutput,
)

from agentflow import ResilientAgent


class PromptPlannerAgent(ResilientAgent[PromptPlanInput, PromptPlanOutput]):
    """Generate structured prompt plan from design intent."""

    name = "PromptPlannerAgent"
    temperature = 0.5

    # Role-specific prompt templates
    ROLE_TEMPLATES: dict[ImageRole, str] = {
        ImageRole.HERO: (
            "{subject}, center frame, main product shot, "
            "dramatic composition, studio backdrop"
        ),
        ImageRole.FEATURE: (
            "{subject}, {feature}, feature highlight, "
            "focused detail, contextual setting"
        ),
        ImageRole.DETAIL: (
            "{subject}, extreme close-up, macro detail, "
            "texture visible, studio lighting"
        ),
        ImageRole.LIFESTYLE: (
            "{subject}, in use, real-world setting, "
            "natural environment, candid feel"
        ),
        ImageRole.COMPARISON: (
            "{subject}, size comparison, with everyday objects, "
            "scale reference"
        ),
        ImageRole.INFOGRAPHIC: (
            "{subject}, clean background, space for text overlay, "
            "minimal composition"
        ),
    }

    def __init__(self, llm_client: Any = None) -> None:
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    def _parse_input(self, input_data: dict[str, Any]) -> PromptPlanInput:
        return PromptPlanInput(**input_data)

    async def process(self, input_data: PromptPlanInput) -> PromptPlanOutput:
        """Generate the prompt plan."""
        if self._llm:
            return await self._plan_with_llm(input_data)
        return self._plan_rule_based(input_data)

    def _build_global_style(self, input_data: PromptPlanInput) -> GlobalStyle:
        """Build global style from intent analysis."""
        intent = input_data.intent

        # Color palette
        if input_data.brand_colors:
            palette = input_data.brand_colors[:5]
        else:
            palette = ["neutral tones"]

        # Style-dependent lighting
        style_lower = intent.style_direction.lower()
        if "dark" in style_lower or "tech" in style_lower:
            lighting = "studio lighting, high contrast, dramatic shadows"
        elif "warm" in style_lower or "cozy" in style_lower:
            lighting = "warm golden hour lighting, soft shadows"
        else:
            lighting = "clean studio lighting, even illumination"

        # Camera angle by category
        camera_angles = {
            "PRODUCT_PHOTOGRAPHY": "product photography, 45-degree angle",
            "SOCIAL_MEDIA": "eye-level, lifestyle photography",
            "BRAND_IDENTITY": "centered, symmetrical composition",
        }
        camera = camera_angles.get(
            intent.category.value, "professional photography"
        )

        return GlobalStyle(
            color_palette=palette,
            lighting=lighting,
            camera_angle=camera,
            mood=intent.style_direction,
            negative_prompt=(
                "blurry, low quality, text, watermark, deformed, "
                "ugly, oversaturated, cropped, out of frame"
            ),
        )

    def _build_image_specs(
        self,
        input_data: PromptPlanInput,
        seed: int,
    ) -> list[ImageSpec]:
        """Build per-image specifications."""
        intent = input_data.intent
        specs: list[ImageSpec] = []
        feature_idx = 0

        for i, role in enumerate(intent.image_roles):
            template = self.ROLE_TEMPLATES.get(role, "{subject}")

            # Pick a feature for FEATURE roles
            feature = ""
            if role == ImageRole.FEATURE and intent.key_features:
                feature = intent.key_features[feature_idx % len(intent.key_features)]
                feature_idx += 1

            prompt = template.format(
                subject=intent.subject,
                feature=feature,
            )

            # Resolve aspect ratio to dimensions
            width, height = self._resolve_dimensions(input_data.aspect_ratio)

            specs.append(ImageSpec(
                image_id=f"img_{i + 1:03d}",
                role=role,
                prompt=prompt,
                seed=seed,
                width=width,
                height=height,
            ))

        return specs

    def _resolve_dimensions(self, aspect_ratio: str) -> tuple[int, int]:
        """Convert aspect ratio string to pixel dimensions."""
        ratio_map: dict[str, tuple[int, int]] = {
            "1:1": (1024, 1024),
            "16:9": (1344, 768),
            "9:16": (768, 1344),
            "4:3": (1152, 896),
            "3:4": (896, 1152),
            "4:5": (896, 1120),
        }
        return ratio_map.get(aspect_ratio, (1024, 1024))

    def _plan_rule_based(self, input_data: PromptPlanInput) -> PromptPlanOutput:
        """Rule-based prompt planning."""
        global_style = self._build_global_style(input_data)
        seed = random.randint(1, 2**31 - 1)
        images = self._build_image_specs(input_data, seed)

        return PromptPlanOutput(
            design_concept=(
                f"{input_data.intent.category.value.replace('_', ' ').title()} "
                f"for {input_data.intent.subject}"
            ),
            global_style=global_style,
            images=images,
            consistency_seed=seed,
        )

    async def _plan_with_llm(self, input_data: PromptPlanInput) -> PromptPlanOutput:
        """LLM-powered prompt planning (enhances prompts)."""
        # Start with rule-based plan, then enhance prompts via LLM
        base_plan = self._plan_rule_based(input_data)

        system_prompt = """You are an expert Stable Diffusion prompt engineer.
Given a base prompt plan, enhance each image prompt for maximum quality.
Keep the same structure, just improve the prompt text.
Output JSON with field "enhanced_prompts": [{"image_id": "...", "prompt": "..."}]"""

        prompts_summary = "\n".join(
            f"- {img.image_id} ({img.role.value}): {img.prompt}"
            for img in base_plan.images
        )
        user_prompt = f"""Subject: {input_data.intent.subject}
Style: {input_data.intent.style_direction}
Features: {', '.join(input_data.intent.key_features)}

Base prompts:
{prompts_summary}"""

        try:
            from agentflow.utils import extract_json

            response = await self._call_llm(f"{system_prompt}\n\n{user_prompt}")
            data = extract_json(response)

            if data and "enhanced_prompts" in data:
                prompt_map = {
                    ep["image_id"]: ep["prompt"]
                    for ep in data["enhanced_prompts"]
                    if "image_id" in ep and "prompt" in ep
                }
                for img in base_plan.images:
                    if img.image_id in prompt_map:
                        img.prompt = prompt_map[img.image_id]
        except Exception:
            self._logger.warning("LLM prompt enhancement failed, using rule-based")

        return base_plan
```

**Step 4: Update schemas/__init__.py to export PromptPlanInput**

Add `PromptPlanInput` to the `__all__` list and import.

**Step 5: Run test to verify it passes**

Run: `pytest tests/unit/test_prompt_planner_agent.py -v`
Expected: All 4 tests PASS

**Step 6: Commit**

```bash
git add apps/design_skills_engine/agents/prompt_planner_agent.py \
       apps/design_skills_engine/schemas/design_schemas.py \
       apps/design_skills_engine/schemas/__init__.py \
       tests/unit/test_prompt_planner_agent.py
git commit -m "feat(dse): add PromptPlannerAgent with style + prompt generation"
```

---

### Task 6: Create WorkflowExecutorAgent

**Files:**
- Create: `apps/design_skills_engine/agents/workflow_executor_agent.py`
- Test: `tests/unit/test_workflow_executor_agent.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_workflow_executor_agent.py
"""Tests for WorkflowExecutorAgent."""
from unittest.mock import AsyncMock, MagicMock, patch

import pytest


class TestWorkflowExecutorAgent:
    """WorkflowExecutorAgent unit tests (mocked ComfyUI)."""

    @pytest.fixture
    def mock_comfyui(self):
        client = MagicMock()
        client.health_check = AsyncMock(return_value=True)
        client.build_workflow_payload = MagicMock(return_value={"prompt": {}})
        client.queue_prompt = AsyncMock(return_value="prompt-001")
        client.poll_until_complete = AsyncMock(return_value={
            "outputs": {
                "7": {
                    "images": [
                        {"filename": "img_001_00001_.png", "subfolder": "", "type": "output"}
                    ]
                }
            }
        })
        client.get_image = AsyncMock(return_value=b"\x89PNG fake")
        client.close = AsyncMock()
        return client

    @pytest.fixture
    def agent(self, mock_comfyui):
        from apps.design_skills_engine.agents.workflow_executor_agent import (
            WorkflowExecutorAgent,
        )
        agent = WorkflowExecutorAgent(llm_client=None)
        agent._comfyui = mock_comfyui
        return agent

    @pytest.fixture
    def sample_plan(self):
        from apps.design_skills_engine.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
            PromptPlanOutput,
        )
        return PromptPlanOutput(
            design_concept="Tech product showcase",
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

    def test_agent_name(self, agent) -> None:
        assert agent.name == "WorkflowExecutorAgent"

    @pytest.mark.asyncio
    async def test_execute_workflow(self, agent, sample_plan) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            WorkflowExecutorInput,
        )

        input_data = WorkflowExecutorInput(
            prompt_plan=sample_plan,
            output_directory="/tmp/output",
        )
        result = await agent.process(input_data)

        assert len(result.images) == 1
        assert result.images[0].image_id == "img_001"
        assert result.images[0].seed_used == 42
        assert result.output_directory == "/tmp/output"

    @pytest.mark.asyncio
    async def test_handles_comfyui_error_gracefully(self, agent, sample_plan) -> None:
        from apps.design_skills_engine.schemas.design_schemas import (
            WorkflowExecutorInput,
        )

        agent._comfyui.queue_prompt = AsyncMock(
            side_effect=Exception("Connection refused")
        )

        input_data = WorkflowExecutorInput(
            prompt_plan=sample_plan,
            output_directory="/tmp/output",
        )
        result = await agent.process(input_data)

        # Should return empty result with error noted
        assert len(result.images) == 0
        assert len(result.errors) > 0
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_workflow_executor_agent.py -v`
Expected: FAIL

**Step 3: Write minimal implementation**

First add `WorkflowExecutorInput` to schemas:

```python
# Append to apps/design_skills_engine/schemas/design_schemas.py

class WorkflowExecutorInput(BaseModel):
    """Input for WorkflowExecutorAgent."""

    prompt_plan: PromptPlanOutput = Field(..., description="Complete prompt plan")
    output_directory: str = Field(
        default="/tmp/design_output", description="Output directory for images",
    )
    save_locally: bool = Field(
        default=True, description="Whether to save images to local disk",
    )
```

Then create the agent:

```python
# apps/design_skills_engine/agents/workflow_executor_agent.py
"""WorkflowExecutorAgent - ComfyUI workflow execution.

Takes a PromptPlanOutput and executes each image spec through ComfyUI:
1. Build workflow JSON from global style + image spec
2. Queue the workflow
3. Poll for completion
4. Collect generated images
"""

import logging
import os
import time
from pathlib import Path
from typing import Any

from apps.design_skills_engine.schemas.design_schemas import (
    GeneratedImage,
    WorkflowExecutorInput,
    WorkflowResult,
)
from apps.design_skills_engine.tools.comfyui_client import ComfyUIClient

from agentflow import ResilientAgent


class WorkflowExecutorAgent(ResilientAgent[WorkflowExecutorInput, WorkflowResult]):
    """Execute prompt plan through ComfyUI and collect results."""

    name = "WorkflowExecutorAgent"
    timeout_seconds = 600  # 10 minutes for batch generation

    def __init__(self, llm_client: Any = None) -> None:
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)
        self._comfyui = ComfyUIClient()

    def _parse_input(self, input_data: dict[str, Any]) -> WorkflowExecutorInput:
        return WorkflowExecutorInput(**input_data)

    async def process(self, input_data: WorkflowExecutorInput) -> WorkflowResult:
        """Execute all image specs through ComfyUI."""
        plan = input_data.prompt_plan
        output_dir = input_data.output_directory

        # Ensure output directory exists
        Path(output_dir).mkdir(parents=True, exist_ok=True)

        generated: list[GeneratedImage] = []
        errors: list[str] = []
        start_time = time.monotonic()

        for spec in plan.images:
            try:
                img_start = time.monotonic()

                # Build workflow
                workflow = self._comfyui.build_workflow_payload(
                    plan.global_style, spec,
                )

                # Queue and wait
                prompt_id = await self._comfyui.queue_prompt(workflow)
                history = await self._comfyui.poll_until_complete(prompt_id)

                # Extract output image info
                file_path = self._extract_output_path(
                    history, spec.image_id, output_dir,
                )

                # Download and save if needed
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

                generated.append(GeneratedImage(
                    image_id=spec.image_id,
                    role=spec.role,
                    file_path=file_path or f"{output_dir}/{spec.image_id}.png",
                    prompt_used=spec.prompt,
                    seed_used=spec.seed,
                    generation_time_seconds=round(img_time, 2),
                ))
                self._logger.info(
                    f"Generated {spec.image_id} ({spec.role.value}) in {img_time:.1f}s"
                )

            except Exception as e:
                self._logger.error(f"Failed to generate {spec.image_id}: {e}")
                errors.append(f"{spec.image_id}: {e}")

        total_time = time.monotonic() - start_time

        return WorkflowResult(
            images=generated,
            output_directory=output_dir,
            total_generation_time_seconds=round(total_time, 2),
            errors=errors,
        )

    def _find_output_image(self, history: dict[str, Any]) -> dict[str, str] | None:
        """Find the output image info from history entry."""
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
        """Extract the output file path from history."""
        info = self._find_output_image(history)
        if info:
            return f"{output_dir}/{info['filename']}"
        return f"{output_dir}/{image_id}.png"
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/test_workflow_executor_agent.py -v`
Expected: All 3 tests PASS

**Step 5: Commit**

```bash
git add apps/design_skills_engine/agents/workflow_executor_agent.py \
       apps/design_skills_engine/schemas/design_schemas.py \
       apps/design_skills_engine/schemas/__init__.py \
       tests/unit/test_workflow_executor_agent.py
git commit -m "feat(dse): add WorkflowExecutorAgent with ComfyUI execution"
```

---

## Phase 4: Engine & Registry

### Task 7: Create AgentRegistry and YAML definitions

**Files:**
- Create: `apps/design_skills_engine/agents/agent_definitions.yaml`
- Create: `apps/design_skills_engine/services/agent_registry.py`
- Test: `tests/unit/test_dse_agent_registry.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_dse_agent_registry.py
"""Tests for DesignSkillsEngine AgentRegistry."""
import pytest


class TestDesignAgentRegistry:
    """AgentRegistry tests."""

    @pytest.fixture
    def registry(self):
        from apps.design_skills_engine.services.agent_registry import (
            DesignAgentRegistry,
        )
        return DesignAgentRegistry(llm_client=None)

    @pytest.mark.asyncio
    async def test_initialize_loads_agents(self, registry) -> None:
        await registry.initialize()
        assert registry.total_agents == 3

    @pytest.mark.asyncio
    async def test_get_agent_by_id(self, registry) -> None:
        await registry.initialize()

        intent = registry.get_agent("intent_analyzer")
        assert intent is not None
        assert intent.name == "IntentAnalyzerAgent"

        planner = registry.get_agent("prompt_planner")
        assert planner is not None

        executor = registry.get_agent("workflow_executor")
        assert executor is not None

    @pytest.mark.asyncio
    async def test_get_ordered_agents(self, registry) -> None:
        await registry.initialize()
        agents = registry.get_ordered_agents()
        assert len(agents) == 3
        assert agents[0].name == "IntentAnalyzerAgent"
        assert agents[1].name == "PromptPlannerAgent"
        assert agents[2].name == "WorkflowExecutorAgent"
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_dse_agent_registry.py -v`
Expected: FAIL

**Step 3: Write the YAML definitions**

```yaml
# apps/design_skills_engine/agents/agent_definitions.yaml
flow_id: design-skills-engine
name: Design Skills Engine
version: "1.0.0"
description: "Lovart-style design skills - natural language to multi-image generation via ComfyUI"

agents:
  - id: intent_analyzer
    name: 意図分析
    label: Design Intent Analysis
    icon: 🎯
    description: "Analyze design brief and extract structured intent"
    class_name: IntentAnalyzerAgent
    is_gate: false
    uses_rag: false
    progress_messages:
      - [30, "Analyzing design brief..."]
      - [60, "Extracting design intent..."]
      - [90, "Determining image roles..."]

  - id: prompt_planner
    name: プロンプト計画
    label: Prompt Planning
    icon: 🎨
    description: "Generate structured prompt plan with global style and per-image specs"
    class_name: PromptPlannerAgent
    is_gate: false
    uses_rag: false
    progress_messages:
      - [20, "Building global style..."]
      - [50, "Generating per-image prompts..."]
      - [80, "Optimizing consistency..."]

  - id: workflow_executor
    name: 生成実行
    label: Workflow Execution
    icon: 🖼️
    description: "Execute prompt plan through ComfyUI and collect generated images"
    class_name: WorkflowExecutorAgent
    is_gate: false
    uses_rag: false
    progress_messages:
      - [10, "Connecting to ComfyUI..."]
      - [30, "Generating hero image..."]
      - [60, "Generating feature images..."]
      - [90, "Collecting results..."]

config:
  max_revisions: 0
  enable_rag: false
```

**Step 4: Write the DesignAgentRegistry**

```python
# apps/design_skills_engine/services/agent_registry.py
"""DesignAgentRegistry - Agent management for Design Skills Engine.

Follows the same pattern as DecisionGovernanceEngine's AgentRegistry.
"""

import importlib
import logging
from pathlib import Path
from typing import Any

from agentflow.core.flow_definition import (
    AgentDefinition,
    FlowDefinition,
    FlowDefinitionRegistry,
)


class DesignAgentRegistry:
    """Agent registry for Design Skills Engine."""

    DEFAULT_YAML_PATH = Path(__file__).parent.parent / "agents" / "agent_definitions.yaml"

    AGENT_CLASS_MAP: dict[str, str] = {
        "IntentAnalyzerAgent": "apps.design_skills_engine.agents.intent_analyzer_agent",
        "PromptPlannerAgent": "apps.design_skills_engine.agents.prompt_planner_agent",
        "WorkflowExecutorAgent": "apps.design_skills_engine.agents.workflow_executor_agent",
    }

    def __init__(
        self,
        yaml_path: str | Path | None = None,
        llm_client: Any = None,
    ) -> None:
        self._logger = logging.getLogger("design_skills.agent_registry")
        self._yaml_path = Path(yaml_path) if yaml_path else self.DEFAULT_YAML_PATH
        self._llm_client = llm_client
        self._agents: dict[str, Any] = {}
        self._flow_definition: FlowDefinition | None = None
        self._initialized = False

    async def initialize(self) -> None:
        """Load YAML definitions and register flow."""
        if self._initialized:
            return

        self._flow_definition = FlowDefinition.from_yaml(self._yaml_path)
        registry = FlowDefinitionRegistry.get_instance()
        registry.register(self._flow_definition)
        self._initialized = True
        self._logger.info(
            f"Registered flow '{self._flow_definition.flow_id}' "
            f"with {len(self._flow_definition.agents)} agents"
        )

    def _ensure_initialized(self) -> None:
        if not self._initialized or not self._flow_definition:
            msg = "DesignAgentRegistry not initialized. Call initialize() first."
            raise RuntimeError(msg)

    def _create_agent(self, agent_def: AgentDefinition) -> Any:
        class_name = agent_def.class_name
        if not class_name:
            msg = f"Agent {agent_def.id} has no class_name"
            raise ValueError(msg)

        module_path = self.AGENT_CLASS_MAP.get(class_name)
        if not module_path:
            msg = f"Unknown agent class: {class_name}"
            raise ValueError(msg)

        module = importlib.import_module(module_path)
        agent_class = getattr(module, class_name)
        return agent_class(llm_client=self._llm_client)

    def get_agent(self, agent_id: str) -> Any:
        self._ensure_initialized()
        if agent_id in self._agents:
            return self._agents[agent_id]

        agent_def = self._flow_definition.get_agent(agent_id)
        if not agent_def:
            msg = f"Agent not found: {agent_id}"
            raise ValueError(msg)

        agent = self._create_agent(agent_def)
        self._agents[agent_id] = agent
        return agent

    def get_ordered_agents(self) -> list[Any]:
        self._ensure_initialized()
        return [self.get_agent(a.id) for a in self._flow_definition.agents]

    @property
    def total_agents(self) -> int:
        self._ensure_initialized()
        return len(self._flow_definition.agents)

    @property
    def flow_id(self) -> str:
        self._ensure_initialized()
        return self._flow_definition.flow_id


__all__ = ["DesignAgentRegistry"]
```

**Step 5: Run test to verify it passes**

Run: `pytest tests/unit/test_dse_agent_registry.py -v`
Expected: All 3 tests PASS

**Step 6: Commit**

```bash
git add apps/design_skills_engine/agents/agent_definitions.yaml \
       apps/design_skills_engine/services/agent_registry.py \
       tests/unit/test_dse_agent_registry.py
git commit -m "feat(dse): add AgentRegistry with YAML definitions"
```

---

### Task 8: Create DesignSkillsEngine (PipelineEngine)

**Files:**
- Create: `apps/design_skills_engine/engine.py`
- Test: `tests/unit/test_design_skills_engine.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_design_skills_engine.py
"""Tests for DesignSkillsEngine."""
from unittest.mock import AsyncMock, MagicMock, patch

import pytest


class TestDesignSkillsEngine:
    """DesignSkillsEngine integration tests (mocked LLM + ComfyUI)."""

    @pytest.fixture
    def mock_comfyui(self):
        client = MagicMock()
        client.health_check = AsyncMock(return_value=True)
        client.build_workflow_payload = MagicMock(return_value={"prompt": {}})
        client.queue_prompt = AsyncMock(return_value="prompt-001")
        client.poll_until_complete = AsyncMock(return_value={
            "outputs": {"7": {"images": [{"filename": "out.png", "subfolder": "", "type": "output"}]}}
        })
        client.get_image = AsyncMock(return_value=b"\x89PNG")
        client.close = AsyncMock()
        return client

    @pytest.mark.asyncio
    async def test_engine_initializes(self) -> None:
        from apps.design_skills_engine.engine import DesignSkillsEngine

        engine = DesignSkillsEngine(llm_client=None)
        assert engine is not None

    @pytest.mark.asyncio
    async def test_engine_run_produces_result(self, mock_comfyui, tmp_path) -> None:
        from apps.design_skills_engine.engine import DesignSkillsEngine

        engine = DesignSkillsEngine(llm_client=None)

        # Inject mock ComfyUI client into executor agent
        with patch(
            "apps.design_skills_engine.agents.workflow_executor_agent.ComfyUIClient",
            return_value=mock_comfyui,
        ):
            result = await engine.run({
                "brief": "Product images for a bluetooth speaker, tech style",
                "num_images": 2,
                "output_directory": str(tmp_path),
            })

        assert result is not None
        # Result should contain generated images or a report
        assert isinstance(result, dict)
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_design_skills_engine.py -v`
Expected: FAIL

**Step 3: Write minimal implementation**

```python
# apps/design_skills_engine/engine.py
"""DesignSkillsEngine - Main engine for design generation pipeline.

PipelineEngine pattern:
    IntentAnalyzer → PromptPlanner → WorkflowExecutor

Usage:
    >>> engine = DesignSkillsEngine()
    >>> result = await engine.run({
    ...     "brief": "Product images for a bluetooth speaker",
    ...     "num_images": 8,
    ... })
"""

import logging
from typing import Any

from apps.design_skills_engine.services.agent_registry import DesignAgentRegistry

from agentflow.engines import EngineConfig, PipelineEngine


class DesignSkillsEngine(PipelineEngine):
    """Design Skills Engine - Lovart-style multi-image generation.

    Pipeline:
        IntentAnalyzer → PromptPlanner → WorkflowExecutor
    """

    def __init__(
        self,
        llm_client: Any = None,
        output_directory: str = "/tmp/design_output",
    ) -> None:
        self._registry = DesignAgentRegistry(llm_client=llm_client)
        self._output_directory = output_directory

        super().__init__(
            stages=[],
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
        """Configure pipeline stages dynamically."""
        await self._registry.initialize()

        self._stage_configs = self._parse_stages([
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
        ])

        for stage in self._stage_configs:
            instances = []
            if stage.agent:
                instances.append(stage.agent)
            if stage.agents:
                instances.extend(stage.agents)
            self._stage_instances[stage.name] = instances

        self._logger.info("DesignSkillsEngine stages configured")

    async def run(self, input_data: dict[str, Any]) -> Any:
        """Run the design pipeline.

        Args:
            input_data: Must contain 'brief'. Optional: 'num_images',
                       'style_preferences', 'target_platform', etc.

        Returns:
            Pipeline result dict with generated images.
        """
        # Normalize input for DesignBriefInput compatibility
        if "output_directory" in input_data:
            self._output_directory = input_data.pop("output_directory")

        return await super().run(input_data)


__all__ = ["DesignSkillsEngine"]
```

**Step 4: Run test to verify it passes**

Run: `pytest tests/unit/test_design_skills_engine.py -v`
Expected: All 2 tests PASS

**Step 5: Commit**

```bash
git add apps/design_skills_engine/engine.py tests/unit/test_design_skills_engine.py
git commit -m "feat(dse): add DesignSkillsEngine PipelineEngine with 3-stage pipeline"
```

---

## Phase 5: AgentFlow Framework Integration

### Task 9: Register DesignSkillsEngine as a builtin skill

**Files:**
- Create: `agentflow/skills/builtin/design-skills/SKILL.md`
- Create: `agentflow/skills/builtin/design-skills/__init__.py`
- Create: `agentflow/skills/builtin/design-skills/design_skills.py`
- Test: `tests/unit/test_design_skills_skill.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_design_skills_skill.py
"""Tests for design-skills skill registration."""
from pathlib import Path

import pytest


class TestDesignSkillsSkill:
    """Verify skill metadata and loading."""

    def test_skill_md_exists(self) -> None:
        skill_path = Path("agentflow/skills/builtin/design-skills/SKILL.md")
        assert skill_path.exists()

    def test_skill_md_has_required_fields(self) -> None:
        skill_path = Path("agentflow/skills/builtin/design-skills/SKILL.md")
        content = skill_path.read_text()
        assert "name:" in content
        assert "description:" in content
        assert "triggers:" in content

    def test_skill_module_importable(self) -> None:
        from agentflow.skills.builtin import __path__ as skills_path

        design_skill_dir = Path(skills_path[0]) / "design-skills"
        assert design_skill_dir.exists()
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_design_skills_skill.py -v`
Expected: FAIL

**Step 3: Write the SKILL.md**

```markdown
---
name: design-skills
description: Lovart-style multi-image generation from natural language design briefs
version: 1.0.0
triggers: [design, image generation, product images, brand design, ComfyUI]
requirements: [httpx>=0.24.0]
tags: [design, image-generation, comfyui, creative]
examples:
  - "Generate product images for a bluetooth speaker"
  - "Create a brand identity kit with logo and color palette"
  - "Design Instagram posts for a coffee brand"
---

# Design Skills

Generate professional multi-image design sets from natural language briefs.

## Overview

This skill implements Lovart's "Design Skills" concept:
1. **Intent Analysis** - Understand the design brief
2. **Prompt Planning** - Generate structured prompts with unified style
3. **Workflow Execution** - Execute through ComfyUI for image generation

## Requirements

- ComfyUI server running (default: http://localhost:8188)
- Set `COMFYUI_URL` environment variable if using a different address

## Usage

```python
from apps.design_skills_engine.engine import DesignSkillsEngine

engine = DesignSkillsEngine()
result = await engine.run({
    "brief": "Product images for a bluetooth speaker, tech style, dark",
    "num_images": 8,
    "target_platform": "amazon",
})
```
```

**Step 4: Write __init__.py and skill module**

```python
# agentflow/skills/builtin/design-skills/__init__.py
"""Design Skills - Lovart-style multi-image generation."""
```

```python
# agentflow/skills/builtin/design-skills/design_skills.py
"""Design Skills skill implementation.

Provides the DesignSkillsEngine as an AgentFlow skill.
"""

from typing import Any


async def run(input_data: dict[str, Any]) -> dict[str, Any]:
    """Run design skills pipeline.

    Args:
        input_data: Design brief parameters

    Returns:
        Generated images and metadata
    """
    from apps.design_skills_engine.engine import DesignSkillsEngine

    engine = DesignSkillsEngine()
    result = await engine.run(input_data)
    return result
```

**Step 5: Run test to verify it passes**

Run: `pytest tests/unit/test_design_skills_skill.py -v`
Expected: All 3 tests PASS

**Step 6: Commit**

```bash
git add agentflow/skills/builtin/design-skills/ tests/unit/test_design_skills_skill.py
git commit -m "feat(dse): register as AgentFlow builtin skill"
```

---

### Task 10: Add stage input mappers for data flow between agents

**Files:**
- Modify: `apps/design_skills_engine/engine.py`
- Test: `tests/unit/test_dse_data_flow.py`

**Step 1: Write the failing test**

```python
# tests/unit/test_dse_data_flow.py
"""Tests for data flow between pipeline stages."""
import pytest


class TestStageDataFlow:
    """Verify data mapping between pipeline stages."""

    def test_intent_to_planner_mapping(self) -> None:
        """IntentAnalyzer output should map to PromptPlannerAgent input."""
        from apps.design_skills_engine.schemas.design_schemas import (
            DesignCategory,
            ImageRole,
            IntentAnalysis,
            PromptPlanInput,
        )

        intent = IntentAnalysis(
            category=DesignCategory.PRODUCT_PHOTOGRAPHY,
            subject="bluetooth speaker",
            key_features=["waterproof"],
            target_audience="",
            style_direction="tech, dark",
            image_roles=[ImageRole.HERO, ImageRole.FEATURE],
        )
        # The mapping function should create PromptPlanInput
        planner_input = PromptPlanInput(intent=intent)
        assert planner_input.intent.subject == "bluetooth speaker"

    def test_planner_to_executor_mapping(self) -> None:
        """PromptPlanner output should map to WorkflowExecutorAgent input."""
        from apps.design_skills_engine.schemas.design_schemas import (
            GlobalStyle,
            ImageRole,
            ImageSpec,
            PromptPlanOutput,
            WorkflowExecutorInput,
        )

        plan = PromptPlanOutput(
            design_concept="Test",
            global_style=GlobalStyle(
                color_palette=["black"],
                lighting="studio",
                camera_angle="front",
                mood="pro",
                negative_prompt="blurry",
            ),
            images=[ImageSpec(
                image_id="img_001",
                role=ImageRole.HERO,
                prompt="test",
                seed=42,
            )],
            consistency_seed=42,
        )
        executor_input = WorkflowExecutorInput(prompt_plan=plan)
        assert len(executor_input.prompt_plan.images) == 1
```

**Step 2: Run test to verify it fails**

Run: `pytest tests/unit/test_dse_data_flow.py -v`
Expected: FAIL (if schemas not imported) or PASS (if schemas correct)

**Step 3: Verify tests pass, then commit**

Run: `pytest tests/unit/test_dse_data_flow.py -v`
Expected: PASS

**Step 4: Commit**

```bash
git add tests/unit/test_dse_data_flow.py
git commit -m "test(dse): add data flow validation tests"
```

---

## Phase 6: Quality & Documentation

### Task 11: Run full test suite, lint, and type-check

**Step 1: Run all DSE tests**

```bash
pytest tests/unit/test_design_skills_*.py tests/unit/test_comfyui_client.py \
       tests/unit/test_intent_analyzer_agent.py tests/unit/test_prompt_planner_agent.py \
       tests/unit/test_workflow_executor_agent.py tests/unit/test_dse_*.py -v
```

Expected: All tests PASS

**Step 2: Run linter**

```bash
make lint
```

Fix any issues found.

**Step 3: Run type-check**

```bash
make type-check
```

Fix any type annotation issues.

**Step 4: Run full project test suite**

```bash
make test
```

Expected: No regressions, all 434+ tests pass.

**Step 5: Commit any fixes**

```bash
git add -A
git commit -m "fix(dse): resolve lint and type-check issues"
```

---

### Task 12: Create the app's __init__.py with public API

**Files:**
- Modify: `apps/design_skills_engine/__init__.py`

**Step 1: Write the public API**

```python
# apps/design_skills_engine/__init__.py
"""Design Skills Engine - Lovart-style multi-image generation.

A PipelineEngine app that converts natural language design briefs
into structured, multi-image generation plans executed via ComfyUI.

Quick Start:
    >>> from apps.design_skills_engine.engine import DesignSkillsEngine
    >>>
    >>> engine = DesignSkillsEngine()
    >>> result = await engine.run({
    ...     "brief": "Product images for a bluetooth speaker",
    ...     "num_images": 8,
    ... })

Architecture:
    IntentAnalyzerAgent → PromptPlannerAgent → WorkflowExecutorAgent
"""

from apps.design_skills_engine.engine import DesignSkillsEngine

__all__ = ["DesignSkillsEngine"]
```

**Step 2: Commit**

```bash
git add apps/design_skills_engine/__init__.py
git commit -m "feat(dse): add public API exports"
```

---

## Summary

| Phase | Tasks | Description |
|-------|-------|-------------|
| 1 | 1-2 | Directory structure + Pydantic schemas |
| 2 | 3 | ComfyUI async HTTP client |
| 3 | 4-6 | IntentAnalyzer, PromptPlanner, WorkflowExecutor agents |
| 4 | 7-8 | AgentRegistry + DesignSkillsEngine |
| 5 | 9-10 | AgentFlow skill registration + data flow |
| 6 | 11-12 | Quality gates + public API |

**Total: 12 tasks, ~35 TDD steps**

**Key design decisions:**
1. **Follows DGE pattern exactly** - Same AgentRegistry, YAML definitions, PipelineEngine inheritance
2. **ComfyUI as external tool** - Not embedded, communicates via HTTP API
3. **Rule-based fallback** - All agents work without LLM (for testing), enhanced with LLM
4. **Consistency via shared seed** - All images in a set use the same seed for visual coherence
5. **Extensible** - New roles (COMPARISON, INFOGRAPHIC), new workflows (inpainting, ControlNet) can be added
