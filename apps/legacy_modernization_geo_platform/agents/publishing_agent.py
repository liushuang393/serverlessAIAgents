"""PublishingAgent - static GEO publisher wrapper."""

from __future__ import annotations

from typing import Any

from apps.legacy_modernization_geo_platform.agents._models import (
    PublishingInput,
    PublishingOutput,
)
from apps.legacy_modernization_geo_platform.backend.publisher import GeoPublisher
from apps.legacy_modernization_geo_platform.backend.settings import APP_ROOT, GeoPlatformSettings
from kernel.agents.resilient_agent import ResilientAgent


class PublishingAgent(ResilientAgent[PublishingInput, PublishingOutput]):
    """GeoPublisher を executable agent として公開する."""

    name = "Publishing"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    def __init__(self, publisher: GeoPublisher | None = None, **kwargs: Any) -> None:
        super().__init__(**kwargs)
        resolved_publisher = publisher or GeoPublisher(GeoPlatformSettings.from_env(app_root=APP_ROOT))
        self._publisher = resolved_publisher

    async def process(self, input_data: PublishingInput) -> PublishingOutput:
        artifact = self._publisher.publish(input_data.task_id, input_data.draft)
        return PublishingOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> PublishingInput:
        return PublishingInput.model_validate(input_data)
