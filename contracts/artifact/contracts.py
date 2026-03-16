"""成果物契約."""

from __future__ import annotations

from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class ArtifactManifest(ContractModel):
    """成果物メタ契約."""

    artifact_id: str = Field(..., description="成果物識別子")
    artifact_type: str = Field(..., description="成果物種別")
    path: str | None = Field(default=None, description="保存先パス")
    trace_id: str | None = Field(default=None, description="トレース識別子")
    flow_id: str | None = Field(default=None, description="フロー識別子")
    metadata: dict[str, Any] = Field(default_factory=dict, description="拡張メタデータ")
