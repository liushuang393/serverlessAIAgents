"""Capability ontology schemas for Platform."""

from __future__ import annotations

from pydantic import BaseModel, Field


CAPABILITY_DOMAINS: tuple[str, ...] = (
    "knowledge",
    "reasoning",
    "interaction",
    "integration",
    "operations",
    "governance",
    "media",
)
"""Capability ontology domains (v1)."""


class CanonicalCapability(BaseModel):
    """Canonical capability object."""

    id: str = Field(..., min_length=3, description="Canonical capability id")
    domain: str = Field(..., min_length=1, description="Capability domain")
    task: str = Field(..., min_length=1, description="Task segment")
    qualifier: str | None = Field(default=None, description="Optional qualifier segment")
    label: str = Field(..., min_length=1, description="Human readable label")
    aliases: list[str] = Field(default_factory=list, description="Observed legacy aliases")


class CapabilityAggregate(CanonicalCapability):
    """Aggregated capability stats across apps."""

    count: int = Field(default=0, ge=0, description="Observed count")
    apps: list[str] = Field(default_factory=list, description="Apps using this capability")
