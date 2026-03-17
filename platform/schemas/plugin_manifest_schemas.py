"""Plugin manifest schemas."""

from __future__ import annotations

from typing import Any, Literal

from pydantic import BaseModel, Field


class PluginSignature(BaseModel):
    """Plugin signature metadata."""

    algorithm: str = Field(..., min_length=1, max_length=50)
    issuer: str = Field(..., min_length=1, max_length=120)
    key_id: str = Field(..., min_length=1, max_length=120)


class PluginCompatibility(BaseModel):
    """Kernel/Product compatibility matrix."""

    kernel: str = Field(..., min_length=1, max_length=50)
    product_lines: list[str] = Field(default_factory=list)
    runtime: str | None = Field(default=None, max_length=120)
    notes: str | None = Field(default=None, max_length=500)


class PluginManifest(BaseModel):
    """`plugin_manifest.json` root schema."""

    id: str = Field(..., min_length=1, max_length=120)
    version: str = Field(..., min_length=1, max_length=50)
    type: Literal[
        "tool",
        "skillpack",
        "protocol_adapter",
        "channel",
        "deploy_codegen",
        "runner_sandbox",
    ]
    capabilities: list[str] = Field(default_factory=list)
    risk_tier: Literal["low", "medium", "high", "critical"]
    side_effects: list[str] = Field(default_factory=list)
    required_permissions: list[str] = Field(default_factory=list)
    signature: PluginSignature
    compatibility: PluginCompatibility
    tests_required: list[str] = Field(default_factory=list)
    metadata: dict[str, Any] = Field(default_factory=dict)
