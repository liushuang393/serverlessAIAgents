# -*- coding: utf-8 -*-
"""LLM モデル統計.

モデル使用統計の追跡と管理を提供。
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass
class ModelStats:
    """モデル使用統計."""

    total_requests: int = 0
    successful_requests: int = 0
    failed_requests: int = 0
    total_input_tokens: int = 0
    total_output_tokens: int = 0
    total_cost: float = 0.0
    total_latency_ms: int = 0
    avg_latency_ms: float = 0.0
    last_used: float = 0.0
    error_rate: float = 0.0

