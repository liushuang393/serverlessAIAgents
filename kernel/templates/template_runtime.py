"""Stable execution entrypoints for executable domain templates.

Executable templates should import runtime classes from here instead of
depending on broader ``kernel`` exports. That makes the supported extension
surface obvious to developers and AI agents.
"""

from __future__ import annotations

from kernel.agents.resilient_agent import ResilientAgent


__all__ = ["ResilientAgent"]
