"""Harness Engine 拡張モジュール.

Kernel の BaseEngine に対する横断関心事（HITL 等）の mixin を提供する。
"""

from harness.engines.hitl_mixin import HITLEngineConfig, HITLEngineMixin


__all__ = ["HITLEngineConfig", "HITLEngineMixin"]
