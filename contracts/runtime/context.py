"""RuntimeContext 契約 — 六層すべてから参照可能な実行時コンテキスト.

contracts 層は依存ゼロであるため、settings の型は Any で保持する。
kernel/runtime/context.py がヘルパー関数と共に本クラスを re-export する。
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any


@dataclass(frozen=True)
class RuntimeContext:
    """実行時コンテキスト（マルチテナント対応）.

    Attributes:
        tenant_id: テナント識別子（マルチテナント分離）。
        request_id: リクエスト識別子（トレーシング）。
        trace_id: トレース識別子（分散トレーシング）。
        settings: 設定オーバーライド（per-tenant config）。contracts 層では Any。
        env_overrides: 環境変数オーバーライド。
        metadata: 任意のメタデータ。
    """

    tenant_id: str | None = None
    request_id: str | None = None
    trace_id: str | None = None
    settings: Any = None
    env_overrides: dict[str, str] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)


__all__ = ["RuntimeContext"]
