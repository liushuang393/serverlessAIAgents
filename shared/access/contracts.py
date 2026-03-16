"""共有 access 契約。"""

from __future__ import annotations

from typing import Any

from pydantic import Field

from contracts.base import ContractModel


class AccessContext(ContractModel):
    """共有 access 解決結果。"""

    subject: dict[str, Any] = Field(default_factory=dict, description="主体属性")
    resource: dict[str, Any] = Field(default_factory=dict, description="対象属性")
    action: str = Field(default="", description="実行アクション")
    environment: dict[str, Any] = Field(default_factory=dict, description="環境属性")
    tenant_id: str | None = Field(default=None, description="テナントID")
