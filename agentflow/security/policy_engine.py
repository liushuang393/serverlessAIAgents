"""統一ポリシーエンジン.

RBAC/ABAC/PBAC を統合した柔軟な認可システム。

Supported Modes:
- RBAC: ロールベースアクセス制御（既存）
- ABAC: 属性ベースアクセス制御（主体・リソース・環境の属性で判定）
- PBAC: ポリシーベースアクセス制御（明示的なポリシールールで判定）

Example:
    >>> from agentflow.security.policy_engine import PolicyEngine, AuthContext
    >>>
    >>> engine = PolicyEngine()
    >>>
    >>> # ABAC モードで認可
    >>> result = await engine.authorize(
    ...     context=AuthContext(
    ...         subject={"user_id": "123", "role": "analyst", "department": "finance"},
    ...         resource={"type": "report", "sensitivity": "high"},
    ...         action="read",
    ...     ),
    ...     mode="abac",
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from collections.abc import Callable


logger = logging.getLogger(__name__)


class AuthMode(str, Enum):
    """認可モード."""

    RBAC = "rbac"  # Role-Based Access Control
    ABAC = "abac"  # Attribute-Based Access Control
    PBAC = "pbac"  # Policy-Based Access Control
    HYBRID = "hybrid"  # 複合モード


class AuthDecision(str, Enum):
    """認可決定."""

    ALLOW = "allow"
    DENY = "deny"
    NOT_APPLICABLE = "not_applicable"  # 該当ポリシーなし


class AuthContext(BaseModel):
    """認可コンテキスト.

    認可判定に必要な情報を保持。
    """

    # 主体（Subject）属性
    subject: dict[str, Any] = Field(
        default_factory=dict,
        description="主体属性（user_id, role, department, etc.）",
    )

    # リソース（Resource）属性
    resource: dict[str, Any] = Field(
        default_factory=dict,
        description="リソース属性（type, owner, sensitivity, etc.）",
    )

    # アクション
    action: str = Field(default="", description="実行するアクション（read, write, execute, etc.）")

    # 環境（Environment）属性
    environment: dict[str, Any] = Field(
        default_factory=dict,
        description="環境属性（time, ip_address, device, etc.）",
    )

    # テナント情報
    tenant_id: str | None = Field(default=None, description="テナントID")


class AuthResult(BaseModel):
    """認可結果."""

    decision: AuthDecision = Field(default=AuthDecision.DENY)
    reason: str = Field(default="", description="判定理由")
    matched_policies: list[str] = Field(default_factory=list, description="マッチしたポリシー")
    evaluated_at: datetime = Field(default_factory=datetime.now)
    mode: AuthMode = Field(default=AuthMode.RBAC)

    @property
    def allowed(self) -> bool:
        """許可されたか."""
        return self.decision == AuthDecision.ALLOW


@dataclass
class Policy:
    """ポリシー定義.

    PBAC モードで使用される明示的なルール定義。
    """

    policy_id: str
    name: str
    description: str = ""
    priority: int = 0  # 高いほど優先

    # 条件
    subject_conditions: dict[str, Any] = field(default_factory=dict)
    resource_conditions: dict[str, Any] = field(default_factory=dict)
    action_conditions: list[str] = field(default_factory=list)
    environment_conditions: dict[str, Any] = field(default_factory=dict)

    # 効果
    effect: AuthDecision = AuthDecision.ALLOW

    # カスタム条件関数
    custom_condition: Callable[[AuthContext], bool] | None = None

    def matches(self, context: AuthContext) -> bool:
        """コンテキストがポリシーにマッチするか判定."""
        # 主体条件
        if not self._match_attributes(context.subject, self.subject_conditions):
            return False

        # リソース条件
        if not self._match_attributes(context.resource, self.resource_conditions):
            return False

        # アクション条件
        if self.action_conditions and context.action not in self.action_conditions:
            return False

        # 環境条件
        if not self._match_attributes(context.environment, self.environment_conditions):
            return False

        # カスタム条件
        return not (self.custom_condition and not self.custom_condition(context))

    def _match_attributes(
        self,
        actual: dict[str, Any],
        conditions: dict[str, Any],
    ) -> bool:
        """属性がの条件にマッチするか."""
        for key, expected in conditions.items():
            actual_value = actual.get(key)

            # リスト条件（いずれかにマッチ）
            if isinstance(expected, list):
                if actual_value not in expected:
                    return False
            # 辞書条件（ネスト）
            elif isinstance(expected, dict):
                if not isinstance(actual_value, dict):
                    return False
                if not self._match_attributes(actual_value, expected):
                    return False
            # 単純比較
            elif actual_value != expected:
                return False

        return True


class PolicyEngine:
    """統一ポリシーエンジン.

    RBAC、ABAC、PBAC を統合した認可エンジン。
    """

    def __init__(self) -> None:
        """初期化."""
        self._policies: dict[str, Policy] = {}
        self._role_permissions: dict[str, set[str]] = {}  # RBAC用
        self._logger = logging.getLogger(__name__)

        # デフォルトロール権限を設定
        self._setup_default_roles()

    def _setup_default_roles(self) -> None:
        """デフォルトロール権限を設定."""
        self._role_permissions = {
            "admin": {"*"},  # 全権限
            "manager": {"read", "write", "execute", "manage"},
            "analyst": {"read", "execute"},
            "viewer": {"read"},
            "guest": set(),
        }

    # =========================================================================
    # ポリシー管理
    # =========================================================================

    def add_policy(self, policy: Policy) -> None:
        """ポリシーを追加.

        Args:
            policy: ポリシー定義
        """
        self._policies[policy.policy_id] = policy
        self._logger.info(f"ポリシー追加: {policy.policy_id}")

    def remove_policy(self, policy_id: str) -> bool:
        """ポリシーを削除.

        Args:
            policy_id: ポリシーID

        Returns:
            削除成功の場合True
        """
        if policy_id in self._policies:
            del self._policies[policy_id]
            return True
        return False

    def list_policies(self) -> list[str]:
        """ポリシー一覧."""
        return list(self._policies.keys())

    def add_role(self, role: str, permissions: set[str]) -> None:
        """ロールを追加（RBAC用）.

        Args:
            role: ロール名
            permissions: 権限セット
        """
        self._role_permissions[role] = permissions

    # =========================================================================
    # 認可
    # =========================================================================

    async def authorize(
        self,
        context: AuthContext,
        mode: AuthMode | str = AuthMode.RBAC,
    ) -> AuthResult:
        """認可を実行.

        Args:
            context: 認可コンテキスト
            mode: 認可モード（rbac/abac/pbac/hybrid）

        Returns:
            認可結果
        """
        if isinstance(mode, str):
            mode = AuthMode(mode)

        self._logger.debug(f"認可実行: mode={mode}, action={context.action}")
        handlers = {
            AuthMode.RBAC: self._authorize_rbac,
            AuthMode.ABAC: self._authorize_abac,
            AuthMode.PBAC: self._authorize_pbac,
            AuthMode.HYBRID: self._authorize_hybrid,
        }
        return await handlers[mode](context)

    async def _authorize_rbac(self, context: AuthContext) -> AuthResult:
        """RBAC による認可."""
        role = context.subject.get("role", "guest")
        permissions = self._role_permissions.get(role, set())

        # 全権限チェック
        if "*" in permissions:
            return AuthResult(
                decision=AuthDecision.ALLOW,
                reason=f"ロール '{role}' は全権限を持っています",
                mode=AuthMode.RBAC,
            )

        # アクション権限チェック
        if context.action in permissions:
            return AuthResult(
                decision=AuthDecision.ALLOW,
                reason=f"ロール '{role}' はアクション '{context.action}' の権限を持っています",
                mode=AuthMode.RBAC,
            )

        return AuthResult(
            decision=AuthDecision.DENY,
            reason=f"ロール '{role}' はアクション '{context.action}' の権限がありません",
            mode=AuthMode.RBAC,
        )

    async def _authorize_abac(self, context: AuthContext) -> AuthResult:
        """ABAC による認可."""
        # 基本的な属性チェック
        checks: list[tuple[bool, str]] = []

        # 例: 機密リソースは管理者のみ
        sensitivity = context.resource.get("sensitivity", "low")
        role = context.subject.get("role", "guest")

        if sensitivity == "high" and role not in ["admin", "manager"]:
            checks.append((False, "高機密リソースへのアクセスには管理者権限が必要"))

        # 例: 部門チェック（同じ部門のリソースのみアクセス可能）
        resource_dept = context.resource.get("department")
        user_dept = context.subject.get("department")

        if resource_dept and user_dept and resource_dept != user_dept:
            # 管理者は部門を超えてアクセス可能
            if role not in ["admin"]:
                checks.append(
                    (
                        False,
                        f"部門 '{user_dept}' のユーザーは部門 '{resource_dept}' のリソースにアクセスできません",
                    )
                )

        # 環境チェック
        allowed_ips = context.environment.get("allowed_ips", [])
        client_ip = context.environment.get("client_ip")

        if allowed_ips and client_ip and client_ip not in allowed_ips:
            checks.append((False, f"IPアドレス '{client_ip}' からのアクセスは許可されていません"))

        # 全チェック通過
        failed = [reason for passed, reason in checks if not passed]
        if failed:
            return AuthResult(
                decision=AuthDecision.DENY,
                reason="; ".join(failed),
                mode=AuthMode.ABAC,
            )

        # RBAC チェックも組み合わせ
        rbac_result = await self._authorize_rbac(context)
        if not rbac_result.allowed:
            return AuthResult(
                decision=AuthDecision.DENY,
                reason=f"ABAC条件はパスしましたが、{rbac_result.reason}",
                mode=AuthMode.ABAC,
            )

        return AuthResult(
            decision=AuthDecision.ALLOW,
            reason="ABAC条件をすべてパスしました",
            mode=AuthMode.ABAC,
        )

    async def _authorize_pbac(self, context: AuthContext) -> AuthResult:
        """PBAC による認可."""
        matched: list[Policy] = []

        # 優先度順にソート
        sorted_policies = sorted(
            self._policies.values(),
            key=lambda p: -p.priority,
        )

        for policy in sorted_policies:
            if policy.matches(context):
                matched.append(policy)

        if not matched:
            return AuthResult(
                decision=AuthDecision.NOT_APPLICABLE,
                reason="マッチするポリシーがありません",
                matched_policies=[],
                mode=AuthMode.PBAC,
            )

        # 最も優先度の高いポリシーを適用
        top_policy = matched[0]

        return AuthResult(
            decision=top_policy.effect,
            reason=f"ポリシー '{top_policy.name}' が適用されました",
            matched_policies=[p.policy_id for p in matched],
            mode=AuthMode.PBAC,
        )

    async def _authorize_hybrid(self, context: AuthContext) -> AuthResult:
        """HYBRID による認可（PBAC -> ABAC -> RBAC の順で評価）."""
        # 1. PBAC チェック
        pbac_result = await self._authorize_pbac(context)
        if pbac_result.decision == AuthDecision.ALLOW:
            return pbac_result
        if pbac_result.decision == AuthDecision.DENY and pbac_result.matched_policies:
            return pbac_result

        # 2. ABAC チェック
        abac_result = await self._authorize_abac(context)
        return AuthResult(
            decision=abac_result.decision,
            reason=f"[HYBRID] {abac_result.reason}",
            matched_policies=pbac_result.matched_policies,
            mode=AuthMode.HYBRID,
        )

    # =========================================================================
    # 便利メソッド
    # =========================================================================

    async def can(
        self,
        user_id: str,
        action: str,
        resource_type: str,
        role: str = "guest",
        **kwargs: Any,
    ) -> bool:
        """シンプルな権限チェック.

        Args:
            user_id: ユーザーID
            action: アクション
            resource_type: リソースタイプ
            role: ロール
            **kwargs: 追加属性

        Returns:
            許可されている場合True
        """
        context = AuthContext(
            subject={"user_id": user_id, "role": role, **kwargs.get("subject", {})},
            resource={"type": resource_type, **kwargs.get("resource", {})},
            action=action,
            environment=kwargs.get("environment", {}),
        )

        result = await self.authorize(context, mode=AuthMode.RBAC)
        return result.allowed
