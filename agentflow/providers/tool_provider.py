"""Tool Provider - 統一ツールアクセスインターフェース.

このモジュールは、MCP/Method/Skills の統一アクセスを提供します。
@tool デコレータで登録されたツールを自動発見し、統一インターフェースで呼び出し。

設計原則（L3セキュリティ層）:
- 全ツールにセキュリティ属性（operation_type, risk_level, 権限, 承認要件）を持たせる
- ツール呼び出し時にポリシーエンジンと連携して権限チェック
- 高リスク操作は監査ログを必須化

使用例:
    >>> tools = ToolProvider.discover()  # @tool を自動発見
    >>> result = await tools.call("search_database", query="test")
    >>> # ツール一覧
    >>> for tool in tools.list_tools():
    ...     print(tool.name, tool.description)
    >>>
    >>> # セキュリティ属性付きツール定義
    >>> @tool(
    ...     name="update_order",
    ...     operation_type="write",
    ...     risk_level="high",
    ...     requires_approval=True,
    ...     required_permissions=["order:write"],
    ...     audit_required=True,
    ... )
    ... def update_order(order_id: str, status: str) -> dict:
    ...     '''注文ステータスを更新'''
    ...     pass
"""

from __future__ import annotations

import functools
import inspect
import logging
from collections.abc import Callable
from enum import Enum
from typing import TYPE_CHECKING, Any, TypeVar

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from agentflow.security.policy_engine import AuthContext


# ツールレジストリ（グローバル）
_tool_registry: dict[str, RegisteredTool] = {}


class OperationType(str, Enum):
    """ツール操作タイプ."""

    READ = "read"       # 読み取り（安全）
    WRITE = "write"     # 書き込み（要注意）
    DELETE = "delete"   # 削除（危険）
    EXECUTE = "execute" # 外部実行（要監視）


class RiskLevel(str, Enum):
    """リスクレベル."""

    LOW = "low"           # 低リスク（自動承認可）
    MEDIUM = "medium"     # 中リスク（監査推奨）
    HIGH = "high"         # 高リスク（承認推奨）
    CRITICAL = "critical" # 重大リスク（承認必須）


class RegisteredTool(BaseModel):
    """登録されたツール情報.

    セキュリティ属性を含む完全なツール定義。
    全てのツールはこの契約に従う。

    Attributes:
        name: ツール名
        description: 説明
        func: 実行関数
        provider_type: プロバイダータイプ (method/mcp/skill)
        parameters: パラメータスキーマ
        operation_type: 操作タイプ (read/write/delete/execute)
        risk_level: リスクレベル (low/medium/high/critical)
        required_permissions: 必要な権限リスト
        requires_approval: HITL承認必須か
        audit_required: 監査ログ必須か
        max_execution_time: 最大実行時間（秒）
        rate_limit: レート制限（毎秒）
        plugin_id: 紐付け plugin ID
        plugin_version: 紐付け plugin バージョン
    """

    name: str = Field(..., description="ツール名")
    description: str = Field(default="", description="説明")
    func: Any = Field(..., description="実行関数")
    provider_type: str = Field(default="method", description="プロバイダータイプ")
    parameters: dict[str, Any] = Field(default_factory=dict, description="パラメータ")

    # ==========================================================================
    # L3セキュリティ属性（新規）
    # ==========================================================================
    operation_type: OperationType = Field(
        default=OperationType.READ,
        description="操作タイプ (read/write/delete/execute)",
    )
    risk_level: RiskLevel = Field(
        default=RiskLevel.LOW,
        description="リスクレベル (low/medium/high/critical)",
    )
    required_permissions: list[str] = Field(
        default_factory=list,
        description="必要な権限リスト（例: ['order:read', 'user:write']）",
    )
    requires_approval: bool = Field(
        default=False,
        description="HITL承認が必須か",
    )
    audit_required: bool = Field(
        default=False,
        description="監査ログ必須か",
    )
    max_execution_time: float = Field(
        default=30.0,
        description="最大実行時間（秒）",
    )
    rate_limit: int | None = Field(
        default=None,
        description="レート制限（毎秒の最大呼び出し数、Noneは無制限）",
    )
    plugin_id: str | None = Field(
        default=None,
        description="紐付け plugin ID",
    )
    plugin_version: str | None = Field(
        default=None,
        description="紐付け plugin バージョン",
    )

    model_config = {"arbitrary_types_allowed": True}

    def is_safe(self) -> bool:
        """安全なツールか（自動実行可）."""
        return (
            self.operation_type == OperationType.READ
            and self.risk_level == RiskLevel.LOW
            and not self.requires_approval
        )

    def needs_audit(self) -> bool:
        """監査が必要か."""
        return (
            self.audit_required
            or self.risk_level in (RiskLevel.HIGH, RiskLevel.CRITICAL)
            or self.operation_type in (OperationType.WRITE, OperationType.DELETE)
        )


T = TypeVar("T", bound=Callable[..., Any])


def tool(
    name: str | None = None,
    description: str | None = None,
    provider: str = "method",
    cache: bool = False,
    ttl: int = 3600,
    # ==========================================================================
    # L3セキュリティ属性（新規）
    # ==========================================================================
    operation_type: OperationType | str = OperationType.READ,
    risk_level: RiskLevel | str = RiskLevel.LOW,
    required_permissions: list[str] | None = None,
    requires_approval: bool = False,
    audit_required: bool = False,
    max_execution_time: float = 30.0,
    rate_limit: int | None = None,
    plugin_id: str | None = None,
    plugin_version: str | None = None,
) -> Callable[[T], T]:
    """ツール登録デコレータ.

    @tool で修飾されたメソッドは自動的にToolProviderに登録されます。
    セキュリティ属性を指定することで、権限チェック・監査・HITL承認を制御。

    Args:
        name: ツール名（省略時は関数名）
        description: 説明（省略時はdocstring）
        provider: プロバイダータイプ (method/mcp/skill)
        cache: キャッシュ有効化
        ttl: キャッシュTTL秒数
        operation_type: 操作タイプ (read/write/delete/execute)
        risk_level: リスクレベル (low/medium/high/critical)
        required_permissions: 必要な権限リスト
        requires_approval: HITL承認必須か
        audit_required: 監査ログ必須か
        max_execution_time: 最大実行時間（秒）
        rate_limit: レート制限（毎秒の最大呼び出し数）
        plugin_id: 紐付け plugin ID
        plugin_version: 紐付け plugin バージョン

    Returns:
        デコレートされた関数

    使用例:
        >>> # 基本的な使い方（低リスク読み取り）
        >>> @tool
        ... def search_products(query: str) -> list[dict]:
        ...     '''商品を検索'''
        ...     return db.search(query)
        >>>
        >>> # 高リスク書き込み操作
        >>> @tool(
        ...     name="update_order",
        ...     operation_type="write",
        ...     risk_level="high",
        ...     requires_approval=True,
        ...     required_permissions=["order:write"],
        ...     audit_required=True,
        ... )
        ... def update_order(order_id: str, status: str) -> dict:
        ...     '''注文ステータスを更新'''
        ...     pass
    """
    # 文字列をEnumに変換
    op_type = OperationType(operation_type) if isinstance(operation_type, str) else operation_type
    risk = RiskLevel(risk_level) if isinstance(risk_level, str) else risk_level
    perms = required_permissions or []

    def decorator(func: T) -> T:
        tool_name = name or func.__name__
        tool_desc = description or (func.__doc__ or "").strip().split("\n")[0]

        # パラメータスキーマを抽出
        sig = inspect.signature(func)
        parameters: dict[str, Any] = {}
        for param_name, param in sig.parameters.items():
            if param_name == "self":
                continue
            param_info: dict[str, Any] = {"type": "string"}  # デフォルト
            if param.annotation != inspect.Parameter.empty:
                if param.annotation == int:
                    param_info["type"] = "integer"
                elif param.annotation == float:
                    param_info["type"] = "number"
                elif param.annotation == bool:
                    param_info["type"] = "boolean"
                elif param.annotation == list:
                    param_info["type"] = "array"
                elif param.annotation == dict:
                    param_info["type"] = "object"
            if param.default != inspect.Parameter.empty:
                param_info["default"] = param.default
            parameters[param_name] = param_info

        # レジストリに登録（セキュリティ属性含む）
        registered = RegisteredTool(
            name=tool_name,
            description=tool_desc,
            func=func,
            provider_type=provider,
            parameters=parameters,
            operation_type=op_type,
            risk_level=risk,
            required_permissions=perms,
            requires_approval=requires_approval,
            audit_required=audit_required,
            max_execution_time=max_execution_time,
            rate_limit=rate_limit,
            plugin_id=plugin_id,
            plugin_version=plugin_version,
        )
        _tool_registry[tool_name] = registered

        # キャッシュラッパー（必要に応じて）
        if cache:
            # 簡易キャッシュ実装
            _cache: dict[str, Any] = {}

            @functools.wraps(func)
            async def cached_wrapper(*args: Any, **kwargs: Any) -> Any:
                cache_key = f"{tool_name}:{args}:{kwargs}"
                if cache_key in _cache:
                    return _cache[cache_key]
                result = await func(*args, **kwargs) if inspect.iscoroutinefunction(func) else func(*args, **kwargs)
                _cache[cache_key] = result
                return result

            return cached_wrapper  # type: ignore

        return func

    return decorator


class ToolExecutionError(Exception):
    """ツール実行エラー."""

    def __init__(
        self,
        message: str,
        tool_name: str,
        error_type: str = "execution_error",
    ) -> None:
        """初期化."""
        super().__init__(message)
        self.tool_name = tool_name
        self.error_type = error_type


class ToolSecurityError(ToolExecutionError):
    """ツールセキュリティエラー（権限不足、承認未取得等）."""

    def __init__(
        self,
        message: str,
        tool_name: str,
        missing_permissions: list[str] | None = None,
    ) -> None:
        """初期化."""
        super().__init__(message, tool_name, "security_error")
        self.missing_permissions = missing_permissions or []


class ToolProvider:
    """ツール統一プロバイダー.

    @tool で登録されたツールを自動発見し、統一インターフェースで呼び出し。
    セキュリティチェック、監査ログ、HITL承認と連携。

    使用例:
        >>> tools = ToolProvider.discover()
        >>> result = await tools.call("search_database", query="test")
        >>>
        >>> # セキュリティチェック付き呼び出し
        >>> from agentflow.security.policy_engine import AuthContext
        >>> auth = AuthContext(user_id="user-123", roles=["admin"])
        >>> result = await tools.call_secure("update_order", auth, order_id="o-1")
    """

    _default_instance: ToolProvider | None = None

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(__name__)
        self._tools = dict(_tool_registry)  # コピー

    @classmethod
    def discover(cls) -> ToolProvider:
        """@tool を自動発見してインスタンスを作成.

        Returns:
            ToolProvider: 発見されたツールを含むインスタンス
        """
        if cls._default_instance is None:
            cls._default_instance = cls()
        else:
            # レジストリを再読み込み
            cls._default_instance._tools = dict(_tool_registry)
        return cls._default_instance

    @classmethod
    def register(cls, registered_tool: RegisteredTool) -> None:
        """ツールを手動登録.

        Args:
            registered_tool: 登録するツール
        """
        _tool_registry[registered_tool.name] = registered_tool

    def list_tools(self) -> list[RegisteredTool]:
        """登録されたツール一覧を取得.

        Returns:
            ツールリスト
        """
        return list(self._tools.values())

    def list_safe_tools(self) -> list[RegisteredTool]:
        """安全なツール（自動実行可）のみ取得.

        Returns:
            安全なツールリスト
        """
        return [t for t in self._tools.values() if t.is_safe()]

    def list_tools_by_risk(
        self,
        max_risk: RiskLevel = RiskLevel.LOW,
    ) -> list[RegisteredTool]:
        """指定リスクレベル以下のツールを取得.

        Args:
            max_risk: 最大リスクレベル

        Returns:
            条件に合致するツールリスト
        """
        risk_order = [RiskLevel.LOW, RiskLevel.MEDIUM, RiskLevel.HIGH, RiskLevel.CRITICAL]
        max_idx = risk_order.index(max_risk)
        return [
            t for t in self._tools.values()
            if risk_order.index(t.risk_level) <= max_idx
        ]

    def list_tools_by_operation(
        self,
        operation_types: list[OperationType],
    ) -> list[RegisteredTool]:
        """指定操作タイプのツールを取得.

        Args:
            operation_types: 許可する操作タイプ

        Returns:
            条件に合致するツールリスト
        """
        return [t for t in self._tools.values() if t.operation_type in operation_types]

    def get_tool(self, name: str) -> RegisteredTool | None:
        """指定名のツールを取得.

        Args:
            name: ツール名

        Returns:
            ツール情報、または None
        """
        return self._tools.get(name)

    def check_permissions(
        self,
        tool_name: str,
        user_permissions: list[str],
    ) -> tuple[bool, list[str]]:
        """ツール実行に必要な権限をチェック.

        Args:
            tool_name: ツール名
            user_permissions: ユーザーの持つ権限リスト

        Returns:
            (許可されているか, 不足している権限リスト)
        """
        tool_info = self._tools.get(tool_name)
        if tool_info is None:
            return False, []

        missing = [p for p in tool_info.required_permissions if p not in user_permissions]
        return len(missing) == 0, missing

    async def call(self, name: str, **kwargs: Any) -> Any:
        """ツールを呼び出し（セキュリティチェックなし）.

        Args:
            name: ツール名
            **kwargs: パラメータ

        Returns:
            ツールの戻り値

        Raises:
            ValueError: ツールが見つからない場合
        """
        tool_info = self._tools.get(name)
        if tool_info is None:
            msg = f"Tool not found: {name}"
            raise ValueError(msg)

        func = tool_info.func

        # 非同期関数の場合
        if inspect.iscoroutinefunction(func):
            return await func(**kwargs)
        return func(**kwargs)

    async def call_secure(
        self,
        name: str,
        auth_context: AuthContext,
        **kwargs: Any,
    ) -> Any:
        """セキュリティチェック付きでツールを呼び出し.

        Args:
            name: ツール名
            auth_context: 認証コンテキスト（権限情報含む）
            **kwargs: パラメータ

        Returns:
            ツールの戻り値

        Raises:
            ValueError: ツールが見つからない場合
            ToolSecurityError: 権限が不足している場合
        """
        tool_info = self._tools.get(name)
        if tool_info is None:
            msg = f"Tool not found: {name}"
            raise ValueError(msg)

        # 権限チェック
        user_permissions = getattr(auth_context, "permissions", [])
        allowed, missing = self.check_permissions(name, user_permissions)
        if not allowed:
            msg = f"Permission denied for tool '{name}': missing {missing}"
            raise ToolSecurityError(msg, name, missing)

        # HITL承認チェック（要承認ツールの場合は例外）
        if tool_info.requires_approval:
            self._logger.warning(
                "Tool '%s' requires HITL approval. Use call_with_approval() instead.",
                name,
            )

        # 監査ログ
        if tool_info.needs_audit():
            self._logger.info(
                "AUDIT: tool=%s, user=%s, args=%s",
                name,
                getattr(auth_context, "user_id", "unknown"),
                kwargs,
            )

        return await self.call(name, **kwargs)

    def to_openai_tools(self) -> list[dict[str, Any]]:
        """OpenAI Function Calling形式に変換.

        Returns:
            OpenAI tools形式のリスト
        """
        result = []
        for tool_info in self._tools.values():
            result.append({
                "type": "function",
                "function": {
                    "name": tool_info.name,
                    "description": tool_info.description,
                    "parameters": {
                        "type": "object",
                        "properties": tool_info.parameters,
                    },
                },
            })
        return result

    def to_mcp_tools(self) -> list[dict[str, Any]]:
        """MCP Tools形式に変換.

        Returns:
            MCP tools形式のリスト
        """
        result = []
        for tool_info in self._tools.values():
            result.append({
                "name": tool_info.name,
                "description": tool_info.description,
                "inputSchema": {
                    "type": "object",
                    "properties": tool_info.parameters,
                },
            })
        return result

    def get_security_summary(self) -> dict[str, Any]:
        """全ツールのセキュリティサマリを取得.

        Returns:
            セキュリティサマリ辞書
        """
        tools = list(self._tools.values())
        return {
            "total_tools": len(tools),
            "by_risk_level": {
                "low": len([t for t in tools if t.risk_level == RiskLevel.LOW]),
                "medium": len([t for t in tools if t.risk_level == RiskLevel.MEDIUM]),
                "high": len([t for t in tools if t.risk_level == RiskLevel.HIGH]),
                "critical": len([t for t in tools if t.risk_level == RiskLevel.CRITICAL]),
            },
            "by_operation": {
                "read": len([t for t in tools if t.operation_type == OperationType.READ]),
                "write": len([t for t in tools if t.operation_type == OperationType.WRITE]),
                "delete": len([t for t in tools if t.operation_type == OperationType.DELETE]),
                "execute": len([t for t in tools if t.operation_type == OperationType.EXECUTE]),
            },
            "requires_approval": len([t for t in tools if t.requires_approval]),
            "audit_required": len([t for t in tools if t.audit_required]),
            "safe_tools": len([t for t in tools if t.is_safe()]),
        }
