"""スキルゲートウェイ.

全てのスキル呼び出しを統一管理するセキュリティゲートウェイ。
Agent は直接スキルを呼び出せず、必ずこのゲートウェイを経由する。

設計原則:
- Agent は直接 OS/Browser コマンドを実行できない
- 全てのセキュリティはコード制御
- 宣言済み API（Skill）のみ呼び出し可能
- 不確定時は停止して人工確認を要求

Example:
    >>> gateway = SkillGateway(config)
    >>> result = await gateway.call("read_file", {"path": "data.txt"})
    >>> # 高リスク操作は人工確認が必要
    >>> result = await gateway.call("write_file", {"path": "out.txt", "content": "..."})
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from collections.abc import Awaitable, Callable


class SkillCategory(str, Enum):
    """スキルカテゴリ."""

    OS_READ = "os_read"        # OS読み取り（安全）
    OS_WRITE = "os_write"      # OS書き込み（要注意）
    OS_EXECUTE = "os_execute"  # OS実行（要監視）
    BROWSER = "browser"        # ブラウザ操作
    NETWORK = "network"        # ネットワーク


class RiskLevel(str, Enum):
    """リスクレベル."""

    LOW = "low"           # 低リスク（自動承認可）
    MEDIUM = "medium"     # 中リスク（監査推奨）
    HIGH = "high"         # 高リスク（承認推奨）
    CRITICAL = "critical" # 重大リスク（承認必須）


@dataclass
class SkillDefinition:
    """スキル定義."""

    name: str
    description: str
    category: SkillCategory
    risk_level: RiskLevel
    handler: Callable[..., Awaitable[Any]]
    parameters: dict[str, Any] = field(default_factory=dict)
    requires_confirmation: bool = False
    allowed_in_isolated: bool = True
    allowed_in_real_machine: bool = True


@dataclass
class SkillResult:
    """スキル実行結果."""

    success: bool
    skill_name: str
    result: Any = None
    error: str | None = None
    duration_ms: float = 0.0
    executed_at: datetime = field(default_factory=datetime.now)
    dry_run: bool = False

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "success": self.success,
            "skill_name": self.skill_name,
            "result": self.result,
            "error": self.error,
            "duration_ms": self.duration_ms,
            "executed_at": self.executed_at.isoformat(),
            "dry_run": self.dry_run,
        }


class GatewayConfig(BaseModel):
    """ゲートウェイ設定."""

    execution_mode: str = Field(default="isolated", description="実行モード (isolated/real_machine)")
    workspace_path: Path = Field(default_factory=Path.cwd, description="ワークスペースパス")
    require_confirmation_for_high_risk: bool = Field(default=True, description="高リスク操作時の確認要求")
    audit_all_calls: bool = Field(default=True, description="全呼び出しを監査")
    max_calls_per_minute: int = Field(default=60, description="1分あたりの最大呼び出し数")

    model_config = {"arbitrary_types_allowed": True}


class SkillGatewayError(Exception):
    """ゲートウェイエラー."""

    def __init__(self, message: str, skill_name: str = "", code: str = "gateway_error") -> None:
        """初期化."""
        super().__init__(message)
        self.skill_name = skill_name
        self.code = code


class SkillNotFoundError(SkillGatewayError):
    """スキルが見つからない."""

    def __init__(self, skill_name: str) -> None:
        super().__init__(f"スキル '{skill_name}' は登録されていません", skill_name, "skill_not_found")


class SkillPermissionError(SkillGatewayError):
    """スキル実行権限エラー."""

    def __init__(self, message: str, skill_name: str) -> None:
        super().__init__(message, skill_name, "permission_denied")


class HumanConfirmationRequired(SkillGatewayError):
    """人工確認が必要."""

    def __init__(self, skill_name: str, reason: str) -> None:
        super().__init__(f"人工確認が必要: {reason}", skill_name, "confirmation_required")
        self.reason = reason


class SkillGateway:
    """スキルゲートウェイ.

    全てのスキル呼び出しを統一管理。セキュリティチェック、監査ログ、
    人工確認連携を提供。
    """

    def __init__(self, config: GatewayConfig | None = None) -> None:
        """初期化."""
        self._config = config or GatewayConfig()
        self._skills: dict[str, SkillDefinition] = {}
        self._logger = logging.getLogger(__name__)
        self._call_count = 0
        self._confirmation_handler: Callable[[str, str, dict], Awaitable[bool]] | None = None
        self._mode_switcher: Any = None  # ModeSwitcher（遅延初期化）

    @property
    def execution_mode(self) -> str:
        """現在の実行モード."""
        return self._config.execution_mode

    @property
    def config(self) -> GatewayConfig:
        """設定を取得."""
        return self._config

    def get_mode_switcher(self) -> Any:
        """モード切替機構を取得（遅延初期化）."""
        if self._mode_switcher is None:
            from agentflow.skills.mode_switcher import ModeSwitcher
            self._mode_switcher = ModeSwitcher(self)
        return self._mode_switcher

    def register_skill(self, skill: SkillDefinition) -> None:
        """スキルを登録."""
        self._skills[skill.name] = skill
        self._logger.debug(f"スキル登録: {skill.name} (category={skill.category.value})")

    def list_skills(self) -> list[SkillDefinition]:
        """登録スキル一覧."""
        return list(self._skills.values())

    def list_available_skills(self) -> list[SkillDefinition]:
        """現在の実行モードで利用可能なスキル一覧."""
        is_isolated = self._config.execution_mode == "isolated"
        return [
            s for s in self._skills.values()
            if (is_isolated and s.allowed_in_isolated) or (not is_isolated and s.allowed_in_real_machine)
        ]

    def set_confirmation_handler(
        self,
        handler: Callable[[str, str, dict], Awaitable[bool]],
    ) -> None:
        """人工確認ハンドラを設定."""
        self._confirmation_handler = handler

    async def call(
        self,
        skill_name: str,
        params: dict[str, Any],
        *,
        dry_run: bool = False,
        skip_confirmation: bool = False,
    ) -> SkillResult:
        """スキルを呼び出し.

        Args:
            skill_name: スキル名
            params: パラメータ
            dry_run: True の場合は検証のみ
            skip_confirmation: 確認をスキップ（危険）

        Returns:
            実行結果

        Raises:
            SkillNotFoundError: スキルが見つからない
            SkillPermissionError: 実行権限がない
            HumanConfirmationRequired: 人工確認が必要
        """
        import asyncio

        # 1. スキル存在チェック
        skill = self._skills.get(skill_name)
        if skill is None:
            raise SkillNotFoundError(skill_name)

        # 2. 実行モードチェック
        is_isolated = self._config.execution_mode == "isolated"
        if is_isolated and not skill.allowed_in_isolated:
            msg = f"スキル '{skill_name}' は isolated モードでは使用できません"
            raise SkillPermissionError(
                msg,
                skill_name,
            )

        # 3. 人工確認チェック
        needs_confirmation = (
            skill.requires_confirmation
            or (skill.risk_level in (RiskLevel.HIGH, RiskLevel.CRITICAL)
                and self._config.require_confirmation_for_high_risk)
        )

        if needs_confirmation and not skip_confirmation and not dry_run:
            if self._confirmation_handler:
                confirmed = await self._confirmation_handler(
                    skill_name,
                    f"リスクレベル: {skill.risk_level.value}",
                    params,
                )
                if not confirmed:
                    raise HumanConfirmationRequired(skill_name, "ユーザーが拒否しました")
            else:
                raise HumanConfirmationRequired(
                    skill_name,
                    f"高リスク操作 ({skill.risk_level.value}) には人工確認が必要です",
                )

        # 4. 監査ログ
        if self._config.audit_all_calls:
            self._logger.info(
                "GATEWAY: skill=%s, params=%s, dry_run=%s, mode=%s",
                skill_name, params, dry_run, self._config.execution_mode,
            )

        # 5. dry_run の場合は検証のみ
        if dry_run:
            return SkillResult(
                success=True,
                skill_name=skill_name,
                result={"validated": True, "params": params},
                dry_run=True,
            )

        # 6. 実行
        start_time = asyncio.get_event_loop().time()
        try:
            result = await skill.handler(**params)
            duration_ms = (asyncio.get_event_loop().time() - start_time) * 1000

            return SkillResult(
                success=True,
                skill_name=skill_name,
                result=result,
                duration_ms=duration_ms,
            )

        except Exception as e:
            duration_ms = (asyncio.get_event_loop().time() - start_time) * 1000
            self._logger.exception(f"スキル実行エラー: {skill_name} - {e}")

            return SkillResult(
                success=False,
                skill_name=skill_name,
                error=str(e),
                duration_ms=duration_ms,
            )

