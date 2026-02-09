"""スキルマネージャーモジュール.

スキルの管理、ワークフロー作成、自然言語からのスキル生成を提供。

使用例:
    >>> manager = SkillsManager(engine, gateway)
    >>> skills = await manager.list_available_skills()
    >>> workflow = await manager.create_workflow(definition)
    >>> result = await manager.run_workflow(workflow.id, params)
"""

from __future__ import annotations

import json
import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow.providers import get_llm


if TYPE_CHECKING:
    from agentflow.skills.gateway import SkillGateway


class WorkflowStatus(str, Enum):
    """ワークフローステータス."""

    DRAFT = "draft"
    ACTIVE = "active"
    PAUSED = "paused"
    ARCHIVED = "archived"


@dataclass
class WorkflowStep:
    """ワークフローステップ.

    Attributes:
        id: ステップID
        skill_name: スキル名
        params: パラメータ（前ステップの結果を参照可能）
        condition: 実行条件（前ステップの結果に基づく）
        on_success: 成功時の次ステップID
        on_failure: 失敗時の次ステップID
        timeout_seconds: タイムアウト秒数
    """

    id: str
    skill_name: str
    params: dict[str, Any] = field(default_factory=dict)
    condition: str | None = None
    on_success: str | None = None
    on_failure: str | None = None
    timeout_seconds: int = 60

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "skill_name": self.skill_name,
            "params": self.params,
            "condition": self.condition,
            "on_success": self.on_success,
            "on_failure": self.on_failure,
            "timeout_seconds": self.timeout_seconds,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> WorkflowStep:
        """辞書から生成."""
        return cls(
            id=data.get("id", str(uuid.uuid4())),
            skill_name=data["skill_name"],
            params=data.get("params", {}),
            condition=data.get("condition"),
            on_success=data.get("on_success"),
            on_failure=data.get("on_failure"),
            timeout_seconds=data.get("timeout_seconds", 60),
        )


@dataclass
class Workflow:
    """ワークフロー定義.

    Attributes:
        id: ワークフローID
        name: ワークフロー名
        description: 説明
        steps: ステップリスト
        entry_step_id: エントリーポイントステップID
        status: ステータス
        created_at: 作成日時
        updated_at: 更新日時
        metadata: メタデータ
    """

    id: str
    name: str
    description: str
    steps: list[WorkflowStep]
    entry_step_id: str
    status: WorkflowStatus = WorkflowStatus.DRAFT
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "name": self.name,
            "description": self.description,
            "steps": [s.to_dict() for s in self.steps],
            "entry_step_id": self.entry_step_id,
            "status": self.status.value,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> Workflow:
        """辞書から生成."""
        steps = [WorkflowStep.from_dict(s) for s in data.get("steps", [])]
        return cls(
            id=data.get("id", str(uuid.uuid4())),
            name=data["name"],
            description=data.get("description", ""),
            steps=steps,
            entry_step_id=data.get("entry_step_id", steps[0].id if steps else ""),
            status=WorkflowStatus(data.get("status", "draft")),
            created_at=datetime.fromisoformat(data["created_at"])
            if "created_at" in data
            else datetime.now(),
            updated_at=datetime.fromisoformat(data["updated_at"])
            if "updated_at" in data
            else datetime.now(),
            metadata=data.get("metadata", {}),
        )


@dataclass
class WorkflowRunResult:
    """ワークフロー実行結果.

    Attributes:
        workflow_id: ワークフローID
        run_id: 実行ID
        status: 最終ステータス
        step_results: ステップ別結果
        started_at: 開始日時
        completed_at: 完了日時
        error: エラーメッセージ
    """

    workflow_id: str
    run_id: str
    status: str
    step_results: list[dict[str, Any]]
    started_at: datetime
    completed_at: datetime | None = None
    error: str | None = None

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "workflow_id": self.workflow_id,
            "run_id": self.run_id,
            "status": self.status,
            "step_results": self.step_results,
            "started_at": self.started_at.isoformat(),
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "error": self.error,
        }


@dataclass
class SkillInfo:
    """スキル情報.

    Attributes:
        name: スキル名
        description: 説明
        category: カテゴリ
        risk_level: リスクレベル
        parameters: パラメータ定義
        requires_confirmation: 確認が必要か
        enabled: 有効フラグ
    """

    name: str
    description: str
    category: str
    risk_level: str
    parameters: dict[str, Any] = field(default_factory=dict)
    requires_confirmation: bool = False
    enabled: bool = True

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "description": self.description,
            "category": self.category,
            "risk_level": self.risk_level,
            "parameters": self.parameters,
            "requires_confirmation": self.requires_confirmation,
            "enabled": self.enabled,
        }


class SkillsManager:
    """スキルマネージャー.

    スキルの管理、ワークフロー作成・実行を担当。
    """

    def __init__(
        self,
        gateway: SkillGateway,
        websocket_hub: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            gateway: スキルゲートウェイ
            websocket_hub: WebSocketHub
        """
        self._gateway = gateway
        self._hub = websocket_hub
        self._workflows: dict[str, Workflow] = {}
        self._disabled_skills: set[str] = set()
        self._logger = logging.getLogger(__name__)

    async def list_available_skills(self) -> list[SkillInfo]:
        """利用可能なスキル一覧.

        Returns:
            スキル情報リスト
        """
        skills = self._gateway.list_available_skills()
        return [
            SkillInfo(
                name=s.name,
                description=s.description,
                category=s.category.value,
                risk_level=s.risk_level.value,
                parameters=s.parameters,
                requires_confirmation=s.requires_confirmation,
                enabled=s.name not in self._disabled_skills,
            )
            for s in skills
        ]

    async def list_all_skills(self) -> list[SkillInfo]:
        """全スキル一覧.

        Returns:
            スキル情報リスト
        """
        skills = self._gateway.list_skills()
        return [
            SkillInfo(
                name=s.name,
                description=s.description,
                category=s.category.value,
                risk_level=s.risk_level.value,
                parameters=s.parameters,
                requires_confirmation=s.requires_confirmation,
                enabled=s.name not in self._disabled_skills,
            )
            for s in skills
        ]

    def enable_skill(self, skill_name: str) -> bool:
        """スキルを有効化."""
        if skill_name in self._disabled_skills:
            self._disabled_skills.remove(skill_name)
            self._logger.info("スキル有効化: %s", skill_name)
            return True
        return False

    def disable_skill(self, skill_name: str) -> bool:
        """スキルを無効化."""
        self._disabled_skills.add(skill_name)
        self._logger.info("スキル無効化: %s", skill_name)
        return True

    async def generate_skill_from_description(
        self,
        description: str,
        examples: list[str] | None = None,
    ) -> dict[str, Any]:
        """自然言語説明からスキルを生成.

        Args:
            description: スキルの説明
            examples: 使用例

        Returns:
            生成されたスキル定義
        """
        llm = get_llm(temperature=0.3)

        prompt = f"""以下の説明に基づいて、スキル定義を JSON 形式で生成してください。

説明: {description}

{"使用例: " + ", ".join(examples) if examples else ""}

出力形式:
{{
    "name": "スキル名（英語、snake_case）",
    "description": "日本語の説明",
    "category": "os_read | os_write | os_execute | browser | network のいずれか",
    "risk_level": "low | medium | high | critical のいずれか",
    "parameters": {{
        "param_name": {{
            "type": "string | number | boolean",
            "description": "パラメータの説明",
            "required": true/false
        }}
    }},
    "workflow_steps": [
        {{
            "skill_name": "使用する既存スキル名",
            "params": {{"param": "value"}}
        }}
    ]
}}

JSON のみを出力してください。"""

        response = await llm.chat(
            [
                {
                    "role": "system",
                    "content": "スキル定義を生成するアシスタントです。JSON形式で出力します。",
                },
                {"role": "user", "content": prompt},
            ]
        )

        content = response.get("content", "")

        # JSON を抽出
        try:
            # コードブロックを除去
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            elif "```" in content:
                content = content.split("```")[1].split("```")[0]

            skill_def = json.loads(content.strip())
            self._logger.info("スキル生成成功: %s", skill_def.get("name", "unknown"))
            return skill_def

        except json.JSONDecodeError as e:
            self._logger.exception("スキル定義のパースに失敗: %s", e)
            return {
                "error": "スキル定義の生成に失敗しました",
                "raw_response": content,
            }

    async def create_workflow(self, definition: dict[str, Any]) -> Workflow:
        """ワークフローを作成.

        Args:
            definition: ワークフロー定義

        Returns:
            作成されたワークフロー
        """
        workflow = Workflow.from_dict(
            {
                "id": str(uuid.uuid4()),
                **definition,
            }
        )

        self._workflows[workflow.id] = workflow
        self._logger.info("ワークフロー作成: id=%s, name=%s", workflow.id, workflow.name)

        return workflow

    async def update_workflow(
        self,
        workflow_id: str,
        updates: dict[str, Any],
    ) -> Workflow | None:
        """ワークフローを更新.

        Args:
            workflow_id: ワークフローID
            updates: 更新内容

        Returns:
            更新されたワークフロー
        """
        workflow = self._workflows.get(workflow_id)
        if workflow is None:
            return None

        if "name" in updates:
            workflow.name = updates["name"]
        if "description" in updates:
            workflow.description = updates["description"]
        if "steps" in updates:
            workflow.steps = [WorkflowStep.from_dict(s) for s in updates["steps"]]
        if "entry_step_id" in updates:
            workflow.entry_step_id = updates["entry_step_id"]
        if "status" in updates:
            workflow.status = WorkflowStatus(updates["status"])
        if "metadata" in updates:
            workflow.metadata.update(updates["metadata"])

        workflow.updated_at = datetime.now()
        self._logger.info("ワークフロー更新: id=%s", workflow_id)

        return workflow

    async def delete_workflow(self, workflow_id: str) -> bool:
        """ワークフローを削除.

        Args:
            workflow_id: ワークフローID

        Returns:
            削除成功したか
        """
        if workflow_id in self._workflows:
            del self._workflows[workflow_id]
            self._logger.info("ワークフロー削除: id=%s", workflow_id)
            return True
        return False

    def get_workflow(self, workflow_id: str) -> Workflow | None:
        """ワークフローを取得."""
        return self._workflows.get(workflow_id)

    def list_workflows(
        self,
        status_filter: WorkflowStatus | None = None,
    ) -> list[Workflow]:
        """ワークフロー一覧.

        Args:
            status_filter: ステータスフィルター

        Returns:
            ワークフローリスト
        """
        workflows = list(self._workflows.values())
        if status_filter:
            workflows = [w for w in workflows if w.status == status_filter]
        return sorted(workflows, key=lambda w: w.updated_at, reverse=True)

    async def run_workflow(
        self,
        workflow_id: str,
        params: dict[str, Any] | None = None,
        dry_run: bool = False,
    ) -> WorkflowRunResult:
        """ワークフローを実行.

        Args:
            workflow_id: ワークフローID
            params: 初期パラメータ
            dry_run: 検証のみ

        Returns:
            実行結果
        """
        workflow = self._workflows.get(workflow_id)
        if workflow is None:
            return WorkflowRunResult(
                workflow_id=workflow_id,
                run_id=str(uuid.uuid4()),
                status="error",
                step_results=[],
                started_at=datetime.now(),
                completed_at=datetime.now(),
                error=f"ワークフローが見つかりません: {workflow_id}",
            )

        if workflow.status != WorkflowStatus.ACTIVE and not dry_run:
            return WorkflowRunResult(
                workflow_id=workflow_id,
                run_id=str(uuid.uuid4()),
                status="error",
                step_results=[],
                started_at=datetime.now(),
                completed_at=datetime.now(),
                error=f"ワークフローはアクティブではありません: {workflow.status.value}",
            )

        run_id = str(uuid.uuid4())
        started_at = datetime.now()
        step_results: list[dict[str, Any]] = []
        context = {"params": params or {}, "results": {}}

        self._logger.info("ワークフロー実行開始: workflow=%s, run=%s", workflow_id, run_id)

        # ステップをマップに変換
        step_map = {s.id: s for s in workflow.steps}
        current_step_id = workflow.entry_step_id

        while current_step_id:
            step = step_map.get(current_step_id)
            if step is None:
                step_results.append(
                    {
                        "step_id": current_step_id,
                        "status": "error",
                        "error": "ステップが見つかりません",
                    }
                )
                break

            # 条件チェック
            if step.condition:
                try:
                    # 簡易的な条件評価（実際にはより安全な方法を使用）
                    condition_result = eval(step.condition, {"__builtins__": {}}, context)
                    if not condition_result:
                        step_results.append(
                            {
                                "step_id": step.id,
                                "skill_name": step.skill_name,
                                "status": "skipped",
                                "reason": "条件不成立",
                            }
                        )
                        current_step_id = step.on_success
                        continue
                except Exception as e:
                    self._logger.warning("条件評価エラー: %s", e)

            # パラメータを解決
            resolved_params = self._resolve_params(step.params, context)

            # スキル実行
            try:
                result = await self._gateway.call(
                    step.skill_name,
                    resolved_params,
                    dry_run=dry_run,
                )

                step_result = {
                    "step_id": step.id,
                    "skill_name": step.skill_name,
                    "status": "success" if result.success else "failed",
                    "result": result.result,
                    "error": result.error,
                    "duration_ms": result.duration_ms,
                    "dry_run": dry_run,
                }
                step_results.append(step_result)

                # コンテキスト更新
                context["results"][step.id] = result.result

                # 次ステップ決定
                current_step_id = step.on_success if result.success else step.on_failure

            except Exception as e:
                self._logger.exception("ステップ実行エラー: %s - %s", step.id, e)
                step_results.append(
                    {
                        "step_id": step.id,
                        "skill_name": step.skill_name,
                        "status": "error",
                        "error": str(e),
                    }
                )
                current_step_id = step.on_failure

        # 最終結果
        completed_at = datetime.now()
        final_status = (
            "success"
            if all(r.get("status") in ("success", "skipped") for r in step_results)
            else "failed"
        )

        result = WorkflowRunResult(
            workflow_id=workflow_id,
            run_id=run_id,
            status=final_status,
            step_results=step_results,
            started_at=started_at,
            completed_at=completed_at,
        )

        self._logger.info(
            "ワークフロー実行完了: workflow=%s, run=%s, status=%s",
            workflow_id,
            run_id,
            final_status,
        )

        return result

    def _resolve_params(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """パラメータを解決.

        パラメータ値に "${results.step_id.key}" 形式の参照があれば
        コンテキストから値を取得。

        Args:
            params: パラメータ
            context: コンテキスト

        Returns:
            解決されたパラメータ
        """
        resolved = {}
        for key, value in params.items():
            if isinstance(value, str) and value.startswith("${") and value.endswith("}"):
                # 参照を解決
                ref_path = value[2:-1].split(".")
                try:
                    current = context
                    for part in ref_path:
                        current = current.get(part, {}) if isinstance(current, dict) else {}
                    resolved[key] = current if current != {} else value
                except Exception:
                    resolved[key] = value
            else:
                resolved[key] = value
        return resolved

    async def validate_workflow(self, workflow: Workflow) -> list[str]:
        """ワークフローを検証.

        Args:
            workflow: ワークフロー

        Returns:
            エラーメッセージリスト
        """
        errors = []

        # エントリーポイントチェック
        step_ids = {s.id for s in workflow.steps}
        if workflow.entry_step_id not in step_ids:
            errors.append(f"エントリーポイントが見つかりません: {workflow.entry_step_id}")

        # 各ステップを検証
        available_skills = {s.name for s in self._gateway.list_available_skills()}

        for step in workflow.steps:
            # スキル存在チェック
            if step.skill_name not in available_skills:
                errors.append(f"ステップ {step.id}: スキルが見つかりません - {step.skill_name}")

            # 次ステップ参照チェック
            if step.on_success and step.on_success not in step_ids:
                errors.append(f"ステップ {step.id}: on_success 参照が無効 - {step.on_success}")
            if step.on_failure and step.on_failure not in step_ids:
                errors.append(f"ステップ {step.id}: on_failure 参照が無効 - {step.on_failure}")

        return errors
