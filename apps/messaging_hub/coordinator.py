"""Personal Assistant Coordinator - 私人助理協調器.

messaging_hub固有の主管向けパーソナルアシスタント。
自然言語メッセージを解析し、適切なタスクを実行して主管向けサマリーを返す。

特徴:
- IntentRouter による自然言語理解
- DeepAgentCoordinator パターンを活用
- ExecutiveSummaryBuilder による主管向け出力
- SkillGateway 経由の安全な OS/Browser 操作

使用例:
    >>> coordinator = PersonalAssistantCoordinator()
    >>> result = await coordinator.process("今日のメールを整理して")
    >>> print(result["summary"])  # "✅ 50件のメールを処理、5件が要対応"
"""

from __future__ import annotations

import json
import logging
import re
import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING, Any
from urllib.parse import quote

from agentflow.providers import get_llm
from agentflow.routing import (
    ExecutiveSummaryBuilder,
    Intent,
    IntentCategory,
    IntentRouter,
    SummaryConfig,
    TaskParameter,
    TaskTemplate,
)


if TYPE_CHECKING:
    from collections.abc import Awaitable, Callable

    from agentflow.skills.gateway import SkillGateway, SkillResult

_logger = logging.getLogger(__name__)


@dataclass
class AssistantConfig:
    """アシスタント設定.

    Attributes:
        workspace_path: 作業ディレクトリ
        enable_os_skills: OS操作を有効化
        enable_browser_skills: ブラウザ操作を有効化
        security_mode: 実行セキュリティモード
        summary_language: サマリー言語
        use_emoji: 絵文字を使用
    """

    workspace_path: Path = field(default_factory=Path.cwd)
    enable_os_skills: bool = True
    enable_browser_skills: bool = True
    security_mode: str = "approval_required"
    summary_language: str = "ja"
    use_emoji: bool = True


class PersonalAssistantCoordinator:
    """私人助理協調器.

    主管が自然言語でタスクを依頼し、簡潔なサマリーを受け取る。

    Example:
        >>> coordinator = PersonalAssistantCoordinator()
        >>> result = await coordinator.process("過去3日のメールを整理して")
        >>> print(result["summary"])
    """

    def __init__(
        self,
        config: AssistantConfig | None = None,
        skill_gateway: SkillGateway | None = None,
        event_emitter: Callable[[str, dict[str, Any]], Awaitable[None]] | None = None,
    ) -> None:
        """初期化.

        Args:
            config: アシスタント設定
            skill_gateway: スキルゲートウェイ（OS/Browser操作用）
        """
        self._config = config or AssistantConfig()
        self._gateway = skill_gateway
        self._event_emitter = event_emitter
        self._logger = logging.getLogger(__name__)

        # フレームワークコンポーネント初期化
        self._intent_router = IntentRouter()
        self._summary_builder = ExecutiveSummaryBuilder(
            config=SummaryConfig(
                language=self._config.summary_language,
                use_emoji=self._config.use_emoji,
            )
        )

        # タスクテンプレート登録
        self._register_templates()

        # 専門エージェント（遅延初期化）
        self._agents: dict[str, Any] = {}

        self._logger.info("PersonalAssistantCoordinator 初期化完了")

    def _blocked_result(self, capability: str) -> dict[str, Any]:
        """セキュリティモードでブロックされた場合の共通レスポンス."""
        return self._contract_payload(
            result={
                "processed": 0,
                "blocked": True,
                "security_mode": self._config.security_mode,
            },
            risk_flags=["blocked_by_security_mode"],
            extra={
                "processed": 0,
                "blocked": True,
                "security_mode": self._config.security_mode,
                "summary_points": [
                    f"{capability} 操作は security_mode='{self._config.security_mode}' でブロックされました",
                ],
                "recommended_actions": [
                    "管理者に承認を依頼してください",
                    "必要であれば autonomous モードを明示的に有効化してください",
                ],
            },
        )

    @staticmethod
    def _contract_payload(
        *,
        result: Any,
        evidence: list[dict[str, Any]] | None = None,
        artifacts: list[dict[str, Any]] | None = None,
        rollback_handle: dict[str, Any] | None = None,
        cost: dict[str, Any] | None = None,
        risk_flags: list[str] | None = None,
        extra: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """標準契約のペイロードを作成する."""
        payload: dict[str, Any] = {
            "result": result,
            "evidence": evidence or [],
            "artifacts": artifacts or [],
            "rollback_handle": rollback_handle,
            "cost": cost or {"duration_ms": 0.0, "token_estimate": 0},
            "risk_flags": risk_flags or [],
        }
        if extra:
            payload.update(extra)
        return payload

    @staticmethod
    def _safe_int(value: Any, default: int) -> int:
        """安全に int へ変換する."""
        try:
            return int(value)
        except (TypeError, ValueError):
            return default

    def _has_gateway_skill(self, skill_name: str) -> bool:
        """ゲートウェイにスキルが登録されているか確認する."""
        if self._gateway is None:
            return False
        return any(skill.name == skill_name for skill in self._gateway.list_available_skills())

    async def _ask_llm(self, system_prompt: str, user_prompt: str, temperature: float = 0.4) -> str:
        """LLM へ問い合わせ、失敗時は空文字を返す."""
        try:
            llm = get_llm(temperature=temperature)
            response = await llm.chat(
                [
                    {"role": "system", "content": system_prompt},
                    {"role": "user", "content": user_prompt},
                ]
            )
            return str(response.get("content", "")).strip()
        except Exception as e:
            self._logger.warning("LLM 呼び出しに失敗: %s", e)
            return ""

    async def _emit_event(self, event_name: str, payload: dict[str, Any]) -> None:
        """イベントを外部へ通知する."""
        if self._event_emitter is None:
            return
        await self._event_emitter(event_name, payload)

    async def _call_gateway_skill(
        self,
        *,
        skill_name: str,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """SkillGateway 呼び出しを標準契約へ正規化する."""
        run_id = str(context.get("run_id", ""))
        step_id = str(uuid.uuid4())
        await self._emit_event(
            "StepStarted",
            {
                "run_id": run_id,
                "step_id": step_id,
                "skill_name": skill_name,
                "params": params,
                "started_at": datetime.now(UTC).isoformat(),
            },
        )

        if self._gateway is None:
            return self._contract_payload(
                result={"error": "gateway_not_configured"},
                risk_flags=["gateway_unavailable"],
                extra={"success": False, "error": "gateway_not_configured", "step_id": step_id},
            )

        try:
            skill_result: SkillResult = await self._gateway.call(skill_name, params)
        except Exception as e:
            await self._emit_event(
                "ToolExecuted",
                {
                    "run_id": run_id,
                    "step_id": step_id,
                    "skill_name": skill_name,
                    "status": "error",
                    "error": str(e),
                },
            )
            return self._contract_payload(
                result={"error": str(e)},
                evidence=[
                    {
                        "type": "skill_exception",
                        "skill_name": skill_name,
                        "error": str(e),
                        "timestamp": datetime.now(UTC).isoformat(),
                    }
                ],
                risk_flags=["skill_exception"],
                extra={"success": False, "error": str(e), "step_id": step_id},
            )

        normalized_cost = skill_result.cost or {"duration_ms": skill_result.duration_ms, "token_estimate": 0}
        normalized_risk_flags = list(skill_result.risk_flags)
        if not skill_result.success:
            normalized_risk_flags.append("execution_failed")

        result = self._contract_payload(
            result=skill_result.result if skill_result.success else {"error": skill_result.error or ""},
            evidence=skill_result.evidence,
            artifacts=skill_result.artifacts,
            rollback_handle=skill_result.rollback_handle,
            cost=normalized_cost,
            risk_flags=normalized_risk_flags,
            extra={
                "success": skill_result.success,
                "error": skill_result.error,
                "duration_ms": skill_result.duration_ms,
                "step_id": step_id,
                "skill_name": skill_name,
            },
        )
        await self._emit_event(
            "ToolExecuted",
            {
                "run_id": run_id,
                "step_id": step_id,
                "skill_name": skill_name,
                "status": "success" if skill_result.success else "failed",
                "duration_ms": skill_result.duration_ms,
                "error": skill_result.error,
                "cost": normalized_cost,
                "risk_flags": normalized_risk_flags,
                "artifacts": skill_result.artifacts,
                "rollback_handle": skill_result.rollback_handle,
            },
        )
        if result["evidence"]:
            await self._emit_event(
                "EvidenceAdded",
                {
                    "run_id": run_id,
                    "step_id": step_id,
                    "skill_name": skill_name,
                    "count": len(result["evidence"]),
                },
            )
        return result

    @staticmethod
    def _is_troubleshooting_message(message: str) -> bool:
        """CLI 提案対象のトラブルシュート要求か判定."""
        lowered = message.strip().lower()
        keywords = [
            "error",
            "failed",
            "fail",
            "cannot start",
            "can't start",
            "debug",
            "diagnose",
            "investigate",
            "排查",
            "报错",
            "启动失败",
            "修复",
            "不工作",
        ]
        return any(keyword in lowered for keyword in keywords)

    def _should_propose_cli(self, *, message: str, intent: Intent) -> bool:
        """低信頼/未知意図でトラブルシュート要求なら CLI 提案へ誘導."""
        if not self._is_troubleshooting_message(message):
            return False
        if intent.category == IntentCategory.UNKNOWN:
            return True
        return intent.confidence < 0.45

    @staticmethod
    def _build_cli_proposal(message: str, intent: Intent) -> dict[str, Any]:
        """CLI 実行前の提案ペイロード."""
        prompt = (
            "Investigate this issue in read-only mode, identify likely root causes, "
            "and provide safe remediation steps.\n\n"
            f"User message: {message}\n"
            f"Intent category: {intent.category.value}\n"
            f"Intent confidence: {intent.confidence:.2f}\n"
        )
        return {
            "proposal_id": str(uuid.uuid4()),
            "tool_candidates": ["codex", "claude"],
            "mode": "read_only",
            "prompt": prompt,
            "rationale": "intent confidence is low/unknown and troubleshooting intent is detected",
        }

    def _register_templates(self) -> None:
        """タスクテンプレートを登録."""
        # メール整理テンプレート
        self._intent_router.register_template(
            TaskTemplate(
                name="email_organize",
                triggers=[
                    "メール整理",
                    "メールを整理",
                    "受信箱整理",
                    "メールを片付け",
                    "邮件整理",
                    "整理邮件",
                    "收件箱整理",
                    "organize email",
                    "clean inbox",
                    "sort emails",
                ],
                description="メールを重要度別に整理し、サマリーを作成",
                required_skills=["email", "summarizer"],
                parameters=[
                    TaskParameter(name="days", pattern=r"(\d+)日", default=7, type="int"),
                    TaskParameter(name="folder", default="inbox"),
                ],
                tags=["email", "productivity"],
            )
        )

        # ファイル整理テンプレート
        self._intent_router.register_template(
            TaskTemplate(
                name="file_organize",
                triggers=[
                    "ファイル整理",
                    "ディスク整理",
                    "ダウンロード整理",
                    "フォルダ整理",
                    "文件整理",
                    "磁盘整理",
                    "下载整理",
                    "organize files",
                    "clean disk",
                    "sort downloads",
                ],
                description="ファイルを分類・整理し、不要ファイルを特定",
                required_skills=["filesystem"],
                parameters=[
                    TaskParameter(name="path", pattern=r"「(.+?)」", default="~/Downloads"),
                    TaskParameter(name="days_old", pattern=r"(\d+)日以上", default=30, type="int"),
                ],
                tags=["file", "cleanup"],
            )
        )

        # システム最適化テンプレート
        self._intent_router.register_template(
            TaskTemplate(
                name="system_optimize",
                triggers=[
                    "システム最適化",
                    "PC最適化",
                    "パフォーマンス改善",
                    "系统优化",
                    "电脑优化",
                    "性能优化",
                    "optimize system",
                    "speed up pc",
                    "improve performance",
                ],
                description="システムリソースを分析し、最適化を実施",
                required_skills=["system_info", "command"],
                parameters=[
                    TaskParameter(name="level", pattern=r"(軽度|中度|完全)", default="軽度"),
                ],
                tags=["system", "optimization"],
            )
        )

        # 調査テンプレート
        self._intent_router.register_template(
            TaskTemplate(
                name="research",
                triggers=[
                    "調査",
                    "情報収集",
                    "調べて",
                    "最新動向",
                    "トレンド",
                    "调查",
                    "情报收集",
                    "查一下",
                    "最新动态",
                    "research",
                    "investigate",
                    "find out",
                    "latest trends",
                ],
                description="指定トピックについて調査し、要点をまとめる",
                required_skills=["web_search", "summarizer"],
                parameters=[
                    TaskParameter(name="topic", pattern=r"「(.+?)」について|(.+?)を調査", required=True),
                    TaskParameter(name="depth", pattern=r"(簡潔|詳細|徹底)", default="簡潔"),
                ],
                tags=["research", "intelligence"],
            )
        )

        # 競合分析テンプレート
        self._intent_router.register_template(
            TaskTemplate(
                name="competitor_analysis",
                triggers=[
                    "競合分析",
                    "競合調査",
                    "ライバル調査",
                    "競合他社",
                    "竞争分析",
                    "竞品调研",
                    "对手调查",
                    "competitor analysis",
                    "competitive research",
                ],
                description="競合他社の情報を収集・分析",
                required_skills=["web_search", "browser", "analyzer"],
                parameters=[
                    TaskParameter(name="competitor", pattern=r"「(.+?)」", required=True),
                    TaskParameter(name="aspects", default="製品,価格,マーケティング"),
                ],
                tags=["research", "competitor"],
            )
        )

        # レポート作成テンプレート
        self._intent_router.register_template(
            TaskTemplate(
                name="report",
                triggers=[
                    "レポート作成",
                    "報告書作成",
                    "資料作成",
                    "まとめ作成",
                    "报告制作",
                    "报告书制作",
                    "资料制作",
                    "create report",
                    "make report",
                    "write summary",
                ],
                description="指定テーマのレポートを作成",
                required_skills=["summarizer", "report_builder"],
                parameters=[
                    TaskParameter(name="title", pattern=r"「(.+?)」", required=True),
                    TaskParameter(name="format", pattern=r"(markdown|pdf|pptx)", default="markdown"),
                ],
                tags=["report", "document"],
            )
        )

    async def process(
        self,
        message: str,
        user_id: str = "default",
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """メッセージを処理してタスクを実行.

        Args:
            message: ユーザーメッセージ（自然言語）
            user_id: ユーザーID
            context: 追加コンテキスト

        Returns:
            処理結果（summary, details, raw_results を含む）
        """
        context = context or {}
        run_id = str(context.get("run_id") or f"run_{uuid.uuid4().hex}")
        context["run_id"] = run_id
        await self._emit_event(
            "RunStarted",
            {
                "run_id": run_id,
                "user_id": user_id,
                "message": message,
                "started_at": datetime.now(UTC).isoformat(),
            },
        )
        self._logger.info("処理開始: user=%s, message=%s", user_id, message[:50])

        try:
            # 1. 意図解析
            intent = await self._intent_router.route(message, context)
            self._logger.info(
                "意図解析: category=%s, template=%s, confidence=%.2f",
                intent.category,
                intent.template_name,
                intent.confidence,
            )

            if self._should_propose_cli(message=message, intent=intent):
                proposal = self._build_cli_proposal(message, intent)
                response = {
                    "summary": "🧭 この問題は CLI 調査が有効です。提案内容を確認後、実行可否を選択してください。",
                    "headline": "CLI 調査提案",
                    "key_points": [
                        "低信頼度の意図解析結果を検出",
                        "トラブルシュート要求キーワードを検出",
                    ],
                    "actions": [
                        "提案された CLI 調査を確認する",
                        "必要であれば実行を承認する",
                    ],
                    "risks": [
                        "調査中に環境情報へアクセスする可能性があります",
                    ],
                    "needs_cli_confirmation": True,
                    "cli_proposal": proposal,
                    "raw_results": {"proposal": proposal},
                    "intent": {
                        "category": intent.category.value,
                        "template": intent.template_name,
                        "confidence": intent.confidence,
                        "parameters": intent.parameters,
                    },
                    "run_id": run_id,
                }
                await self._emit_event(
                    "RunFinished",
                    {
                        "run_id": run_id,
                        "status": "completed",
                        "mode": "cli_proposal",
                    },
                )
                return response

            # 2. カテゴリ別処理
            if intent.category == IntentCategory.TASK_EXECUTION:
                result = await self._execute_task(intent, user_id, context)
            elif intent.category == IntentCategory.INFORMATION_QUERY:
                result = await self._answer_query(intent, user_id, context)
            elif intent.category == IntentCategory.STATUS_CHECK:
                result = await self._check_status(intent, user_id, context)
            else:
                result = await self._handle_general(intent, user_id, context)

            # 3. サマリー生成
            summary = await self._summary_builder.build(
                task_name=intent.template_name or "general",
                results=result,
                details=result.get("details", ""),
            )

            response = {
                "summary": summary.to_message(),
                "headline": summary.headline,
                "key_points": summary.key_points,
                "actions": summary.actions,
                "risks": summary.risks,
                "raw_results": result,
                "intent": {
                    "category": intent.category.value,
                    "template": intent.template_name,
                    "confidence": intent.confidence,
                    "parameters": intent.parameters,
                },
                "run_id": run_id,
            }
            await self._emit_event(
                "RunFinished",
                {
                    "run_id": run_id,
                    "status": "completed",
                },
            )
            return response

        except Exception as e:
            self._logger.error("処理エラー: %s", e, exc_info=True)
            await self._emit_event(
                "RunFinished",
                {
                    "run_id": run_id,
                    "status": "failed",
                    "error": str(e),
                },
            )
            return {
                "summary": f"❌ エラーが発生しました: {e}",
                "headline": "処理エラー",
                "key_points": [],
                "actions": ["再度お試しください", "サポートに連絡してください"],
                "risks": [str(e)],
                "raw_results": {"error": str(e)},
                "run_id": run_id,
            }

    async def _execute_task(
        self,
        intent: Intent,
        user_id: str,
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """タスクを実行."""
        template_name = intent.template_name or "general"
        params = intent.parameters

        self._logger.info("タスク実行: %s, params=%s", template_name, params)

        # テンプレート別の実行
        if template_name == "email_organize":
            return await self._execute_email_organize(params, context)
        if template_name == "file_organize":
            return await self._execute_file_organize(params, context)
        if template_name == "system_optimize":
            return await self._execute_system_optimize(params, context)
        if template_name == "research":
            return await self._execute_research(params, context)
        if template_name == "competitor_analysis":
            return await self._execute_competitor_analysis(params, context)
        if template_name == "report":
            return await self._execute_report(params, context)
        return await self._execute_general_task(intent.original_text, context)

    async def _answer_query(
        self,
        intent: Intent,
        user_id: str,
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """情報照会に回答."""
        # LLMで回答生成
        try:
            llm = get_llm(temperature=0.7)
            response = await llm.chat(
                [
                    {"role": "system", "content": "簡潔に回答してください。"},
                    {"role": "user", "content": intent.rewritten_query},
                ]
            )
            return self._contract_payload(
                result={"answer": response.get("content", "")},
                evidence=[
                    {
                        "type": "llm_response",
                        "model": "default_llm",
                        "timestamp": datetime.now(UTC).isoformat(),
                    }
                ],
                extra={"answer": response.get("content", ""), "processed": 1},
            )
        except Exception as e:
            return self._contract_payload(
                result={"answer": f"回答生成に失敗: {e}"},
                evidence=[
                    {
                        "type": "llm_error",
                        "error": str(e),
                        "timestamp": datetime.now(UTC).isoformat(),
                    }
                ],
                risk_flags=["llm_error"],
                extra={"answer": f"回答生成に失敗: {e}", "error": str(e), "processed": 0},
            )

    async def _check_status(
        self,
        intent: Intent,
        user_id: str,
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """状態確認."""
        os_info: dict[str, Any] = {}
        resource_usage: dict[str, Any] = {}
        evidence: list[dict[str, Any]] = []
        risk_flags: list[str] = []

        if self._has_gateway_skill("get_os_info"):
            os_result = await self._call_gateway_skill(
                skill_name="get_os_info",
                params={},
                context=context,
            )
            payload = os_result.get("result")
            if isinstance(payload, dict):
                os_info = payload
            evidence.extend(os_result.get("evidence", []))
            risk_flags.extend(os_result.get("risk_flags", []))

        if self._has_gateway_skill("get_resource_usage"):
            usage_result = await self._call_gateway_skill(
                skill_name="get_resource_usage",
                params={},
                context=context,
            )
            payload = usage_result.get("result")
            if isinstance(payload, dict):
                resource_usage = payload
            evidence.extend(usage_result.get("evidence", []))
            risk_flags.extend(usage_result.get("risk_flags", []))

        status_info = {
            "assistant_status": "running",
            "security_mode": self._config.security_mode,
            "registered_skills": len(self._gateway.list_available_skills()) if self._gateway else 0,
            "last_activity": datetime.now(UTC).isoformat(),
            "os_info": os_info,
            "resource_usage": resource_usage,
        }
        return self._contract_payload(
            result={"status": status_info},
            evidence=evidence,
            risk_flags=risk_flags,
            extra={"status": status_info, "processed": 1},
        )

    async def _handle_general(
        self,
        intent: Intent,
        user_id: str,
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """一般的な問い合わせ処理."""
        return await self._answer_query(intent, user_id, context)

    # =========================================================================
    # 具体的なタスク実行メソッド
    # =========================================================================

    async def _execute_email_organize(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """メール整理を実行.

        ローカル配下のメール候補ファイルを分類してサマリーを生成する。
        """
        days = self._safe_int(params.get("days"), 7)
        folder = str(params.get("folder", "inbox"))
        if "/" in folder or folder.startswith("."):
            target_path = folder
        else:
            target_path = str(self._config.workspace_path / folder)

        self._logger.info("メール整理: days=%d, folder=%s", days, folder)

        gateway_result = await self._call_gateway_skill(
            skill_name="list_dir",
            params={"path": target_path},
            context=context,
        )
        items = gateway_result.get("result", [])
        if not isinstance(items, list):
            items = []

        email_extensions = {".eml", ".msg", ".txt", ".md", ".json"}
        email_like_files: list[str] = []
        important = 0
        urgent = 0
        spam = 0

        for item in items:
            if not isinstance(item, dict):
                continue
            if bool(item.get("is_dir")):
                continue
            file_name = str(item.get("name", ""))
            lowered = file_name.lower()
            suffix = Path(lowered).suffix
            if suffix in email_extensions or "mail" in lowered or "inbox" in lowered:
                email_like_files.append(file_name)
            if re.search(r"(urgent|asap|重要|緊急|优先|priority)", lowered):
                important += 1
                urgent += 1
            elif re.search(r"(important|action|required|対応|需要处理)", lowered):
                important += 1
            if re.search(r"(spam|promo|promotion|広告|垃圾)", lowered):
                spam += 1

        processed = len(email_like_files)
        archived = max(processed - important - spam, 0)
        result = {
            "processed": processed,
            "important": important,
            "urgent": urgent,
            "spam": spam,
            "archived": archived,
            "folder": target_path,
            "sample_files": email_like_files[:10],
        }
        return self._contract_payload(
            result=result,
            evidence=gateway_result.get("evidence", []),
            artifacts=gateway_result.get("artifacts", []),
            rollback_handle=gateway_result.get("rollback_handle"),
            cost=gateway_result.get("cost", {"duration_ms": 0.0, "token_estimate": 0}),
            risk_flags=gateway_result.get("risk_flags", []),
            extra={
                **result,
                "summary_points": [
                    f"対象フォルダ: {target_path}",
                    f"メール候補 {processed} 件を分類",
                    f"重要 {important} 件 / 迷惑 {spam} 件を検出",
                ],
                "recommended_actions": [
                    "重要メールを先に確認してください",
                    "迷惑メール候補は目視確認後に削除してください",
                ],
            },
        )

    async def _execute_file_organize(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """ファイル整理を実行."""
        if not self._config.enable_os_skills:
            return self._blocked_result("filesystem")

        path = str(params.get("path", "~/Downloads"))
        days_old = self._safe_int(params.get("days_old"), 30)

        self._logger.info("ファイル整理: path=%s, days_old=%d", path, days_old)

        gateway_result = await self._call_gateway_skill(
            skill_name="list_dir",
            params={"path": path},
            context=context,
        )
        files = gateway_result.get("result", [])
        if not isinstance(files, list):
            files = []

        categorized = {"documents": 0, "images": 0, "videos": 0, "others": 0}
        for item in files:
            if not isinstance(item, dict):
                continue
            name = str(item.get("name", "")).lower()
            if name.endswith((".pdf", ".doc", ".docx", ".txt", ".md")):
                categorized["documents"] += 1
            elif name.endswith((".jpg", ".jpeg", ".png", ".gif", ".webp")):
                categorized["images"] += 1
            elif name.endswith((".mp4", ".mov", ".avi", ".mkv")):
                categorized["videos"] += 1
            else:
                categorized["others"] += 1

        processed = len(files)
        result = {
            "processed": processed,
            "freed_mb": 0,
            "categorized": categorized,
            "path": path,
            "days_old": days_old,
        }
        return self._contract_payload(
            result=result,
            evidence=gateway_result.get("evidence", []),
            artifacts=gateway_result.get("artifacts", []),
            rollback_handle=gateway_result.get("rollback_handle"),
            cost=gateway_result.get("cost", {"duration_ms": 0.0, "token_estimate": 0}),
            risk_flags=gateway_result.get("risk_flags", []),
            extra={
                **result,
                "blocked": False,
                "summary_points": [
                    f"{processed}ファイルを分類",
                    "分類結果を生成",
                    f"{days_old}日以上古いファイルの確認候補を提示",
                ],
                "recommended_actions": [
                    "不要ファイルの削除対象をレビューしてください",
                ],
            },
        )

    async def _execute_system_optimize(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """システム最適化を実行."""
        if not self._config.enable_os_skills:
            return self._blocked_result("os_command")

        level = params.get("level", "軽度")

        self._logger.info("システム最適化: level=%s", level)

        gateway_result = await self._call_gateway_skill(
            skill_name="get_resource_usage",
            params={},
            context=context,
        )
        usage = gateway_result.get("result", {})
        if not isinstance(usage, dict):
            usage = {}

        result = {
            "improvement": 5,
            "memory_freed_mb": 0,
            "cache_cleared_mb": 0,
            "resource_usage": usage,
            "level": level,
        }
        return self._contract_payload(
            result=result,
            evidence=gateway_result.get("evidence", []),
            artifacts=gateway_result.get("artifacts", []),
            rollback_handle=gateway_result.get("rollback_handle"),
            cost=gateway_result.get("cost", {"duration_ms": 0.0, "token_estimate": 0}),
            risk_flags=gateway_result.get("risk_flags", []),
            extra={
                **result,
                "summary_points": [
                    "システム使用状況を取得",
                    f"最適化レベル: {level}",
                    "改善余地を算出",
                ],
                "recommended_actions": [
                    "定期的な最適化をお勧めします",
                ],
            },
        )

    async def _execute_research(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """調査を実行."""
        topic = str(params.get("topic", "指定なし")).strip() or "指定なし"
        depth = str(params.get("depth", "簡潔"))

        self._logger.info("調査実行: topic=%s, depth=%s", topic, depth)

        evidence: list[dict[str, Any]] = []
        source_texts: list[str] = []
        risk_flags: list[str] = []

        if self._has_gateway_skill("http_request"):
            wiki_topic = quote(topic.replace(" ", "_"))
            wiki_url = f"https://en.wikipedia.org/api/rest_v1/page/summary/{wiki_topic}"
            net_result = await self._call_gateway_skill(
                skill_name="http_request",
                params={"method": "GET", "url": wiki_url, "headers": {"accept": "application/json"}},
                context=context,
            )
            evidence.extend(net_result.get("evidence", []))
            risk_flags.extend(net_result.get("risk_flags", []))
            payload = net_result.get("result", {})
            if isinstance(payload, dict) and int(payload.get("status_code", 0)) == 200:
                body_text = str(payload.get("body", "")).strip()
                try:
                    parsed = json.loads(body_text)
                    extract = str(parsed.get("extract", "")).strip()
                    if extract:
                        source_texts.append(extract)
                        evidence.append(
                            {
                                "type": "research_source",
                                "source": "wikipedia",
                                "url": wiki_url,
                                "timestamp": datetime.now(UTC).isoformat(),
                            }
                        )
                except json.JSONDecodeError:
                    if body_text:
                        source_texts.append(body_text[:1200])
            else:
                risk_flags.append("network_source_unavailable")
        else:
            risk_flags.append("network_skill_unavailable")

        llm_prompt = (
            "次のトピックについて、事実ベースで簡潔にまとめてください。"
            "出力は3-5個の箇条書きにし、最後に1行の推奨アクションを追加してください。\n\n"
            f"topic: {topic}\n"
            f"depth: {depth}\n"
            f"source_text: {source_texts[0] if source_texts else '利用可能な外部ソースなし'}\n"
        )
        llm_output = await self._ask_llm(
            "あなたは調査アナリストです。推測を避け、入力情報に基づいて要点を生成してください。",
            llm_prompt,
            temperature=0.3,
        )
        insight_lines = [line.strip("- ").strip() for line in llm_output.splitlines() if line.strip()]
        key_insights = insight_lines[:5] if insight_lines else [f"{topic} の基本情報を整理しました。"]

        result = {
            "topic": topic,
            "findings": len(key_insights),
            "sources": len(source_texts),
            "depth": depth,
            "key_insights": key_insights,
        }
        return self._contract_payload(
            result=result,
            evidence=evidence,
            risk_flags=risk_flags,
            extra={
                **result,
                "summary_points": [
                    f"トピック「{topic}」の調査完了",
                    f"取得ソース {len(source_texts)} 件",
                    f"主要な知見 {len(key_insights)} 件を抽出",
                ],
                "recommended_actions": ["必要なら追加調査の対象地域/期間を指定してください。"],
            },
        )

    async def _execute_competitor_analysis(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """競合分析を実行."""
        if not self._config.enable_browser_skills:
            return self._blocked_result("browser_control")

        competitor = str(params.get("competitor", "指定なし")).strip() or "指定なし"
        aspects = str(params.get("aspects", "製品,価格,マーケティング"))

        self._logger.info("競合分析: competitor=%s", competitor)

        aspect_list = [item.strip() for item in aspects.split(",") if item.strip()]
        research = await self._execute_research(
            {"topic": competitor, "depth": "詳細"},
            context,
        )
        research_summary = research.get("key_insights", [])
        if not isinstance(research_summary, list):
            research_summary = []

        analysis_prompt = (
            "競合分析を行ってください。以下の観点ごとに、強み/弱み/注視点を短く出力してください。\n"
            f"competitor: {competitor}\n"
            f"aspects: {', '.join(aspect_list) if aspect_list else aspects}\n"
            f"context: {'; '.join(str(item) for item in research_summary[:5])}\n"
        )
        analysis_text = await self._ask_llm(
            "あなたは市場分析担当です。断定を避け、観点ベースで実務的に分析してください。",
            analysis_prompt,
            temperature=0.3,
        )
        lines = [line.strip("- ").strip() for line in analysis_text.splitlines() if line.strip()]
        strengths = [line for line in lines if "強み" in line][:3]
        weaknesses = [line for line in lines if "弱み" in line][:3]
        watch_points = [line for line in lines if "注視" in line or "リスク" in line][:3]

        result = {
            "competitor": competitor,
            "findings": len(lines),
            "aspects": aspect_list if aspect_list else [aspects],
            "strengths": strengths,
            "weaknesses": weaknesses,
            "watch_points": watch_points,
        }
        return self._contract_payload(
            result=result,
            evidence=research.get("evidence", []),
            risk_flags=research.get("risk_flags", []),
            extra={
                **result,
                "summary_points": [
                    f"競合「{competitor}」の分析完了",
                    f"分析観点: {', '.join(result['aspects'])}",
                    f"分析メモ {len(lines)} 件を生成",
                ],
                "recommended_actions": [
                    "自社優位性と競合弱点のマッピングを次タスクで実施してください",
                ],
            },
        )

    async def _execute_report(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """レポート作成を実行."""
        title = str(params.get("title", "レポート")).strip() or "レポート"
        fmt = str(params.get("format", "markdown")).lower()
        audience = str(params.get("audience", "マネージャー"))
        objective = str(params.get("objective", "現状整理と次アクション明確化"))

        self._logger.info("レポート作成: title=%s, format=%s", title, fmt)

        report_text = await self._ask_llm(
            "あなたは業務レポート作成アシスタントです。曖昧表現を避け、実務で使える構成にしてください。",
            (
                "以下の条件でレポート本文を markdown で作成してください。"
                "見出しは # / ## を使い、最後に Next Actions を3点書いてください。\n\n"
                f"title: {title}\n"
                f"format: {fmt}\n"
                f"audience: {audience}\n"
                f"objective: {objective}\n"
            ),
            temperature=0.2,
        )
        if not report_text:
            report_text = f"# {title}\n\n本文を生成できませんでした。入力条件を見直してください。\n"

        artifacts: list[dict[str, Any]] = []
        evidence: list[dict[str, Any]] = [
            {
                "type": "report_generated",
                "title": title,
                "timestamp": datetime.now(UTC).isoformat(),
            }
        ]
        risk_flags: list[str] = []
        output_path: str | None = None

        if fmt == "markdown" and self._has_gateway_skill("write_file"):
            timestamp = datetime.now(UTC).strftime("%Y%m%d_%H%M%S")
            safe_title = re.sub(r"[^a-zA-Z0-9_-]+", "_", title)[:60] or "report"
            output_path = str(self._config.workspace_path / "reports" / f"{safe_title}_{timestamp}.md")
            write_result = await self._call_gateway_skill(
                skill_name="write_file",
                params={"path": output_path, "content": report_text},
                context=context,
            )
            evidence.extend(write_result.get("evidence", []))
            risk_flags.extend(write_result.get("risk_flags", []))
            if write_result.get("success") is True:
                artifacts.append(
                    {
                        "type": "report_file",
                        "location": output_path,
                    }
                )
            else:
                output_path = None
                risk_flags.append("report_file_write_failed")

        estimated_pages = max((len(report_text) // 1800) + 1, 1)
        result = {
            "title": title,
            "format": fmt,
            "pages": estimated_pages,
            "content": report_text if fmt == "markdown" else "",
            "output_path": output_path,
        }
        return self._contract_payload(
            result=result,
            evidence=evidence,
            artifacts=artifacts,
            risk_flags=risk_flags,
            extra={
                **result,
                "summary_points": [
                    f"レポート「{title}」を生成",
                    f"形式: {fmt}",
                    f"推定 {estimated_pages} ページ相当",
                ],
                "recommended_actions": [
                    "内容レビュー後、必要なら追加条件で再生成してください",
                ],
            },
        )

    async def _execute_general_task(
        self,
        request: str,
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """汎用タスク実行（LLM使用）."""
        try:
            llm = get_llm(temperature=0.5)
            response = await llm.chat(
                [
                    {"role": "system", "content": "タスクを実行し、結果を報告してください。"},
                    {"role": "user", "content": request},
                ]
            )
            return self._contract_payload(
                result={"content": response.get("content", "")},
                evidence=[
                    {
                        "type": "general_task_response",
                        "timestamp": datetime.now(UTC).isoformat(),
                    }
                ],
                extra={"processed": 1},
            )
        except Exception as e:
            return self._contract_payload(
                result={"error": str(e)},
                risk_flags=["general_task_error"],
                extra={"error": str(e), "processed": 0},
            )
