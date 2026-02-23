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

import logging
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import TYPE_CHECKING, Any

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
    from agentflow.skills.gateway import SkillGateway

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

    workspace_path: Path = field(default_factory=lambda: Path.cwd())
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
    ) -> None:
        """初期化.

        Args:
            config: アシスタント設定
            skill_gateway: スキルゲートウェイ（OS/Browser操作用）
        """
        self._config = config or AssistantConfig()
        self._gateway = skill_gateway
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
        return {
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
        }

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
                return {
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
                }

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

            return {
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
            }

        except Exception as e:
            self._logger.error("処理エラー: %s", e, exc_info=True)
            return {
                "summary": f"❌ エラーが発生しました: {e}",
                "headline": "処理エラー",
                "key_points": [],
                "actions": ["再度お試しください", "サポートに連絡してください"],
                "risks": [str(e)],
                "raw_results": {"error": str(e)},
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
            return {
                "answer": response.get("content", ""),
                "processed": 1,
            }
        except Exception as e:
            return {"answer": f"回答生成に失敗: {e}", "error": str(e)}

    async def _check_status(
        self,
        intent: Intent,
        user_id: str,
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """状態確認."""
        # 非同期操作のプレースホルダー（将来の拡張用）
        import asyncio

        await asyncio.sleep(0)  # 非同期コンテキスト維持
        status_info = {
            "assistant_status": "running",
            "pending_tasks": 0,
            "last_activity": "now",
        }
        return {"status": status_info, "processed": 1}

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

        Note:
            実際のメール操作はEmailAgentが担当。
            ここではモック結果を返す（実装時に置き換え）。
        """
        import asyncio

        await asyncio.sleep(0)  # 非同期コンテキスト維持

        days = params.get("days", 7)
        folder = params.get("folder", "inbox")

        self._logger.info("メール整理: days=%d, folder=%s", days, folder)

        # モック結果（実装時にEmailAgent連携に置き換え）
        return {
            "processed": 50,
            "important": 5,
            "urgent": 2,
            "spam": 10,
            "archived": 33,
            "summary_points": [
                f"過去{days}日間のメールを処理",
                "重要メール5件を上位に移動",
                "スパム10件を削除",
            ],
            "recommended_actions": [
                "重要メール5件を確認してください",
                "緊急メール2件に返信が必要です",
            ],
        }

    async def _execute_file_organize(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """ファイル整理を実行."""
        if not self._config.enable_os_skills:
            return self._blocked_result("filesystem")

        import asyncio

        await asyncio.sleep(0)  # 非同期コンテキスト維持

        path = params.get("path", "~/Downloads")
        days_old = params.get("days_old", 30)

        self._logger.info("ファイル整理: path=%s, days_old=%d", path, days_old)

        # SkillGateway経由でFileSystemSkillを呼び出す（実装時）
        if self._gateway:
            _ = await self._gateway.call("list_dir", {"path": path})

        # モック結果
        return {
            "processed": 120,
            "freed_mb": 1500,
            "categorized": {"documents": 30, "images": 50, "videos": 20, "others": 20},
            "summary_points": [
                "120ファイルを分類",
                "1.5GB の空き容量を確保",
                f"{days_old}日以上古いファイル20件を特定",
            ],
            "recommended_actions": [
                "古いファイル20件の削除を検討してください",
            ],
        }

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

        # SkillGateway経由でSystemInfoSkill/CommandSkillを呼び出す（実装時）
        if self._gateway:
            # 実際の実装
            _ = await self._gateway.call("get_resource_usage", {})

        # モック結果
        return {
            "improvement": 15,
            "memory_freed_mb": 500,
            "cache_cleared_mb": 200,
            "summary_points": [
                "不要プロセス5件を終了",
                "キャッシュ200MBをクリア",
                f"最適化レベル: {level}",
            ],
            "recommended_actions": [
                "定期的な最適化をお勧めします",
            ],
        }

    async def _execute_research(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """調査を実行."""
        import asyncio

        await asyncio.sleep(0)  # 非同期コンテキスト維持

        topic = params.get("topic", "指定なし")
        depth = params.get("depth", "簡潔")

        self._logger.info("調査実行: topic=%s, depth=%s", topic, depth)

        # モック結果
        return {
            "topic": topic,
            "findings": 5,
            "sources": 10,
            "summary_points": [
                f"トピック「{topic}」の調査完了",
                f"信頼性の高い情報源{10}件を参照",
                f"主要な知見{5}件を抽出",
            ],
            "key_insights": [
                "市場は前年比15%成長",
                "主要プレイヤー3社が台頭",
                "技術トレンドはAI活用が中心",
            ],
        }

    async def _execute_competitor_analysis(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """競合分析を実行."""
        if not self._config.enable_browser_skills:
            return self._blocked_result("browser_control")

        import asyncio

        await asyncio.sleep(0)  # 非同期コンテキスト維持

        competitor = params.get("competitor", "指定なし")
        aspects = params.get("aspects", "製品,価格,マーケティング")

        self._logger.info("競合分析: competitor=%s", competitor)

        # モック結果
        return {
            "competitor": competitor,
            "findings": 8,
            "summary_points": [
                f"競合「{competitor}」の分析完了",
                f"分析観点: {aspects}",
                "強み・弱みを特定",
            ],
            "strengths": ["ブランド認知度", "価格競争力"],
            "weaknesses": ["カスタマーサポート", "製品ラインナップ"],
        }

    async def _execute_report(
        self,
        params: dict[str, Any],
        context: dict[str, Any],
    ) -> dict[str, Any]:
        """レポート作成を実行."""
        import asyncio

        await asyncio.sleep(0)  # 非同期コンテキスト維持

        title = params.get("title", "レポート")
        fmt = params.get("format", "markdown")

        self._logger.info("レポート作成: title=%s, format=%s", title, fmt)

        # モック結果
        return {
            "title": title,
            "format": fmt,
            "pages": 5,
            "summary_points": [
                f"レポート「{title}」を作成",
                f"形式: {fmt}",
                "5ページ構成",
            ],
            "recommended_actions": [
                "レポートを確認してください",
            ],
        }

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
            return {
                "processed": 1,
                "result": response.get("content", ""),
            }
        except Exception as e:
            return {"error": str(e), "processed": 0}
