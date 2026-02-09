"""Executive Summary Builder - 主管向け簡潔サマリー生成.

複雑なAgent出力を主管が理解しやすい形式に変換する。
技術詳細を隠蔽し、行動可能な情報を提示。

設計原則:
- 簡潔性: 3-5行の要点
- 行動指向: 次に何をすべきか明確
- リスク強調: 問題点を先に
- 松耦合: LLMプロバイダーを意識しない

使用例:
    >>> builder = ExecutiveSummaryBuilder()
    >>> summary = await builder.build(
    ...     task_name="メール整理",
    ...     results={"processed": 50, "important": 5, "spam": 10},
    ...     details=long_report_text,
    ... )
    >>> print(summary.headline)  # "✅ 50件のメールを処理、5件が要対応"
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

from agentflow.providers import get_llm


_logger = logging.getLogger(__name__)


@dataclass
class SummaryConfig:
    """サマリー設定.

    Attributes:
        max_lines: 最大行数
        include_actions: アクション項目を含むか
        include_risks: リスク項目を含むか
        language: 出力言語
        use_emoji: 絵文字を使用するか
    """

    max_lines: int = 5
    include_actions: bool = True
    include_risks: bool = True
    language: str = "ja"
    use_emoji: bool = True


@dataclass
class ExecutiveSummary:
    """主管向けサマリー.

    Attributes:
        headline: 一行見出し（最重要）
        key_points: 重要ポイント（3-5項目）
        actions: 推奨アクション
        risks: リスク・注意点
        details_available: 詳細が利用可能か
        raw_results: 元の結果データ
    """

    headline: str = ""
    key_points: list[str] = field(default_factory=list)
    actions: list[str] = field(default_factory=list)
    risks: list[str] = field(default_factory=list)
    details_available: bool = False
    raw_results: dict[str, Any] = field(default_factory=dict)

    def to_message(self, config: SummaryConfig | None = None) -> str:
        """メッセージ形式に変換."""
        config = config or SummaryConfig()
        lines: list[str] = []

        # 見出し
        lines.append(self.headline)
        lines.append("")

        # 重要ポイント
        if self.key_points:
            for point in self.key_points[: config.max_lines - 2]:
                prefix = "• " if not config.use_emoji else "📌 "
                lines.append(f"{prefix}{point}")
            lines.append("")

        # リスク
        if config.include_risks and self.risks:
            prefix = "⚠️ " if config.use_emoji else "[注意] "
            for risk in self.risks[:2]:
                lines.append(f"{prefix}{risk}")
            lines.append("")

        # アクション
        if config.include_actions and self.actions:
            prefix = "👉 " if config.use_emoji else "[次のステップ] "
            for action in self.actions[:2]:
                lines.append(f"{prefix}{action}")

        return "\n".join(lines).strip()


class ExecutiveSummaryBuilder:
    """主管向けサマリービルダー."""

    # タスク別テンプレート
    _TEMPLATES = {
        "email_organize": {
            "headline": "{emoji} {processed}件のメールを処理、{important}件が要対応",
            "emoji_success": "✅",
            "emoji_warning": "⚠️",
        },
        "file_organize": {
            "headline": "{emoji} {processed}ファイルを整理、{freed_mb}MB解放",
            "emoji_success": "🗂️",
            "emoji_warning": "⚠️",
        },
        "system_optimize": {
            "headline": "{emoji} システム最適化完了、パフォーマンス{improvement}%向上",
            "emoji_success": "🚀",
            "emoji_warning": "⚠️",
        },
        "research": {
            "headline": "{emoji} {topic}の調査完了、{findings}件の知見",
            "emoji_success": "🔍",
            "emoji_warning": "⚠️",
        },
        "report": {
            "headline": "{emoji} レポート作成完了: {title}",
            "emoji_success": "📊",
            "emoji_warning": "⚠️",
        },
        "default": {
            "headline": "{emoji} タスク完了: {task_name}",
            "emoji_success": "✅",
            "emoji_warning": "⚠️",
        },
    }

    def __init__(self, config: SummaryConfig | None = None) -> None:
        """初期化."""
        self._config = config or SummaryConfig()
        self._logger = logging.getLogger(__name__)

    async def build(
        self,
        task_name: str,
        results: dict[str, Any],
        details: str = "",
        *,
        use_llm: bool = False,
    ) -> ExecutiveSummary:
        """サマリーを構築.

        Args:
            task_name: タスク名（テンプレート選択用）
            results: タスク結果（数値データ等）
            details: 詳細テキスト（オプション）
            use_llm: LLMで要約するか

        Returns:
            ExecutiveSummary
        """
        # テンプレート選択
        template = self._TEMPLATES.get(task_name, self._TEMPLATES["default"])

        # 成功/警告判定
        has_warning = results.get("has_warning", False) or results.get("errors", [])
        emoji = template["emoji_warning"] if has_warning else template["emoji_success"]

        # 見出し生成
        headline_template = template["headline"]
        headline = headline_template.format(
            emoji=emoji if self._config.use_emoji else "",
            task_name=task_name,
            **{k: v for k, v in results.items() if isinstance(v, (str, int, float))},
        )

        # 重要ポイント抽出
        key_points = self._extract_key_points(results)

        # リスク抽出
        risks = self._extract_risks(results)

        # アクション抽出
        actions = self._extract_actions(results, task_name)

        # LLMで詳細を要約（オプション）
        if use_llm and details:
            llm_summary = await self._summarize_with_llm(details)
            if llm_summary:
                key_points.extend(llm_summary)

        return ExecutiveSummary(
            headline=headline.strip(),
            key_points=key_points[: self._config.max_lines],
            actions=actions,
            risks=risks,
            details_available=bool(details),
            raw_results=results,
        )

    def _extract_key_points(self, results: dict[str, Any]) -> list[str]:
        """重要ポイントを抽出."""
        points: list[str] = []

        # 数値結果を整形
        key_metrics = ["processed", "important", "urgent", "completed", "failed"]
        for key in key_metrics:
            if key in results:
                label = self._translate_key(key)
                points.append(f"{label}: {results[key]}")

        # カスタムポイント
        if "summary_points" in results:
            points.extend(results["summary_points"])

        return points

    def _extract_risks(self, results: dict[str, Any]) -> list[str]:
        """リスクを抽出."""
        risks: list[str] = []

        if results.get("errors"):
            risks.append(f"エラー {len(results['errors'])}件発生")

        if results.get("warnings"):
            for w in results["warnings"][:2]:
                risks.append(str(w))

        if results.get("failed", 0) > 0:
            risks.append(f"{results['failed']}件の処理に失敗")

        return risks

    def _extract_actions(self, results: dict[str, Any], task_name: str) -> list[str]:
        """推奨アクションを抽出."""
        actions: list[str] = []

        # 結果に含まれるアクション
        if "recommended_actions" in results:
            actions.extend(results["recommended_actions"])

        # タスク別デフォルトアクション
        if results.get("important", 0) > 0:
            actions.append(f"重要な{results['important']}件を確認してください")

        if results.get("needs_review"):
            actions.append("レビューが必要な項目があります")

        return actions

    def _translate_key(self, key: str) -> str:
        """キー名を日本語に."""
        translations = {
            "processed": "処理済み",
            "important": "重要",
            "urgent": "緊急",
            "completed": "完了",
            "failed": "失敗",
            "total": "合計",
            "freed_mb": "解放容量(MB)",
        }
        return translations.get(key, key)

    async def _summarize_with_llm(self, details: str) -> list[str]:
        """LLMで詳細を要約."""
        try:
            llm = get_llm(temperature=0.3)
            prompt = f"""以下の内容を、主管向けに3つの重要ポイントにまとめてください。
箇条書きで、各項目は20文字以内で。

{details[:2000]}"""

            response = await llm.chat([{"role": "user", "content": prompt}])
            content = response.get("content", "")

            # 箇条書きをパース
            points = []
            for line in content.split("\n"):
                line = line.strip().lstrip("•-・").strip()
                if line and len(line) < 50:
                    points.append(line)
            return points[:3]
        except Exception as e:
            self._logger.warning("LLM要約失敗: %s", e)
            return []

