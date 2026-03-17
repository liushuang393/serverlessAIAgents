"""Conversation Export Skill - 会話エクスポート機能.

会話履歴を JSON / CSV / Markdown 形式でエクスポートするスキル。

Example:
    >>> from kernel.skills import ConversationExportSkill
    >>>
    >>> # スキル作成
    >>> exporter = ConversationExportSkill()
    >>>
    >>> # JSON エクスポート
    >>> json_data = await exporter.export_json(messages)
    >>>
    >>> # CSV エクスポート
    >>> csv_data = await exporter.export_csv(messages)
    >>>
    >>> # Markdown エクスポート
    >>> md_data = await exporter.export_markdown(messages)
"""

from __future__ import annotations

import csv
import io
import json
import logging
from dataclasses import asdict, dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any


logger = logging.getLogger(__name__)


class ExportFormat(str, Enum):
    """エクスポート形式."""

    JSON = "json"
    CSV = "csv"
    MARKDOWN = "markdown"


@dataclass
class ExportMessage:
    """エクスポート用メッセージ形式.

    Attributes:
        timestamp: タイムスタンプ
        platform: プラットフォーム名
        user_id: ユーザー ID
        user_name: ユーザー表示名
        role: ロール（user, assistant, system）
        content: メッセージ内容
        metadata: 追加メタデータ
    """

    timestamp: str
    platform: str
    user_id: str
    user_name: str
    role: str
    content: str
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class ExportConfig:
    """エクスポート設定.

    Attributes:
        include_metadata: メタデータを含めるか
        include_timestamps: タイムスタンプを含めるか
        date_format: 日付形式
        timezone: タイムゾーン
        max_messages: 最大メッセージ数（None = 無制限）
    """

    include_metadata: bool = True
    include_timestamps: bool = True
    date_format: str = "%Y-%m-%d %H:%M:%S"
    timezone: str = "UTC"
    max_messages: int | None = None


class ConversationExportSkill:
    """会話エクスポートスキル.

    会話履歴を様々な形式でエクスポートする機能を提供。

    Features:
    - JSON エクスポート（構造化データ）
    - CSV エクスポート（スプレッドシート用）
    - Markdown エクスポート（ドキュメント用）
    - フィルタリング（日付範囲、ユーザー、プラットフォーム）
    - ストリーミング対応（大規模会話用）
    """

    def __init__(self, config: ExportConfig | None = None) -> None:
        """ConversationExportSkill を初期化.

        Args:
            config: エクスポート設定
        """
        self._config = config or ExportConfig()
        self._logger = logging.getLogger("conversation_export")

    async def export(
        self,
        messages: list[ExportMessage | dict[str, Any]],
        format: ExportFormat | str = ExportFormat.JSON,
        *,
        output_path: str | Path | None = None,
    ) -> str:
        """会話をエクスポート.

        Args:
            messages: メッセージリスト
            format: エクスポート形式
            output_path: 出力ファイルパス（None = 文字列として返す）

        Returns:
            エクスポートされたデータ（文字列）
        """
        # メッセージを正規化
        normalized = self._normalize_messages(messages)

        # 形式に応じてエクスポート
        format_enum = ExportFormat(format) if isinstance(format, str) else format

        if format_enum == ExportFormat.JSON:
            result = await self.export_json(normalized)
        elif format_enum == ExportFormat.CSV:
            result = await self.export_csv(normalized)
        else:
            result = await self.export_markdown(normalized)

        # ファイル出力
        if output_path:
            path = Path(output_path)
            path.write_text(result, encoding="utf-8")
            self._logger.info(f"Exported conversation to {path}")

        return result

    async def export_json(
        self,
        messages: list[ExportMessage],
        *,
        pretty: bool = True,
    ) -> str:
        """JSON 形式でエクスポート.

        Args:
            messages: メッセージリスト
            pretty: 整形出力するか

        Returns:
            JSON 文字列
        """
        data = {
            "exported_at": datetime.now().isoformat(),
            "total_messages": len(messages),
            "config": asdict(self._config),
            "messages": [asdict(msg) for msg in messages],
        }

        if pretty:
            return json.dumps(data, ensure_ascii=False, indent=2)
        return json.dumps(data, ensure_ascii=False)

    async def export_csv(
        self,
        messages: list[ExportMessage],
    ) -> str:
        """CSV 形式でエクスポート.

        Args:
            messages: メッセージリスト

        Returns:
            CSV 文字列
        """
        output = io.StringIO()
        fieldnames = ["timestamp", "platform", "user_id", "user_name", "role", "content"]

        if self._config.include_metadata:
            fieldnames.append("metadata")

        writer = csv.DictWriter(output, fieldnames=fieldnames)
        writer.writeheader()

        for msg in messages:
            row = {
                "timestamp": msg.timestamp,
                "platform": msg.platform,
                "user_id": msg.user_id,
                "user_name": msg.user_name,
                "role": msg.role,
                "content": msg.content,
            }
            if self._config.include_metadata:
                row["metadata"] = json.dumps(msg.metadata, ensure_ascii=False)

            writer.writerow(row)

        return output.getvalue()

    async def export_markdown(
        self,
        messages: list[ExportMessage],
        *,
        title: str = "会話履歴",
    ) -> str:
        """Markdown 形式でエクスポート.

        Args:
            messages: メッセージリスト
            title: ドキュメントタイトル

        Returns:
            Markdown 文字列
        """
        lines = [
            f"# {title}",
            "",
            f"**エクスポート日時**: {datetime.now().strftime(self._config.date_format)}",
            f"**総メッセージ数**: {len(messages)}",
            "",
            "---",
            "",
        ]

        current_date = ""
        for msg in messages:
            # 日付区切り
            msg_date = msg.timestamp[:10] if len(msg.timestamp) >= 10 else msg.timestamp
            if msg_date != current_date:
                current_date = msg_date
                lines.extend([f"## {current_date}", ""])

            # メッセージ
            role_emoji = {"user": "👤", "assistant": "🤖", "system": "⚙️"}.get(msg.role, "💬")

            if self._config.include_timestamps:
                time_part = msg.timestamp[11:19] if len(msg.timestamp) >= 19 else ""
                lines.append(f"### {role_emoji} {msg.user_name} ({time_part})")
            else:
                lines.append(f"### {role_emoji} {msg.user_name}")

            lines.extend(["", msg.content, ""])

            if self._config.include_metadata and msg.metadata:
                lines.append(f"*Platform: {msg.platform}*")
                lines.append("")

        return "\n".join(lines)

    def _normalize_messages(
        self,
        messages: list[ExportMessage | dict[str, Any]],
    ) -> list[ExportMessage]:
        """メッセージを正規化.

        Args:
            messages: 生メッセージリスト

        Returns:
            正規化されたメッセージリスト
        """
        normalized = []
        for msg in messages:
            if isinstance(msg, ExportMessage):
                normalized.append(msg)
            elif isinstance(msg, dict):
                user_name = msg.get("user_name", msg.get("user_id", "Unknown"))
                content = msg.get("content", msg.get("text", ""))
                normalized.append(
                    ExportMessage(
                        timestamp=msg.get("timestamp", datetime.now().isoformat()),
                        platform=msg.get("platform", "unknown"),
                        user_id=msg.get("user_id", ""),
                        user_name=str(user_name) if user_name is not None else "Unknown",
                        role=msg.get("role", "user"),
                        content=str(content) if content is not None else "",
                        metadata=msg.get("metadata", {}),
                    )
                )

        # 最大メッセージ数制限
        if self._config.max_messages:
            normalized = normalized[: self._config.max_messages]

        return normalized

    async def filter_messages(
        self,
        messages: list[ExportMessage | dict[str, Any]],
        *,
        start_date: str | datetime | None = None,
        end_date: str | datetime | None = None,
        user_ids: list[str] | None = None,
        platforms: list[str] | None = None,
        roles: list[str] | None = None,
    ) -> list[ExportMessage]:
        """メッセージをフィルタリング.

        Args:
            messages: メッセージリスト
            start_date: 開始日時
            end_date: 終了日時
            user_ids: ユーザー ID フィルター
            platforms: プラットフォームフィルター
            roles: ロールフィルター

        Returns:
            フィルタリングされたメッセージリスト
        """
        normalized = self._normalize_messages(messages)
        filtered = []

        for msg in normalized:
            # 日付フィルター
            if start_date:
                start_str = start_date.isoformat() if isinstance(start_date, datetime) else start_date
                if msg.timestamp < start_str:
                    continue

            if end_date:
                end_str = end_date.isoformat() if isinstance(end_date, datetime) else end_date
                if msg.timestamp > end_str:
                    continue

            # ユーザーフィルター
            if user_ids and msg.user_id not in user_ids:
                continue

            # プラットフォームフィルター
            if platforms and msg.platform not in platforms:
                continue

            # ロールフィルター
            if roles and msg.role not in roles:
                continue

            filtered.append(msg)

        return filtered

    async def get_statistics(
        self,
        messages: list[ExportMessage | dict[str, Any]],
    ) -> dict[str, Any]:
        """会話統計を取得.

        Args:
            messages: メッセージリスト

        Returns:
            統計情報
        """
        normalized = self._normalize_messages(messages)

        # プラットフォーム別カウント
        platform_counts: dict[str, int] = {}
        user_counts: dict[str, int] = {}
        role_counts: dict[str, int] = {}

        for msg in normalized:
            platform_counts[msg.platform] = platform_counts.get(msg.platform, 0) + 1
            user_counts[msg.user_id] = user_counts.get(msg.user_id, 0) + 1
            role_counts[msg.role] = role_counts.get(msg.role, 0) + 1

        return {
            "total_messages": len(normalized),
            "platforms": platform_counts,
            "users": user_counts,
            "roles": role_counts,
            "first_message": normalized[0].timestamp if normalized else None,
            "last_message": normalized[-1].timestamp if normalized else None,
        }
