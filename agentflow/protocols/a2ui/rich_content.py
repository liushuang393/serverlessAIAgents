"""A2UI 富文本コンポーネント - Markdown/表格/コード/チャート対応.

このモジュールは AG-UI/A2UI 規範に準拠した富文本コンポーネントを提供します。

含まれるコンポーネント:
- MarkdownContent: Markdown テキスト（リンク、リスト、引用対応）
- CodeBlock: コードブロック（シンタックスハイライト対応）
- DataTable: データテーブル（ソート、検索対応）
- ChartView: 各種チャート（ECharts 互換）
- Citation: 引用/ソース表示
- CollapsibleSection: 折りたたみセクション

使用例:
    >>> from agentflow.protocols.a2ui.rich_content import (
    ...     MarkdownContent,
    ...     CodeBlock,
    ...     DataTable,
    ...     RichResponse,
    ... )
    >>>
    >>> response = RichResponse()
    >>> response.add_markdown("# 分析結果\\n結果は以下の通りです...")
    >>> response.add_table(data=[{"name": "A", "value": 100}])
    >>> response.add_code("print('hello')", language="python")
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


class RichComponentType(str, Enum):
    """富文本コンポーネント種別."""

    MARKDOWN = "markdown"
    CODE_BLOCK = "code_block"
    DATA_TABLE = "data_table"
    CHART = "chart"
    CITATION = "citation"
    COLLAPSIBLE = "collapsible"
    LINK = "link"
    IMAGE_GALLERY = "image_gallery"
    PROGRESS = "progress"
    ALERT = "alert"
    TABS = "tabs"
    TIMELINE = "timeline"


class ChartType(str, Enum):
    """チャート種別."""

    BAR = "bar"
    LINE = "line"
    PIE = "pie"
    SCATTER = "scatter"
    AREA = "area"
    RADAR = "radar"
    HEATMAP = "heatmap"
    TREEMAP = "treemap"


class AlertType(str, Enum):
    """アラート種別."""

    INFO = "info"
    SUCCESS = "success"
    WARNING = "warning"
    ERROR = "error"


@dataclass
class RichComponent:
    """富文本コンポーネント基底クラス."""

    component_type: RichComponentType
    id: str | None = None
    props: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "type": self.component_type.value,
            "id": self.id,
            "props": self.props,
            "metadata": self.metadata,
        }


@dataclass
class MarkdownContent(RichComponent):
    """Markdown コンテンツ.

    特徴:
    - 自動リンク検出
    - コードブロック自動抽出
    - 見出し階層対応
    """

    def __init__(
        self,
        content: str,
        allow_html: bool = False,
        highlight_keywords: list[str] | None = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            content: Markdown テキスト
            allow_html: HTML タグを許可するか
            highlight_keywords: ハイライトするキーワード
        """
        super().__init__(
            component_type=RichComponentType.MARKDOWN,
            props={
                "content": content,
                "allowHtml": allow_html,
                "highlightKeywords": highlight_keywords or [],
                **kwargs,
            },
        )


@dataclass
class CodeBlock(RichComponent):
    """コードブロック（シンタックスハイライト対応）.

    対応言語: python, javascript, typescript, sql, json, yaml, bash, etc.
    """

    def __init__(
        self,
        code: str,
        language: str = "text",
        title: str | None = None,
        show_line_numbers: bool = True,
        highlight_lines: list[int] | None = None,
        collapsible: bool = False,
        max_height: int | None = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            code: コード内容
            language: 言語（シンタックスハイライト用）
            title: タイトル（ファイル名等）
            show_line_numbers: 行番号を表示するか
            highlight_lines: ハイライトする行番号
            collapsible: 折りたたみ可能か
            max_height: 最大高さ（px）
        """
        super().__init__(
            component_type=RichComponentType.CODE_BLOCK,
            props={
                "code": code,
                "language": language,
                "title": title,
                "showLineNumbers": show_line_numbers,
                "highlightLines": highlight_lines or [],
                "collapsible": collapsible,
                "maxHeight": max_height,
                **kwargs,
            },
        )


@dataclass
class DataTable(RichComponent):
    """データテーブル.

    特徴:
    - カラムソート
    - 検索フィルタ
    - ページネーション
    - CSV エクスポート
    """

    def __init__(
        self,
        data: list[dict[str, Any]],
        columns: list[dict[str, Any]] | None = None,
        title: str | None = None,
        sortable: bool = True,
        filterable: bool = True,
        paginated: bool = True,
        page_size: int = 10,
        exportable: bool = True,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            data: テーブルデータ（行のリスト）
            columns: カラム定義 [{"key": "name", "label": "名前", "type": "string"}]
            title: テーブルタイトル
            sortable: ソート可能か
            filterable: フィルタ可能か
            paginated: ページネーションするか
            page_size: ページサイズ
            exportable: エクスポート可能か
        """
        # カラム自動生成
        if columns is None and data:
            columns = [{"key": k, "label": k, "type": self._infer_type(v)} for k, v in data[0].items()]

        super().__init__(
            component_type=RichComponentType.DATA_TABLE,
            props={
                "data": data,
                "columns": columns or [],
                "title": title,
                "sortable": sortable,
                "filterable": filterable,
                "paginated": paginated,
                "pageSize": page_size,
                "exportable": exportable,
                **kwargs,
            },
            metadata={
                "rowCount": len(data),
                "columnCount": len(columns) if columns else 0,
            },
        )

    def _infer_type(self, value: Any) -> str:
        """値から型を推論."""
        if isinstance(value, bool):
            return "boolean"
        if isinstance(value, int):
            return "integer"
        if isinstance(value, float):
            return "number"
        if isinstance(value, (list, dict)):
            return "object"
        return "string"


@dataclass
class ChartView(RichComponent):
    """チャートビュー（ECharts 互換）.

    対応チャート: bar, line, pie, scatter, area, radar, heatmap, treemap
    """

    def __init__(
        self,
        chart_type: ChartType | str,
        data: dict[str, Any],
        title: str | None = None,
        width: str = "100%",
        height: str = "300px",
        theme: str = "default",
        interactive: bool = True,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            chart_type: チャート種別
            data: ECharts 形式のデータ
            title: チャートタイトル
            width: 幅
            height: 高さ
            theme: テーマ
            interactive: インタラクティブか
        """
        if isinstance(chart_type, str):
            chart_type = ChartType(chart_type)

        super().__init__(
            component_type=RichComponentType.CHART,
            props={
                "chartType": chart_type.value,
                "data": data,
                "title": title,
                "width": width,
                "height": height,
                "theme": theme,
                "interactive": interactive,
                **kwargs,
            },
        )

    @classmethod
    def from_table_data(
        cls,
        data: list[dict[str, Any]],
        x_key: str,
        y_key: str,
        chart_type: ChartType = ChartType.BAR,
        title: str | None = None,
    ) -> ChartView:
        """テーブルデータからチャートを作成.

        Args:
            data: テーブルデータ
            x_key: X軸のキー
            y_key: Y軸のキー
            chart_type: チャート種別
            title: タイトル

        Returns:
            ChartView インスタンス
        """
        labels = [str(row.get(x_key, "")) for row in data]
        values = [row.get(y_key, 0) for row in data]

        echarts_data = {
            "xAxis": {"type": "category", "data": labels},
            "yAxis": {"type": "value"},
            "series": [{"type": chart_type.value, "data": values}],
            "tooltip": {"trigger": "axis"},
        }

        if title:
            echarts_data["title"] = {"text": title}

        return cls(chart_type=chart_type, data=echarts_data, title=title)


@dataclass
class Citation(RichComponent):
    """引用/ソース表示."""

    def __init__(
        self,
        source_id: str,
        title: str,
        snippet: str,
        url: str | None = None,
        relevance_score: float | None = None,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            source_id: ソース ID
            title: タイトル
            snippet: 抜粋
            url: リンク URL
            relevance_score: 関連度スコア
        """
        super().__init__(
            component_type=RichComponentType.CITATION,
            id=source_id,
            props={
                "title": title,
                "snippet": snippet,
                "url": url,
                "relevanceScore": relevance_score,
                **kwargs,
            },
        )


@dataclass
class CollapsibleSection(RichComponent):
    """折りたたみセクション."""

    def __init__(
        self,
        title: str,
        content: list[RichComponent] | str,
        expanded: bool = False,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            title: セクションタイトル
            content: コンテンツ（コンポーネントリストまたはテキスト）
            expanded: 初期状態で展開するか
        """
        content_data = content if isinstance(content, str) else [c.to_dict() for c in content]

        super().__init__(
            component_type=RichComponentType.COLLAPSIBLE,
            props={
                "title": title,
                "content": content_data,
                "expanded": expanded,
                **kwargs,
            },
        )


@dataclass
class Link(RichComponent):
    """リンク."""

    def __init__(
        self,
        text: str,
        url: str,
        external: bool = True,
        icon: str | None = None,
        **kwargs: Any,
    ) -> None:
        """初期化."""
        super().__init__(
            component_type=RichComponentType.LINK,
            props={
                "text": text,
                "url": url,
                "external": external,
                "icon": icon,
                **kwargs,
            },
        )


@dataclass
class Progress(RichComponent):
    """進捗表示."""

    def __init__(
        self,
        value: float,
        max_value: float = 100,
        label: str | None = None,
        show_percentage: bool = True,
        status: str = "active",
        **kwargs: Any,
    ) -> None:
        """初期化."""
        super().__init__(
            component_type=RichComponentType.PROGRESS,
            props={
                "value": value,
                "maxValue": max_value,
                "label": label,
                "showPercentage": show_percentage,
                "status": status,
                **kwargs,
            },
        )


@dataclass
class Alert(RichComponent):
    """アラート/通知."""

    def __init__(
        self,
        message: str,
        alert_type: AlertType = AlertType.INFO,
        title: str | None = None,
        dismissible: bool = True,
        **kwargs: Any,
    ) -> None:
        """初期化."""
        super().__init__(
            component_type=RichComponentType.ALERT,
            props={
                "message": message,
                "alertType": alert_type.value,
                "title": title,
                "dismissible": dismissible,
                **kwargs,
            },
        )


@dataclass
class Tabs(RichComponent):
    """タブコンテナ."""

    def __init__(
        self,
        tabs: list[dict[str, Any]],
        active_tab: int = 0,
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            tabs: タブ定義 [{"title": "Tab1", "content": [...]}]
            active_tab: 初期アクティブタブ
        """
        super().__init__(
            component_type=RichComponentType.TABS,
            props={
                "tabs": tabs,
                "activeTab": active_tab,
                **kwargs,
            },
        )


@dataclass
class Timeline(RichComponent):
    """タイムライン."""

    def __init__(
        self,
        events: list[dict[str, Any]],
        **kwargs: Any,
    ) -> None:
        """初期化.

        Args:
            events: イベントリスト [{"time": "...", "title": "...", "description": "..."}]
        """
        super().__init__(
            component_type=RichComponentType.TIMELINE,
            props={
                "events": events,
                **kwargs,
            },
        )


# =============================================================================
# 富文本レスポンスビルダー
# =============================================================================


@dataclass
class RichResponse:
    """富文本レスポンスビルダー.

    複数のコンポーネントを組み合わせてレスポンスを構築します。

    使用例:
        >>> response = RichResponse()
        >>> response.add_markdown("# 結果")
        >>> response.add_table([{"name": "A", "value": 1}])
        >>> response.add_chart(ChartType.BAR, data)
        >>> return response.to_dict()
    """

    components: list[RichComponent] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.now)

    def add_markdown(
        self,
        content: str,
        **kwargs: Any,
    ) -> RichResponse:
        """Markdown を追加."""
        self.components.append(MarkdownContent(content, **kwargs))
        return self

    def add_code(
        self,
        code: str,
        language: str = "text",
        title: str | None = None,
        **kwargs: Any,
    ) -> RichResponse:
        """コードブロックを追加."""
        self.components.append(CodeBlock(code, language, title, **kwargs))
        return self

    def add_table(
        self,
        data: list[dict[str, Any]],
        columns: list[dict[str, Any]] | None = None,
        title: str | None = None,
        **kwargs: Any,
    ) -> RichResponse:
        """テーブルを追加."""
        self.components.append(DataTable(data, columns, title, **kwargs))
        return self

    def add_chart(
        self,
        chart_type: ChartType | str,
        data: dict[str, Any],
        title: str | None = None,
        **kwargs: Any,
    ) -> RichResponse:
        """チャートを追加."""
        self.components.append(ChartView(chart_type, data, title, **kwargs))
        return self

    def add_chart_from_data(
        self,
        data: list[dict[str, Any]],
        x_key: str,
        y_key: str,
        chart_type: ChartType = ChartType.BAR,
        title: str | None = None,
    ) -> RichResponse:
        """テーブルデータからチャートを追加."""
        chart = ChartView.from_table_data(data, x_key, y_key, chart_type, title)
        self.components.append(chart)
        return self

    def add_citation(
        self,
        source_id: str,
        title: str,
        snippet: str,
        url: str | None = None,
        relevance_score: float | None = None,
    ) -> RichResponse:
        """引用を追加."""
        self.components.append(Citation(source_id, title, snippet, url, relevance_score))
        return self

    def add_citations(
        self,
        citations: list[dict[str, Any]],
    ) -> RichResponse:
        """複数の引用を追加."""
        for c in citations:
            self.add_citation(
                source_id=c.get("id", ""),
                title=c.get("title", ""),
                snippet=c.get("snippet", c.get("content", "")[:200]),
                url=c.get("url"),
                relevance_score=c.get("score"),
            )
        return self

    def add_collapsible(
        self,
        title: str,
        content: list[RichComponent] | str,
        expanded: bool = False,
    ) -> RichResponse:
        """折りたたみセクションを追加."""
        self.components.append(CollapsibleSection(title, content, expanded))
        return self

    def add_progress(
        self,
        value: float,
        max_value: float = 100,
        label: str | None = None,
    ) -> RichResponse:
        """進捗を追加."""
        self.components.append(Progress(value, max_value, label))
        return self

    def add_alert(
        self,
        message: str,
        alert_type: AlertType = AlertType.INFO,
        title: str | None = None,
    ) -> RichResponse:
        """アラートを追加."""
        self.components.append(Alert(message, alert_type, title))
        return self

    def add_link(
        self,
        text: str,
        url: str,
        external: bool = True,
    ) -> RichResponse:
        """リンクを追加."""
        self.components.append(Link(text, url, external))
        return self

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "components": [c.to_dict() for c in self.components],
            "metadata": self.metadata,
            "createdAt": self.created_at.isoformat(),
        }

    def to_json(self) -> str:
        """JSON 文字列に変換."""
        return json.dumps(self.to_dict(), ensure_ascii=False, default=str)


__all__ = [
    "Alert",
    "AlertType",
    "ChartType",
    "ChartView",
    "Citation",
    "CodeBlock",
    "CollapsibleSection",
    "DataTable",
    "Link",
    "MarkdownContent",
    "Progress",
    # コンポーネント
    "RichComponent",
    # 列挙型
    "RichComponentType",
    # ビルダー
    "RichResponse",
    "Tabs",
    "Timeline",
]
