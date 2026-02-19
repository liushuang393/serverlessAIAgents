"""Rich Response Builder - 統一富文本構築器.

設計原則:
- 流暢API: メソッドチェーンによる構築
- 型安全: コンポーネント型の厳密な定義
- 再利用性: 共通パターンの抽出

使用例:
    >>> from agentflow.api import RichResponseBuilder
    >>>
    >>> builder = RichResponseBuilder()
    >>> response = (
    ...     builder
    ...     .add_markdown("# 分析結果")
    ...     .add_table(data, title="データ一覧")
    ...     .add_chart("bar", chart_data, title="可視化")
    ...     .add_code(sql, language="sql")
    ...     .add_citations(sources)
    ...     .build()
    ... )
"""

from __future__ import annotations

from typing import Any

from agentflow.protocols.a2ui.rich_content import (
    Alert,
    AlertType,
    ChartType,
    ChartView,
    Citation,
    CodeBlock,
    CollapsibleSection,
    DataTable,
    Link,
    MarkdownContent,
    Progress,
    RichComponent,
    RichResponse,
)


class ComponentFactory:
    """コンポーネントファクトリ.

    よく使うコンポーネントパターンを提供します。
    """

    @staticmethod
    def info_card(title: str, content: str, icon: str = "ℹ️") -> MarkdownContent:
        """情報カード."""
        return MarkdownContent(f"### {icon} {title}\n\n{content}")

    @staticmethod
    def warning_box(message: str, title: str = "注意") -> Alert:
        """警告ボックス."""
        return Alert(message, AlertType.WARNING, title=title)

    @staticmethod
    def error_box(message: str, title: str = "エラー") -> Alert:
        """エラーボックス."""
        return Alert(message, AlertType.ERROR, title=title)

    @staticmethod
    def success_box(message: str, title: str = "成功") -> Alert:
        """成功ボックス."""
        return Alert(message, AlertType.SUCCESS, title=title)

    @staticmethod
    def key_value_table(data: dict[str, Any], title: str | None = None) -> DataTable:
        """キー・バリュー形式のテーブル."""
        table_data = [{"項目": k, "値": str(v)} for k, v in data.items()]
        return DataTable(table_data, title=title, sortable=False, filterable=False)

    @staticmethod
    def score_gauge(
        value: float,
        max_value: float = 100,
        title: str = "スコア",
    ) -> ChartView:
        """スコアゲージ."""
        chart_data = {
            "series": [
                {
                    "type": "gauge",
                    "data": [{"value": value, "name": title}],
                    "max": max_value,
                    "detail": {"formatter": f"{{value}}/{max_value}"},
                }
            ],
        }
        return ChartView(chart_type="gauge", data=chart_data, title=title)

    @staticmethod
    def comparison_table(
        items: list[dict[str, Any]],
        compare_key: str,
        metrics: list[str],
        title: str | None = None,
    ) -> DataTable:
        """比較テーブル."""
        # 比較用に再構成
        table_data = [
            {compare_key: item.get(compare_key, ""), **{m: item.get(m, "") for m in metrics}}
            for item in items
        ]
        return DataTable(table_data, title=title)


class RichResponseBuilder:
    """富文本レスポンスビルダー.

    流暢APIでリッチなレスポンスを構築します。

    特徴:
    - メソッドチェーン
    - セクション管理
    - 自動フォーマット
    """

    def __init__(self) -> None:
        """初期化."""
        self._components: list[RichComponent] = []
        self._metadata: dict[str, Any] = {}
        self._sections: dict[str, list[RichComponent]] = {}
        self._current_section: str | None = None

    # =========================================================================
    # セクション管理
    # =========================================================================

    def section(self, name: str) -> RichResponseBuilder:
        """セクション開始.

        Args:
            name: セクション名

        Returns:
            self
        """
        self._current_section = name
        if name not in self._sections:
            self._sections[name] = []
        return self

    def end_section(self) -> RichResponseBuilder:
        """セクション終了."""
        self._current_section = None
        return self

    def _add_component(self, component: RichComponent) -> RichResponseBuilder:
        """コンポーネント追加."""
        if self._current_section:
            self._sections[self._current_section].append(component)
        else:
            self._components.append(component)
        return self

    # =========================================================================
    # 基本コンポーネント
    # =========================================================================

    def add_markdown(
        self,
        content: str,
        allow_html: bool = False,
    ) -> RichResponseBuilder:
        """Markdown 追加.

        Args:
            content: Markdown テキスト
            allow_html: HTML を許可するか

        Returns:
            self
        """
        return self._add_component(MarkdownContent(content, allow_html=allow_html))

    def add_heading(self, text: str, level: int = 1) -> RichResponseBuilder:
        """見出し追加.

        Args:
            text: 見出しテキスト
            level: 見出しレベル（1-6）

        Returns:
            self
        """
        prefix = "#" * min(max(level, 1), 6)
        return self.add_markdown(f"{prefix} {text}")

    def add_paragraph(self, text: str) -> RichResponseBuilder:
        """段落追加.

        Args:
            text: 段落テキスト

        Returns:
            self
        """
        return self.add_markdown(text)

    def add_code(
        self,
        code: str,
        language: str = "text",
        title: str | None = None,
        show_line_numbers: bool = True,
    ) -> RichResponseBuilder:
        """コードブロック追加.

        Args:
            code: コード内容
            language: 言語
            title: タイトル
            show_line_numbers: 行番号表示

        Returns:
            self
        """
        return self._add_component(
            CodeBlock(
                code,
                language=language,
                title=title,
                show_line_numbers=show_line_numbers,
            )
        )

    def add_table(
        self,
        data: list[dict[str, Any]],
        columns: list[dict[str, Any]] | None = None,
        title: str | None = None,
        sortable: bool = True,
        paginated: bool = True,
    ) -> RichResponseBuilder:
        """テーブル追加.

        Args:
            data: テーブルデータ
            columns: カラム定義
            title: タイトル
            sortable: ソート可能か
            paginated: ページネーションするか

        Returns:
            self
        """
        return self._add_component(
            DataTable(
                data,
                columns=columns,
                title=title,
                sortable=sortable,
                paginated=paginated,
            )
        )

    def add_chart(
        self,
        chart_type: ChartType | str,
        data: dict[str, Any],
        title: str | None = None,
        height: str = "300px",
    ) -> RichResponseBuilder:
        """チャート追加.

        Args:
            chart_type: チャート種別
            data: ECharts 形式データ
            title: タイトル
            height: 高さ

        Returns:
            self
        """
        return self._add_component(ChartView(chart_type, data, title=title, height=height))

    def add_bar_chart(
        self,
        labels: list[str],
        values: list[float],
        title: str | None = None,
    ) -> RichResponseBuilder:
        """棒グラフ追加（簡易版）.

        Args:
            labels: ラベル
            values: 値
            title: タイトル

        Returns:
            self
        """
        data = {
            "xAxis": {"type": "category", "data": labels},
            "yAxis": {"type": "value"},
            "series": [{"type": "bar", "data": values}],
            "tooltip": {"trigger": "axis"},
        }
        if title:
            data["title"] = {"text": title}
        return self.add_chart("bar", data, title=title)

    def add_line_chart(
        self,
        labels: list[str],
        values: list[float],
        title: str | None = None,
    ) -> RichResponseBuilder:
        """折れ線グラフ追加（簡易版）."""
        data = {
            "xAxis": {"type": "category", "data": labels},
            "yAxis": {"type": "value"},
            "series": [{"type": "line", "data": values, "smooth": True}],
            "tooltip": {"trigger": "axis"},
        }
        if title:
            data["title"] = {"text": title}
        return self.add_chart("line", data, title=title)

    def add_pie_chart(
        self,
        data_items: list[dict[str, Any]],
        title: str | None = None,
    ) -> RichResponseBuilder:
        """円グラフ追加（簡易版）.

        Args:
            data_items: [{"name": "A", "value": 100}, ...]
            title: タイトル

        Returns:
            self
        """
        data = {
            "series": [{"type": "pie", "radius": "50%", "data": data_items}],
            "tooltip": {"trigger": "item"},
        }
        if title:
            data["title"] = {"text": title}
        return self.add_chart("pie", data, title=title)

    # =========================================================================
    # 引用・リンク
    # =========================================================================

    def add_citation(
        self,
        source_id: str,
        title: str,
        snippet: str,
        url: str | None = None,
        score: float | None = None,
    ) -> RichResponseBuilder:
        """引用追加.

        Args:
            source_id: ソースID
            title: タイトル
            snippet: 抜粋
            url: URL
            score: 関連度スコア

        Returns:
            self
        """
        return self._add_component(
            Citation(source_id, title, snippet, url=url, relevance_score=score)
        )

    def add_citations(self, citations: list[dict[str, Any]]) -> RichResponseBuilder:
        """複数引用追加.

        Args:
            citations: 引用リスト

        Returns:
            self
        """
        for c in citations:
            self.add_citation(
                source_id=c.get("id", ""),
                title=c.get("title", ""),
                snippet=c.get("snippet", c.get("content", "")[:200]),
                url=c.get("url"),
                score=c.get("score"),
            )
        return self

    def add_link(
        self,
        text: str,
        url: str,
        external: bool = True,
    ) -> RichResponseBuilder:
        """リンク追加.

        Args:
            text: リンクテキスト
            url: URL
            external: 外部リンクか

        Returns:
            self
        """
        return self._add_component(Link(text, url, external=external))

    # =========================================================================
    # アラート・通知
    # =========================================================================

    def add_alert(
        self,
        message: str,
        alert_type: AlertType | str = AlertType.INFO,
        title: str | None = None,
    ) -> RichResponseBuilder:
        """アラート追加.

        Args:
            message: メッセージ
            alert_type: アラート種別
            title: タイトル

        Returns:
            self
        """
        if isinstance(alert_type, str):
            alert_type = AlertType(alert_type)
        return self._add_component(Alert(message, alert_type, title=title))

    def add_info(self, message: str, title: str | None = None) -> RichResponseBuilder:
        """情報アラート追加."""
        return self.add_alert(message, AlertType.INFO, title)

    def add_success(self, message: str, title: str | None = None) -> RichResponseBuilder:
        """成功アラート追加."""
        return self.add_alert(message, AlertType.SUCCESS, title)

    def add_warning(self, message: str, title: str | None = None) -> RichResponseBuilder:
        """警告アラート追加."""
        return self.add_alert(message, AlertType.WARNING, title)

    def add_error(self, message: str, title: str | None = None) -> RichResponseBuilder:
        """エラーアラート追加."""
        return self.add_alert(message, AlertType.ERROR, title)

    # =========================================================================
    # 進捗・折りたたみ
    # =========================================================================

    def add_progress(
        self,
        value: float,
        max_value: float = 100,
        label: str | None = None,
    ) -> RichResponseBuilder:
        """進捗バー追加.

        Args:
            value: 現在値
            max_value: 最大値
            label: ラベル

        Returns:
            self
        """
        return self._add_component(Progress(value, max_value, label=label))

    def add_collapsible(
        self,
        title: str,
        content: str | list[RichComponent],
        expanded: bool = False,
    ) -> RichResponseBuilder:
        """折りたたみセクション追加.

        Args:
            title: タイトル
            content: コンテンツ
            expanded: 初期展開状態

        Returns:
            self
        """
        return self._add_component(CollapsibleSection(title, content, expanded=expanded))

    # =========================================================================
    # メタデータ
    # =========================================================================

    def set_metadata(self, key: str, value: Any) -> RichResponseBuilder:
        """メタデータ設定.

        Args:
            key: キー
            value: 値

        Returns:
            self
        """
        self._metadata[key] = value
        return self

    # =========================================================================
    # ビルド
    # =========================================================================

    def build(self) -> dict[str, Any]:
        """レスポンスをビルド.

        Returns:
            富文本レスポンス辞書
        """
        # セクションをマージ
        all_components = list(self._components)
        for section_name, section_components in self._sections.items():
            if section_components:
                # セクションヘッダー追加
                all_components.append(MarkdownContent(f"## {section_name}"))
                all_components.extend(section_components)

        response = RichResponse(components=all_components, metadata=self._metadata)
        return response.to_dict()

    def build_raw(self) -> RichResponse:
        """RichResponse オブジェクトとしてビルド.

        Returns:
            RichResponse インスタンス
        """
        all_components = list(self._components)
        for section_components in self._sections.values():
            all_components.extend(section_components)

        return RichResponse(components=all_components, metadata=self._metadata)


__all__ = [
    "ComponentFactory",
    "RichResponseBuilder",
]
