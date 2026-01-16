# -*- coding: utf-8 -*-
"""語義層（Semantic Layer）サービス.

自然言語→SQL変換における指標定義・ディメンション管理を提供。
口径の統一と安全なSQL生成を実現。

設計原則:
- 指標定義の一元管理（GMV/売上/粗利等の定義統一）
- ディメンション辞書（時間/地域/商品等）
- アクセス制御（ホワイトリスト/ブラックリスト）

使用例:
    >>> from agentflow.services.semantic_layer import SemanticLayerService
    >>>
    >>> service = SemanticLayerService()
    >>>
    >>> # 指標解決
    >>> resolved = await service.resolve_metrics("今月の売上TOP10")
    >>> print(resolved.metrics)  # [Metric(name="売上", ...)]
    >>>
    >>> # SQL生成ヒント
    >>> hints = service.get_sql_hints(resolved)
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

logger = logging.getLogger(__name__)


class MetricType(str, Enum):
    """指標タイプ."""

    MEASURE = "measure"  # 数値指標（売上、件数等）
    RATE = "rate"  # 率指標（成長率、CVR等）
    RATIO = "ratio"  # 比率指標（構成比等）
    COUNT = "count"  # カウント指標


class AggregationType(str, Enum):
    """集計タイプ."""

    SUM = "sum"
    COUNT = "count"
    AVG = "avg"
    MAX = "max"
    MIN = "min"
    DISTINCT_COUNT = "distinct_count"


class TimeGranularity(str, Enum):
    """時間粒度."""

    DAY = "day"
    WEEK = "week"
    MONTH = "month"
    QUARTER = "quarter"
    YEAR = "year"


@dataclass
class Metric:
    """指標定義.

    Attributes:
        name: 指標名（日本語）
        metric_id: 指標ID（英語）
        description: 説明
        definition: 定義（計算式）
        table: 参照テーブル
        column: 参照カラム
        aggregation: 集計タイプ
        unit: 単位
        aliases: 別名リスト
    """

    name: str
    metric_id: str
    description: str = ""
    definition: str = ""
    table: str = ""
    column: str = ""
    aggregation: AggregationType = AggregationType.SUM
    metric_type: MetricType = MetricType.MEASURE
    unit: str = ""
    aliases: list[str] = field(default_factory=list)
    filters: dict[str, Any] = field(default_factory=dict)
    # 口径情報
    scope_note: str = ""  # 例: "確定済み注文のみ、キャンセル除外"


@dataclass
class Dimension:
    """ディメンション定義.

    Attributes:
        name: ディメンション名
        dimension_id: ディメンションID
        table: 参照テーブル
        column: 参照カラム
        hierarchy: 階層（例: 年→四半期→月→日）
        aliases: 別名リスト
    """

    name: str
    dimension_id: str
    description: str = ""
    table: str = ""
    column: str = ""
    hierarchy: list[str] = field(default_factory=list)
    aliases: list[str] = field(default_factory=list)
    values: list[str] = field(default_factory=list)  # 固定値リスト


@dataclass
class ResolvedQuery:
    """解決済みクエリ.

    Attributes:
        original_query: 元のクエリ
        metrics: 解決された指標
        dimensions: 解決されたディメンション
        filters: フィルタ条件
        time_range: 時間範囲
        granularity: 時間粒度
        limit: 取得件数
        order_by: ソート
    """

    original_query: str
    metrics: list[Metric] = field(default_factory=list)
    dimensions: list[Dimension] = field(default_factory=list)
    filters: dict[str, Any] = field(default_factory=dict)
    time_range: dict[str, str] = field(default_factory=dict)
    granularity: TimeGranularity | None = None
    limit: int | None = None
    order_by: list[tuple[str, str]] = field(default_factory=list)
    confidence: float = 1.0
    unresolved_terms: list[str] = field(default_factory=list)


@dataclass
class SQLHints:
    """SQL生成ヒント.

    Attributes:
        select_clause: SELECT句のヒント
        from_clause: FROM句のヒント
        where_clause: WHERE句のヒント
        group_by_clause: GROUP BY句のヒント
        order_by_clause: ORDER BY句のヒント
        limit_clause: LIMIT句のヒント
    """

    select_clause: list[str] = field(default_factory=list)
    from_clause: list[str] = field(default_factory=list)
    where_clause: list[str] = field(default_factory=list)
    group_by_clause: list[str] = field(default_factory=list)
    order_by_clause: list[str] = field(default_factory=list)
    limit_clause: str = ""
    joins: list[str] = field(default_factory=list)


@dataclass
class SemanticLayerConfig:
    """語義層設定."""

    # アクセス制御
    whitelist_tables: list[str] = field(default_factory=list)
    blacklist_columns: list[str] = field(default_factory=list)

    # デフォルト設定
    default_limit: int = 1000
    max_limit: int = 10000
    default_time_range_days: int = 30

    # 検証設定
    require_metric: bool = True
    allow_raw_columns: bool = False


class SemanticLayerService:
    """語義層サービス.

    指標・ディメンション定義を管理し、自然言語クエリを解決。

    Example:
        >>> service = SemanticLayerService()
        >>>
        >>> # 指標を定義
        >>> service.add_metric(Metric(
        ...     name="売上",
        ...     metric_id="revenue",
        ...     table="sales",
        ...     column="amount",
        ...     aggregation=AggregationType.SUM,
        ...     aliases=["売上金額", "売上高", "セールス"],
        ...     scope_note="税抜金額、確定済み注文のみ",
        ... ))
        >>>
        >>> # クエリ解決
        >>> resolved = await service.resolve("今月の売上TOP10")
    """

    # デフォルト指標
    DEFAULT_METRICS = [
        Metric(
            name="売上",
            metric_id="revenue",
            description="税抜売上金額",
            definition="SUM(sales.amount)",
            table="sales",
            column="amount",
            aggregation=AggregationType.SUM,
            unit="円",
            aliases=["売上金額", "売上高", "セールス", "Revenue"],
            scope_note="税抜金額、確定済み注文のみ（キャンセル・返品除外）",
        ),
        Metric(
            name="GMV",
            metric_id="gmv",
            description="総取引額",
            definition="SUM(orders.total_amount)",
            table="orders",
            column="total_amount",
            aggregation=AggregationType.SUM,
            unit="円",
            aliases=["総取引額", "流通総額"],
            scope_note="税込金額、キャンセル含む",
        ),
        Metric(
            name="粗利",
            metric_id="gross_profit",
            description="粗利益",
            definition="SUM(sales.amount - sales.cost)",
            table="sales",
            column="profit",
            aggregation=AggregationType.SUM,
            unit="円",
            aliases=["粗利益", "利益", "マージン"],
            scope_note="配送完了後の確定利益",
        ),
        Metric(
            name="注文数",
            metric_id="order_count",
            description="注文件数",
            definition="COUNT(orders.id)",
            table="orders",
            column="id",
            aggregation=AggregationType.COUNT,
            metric_type=MetricType.COUNT,
            unit="件",
            aliases=["オーダー数", "受注数", "件数"],
        ),
        Metric(
            name="顧客数",
            metric_id="customer_count",
            description="ユニーク顧客数",
            definition="COUNT(DISTINCT customers.id)",
            table="customers",
            column="id",
            aggregation=AggregationType.DISTINCT_COUNT,
            metric_type=MetricType.COUNT,
            unit="人",
            aliases=["ユーザー数", "会員数"],
        ),
    ]

    # デフォルトディメンション
    DEFAULT_DIMENSIONS = [
        Dimension(
            name="日付",
            dimension_id="date",
            description="日付ディメンション",
            table="sales",
            column="order_date",
            hierarchy=["year", "quarter", "month", "week", "day"],
            aliases=["日", "日時", "時間", "期間"],
        ),
        Dimension(
            name="商品カテゴリ",
            dimension_id="category",
            description="商品カテゴリ",
            table="products",
            column="category",
            aliases=["カテゴリ", "分類", "種別"],
        ),
        Dimension(
            name="地域",
            dimension_id="region",
            description="地域ディメンション",
            table="customers",
            column="region",
            hierarchy=["country", "region", "prefecture", "city"],
            aliases=["エリア", "都道府県", "地方"],
        ),
        Dimension(
            name="顧客セグメント",
            dimension_id="segment",
            description="顧客セグメント",
            table="customers",
            column="segment",
            aliases=["セグメント", "グループ", "層"],
        ),
        Dimension(
            name="販売チャネル",
            dimension_id="channel",
            description="販売チャネル",
            table="sales",
            column="channel",
            aliases=["チャネル", "経路", "チャンネル"],
            values=["EC", "店舗", "卸"],
        ),
    ]

    # 時間表現パターン
    TIME_PATTERNS = [
        (r"今月", "current_month"),
        (r"先月", "last_month"),
        (r"今年", "current_year"),
        (r"昨年|前年", "last_year"),
        (r"今日", "today"),
        (r"昨日", "yesterday"),
        (r"今週", "current_week"),
        (r"先週", "last_week"),
        (r"過去(\d+)日", "last_n_days"),
        (r"(\d{4})年(\d{1,2})月", "specific_month"),
        (r"第(\d)四半期|Q(\d)", "quarter"),
    ]

    # 順序表現パターン
    ORDER_PATTERNS = [
        (r"TOP\s*(\d+)|上位\s*(\d+)", "top_n"),
        (r"ワースト\s*(\d+)|下位\s*(\d+)", "bottom_n"),
        (r"降順|多い順", "desc"),
        (r"昇順|少ない順", "asc"),
    ]

    def __init__(
        self,
        config: SemanticLayerConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or SemanticLayerConfig()
        self._metrics: dict[str, Metric] = {}
        self._dimensions: dict[str, Dimension] = {}
        self._logger = logging.getLogger(__name__)

        # デフォルト定義を登録
        for metric in self.DEFAULT_METRICS:
            self.add_metric(metric)
        for dimension in self.DEFAULT_DIMENSIONS:
            self.add_dimension(dimension)

    def add_metric(self, metric: Metric) -> None:
        """指標を追加.

        Args:
            metric: 指標定義
        """
        self._metrics[metric.metric_id] = metric
        self._logger.debug("Added metric: %s", metric.name)

    def add_dimension(self, dimension: Dimension) -> None:
        """ディメンションを追加.

        Args:
            dimension: ディメンション定義
        """
        self._dimensions[dimension.dimension_id] = dimension
        self._logger.debug("Added dimension: %s", dimension.name)

    def get_metric(self, metric_id: str) -> Metric | None:
        """指標を取得.

        Args:
            metric_id: 指標ID

        Returns:
            指標定義、または None
        """
        return self._metrics.get(metric_id)

    def get_dimension(self, dimension_id: str) -> Dimension | None:
        """ディメンションを取得.

        Args:
            dimension_id: ディメンションID

        Returns:
            ディメンション定義、または None
        """
        return self._dimensions.get(dimension_id)

    def list_metrics(self) -> list[Metric]:
        """全指標を一覧.

        Returns:
            指標リスト
        """
        return list(self._metrics.values())

    def list_dimensions(self) -> list[Dimension]:
        """全ディメンションを一覧.

        Returns:
            ディメンションリスト
        """
        return list(self._dimensions.values())

    async def resolve(self, query: str) -> ResolvedQuery:
        """自然言語クエリを解決.

        Args:
            query: 自然言語クエリ

        Returns:
            解決済みクエリ
        """
        resolved = ResolvedQuery(original_query=query)

        # 1. 指標を抽出
        resolved.metrics = self._extract_metrics(query)

        # 2. ディメンションを抽出
        resolved.dimensions = self._extract_dimensions(query)

        # 3. 時間範囲を抽出
        resolved.time_range = self._extract_time_range(query)

        # 4. 時間粒度を抽出
        resolved.granularity = self._extract_granularity(query)

        # 5. 順序・制限を抽出
        order_limit = self._extract_order_limit(query)
        resolved.limit = order_limit.get("limit")
        resolved.order_by = order_limit.get("order_by", [])

        # 6. 信頼度を計算
        resolved.confidence = self._calculate_confidence(resolved)

        # 7. 未解決の用語を特定
        resolved.unresolved_terms = self._find_unresolved_terms(
            query, resolved
        )

        return resolved

    def get_sql_hints(self, resolved: ResolvedQuery) -> SQLHints:
        """SQL生成ヒントを取得.

        Args:
            resolved: 解決済みクエリ

        Returns:
            SQL生成ヒント
        """
        hints = SQLHints()

        # SELECT句
        for metric in resolved.metrics:
            agg = metric.aggregation.value.upper()
            col = f"{metric.table}.{metric.column}"
            hints.select_clause.append(
                f"{agg}({col}) AS {metric.metric_id}"
            )

        for dim in resolved.dimensions:
            hints.select_clause.append(
                f"{dim.table}.{dim.column} AS {dim.dimension_id}"
            )
            hints.group_by_clause.append(
                f"{dim.table}.{dim.column}"
            )

        # FROM句
        tables = set()
        for metric in resolved.metrics:
            if metric.table:
                tables.add(metric.table)
        for dim in resolved.dimensions:
            if dim.table:
                tables.add(dim.table)

        hints.from_clause = list(tables)

        # WHERE句
        if resolved.time_range:
            date_col = self._get_date_column(resolved)
            if date_col:
                if resolved.time_range.get("start"):
                    hints.where_clause.append(
                        f"{date_col} >= '{resolved.time_range['start']}'"
                    )
                if resolved.time_range.get("end"):
                    hints.where_clause.append(
                        f"{date_col} <= '{resolved.time_range['end']}'"
                    )

        # ORDER BY句
        for col, direction in resolved.order_by:
            hints.order_by_clause.append(f"{col} {direction}")

        # LIMIT句
        if resolved.limit:
            hints.limit_clause = f"LIMIT {resolved.limit}"
        else:
            hints.limit_clause = f"LIMIT {self._config.default_limit}"

        return hints

    def validate_access(
        self,
        resolved: ResolvedQuery,
        user_context: dict[str, Any] | None = None,
    ) -> tuple[bool, list[str]]:
        """アクセス権限を検証.

        Args:
            resolved: 解決済みクエリ
            user_context: ユーザーコンテキスト

        Returns:
            (許可フラグ, 違反リスト)
        """
        violations = []

        # ホワイトリストチェック
        if self._config.whitelist_tables:
            for metric in resolved.metrics:
                if metric.table and metric.table not in self._config.whitelist_tables:
                    violations.append(
                        f"Table not in whitelist: {metric.table}"
                    )

        # ブラックリストチェック
        for metric in resolved.metrics:
            col_pattern = f"{metric.table}.{metric.column}"
            for blacklisted in self._config.blacklist_columns:
                if self._match_column_pattern(col_pattern, blacklisted):
                    violations.append(
                        f"Column blacklisted: {col_pattern}"
                    )

        return len(violations) == 0, violations

    def _extract_metrics(self, query: str) -> list[Metric]:
        """指標を抽出.

        Args:
            query: クエリ

        Returns:
            指標リスト
        """
        found = []
        query_lower = query.lower()

        for metric in self._metrics.values():
            # 名前でマッチ
            if metric.name.lower() in query_lower:
                found.append(metric)
                continue

            # 別名でマッチ
            for alias in metric.aliases:
                if alias.lower() in query_lower:
                    found.append(metric)
                    break

        return found

    def _extract_dimensions(self, query: str) -> list[Dimension]:
        """ディメンションを抽出.

        Args:
            query: クエリ

        Returns:
            ディメンションリスト
        """
        found = []
        query_lower = query.lower()

        for dim in self._dimensions.values():
            # 名前でマッチ
            if dim.name.lower() in query_lower:
                found.append(dim)
                continue

            # 別名でマッチ
            for alias in dim.aliases:
                if alias.lower() in query_lower:
                    found.append(dim)
                    break

        return found

    def _extract_time_range(self, query: str) -> dict[str, str]:
        """時間範囲を抽出.

        Args:
            query: クエリ

        Returns:
            {"start": "YYYY-MM-DD", "end": "YYYY-MM-DD"}
        """
        now = datetime.now()

        for pattern, time_type in self.TIME_PATTERNS:
            match = re.search(pattern, query)
            if match:
                if time_type == "current_month":
                    start = now.replace(day=1).strftime("%Y-%m-%d")
                    end = now.strftime("%Y-%m-%d")
                    return {"start": start, "end": end}

                elif time_type == "last_month":
                    first_of_month = now.replace(day=1)
                    last_month_end = first_of_month - timedelta(days=1)
                    last_month_start = last_month_end.replace(day=1)
                    return {
                        "start": last_month_start.strftime("%Y-%m-%d"),
                        "end": last_month_end.strftime("%Y-%m-%d"),
                    }

                elif time_type == "current_year":
                    start = now.replace(month=1, day=1).strftime("%Y-%m-%d")
                    end = now.strftime("%Y-%m-%d")
                    return {"start": start, "end": end}

                elif time_type == "today":
                    today = now.strftime("%Y-%m-%d")
                    return {"start": today, "end": today}

                elif time_type == "last_n_days":
                    days = int(match.group(1))
                    start = (now - timedelta(days=days)).strftime("%Y-%m-%d")
                    end = now.strftime("%Y-%m-%d")
                    return {"start": start, "end": end}

        # デフォルト: 過去30日
        return {
            "start": (now - timedelta(days=self._config.default_time_range_days)).strftime("%Y-%m-%d"),
            "end": now.strftime("%Y-%m-%d"),
        }

    def _extract_granularity(self, query: str) -> TimeGranularity | None:
        """時間粒度を抽出.

        Args:
            query: クエリ

        Returns:
            時間粒度
        """
        granularity_patterns = [
            (r"日別|日ごと", TimeGranularity.DAY),
            (r"週別|週ごと", TimeGranularity.WEEK),
            (r"月別|月ごと", TimeGranularity.MONTH),
            (r"四半期別|四半期ごと", TimeGranularity.QUARTER),
            (r"年別|年ごと", TimeGranularity.YEAR),
        ]

        for pattern, granularity in granularity_patterns:
            if re.search(pattern, query):
                return granularity

        return None

    def _extract_order_limit(self, query: str) -> dict[str, Any]:
        """順序と制限を抽出.

        Args:
            query: クエリ

        Returns:
            {"limit": int, "order_by": [(col, dir)]}
        """
        result: dict[str, Any] = {}

        for pattern, order_type in self.ORDER_PATTERNS:
            match = re.search(pattern, query, re.IGNORECASE)
            if match:
                if order_type == "top_n":
                    n = int(match.group(1) or match.group(2))
                    result["limit"] = min(n, self._config.max_limit)
                    result["order_by"] = [("metric", "DESC")]
                elif order_type == "bottom_n":
                    n = int(match.group(1) or match.group(2))
                    result["limit"] = min(n, self._config.max_limit)
                    result["order_by"] = [("metric", "ASC")]
                elif order_type == "desc":
                    result["order_by"] = [("metric", "DESC")]
                elif order_type == "asc":
                    result["order_by"] = [("metric", "ASC")]

        return result

    def _calculate_confidence(self, resolved: ResolvedQuery) -> float:
        """信頼度を計算.

        Args:
            resolved: 解決済みクエリ

        Returns:
            信頼度（0.0-1.0）
        """
        score = 1.0

        # 指標が見つからない場合
        if not resolved.metrics:
            score -= 0.3

        # 時間範囲が特定できない場合
        if not resolved.time_range:
            score -= 0.1

        # 未解決の用語がある場合
        if resolved.unresolved_terms:
            score -= 0.1 * len(resolved.unresolved_terms)

        return max(0.0, score)

    def _find_unresolved_terms(
        self,
        query: str,
        resolved: ResolvedQuery,
    ) -> list[str]:
        """未解決の用語を特定.

        Args:
            query: 元のクエリ
            resolved: 解決済みクエリ

        Returns:
            未解決用語リスト
        """
        # 簡易実装: 主要な名詞を抽出して未解決を判定
        # 実際の実装では形態素解析を使用
        return []

    def _get_date_column(self, resolved: ResolvedQuery) -> str | None:
        """日付カラムを取得.

        Args:
            resolved: 解決済みクエリ

        Returns:
            日付カラム名
        """
        # 日付ディメンションから取得
        for dim in resolved.dimensions:
            if dim.dimension_id == "date":
                return f"{dim.table}.{dim.column}"

        # メトリクスのテーブルからデフォルト
        if resolved.metrics:
            return f"{resolved.metrics[0].table}.order_date"

        return None

    def _match_column_pattern(self, column: str, pattern: str) -> bool:
        """カラムパターンがマッチするか判定.

        Args:
            column: カラム名（table.column）
            pattern: パターン（*.column or table.column）

        Returns:
            マッチする場合True
        """
        if pattern.startswith("*."):
            col_only = pattern[2:]
            return column.endswith(f".{col_only}")
        return column == pattern

    def get_metric_documentation(self) -> str:
        """指標ドキュメントを生成.

        Returns:
            Markdown形式のドキュメント
        """
        lines = ["# 指標辞書\n"]

        for metric in self._metrics.values():
            lines.append(f"## {metric.name} ({metric.metric_id})")
            lines.append(f"- **説明**: {metric.description}")
            lines.append(f"- **定義**: `{metric.definition}`")
            lines.append(f"- **単位**: {metric.unit}")
            lines.append(f"- **口径**: {metric.scope_note}")
            lines.append(f"- **別名**: {', '.join(metric.aliases)}")
            lines.append("")

        return "\n".join(lines)


# timedelta import for time calculations
from datetime import timedelta


__all__ = [
    "SemanticLayerService",
    "SemanticLayerConfig",
    "Metric",
    "Dimension",
    "ResolvedQuery",
    "SQLHints",
    "MetricType",
    "AggregationType",
    "TimeGranularity",
]
