# -*- coding: utf-8 -*-
"""分析Agent - 高層データ分析.

経営層・分析者向けのデータ分析Agent。
語義層 + SQL護欄 + 証拠チェーンを実現。

設計原則:
- 語義層による指標口径統一
- SQL護欄（ホワイトリスト・禁止操作）
- 証拠チェーン出力

使用例:
    >>> from apps.faq_system.backend.agents import AnalyticsAgent
    >>>
    >>> agent = AnalyticsAgent()
    >>> result = await agent.run({
    ...     "question": "今月の売上TOP10を教えて",
    ...     "user_context": {"user_id": "123", "role": "analyst"},
    ... })
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, AsyncIterator

from pydantic import BaseModel, Field

from agentflow.core import ResilientAgent
from agentflow.services.semantic_layer import (
    SemanticLayerService,
    SemanticLayerConfig,
    ResolvedQuery,
)
from agentflow.security.policy_engine import PolicyEngine, AuthContext, AuthMode

logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


@dataclass
class SQLGuardrails:
    """SQL護欄設定."""

    # 許可されるSQL操作
    allowed_operations: list[str] = field(default_factory=lambda: ["SELECT"])

    # 禁止キーワード
    forbidden_keywords: list[str] = field(default_factory=lambda: [
        "INSERT", "UPDATE", "DELETE", "DROP", "TRUNCATE",
        "ALTER", "CREATE", "GRANT", "REVOKE", "EXEC",
    ])

    # ホワイトリストテーブル
    whitelist_tables: list[str] = field(default_factory=lambda: [
        "sales", "orders", "products", "categories", "regions",
    ])

    # ブラックリストカラム
    blacklist_columns: list[str] = field(default_factory=lambda: [
        "*.password", "*.mynumber", "*.salary", "*.ssn",
        "employees.salary", "customers.credit_card",
        "customers.phone", "customers.address",
    ])

    # 自動制限
    auto_limit: int = 1000
    query_timeout_seconds: int = 30
    max_cost_threshold: float = 100.0


@dataclass
class AnalyticsConfig:
    """分析Agent設定."""

    # 語義層
    semantic_layer_config: SemanticLayerConfig = field(
        default_factory=SemanticLayerConfig
    )

    # SQL護欄
    guardrails: SQLGuardrails = field(default_factory=SQLGuardrails)

    # 出力設定
    include_evidence_chain: bool = True
    include_chart: bool = True
    include_alternatives: bool = True

    # LLM設定
    temperature: float = 0.1


class EvidenceChain(BaseModel):
    """証拠チェーン."""

    data_sources: list[dict[str, str]] = Field(default_factory=list)
    query_conditions: list[str] = Field(default_factory=list)
    assumptions: list[str] = Field(default_factory=list)
    limitations: list[str] = Field(default_factory=list)
    alternatives: list[str] = Field(default_factory=list)


class AnalyticsResponse(BaseModel):
    """分析Agent レスポンス."""

    question: str = ""
    answer: str = ""
    sql: str = ""
    data: list[dict[str, Any]] = Field(default_factory=list)
    columns: list[str] = Field(default_factory=list)

    # チャート
    chart: dict[str, Any] | None = None

    # 証拠チェーン
    evidence_chain: EvidenceChain = Field(default_factory=EvidenceChain)

    # 信頼度
    confidence: float = 0.0
    uncertainty_level: str = "low"  # low, medium, high
    needs_verification: bool = False

    # メタデータ
    resolved_metrics: list[str] = Field(default_factory=list)
    resolved_dimensions: list[str] = Field(default_factory=list)
    execution_time_ms: float = 0
    error: str = ""


# =============================================================================
# 分析Agent
# =============================================================================


class AnalyticsAgent(ResilientAgent):
    """分析Agent（語義層 + SQL護欄 + 証拠チェーン）.

    特徴:
    - 語義層による指標口径統一
    - SQL護欄（セキュリティ）
    - 証拠チェーン出力
    """

    name = "AnalyticsAgent"

    SYSTEM_PROMPT = """あなたはビジネスデータ分析の専門家です。

職責:
1. ユーザーの質問をSQLクエリに変換する
2. 結果を分析して洞察を提供する
3. 証拠チェーン（データソース、前提、制限）を明示する

回答ルール:
- データに基づいた客観的な分析
- 数値は適切にフォーマット
- 不確実な点は明示する
- 代替案を提案する"""

    def __init__(
        self,
        config: AnalyticsConfig | None = None,
        semantic_layer: SemanticLayerService | None = None,
        policy_engine: PolicyEngine | None = None,
    ) -> None:
        """初期化."""
        super().__init__()
        self._config = config or AnalyticsConfig()
        self._semantic_layer = semantic_layer or SemanticLayerService()
        self._policy_engine = policy_engine or PolicyEngine()
        self._logger = logging.getLogger(self.name)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent 実行."""
        start_time = datetime.now()
        question = input_data.get("question", "")
        user_context = input_data.get("user_context", {})

        if not question:
            return AnalyticsResponse(
                error="質問が指定されていません"
            ).model_dump()

        try:
            # 1. 権限チェック
            auth_result = await self._check_permission(user_context)
            if not auth_result["allowed"]:
                return AnalyticsResponse(
                    question=question,
                    error=f"アクセス権限がありません: {auth_result.get('reason', '')}",
                ).model_dump()

            # 2. 語義層で解決
            resolved = await self._semantic_layer.resolve(question)

            # 3. アクセス検証
            access_ok, violations = self._semantic_layer.validate_access(
                resolved, user_context
            )
            if not access_ok:
                return AnalyticsResponse(
                    question=question,
                    error=f"データアクセス制限: {', '.join(violations)}",
                ).model_dump()

            # 4. SQL生成
            sql = await self._generate_sql(resolved, user_context)

            # 5. SQL検証（護欄）
            validation_result = self._validate_sql(sql)
            if not validation_result["valid"]:
                return AnalyticsResponse(
                    question=question,
                    error=f"SQLセキュリティ違反: {validation_result['reason']}",
                ).model_dump()

            # 6. SQL実行（プレースホルダー）
            data, columns = await self._execute_sql(sql)

            # 7. 回答生成
            answer = await self._generate_answer(question, data, resolved)

            # 8. チャート生成
            chart = None
            if self._config.include_chart and data:
                chart = self._generate_chart(question, data, columns)

            # 9. 証拠チェーン構築
            evidence = self._build_evidence_chain(
                question, sql, resolved, data
            )

            # 10. 信頼度評価
            confidence = self._calculate_confidence(resolved, data)

            response = AnalyticsResponse(
                question=question,
                answer=answer,
                sql=sql,
                data=data[:100],  # 最大100行
                columns=columns,
                chart=chart,
                evidence_chain=evidence,
                confidence=confidence,
                uncertainty_level=self._get_uncertainty_level(confidence),
                needs_verification=confidence < 0.7,
                resolved_metrics=[m.name for m in resolved.metrics],
                resolved_dimensions=[d.name for d in resolved.dimensions],
                execution_time_ms=(
                    datetime.now() - start_time
                ).total_seconds() * 1000,
            )

            return response.model_dump()

        except Exception as e:
            self._logger.exception("AnalyticsAgent エラー: %s", e)
            return AnalyticsResponse(
                question=question,
                error=str(e),
            ).model_dump()

    async def run_stream(
        self, input_data: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行."""
        question = input_data.get("question", "")

        yield {
            "type": "progress",
            "progress": 0,
            "message": "クエリを解析中...",
        }

        yield {
            "type": "progress",
            "progress": 20,
            "message": "指標・ディメンションを解決中...",
        }

        yield {
            "type": "progress",
            "progress": 40,
            "message": "SQLを生成中...",
        }

        yield {
            "type": "progress",
            "progress": 60,
            "message": "データを取得中...",
        }

        yield {
            "type": "progress",
            "progress": 80,
            "message": "分析結果を生成中...",
        }

        result = await self.run(input_data)

        yield {
            "type": "progress",
            "progress": 100,
            "message": "完了",
        }

        yield {
            "type": "result",
            "data": result,
        }

    async def _check_permission(
        self, user_context: dict[str, Any]
    ) -> dict[str, Any]:
        """権限チェック."""
        if not user_context.get("user_id"):
            return {"allowed": False, "reason": "認証が必要です"}

        role = user_context.get("role", "guest")
        if role not in ["admin", "manager", "analyst"]:
            return {
                "allowed": False,
                "reason": f"ロール '{role}' にはアクセス権限がありません",
            }

        return {"allowed": True}

    async def _generate_sql(
        self,
        resolved: ResolvedQuery,
        user_context: dict[str, Any],
    ) -> str:
        """SQL生成."""
        hints = self._semantic_layer.get_sql_hints(resolved)

        # SELECT句
        select_parts = hints.select_clause or ["*"]

        # FROM句
        from_parts = hints.from_clause or ["sales"]

        # WHERE句
        where_parts = hints.where_clause

        # GROUP BY句
        group_by_parts = hints.group_by_clause

        # ORDER BY句
        order_by_parts = hints.order_by_clause

        # SQL構築
        sql = f"SELECT {', '.join(select_parts)}"
        sql += f"\nFROM {', '.join(from_parts)}"

        if where_parts:
            sql += f"\nWHERE {' AND '.join(where_parts)}"

        if group_by_parts:
            sql += f"\nGROUP BY {', '.join(group_by_parts)}"

        if order_by_parts:
            sql += f"\nORDER BY {', '.join(order_by_parts)}"

        # LIMIT追加
        sql += f"\nLIMIT {self._config.guardrails.auto_limit}"

        return sql

    def _validate_sql(self, sql: str) -> dict[str, Any]:
        """SQL検証（護欄チェック）."""
        sql_upper = sql.upper()

        # 禁止キーワードチェック
        for keyword in self._config.guardrails.forbidden_keywords:
            if keyword in sql_upper:
                return {
                    "valid": False,
                    "reason": f"禁止されたSQL操作: {keyword}",
                }

        # ブラックリストカラムチェック
        for column in self._config.guardrails.blacklist_columns:
            col_name = column.split(".")[-1]
            if col_name in sql_upper:
                return {
                    "valid": False,
                    "reason": f"アクセス禁止カラム: {column}",
                }

        return {"valid": True}

    async def _execute_sql(
        self, sql: str
    ) -> tuple[list[dict[str, Any]], list[str]]:
        """SQL実行（プレースホルダー）."""
        # 実際の実装では DB プロバイダーを使用
        # ここではサンプルデータを返す
        sample_data = [
            {"product_name": "商品A", "revenue": 15000000, "order_count": 1200},
            {"product_name": "商品B", "revenue": 12300000, "order_count": 980},
            {"product_name": "商品C", "revenue": 9800000, "order_count": 850},
            {"product_name": "商品D", "revenue": 8500000, "order_count": 720},
            {"product_name": "商品E", "revenue": 7200000, "order_count": 650},
        ]

        columns = ["product_name", "revenue", "order_count"]

        return sample_data, columns

    async def _generate_answer(
        self,
        question: str,
        data: list[dict[str, Any]],
        resolved: ResolvedQuery,
    ) -> str:
        """回答生成."""
        if not data:
            return "該当するデータが見つかりませんでした。条件を変更してお試しください。"

        # 簡易回答生成
        metric_names = [m.name for m in resolved.metrics] or ["データ"]

        answer = f"ご質問「{question}」に対する分析結果です。\n\n"

        if len(data) >= 1:
            top = data[0]
            answer += f"1位は「{top.get('product_name', top.get(list(top.keys())[0], 'N/A'))}」"

            if "revenue" in top:
                answer += f"で、売上は{top['revenue']:,}円です。"
            elif "order_count" in top:
                answer += f"で、注文数は{top['order_count']:,}件です。"

        answer += f"\n\n合計{len(data)}件のデータが見つかりました。"

        return answer

    def _generate_chart(
        self,
        question: str,
        data: list[dict[str, Any]],
        columns: list[str],
    ) -> dict[str, Any]:
        """チャート生成."""
        if not data or len(columns) < 2:
            return None

        # X軸・Y軸を決定
        x_key = columns[0]
        y_key = columns[1] if len(columns) > 1 else columns[0]

        labels = [str(d.get(x_key, "")) for d in data[:10]]
        values = [d.get(y_key, 0) for d in data[:10]]

        return {
            "type": "bar",
            "title": question[:50],
            "xAxis": {"type": "category", "data": labels},
            "yAxis": {"type": "value"},
            "series": [{
                "type": "bar",
                "data": values,
            }],
        }

    def _build_evidence_chain(
        self,
        question: str,
        sql: str,
        resolved: ResolvedQuery,
        data: list[dict[str, Any]],
    ) -> EvidenceChain:
        """証拠チェーン構築."""
        evidence = EvidenceChain()

        # データソース
        for metric in resolved.metrics:
            evidence.data_sources.append({
                "table": metric.table,
                "description": metric.description,
            })

        # クエリ条件
        if resolved.time_range:
            evidence.query_conditions.append(
                f"期間: {resolved.time_range.get('start', '')} 〜 {resolved.time_range.get('end', '')}"
            )

        # 前提条件
        for metric in resolved.metrics:
            if metric.scope_note:
                evidence.assumptions.append(
                    f"{metric.name}定義: {metric.scope_note}"
                )

        # 制限事項
        evidence.limitations.append(
            f"データ取得上限: {self._config.guardrails.auto_limit}件"
        )
        evidence.limitations.append(
            f"データ更新: {datetime.now().strftime('%Y-%m-%d %H:%M')}時点"
        )

        # 代替案
        if self._config.include_alternatives:
            evidence.alternatives.extend([
                "GMVベースで見る場合は「GMVでTOP10」と質問してください",
                "カテゴリ別内訳は「カテゴリ別売上」で確認できます",
            ])

        return evidence

    def _calculate_confidence(
        self,
        resolved: ResolvedQuery,
        data: list[dict[str, Any]],
    ) -> float:
        """信頼度計算."""
        score = resolved.confidence

        # データがない場合
        if not data:
            score *= 0.5

        # 未解決用語がある場合
        if resolved.unresolved_terms:
            score *= 0.8

        return min(score, 1.0)

    def _get_uncertainty_level(self, confidence: float) -> str:
        """不確実性レベルを取得."""
        if confidence >= 0.8:
            return "low"
        elif confidence >= 0.5:
            return "medium"
        return "high"


__all__ = [
    "AnalyticsAgent",
    "AnalyticsConfig",
    "AnalyticsResponse",
    "SQLGuardrails",
    "EvidenceChain",
]
