# -*- coding: utf-8 -*-
"""RAG 配置スキーマ.

各 Agent が使用する RAG 知識ベースの設定を管理。
"""

from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class RAGSourceType(str, Enum):
    """RAG ソースタイプ."""

    DIRECTORY = "directory"  # ローカルディレクトリ
    URL = "url"  # Webページ
    DATABASE = "database"  # データベース
    API = "api"  # 外部API


class RAGSource(BaseModel):
    """RAG 知識ベースソース."""

    id: str = Field(..., description="ソースID")
    name: str = Field(..., description="ソース名")
    type: RAGSourceType = Field(..., description="ソースタイプ")
    path: str = Field(..., description="パス/URL")
    description: str = Field(default="", description="説明")
    enabled: bool = Field(default=True, description="有効フラグ")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタデータ")


class AgentRAGBinding(BaseModel):
    """Agent-RAG バインディング."""

    agent_id: str = Field(..., description="Agent ID")
    agent_name: str = Field(..., description="Agent 表示名")
    rag_source_ids: list[str] = Field(default_factory=list, description="バインドされた RAG ソース ID リスト")
    enabled: bool = Field(default=True, description="RAG 使用フラグ")


class RAGConfig(BaseModel):
    """RAG 設定全体."""

    sources: list[RAGSource] = Field(default_factory=list, description="RAG ソースリスト")
    bindings: list[AgentRAGBinding] = Field(default_factory=list, description="Agent-RAG バインディング")


# デフォルト設定
DEFAULT_RAG_SOURCES: list[RAGSource] = [
    RAGSource(
        id="industry_practices",
        name="業界プラクティス",
        type=RAGSourceType.DIRECTORY,
        path="./knowledge/industry_practices",
        description="業界標準・ベストプラクティス集",
    ),
    RAGSource(
        id="tech_docs",
        name="技術ドキュメント",
        type=RAGSourceType.DIRECTORY,
        path="./knowledge/tech_docs",
        description="技術仕様・API ドキュメント",
    ),
    RAGSource(
        id="case_studies",
        name="事例データベース",
        type=RAGSourceType.DIRECTORY,
        path="./knowledge/case_studies",
        description="過去の意思決定事例",
    ),
    RAGSource(
        id="compliance",
        name="コンプライアンス",
        type=RAGSourceType.DIRECTORY,
        path="./knowledge/compliance",
        description="法規制・コンプライアンス情報",
    ),
]

DEFAULT_AGENT_BINDINGS: list[AgentRAGBinding] = [
    AgentRAGBinding(
        agent_id="cognitive_gate",
        agent_name="認知（認知前処理）",
        rag_source_ids=[],  # RAG不使用
        enabled=False,
    ),
    AgentRAGBinding(
        agent_id="gatekeeper",
        agent_name="門番（入口検証）",
        rag_source_ids=[],  # RAG不使用
        enabled=False,
    ),
    AgentRAGBinding(
        agent_id="clarification",
        agent_name="診断（問題診断）",
        rag_source_ids=[],  # RAG不使用
        enabled=False,
    ),
    AgentRAGBinding(
        agent_id="dao",
        agent_name="道（本質分析）",
        rag_source_ids=["case_studies"],
        enabled=True,
    ),
    AgentRAGBinding(
        agent_id="fa",
        agent_name="法（戦略選定）",
        rag_source_ids=["industry_practices", "case_studies"],
        enabled=True,
    ),
    AgentRAGBinding(
        agent_id="shu",
        agent_name="術（実行計画）",
        rag_source_ids=["industry_practices"],
        enabled=True,
    ),
    AgentRAGBinding(
        agent_id="qi",
        agent_name="器（技術実装）",
        rag_source_ids=["tech_docs"],
        enabled=True,
    ),
    AgentRAGBinding(
        agent_id="review",
        agent_name="検証（最終検証）",
        rag_source_ids=["compliance"],
        enabled=True,
    ),
]


def get_default_config() -> RAGConfig:
    """デフォルト RAG 設定を取得."""
    return RAGConfig(
        sources=DEFAULT_RAG_SOURCES,
        bindings=DEFAULT_AGENT_BINDINGS,
    )

