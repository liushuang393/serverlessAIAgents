"""Core Interface Types - 核心接口の型定義.

全てのインターフェースで使用するデータ型を定義します。
このファイルは安定しており、変更は慎重に行う必要があります。
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Literal


# =============================================================================
# Enums
# =============================================================================


class CodeOutputType(str, Enum):
    """コード出力タイプ."""

    FRONTEND = "frontend"      # React/Vue フロントエンド
    BACKEND = "backend"        # FastAPI バックエンド
    FULLSTACK = "fullstack"    # 前後端完全アプリ


class DeployTarget(str, Enum):
    """デプロイターゲット."""

    VERCEL = "vercel"
    AWS_LAMBDA = "aws_lambda"
    DOCKER = "docker"
    DOCKER_COMPOSE = "docker_compose"
    GITHUB_ACTIONS = "github_actions"
    CLOUDFLARE = "cloudflare"


# =============================================================================
# Workflow Definitions (Studio で保存するフォーマット)
# =============================================================================


@dataclass
class NodeDefinition:
    """ノード定義.

    Attributes:
        id: ノード ID
        type: ノードタイプ（agent/gate/review/parallel）
        agent_type: エージェントタイプ名
        config: ノード設定
        position: UI 位置
    """

    id: str
    type: str = "agent"
    agent_type: str = ""
    config: dict[str, Any] = field(default_factory=dict)
    position: dict[str, float] = field(default_factory=dict)


@dataclass
class EdgeDefinition:
    """エッジ定義.

    Attributes:
        id: エッジ ID
        source: ソースノード ID
        target: ターゲットノード ID
        label: エッジラベル
    """

    id: str
    source: str
    target: str
    label: str = ""


@dataclass
class WorkflowDefinition:
    """ワークフロー定義.

    Studio が保存するフォーマット。全ての操作はこの型を使用。

    Attributes:
        id: ワークフロー ID
        name: ワークフロー名
        description: 説明
        nodes: ノードリスト
        edges: エッジリスト
        metadata: メタデータ
    """

    id: str
    name: str
    description: str = ""
    nodes: list[NodeDefinition] = field(default_factory=list)
    edges: list[EdgeDefinition] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> WorkflowDefinition:
        """辞書から作成."""
        nodes = [
            NodeDefinition(
                id=n.get("id", ""),
                type=n.get("type", "agent"),
                agent_type=n.get("data", {}).get("agentType", ""),
                config=n.get("data", {}).get("config", {}),
                position=n.get("position", {}),
            )
            for n in data.get("nodes", [])
        ]
        edges = [
            EdgeDefinition(
                id=e.get("id", ""),
                source=e.get("source", ""),
                target=e.get("target", ""),
                label=e.get("label", ""),
            )
            for e in data.get("edges", [])
        ]
        return cls(
            id=data.get("id", ""),
            name=data.get("name", ""),
            description=data.get("description", ""),
            nodes=nodes,
            edges=edges,
            metadata=data.get("metadata", {}),
        )

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "name": self.name,
            "description": self.description,
            "nodes": [
                {
                    "id": n.id,
                    "type": n.type,
                    "data": {
                        "agentType": n.agent_type,
                        "config": n.config,
                    },
                    "position": n.position,
                }
                for n in self.nodes
            ],
            "edges": [
                {
                    "id": e.id,
                    "source": e.source,
                    "target": e.target,
                    "label": e.label,
                }
                for e in self.edges
            ],
            "metadata": self.metadata,
        }


# =============================================================================
# Code Generation Types
# =============================================================================


@dataclass
class FilePreview:
    """ファイルプレビュー（ダウンロード前の確認用）.

    Attributes:
        path: ファイルパス
        content_preview: 内容プレビュー（先頭 N 行）
        size: ファイルサイズ（バイト）
        lines: 行数
    """

    path: str
    content_preview: str
    size: int
    lines: int


@dataclass
class CodeGenOptions:
    """コード生成オプション.

    Attributes:
        app_name: アプリケーション名
        version: バージョン
        include_tests: テストコードを含める
        include_readme: README を含める
        include_docker: Docker ファイルを含める
        framework: フレームワーク（react/vue/fastapi など）
    """

    app_name: str = ""
    version: str = "1.0.0"
    include_tests: bool = True
    include_readme: bool = True
    include_docker: bool = False
    framework: str = "fastapi"


@dataclass
class GeneratedCode:
    """生成されたコード.

    Attributes:
        files: ファイルパス -> 内容のマップ
        entry_point: エントリーポイントファイル
        build_command: ビルドコマンド
        start_command: 起動コマンド
        output_type: 出力タイプ
    """

    files: dict[str, str] = field(default_factory=dict)
    entry_point: str = ""
    build_command: str | None = None
    start_command: str | None = None
    output_type: CodeOutputType = CodeOutputType.BACKEND


# =============================================================================
# Deploy Types
# =============================================================================


@dataclass
class DeployConfig:
    """デプロイ設定.

    Attributes:
        target: デプロイターゲット
        credentials: 認証情報（API トークンなど）
        settings: プラットフォーム固有設定
        env_vars: 環境変数
    """

    target: DeployTarget
    credentials: dict[str, str] = field(default_factory=dict)
    settings: dict[str, Any] = field(default_factory=dict)
    env_vars: dict[str, str] = field(default_factory=dict)


@dataclass
class DeployEvent:
    """デプロイイベント（進捗報告用）.

    Attributes:
        type: イベントタイプ
        message: メッセージ
        progress: 進捗（0-100）
        phase: フェーズ名
        data: 追加データ
        timestamp: タイムスタンプ
    """

    type: Literal["progress", "log", "success", "error"]
    message: str
    progress: float | None = None
    phase: str | None = None
    data: dict[str, Any] | None = None
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "type": self.type,
            "message": self.message,
            "progress": self.progress,
            "phase": self.phase,
            "data": self.data,
            "timestamp": self.timestamp.isoformat(),
        }


@dataclass
class DeployResult:
    """デプロイ結果.

    Attributes:
        success: 成功したか
        deployment_id: デプロイメント ID
        url: デプロイ URL
        logs: ログメッセージ
        error: エラーメッセージ
    """

    success: bool
    deployment_id: str | None = None
    url: str | None = None
    logs: list[str] = field(default_factory=list)
    error: str | None = None


# =============================================================================
# Config Types
# =============================================================================


@dataclass
class ConfigField:
    """設定フィールド（UI レンダリング用）.

    Attributes:
        name: フィールド名（キー）
        label: 表示ラベル
        type: 入力タイプ
        required: 必須かどうか
        default: デフォルト値
        options: 選択肢（type=select の場合）
        description: 説明文
        placeholder: プレースホルダー
        group: グループ名（UI でのグルーピング用）
    """

    name: str
    label: str
    type: Literal["string", "password", "select", "boolean", "number", "textarea"]
    required: bool = False
    default: Any | None = None
    options: list[str] | None = None
    description: str = ""
    placeholder: str = ""
    group: str = "general"

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "label": self.label,
            "type": self.type,
            "required": self.required,
            "default": self.default,
            "options": self.options,
            "description": self.description,
            "placeholder": self.placeholder,
            "group": self.group,
        }


@dataclass
class ConfigTemplate:
    """設定テンプレート.

    Attributes:
        target: ターゲットプラットフォーム
        name: テンプレート名
        description: 説明
        fields: 設定フィールド
        defaults: デフォルト値
    """

    target: DeployTarget
    name: str
    description: str = ""
    fields: list[ConfigField] = field(default_factory=list)
    defaults: dict[str, Any] = field(default_factory=dict)


@dataclass
class ValidationResult:
    """検証結果.

    Attributes:
        valid: 有効かどうか
        errors: エラーメッセージ（フィールド名 -> エラー）
        warnings: 警告メッセージ
    """

    valid: bool
    errors: dict[str, str] = field(default_factory=dict)
    warnings: list[str] = field(default_factory=list)


# =============================================================================
# Execution Types (Preview/Debug 用)
# =============================================================================


@dataclass
class ExecutionEvent:
    """実行イベント.

    Attributes:
        type: イベントタイプ
        node_id: ノード ID
        message: メッセージ
        progress: 進捗
        data: データ
        timestamp: タイムスタンプ
    """

    type: Literal["start", "progress", "node_start", "node_complete", "complete", "error"]
    node_id: str | None = None
    message: str = ""
    progress: float | None = None
    data: dict[str, Any] | None = None
    timestamp: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "type": self.type,
            "node_id": self.node_id,
            "message": self.message,
            "progress": self.progress,
            "data": self.data,
            "timestamp": self.timestamp.isoformat(),
        }


@dataclass
class DebugEvent(ExecutionEvent):
    """デバッグイベント.

    Attributes:
        breakpoint_hit: ブレークポイントに到達したか
        variables: 変数状態
        call_stack: コールスタック
    """

    breakpoint_hit: bool = False
    variables: dict[str, Any] = field(default_factory=dict)
    call_stack: list[str] = field(default_factory=list)


# =============================================================================
# Exports
# =============================================================================

__all__ = [
    "CodeGenOptions",
    # Enums
    "CodeOutputType",
    # Config
    "ConfigField",
    "ConfigTemplate",
    "DebugEvent",
    # Deploy
    "DeployConfig",
    "DeployEvent",
    "DeployResult",
    "DeployTarget",
    "EdgeDefinition",
    # Execution
    "ExecutionEvent",
    "FilePreview",
    # Code Generation
    "GeneratedCode",
    "NodeDefinition",
    "ValidationResult",
    # Workflow
    "WorkflowDefinition",
]
