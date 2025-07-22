"""
AI Blocksの共通データモデル

このモジュールは、各コンポーネント間で使用される共通のデータ構造を定義します。
Pydanticを使用してデータバリデーションと型安全性を提供します。
"""

from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional

from pydantic import BaseModel, Field


class MessageRole(str, Enum):
    """メッセージの役割を表す列挙型"""

    USER = "user"  # ユーザーからのメッセージ
    ASSISTANT = "assistant"  # アシスタントからの応答
    SYSTEM = "system"  # システムメッセージ
    TOOL = "tool"  # ツール実行結果


class Message(BaseModel):
    """会話メッセージのデータモデル"""

    role: MessageRole = Field(..., description="メッセージの役割")
    content: str = Field(..., description="メッセージの内容")
    timestamp: datetime = Field(
        default_factory=datetime.now, description="メッセージのタイムスタンプ"
    )
    metadata: Dict[str, Any] = Field(default_factory=dict, description="追加のメタデータ")

    class Config:
        json_encoders = {datetime: lambda v: v.isoformat()}


class MemoryItem(BaseModel):
    """記憶アイテムのデータモデル"""

    id: str = Field(..., description="記憶アイテムの一意識別子")
    content: str = Field(..., description="記憶の内容")
    metadata: Dict[str, Any] = Field(default_factory=dict, description="記憶に関連するメタデータ")
    similarity_score: Optional[float] = Field(None, description="検索時の類似度スコア")
    created_at: datetime = Field(default_factory=datetime.now, description="作成日時")

    class Config:
        json_encoders = {datetime: lambda v: v.isoformat()}


class ToolResult(BaseModel):
    """ツール実行結果のデータモデル"""

    success: bool = Field(..., description="実行が成功したかどうか")
    result: Any = Field(None, description="実行結果")
    error_message: Optional[str] = Field(None, description="エラーメッセージ（失敗時）")
    execution_time: float = Field(..., description="実行時間（秒）")
    metadata: Dict[str, Any] = Field(default_factory=dict, description="追加のメタデータ")


class ToolDefinition(BaseModel):
    """ツール定義のデータモデル"""

    name: str = Field(..., description="ツール名")
    description: str = Field(..., description="ツールの説明")
    parameters: Dict[str, Any] = Field(default_factory=dict, description="パラメータ定義")
    function: Optional[Callable] = Field(None, description="実行する関数")

    class Config:
        arbitrary_types_allowed = True


class ParsedDocument(BaseModel):
    """パース済みドキュメントのデータモデル"""

    text: str = Field(..., description="抽出されたテキスト")
    metadata: Dict[str, Any] = Field(default_factory=dict, description="ドキュメントのメタデータ")
    chunks: Optional[List[str]] = Field(None, description="分割されたテキストチャンク")
    source_type: str = Field(..., description="元のドキュメントタイプ")


class TextChunk(BaseModel):
    """テキストチャンクのデータモデル"""

    text: str = Field(..., description="チャンクのテキスト内容")
    start_index: int = Field(..., description="元テキストでの開始位置")
    end_index: int = Field(..., description="元テキストでの終了位置")
    metadata: Dict[str, Any] = Field(default_factory=dict, description="チャンクのメタデータ")
    chunk_id: Optional[str] = Field(None, description="チャンクの一意識別子")


class RouteResult(BaseModel):
    """ルーティング結果のデータモデル"""

    target: str = Field(..., description="振り分け先のターゲット")
    confidence: float = Field(..., description="判定の信頼度（0.0-1.0）")
    parameters: Dict[str, Any] = Field(
        default_factory=dict, description="ターゲットに渡すパラメータ"
    )
    reasoning: Optional[str] = Field(None, description="判定の理由")


class RouteDefinition(BaseModel):
    """ルート定義のデータモデル"""

    pattern: str = Field(..., description="マッチングパターン")
    target: str = Field(..., description="振り分け先")
    priority: int = Field(default=0, description="優先度（高い値が優先）")
    conditions: Dict[str, Any] = Field(default_factory=dict, description="追加の条件")
    description: str = Field(..., description="ルートの説明")


class EvaluationResult(BaseModel):
    """評価結果のデータモデル"""

    score: float = Field(..., description="総合スコア（0.0-1.0）")
    criteria_scores: Dict[str, float] = Field(
        default_factory=dict, description="各基準のスコア"
    )
    feedback: str = Field(..., description="評価のフィードバック")
    passed: bool = Field(..., description="評価に合格したかどうか")
    suggestions: List[str] = Field(default_factory=list, description="改善提案")


class AgentConfig(BaseModel):
    """エージェント設定のデータモデル"""

    name: str = Field(..., description="エージェント名")
    description: str = Field(..., description="エージェントの説明")
    max_iterations: int = Field(default=3, description="最大反復回数")
    timeout: float = Field(default=30.0, description="タイムアウト時間（秒）")
    parameters: Dict[str, Any] = Field(
        default_factory=dict, description="エージェント固有のパラメータ"
    )


class ChainResult(BaseModel):
    """チェーン実行結果のデータモデル"""

    final_output: str = Field(..., description="最終出力")
    intermediate_results: List[Any] = Field(
        default_factory=list, description="中間結果のリスト"
    )
    execution_time: float = Field(..., description="総実行時間（秒）")
    success: bool = Field(..., description="チェーン実行が成功したかどうか")
