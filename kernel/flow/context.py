"""Flow実行コンテキスト.

フロー実行中のすべての状態を管理：入力、結果、メモリ、リビジョンカウント等。

設計原則:
- 型安全：Pydantic BaseModelによる型強制
- 単一データソース：すべての状態を一元管理
- シリアライズ可能：デバッグと永続化に便利
- 明確なライフサイクル：リセットとクリーンアップをサポート
- 後方互換：既存コードが壊れないよう既存APIを維持
"""

from __future__ import annotations

import copy
import logging
import uuid
from datetime import UTC, datetime
from typing import Any

from pydantic import BaseModel, ConfigDict, Field


class FlowMetadata(BaseModel):
    """フロー実行メタデータ."""

    flow_id: str = ""
    run_id: str = ""
    started_at: datetime = Field(
        default_factory=lambda: datetime.now(UTC),
    )
    current_node: str = ""


class FlowContext(BaseModel):
    """フロー実行コンテキスト（型安全なPydantic model）.

    Example:
        >>> ctx = FlowContext()
        >>> ctx.set_inputs({"question": "..."})
        >>> ctx.set_result("agent1", {"answer": "..."})
        >>> ctx.get_result("agent1")
        {'answer': '...'}

        >>> # 新しいAPI
        >>> ctx = FlowContext(inputs={"q": "hello"})
        >>> ctx.set_output("agent1", {"answer": "world"})
        >>> ctx.get_output("agent1")
        {'answer': 'world'}
    """

    model_config = ConfigDict(
        extra="allow",
        arbitrary_types_allowed=True,
        populate_by_name=True,
    )

    # --- 公開フィールド ---
    inputs: dict[str, Any] = Field(default_factory=dict)
    agent_outputs: dict[str, Any] = Field(default_factory=dict)
    metadata: FlowMetadata = Field(default_factory=FlowMetadata)
    shared: dict[str, Any] = Field(default_factory=dict)

    # --- 型付き State スキーマ（オプション） ---
    # FlowBuilder(state_schema=MyModel) で指定すると、set_result 時に検証
    state_schema_: type[Any] | None = Field(default=None, exclude=True)

    # --- 内部フィールド（後方互換のため維持） ---
    flow_id: str = ""
    results_: dict[str, dict[str, Any]] = Field(default_factory=dict)
    memory_: dict[str, Any] = Field(default_factory=dict)
    revision_count_: int = 0
    current_node_: str | None = None
    completed_nodes_: list[str] = Field(default_factory=list)

    def __init__(self, flow_id: str | None = None, /, **kwargs: Any) -> None:
        """初期化（後方互換: 位置引数でflow_idを受け取る）.

        Args:
            flow_id: フローID（位置引数、省略可能）
            **kwargs: Pydanticフィールド
        """
        if flow_id is not None:
            kwargs.setdefault("flow_id", flow_id)
        super().__init__(**kwargs)

    def model_post_init(self, _context: Any) -> None:
        """初期化後処理."""
        self._logger = logging.getLogger("kernel.flow.context")
        # flow_idが未設定の場合は自動生成
        if not self.flow_id:
            self.flow_id = f"flow-{uuid.uuid4().hex[:8]}"
        # metadataのflow_idを同期
        if not self.metadata.flow_id:
            self.metadata.flow_id = self.flow_id

    # ========================================
    # 入力管理
    # ========================================

    def set_inputs(self, inputs: dict[str, Any]) -> None:
        """入力を設定."""
        self.inputs = copy.deepcopy(inputs)

    def get_inputs(self) -> dict[str, Any]:
        """入力のコピーを取得."""
        return copy.deepcopy(self.inputs)

    def get_input(self, key: str, default: Any = None) -> Any:
        """特定の入力値を取得."""
        return self.inputs.get(key, default)

    # ========================================
    # 結果管理（既存API）
    # ========================================

    def set_result(self, node_id: str, result: dict[str, Any]) -> None:
        """ノード結果を保存.

        state_schema_ が設定されている場合、結果を検証する。
        """
        # 型付き State 検証（オプション）
        if self.state_schema_ is not None:
            try:
                if hasattr(self.state_schema_, "model_validate"):
                    # Pydantic v2: 部分検証（結果キーがスキーマフィールドに存在するか）
                    schema_fields = set(self.state_schema_.model_fields.keys())
                    unknown_keys = set(result.keys()) - schema_fields
                    if unknown_keys:
                        self._logger.warning(
                            "state_schema にないキー: %s (node: %s)", unknown_keys, node_id,
                        )
            except Exception as e:
                self._logger.warning("state_schema 検証エラー: %s (node: %s)", e, node_id)

        deep = copy.deepcopy(result)
        self.results_[node_id] = deep
        # agent_outputsにも同期
        self.agent_outputs[node_id] = deep
        if node_id not in self.completed_nodes_:
            self.completed_nodes_.append(node_id)

    def get_result(
        self,
        node_id: str,
        default: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """ノード結果を取得."""
        result = self.results_.get(node_id)
        return copy.deepcopy(result) if result else (default or {})

    def has_result(self, node_id: str) -> bool:
        """結果があるかチェック."""
        return node_id in self.results_

    def get_all_results(self) -> dict[str, dict[str, Any]]:
        """すべての結果を取得."""
        return copy.deepcopy(self.results_)

    # ========================================
    # 出力管理（新API）
    # ========================================

    def set_output(self, agent_name: str, output: Any) -> None:
        """Agent出力を保存（set_resultのエイリアス）."""
        data = output if isinstance(output, dict) else {"value": output}
        self.set_result(agent_name, data)

    def get_output(self, agent_name: str, default: Any = None) -> Any:
        """Agent出力を取得."""
        result = self.agent_outputs.get(agent_name)
        if result is None:
            return default
        return copy.deepcopy(result)

    # ========================================
    # メモリシステム（後方互換）
    # ========================================

    def set(self, key: str, value: Any) -> None:
        """カスタムデータを保存（sharedにも同期）."""
        self.memory_[key] = value
        self.shared[key] = value

    def get(self, key: str, default: Any = None) -> Any:
        """カスタムデータを取得."""
        return self.memory_.get(key, default)

    def remove(self, key: str) -> None:
        """カスタムデータを削除."""
        self.memory_.pop(key, None)
        self.shared.pop(key, None)

    # ========================================
    # リビジョン管理
    # ========================================

    @property
    def revision_count(self) -> int:
        """現在のリビジョン回数."""
        return self.revision_count_

    def increment_revision(self) -> int:
        """リビジョンカウントを増加."""
        self.revision_count_ += 1
        return self.revision_count_

    def clear_results_from(self, node_id: str) -> None:
        """指定ノードから開始する結果をクリア（REVISEロールバック用）."""
        if node_id not in self.completed_nodes_:
            return
        idx = self.completed_nodes_.index(node_id)
        for nid in self.completed_nodes_[idx:]:
            self.results_.pop(nid, None)
            self.agent_outputs.pop(nid, None)
        self.completed_nodes_ = self.completed_nodes_[:idx]
        self._logger.info(f"{node_id}から開始する結果をクリアしました")

    # ========================================
    # 状態
    # ========================================

    @property
    def current_node(self) -> str | None:
        """現在のノード."""
        return self.current_node_

    def set_current_node(self, node_id: str | None) -> None:
        """現在のノードを設定."""
        self.current_node_ = node_id
        if node_id:
            self.metadata.current_node = node_id

    @property
    def completed_nodes(self) -> list[str]:
        """完了したノードリスト."""
        return self.completed_nodes_.copy()

    def clear(self) -> None:
        """すべての状態をクリア."""
        self.inputs.clear()
        self.results_.clear()
        self.agent_outputs.clear()
        self.memory_.clear()
        self.shared.clear()
        self.revision_count_ = 0
        self.current_node_ = None
        self.completed_nodes_.clear()

    def to_dict(self) -> dict[str, Any]:
        """シリアライズ（デバッグ用）."""
        return {
            "flow_id": self.flow_id,
            "inputs": self.inputs,
            "results": self.results_,
            "agent_outputs": self.agent_outputs,
            "memory": self.memory_,
            "shared": self.shared,
            "metadata": self.metadata.model_dump(),
            "revision_count": self.revision_count_,
            "completed_nodes": self.completed_nodes_,
        }

    # ========================================
    # dict風アクセス（後方互換）
    # ========================================

    def __getitem__(self, key: str) -> Any:
        """dict風の読み取りアクセス."""
        if key in self.shared:
            return self.shared[key]
        if key in self.memory_:
            return self.memory_[key]
        if key in self.agent_outputs:
            return self.agent_outputs[key]
        if key in self.inputs:
            return self.inputs[key]
        msg = f"KeyError: {key!r}"
        raise KeyError(msg)

    def __setitem__(self, key: str, value: Any) -> None:
        """dict風の書き込みアクセス."""
        self.set(key, value)

    def __contains__(self, key: object) -> bool:
        """dict風のin演算子."""
        if not isinstance(key, str):
            return False
        return key in self.shared or key in self.memory_ or key in self.agent_outputs or key in self.inputs

    def __repr__(self) -> str:
        return f"FlowContext({self.flow_id!r}, completed={len(self.completed_nodes_)})"


class _ReadOnlyDict(dict[str, Any]):
    """読み取り専用の dict ラッパー."""

    def __setitem__(self, key: str, value: Any) -> None:
        msg = "ScopedContextView: 他の Agent の出力は読み取り専用です"
        raise TypeError(msg)

    def __delitem__(self, key: str) -> None:
        msg = "ScopedContextView: 他の Agent の出力は読み取り専用です"
        raise TypeError(msg)

    def pop(self, *args: Any, **kwargs: Any) -> Any:
        msg = "ScopedContextView: 他の Agent の出力は読み取り専用です"
        raise TypeError(msg)

    def update(self, *args: Any, **kwargs: Any) -> None:
        msg = "ScopedContextView: 他の Agent の出力は読み取り専用です"
        raise TypeError(msg)

    def clear(self) -> None:
        msg = "ScopedContextView: 他の Agent の出力は読み取り専用です"
        raise TypeError(msg)


class ScopedContextView:
    """Agent ごとのスコープ付きコンテキストビュー.

    自 Agent の名前空間のみ書き込み可能。
    他 Agent の出力は読み取り専用で参照可能。

    Example:
        >>> view = ScopedContextView(ctx, "agent_a")
        >>> view.set_output({"answer": "hello"})  # 自分の出力を設定
        >>> view.get_output("agent_b")              # 他 Agent の出力を取得（読み取り専用）
        >>> view.get_peer_output("agent_b")         # 同上
    """

    def __init__(self, ctx: FlowContext, agent_id: str) -> None:
        """初期化.

        Args:
            ctx: 元の FlowContext
            agent_id: このビューが所属する Agent の ID
        """
        self._ctx = ctx
        self._agent_id = agent_id

    @property
    def agent_id(self) -> str:
        """このビューの Agent ID."""
        return self._agent_id

    @property
    def inputs(self) -> dict[str, Any]:
        """フロー入力（読み取り専用コピー）."""
        return self._ctx.get_inputs()

    @property
    def flow_id(self) -> str:
        """フロー ID."""
        return self._ctx.flow_id

    def set_output(self, data: dict[str, Any]) -> None:
        """自 Agent の出力を設定."""
        self._ctx.set_result(self._agent_id, data)

    def get_my_output(self) -> dict[str, Any]:
        """自 Agent の出力を取得."""
        return self._ctx.get_result(self._agent_id)

    def get_peer_output(
        self, agent_id: str, default: Any = None
    ) -> _ReadOnlyDict | Any:
        """他 Agent の出力を読み取り専用で取得.

        Args:
            agent_id: 参照先の Agent ID
            default: 結果が存在しない場合のデフォルト値
        """
        result = self._ctx.get_result(agent_id)
        if not result:
            return default
        return _ReadOnlyDict(result)

    def get_output(self, agent_id: str, default: Any = None) -> Any:
        """任意の Agent の出力を取得.

        自 Agent の場合は通常の dict、他 Agent の場合は読み取り専用。
        """
        if agent_id == self._agent_id:
            return self.get_my_output()
        return self.get_peer_output(agent_id, default)

    def get(self, key: str, default: Any = None) -> Any:
        """共有メモリから値を取得."""
        return self._ctx.get(key, default)

    def set(self, key: str, value: Any) -> None:
        """共有メモリに値を設定."""
        self._ctx.set(key, value)

    @property
    def completed_nodes(self) -> list[str]:
        """完了したノードリスト."""
        return self._ctx.completed_nodes

    def __repr__(self) -> str:
        return f"ScopedContextView({self._agent_id!r}, flow={self._ctx.flow_id!r})"


__all__ = ["FlowContext", "FlowMetadata", "ScopedContextView"]
