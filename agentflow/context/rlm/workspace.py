"""Workspace - RLM中間結果の可変空間.

RLMコントローラーが抽出・生成した中間結果を保存する可変空間。
ワークスペースはLLMのスクラッチパッドとして機能し、
最終回答合成時に参照される。

設計原則:
- 容量制限: Token予算と変数数の上限を設定
- 自動退避: 容量超過時は古い非重要変数を自動削除
- 型付け: 変数タイプで重要度と用途を明示

使用例:
    >>> workspace = Workspace(capacity=50, token_budget=10000)
    >>> workspace.set("summary_1", "ドキュメントの要約...", var_type="summary")
    >>> workspace.set("answer", "最終回答...", var_type="final")
    >>> context_str = workspace.to_context_string()
"""

from __future__ import annotations

import re
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import Any


class VariableType(str, Enum):
    """ワークスペース変数タイプ."""

    # 高優先度（削除されにくい）
    FINAL = "final"  # 最終結果
    EVIDENCE = "evidence"  # 根拠・エビデンス
    KEY_FACT = "key_fact"  # 重要事実

    # 中優先度
    SUMMARY = "summary"  # 要約
    EXTRACT = "extract"  # 抽出情報
    SEARCH_RESULT = "search_result"  # 検索結果

    # 低優先度（削除されやすい）
    TEMP = "temp"  # 一時的
    DEBUG = "debug"  # デバッグ用


# 優先度マッピング（高い方が削除されにくい）
_TYPE_PRIORITY: dict[VariableType, int] = {
    VariableType.FINAL: 100,
    VariableType.EVIDENCE: 90,
    VariableType.KEY_FACT: 80,
    VariableType.SUMMARY: 60,
    VariableType.EXTRACT: 50,
    VariableType.SEARCH_RESULT: 40,
    VariableType.TEMP: 20,
    VariableType.DEBUG: 10,
}


@dataclass
class WorkspaceVariable:
    """ワークスペース変数.

    Attributes:
        name: 変数名
        value: 値
        var_type: 変数タイプ
        tokens: 推定Token数
        created_at: 作成タイムスタンプ
        accessed_at: 最終アクセスタイムスタンプ
        access_count: アクセス回数
        metadata: 追加メタデータ
    """

    name: str
    value: Any
    var_type: VariableType = VariableType.TEMP
    tokens: int = 0
    created_at: float = field(default_factory=time.time)
    accessed_at: float = field(default_factory=time.time)
    access_count: int = 0
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def priority(self) -> int:
        """優先度を取得（高いほど削除されにくい）."""
        return _TYPE_PRIORITY.get(self.var_type, 0)

    def eviction_score(self, current_time: float) -> float:
        """退避スコアを計算（低いほど退避されやすい）.

        スコア = 優先度 * 0.5 + アクセス頻度 * 0.3 + 新しさ * 0.2

        Args:
            current_time: 現在時刻

        Returns:
            退避スコア（0-100）
        """
        # 優先度スコア（0-50）
        priority_score = self.priority * 0.5

        # アクセス頻度スコア（0-30）
        # 最大10回で30点
        frequency_score = min(self.access_count / 10, 1.0) * 30

        # 新しさスコア（0-20）
        # 1時間以内なら20点、24時間で0点
        age_hours = (current_time - self.accessed_at) / 3600
        freshness_score = max(0, 1 - age_hours / 24) * 20

        return priority_score + frequency_score + freshness_score


class Workspace:
    """RLMワークスペース.

    中間結果を保存する可変空間。容量制限と自動退避機能を持つ。

    使用例:
        >>> ws = Workspace(capacity=50, token_budget=10000)
        >>> ws.set("key1", "value1", var_type="summary")
        >>> value = ws.get("key1")
        >>> print(ws.to_context_string())
    """

    def __init__(
        self,
        capacity: int = 50,
        token_budget: int = 10_000,
    ) -> None:
        """初期化.

        Args:
            capacity: 最大変数数
            token_budget: Token予算
        """
        self._capacity = capacity
        self._token_budget = token_budget
        self._variables: dict[str, WorkspaceVariable] = {}
        self._total_tokens = 0

    def _estimate_tokens(self, value: Any) -> int:
        """値のToken数を推定.

        Args:
            value: 値

        Returns:
            推定Token数
        """
        text = str(value)
        if not text:
            return 0

        # CJK文字をカウント
        cjk_chars = len(re.findall(r"[\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff]", text))
        # ASCII文字をカウント
        ascii_chars = len(re.findall(r"[a-zA-Z0-9\s]", text))
        # その他
        other_chars = len(text) - cjk_chars - ascii_chars

        return int(cjk_chars * 1.5 + ascii_chars * 0.25 + other_chars)

    def _evict_if_needed(self) -> list[str]:
        """必要に応じて変数を退避.

        Returns:
            退避された変数名リスト
        """
        evicted: list[str] = []
        current_time = time.time()

        # 容量チェック
        while len(self._variables) >= self._capacity:
            # 最も退避スコアが低い変数を探す
            # ただしFINALタイプは除外
            candidates = [
                (name, var)
                for name, var in self._variables.items()
                if var.var_type != VariableType.FINAL
            ]

            if not candidates:
                break  # FINALしか残っていない

            # 最低スコアの変数を退避
            min_var = min(candidates, key=lambda x: x[1].eviction_score(current_time))
            self._delete_variable(min_var[0])
            evicted.append(min_var[0])

        # Token予算チェック
        while self._total_tokens > self._token_budget:
            candidates = [
                (name, var)
                for name, var in self._variables.items()
                if var.var_type != VariableType.FINAL
            ]

            if not candidates:
                break

            min_var = min(candidates, key=lambda x: x[1].eviction_score(current_time))
            self._delete_variable(min_var[0])
            evicted.append(min_var[0])

        return evicted

    def _delete_variable(self, name: str) -> None:
        """変数を削除.

        Args:
            name: 変数名
        """
        if name in self._variables:
            var = self._variables[name]
            self._total_tokens -= var.tokens
            del self._variables[name]

    def set(
        self,
        name: str,
        value: Any,
        var_type: VariableType | str = VariableType.TEMP,
        metadata: dict[str, Any] | None = None,
    ) -> list[str]:
        """変数を設定.

        Args:
            name: 変数名
            value: 値
            var_type: 変数タイプ
            metadata: メタデータ

        Returns:
            退避された変数名リスト
        """
        # 文字列をEnumに変換
        if isinstance(var_type, str):
            var_type = VariableType(var_type)

        tokens = self._estimate_tokens(value)

        # 既存変数の更新
        if name in self._variables:
            old_tokens = self._variables[name].tokens
            self._total_tokens -= old_tokens

        # 新規変数を作成
        var = WorkspaceVariable(
            name=name,
            value=value,
            var_type=var_type,
            tokens=tokens,
            metadata=metadata or {},
        )

        self._variables[name] = var
        self._total_tokens += tokens

        # 退避チェック
        return self._evict_if_needed()

    def get(self, name: str, default: Any = None) -> Any:
        """変数を取得.

        Args:
            name: 変数名
            default: デフォルト値

        Returns:
            変数値
        """
        if name not in self._variables:
            return default

        var = self._variables[name]
        var.accessed_at = time.time()
        var.access_count += 1

        return var.value

    def get_variable(self, name: str) -> WorkspaceVariable | None:
        """変数オブジェクトを取得.

        Args:
            name: 変数名

        Returns:
            WorkspaceVariable、または None
        """
        return self._variables.get(name)

    def has(self, name: str) -> bool:
        """変数が存在するかチェック.

        Args:
            name: 変数名

        Returns:
            存在すればTrue
        """
        return name in self._variables

    def delete(self, name: str) -> bool:
        """変数を削除.

        Args:
            name: 変数名

        Returns:
            削除成功ならTrue
        """
        if name not in self._variables:
            return False

        self._delete_variable(name)
        return True

    def list_variables(
        self,
        var_type: VariableType | None = None,
    ) -> list[WorkspaceVariable]:
        """変数リストを取得.

        Args:
            var_type: フィルタするタイプ（Noneなら全て）

        Returns:
            変数リスト
        """
        if var_type is None:
            return list(self._variables.values())

        return [var for var in self._variables.values() if var.var_type == var_type]

    def list_names(self) -> list[str]:
        """変数名リストを取得.

        Returns:
            変数名リスト
        """
        return list(self._variables.keys())

    def to_context_string(
        self,
        max_tokens: int | None = None,
        include_types: list[VariableType] | None = None,
        exclude_types: list[VariableType] | None = None,
    ) -> str:
        """LLMコンテキスト用の文字列を生成.

        Args:
            max_tokens: 最大Token数（Noneなら制限なし）
            include_types: 含めるタイプ（Noneなら全て）
            exclude_types: 除外するタイプ

        Returns:
            コンテキスト文字列
        """
        lines = ["# Workspace Variables"]

        # フィルタリング
        variables = list(self._variables.values())

        if include_types is not None:
            variables = [v for v in variables if v.var_type in include_types]

        if exclude_types is not None:
            variables = [v for v in variables if v.var_type not in exclude_types]

        # 優先度でソート（高い順）
        variables.sort(key=lambda v: v.priority, reverse=True)

        # Token制限チェック
        current_tokens = self._estimate_tokens("# Workspace Variables\n")
        included_vars: list[WorkspaceVariable] = []

        for var in variables:
            var_str = f"\n## {var.name} ({var.var_type.value})\n{var.value}"
            var_tokens = self._estimate_tokens(var_str)

            if max_tokens is not None and current_tokens + var_tokens > max_tokens:
                continue

            current_tokens += var_tokens
            included_vars.append(var)

        # 文字列生成
        for var in included_vars:
            lines.append(f"\n## {var.name} ({var.var_type.value})")
            lines.append(str(var.value))

        if not included_vars:
            lines.append("\n(No variables)")

        return "\n".join(lines)

    def get_final_answer(self) -> Any:
        """最終回答を取得.

        FINAL タイプの変数を探して返す。

        Returns:
            最終回答、または None
        """
        final_vars = self.list_variables(VariableType.FINAL)
        if final_vars:
            # 最新のFINAL変数を返す
            final_vars.sort(key=lambda v: v.created_at, reverse=True)
            return final_vars[0].value
        return None

    def get_evidence(self) -> list[Any]:
        """エビデンスを取得.

        EVIDENCE タイプの変数を全て返す。

        Returns:
            エビデンスリスト
        """
        return [var.value for var in self.list_variables(VariableType.EVIDENCE)]

    def clear(self) -> None:
        """全変数をクリア."""
        self._variables.clear()
        self._total_tokens = 0

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報辞書
        """
        type_counts: dict[str, int] = {}
        for var in self._variables.values():
            type_name = var.var_type.value
            type_counts[type_name] = type_counts.get(type_name, 0) + 1

        return {
            "variable_count": len(self._variables),
            "total_tokens": self._total_tokens,
            "capacity": self._capacity,
            "token_budget": self._token_budget,
            "remaining_capacity": self._capacity - len(self._variables),
            "remaining_token_budget": self._token_budget - self._total_tokens,
            "by_type": type_counts,
        }

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換.

        Returns:
            ワークスペース状態の辞書
        """
        return {
            "variables": {
                name: {
                    "value": var.value,
                    "type": var.var_type.value,
                    "tokens": var.tokens,
                    "created_at": var.created_at,
                    "accessed_at": var.accessed_at,
                    "access_count": var.access_count,
                }
                for name, var in self._variables.items()
            },
            "stats": self.get_stats(),
        }
