# -*- coding: utf-8 -*-
"""ReportBuilder - パイプライン結果レポート生成インターフェース.

PipelineEngine の最終出力を標準化するための抽象基底クラス。
各アプリケーションは独自の ReportBuilder を実装し、
結果のマージ・要約生成・ID付与などを行う。

使用例:
    >>> from agentflow.engines.report_builder import ReportBuilder
    >>>
    >>> class MyReportBuilder(ReportBuilder):
    ...     def build(self, results: dict, **kwargs) -> dict:
    ...         return {
    ...             "report_id": self.generate_id(),
    ...             "summary": self.generate_summary(results),
    ...             "results": results,
    ...         }
    >>>
    >>> engine = PipelineEngine(
    ...     stages=[...],
    ...     report_builder=MyReportBuilder(),
    ... )
"""

from __future__ import annotations

import hashlib
import uuid
from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any


class ReportBuilder(ABC):
    """レポート生成の抽象基底クラス.
    
    パイプライン実行結果から最終レポートを生成する。
    各アプリケーションは build() を実装してカスタマイズ。
    
    Attributes:
        id_prefix: レポートID接頭辞（例: "PROP-", "RPT-"）
    """
    
    id_prefix: str = "RPT-"
    
    @abstractmethod
    def build(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """結果からレポートを生成.
        
        Args:
            results: 各ステージの実行結果 {stage_name: result}
            inputs: 元の入力データ
            **kwargs: 追加パラメータ
            
        Returns:
            JSON シリアライズ可能なレポート辞書
        """
        pass
    
    def generate_id(self, prefix: str | None = None) -> str:
        """レポートIDを生成.
        
        Args:
            prefix: ID接頭辞（省略時は self.id_prefix）
            
        Returns:
            一意のレポートID（例: "RPT-202601-ABC123"）
        """
        prefix = prefix or self.id_prefix
        date_part = datetime.now().strftime("%Y%m")
        unique_part = uuid.uuid4().hex[:6].upper()
        return f"{prefix}{date_part}-{unique_part}"
    
    def generate_summary(
        self,
        results: dict[str, Any],
        max_length: int = 200,
    ) -> str:
        """結果から要約を生成.
        
        Args:
            results: 実行結果
            max_length: 要約最大文字数
            
        Returns:
            要約文字列
        """
        # デフォルト実装: キーのリスト
        stages = list(results.keys())
        summary = f"Completed stages: {', '.join(stages)}"
        return summary[:max_length]
    
    def to_json_serializable(self, data: Any) -> Any:
        """データをJSONシリアライズ可能な形式に変換.
        
        datetime、Pydantic モデル等を処理。
        
        Args:
            data: 変換対象データ
            
        Returns:
            JSONシリアライズ可能なデータ
        """
        if hasattr(data, "model_dump"):
            return data.model_dump(mode="json")
        elif isinstance(data, datetime):
            return data.isoformat()
        elif isinstance(data, dict):
            return {k: self.to_json_serializable(v) for k, v in data.items()}
        elif isinstance(data, list):
            return [self.to_json_serializable(item) for item in data]
        return data


class SimpleReportBuilder(ReportBuilder):
    """シンプルなレポートビルダー.
    
    基本的なレポート構造を生成。
    カスタマイズ不要な場合のデフォルト実装。
    """
    
    def build(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """結果からシンプルなレポートを生成."""
        return {
            "report_id": self.generate_id(),
            "created_at": datetime.now().isoformat(),
            "status": "success",
            "summary": self.generate_summary(results),
            "inputs": self.to_json_serializable(inputs) if inputs else {},
            "results": self.to_json_serializable(results),
        }


__all__ = ["ReportBuilder", "SimpleReportBuilder"]

