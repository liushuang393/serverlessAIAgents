# -*- coding: utf-8 -*-
"""{{ app_name }} サービス層.

ビジネスロジックを提供する。
"""
from typing import Any


class BaseService:
    """サービス基底クラス.
    
    全サービスはこのクラスを継承する。
    """
    
    def __init__(self) -> None:
        """初期化."""
        pass


# ========================================
# サンプルサービス
# ========================================

class ExampleService(BaseService):
    """サンプルサービス.
    
    サービス層の実装例。
    実際のビジネスロジックに置き換えること。
    """
    
    async def process(self, data: dict[str, Any]) -> dict[str, Any]:
        """データを処理.
        
        Args:
            data: 入力データ
            
        Returns:
            処理結果
        """
        # TODO: 実際のビジネスロジックを実装
        return {
            "status": "processed",
            "input": data,
        }


__all__ = [
    "BaseService",
    "ExampleService",
]

