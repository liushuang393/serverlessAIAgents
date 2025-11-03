"""Calculator Agent - 数式を計算するエージェント."""

from __future__ import annotations

import ast
import operator
from typing import Any

from agentflow.core.agent_block import AgentBlock


class CalculatorAgent(AgentBlock):
    """数式を計算するエージェント.
    
    このエージェントは安全に数式を評価して計算結果を返します。
    eval() を使用せず、AST を使用して安全に評価します。
    """
    
    # サポートされる演算子
    OPERATORS = {
        ast.Add: operator.add,
        ast.Sub: operator.sub,
        ast.Mult: operator.mul,
        ast.Div: operator.truediv,
        ast.FloorDiv: operator.floordiv,
        ast.Mod: operator.mod,
        ast.Pow: operator.pow,
        ast.USub: operator.neg,
        ast.UAdd: operator.pos,
    }
    
    async def initialize(self) -> None:
        """初期化処理."""
        await super().initialize()
        print("🧮 Calculator Agent を初期化しました")
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """数式を計算.
        
        Args:
            input_data: 入力データ
                - expression (str): 計算する数式
        
        Returns:
            計算結果
                - result (float): 計算結果
                - expression (str): 計算した数式
        
        Raises:
            ValueError: expression が指定されていない場合
            SyntaxError: 数式が不正な場合
            TypeError: サポートされていない演算子が使用された場合
        """
        expression = input_data.get("expression")
        if not expression:
            raise ValueError("expression は必須です")
        
        # 数式を評価
        try:
            result = self._evaluate(expression)
        except (SyntaxError, TypeError, ZeroDivisionError) as e:
            raise ValueError(f"数式の評価に失敗しました: {e}") from e
        
        return {
            "result": result,
            "expression": expression,
        }
    
    def _evaluate(self, expression: str) -> float:
        """数式を安全に評価.
        
        Args:
            expression: 数式
        
        Returns:
            計算結果
        
        Raises:
            SyntaxError: 数式が不正な場合
            TypeError: サポートされていない演算子が使用された場合
        """
        # 数式をパース
        node = ast.parse(expression, mode="eval").body
        
        # AST を評価
        return self._eval_node(node)
    
    def _eval_node(self, node: ast.AST) -> float:
        """AST ノードを評価.
        
        Args:
            node: AST ノード
        
        Returns:
            評価結果
        
        Raises:
            TypeError: サポートされていないノードタイプの場合
        """
        if isinstance(node, ast.Constant):
            # 定数（数値）
            return float(node.value)
        
        elif isinstance(node, ast.BinOp):
            # 二項演算子
            left = self._eval_node(node.left)
            right = self._eval_node(node.right)
            op_type = type(node.op)
            
            if op_type not in self.OPERATORS:
                raise TypeError(f"サポートされていない演算子: {op_type.__name__}")
            
            return self.OPERATORS[op_type](left, right)
        
        elif isinstance(node, ast.UnaryOp):
            # 単項演算子
            operand = self._eval_node(node.operand)
            op_type = type(node.op)
            
            if op_type not in self.OPERATORS:
                raise TypeError(f"サポートされていない演算子: {op_type.__name__}")
            
            return self.OPERATORS[op_type](operand)
        
        else:
            raise TypeError(f"サポートされていないノードタイプ: {type(node).__name__}")
    
    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        print("🧹 Calculator Agent をクリーンアップしました")
        await super().cleanup()


# エージェントのエントリーポイント
if __name__ == "__main__":
    import asyncio
    
    async def main():
        """メイン関数."""
        async with CalculatorAgent(metadata_path="agent.yaml") as agent:
            # いくつかの数式を計算
            expressions = [
                "2 + 3",
                "10 - 4",
                "5 * 6",
                "20 / 4",
                "2 ** 8",
                "2 + 3 * 4",
                "(2 + 3) * 4",
                "10 % 3",
                "-5 + 10",
            ]
            
            print("\n🧮 計算結果:")
            for expr in expressions:
                result = await agent.run({"expression": expr})
                print(f"  {expr} = {result['result']}")
    
    asyncio.run(main())

