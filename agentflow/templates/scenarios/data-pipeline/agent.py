"""{{ agent_name | title }} エージェント.

{{ agent_description }}

Author: {{ author }} <{{ email }}>
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

from agentflow.core.agent_block import AgentBlock


class {{ agent_name | replace('-', '_') | title | replace('_', '') }}Agent(AgentBlock):
    """{{ agent_name | title }} エージェント.

    データ処理パイプラインエージェント。
    """

    async def initialize(self) -> None:
        """エージェントを初期化.

        データ処理ツールをセットアップします。
        """
        await super().initialize()

        # データ処理設定
        self.input_format = "{{ input_format }}"
        self.output_format = "{{ output_format }}"
        self.enable_validation = {{ enable_validation | lower }}
        self.enable_transformation = {{ enable_transformation | lower }}

        print(f"Initialized {self.metadata.name} agent")
        print(f"Input format: {self.input_format}")
        print(f"Output format: {self.output_format}")
        print(f"Validation enabled: {self.enable_validation}")
        print(f"Transformation enabled: {self.enable_transformation}")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """エージェントを実行.

        Args:
            input_data: 入力データ
                - input_path: 入力ファイルパス
                - output_path: 出力ファイルパス
                - transformations: 適用する変換のリスト

        Returns:
            実行結果
                - processed_data: 処理統計
                - status: 処理ステータス
        """
        input_path = input_data.get("input_path")
        output_path = input_data.get("output_path")
        transformations = input_data.get("transformations", [])

        if not input_path or not output_path:
            raise ValueError("input_path and output_path are required")

        # データを読み込む
        data = await self._read_data(input_path)

        # データを検証
        if self.enable_validation:
            valid_data, invalid_data = await self._validate_data(data)
        else:
            valid_data = data
            invalid_data = []

        # データを変換
        if self.enable_transformation:
            transformed_data = await self._transform_data(valid_data, transformations)
        else:
            transformed_data = valid_data

        # データを書き込む
        await self._write_data(transformed_data, output_path)

        return {
            "processed_data": {
                "rows_processed": len(data),
                "rows_valid": len(valid_data),
                "rows_invalid": len(invalid_data),
            },
            "status": "success",
        }

    async def _read_data(self, input_path: str) -> list[dict[str, Any]]:
        """データを読み込む.

        Args:
            input_path: 入力ファイルパス

        Returns:
            読み込んだデータ
        """
        # TODO: 実際のデータ読み込み処理を実装
        print(f"Reading {self.input_format} data from: {input_path}")

        # ダミーデータ
        return [
            {"id": 1, "name": "Item 1", "value": 100},
            {"id": 2, "name": "Item 2", "value": 200},
            {"id": 3, "name": "Item 3", "value": 300},
        ]

    async def _validate_data(
        self,
        data: list[dict[str, Any]],
    ) -> tuple[list[dict[str, Any]], list[dict[str, Any]]]:
        """データを検証.

        Args:
            data: 検証するデータ

        Returns:
            有効なデータと無効なデータのタプル
        """
        # TODO: 実際のデータ検証処理を実装
        print("Validating data")

        valid_data = []
        invalid_data = []

        for row in data:
            # 簡単な検証例
            if "id" in row and "name" in row and "value" in row:
                valid_data.append(row)
            else:
                invalid_data.append(row)

        print(f"Valid rows: {len(valid_data)}, Invalid rows: {len(invalid_data)}")
        return valid_data, invalid_data

    async def _transform_data(
        self,
        data: list[dict[str, Any]],
        transformations: list[str],
    ) -> list[dict[str, Any]]:
        """データを変換.

        Args:
            data: 変換するデータ
            transformations: 適用する変換のリスト

        Returns:
            変換されたデータ
        """
        # TODO: 実際のデータ変換処理を実装
        print(f"Applying {len(transformations)} transformations")

        transformed_data = data.copy()

        for transformation in transformations:
            print(f"Applying transformation: {transformation}")
            # 変換処理をここに実装

        return transformed_data

    async def _write_data(
        self,
        data: list[dict[str, Any]],
        output_path: str,
    ) -> None:
        """データを書き込む.

        Args:
            data: 書き込むデータ
            output_path: 出力ファイルパス
        """
        # TODO: 実際のデータ書き込み処理を実装
        print(f"Writing {self.output_format} data to: {output_path}")
        print(f"Rows written: {len(data)}")

    async def cleanup(self) -> None:
        """エージェントをクリーンアップ.

        リソースを解放します。
        """
        print(f"Cleaning up {self.metadata.name} agent")
        await super().cleanup()


# エージェントのエントリーポイント
async def main():
    """エージェントを実行."""
    agent = {{ agent_name | replace('-', '_') | title | replace('_', '') }}Agent()

    async with agent:
        result = await agent.run({
            "input_path": "input.{{ input_format }}",
            "output_path": "output.{{ output_format }}",
            "transformations": ["normalize", "deduplicate"],
        })

        print("Result:", result)


if __name__ == "__main__":
    import asyncio
    asyncio.run(main())
