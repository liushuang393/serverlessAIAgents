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
    
    PDF請求書を処理し、データを抽出してデータベースに保存します。
    """

    async def initialize(self) -> None:
        """エージェントを初期化.
        
        必要なツールやリソースをセットアップします。
        """
        await super().initialize()
        
        # データベース接続を初期化
        self.db_type = "{{ database_type }}"
        self.enable_ocr = {{ enable_ocr | lower }}
        self.supported_languages = {{ supported_languages }}
        
        print(f"Initialized {self.metadata.name} agent")
        print(f"Database type: {self.db_type}")
        print(f"OCR enabled: {self.enable_ocr}")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """エージェントを実行.
        
        Args:
            input_data: 入力データ
                - pdf_path: PDF ファイルのパス
                - output_format: 出力形式 (json, csv, database)
        
        Returns:
            実行結果
                - invoice_data: 抽出された請求書データ
                - status: 処理ステータス
        """
        pdf_path = input_data.get("pdf_path")
        output_format = input_data.get("output_format", "json")
        
        if not pdf_path:
            raise ValueError("pdf_path is required")
        
        # PDF ファイルを読み込む
        pdf_content = await self._read_pdf(pdf_path)
        
        # OCR を実行（有効な場合）
        if self.enable_ocr:
            text = await self._perform_ocr(pdf_content)
        else:
            text = pdf_content
        
        # データを抽出
        invoice_data = await self._extract_data(text)
        
        # 出力形式に応じて処理
        if output_format == "database":
            await self._save_to_database(invoice_data)
        elif output_format == "csv":
            await self._save_to_csv(invoice_data)
        
        return {
            "invoice_data": invoice_data,
            "status": "success",
            "output_format": output_format,
        }

    async def _read_pdf(self, pdf_path: str) -> str:
        """PDF ファイルを読み込む.
        
        Args:
            pdf_path: PDF ファイルのパス
        
        Returns:
            PDF のテキスト内容
        """
        # TODO: 実際の PDF 読み込み処理を実装
        print(f"Reading PDF: {pdf_path}")
        return "Sample PDF content"

    async def _perform_ocr(self, content: str) -> str:
        """OCR を実行.
        
        Args:
            content: PDF 内容
        
        Returns:
            OCR で抽出されたテキスト
        """
        # TODO: 実際の OCR 処理を実装
        print(f"Performing OCR with languages: {self.supported_languages}")
        return content

    async def _extract_data(self, text: str) -> dict[str, Any]:
        """テキストからデータを抽出.
        
        Args:
            text: テキスト内容
        
        Returns:
            抽出されたデータ
        """
        # TODO: 実際のデータ抽出処理を実装
        print("Extracting invoice data")
        return {
            "invoice_number": "INV-2025-001",
            "date": "2025-11-03",
            "total_amount": 10000.0,
            "items": [
                {"name": "Item 1", "quantity": 2, "price": 5000.0},
            ],
        }

    async def _save_to_database(self, data: dict[str, Any]) -> None:
        """データをデータベースに保存.
        
        Args:
            data: 保存するデータ
        """
        # TODO: 実際のデータベース保存処理を実装
        print(f"Saving to {self.db_type} database")
        print(f"Data: {data}")

    async def _save_to_csv(self, data: dict[str, Any]) -> None:
        """データを CSV に保存.
        
        Args:
            data: 保存するデータ
        """
        # TODO: 実際の CSV 保存処理を実装
        print("Saving to CSV")
        print(f"Data: {data}")

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
            "pdf_path": "sample_invoice.pdf",
            "output_format": "json",
        })
        
        print("Result:", result)


if __name__ == "__main__":
    import asyncio
    asyncio.run(main())

