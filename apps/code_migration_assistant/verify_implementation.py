# -*- coding: utf-8 -*-
"""Implementation Verification Script.

このスクリプトは実装の完全性を検証します。
"""

import asyncio
import sys
from pathlib import Path

# プロジェクトルートをPythonパスに追加
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))


async def verify_mcp_tools() -> bool:
    """MCPツールの検証."""
    print("🔍 MCPツールを検証中...")

    try:
        from apps.code_migration_assistant.mcp_tools import (
            COBOLParser,
            CodeValidator,
            JavaGenerator,
            MCPTool,
            MCPToolRequest,
            MCPToolResponse,
            MemorySystem,
            ReflectionPattern,
        )

        print("  ✅ 全MCPツールのインポート成功")

        # COBOLParserテスト
        parser = COBOLParser()
        assert parser.tool_name == "cobol_parser"
        print("  ✅ COBOLParser初期化成功")

        # JavaGeneratorテスト
        generator = JavaGenerator()
        assert generator.tool_name == "java_generator"
        print("  ✅ JavaGenerator初期化成功")

        # CodeValidatorテスト
        validator = CodeValidator()
        assert validator.tool_name == "code_validator"
        print("  ✅ CodeValidator初期化成功")

        # ReflectionPatternテスト
        reflection = ReflectionPattern()
        assert reflection.tool_name == "reflection_pattern"
        print("  ✅ ReflectionPattern初期化成功")

        # MemorySystemテスト
        memory = MemorySystem()
        assert memory.tool_name == "memory_system"
        print("  ✅ MemorySystem初期化成功")

        return True

    except Exception as e:
        print(f"  ❌ MCPツール検証失敗: {e}")
        return False


async def verify_orchestrator() -> bool:
    """Orchestratorの検証."""
    print("\n🔍 Orchestratorを検証中...")

    try:
        from agentflow import MCPToolClient as MCPClient
        from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator

        # MCPClientテスト
        client = MCPClient()
        assert len(client.list_tools()) == 0
        print("  ✅ MCPClient初期化成功")

        # Orchestratorテスト
        orchestrator = CodeMigrationOrchestrator(client)
        assert orchestrator.mcp == client
        print("  ✅ CodeMigrationOrchestrator初期化成功")

        return True

    except Exception as e:
        print(f"  ❌ Orchestrator検証失敗: {e}")
        return False


async def verify_basic_workflow() -> bool:
    """基本ワークフローの検証."""
    print("\n🔍 基本ワークフローを検証中...")

    try:
        from agentflow import MCPToolClient as MCPClient
        from apps.code_migration_assistant.mcp_tools import COBOLParser, MCPToolRequest

        # MCPClientを作成
        client = MCPClient()

        # COBOLParserを登録
        parser = COBOLParser()
        client.register_tool("cobol_parser", parser)

        # 簡単なCOBOLコード
        cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM PIC 9(5).
        """

        # 解析実行
        request = MCPToolRequest(
            tool="cobol_parser",
            version="1.0.0",
            input={
                "cobol_code": cobol_code,
            },
        )

        response = await client.call_tool(request)

        if response.success:
            print("  ✅ COBOL解析成功")
            ast = response.output.get("ast")
            if ast and ast.get("program_id") == "TEST":
                print("  ✅ PROGRAM-ID抽出成功")
            else:
                print("  ⚠️  PROGRAM-ID抽出に問題あり")
                return False
        else:
            print(f"  ❌ COBOL解析失敗: {response.errors}")
            return False

        return True

    except Exception as e:
        print(f"  ❌ 基本ワークフロー検証失敗: {e}")
        import traceback

        traceback.print_exc()
        return False


async def main() -> None:
    """メイン検証関数."""
    print("=" * 60)
    print("Code Migration Assistant - 実装検証")
    print("=" * 60)

    results = []

    # MCPツール検証
    results.append(await verify_mcp_tools())

    # Orchestrator検証
    results.append(await verify_orchestrator())

    # 基本ワークフロー検証
    results.append(await verify_basic_workflow())

    # 結果サマリー
    print("\n" + "=" * 60)
    print("検証結果サマリー")
    print("=" * 60)

    total = len(results)
    passed = sum(results)
    failed = total - passed

    print(f"✅ 成功: {passed}/{total}")
    print(f"❌ 失敗: {failed}/{total}")

    if all(results):
        print("\n🎉 全ての検証に成功しました！")
        print("\n次のステップ:")
        print("1. pytest をインストール: pip install pytest pytest-asyncio")
        print("2. テスト実行: pytest apps/code_migration_assistant/tests/ -v")
        print("3. デプロイメントガイドを参照: DEPLOYMENT_GUIDE.md")
    else:
        print("\n⚠️  一部の検証に失敗しました。エラーを確認してください。")


if __name__ == "__main__":
    asyncio.run(main())

