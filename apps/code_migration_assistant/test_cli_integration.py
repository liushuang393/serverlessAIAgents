"""Code Migration Assistant CLI 統合テスト.

このスクリプトはCLIの統合テストを実行します。
"""

import asyncio
import sys
from pathlib import Path


# プロジェクトルートをPythonパスに追加
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))


async def test_cli_migration() -> bool:
    """CLI移行テスト."""
    print("🔍 CLI移行をテスト中...")

    try:
        from apps.code_migration_assistant.cli import migrate_cobol_file

        # サンプルCOBOLファイルを移行
        result = await migrate_cobol_file("apps/code_migration_assistant/examples/calculator.cob")

        if result["success"]:
            print("  ✅ CLI移行成功")
            print(f"    - Javaクラス: {result['class_name']}")
            print(f"    - 品質スコア: {result['score']:.1f}/100")
            print(f"    - 反復回数: {result['iterations']}")
            print(f"    - 受け入れ可能: {result['is_acceptable']}")
            return True
        print(f"  ❌ CLI移行失敗: {result.get('errors')}")
        return False

    except Exception as e:
        print(f"  ❌ CLI移行テスト失敗: {e}")
        import traceback

        traceback.print_exc()
        return False


async def test_mcp_tools() -> bool:
    """MCPツールテスト."""
    print("\n🔍 MCPツールをテスト中...")

    try:
        from apps.code_migration_assistant.mcp_tools import (
            COBOLParser,
            CodeValidator,
            JavaGenerator,
            MCPToolRequest,
        )

        from kernel import MCPToolClient as MCPClient

        # MCPClientを作成
        client = MCPClient()

        # MCPツールを登録
        client.register_tool("cobol_parser", COBOLParser())
        client.register_tool("java_generator", JavaGenerator())
        client.register_tool("code_validator", CodeValidator())

        # サンプルCOBOLコード
        cobol_code = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM PIC 9(5).
        """

        # COBOLParser テスト
        parse_request = MCPToolRequest(tool="cobol_parser", version="1.0.0", input={"cobol_code": cobol_code})
        parse_response = await client.call_tool(parse_request)

        if not parse_response.success:
            print(f"  ❌ COBOLParser失敗: {parse_response.errors}")
            return False

        print("  ✅ COBOLParser成功")

        # JavaGenerator テスト
        gen_request = MCPToolRequest(
            tool="java_generator",
            version="1.0.0",
            input={
                "ast": parse_response.output["ast"],
                "metadata": parse_response.output["metadata"],
            },
        )
        gen_response = await client.call_tool(gen_request)

        if not gen_response.success:
            print(f"  ❌ JavaGenerator失敗: {gen_response.errors}")
            return False

        print("  ✅ JavaGenerator成功")

        # CodeValidator テスト
        val_request = MCPToolRequest(
            tool="code_validator",
            version="1.0.0",
            input={
                "java_code": gen_response.output["java_code"],
                "ast": parse_response.output["ast"],
                "metadata": parse_response.output["metadata"],
                "mappings": gen_response.output["mappings"],
            },
        )
        val_response = await client.call_tool(val_request)

        if not val_response.success:
            print(f"  ❌ CodeValidator失敗: {val_response.errors}")
            return False

        print("  ✅ CodeValidator成功")
        print(f"    - 品質スコア: {val_response.output['score']:.1f}/100")

        return True

    except Exception as e:
        print(f"  ❌ MCPツールテスト失敗: {e}")
        import traceback

        traceback.print_exc()
        return False


async def main() -> None:
    """メイン統合テスト関数."""
    print("=" * 60)
    print("Code Migration Assistant - 統合テスト")
    print("=" * 60)

    results = []

    # MCPツールテスト
    results.append(await test_mcp_tools())

    # CLI移行テスト
    results.append(await test_cli_migration())

    # 結果サマリー
    print("\n" + "=" * 60)
    print("統合テスト結果サマリー")
    print("=" * 60)

    total = len(results)
    passed = sum(results)
    failed = total - passed

    print(f"✅ 成功: {passed}/{total}")
    print(f"❌ 失敗: {failed}/{total}")

    if all(results):
        print("\n🎉 全ての統合テストに成功しました！")
        print("\nCode Migration Assistant は正常に動作しています。")
        print("\n使用方法:")
        print("  python apps/code_migration_assistant/cli.py <input.cob> [-o <output.java>]")
    else:
        print("\n⚠️  一部の統合テストに失敗しました。")


if __name__ == "__main__":
    asyncio.run(main())
