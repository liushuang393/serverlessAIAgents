# -*- coding: utf-8 -*-
"""Code Migration Assistant CLI.

このスクリプトはCode Migration AssistantのCLIインターフェースを提供します。
"""

import asyncio
import sys
from pathlib import Path
from typing import Any

# プロジェクトルートをPythonパスに追加
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))


async def migrate_cobol_file(file_path: str) -> dict[str, Any]:
    """COBOLファイルをJavaに移行.

    Args:
        file_path: COBOLファイルのパス

    Returns:
        移行結果
    """
    from agentflow import MCPToolClient as MCPClient
    from apps.code_migration_assistant.mcp_tools import (
        COBOLParser,
        CodeValidator,
        JavaGenerator,
        ReflectionPattern,
    )
    from apps.code_migration_assistant.orchestrator import CodeMigrationOrchestrator

    # MCPClientを作成
    client = MCPClient()

    # MCP工具を登録
    client.register_tool("cobol_parser", COBOLParser())
    client.register_tool("java_generator", JavaGenerator())
    client.register_tool("code_validator", CodeValidator())
    client.register_tool("reflection_pattern", ReflectionPattern(mcp_client=client))

    # Orchestratorを作成
    orchestrator = CodeMigrationOrchestrator(client)

    # COBOLファイルを読み込み
    with open(file_path, encoding="utf-8") as f:
        cobol_code = f.read()

    # 移行実行
    result = await orchestrator.migrate(
        cobol_code=cobol_code, file_name=Path(file_path).name
    )

    return result


async def main() -> None:
    """メインCLI関数."""
    import argparse

    parser = argparse.ArgumentParser(
        description="Code Migration Assistant - COBOL→Java移行ツール"
    )
    parser.add_argument("input_file", help="入力COBOLファイル")
    parser.add_argument(
        "-o", "--output", help="出力Javaファイル（オプション）", default=None
    )
    parser.add_argument(
        "--max-iterations",
        type=int,
        default=3,
        help="最大反復回数（デフォルト: 3）",
    )
    parser.add_argument(
        "--threshold",
        type=float,
        default=85.0,
        help="受け入れ閾値（デフォルト: 85.0）",
    )

    args = parser.parse_args()

    print("=" * 60)
    print("Code Migration Assistant - COBOL→Java移行")
    print("=" * 60)
    print(f"入力ファイル: {args.input_file}")
    print(f"最大反復回数: {args.max_iterations}")
    print(f"受け入れ閾値: {args.threshold}")
    print()

    try:
        # 移行実行
        print("🔄 移行を開始します...")
        result = await migrate_cobol_file(args.input_file)

        if result["success"]:
            print("\n✅ 移行成功！")
            print(f"  - Javaクラス: {result['class_name']}")
            print(f"  - 品質スコア: {result['score']:.1f}/100")
            print(f"  - 反復回数: {result['iterations']}")
            print(f"  - 受け入れ可能: {'はい' if result['is_acceptable'] else 'いいえ'}")

            # Javaコードを表示
            print("\n" + "=" * 60)
            print("生成されたJavaコード:")
            print("=" * 60)
            print(result["java_code"])

            # 出力ファイルに保存
            if args.output:
                with open(args.output, "w", encoding="utf-8") as f:
                    f.write(result["java_code"])
                print(f"\n✅ Javaコードを {args.output} に保存しました")
            else:
                # デフォルトの出力ファイル名
                output_file = Path(args.input_file).stem + ".java"
                with open(output_file, "w", encoding="utf-8") as f:
                    f.write(result["java_code"])
                print(f"\n✅ Javaコードを {output_file} に保存しました")

            # フィードバックを表示
            if result.get("feedback"):
                print("\n" + "=" * 60)
                print("フィードバック:")
                print("=" * 60)
                for feedback in result["feedback"]:
                    print(f"  - {feedback}")

        else:
            print("\n❌ 移行失敗")
            print(f"エラー: {result.get('errors', ['Unknown error'])}")

    except FileNotFoundError:
        print(f"\n❌ エラー: ファイルが見つかりません: {args.input_file}")
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ エラー: {e}")
        import traceback

        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    asyncio.run(main())

