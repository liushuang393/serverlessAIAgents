# -*- coding: utf-8 -*-
"""Decision Governance Engine - メインエントリーポイント.

使用例:
    # CLIモード
    python -m apps.decision_governance_engine.main "新規事業への投資判断をしたい"

    # インタラクティブモード
    python -m apps.decision_governance_engine.main --interactive
"""

import argparse
import asyncio
import json
import logging
import sys
from pathlib import Path

from apps.decision_governance_engine.schemas.input_schemas import (
    BudgetConstraint,
    ConstraintSet,
    DecisionRequest,
    TimelineConstraint,
)
from apps.decision_governance_engine.workflow import DecisionEngine


def setup_logging(verbose: bool = False) -> None:
    """ログ設定を初期化."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(sys.stdout)],
    )


async def run_decision_engine(question: str, constraints: ConstraintSet | None = None) -> dict:
    """Decision Engineを実行.

    Args:
        question: 意思決定の質問
        constraints: 制約条件（オプション）

    Returns:
        決策レポートまたはエラー情報
    """
    engine = DecisionEngine()

    request = DecisionRequest(
        question=question,
        constraints=constraints or ConstraintSet(),
    )

    result = await engine.process(request)

    # Pydanticモデルの場合はdictに変換
    if hasattr(result, "model_dump"):
        return result.model_dump()
    return result


def interactive_mode() -> None:
    """インタラクティブモードで実行."""
    print("=" * 60)
    print("Decision Governance Engine - インタラクティブモード")
    print("=" * 60)
    print("意思決定の質問を入力してください。")
    print("終了するには 'quit' または 'exit' を入力してください。")
    print("-" * 60)

    while True:
        try:
            question = input("\n質問> ").strip()

            if question.lower() in ("quit", "exit", "q"):
                print("終了します。")
                break

            if not question:
                print("質問を入力してください。")
                continue

            print("\n処理中...")
            result = asyncio.run(run_decision_engine(question))

            if result.get("status") == "rejected":
                print(f"\n❌ 拒否: {result.get('message')}")
                if result.get("suggested_rephrase"):
                    print(f"💡 提案: {result.get('suggested_rephrase')}")
            else:
                print("\n✅ 分析完了")
                print(json.dumps(result, ensure_ascii=False, indent=2))

        except KeyboardInterrupt:
            print("\n中断されました。")
            break
        except Exception as e:
            print(f"\n❌ エラー: {e}")


def main() -> None:
    """メイン関数."""
    parser = argparse.ArgumentParser(
        description="Decision Governance Engine - 意思決定支援システム",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "question",
        nargs="?",
        help="意思決定の質問",
    )
    parser.add_argument(
        "-i", "--interactive",
        action="store_true",
        help="インタラクティブモードで実行",
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="詳細ログを出力",
    )
    parser.add_argument(
        "-o", "--output",
        type=str,
        help="結果をファイルに出力",
    )
    parser.add_argument(
        "--budget",
        type=float,
        help="予算制約（万円）",
    )
    parser.add_argument(
        "--timeline",
        type=int,
        help="期間制約（月）",
    )

    args = parser.parse_args()
    setup_logging(args.verbose)

    if args.interactive:
        interactive_mode()
        return

    if not args.question:
        parser.print_help()
        sys.exit(1)

    # 制約条件を構築
    constraints = ConstraintSet()
    if args.budget:
        constraints.budget = BudgetConstraint(amount=args.budget, currency="JPY")
    if args.timeline:
        constraints.timeline = TimelineConstraint(months=args.timeline)

    # 実行
    result = asyncio.run(run_decision_engine(args.question, constraints))

    # 出力
    output_json = json.dumps(result, ensure_ascii=False, indent=2)

    if args.output:
        Path(args.output).write_text(output_json, encoding="utf-8")
        print(f"結果を {args.output} に保存しました。")
    else:
        print(output_json)


if __name__ == "__main__":
    main()

