"""
Augmented LLM の使用例

このサンプルは、LLM + Memory + Tool を組み合わせた基本的なエージェントの使用方法を示します。
"""

import asyncio

from ai_blocks.architectures.augmented_llm import AugmentedLLM

# AI Blocksのコンポーネントをインポート
from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.tool import ToolManager, tool


class MockLLMProvider:
    """モックLLMプロバイダー（実際の使用時はOpenAIProviderなどを使用）"""

    async def generate(self, prompt: str) -> str:
        """簡単な応答生成（実際のLLMの代わり）"""
        if "計算" in prompt or "+" in prompt or "-" in prompt or "*" in prompt:
            return "計算が必要ですね。addツールやmultiplyツールを使用できます。"
        elif "時刻" in prompt or "時間" in prompt:
            return "現在の時刻を確認しますね。get_current_timeツールを使用します。"
        elif "記憶" in prompt or "覚えて" in prompt:
            return "情報を記憶に保存しました。後で検索できます。"
        else:
            return f"ご質問「{prompt[:50]}...」について回答します。関連する記憶やツール結果を参考にしています。"


@tool(name="weather", description="指定された都市の天気情報を取得する")
async def get_weather(city: str) -> str:
    """天気情報を取得するカスタムツール"""
    # 実際の実装では天気APIを呼び出す
    weather_data = {
        "東京": "晴れ、気温25度",
        "大阪": "曇り、気温23度",
        "札幌": "雨、気温18度",
        "福岡": "晴れ、気温27度",
    }

    return weather_data.get(city, f"{city}の天気情報は取得できませんでした")


@tool(name="translate", description="テキストを英語に翻訳する")
async def translate_to_english(text: str) -> str:
    """翻訳ツール（簡易版）"""
    # 実際の実装では翻訳APIを使用
    translations = {
        "こんにちは": "Hello",
        "ありがとう": "Thank you",
        "さようなら": "Goodbye",
        "おはよう": "Good morning",
        "こんばんは": "Good evening",
    }

    return translations.get(
        text, f"Translation of '{text}' is not available in this demo"
    )


async def main():
    """メイン実行関数"""
    print("🤖 AI Blocks - Augmented LLM サンプル")
    print("=" * 50)

    # 1. コンポーネントを初期化
    print("📦 コンポーネントを初期化中...")

    # メモリコンポーネント
    memory = VectorMemory(max_items=100, similarity_threshold=0.6)

    # ツールマネージャー
    tools = ToolManager()

    # カスタムツールを登録
    tools.register_function(get_weather)
    tools.register_function(translate_to_english)

    # モックLLMプロバイダー
    llm = MockLLMProvider()

    # Augmented LLMエージェントを作成
    agent = AugmentedLLM(
        llm_provider=llm,
        memory=memory,
        tool_manager=tools,
        name="DemoAgent",
        config={"max_memory_items": 5, "enable_tool_use": True, "max_tool_calls": 3},
    )

    print("✅ 初期化完了！")
    print()

    # 2. 知識をメモリに追加
    print("🧠 知識をメモリに追加中...")

    knowledge_items = [
        "AI Blocksは軽量で柔軟なAIエージェント構築ライブラリです。",
        "Pythonは汎用プログラミング言語で、AI開発によく使われます。",
        "機械学習は人工知能の一分野で、データからパターンを学習します。",
        "ベクトル検索は類似性に基づいて情報を検索する技術です。",
        "LLMは大規模言語モデルの略で、自然言語処理に使用されます。",
    ]

    for knowledge in knowledge_items:
        await agent.add_knowledge(knowledge, {"type": "general_knowledge"})

    print(f"✅ {len(knowledge_items)}件の知識を追加しました")
    print()

    # 3. 利用可能なツールを表示
    print("🔧 利用可能なツール:")
    available_tools = tools.get_available_tools()
    for tool_def in available_tools:
        print(f"  - {tool_def.name}: {tool_def.description}")
    print()

    # 4. 対話例を実行
    print("💬 対話例:")
    print("-" * 30)

    # 例1: 基本的な質問（メモリ検索）
    query1 = "AI Blocksについて教えて"
    print(f"👤 ユーザー: {query1}")
    response1 = await agent.process(query1)
    print(f"🤖 エージェント: {response1}")
    print()

    # 例2: 計算（ツール使用）
    query2 = "15 + 27 を計算して"
    print(f"👤 ユーザー: {query2}")
    response2 = await agent.process(query2)
    print(f"🤖 エージェント: {response2}")
    print()

    # 例3: 天気情報（カスタムツール）
    query3 = "東京の天気を教えて"
    print(f"👤 ユーザー: {query3}")
    response3 = await agent.process(query3)
    print(f"🤖 エージェント: {response3}")
    print()

    # 例4: 翻訳（カスタムツール）
    query4 = "「こんにちは」を英語に翻訳して"
    print(f"👤 ユーザー: {query4}")
    response4 = await agent.process(query4)
    print(f"🤖 エージェント: {response4}")
    print()

    # 例5: 時刻取得（組み込みツール）
    query5 = "現在の時刻を教えて"
    print(f"👤 ユーザー: {query5}")
    response5 = await agent.process(query5)
    print(f"🤖 エージェント: {response5}")
    print()

    # 5. エージェントの状態を表示
    print("📊 エージェントの状態:")
    print("-" * 20)
    status = await agent.get_status()

    print(f"名前: {status['name']}")
    print(f"メモリ件数: {status.get('memory_count', 'N/A')}")
    print(f"利用可能ツール: {len(status.get('available_tools', []))}")

    metrics = status["metrics"]
    print(f"総リクエスト数: {metrics['total_requests']}")
    print(f"成功率: {metrics['success_rate']:.2%}")
    print(f"平均実行時間: {metrics['average_execution_time']:.3f}秒")
    print()

    # 6. メモリ検索のデモ
    print("🔍 メモリ検索のデモ:")
    print("-" * 20)

    search_query = "プログラミング"
    print(f"検索クエリ: {search_query}")

    search_results = await memory.search(search_query, limit=3)
    print(f"検索結果: {len(search_results)}件")

    for i, result in enumerate(search_results, 1):
        print(f"  {i}. {result.content[:50]}... (類似度: {result.similarity_score:.3f})")

    print()
    print("🎉 デモ完了！")


if __name__ == "__main__":
    # 非同期実行
    asyncio.run(main())
