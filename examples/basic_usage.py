"""
AI Blocks 基本使用例

このサンプルは、AI Blocksの各コンポーネントの基本的な使用方法を示します。
"""

import asyncio
from datetime import datetime

# AI Blocksのコンポーネントをインポート
from ai_blocks.core.memory import VectorMemory
from ai_blocks.core.thread import SimpleThread
from ai_blocks.core.tool import ToolManager, tool
from ai_blocks.core.parser import MultiParser
from ai_blocks.core.chunker import SmartChunker
from ai_blocks.core.router import RuleBasedRouter
from ai_blocks.core.evaluator import RuleBasedEvaluator
from ai_blocks.core.models import Message, MessageRole, RouteDefinition


async def demo_memory():
    """メモリコンポーネントのデモ"""
    print("🧠 Memory コンポーネントのデモ")
    print("-" * 30)
    
    # メモリを初期化
    memory = VectorMemory(max_items=10)
    
    # 知識を保存
    knowledge_items = [
        "東京は日本の首都です。",
        "富士山は日本で最も高い山です。",
        "桜は日本の国花です。",
        "寿司は日本の伝統的な料理です。",
        "新幹線は日本の高速鉄道システムです。"
    ]
    
    print("知識を保存中...")
    for item in knowledge_items:
        memory_id = await memory.store(item, {"category": "japan_facts"})
        print(f"  保存: {item} (ID: {memory_id[:8]}...)")
    
    print(f"\n総メモリ件数: {await memory.count()}")
    
    # 検索テスト
    print("\n検索テスト:")
    queries = ["日本の首都", "高い山", "料理"]
    
    for query in queries:
        results = await memory.search(query, limit=2)
        print(f"  クエリ: '{query}'")
        for result in results:
            print(f"    - {result.content} (類似度: {result.similarity_score:.3f})")
    
    print()


async def demo_thread():
    """スレッドコンポーネントのデモ"""
    print("💬 Thread コンポーネントのデモ")
    print("-" * 30)
    
    # スレッドを初期化
    thread = SimpleThread(max_history=5)
    
    # メッセージを追加
    messages = [
        Message(role=MessageRole.USER, content="こんにちは"),
        Message(role=MessageRole.ASSISTANT, content="こんにちは！何かお手伝いできることはありますか？"),
        Message(role=MessageRole.USER, content="天気について教えて"),
        Message(role=MessageRole.ASSISTANT, content="天気について説明します..."),
    ]
    
    print("メッセージを追加中...")
    for msg in messages:
        await thread.add_message(msg)
        print(f"  {msg.role}: {msg.content}")
    
    # 履歴を取得
    print(f"\nスレッドID: {await thread.get_thread_id()}")
    history = await thread.get_history(limit=3)
    print(f"履歴件数: {len(history)}")
    
    # 状態管理
    await thread.update_state({"user_name": "田中", "topic": "weather"})
    state = await thread.get_state()
    print(f"状態: {state}")
    
    print()


async def demo_tools():
    """ツールコンポーネントのデモ"""
    print("🔧 Tool コンポーネントのデモ")
    print("-" * 30)
    
    # ツールマネージャーを初期化
    tools = ToolManager()
    
    # カスタムツールを定義
    @tool(name="greet", description="挨拶を生成する")
    def create_greeting(name: str, time_of_day: str = "day") -> str:
        greetings = {
            "morning": "おはようございます",
            "day": "こんにちは", 
            "evening": "こんばんは"
        }
        greeting = greetings.get(time_of_day, "こんにちは")
        return f"{greeting}、{name}さん！"
    
    # ツールを登録
    tools.register_function(create_greeting)
    
    # 利用可能なツールを表示
    available_tools = tools.get_available_tools()
    print("利用可能なツール:")
    for tool_def in available_tools:
        print(f"  - {tool_def.name}: {tool_def.description}")
    
    # ツールを実行
    print("\nツール実行例:")
    
    # 組み込みツール
    result1 = await tools.execute("add", {"a": 10, "b": 20})
    print(f"  add(10, 20) = {result1.result} (成功: {result1.success})")
    
    # カスタムツール
    result2 = await tools.execute("greet", {"name": "山田", "time_of_day": "morning"})
    print(f"  greet('山田', 'morning') = {result2.result}")
    
    print()


async def demo_parser():
    """パーサーコンポーネントのデモ"""
    print("📄 Parser コンポーネントのデモ")
    print("-" * 30)
    
    # マルチパーサーを初期化
    parser = MultiParser()
    
    # サポートされているタイプを表示
    supported_types = parser.get_supported_types()
    print(f"サポートされているコンテンツタイプ: {len(supported_types)}種類")
    for content_type in supported_types[:5]:  # 最初の5つを表示
        print(f"  - {content_type}")
    
    # テキストファイルをパース
    sample_text = """
    # AI Blocks サンプルドキュメント
    
    これはテストドキュメントです。
    
    ## 特徴
    - 軽量
    - 高速
    - 柔軟
    
    詳細については公式ドキュメントを参照してください。
    """
    
    print("\nテキストパース例:")
    parsed_doc = await parser.parse(
        sample_text.encode('utf-8'),
        "text/markdown",
        {"source": "sample.md"}
    )
    
    print(f"  パース結果長: {len(parsed_doc.text)}文字")
    print(f"  メタデータ: {parsed_doc.metadata}")
    print(f"  内容プレビュー: {parsed_doc.text[:100]}...")
    
    print()


async def demo_chunker():
    """チャンカーコンポーネントのデモ"""
    print("✂️ Chunker コンポーネントのデモ")
    print("-" * 30)
    
    # スマートチャンカーを初期化
    chunker = SmartChunker()
    
    # サンプルテキスト
    long_text = """
    人工知能（AI）は、コンピューターシステムが人間の知能を模倣する技術です。
    
    機械学習は、AIの一分野で、データからパターンを学習してタスクを実行します。
    深層学習は、機械学習の手法の一つで、ニューラルネットワークを使用します。
    
    自然言語処理（NLP）は、コンピューターが人間の言語を理解し、生成する技術です。
    大規模言語モデル（LLM）は、NLPの最新の成果の一つです。
    
    これらの技術は、様々な分野で応用されています。
    医療、金融、教育、エンターテインメントなど、多くの業界でAIが活用されています。
    """
    
    print("テキストをチャンクに分割中...")
    chunks = await chunker.chunk(long_text, chunk_size=100, overlap=20)
    
    print(f"分割結果: {len(chunks)}個のチャンク")
    for i, chunk in enumerate(chunks):
        print(f"  チャンク{i+1}: {chunk.text[:50]}... ({chunk.start_index}-{chunk.end_index})")
    
    # チャンクを結合
    merged_text = await chunker.merge_chunks(chunks)
    print(f"\n結合後の長さ: {len(merged_text)}文字")
    
    print()


async def demo_router():
    """ルーターコンポーネントのデモ"""
    print("🚦 Router コンポーネントのデモ")
    print("-" * 30)
    
    # ルールベースルーターを初期化
    router = RuleBasedRouter(default_target="general")
    
    # カスタムルートを追加
    from ai_blocks.core.router import RouteDefinition, RouteType
    
    custom_routes = [
        RouteDefinition(
            pattern="weather",
            target="weather_agent",
            priority=90,
            conditions={"type": RouteType.KEYWORD, "keywords": ["天気", "weather", "気温"]}
        ),
        RouteDefinition(
            pattern="math",
            target="math_agent", 
            priority=85,
            conditions={"type": RouteType.KEYWORD, "keywords": ["計算", "数学", "math"]}
        )
    ]
    
    for route in custom_routes:
        router.register_route(route)
    
    # ルーティングテスト
    print("ルーティングテスト:")
    test_inputs = [
        "今日の天気はどうですか？",
        "2 + 3 を計算して",
        "こんにちは",
        "ヘルプが必要です"
    ]
    
    for input_text in test_inputs:
        result = await router.route(input_text)
        print(f"  入力: '{input_text}'")
        print(f"    → ターゲット: {result.target} (信頼度: {result.confidence:.2f})")
    
    print()


async def demo_evaluator():
    """評価器コンポーネントのデモ"""
    print("📊 Evaluator コンポーネントのデモ")
    print("-" * 30)
    
    # ルールベース評価器を初期化
    evaluator = RuleBasedEvaluator(passing_threshold=0.7)
    
    # 評価テスト
    test_outputs = [
        "これは明確で有用な回答です。質問に対して適切に答えています。",
        "短い回答。",
        "これは非常に長い回答で、多くの詳細情報を含んでいますが、時として冗長になる可能性があります。" * 5
    ]
    
    criteria = ["clarity", "relevance", "length", "safety"]
    
    print("評価テスト:")
    for i, output in enumerate(test_outputs, 1):
        print(f"\n  テスト{i}: {output[:50]}...")
        
        evaluation = await evaluator.evaluate(
            output,
            criteria,
            {"query": "質問に答えて", "min_length": 20, "max_length": 200}
        )
        
        print(f"    総合スコア: {evaluation.score:.2f}")
        print(f"    合格: {evaluation.passed}")
        print(f"    基準別スコア: {evaluation.criteria_scores}")
        
        # 改善提案
        suggestions = await evaluator.suggest_improvements(output, evaluation)
        if suggestions:
            print(f"    改善提案: {suggestions[0]}")
    
    print()


async def main():
    """メイン実行関数"""
    print("🚀 AI Blocks - 基本使用例デモ")
    print("=" * 50)
    print()
    
    # 各コンポーネントのデモを実行
    await demo_memory()
    await demo_thread()
    await demo_tools()
    await demo_parser()
    await demo_chunker()
    await demo_router()
    await demo_evaluator()
    
    print("🎉 全てのデモが完了しました！")


if __name__ == "__main__":
    # 非同期実行
    asyncio.run(main())
