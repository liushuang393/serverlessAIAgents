# AI Blocks - サーバーレスAIエージェント基盤

[![Python](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Tests](https://img.shields.io/badge/tests-passing-green.svg)](tests/)

AI Blocksは「積木（レゴ）方式」でAIエージェントを構築するための軽量で柔軟なPythonライブラリです。LangChainなどの巨大フレームワークに依存せず、AIプリミティブ（基本単位）を直接組み合わせて開発できます。

## 🌟 特徴

- **軽量・高速**: 最小限の依存関係で高速な実験と開発
- **モジュラー設計**: 各コンポーネントは独立して開発・テスト・交換可能
- **柔軟な組み合わせ**: 8つの代表的なアーキテクチャパターンを提供
- **プロダクション対応**: 本格的なPythonプロジェクト構成とテストスイート

## 🌟優秀な点：

- **抽象インターフェース設計**: 各コンポーネント（Memory、Tool、Parser等）にABCベースの抽象インターフェースが適切に定義されている
- **モジュラー構造**: コンポーネントが独立したモジュールとして実装され、交換可能性を確保
- **型安全性**: Pydanticを使用した堅牢なデータモデル設計
- **テスト構造**: 単体・統合・パフォーマンステストが適切に分離されている

## 🧱 コアコンポーネント

| コンポーネント | 役割 | 実装ポイント |
|---------------|------|-------------|
| **Memory** | ベクトル検索を伴う長期記憶 | eager書込・kNN検索・要約格納 |
| **Thread** | 会話履歴・状態管理 | Messageログ＋履歴フォーマット |
| **Tool** | 外部API/関数実行 | 型安全なI/O→戻り値整形 |
| **Parser** | 多種ドキュメント→テキスト | PDF/HTML/画像OCR等を抽象化 |
| **Chunker** | 長文分割・前処理 | 長さ制御・重複トークン削減 |
| **Router** | インテント判定→Agent/Tool振分け | ルールorLLM判定 |
| **Evaluator** | 出力品質チェック＆改善指示 | RLHFライト版／自己反省プロンプト |

## 🏗️ アーキテクチャパターン

### ✅ 実装済み
1. **Augmented LLM** - LLM＋Memory＋Tool
2. **Prompt Chaining** - 専用Agentを直列化
3. **Agent Router** - 入力内容で最適Agentに振分け
4. **Parallel Agents** - 複数Agentを非同期並列実行

### 🚧 実装予定
5. **Orchestrator-Worker** - タスク分割＋統合
6. **Evaluator-Optimizer** - 自己評価による再生成
7. **Tool Calling** - 外部システム連携
8. **Memory-centric** - ドキュメントQAなど知識特化

## 🚀 クイックスタート

### インストール

#### 🎯 用途別インストール

**初心者・学習用（推奨）：**
```bash
# 全機能を一括インストール
pip install -e ".[all]"
```

**本番環境用：**
```bash
# 必要な機能のみ選択（例：LLM + ベクトル検索）
pip install -e ".[llm,vector]"
```

**軽量版：**
```bash
# コア機能のみ
pip install -e .
```

#### 📦 機能別インストール詳細

| オプション | 含まれる機能 | 用途 |
|-----------|-------------|------|
| `[all]` | 全機能 | 開発・学習・フル機能アプリ |
| `[llm]` | OpenAI, Anthropic | チャットボット、テキスト生成 |
| `[vector]` | ChromaDB, Pinecone | 検索、RAGシステム |
| `[document]` | PDF, HTML, OCR | ドキュメント処理 |
| `[dev]` | 開発ツール | テスト、フォーマット、型チェック |

### 基本的な使用例

```python
import asyncio
from ai_blocks.core import VectorMemory, ToolManager
from ai_blocks.architectures import AugmentedLLM
from ai_blocks.interfaces import OpenAIProvider

async def main():
    # コンポーネントを初期化
    memory = VectorMemory()
    tools = ToolManager()

    # LLMプロバイダーを設定（例：OpenAI）
    llm = OpenAIProvider(api_key="your-api-key")

    # Augmented LLMエージェントを作成
    agent = AugmentedLLM(
        llm_provider=llm,
        memory=memory,
        tool_manager=tools
    )

    # 知識を追加
    await agent.add_knowledge("Pythonは汎用プログラミング言語です。")

    # 質問に回答
    response = await agent.process("Pythonについて教えて")
    print(response)

if __name__ == "__main__":
    asyncio.run(main())
```

### カスタムツールの作成

```python
from ai_blocks.core.tool import tool

@tool(name="weather", description="天気情報を取得")
async def get_weather(city: str) -> str:
    # 実際のAPI呼び出しをここに実装
    return f"{city}の天気は晴れです"

# ツールマネージャーに登録
tools.register_function(get_weather)
```

## 🎯 実用例：フロントエンド移行自動化システム

AI Blocksを活用した企業級フロントエンド移行システムを実装しています：

### 🔧 システム概要
レガシーフロントエンドコード（JSP、jQuery、IE対応）を現代的な技術スタック（React、TypeScript、Tailwind CSS）に自動移行する包括的なAIエージェントシステム。

### 🤖 専門エージェント構成
- **InventoryAgent**: プロジェクト構造の自動スキャンとマニフェスト生成
- **AnalyzerAgent**: 技術的問題の特定と分析（jQuery依存、IE互換性等）
- **MigrationPlannerAgent**: 技術スタック分析と移行戦略の策定
- **RefactorAgent**: JSP/HTML → React TSX変換
- **ResponsiveAgent**: レスポンシブデザインへの自動変換
- **CompatFixAgent**: ブラウザ互換性問題の修正とPolyfill設定
- **TestAgent**: Jest + React Testing Library による自動テスト生成
- **QAReportAgent**: Lighthouse、WAVE、axe-core統合品質レポート
- **CDOrchestrator**: GitHub Actions、Vercel、Netlify対応の自動デプロイ設定

### 📊 品質基準
- **パフォーマンス**: Lighthouse スコア 80点以上
- **アクセシビリティ**: WCAG 2.2 AA準拠（90点以上）
- **SEO**: 90点以上
- **テストカバレッジ**: 90%以上

### 🚀 使用例
```python
import asyncio
from products.frontend_migration.workflows.migration_workflow import MigrationOrchestrator

async def main():
    # オーケストレーターを初期化
    orchestrator = MigrationOrchestrator()

    # フロントエンド移行を実行
    result = await orchestrator.execute_migration(
        project_path="./legacy_project",
        config={
            "target_framework": "React",
            "css_framework": "Tailwind CSS",
            "typescript": True
        }
    )

    print(f"移行完了: {result.status}")
    print(f"品質スコア: {result.quality_scores.get('overall', 0):.2f}")

if __name__ == "__main__":
    asyncio.run(main())
```

## 📁 プロジェクト構造

```
ai_blocks/                   # AIエージェント基盤フレームワーク
├── core/                    # コアコンポーネント
│   ├── memory.py           # 長期記憶
│   ├── thread.py           # 会話履歴
│   ├── tool.py             # ツール実行
│   ├── parser.py           # ドキュメント変換
│   ├── chunker.py          # テキスト分割
│   ├── router.py           # ルーティング
│   └── evaluator.py        # 品質評価
├── architectures/           # アーキテクチャパターン
│   ├── augmented_llm.py    # 基本パターン
│   └── ...                 # その他のパターン
├── utils/                   # ユーティリティ
├── config/                  # 設定管理
└── interfaces/              # 外部インターフェース

products/                    # 実用アプリケーション
└── frontend_migration/      # フロントエンド移行システム
    ├── agents/              # 専門エージェント
    │   ├── inventory_agent.py     # プロジェクト分析
    │   ├── analyzer_agent.py      # 問題分析
    │   ├── responsive_agent.py    # レスポンシブ変換
    │   ├── migration_planner_agent.py  # 移行計画
    │   ├── refactor_agent.py      # コードリファクタリング
    │   ├── compat_fix_agent.py    # 互換性修正
    │   ├── test_agent.py          # テスト生成
    │   ├── qa_report_agent.py     # 品質評価
    │   └── cd_orchestrator.py     # CD設定
    ├── workflows/           # ワークフロー管理
    │   └── migration_workflow.py  # メインオーケストレーター
    └── tests/               # テストスイート
        ├── unit/            # 単体テスト
        ├── integration/     # 統合テスト
        └── performance/     # パフォーマンステスト
```

## 🔧 開発環境のセットアップ

```bash
# リポジトリをクローン
git clone https://github.com/ai-blocks/ai-blocks.git
cd ai-blocks

# 開発環境をセットアップ
make setup-dev

# テストを実行
make test

# コードフォーマット
make format

# 型チェック
make type-check
```

## 📊 テスト

```bash
# 全てのテストを実行
make test

# 単体テストのみ
make test-unit

# 統合テストのみ
make test-integration

# パフォーマンステスト
make test-performance
```

## 🔧 設定

### 環境変数設定

環境変数または`.env`ファイルで設定を管理：

```bash
# LLM設定
AI_BLOCKS_OPENAI_API_KEY=your_openai_api_key
AI_BLOCKS_ANTHROPIC_API_KEY=your_anthropic_api_key
AI_BLOCKS_DEFAULT_MODEL=gpt-3.5-turbo
AI_BLOCKS_MAX_TOKENS=1000
AI_BLOCKS_TEMPERATURE=0.7

# ベクトルデータベース設定
AI_BLOCKS_VECTOR_STORE_TYPE=chroma
AI_BLOCKS_CHROMA_PERSIST_DIRECTORY=./chroma_db

# メモリ設定
AI_BLOCKS_MEMORY_MAX_ITEMS=1000
AI_BLOCKS_MEMORY_SIMILARITY_THRESHOLD=0.7

# ツール設定
AI_BLOCKS_TOOL_TIMEOUT=30.0
AI_BLOCKS_MAX_TOOL_CALLS=10
```

### 設定ファイルの作成

```bash
# デフォルト設定ファイルを生成
python -c "from ai_blocks.config.settings import create_default_env_file; create_default_env_file()"
```

## 🔧 トラブルシューティング

### よくある問題と解決方法

#### 1. インストールエラー
```bash
# 依存関係の競合が発生した場合
pip install --upgrade pip
pip install -e ".[all]" --force-reinstall

# 特定のパッケージでエラーが出る場合
pip install -e ".[llm]"  # 最小構成から始める
```

#### 2. インポートエラー
```python
# モジュールが見つからない場合
import sys
sys.path.append('.')
from ai_blocks import AugmentedLLM
```

#### 3. API キーエラー
```bash
# 環境変数が設定されているか確認
echo $AI_BLOCKS_OPENAI_API_KEY

# .envファイルを作成
cat > .env << EOF
AI_BLOCKS_OPENAI_API_KEY=your_actual_api_key
EOF
```

#### 4. メモリ不足エラー
```python
# 設定を調整
from ai_blocks.config import get_settings
settings = get_settings()
settings.memory_max_items = 500  # デフォルトより少なく
```

## 🤝 コントリビューション

1. このリポジトリをフォーク
2. 機能ブランチを作成 (`git checkout -b feature/amazing-feature`)
3. 変更をコミット (`git commit -m 'Add amazing feature'`)
4. ブランチにプッシュ (`git push origin feature/amazing-feature`)
5. プルリクエストを作成

## 📄 ライセンス

このプロジェクトはMITライセンスの下で公開されています。詳細は[LICENSE](LICENSE)ファイルを参照してください。

## 🙏 謝辞

- [OpenAI](https://openai.com/) - GPTモデルの提供
- [Anthropic](https://www.anthropic.com/) - Claudeモデルの提供
- [ChromaDB](https://www.trychroma.com/) - ベクトルデータベース
- オープンソースコミュニティの皆様

## 📞 サポート

- 📧 Email: contact@aiblocks.dev
- 🐛 Issues: [GitHub Issues](https://github.com/ai-blocks/ai-blocks/issues)
- 📖 Documentation: [AI Blocks Docs](https://ai-blocks.readthedocs.io)

---

**AI Blocks** - 次世代のAIエージェント開発を、もっとシンプルに。
