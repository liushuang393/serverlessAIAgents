# Template Project

PocketFlowフレームワークを使用したAIエージェントプロジェクトのテンプレートです。このテンプレートは、LLM（大規模言語モデル）とWeb検索機能を組み合わせたワークフローを構築するための基盤を提供します。

## 🚀 概要

このプロジェクトは、以下の機能を持つAIエージェントシステムのテンプレートです：

- **PocketFlowベース**: 軽量で柔軟なワークフローエンジン
- **LLM統合**: OpenAI APIを使用した自然言語処理
- **Web検索機能**: インターネット検索による情報収集
- **モジュラー設計**: 再利用可能なコンポーネント構造

## 📁 プロジェクト構造

```
template_project/
├── README.md              # このファイル
├── requirements.txt       # 依存関係
├── main.py               # メインエントリーポイント
├── flow.py               # ワークフロー定義
├── nodes.py              # カスタムノード実装
├── utils/                # ユーティリティモジュール
│   ├── __init__.py
│   ├── call_llm.py       # LLM呼び出し機能
│   └── search_web.py     # Web検索機能
└── docs/                 # ドキュメント
    └── design.md         # 設計ドキュメント
```
cookbook\pocketflow-chat
## 🛠️ セットアップ

### 前提条件

- Python 3.10以上
- pip パッケージマネージャー

### インストール

1. **依存関係のインストール**
   ```bash
   pip install -r requirements.txt
   ```

2. **環境変数の設定**
   ```bash
   # OpenAI API キー（必要に応じて）
   export OPENAI_API_KEY="your-openai-api-key"

   # カスタムベースURLOpenAIの場合の環境変数（ディフォルトはローカルLLM"http://localhost:11434/v1"または"http://host.docker.internal:11434/v1"）
   export BASE_URL="https://api.openai.com/v1"
   # カスタムベースURL（DeepSeekを使用する場合）
   export BASE_URL="https://api.deepseek.com"
   # カスタムベースURL（ローカルvLLMを使用する場合）
   export BASE_URL="http://localhost:8002/v1"
   ```

## 🚀 使用方法

### 基本的な実行

```bash
python main.py
```

### モジュールとしての実行

```bash
python -m products.template_project.main
```

## 🏗️ アーキテクチャ

### PocketFlowの基本概念

このテンプレートは、PocketFlowフレームワークの以下の概念を使用しています：

- **Node**: 処理の単位（prep → exec → post のライフサイクル）
- **Flow**: ノードを連結したワークフロー
- **Shared**: ノード間でデータを共有するためのコンテキスト

### 基本的なワークフロー例

```python
#flow.py
from pocketflow import Flow, Node
from utils.call_llm import call_llm

class ProcessNode(Node):
    def prep(self, shared):
        # 共有データから入力を取得
        return shared["input"]

    def exec(self, prep_res):
        # LLMを呼び出して処理
        return call_llm(prep_res)

    def post(self, shared, prep_res, exec_res):
        # 結果を共有データに保存
        shared["output"] = exec_res

# フローの作成と実行
node = ProcessNode()
flow = Flow(start=node)
shared = {"input": "処理したいテキスト"}
flow.run(shared)
print(shared["output"])
```

## 🔧 カスタマイズ

### 新しいノードの追加

1. `nodes.py`に新しいNodeクラスを定義
2. `prep`, `exec`, `post`メソッドを実装
3. `flow.py`でワークフローに組み込み

### LLM設定のカスタマイズ

`utils/call_llm.py`を編集して：
- 使用するモデルの変更
- プロンプトテンプレートの調整
- レスポンス処理のカスタマイズ

### Web検索機能の拡張

`utils/search_web.py`を編集して：
- 検索エンジンの変更
- 検索結果の処理方法の調整
- フィルタリング機能の追加

## 📚 参考資料

### PocketFlow公式ドキュメント
- [GitHub Repository](https://github.com/the-pocket/pocketflow)
- [Cookbook Examples](https://github.com/the-pocket/pocketflow/tree/main/cookbook)

### 関連チュートリアル
- [基本的なチャットボット](https://github.com/The-Pocket/PocketFlow/tree/main/cookbook/pocketflow-chat)
- [非同期処理](https://github.com/The-Pocket/PocketFlow/tree/main/cookbook/pocketflow-async-basic)
- [バッチ処理](https://github.com/The-Pocket/PocketFlow/tree/main/cookbook/pocketflow-batch)

## 🤝 開発ガイドライン

### コード品質
- 日本語でのコメント記述
- 型ヒントの使用を推奨
- エラーハンドリングの実装

### テスト
- 各ノードの単体テスト作成
- ワークフロー全体の統合テスト
- モック機能を使用したテスト

### デプロイメント
- 環境変数による設定管理
- ログ出力の適切な設定
- エラー監視の実装

## 📝 ライセンス

このテンプレートプロジェクトは、元のPocketFlowフレームワークのライセンスに従います。

## 🆘 サポート

問題や質問がある場合は、以下のリソースを参照してください：

- [PocketFlow Issues](https://github.com/the-pocket/pocketflow/issues)
- [PocketFlow Discussions](https://github.com/the-pocket/pocketflow/discussions)

---

**注意**: このテンプレートは開発の出発点として設計されています。実際のプロジェクトでは、セキュリティ、パフォーマンス、エラーハンドリングなどの本番環境要件を満たすように拡張してください。
