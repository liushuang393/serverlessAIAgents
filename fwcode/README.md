# RAGシステム (Retrieval-Augmented Generation)

このプロジェクトは、FastAPIとLlamaIndexを使用したRAG（Retrieval-Augmented Generation）システムです。ドキュメントのベクトル化、検索、要約機能を提供します。

## 🚀 機能

- **ドキュメントインデックス作成**: PDF、CSV、テキストファイルからベクトルインデックスを作成
- **セマンティック検索**: 自然言語クエリによるドキュメント検索
- **ドキュメント要約**: 簡潔または詳細な要約の生成
- **インデックス管理**: 複数のインデックスの管理と一覧表示
- **RESTful API**: FastAPIによる標準的なWeb API
- **Docker対応**: コンテナ化による簡単なデプロイ

## 📁 ファイル構成

```
fwcode/
├── mcp_rag_server.py      # RAGサーバー（FastAPI）
├── mcp_rag_client.py      # RAGクライアント（HTTP）
├── mcp_config.json        # MCP設定ファイル
├── doc_config.json        # ドキュメント処理設定
├── requirements.txt       # Python依存関係
├── Dockerfile            # Dockerイメージ定義
├── docker-compose.yml    # Docker Compose設定
├── test_rag_system.py    # システムテストスクリプト
├── simple_test.py        # 簡単なテスト（依存関係なし）
└── README.md            # このファイル
```

## 🛠️ セットアップ

### 1. 依存関係のインストール

```bash
pip install -r requirements.txt
```

### 2. 環境変数の設定

OpenAI APIキーを設定してください：

```bash
export OPENAI_API_KEY="your-openai-api-key-here"
```

### 3. サーバーの起動

```bash
python mcp_rag_server.py
```

サーバーは `http://localhost:8000` で起動します。

## 📖 使用方法

### API エンドポイント

#### 1. ヘルスチェック
```bash
GET /health
```

#### 2. インデックス作成
```bash
POST /create_index
Content-Type: application/json

{
    "file_path": "path/to/document.pdf",
    "index_name": "my-document",
    "chunk_size": 1024,
    "chunk_overlap": 200,
    "force_recreate": false
}
```

#### 3. ドキュメント検索
```bash
POST /query
Content-Type: application/json

{
    "index_name": "my-document",
    "query": "検索したい内容",
    "top_k": 5
}
```

#### 4. ドキュメント要約
```bash
POST /summary
Content-Type: application/json

{
    "index_name": "my-document",
    "summary_type": "brief"
}
```

#### 5. インデックス一覧
```bash
GET /indices
```

### クライアントの使用例

```python
import asyncio
from mcp_rag_client import RAGClient

async def main():
    client = RAGClient()

    # インデックス作成
    result = await client.create_index(
        file_path="document.pdf",
        index_name="test-doc"
    )
    print(result)

    # 検索
    search_result = await client.query_document(
        index_name="test-doc",
        query="主要なポイントは何ですか？"
    )
    print(search_result)

    await client.close()

asyncio.run(main())
```

## 🐳 Docker使用方法

### 1. Dockerイメージのビルド

```bash
docker build -t rag-system .
```

### 2. Docker Composeでの起動

```bash
docker-compose up -d
```

## 🧪 テスト

### 1. システムテスト

```bash
python test_rag_system.py
```

### 2. 簡単なテスト（依存関係なし）

```bash
python simple_test.py
```

## ⚙️ 設定

### doc_config.json
```json
{
  "default_chunk_size": 1024,
  "default_chunk_overlap": 200,
  "supported_formats": ["pdf", "csv", "txt", "docx"],
  "embedding_model": "text-embedding-3-small",
  "llm_model": "gpt-4o-mini",
  "max_cache_size": 1000
}
```

### mcp_config.json
```json
{
  "server_url": "http://localhost:8000",
  "available_indices": ["tax-beijing", "tax-shanghai"],
  "document_descriptions": {
    "tax-beijing": "北京市税収政策ファイル集合",
    "tax-shanghai": "上海市税収政策ファイル集合"
  }
}
```

## 🔧 トラブルシューティング

### よくある問題

1. **ModuleNotFoundError**: 依存関係が正しくインストールされているか確認
2. **OpenAI API エラー**: API キーが正しく設定されているか確認
3. **ファイル読み込みエラー**: ファイルパスとアクセス権限を確認

### ログの確認

サーバーのログを確認して詳細なエラー情報を取得：

```bash
python mcp_rag_server.py
```

## 📝 ライセンス

このプロジェクトはMITライセンスの下で公開されています。

## 🤝 貢献

プルリクエストやイシューの報告を歓迎します。

## 📞 サポート

問題や質問がある場合は、GitHubのIssuesページでお知らせください。
