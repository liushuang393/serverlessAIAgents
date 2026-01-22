# AgentFlow インストール・セットアップガイド

> **対象**: 初心者向けの詳細な手順書

---

## 📋 目次

1. [前提条件](#前提条件)
2. [AgentFlow フレームワークのインストール](#agentflow-フレームワークのインストール)
3. [decision_governance_engine のセットアップ](#decision_governance_engine-のセットアップ)
4. [環境変数の設定](#環境変数の設定)
5. [アプリケーションの起動](#アプリケーションの起動)
6. [動作確認](#動作確認)
7. [トラブルシューティング](#トラブルシューティング)

---

## 前提条件

### 必要なソフトウェア

| ソフトウェア | バージョン | 説明 |
|------------|----------|------|
| **Python** | 3.13+ | Python プログラミング言語 |
| **Node.js** | 22+ | フロントエンド開発用（React が必要） |
| **npm** または **pnpm** | 最新版 | Node.js パッケージマネージャー |
| **Git** | 最新版 | コードの取得用（オプション） |

### インストール確認

ターミナル（コマンドプロンプト）で以下を実行して確認：

```bash
# Python のバージョン確認
python --version
# または
python3 --version
# 出力例: Python 3.13.0

# Node.js のバージョン確認
node --version
# 出力例: v18.17.0

# npm のバージョン確認
npm --version
# 出力例: 9.6.7
```

**注意**: Python 3.13 未満の場合は、[Python公式サイト](https://www.python.org/downloads/)から最新版をインストールしてください。

---

## AgentFlow フレームワークのインストール

### ステップ 1: プロジェクトの取得

```bash
# Git でクローンする場合
git clone https://github.com/liushuang393/serverlessAIAgents.git
cd serverlessAIAgents

# または ZIP でダウンロードした場合
# 解凍してフォルダに移動
cd serverlessAIAgents
```

### ステップ 2: Python 仮想環境の作成（推奨）

**Windows:**
```bash
# 仮想環境を作成
python -m venv venv

# 仮想環境を有効化
venv\Scripts\activate
```

**macOS / Linux:**
```bash
# 仮想環境を作成
python3 -m venv venv

# 仮想環境を有効化
source venv/bin/activate
```

**確認**: ターミナルの先頭に `(venv)` が表示されれば成功です。

### ステップ 3: AgentFlow のインストール

```bash
# プロジェクトルートで実行
pip install -e .

# または、開発用依存関係も含める場合
pip install -e ".[dev]"
```

**インストール確認:**
```bash
# AgentFlow がインストールされたか確認
python -c "import agentflow; print(agentflow.__version__)"
# 出力例: 0.2.0

# CLI コマンドが使えるか確認
agentflow --help
```

---

## decision_governance_engine のセットアップ

### ステップ 1: バックエンド依存関係の確認

AgentFlow をインストール済みであれば、追加のインストールは不要です。

**確認:**
```bash
# FastAPI がインストールされているか確認
python -c "import fastapi; print(fastapi.__version__)"
# 出力例: 0.123.0
```

### ステップ 2: フロントエンド依存関係のインストール

```bash
# フロントエンドディレクトリに移動　例：
cd apps/decision_governance_engine/frontend

# 依存関係をインストール
npm install
# または
pnpm install

# インストール確認
npm list --depth=0
```

**インストールされる主なパッケージ:**
- React 18
- TypeScript
- Vite（ビルドツール）
- Tailwind CSS
- Zustand（状態管理）

---

## 環境変数の設定

### ステップ 1: .env ファイルの作成

プロジェクトルート（`serverlessAIAgents/`）に `.env` ファイルを作成：

```bash
# プロジェクトルートで実行
touch .env
# Windows の場合: type nul > .env
```

### ステップ 2: LLM API キーの設定

`.env` ファイルに以下を記述：

```env
# ============================================
# LLM Provider 設定（必須）
# ============================================

# OpenAI を使用する場合
OPENAI_API_KEY=sk-your-openai-api-key-here

# または Anthropic Claude を使用する場合
ANTHROPIC_API_KEY=sk-ant-your-anthropic-api-key-here

# または Google Gemini を使用する場合
GOOGLE_API_KEY=your-google-api-key-here

# ============================================
# モデル選択（オプション）
# ============================================
# LLM_MODEL=gpt-4o-mini          # OpenAI
# LLM_MODEL=claude-3-5-sonnet-20241022  # Anthropic
# LLM_MODEL=gemini-1.5-pro       # Google

# ============================================
# デバッグモード（オプション）
# ============================================
# DEBUG=true

# ============================================
# 知識ストア設定（オプション - 長期記憶機能）
# ============================================
# バックエンド選択:
#   auto   - 自動検出（memvid-sdkがあれば使用、なければmemoryにフォールバック）
#   memvid - Rust製高性能RAG（.mv2ファイル、セマンティック+BM25検索）
#   memory - インメモリ実装（BM25検索、JSON永続化可能、依存なし）
# KNOWLEDGE_BACKEND=auto

# 知識データの保存先パス
# KNOWLEDGE_STORAGE_PATH=memory/knowledge

# 自動永続化（終了時に保存）
# KNOWLEDGE_AUTO_PERSIST=true
```

**重要**:
- API キーは必ず `.env` ファイルに設定してください
- `.env` ファイルは Git にコミットしないでください（既に `.gitignore` に含まれています）

### ステップ 3: 環境変数の取得方法

#### OpenAI API キー
1. [OpenAI Platform](https://platform.openai.com/) にアクセス
2. アカウント作成・ログイン
3. 「API Keys」→「Create new secret key」
4. キーをコピーして `.env` に貼り付け

#### Anthropic API キー
1. [Anthropic Console](https://console.anthropic.com/) にアクセス
2. アカウント作成・ログイン
3. 「API Keys」→「Create Key」
4. キーをコピーして `.env` に貼り付け

---

## アプリケーションの起動

### 方法 1: フロントエンド開発サーバーの起動

**ターミナル 1（バックエンド）:**

```bash
# プロジェクトルートで実行

# FastAPI サーバーを起動
uvicorn apps.decision_governance_engine.api:app --reload --host 0.0.0.0 --port 8000

# または、api.py を直接実行
python api.py
```

**起動確認:**
- ブラウザで `http://localhost:8000/docs` にアクセス
- Swagger UI が表示されれば成功

**API エンドポイント:**
- `http://localhost:8000/api/health` - ヘルスチェック
- `http://localhost:8000/api/agents` - Agent 一覧
- `http://localhost:8000/docs` - API ドキュメント


**ターミナル 2（フロントエンド）:**

```bash
# フロントエンドディレクトリで実行
cd apps/decision_governance_engine/frontend

# 開発サーバーを起動
npm run dev
# または
pnpm dev
```

**起動確認:**
- ブラウザで `http://localhost:5173` にアクセス（Vite のデフォルトポート）
- 画面が表示されれば成功

**注意**: フロントエンドはバックエンド API（`http://localhost:8000`）に接続します。

### 方法 2: CLI モードで実行（フロントエンド不要）

**ターミナル:**

```bash
# プロジェクトルートで実行
cd apps/decision_governance_engine

# インタラクティブモード
python -m apps.decision_governance_engine.main --interactive

# または、直接質問を指定
python -m apps.decision_governance_engine.main "新規事業への投資判断をしたい"
```

---

## 動作確認

### 1. バックエンド API の確認

```bash
# ヘルスチェック
curl http://localhost:8000/api/health

# Agent 一覧取得
curl http://localhost:8000/api/agents
```

### 2. フロントエンドの確認

1. ブラウザで `http://localhost:5173` を開く
2. ログイン画面が表示される
3. テストユーザーでログイン（実装により異なる）
4. 意思決定質問を入力して実行

### 3. CLI モードの確認

```bash
# インタラクティブモードで起動
python -m apps.decision_governance_engine.main --interactive

# プロンプトが表示されたら質問を入力
質問> 新規事業への投資判断をしたい
```

---

## トラブルシューティング

### 問題 1: Python のバージョンエラー

**エラー:**
```
Python 3.13+ required, but you have Python 3.10
```

**解決方法:**
1. [Python 公式サイト](https://www.python.org/downloads/)から Python 3.13+ をインストール
2. 仮想環境を再作成:
   ```bash
   rm -rf venv
   python3.13 -m venv venv
   source venv/bin/activate  # Windows: venv\Scripts\activate
   pip install -e .
   ```

### 問題 2: API キーが見つからない

**エラー:**
```
AgentFlowError: LLM API key not found
```

**解決方法:**
1. `.env` ファイルがプロジェクトルートにあるか確認
2. `.env` ファイルに正しい API キーが設定されているか確認
3. 環境変数が読み込まれているか確認:
   ```bash
   python -c "import os; print(os.getenv('OPENAI_API_KEY'))"
   ```

### 問題 3: ポートが既に使用されている

**エラー:**
```
Address already in use: 8000
```

**解決方法:**
1. 別のポートを指定:
   ```bash
   python -m uvicorn api:app --reload --port 8001
   ```
2. または、使用中のプロセスを終了:
   ```bash
   # Windows
   netstat -ano | findstr :8000
   taskkill /PID <PID番号> /F
   
   # macOS / Linux
   lsof -ti:8000 | xargs kill -9
   ```

### 問題 4: npm install が失敗する

**エラー:**
```
npm ERR! code ERESOLVE
```

**解決方法:**
1. `package-lock.json` を削除して再インストール:
   ```bash
   rm package-lock.json
   npm install
   ```
2. または、`--legacy-peer-deps` を使用:
   ```bash
   npm install --legacy-peer-deps
   ```

### 問題 5: フロントエンドがバックエンドに接続できない

**エラー:**
```
Failed to fetch: http://localhost:8000/api/...
```

**解決方法:**
1. バックエンドが起動しているか確認
2. CORS 設定を確認（`api.py` の `allow_origins`）
3. フロントエンドの API クライアント設定を確認:
   ```typescript
   // apps/decision_governance_engine/frontend/src/api/client.ts
   // baseUrl が正しいか確認
   ```

### 問題 6: モジュールが見つからない

**エラー:**
```
ModuleNotFoundError: No module named 'agentflow'
```

**解決方法:**
1. 仮想環境が有効化されているか確認
2. AgentFlow がインストールされているか確認:
   ```bash
   pip list | grep agentflow
   ```
3. 再インストール:
   ```bash
   pip install -e .
   ```

---

## よくある質問（FAQ）

### Q1: どの LLM プロバイダーを使えばいいですか？

**A:** プロジェクトの要件に応じて選択してください：

| プロバイダー | 特徴 | 推奨用途 |
|------------|------|---------|
| **OpenAI** | 最も一般的、安定 | 汎用的な用途 |
| **Anthropic** | 高品質、長文対応 | 複雑な分析が必要な場合 |
| **Google** | コスト効率 | 大量処理が必要な場合 |

`.env` ファイルで API キーを設定すれば、自動的に検出されます。

### Q2: フロントエンドなしで使えますか？

**A:** はい、CLI モードで使用できます：

```bash
python -m apps.decision_governance_engine.main --interactive
```

### Q3: 本番環境でのデプロイ方法は？

**A:** 
1. **バックエンド**: Docker コンテナまたはサーバーレス（AWS Lambda、Vercel など）
2. **フロントエンド**: `npm run build` でビルド後、静的ホスティング（Vercel、Netlify など）

詳細は `agentflow/deploy/` モジュールを参照してください。

### Q4: 他のアプリ（market_trend_monitor など）も同じ手順ですか？

**A:** 基本的に同じです。各アプリの `README.md` を確認してください。

### Q5: 高性能な長期記憶（Memvid）を使うには？

**A:** Memvid は Rust 製の高性能 RAG ライブラリです。以下の手順でインストール：

```bash
# Memvid オプション依存をインストール
pip install agentflow[memvid]
```

インストール後、自動的に Memvid が使用されます。インストールされていない場合は、メモリ内実装（BM25 検索）にフォールバックします。

**使用例：**

```python
from agentflow import get_knowledge_manager

# 知識マネージャーを取得（環境に応じて自動選択）
manager = get_knowledge_manager()
await manager.start()

# 知識を追加
await manager.add(
    content="FAQ: よくある質問の回答",
    title="FAQ項目",
    tags=["faq", "support"]
)

# 知識を検索
results = await manager.query("質問内容")

# プロンプト用コンテキスト生成
context = manager.format_context(results)

await manager.stop()
```

---

## 次のステップ

インストールが完了したら：

1. **チュートリアル**: `docs/quickstart.md` を参照
2. **API ドキュメント**: `http://localhost:8000/docs` で確認
3. **サンプルコード**: `examples/` ディレクトリを参照
4. **開発ガイド**: `CONTRIBUTING.md` を参照

---

## サポート

問題が解決しない場合：

1. **GitHub Issues**: [Issues](https://github.com/liushuang393/serverlessAIAgents/issues) で報告
2. **ドキュメント**: `docs/` ディレクトリを確認
3. **ログ確認**: `DEBUG=true` を設定して詳細ログを確認

---

**最終更新**: 2025-01-20  
**バージョン**: 1.0.0

