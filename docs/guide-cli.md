# CLI 操作ガイド

> **使用シナリオ**: コマンドラインから素早くエージェントを実行・管理したい

AgentFlow CLI は、ターミナルからエージェントを実行・管理するためのコマンドラインツールです。スクリプト化や自動化に最適で、GUI なしで高速に操作できます。

> **詳細なリファレンス**: 全コマンドの詳細は [CLI リファレンス](cli.md) を参照してください。

---

## 📋 目次

1. [インストール](#インストール)
2. [基本操作](#基本操作)
3. [エージェントの実行](#エージェントの実行)
4. [エージェントの管理](#エージェントの管理)
5. [ワークフローの実行](#ワークフローの実行)
6. [Skills の管理](#skills-の管理)
7. [注意事項](#注意事項)
8. [ベストプラクティス](#ベストプラクティス)
9. [トラブルシューティング](#トラブルシューティング)

---

## 📦 インストール

### 前提条件

- Python 3.13 以上
- pip パッケージマネージャー

### ステップ 1: AgentFlow のインストール

```bash
# Conda 環境を使用する場合（推奨）
conda env create -f environment.yml
conda activate agentflow
pip install -e .

# または pip のみ
pip install -e .
```

### ステップ 2: インストール確認

```bash
# バージョン確認
agentflow --version

# ヘルプ表示
agentflow --help
```

正常にインストールされていれば、バージョン情報が表示されます。

---

## 🎯 基本操作

### コマンド構造

```bash
agentflow [OPTIONS] COMMAND [ARGS]...
```

### グローバルオプション

| オプション | 説明 | 例 |
|-----------|------|-----|
| `--version` | バージョン情報を表示 | `agentflow --version` |
| `--verbose, -v` | 詳細ログを表示 | `agentflow -v run ...` |
| `--help` | ヘルプメッセージを表示 | `agentflow --help` |

### 主要コマンド一覧

| コマンド | 説明 | 使用例 |
|---------|------|--------|
| `init` | プロジェクトを初期化 | `agentflow init my-agent` |
| `run` | エージェントを実行 | `agentflow run ./my-agent` |
| `flow` | workflow YAML を実行 | `agentflow flow run workflow.yaml` |
| `skills` | Skills を管理 | `agentflow skills list` |
| `create` | エージェントを作成 | `agentflow create agent text-processor` |
| `list` | インストール済みエージェント一覧 | `agentflow list` |
| `marketplace` | 検索・インストール | `agentflow marketplace search "text"` |
| `info` | エージェント情報を表示 | `agentflow info text-processor` |
| `chat` | 対話チャット（簡易） | `agentflow chat` |
| `studio` | Studio サーバー起動 | `agentflow studio --reload` |

---

## 🚀 エージェントの実行

### 基本的な実行

```bash
# エージェントディレクトリを指定
agentflow run ./my-agent

# 入力データを JSON 文字列で指定
agentflow run ./my-agent --input '{"input": "hello"}'

# 入力ファイルから読み込み
agentflow run ./my-agent --input input.json

# 結果をファイルに保存
agentflow run ./my-agent --input input.json --output output.json

# JSON 形式で出力
agentflow run ./my-agent --input '{"input": "hello"}' --json
```

### 入力ファイルの作成

`input.json` を作成：

```json
{
  "input": "Hello, AgentFlow!"
}
```

実行：

```bash
agentflow run ./my-agent --input input.json
```

### 出力の確認

**標準出力**（デフォルト）:

```bash
agentflow run ./my-agent --input input.json
# 結果がコンソールに表示される
```

**JSON 形式**:

```bash
agentflow run ./my-agent --input input.json --json
# JSON 形式で出力される
```

**ファイルに保存**:

```bash
agentflow run ./my-agent --input input.json --output result.json
# result.json に保存される
```

---

## 📦 エージェントの管理

### エージェントの作成

#### 方法 1: プロジェクトを初期化

```bash
# 新しいプロジェクトを作成
agentflow init my-agent
cd my-agent

# 生成されるファイル:
# - agent.yaml (メタデータ)
# - main.py (エージェント実装)
# - requirements.txt (依存関係)
```

#### 方法 2: 既存プロジェクトに追加

```bash
# プロジェクト内でエージェントを作成
agentflow create agent text-processor

# オプションを指定
agentflow create agent text-processor \
  --author "Your Name" \
  --description "Process text data" \
  --icon "📝" \
  --category "utility"
```

### エージェントの一覧表示

```bash
# インストール済みエージェント一覧
agentflow list

# 出力例:
# Installed agents:
# 1. text-processor (v1.0.0)
#    Installed: 2024-01-15
#    Location: ~/.agentflow/agents/text-processor
```

### エージェント情報の表示

```bash
# エージェントの詳細情報
agentflow info text-processor

# 出力例:
# Agent: text-processor
# Version: 1.0.0
# Author: John Doe
# Protocols: MCP, A2A
# ...
```

### マーケットプレイスから検索・インストール

```bash
# エージェントを検索
agentflow marketplace search "text processor"

# カテゴリでフィルター
agentflow marketplace search --category utility

# プロトコルでフィルター
agentflow marketplace search -p mcp -p a2a

# エージェントをインストール
agentflow marketplace install text-processor

# 強制上書き
agentflow marketplace install text-processor --force
```

### エージェントのアンインストール

```bash
agentflow marketplace uninstall text-processor
```

---

## 🔄 ワークフローの実行

`workflow.yaml` を CLI から直接実行できます。

```bash
# 最小実行
agentflow flow run workflow.yaml

# JSON で結果を表示
agentflow flow run workflow.yaml --json

# 入力を上書き
agentflow flow run workflow.yaml --input '{"text":"hello"}'

# ストリーム実行
agentflow flow run workflow.yaml --stream --json
```

最小 YAML 例:

```yaml
workflow_type: reflection
task: "Summarize this ticket"
input_data:
  text: "..."
config:
  max_iterations: 1
```

注意:
- `workflow_type` は必須です（`type` でも可）。
- `agentflow run` は従来どおり Agent 実行用です。

## 🧩 Skills の管理

外部 Skills をプロジェクトまたはグローバルに取り込めます。

```bash
# 単一 Skill をプロジェクトにマウント
agentflow skills mount ./external/my-skill --scope project

# 複数 Skill を含むディレクトリを一括マウント
agentflow skills mount ./external/skills --scope project

# 既存 Skill を上書き
agentflow skills mount ./external/my-skill --scope global --force
```

補足:
- `SOURCE` は Skill ディレクトリ、`SKILL.md`、または複数 Skill ルートを指定できます。
- `--name` は単一 Skill マウント時のみ利用できます。

---

## ⚠️ 注意事項

### 1. 環境のアクティベート

**Conda 環境を使用する場合**:

```bash
# 必ず環境をアクティベート
conda activate agentflow

# 確認
which agentflow  # agentflow コマンドのパスが表示される
```

### 2. パスの指定

- **相対パス**: `./my-agent` または `my-agent`
- **絶対パス**: `/path/to/my-agent`
- **現在のディレクトリ**: `.`

### 3. JSON 形式の入力

- **シングルクォート**で囲む（シェルエスケープのため）
- **ダブルクォート**は JSON 内で使用

**正しい例**:
```bash
agentflow run . --input '{"input": "hello"}'
```

**間違った例**:
```bash
agentflow run . --input {"input": "hello"}  # シェルエラー
agentflow run . --input "{\"input\": \"hello\"}"  # エスケープが必要
```

### 4. エラーハンドリング

- **エラー時は終了コード 1**を返します
- **詳細ログ**は `--verbose` オプションを使用

```bash
agentflow run . --input input.json --verbose
```

### 5. パフォーマンス

- **大量のデータ**を処理する場合は、入力ファイルを使用
- **長時間実行**する場合は、バックグラウンドで実行

```bash
# バックグラウンド実行
nohup agentflow run . --input input.json > output.log 2>&1 &
```

---

## 💡 ベストプラクティス

### 1. スクリプト化

**シェルスクリプト**を作成：

```bash
#!/bin/bash
# run_agent.sh

INPUT_FILE="input.json"
OUTPUT_FILE="output.json"

agentflow run ./my-agent \
  --input "$INPUT_FILE" \
  --output "$OUTPUT_FILE" \
  --json

if [ $? -eq 0 ]; then
  echo "✅ 実行成功"
else
  echo "❌ 実行失敗"
  exit 1
fi
```

### 2. 環境変数の使用

```bash
# 環境変数を設定
export AGENTFLOW_LOG_LEVEL=DEBUG
export AGENTFLOW_HOME=/custom/path

# 実行
agentflow run ./my-agent
```

### 3. エイリアスの設定

`.bashrc` または `.zshrc` に追加：

```bash
# AgentFlow エイリアス
alias af='agentflow'
alias afrun='agentflow run'
alias aflist='agentflow list'
```

使用：

```bash
afrun ./my-agent --input input.json
```

### 4. バッチ処理

**複数の入力ファイルを処理**:

```bash
#!/bin/bash
# batch_process.sh

for file in inputs/*.json; do
  output="outputs/$(basename $file)"
  agentflow run ./my-agent --input "$file" --output "$output"
done
```

### 5. ログの管理

```bash
# ログファイルに出力
agentflow run ./my-agent --input input.json --verbose 2>&1 | tee run.log

# タイムスタンプ付きログ
agentflow run ./my-agent --input input.json --verbose 2>&1 | \
  ts '[%Y-%m-%d %H:%M:%S]' | tee run.log
```

---

## 🔧 トラブルシューティング

### 問題 1: コマンドが見つからない

**症状**: `agentflow: command not found`

**解決方法**:

```bash
# Conda 環境をアクティベート
conda activate agentflow

# インストール確認
pip show agentflow

# PATH を確認
which agentflow

# 再インストール
pip install -e .
```

### 問題 2: JSON パースエラー

**症状**: `JSON decode error`

**解決方法**:

```bash
# JSON の形式を確認
echo '{"input": "hello"}' | python -m json.tool

# ファイルのエンコーディングを確認
file input.json

# 正しい形式で再作成
cat > input.json << EOF
{
  "input": "hello"
}
EOF
```

### 問題 3: エージェントが見つからない

**症状**: `Agent not found: my-agent`

**解決方法**:

```bash
# エージェントの一覧を確認
agentflow list

# パスを確認
ls -la ./my-agent

# agent.yaml が存在するか確認
ls -la ./my-agent/agent.yaml
```

### 問題 4: 権限エラー

**症状**: `Permission denied`

**解決方法**:

```bash
# ユーザーインストール
pip install --user -e .

# または仮想環境を使用
python -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate
pip install -e .
```

### 問題 5: タイムアウトエラー

**症状**: `TimeoutError`

**解決方法**:

```bash
# タイムアウトを延長（エージェントの設定で）
# agent.yaml を編集して timeout を増やす

# または環境変数で設定
export AGENTFLOW_TIMEOUT=60
agentflow run ./my-agent
```

---

## 📚 次のステップ

- [Studio UI 操作ガイド](guide-studio-ui.md) - ビジュアルエディタでの操作
- [コーディングガイド](guide-coding.md) - Python コードでの開発
- [CLI リファレンス](cli.md) - 詳細なコマンド一覧
- [API リファレンス](api.md) - Python API の詳細

---

## 🎓 実践例

### 例 1: シンプルなテキスト処理

```bash
# 1. エージェントを作成
agentflow init text-processor
cd text-processor

# 2. 入力ファイルを作成
cat > input.json << EOF
{
  "input": "Hello, World!"
}
EOF

# 3. 実行
agentflow run . --input input.json --output output.json

# 4. 結果を確認
cat output.json
```

### 例 2: バッチ処理

```bash
#!/bin/bash
# process_all.sh

for i in {1..10}; do
  echo "処理中: $i"
  agentflow run ./my-agent \
    --input "inputs/input_$i.json" \
    --output "outputs/output_$i.json"
done
```

### 例 3: エラーハンドリング付きスクリプト

```bash
#!/bin/bash
# safe_run.sh

set -e  # エラー時に終了

INPUT_FILE="${1:-input.json}"
OUTPUT_FILE="${2:-output.json}"

if [ ! -f "$INPUT_FILE" ]; then
  echo "❌ 入力ファイルが見つかりません: $INPUT_FILE"
  exit 1
fi

echo "🚀 エージェントを実行中..."
agentflow run ./my-agent \
  --input "$INPUT_FILE" \
  --output "$OUTPUT_FILE" \
  --verbose

if [ $? -eq 0 ]; then
  echo "✅ 実行成功: $OUTPUT_FILE"
else
  echo "❌ 実行失敗"
  exit 1
fi
```

---

**CLI で AI エージェントを高速に実行・管理しましょう！** ⚡
