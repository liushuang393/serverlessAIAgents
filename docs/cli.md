# AgentFlow CLI リファレンス

AgentFlow CLI の全コマンドの詳細なリファレンスです。

> **注意**: 実用的な操作ガイドは [CLI 操作ガイド](guide-cli.md) を参照してください。

## グローバルオプション

すべてのコマンドで使用可能なオプション：

```bash
agentflow [OPTIONS] COMMAND [ARGS]...
```

**オプション:**

- `--version`: バージョン情報を表示
- `--verbose, -v`: 詳細ログを表示
- `--help`: ヘルプメッセージを表示

## コマンド一覧

### `init` - プロジェクト初期化

新しい AgentFlow プロジェクトを作成します。

```bash
agentflow init <project-name> [OPTIONS]
```

**引数:**

- `project-name`: プロジェクト名（kebab-case）

**オプション:**

- `--protocols <protocols>`: 有効化するプロトコル（カンマ区切り）
  - 例: `--protocols mcp,a2a,agui`
  - デフォルト: すべて有効
- `--author <name>`: 作者名
  - デフォルト: システムユーザー名
- `--description <text>`: プロジェクト説明
  - デフォルト: "A new AgentFlow project"
- `--dry-run`: ファイルを作成せずにプレビュー
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# 基本的な使い方
agentflow init my-agent

# プロトコルを指定
agentflow init my-agent --protocols mcp,a2a

# 作者と説明を指定
agentflow init my-agent --author "John Doe" --description "My first agent"

# ドライラン（プレビュー）
agentflow init my-agent --dry-run
```

**生成されるファイル:**

```
my-agent/
├── agent.yaml          # エージェントメタデータ
├── main.py             # エージェント実装
├── requirements.txt    # 依存関係
├── .gitignore          # Git 除外設定
└── README.md           # プロジェクト説明
```

---

### `create` - エージェント作成

既存プロジェクト内に新しいエージェントを作成します。

```bash
agentflow create agent <agent-name> [OPTIONS]
```

**引数:**

- `agent-name`: エージェント名（kebab-case）

**オプション:**

- `--interactive, -i`: 対話モードで作成
- `--mcp / --no-mcp`: MCP プロトコルを有効化/無効化
- `--a2a / --no-a2a`: A2A プロトコルを有効化/無効化
- `--agui / --no-agui`: AG-UI プロトコルを有効化/無効化
- `--author <name>`: 作者名
- `--description <text>`: エージェント説明
- `--icon <emoji>`: アイコン（絵文字）
- `--category <category>`: カテゴリ
  - 選択肢: `utility`, `data`, `communication`, `automation`, `analysis`, `other`
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# 基本的な使い方
agentflow create agent text-processor

# 対話モード
agentflow create agent text-processor --interactive

# プロトコルを指定
agentflow create agent text-processor --mcp --no-a2a --no-agui

# カスタマイズ
agentflow create agent text-processor \
  --author "John Doe" \
  --description "Process text data" \
  --icon "📝" \
  --category "utility"
```

**生成されるファイル:**

```
text-processor/
├── agent.yaml          # エージェントメタデータ
└── main.py             # エージェント実装
```

---

### `run` - エージェント実行

エージェントを実行します。

```bash
agentflow run <agent-path> [OPTIONS]
```

**引数:**

- `agent-path`: エージェントディレクトリのパス

**オプション:**

- `--input <json>`: 入力データ（JSON 文字列または JSON ファイルパス）
- `--output <file>`: 出力ファイルパス
- `--json`: JSON 形式で出力
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# 基本的な使い方
agentflow run ./my-agent

# JSON 文字列で入力
agentflow run ./my-agent --input '{"text": "hello"}'

# JSON ファイルから入力
agentflow run ./my-agent --input input.json

# 結果をファイルに保存
agentflow run ./my-agent --input input.json --output output.json

# JSON 形式で出力
agentflow run ./my-agent --input '{"text": "hello"}' --json
```

**入力ファイル例 (input.json):**

```json
{
  "text": "hello world",
  "operation": "upper"
}
```

**出力例:**

```json
{
  "result": "HELLO WORLD",
  "original": "hello world"
}
```

---

### `search` - マーケットプレイス検索

マーケットプレイスからエージェントを検索します。

```bash
agentflow search [OPTIONS]
```

**オプション:**

- `--query <text>`: 検索クエリ
- `--category <category>`: カテゴリでフィルター
- `--protocols <protocols>`: プロトコルでフィルター（カンマ区切り）
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# 全エージェントを表示
agentflow search

# キーワードで検索
agentflow search --query "text processor"

# カテゴリでフィルター
agentflow search --category utility

# プロトコルでフィルター
agentflow search --protocols mcp,a2a

# 複合検索
agentflow search --query "text" --category utility --protocols mcp
```

**出力例:**

```
Found 3 agents:

1. text-processor (v1.0.0)
   Author: John Doe
   Category: utility
   Protocols: MCP, A2A
   Description: Process text data

2. data-analyzer (v2.1.0)
   Author: Jane Smith
   Category: analysis
   Protocols: MCP, A2A, AG-UI
   Description: Analyze data patterns

3. file-manager (v1.5.0)
   Author: Bob Johnson
   Category: utility
   Protocols: MCP
   Description: Manage files and directories
```

---

### `install` - エージェントインストール

マーケットプレイスからエージェントをインストールします。

```bash
agentflow install <agent-id> [OPTIONS]
```

**引数:**

- `agent-id`: エージェント ID

**オプション:**

- `--force, -f`: 既存のエージェントを上書き
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# エージェントをインストール
agentflow install text-processor

# 強制上書き
agentflow install text-processor --force
```

**インストール先:**

```
~/.agentflow/agents/text-processor/
├── agent.yaml
└── main.py
```

---

### `uninstall` - エージェントアンインストール

インストール済みエージェントをアンインストールします。

```bash
agentflow uninstall <agent-id> [OPTIONS]
```

**引数:**

- `agent-id`: エージェント ID

**オプション:**

- `--help`: ヘルプメッセージを表示

**例:**

```bash
agentflow uninstall text-processor
```

---

### `list` - インストール済みエージェント一覧

インストール済みエージェントの一覧を表示します。

```bash
agentflow list [OPTIONS]
```

**オプション:**

- `--help`: ヘルプメッセージを表示

**例:**

```bash
agentflow list
```

**出力例:**

```
Installed agents:

1. text-processor (v1.0.0)
   Installed: 2024-01-15
   Location: ~/.agentflow/agents/text-processor

2. data-analyzer (v2.1.0)
   Installed: 2024-01-20
   Location: ~/.agentflow/agents/data-analyzer
```

---

### `info` - エージェント情報表示

エージェントの詳細情報を表示します。

```bash
agentflow info <agent-id> [OPTIONS]
```

**引数:**

- `agent-id`: エージェント ID

**オプション:**

- `--help`: ヘルプメッセージを表示

**例:**

```bash
agentflow info text-processor
```

**出力例:**

```
Agent: text-processor
Version: 1.0.0
Author: John Doe
License: MIT
Category: utility
Description: Process text data

Protocols:
  - MCP: enabled
  - A2A: enabled (http://localhost:8000)
  - AG-UI: disabled

Inputs:
  - text (string, required): Input text to process
  - operation (string, optional): Operation to perform

Outputs:
  - result (string): Processed result

Skills:
  - process: Process text data
  - analyze: Analyze text patterns

Installation:
  Location: ~/.agentflow/agents/text-processor
  Installed: 2024-01-15
```

---

### `skills` - Skills 管理

Skills を管理します。Claude Code Skills 形式と完全互換です。

```bash
agentflow skills <subcommand> [OPTIONS]
```

#### `skills list` - Skills 一覧

```bash
agentflow skills list [OPTIONS]
```

**オプション:**

- `--learned, -l`: 学習済み Skills のみ表示
- `--project, -p`: プロジェクト Skills のみ表示
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# 全 Skills を表示
agentflow skills list

# 学習済み Skills のみ
agentflow skills list --learned
```

**出力例:**

```
Skills
┏━━━━━━━━━━━━━━━┳━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━┓
┃ Name          ┃ Version ┃ Description              ┃ Triggers             ┃ Learned ┃
┡━━━━━━━━━━━━━━━╇━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━┩
│ pdf-extractor │ 1.0.0   │ Extract text from PDF... │ pdf, extract text... │         │
└───────────────┴─────────┴──────────────────────────┴──────────────────────┴─────────┘
Total: 1 skills
```

#### `skills show` - Skill 詳細

```bash
agentflow skills show <name>
```

**引数:**

- `name`: Skill 名

**例:**

```bash
agentflow skills show pdf-extractor
```

#### `skills create` - Skill 作成

```bash
agentflow skills create <name> [OPTIONS]
```

**引数:**

- `name`: Skill 名 (kebab-case)

**オプション:**

- `--description, -d`: Skill の説明
- `--triggers, -t`: トリガーワード（カンマ区切り）
- `--scope, -s`: 保存先 (`project` | `global`)
- `--interactive, -i`: 対話モードで作成
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# 基本的な使い方
agentflow skills create my-skill

# 対話モード
agentflow skills create my-skill --interactive

# オプション指定
agentflow skills create my-skill -d "My skill" -t "my,skill" -s global
```

#### `skills validate` - Skill 検証

```bash
agentflow skills validate <path> [OPTIONS]
```

**引数:**

- `path`: Skill ディレクトリまたは SKILL.md ファイルのパス

**オプション:**

- `--strict`: 厳格モード（警告もエラーとして扱う）
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# ディレクトリを検証
agentflow skills validate .agentflow/skills/my-skill

# ファイルを検証
agentflow skills validate ./SKILL.md --strict
```

#### `skills search` - Skills 検索

```bash
agentflow skills search <query> [OPTIONS]
```

**引数:**

- `query`: 検索クエリ（自然言語）

**オプション:**

- `--top, -n`: 表示する結果数（デフォルト: 5）
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# 自然言語で検索
agentflow skills search "PDF からテキスト抽出"

# 結果数を指定
agentflow skills search "excel spreadsheet" --top 10
```

**出力例:**

```
Search Results for: 'pdf text extraction'
┏━━━━┳━━━━━━━━━━━━━━━┳━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ #  ┃ Name          ┃ Score ┃ Reason                         ┃ Description            ┃
┡━━━━╇━━━━━━━━━━━━━━━╇━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╇━━━━━━━━━━━━━━━━━━━━━━━━┩
│ 1  │ pdf-extractor │ 0.90  │ Matched by: trigger 'pdf'...   │ Extract text, tables...│
└────┴───────────────┴───────┴────────────────────────────────┴────────────────────────┘
```

#### `skills delete` - Skill 削除

```bash
agentflow skills delete <name> [OPTIONS]
```

**引数:**

- `name`: Skill 名

**オプション:**

- `--scope, -s`: 削除対象 (`learned` | `project` | `global`)
- `--force, -f`: 確認なしで削除
- `--help`: ヘルプメッセージを表示

**例:**

```bash
# 学習済み Skill を削除
agentflow skills delete my-skill

# プロジェクト Skill を強制削除
agentflow skills delete my-skill --scope project --force
```

---

## 環境変数

AgentFlow CLI は以下の環境変数をサポートします：

- `AGENTFLOW_HOME`: AgentFlow ホームディレクトリ
  - デフォルト: `~/.agentflow`
- `AGENTFLOW_MARKETPLACE_URL`: マーケットプレイス API URL
  - デフォルト: `https://marketplace.agentflow.dev`
- `AGENTFLOW_LOG_LEVEL`: ログレベル
  - 選択肢: `DEBUG`, `INFO`, `WARNING`, `ERROR`
  - デフォルト: `INFO`

**例:**

```bash
export AGENTFLOW_HOME=/custom/path
export AGENTFLOW_LOG_LEVEL=DEBUG
agentflow list
```

---

## 設定ファイル

### グローバル設定

`~/.agentflow/config.yaml`:

```yaml
marketplace:
  url: https://marketplace.agentflow.dev
  cache_ttl: 3600

mcp:
  config_path: ~/.agentflow/mcp_config.yaml

logging:
  level: INFO
  format: "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
```

### MCP 設定

`~/.agentflow/mcp_config.yaml`:

```yaml
servers:
  filesystem:
    command: npx
    args:
      - -y
      - "@modelcontextprotocol/server-filesystem"
      - /path/to/directory
    enabled: true
```

---

## トラブルシューティング

### コマンドが見つからない

```bash
# インストールを確認
pip show agentflow

# PATH を確認
echo $PATH

# 再インストール
pip install --upgrade agentflow
```

### 権限エラー

```bash
# ユーザーインストール
pip install --user agentflow

# または仮想環境を使用
python -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate
pip install agentflow
```

### 詳細ログを表示

```bash
agentflow --verbose <command>
```

---

## 次のステップ

- [クイックスタートガイド](quickstart.md) - 基本的な使い方
- [API リファレンス](api.md) - Python API の詳細
- [プロトコルガイド](protocols.md) - MCP/A2A/AG-UI の詳細
