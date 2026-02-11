# AgentFlow CLI リファレンス

AgentFlow CLI のコマンド仕様（実装準拠）です。

> **操作の流れ**（ユースケース別）は [CLI 操作ガイド](guide-cli.md) を参照してください。

## グローバルオプション

```bash
agentflow [OPTIONS] COMMAND [ARGS]...
```

- `--version`: バージョン表示
- `--verbose, -v`: 詳細ログ
- `--help`: ヘルプ

## コマンド一覧（トップレベル）

- `run`: エージェントを実行
- `init`: 新規プロジェクトを初期化
- `create`: エージェント等を作成（サブコマンド）
- `marketplace`: 検索/インストール（サブコマンド）
- `list`: インストール済みエージェントを一覧表示
- `info`: エージェント情報表示（現状未実装）
- `chat`: 対話チャット（簡易）
- `studio`: Studio サーバー起動
- `sandbox`: サンドボックス関連（サブコマンド）
- `skills`: Skills 管理（サブコマンド）
- `template`: テンプレート操作（サブコマンド）
- `workspace`: ワークスペース操作（サブコマンド）

## `init` - プロジェクト初期化

```bash
agentflow init PROJECT_NAME [OPTIONS]
```

- `PROJECT_NAME`: kebab-case 推奨（自動で変換されます）
- `--protocols, -p`: 有効化するプロトコル（複数指定可、デフォルト: すべて）
- `--author, -a`: 作者名（デフォルト: `AgentFlow User`）
- `--description, -d`: 説明（デフォルト: `A new AgentFlow agent`）
- `--dry-run`: ファイルを作成しない

例:

```bash
agentflow init my-agent
agentflow init my-agent -p mcp -p a2a
agentflow init my-agent --dry-run
```

生成物（最小）:

```
my-agent/
├── agent.yaml
├── main.py
├── README.md
├── requirements.txt
├── .gitignore
└── tests/
```

## `create agent` - エージェント作成

```bash
agentflow create agent AGENT_NAME [OPTIONS]
```

- `--interactive, -I`: 対話モード
- `--mcp/--no-mcp`, `--a2a/--no-a2a`, `--agui/--no-agui`: プロトコル有効/無効
- `--author, -a`, `--description, -d`, `--icon, -i`, `--category, -c`: メタ情報

例:

```bash
agentflow create agent text-processor
agentflow create agent text-processor --interactive
agentflow create agent text-processor --no-agui
```

## `run` - エージェント実行

```bash
agentflow run AGENT_PATH [OPTIONS]
```

- `AGENT_PATH`: ディレクトリまたはファイル（存在必須）
- `--input, -i`: JSON 文字列または JSON ファイルパス
- `--output, -o`: 出力ファイル
- `--json`: JSON 形式で出力
- `--agent-name, -n`: `@agent` で定義した Agent 名（この場合 `agent.yaml` は不要）
- `--stream, -s`: ストリームモード（進捗を逐次表示）

例:

```bash
# agent.yaml ベース
agentflow run ./my-agent --input '{"input": "hello"}' --json

# @agent デコレータベース
agentflow run . --agent-name QAAgent --input '{"question": "hello"}' --json

# ストリームモード
agentflow run ./my-agent --input input.json --stream
```

## `marketplace` - 検索/インストール

```bash
agentflow marketplace search [QUERY] [OPTIONS]
agentflow marketplace install AGENT_ID [OPTIONS]
agentflow marketplace uninstall AGENT_ID [OPTIONS]
```

- `search`: `--category/-c`, `--protocols/-p`（複数指定可）, `--limit/-l`
- `install`: `--version/-v`, `--force/-f`
- `uninstall`: `--yes/-y`

## `list` / `info`

```bash
agentflow list
agentflow info AGENT_ID
```

Note:
- `list` は主に `@agent` で登録された Agent を表示します（agent.yaml レジストリは将来実装）。
- `info` は現状未実装です。

## `chat`

```bash
agentflow chat [--system/-s TEXT] [--model/-m MODEL]
```

## `studio`

```bash
agentflow studio [--host/-h HOST] [--port/-p PORT] [--reload]
```

## 追加コマンド（サブコマンド）

以下は実装が頻繁に変わるため、最新版は `--help` を参照してください。

- `agentflow sandbox --help`
- `agentflow skills --help`（参考: `docs/skills-guide.md`, `docs/guide-skills.md`）
- `agentflow template --help`
- `agentflow workspace --help`
