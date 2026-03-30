# CLI-Native Skills 利用手順

## 概要

CLI-Native は `CLI-Anything` で生成した `agent-harness` を AgentFlow に import し、
project-local skill として再公開する仕組みです。

実行時は:

1. `agent-harness` を `.bizcore/cli_native/harnesses/<harness_id>` に配置
2. `pip install -e` を `conda run -n agentflow` 経由で実行
3. `.bizcore/skills/<harness_id>/SKILL.md` を自動生成
4. SkillGateway に `cli_native_<software>_execute` を自動登録

## 前提

- Python 実行は `conda run -n agentflow ...`
- import 対象は `agent-harness` ディレクトリ
- harness 側に `setup.py` または `pyproject.toml` が必要
- 可能なら `skills/SKILL.md` を含める

## 使い方

### 1. build 計画を作る

`CLI-Anything` の pinned checkout を使って build コマンドを計画します。

```bash
conda run -n agentflow bizcore skills cli-native-build \
  libreoffice \
  /absolute/path/to/software/source \
  --runtime-cli codex \
  --dry-run
```

実行モードにする場合は `--no-dry-run` を付けます。

```bash
conda run -n agentflow bizcore skills cli-native-build \
  libreoffice \
  /absolute/path/to/software/source \
  --runtime-cli codex \
  --no-dry-run
```

### 2. 生成済み harness を import する

```bash
conda run -n agentflow bizcore skills cli-native-import \
  /absolute/path/to/agent-harness \
  --harness-id libreoffice \
  --software-name LibreOffice
```

成功すると以下が作られます。

- `.bizcore/cli_native/harnesses/libreoffice`
- `.bizcore/cli_native/manifests/libreoffice.json`
- `.bizcore/skills/libreoffice/SKILL.md`

### 3. import 済み harness を確認する

```bash
conda run -n agentflow bizcore skills cli-native-list
```

Framework API でも確認できます。

- `GET /api/studios/framework/skills/cli-native`
- `GET /api/studios/framework/skills/cli-native/{harness_id}`

### 4. Skill として使う

import 後は SkillGateway に execute tool が自動登録されます。

例:

- `cli_native_libreoffice_execute`

入力は固定です。

- `subcommand`: `"<group> <command>"`
- `args`: 追加 CLI 引数配列
- `project_path`: `--project` に渡すパス
- `dry_run`: 実行せず計画だけ返す

内部では常に `--json` が強制され、未許可の command / option は弾かれます。

## API 利用例

### import

```bash
curl -X POST http://localhost:8000/api/studios/framework/skills/cli-native/import \
  -H "Content-Type: application/json" \
  -d '{
    "harness_path": "/absolute/path/to/agent-harness",
    "harness_id": "libreoffice",
    "software_name": "LibreOffice",
    "force": true
  }'
```

### build

```bash
curl -X POST http://localhost:8000/api/studios/framework/skills/cli-native/build \
  -H "Content-Type: application/json" \
  -d '{
    "software_name": "LibreOffice",
    "source_path": "/absolute/path/to/software/source",
    "runtime_cli": "codex",
    "dry_run": true
  }'
```

## 実行例

LibreOffice harness が import 済みなら、tool 呼び出しは次の形です。

```json
{
  "subcommand": "document new",
  "args": ["--type", "writer", "--output", "demo.odt"],
  "project_path": "/absolute/path/to/workspace",
  "dry_run": true
}
```

次に:

```json
{
  "subcommand": "writer add-heading",
  "args": ["--file", "demo.odt", "--text", "Quarterly Report"],
  "project_path": "/absolute/path/to/workspace",
  "dry_run": true
}
```

最後に:

```json
{
  "subcommand": "export render",
  "args": ["--file", "demo.odt", "--format", "pdf", "--output", "demo.pdf"],
  "project_path": "/absolute/path/to/workspace",
  "dry_run": true
}
```

`dry_run` を `false` にすると実行します。

## 注意点

- `list/import/validate/help-introspect` は低リスク寄りですが、生成 CLI 実行は高リスク扱いです
- gateway 登録時は `requires_confirmation=True`
- `CLI-Anything` 自体は vendor しておらず、managed cache に pinned ref を checkout します
- まずは LibreOffice のようにスクリプト可能なソフトで使うのが前提です

