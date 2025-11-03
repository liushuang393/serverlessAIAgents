# テンプレートガイド

AgentFlow のテンプレートシステムを使用して、プロジェクトを素早く開始できます。

## 📋 目次

1. [テンプレートとは](#テンプレートとは)
2. [利用可能なテンプレート](#利用可能なテンプレート)
3. [テンプレートの使用](#テンプレートの使用)
4. [カスタムテンプレートの作成](#カスタムテンプレートの作成)

---

## テンプレートとは

テンプレートは、特定のユースケースに最適化されたプロジェクト構造とコードを提供します。

### テンプレートの利点

- **高速な開発開始**: ボイラープレートコードを自動生成
- **ベストプラクティス**: 実証済みのパターンを使用
- **カスタマイズ可能**: パラメーターで柔軟に調整
- **学習リソース**: 実装例として活用

---

## 利用可能なテンプレート

### 1. Invoice Processor

**説明**: PDF 請求書を処理するエージェント

**ユースケース**:
- PDF からデータ抽出
- 請求書情報の構造化
- データベースへの保存

**パラメーター**:
- `agent_name`: エージェント名
- `database_type`: データベースタイプ（sqlite, postgresql, mysql）

**使用例**:
```bash
agentflow template generate invoice-processor my-invoice-agent \
  -p agent_name=invoice-processor \
  -p database_type=postgresql
```

---

### 2. Chatbot

**説明**: 対話型チャットボットエージェント

**ユースケース**:
- カスタマーサポート
- FAQ 応答
- 会話型インターフェース

**パラメーター**:
- `agent_name`: エージェント名
- `llm_provider`: LLM プロバイダー（openai, anthropic, local）
- `memory_type`: メモリタイプ（short-term, long-term, hybrid）

**使用例**:
```bash
agentflow template generate chatbot my-chatbot \
  -p agent_name=support-bot \
  -p llm_provider=openai \
  -p memory_type=hybrid
```

---

### 3. Data Pipeline

**説明**: データ処理パイプラインエージェント

**ユースケース**:
- ETL 処理
- データ変換
- バッチ処理

**パラメーター**:
- `agent_name`: エージェント名
- `input_format`: 入力形式（csv, json, parquet）
- `output_format`: 出力形式（csv, json, parquet）

**使用例**:
```bash
agentflow template generate data-pipeline my-pipeline \
  -p agent_name=etl-pipeline \
  -p input_format=csv \
  -p output_format=parquet
```

---

## テンプレートの使用

### 基本的な使用方法

```bash
# テンプレート一覧を表示
agentflow template list

# テンプレート詳細を表示
agentflow template show <template-name>

# テンプレートからプロジェクトを生成
agentflow template generate <template-name> <output-dir> [options]
```

### パラメーター指定

#### コマンドラインで指定

```bash
agentflow template generate chatbot my-bot \
  -p agent_name=my-chatbot \
  -p llm_provider=openai
```

#### 対話モードで指定

```bash
agentflow template generate chatbot my-bot -i
# プロンプトに従ってパラメーターを入力
```

#### 設定ファイルで指定

```bash
# config.yaml を作成
cat > config.yaml << EOF
agent_name: my-chatbot
llm_provider: openai
memory_type: hybrid
EOF

# 設定ファイルを使用
agentflow template generate chatbot my-bot -c config.yaml
```

---

## カスタムテンプレートの作成

### テンプレート構造

```
my-template/
├── template.yaml       # テンプレートメタデータ
├── agent.py.j2         # Jinja2 テンプレート
├── agent.yaml.j2       # Jinja2 テンプレート
└── README.md.j2        # Jinja2 テンプレート
```

### template.yaml

```yaml
meta:
  id: my-template
  name: My Template
  version: 1.0.0
  description: カスタムテンプレートの説明
  author: Your Name
  category: custom

parameters:
  - name: agent_name
    type: string
    description: エージェント名
    required: true
  
  - name: feature_enabled
    type: boolean
    description: 機能を有効化
    required: false
    default: true

files:
  - src: agent.py.j2
    dest: agent.py
  - src: agent.yaml.j2
    dest: agent.yaml
  - src: README.md.j2
    dest: README.md
```

### Jinja2 テンプレート

```python
# agent.py.j2
from agentflow.core.agent_block import AgentBlock
from typing import Any

class {{ agent_name | capitalize }}Agent(AgentBlock):
    """{{ description }}."""
    
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        {% if feature_enabled %}
        # 機能が有効
        result = await self._process_with_feature(input_data)
        {% else %}
        # 機能が無効
        result = await self._process_basic(input_data)
        {% endif %}
        
        return {"result": result}
```

### テンプレートの登録

```bash
# テンプレートディレクトリに配置
cp -r my-template agentflow/templates/scenarios/

# または環境変数で指定
export AGENTFLOW_TEMPLATE_PATH=/path/to/templates
```

---

## 高度な使用方法

### 条件付きファイル生成

```yaml
# template.yaml
files:
  - src: agent.py.j2
    dest: agent.py
  
  - src: database.py.j2
    dest: database.py
    condition: "{{ database_enabled }}"
```

### ネストされたパラメーター

```yaml
parameters:
  - name: database
    type: object
    properties:
      type:
        type: string
        enum: [sqlite, postgresql, mysql]
      host:
        type: string
      port:
        type: integer
```

```python
# テンプレート内で使用
DATABASE_TYPE = "{{ database.type }}"
DATABASE_HOST = "{{ database.host }}"
DATABASE_PORT = {{ database.port }}
```

### カスタムフィルター

```python
# custom_filters.py
def snake_case(value: str) -> str:
    """文字列を snake_case に変換."""
    return value.lower().replace(" ", "_")

# テンプレートで使用
{{ agent_name | snake_case }}
```

---

詳細な API リファレンスは [api.md](api.md#template-system) を参照してください。

