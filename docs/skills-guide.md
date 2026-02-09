# Skills ガイド - Claude Code Skills 互換

> **バージョン**: 0.2.0  
> **参照**: [Claude Code Skills 公式ドキュメント](https://code.claude.com/docs/en/skills)

> **注意**: Skills 自動進化システムの詳細は [Skills 自動進化システム ガイド](guide-skills.md) を参照してください。

## 概要

AgentFlow の Skills システムは Claude Code Skills 規範に完全互換です。
Skills は LLM にドメイン知識と手順を教える Markdown ファイルです。

## 制限と仕様

### メタデータ制限

| フィールド | 制限 | 必須 | 説明 |
|-----------|------|------|------|
| `name` | ≤ 64 文字 | ✅ | 小文字+数字+ハイフンのみ |
| `description` | ≤ 1024 文字 | ✅ | Claude が使用判断に使う |
| `version` | semver 形式 | ❌ | 例: `1.0.0` |
| `triggers` | 配列 | ❌ | マッチングキーワード |
| `requirements` | 配列 | ❌ | 必要パッケージ |

### 禁止事項

- `name` に `anthropic`、`claude` を含めない
- XML タグを含めない
- 500 行以上の SKILL.md（分割推奨）

### Context Window 共有

```
起動時: name + description のみ（全 Skills）
  ↓
触発時: SKILL.md 全体を読み込み
  ↓
必要時: 追加ファイル（reference.md 等）を読み込み
```

## SKILL.md 形式

```markdown
---
name: my-skill
description: |
  説明文。Claude がこの説明で使用判断する。
  複数行可能。
version: 1.0.0
triggers:
  - キーワード1
  - キーワード2
requirements:
  - package1
  - package2
---

# Skill タイトル

## 指示

ここに LLM への指示を記述。
```

## ディレクトリ構造

### 単一ファイル（シンプル）

```
skills/
└── my-skill/
    └── SKILL.md
```

### 複数ファイル（渐进式披露）

```
skills/
└── my-skill/
    ├── SKILL.md          # メイン（500 行以下）
    ├── reference.md      # 詳細参照（必要時読み込み）
    ├── examples.md       # 使用例
    └── scripts/
        └── helper.py     # ユーティリティ（実行用）
```

**重要**: 参照は1レベルの深さまで。ネストされた参照は避ける。

### 実行時の探索パス（プロジェクト本体）

AgentFlow 本体でスキルを自動発見する際の既定探索先は次の通りです。

```
agentflow/skills/builtin/   # フレームワーク提供スキル
~/.agentflow/skills/        # ユーザースキル
apps/*/skills/              # アプリ固有スキル
```

## 使用方法

### Agent に Skills を設定

```python
from agentflow import agent

# 方法1: デコレータ引数
@agent(skills=["chatbot", "rag"])
class MyAgent:
    system_prompt = "..."

# 方法2: クラス属性
@agent
class MyAgent:
    skills = ["rag", "database-manager"]
    system_prompt = "..."
```

### Skills API

```python
from agentflow import get_skill, list_skills

# 利用可能な Skills 一覧
print(list_skills())
# ['auth-provider', 'chatbot', 'database-manager', 'deployment-manager', 'rag', 'stripe-payment']

# Skill 取得
skill = get_skill("rag")
print(skill.instructions)  # 指示内容

# プロンプト生成
prompt = skill.to_prompt()  # システムプロンプト用
```

### カスタム Skill 作成

```python
from agentflow.skills.base import Skill

skill = Skill.create(
    name="my-custom-skill",
    description="カスタム Skill の説明",
    instructions="""
## 指示

1. ステップ1
2. ステップ2
""",
    triggers=["keyword1", "keyword2"],
    requirements=["package1"],
)

# 保存
skill.save(Path("./skills/my-custom-skill"))
```

## ビルトイン Skills

| Skill | 説明 |
|-------|------|
| **chatbot** | マルチターン対話管理、RAG 統合 |
| **rag** | 検索増強生成、知識ベース Q&A |
| **auth-provider** | 認証統合（Supabase, Clerk, Firebase） |
| **database-manager** | データベース操作（SQL, Vector） |
| **deployment-manager** | デプロイ管理（Vercel, AWS） |
| **stripe-payment** | 決済統合（Stripe） |

## クエリマッチング

Skills は以下の優先順位でマッチング：

1. **triggers 完全一致** → スコア 0.9
2. **name 一致** → スコア 0.8
3. **description 部分一致** → スコア 0.7
4. **tags 一致** → スコア 0.6

```python
metadata = skill.metadata
score = metadata.matches_query("PDF からテキスト抽出")
# triggers に "pdf" があれば score = 0.9
```

## LLM 統合例

```python
from openai import AsyncOpenAI
from agentflow.skills.base import Skill

client = AsyncOpenAI()

# Skill から system prompt 生成
skill = get_skill("rag")
system_prompt = skill.to_prompt()

# LLM 呼び出し
response = await client.chat.completions.create(
    model="gpt-4o-mini",
    messages=[
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": "質問"},
    ],
)
```

## ベストプラクティス

### 1. 簡潔に書く

```markdown
# ❌ 悪い例（冗長）
PDF (Portable Document Format) は一般的なファイル形式で...
様々なライブラリがありますが、pdfplumber を推奨します...

# ✅ 良い例（簡潔）
pdfplumber でテキスト抽出:
```python
import pdfplumber
with pdfplumber.open("file.pdf") as pdf:
    text = pdf.pages[0].extract_text()
```
```

### 2. description は具体的に

```markdown
# ❌ 悪い例
description: ドキュメントを処理

# ✅ 良い例
description: |
  PDF ファイルからテキストと表を抽出。フォーム記入、文書結合に対応。
  PDF、フォーム、ドキュメント抽出が必要な場合に使用。
```

### 3. 渐进式披露を活用

```markdown
# SKILL.md（メイン）
## 概要
[基本的な使い方]

## 詳細
- フォーム記入: [FORMS.md](FORMS.md) を参照
- API リファレンス: [REFERENCE.md](REFERENCE.md) を参照
```

### 4. エラーハンドリング

```markdown
## エラー処理

ファイルが見つからない場合:
```python
try:
    result = process_file(path)
except FileNotFoundError:
    print(f"ファイルが見つかりません: {path}")
    result = create_default()
```
```

## モデル別考慮事項

| モデル | 特性 | 推奨 |
|--------|------|------|
| Haiku | 高速、経済的 | より詳細な指示 |
| Sonnet | バランス | 標準的な指示 |
| Opus | 強力な推論 | 簡潔な指示で OK |

## トラブルシューティング

### Skill が読み込まれない

1. パス確認: `skills/skill-name/SKILL.md`
2. YAML 構文確認: `---` で囲まれているか
3. name フォーマット確認: 小文字+数字+ハイフンのみ

### Skill が発火しない

1. description に具体的なキーワードを追加
2. triggers にユーザーが使う言葉を追加
3. 他の Skill と description が被っていないか確認

### Context オーバーフロー

1. SKILL.md を 500 行以下に分割
2. 詳細を別ファイルに移動
3. 不要な説明を削除

## 参照

- [Claude Code Skills 公式ドキュメント](https://code.claude.com/docs/en/skills)
- [Skills ベストプラクティス](https://platform.claude.com/docs/en/agents-and-tools/agent-skills/best-practices)
- [ビルトイン Skills](../agentflow/skills/builtin/)
