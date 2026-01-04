# Skills 自動進化システム ガイド

> **バージョン**: 1.0.0
> **更新日**: 2025-01-20

> **注意**: Claude Code Skills 互換性の詳細は [Skills ガイド](skills-guide.md) を参照してください。

---

## 📋 概要

AgentFlow の **Skills 自動進化システム** は、Claude Code Skills 完全互換の能力パッケージシステムです。

### 核心理念

```
用户需求 → 技能匹配 → 存在なら実行
                   → 不在なら自動生成 → 検証 → 固化 → 実行
= 越用越厉害（使うほど強くなる）
```

### 主な特徴

| 特徴 | 説明 |
|------|------|
| 🎯 **自動マッチング** | triggers/description ベースで最適なスキルを検索 |
| 🤖 **自動生成** | マッチなし時、LLM で新スキルを自動作成 |
| ✅ **自動検証** | フォーマット・セキュリティを自動チェック |
| 💾 **自動固化** | 検証済みスキルを learned_skills に保存 |
| 📈 **使用統計** | 使用回数を追跡、信頼度を更新 |

---

## 🚀 クイックスタート

### 基本的な使用

```python
from agentflow.skills import SkillEngine

# エンジン初期化（自動学習有効）
engine = SkillEngine(auto_learn=True)

# クエリを解決（マッチ or 自動生成）
result = await engine.resolve("PDFからテキストを抽出したい")

if result.generated:
    print(f"🆕 新スキル生成: {result.skill.name}")
else:
    print(f"✅ 既存スキル: {result.skill.name}")

# 指示内容を取得
print(result.instructions)
```

### マッチングのみ（生成なし）

```python
from agentflow.skills import SkillMatcher, SkillLoader, SkillRegistry

# スキルを読み込み
registry = SkillRegistry()
loader = SkillLoader(registry)
skills = loader.load_directory(".agentflow/skills")

# マッチング
matcher = SkillMatcher(skills)
results = matcher.match("PDF解析")

for r in results:
    print(f"{r.skill.name}: {r.score:.2f} - {r.reason}")
```

---

## 📝 SKILL.md フォーマット

Claude Code Skills 完全互換のファイル形式です。

### 基本構造

```markdown
---
name: skill-name-kebab-case
description: 具体的な説明。何ができるか、いつ使うべきかを明記。
version: 1.0.0
author: your-name
triggers:
  - トリガーワード1
  - トリガーワード2
requirements:
  - 必要なパッケージ
tags:
  - カテゴリ
---

# Instructions

具体的な実行手順をここに記述。
コード例を含めることを推奨。
```

### フィールド説明

| フィールド | 必須 | 説明 |
|-----------|------|------|
| `name` | ✅ | kebab-case のスキル名 |
| `description` | ✅ | スキルの説明（マッチングに使用） |
| `version` | ❌ | semver 形式（デフォルト: 1.0.0） |
| `triggers` | ❌ | マッチング用キーワード |
| `requirements` | ❌ | 依存パッケージ |
| `tags` | ❌ | カテゴリタグ |
| `author` | ❌ | 作成者 |
| `examples` | ❌ | 使用例 |

### 実例

```markdown
---
name: pdf-extractor
description: PDFファイルからテキスト、表、メタデータを抽出。PDF操作時に使用。
version: 1.0.0
triggers:
  - pdf
  - extract text
  - parse document
requirements:
  - pypdf
  - pdfplumber
tags:
  - document
  - extraction
---

# PDF Extraction Instructions

## Basic Usage
\`\`\`python
import pdfplumber

with pdfplumber.open("document.pdf") as pdf:
    for page in pdf.pages:
        text = page.extract_text()
        print(text)
\`\`\`
```

---

## 📁 ディレクトリ構成

```
~/.agentflow/
├── skills/              # グローバルスキル（手動作成）
│   └── my-skill/
│       └── SKILL.md
└── learned_skills/      # 自動学習スキル（自動生成）
    └── auto-generated-skill/
        └── SKILL.md

project/
└── .agentflow/
    └── skills/          # プロジェクトスキル
        └── project-skill/
            └── SKILL.md
```

### 読み込み優先順位

1. `~/.agentflow/skills/` - グローバル
2. `~/.agentflow/learned_skills/` - 学習済み
3. `.agentflow/skills/` - プロジェクト

---

## 🔧 コンポーネント詳細

### SkillMatcher

クエリに最適なスキルを検索：

```python
from agentflow.skills import SkillMatcher

matcher = SkillMatcher(skills, threshold=0.3)

# 複数結果を取得
results = matcher.match("PDFを解析", top_k=5)

# 最良のみ取得
best = matcher.find_best("PDFを解析")

# マッチ存在確認
if matcher.has_match("PDF"):
    print("PDFスキルあり")
```

### SkillGenerator

LLM で新スキルを自動生成：

```python
from agentflow.skills import SkillGenerator
from agentflow.llm.llm_client import LLMClient

generator = SkillGenerator(llm_client=LLMClient())

result = await generator.generate("Excelファイルを読み込む方法")
if result.success:
    print(f"生成成功: {result.skill.name}")
```

### SkillValidator

スキルの品質・安全性を検証：

```python
from agentflow.skills import SkillValidator

validator = SkillValidator(strict=False)
result = validator.validate(skill)

if result.valid:
    print("検証OK")
else:
    for error in result.errors:
        print(f"エラー: {error}")
```

### SkillPersister

スキルをファイルに固化：

```python
from agentflow.skills import SkillPersister

persister = SkillPersister()

# 学習スキルとして保存
path = persister.save(skill, scope="learned")

# 既存を上書き
path = persister.save(skill, scope="learned", force=True)
```

---

## 📚 関連ドキュメント

- [アーキテクチャ](architecture.md) - システム全体設計
- [コーディングガイド](guide-coding.md) - Python 開発ガイド
- [API リファレンス](api.md) - 全 API 詳細

