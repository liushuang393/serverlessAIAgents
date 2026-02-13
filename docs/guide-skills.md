# Skills 自動進化システム ガイド

> **バージョン**: 2.0.0
> **更新日**: 2025-01-15

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
| 🔗 **Agent統合** | DeepAgentCoordinator と自動連携 |
| 🧠 **進化学習** | 成功パターンから自動で Skill を固化 |

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

外部 Skill を取り込む場合は、CLI で同じ配置へマウントできます。

```bash
# プロジェクトローカルにマウント
agentflow skills mount ./external/my-skill --scope project

# グローバルにマウント
agentflow skills mount ./external/my-skill --scope global
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

## 🔗 DeepAgentCoordinator との統合

Skills は DeepAgentCoordinator の実行フローに自動統合されています。

### 統合ポイント

| 統合箇所 | 機能 |
|----------|------|
| **タスク分解** | 各サブタスクに最適な Skill を自動マッチ/生成 |
| **DynamicAgent** | Skill の instructions を system prompt に注入 |
| **Evolver** | 成功パターンから自動で Skill を固化 |
| **AgentPool** | SkillEngine を共有、動的解析をサポート |

### タスク分解時の自動マッチング

```python
# DeepAgentCoordinator が内部で自動実行
coordinator = DeepAgentCoordinator(
    llm_client=llm,
    enable_skill_auto_learn=True,  # Skill 自動学習を有効化
)

# タスク分解時、各 todo に適切な Skill が自動でバインド
result = await coordinator.run("PDFを解析してレポートを作成")
# → 内部で pdf-extractor skill が自動マッチ
```

### DynamicAgent での Skill 活用

```python
# DynamicAgent は自動で Skill の指示を LLM に注入
agent = DynamicAgent(
    llm_client=llm,
    skills=[pdf_skill, report_skill],  # Skill リスト
)

# run() 時、skills の instructions が system prompt に追加される
result = await agent.run("PDFからデータを抽出")
```

### 成功パターンからの自動固化

```python
# Evolver が高信頼度の成功パターンを自動で Skill に固化
evolver = Evolver(
    llm_client=llm,
    skill_engine=skill_engine,  # SkillEngine を渡す
)

# 成功時、confidence >= 0.8 なら自動で Skill 生成
await evolver.learn_from_success(
    task="PDF解析タスク",
    result={"success": True, ...},
    confidence=0.85,
)
# → learned_skills に自動保存
```

---

## 🛠️ Skill の作成方法

### 方法1: 手動作成（推奨）

1. ディレクトリを作成:
```bash
mkdir -p ~/.agentflow/skills/my-skill
```

2. SKILL.md を作成:
```bash
cat > ~/.agentflow/skills/my-skill/SKILL.md << 'EOF'
---
name: my-skill
description: 説明文
triggers:
  - キーワード1
  - キーワード2
---

# Instructions

具体的な手順をここに記述。
EOF
```

### 方法1-b: CLI で外部 Skill をマウント

既存リポジトリや共有ディレクトリの Skill をそのまま取り込む場合:

```bash
# 単一 Skill
agentflow skills mount ./third_party/pdf-extractor --scope project

# 複数 Skill を含むルート
agentflow skills mount ./third_party/skills --scope project

# 既存を上書き
agentflow skills mount ./third_party/pdf-extractor --scope project --force
```

補足:
- `SOURCE` は Skill ディレクトリ、`SKILL.md`、複数 Skill ルートを指定可能です。
- `--name` は単一 Skill のときのみ指定できます。

### 方法2: 自動生成

```python
from agentflow.skills import SkillEngine

engine = SkillEngine(auto_learn=True)

# クエリから自動生成
result = await engine.resolve("新しいタスクの説明")
if result.generated:
    print(f"新 Skill 生成: {result.skill.name}")
    # ~/.agentflow/learned_skills/ に自動保存
```

### 方法3: 成功パターンから固化

タスク実行が成功し、信頼度が高い場合、Evolver が自動で Skill を生成します。

```python
# 手動で固化をトリガー
await evolver.learn_from_success(
    task="タスク説明",
    result=execution_result,
    confidence=0.9,
)
```

---

## 📚 関連ドキュメント

- [アーキテクチャ](architecture.md) - システム全体設計
- [コーディングガイド](guide-coding.md) - Python 開発ガイド
- [API リファレンス](api.md) - 全 API 詳細
- [DeepAgent 設計](design/DEEP_AGENT_COORDINATOR_DESIGN.md) - Coordinator 詳細
