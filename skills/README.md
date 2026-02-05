# Skills ディレクトリ

AgentFlow Skills の一元管理ディレクトリ。Claude Code Skills 形式に完全互換。

## ディレクトリ構成

```
skills/
├── builtin/          # フレームワーク提供のビルトインスキル
│   ├── rag/SKILL.md
│   ├── chatbot/SKILL.md
│   └── ...
├── user/             # ユーザー定義スキル
│   └── my-skill/SKILL.md
└── apps/             # アプリケーション固有スキル
    └── decision_governance_engine/
        ├── dao/SKILL.md
        └── ...
```

## SKILL.md 形式

```markdown
---
name: skill-name
description: スキルの説明
version: 1.0.0
triggers:
  - キーワード1
  - キーワード2
requirements:
  - package>=1.0.0
tags:
  - category
---

# Instructions

スキルの詳細な指示...
```

## 使用方法

### @agent デコレータでスキルを指定

```python
from agentflow import agent

@agent(skills=["rag", "chatbot"])
class MyAgent:
    """RAG と Chatbot スキルを使用する Agent."""
    system_prompt = "あなたは親切なアシスタントです"
```

### SimpleEngine でスキルを使用

```python
from agentflow.engines import SimpleEngine

engine = SimpleEngine(
    agent=MyAgent,
    skills=["rag", "chatbot"],
)
result = await engine.run({"query": "質問"})
```

### ToolDiscoveryService でスキルを発見

```python
from agentflow.core.tool_discovery import ToolDiscoveryService
from agentflow.core.tool_registry import get_global_tool_registry

registry = get_global_tool_registry()
service = ToolDiscoveryService(registry)

# Skills を自動発見してツールとして登録
count = await service.discover_skills_from_engine()
print(f"発見されたスキル: {count}")

# スキルをツールとして取得
rag_tool = registry.get("tool://skill/rag")
```

## スキルの追加

### ビルトインスキル

`skills/builtin/` ディレクトリに新しいフォルダを作成し、`SKILL.md` を追加:

```bash
mkdir -p skills/builtin/my-new-skill
cat > skills/builtin/my-new-skill/SKILL.md << 'EOF'
---
name: my-new-skill
description: 新しいスキルの説明
version: 1.0.0
triggers:
  - my-skill
  - new-skill
tags:
  - utility
---

# Instructions

このスキルは...
EOF
```

### ユーザースキル

`skills/user/` または `~/.agentflow/skills/` に追加。

### アプリケーション固有スキル

`skills/apps/<app-name>/` に追加。

## 関連ドキュメント

- [Skills ガイド](../docs/guide-skills.md)
- [Skills 自動進化システム](../docs/skills-guide.md)
- [Auto-Agent アーキテクチャ](../docs/auto-agent-architecture.md)
