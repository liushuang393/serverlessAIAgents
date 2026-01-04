---
name: user-interaction
description: 用户交互 Skill - Agent 向用户请求补充信息
version: 1.0.0
author: AgentFlow
tags:
  - interaction
  - clarification
  - input
  - dialog
triggers:
  - 確認
  - 質問
  - 補足
  - 入力
  - clarify
requirements: []
dependencies: []
---

# User Interaction Skill

Agent がユーザーに情報補足を求めるための能力。

## 機能

1. **情報不足検出**: 質問から不足情報を特定
2. **補足要求生成**: ユーザーへの質問を生成
3. **入力待機**: SSE でユーザー入力を待機
4. **文脈統合**: 補足情報を元の質問に統合

## 使用方法

```python
from agentflow.skills.builtin.user_interaction import (
    UserInteractionSkill,
    ClarificationRequest,
)

# 初期化
interaction = UserInteractionSkill()

# 不足情報を検出
missing = await interaction.detect_missing_info(
    question="新しいサービスを始めたい",
    context={"domain": "business"}
)
# missing = ["予算規模", "対象市場", "タイムライン"]

# 補足要求を生成
request = await interaction.create_clarification_request(
    original_question="新しいサービスを始めたい",
    missing_info=missing,
)
# request.questions = [
#   {"id": "budget", "text": "予算規模を教えてください", "type": "number"},
#   {"id": "market", "text": "対象市場は？", "type": "select", "options": [...]},
# ]

# SSE イベントとして送信
yield ClarificationEvent(
    event_type="clarification.required",
    questions=request.questions,
    original_question=request.original_question,
)

# ユーザー回答を受信後、統合
enriched = await interaction.integrate_answers(
    original_question="新しいサービスを始めたい",
    answers={"budget": "1000万円", "market": "B2B"},
)
```

## イベントタイプ

| イベント | 説明 |
|---------|------|
| `clarification.required` | 補足情報が必要 |
| `clarification.received` | ユーザー回答を受信 |
| `clarification.timeout` | 入力タイムアウト |

## Decision Engine での使用例

```python
class CognitiveGateAgent:
    """認知門 - 情報不足時にユーザーに確認"""
    
    def __init__(self):
        self.interaction = UserInteractionSkill()
        self.search = WebSearchSkill()
    
    async def process(self, question: str) -> AsyncGenerator:
        # 1. 情報不足を検出
        missing = await self.interaction.detect_missing_info(question)
        
        if missing:
            # 2. まず検索で補完を試みる
            enriched = {}
            for info in missing:
                results = await self.search.search(f"{question} {info}")
                if results:
                    enriched[info] = results[0].snippet
                    missing.remove(info)
            
            # 3. 検索で補完できなかった情報はユーザーに確認
            if missing:
                request = await self.interaction.create_clarification_request(
                    original_question=question,
                    missing_info=missing,
                )
                yield ClarificationEvent(questions=request.questions)
                
                # 4. ユーザー回答を待機（フロントエンドが処理）
                return {"status": "awaiting_clarification", "request": request}
        
        # 情報十分 - 次のステップへ
        return {"status": "proceed", "enriched_question": question}
```

## フロントエンド連携

フロントエンドは `clarification.required` イベントを受信したら：

1. 補足入力フォームを表示
2. ユーザー入力を収集
3. `POST /api/decision/clarify` で回答を送信
4. 処理を継続

