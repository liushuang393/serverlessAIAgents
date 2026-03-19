---
name: design-skills
description: デザインスキルエンジン - インテント解析・プロンプト計画・ワークフロー実行
version: "1.0.0"
author: AgentFlow
tags:
  - design
  - intent-analysis
  - prompt-planning
  - workflow
triggers:
  - デザイン
  - design
  - インテント解析
  - ワークフロー
---

# Design Skills Engine

デザインスキルエンジンは、ユーザーの意図を解析し、プロンプト計画を策定し、
ワークフローを実行する統合スキルです。

## Agents

- **IntentAnalyzerAgent**: ユーザー入力からインテントを解析
- **PromptPlannerAgent**: 解析結果に基づきプロンプト計画を策定
- **WorkflowExecutorAgent**: 計画に基づきワークフローを実行

