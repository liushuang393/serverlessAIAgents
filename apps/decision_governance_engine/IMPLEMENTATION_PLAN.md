# Decision Governance Engine 改善実装計画

## 📋 概要

本ドキュメントは、Decision Governance EngineをAgentFlowフレームワークの最佳実践に準拠させるための改善計画を定義する。

---

## 🎯 改善目標

1. **AgentFlowフレームワーク完全活用**
2. **設計書要件の完全実装**
3. **A2UI/GUI2Agent画面技術の統合**
4. **Skills自動進化システムの活用**

---

## 📊 現状分析

### 実装完了状況（✅ 全Phase完了）

| 機能 | 現状 | 目標 |
|-----|-----|-----|
| Agent継承 | ✅ AgentBlock継承 | ✅ 維持 |
| 協調実行 | ⚠️ 手動チェーン | AgentCoordinator使用 |
| 状態共有 | ⚠️ SharedContext使用 | ✅ 維持 |
| Skills | ❌ 未使用 | SKILL.md形式で定義 特にUtils利用ところ、PDF出力|
| プロトコル | ❌ 未使用 | @auto_adapt統合 |
| A2UI画面 | ❌ 未使用 | 宣言式UI生成 dao,fa,shu,qi　agent出力表示分|
| REVISE回退 | ❌ 未実装 | 状態機械で実装 |
| リトライ | ❌ 未実装 | max_retry=2 |
| タイムアウト | ⚠️ 属性のみ | asyncio.timeout実装 |
| 即座拒否正規 | ❌ 未実装 | 設計書パターン実装 |

---

## 🔴 Phase 1: 高優先度（1週間）

### 1.1 AgentCoordinator統合

**目的**: 手動チェーン呼び出しをAgentCoordinator協調パターンに置換

**作業内容**:

```python
# workflow.py の改善

# Before (現状)
async def process(self, request):
    gatekeeper_result = await self._gatekeeper.run(...)
    dao_result = await self._dao.run(...)
    fa_result = await self._fa.run(...)
    # ... 手動チェーン

# After (改善後)
from agentflow.patterns.multi_agent import AgentCoordinator

class DecisionEngine:
    def __init__(self):
        self._context = SharedContext(enable_memory=True)
        self._coordinator = AgentCoordinator(
            agents=[
                self._gatekeeper,
                self._dao,
                self._fa,
                self._shu,
                self._qi,
                self._review,
            ],
            pattern="sequential",
            shared_context=self._context,
        )
    
    async def process(self, request):
        result = await self._coordinator.execute(request)
        return self._handle_result(result)
```

**成功条件**:
- [ ] AgentCoordinatorで6 Agent順次実行
- [ ] SharedContextで全結果共有
- [ ] テスト合格

---

### 1.2 Review REVISE回退ロジック

**目的**: ReviewAgent結果がREVISEの場合、該当Agentに回退して再実行

**作業内容**:

```python
# workflow.py に追加

class DecisionEngine:
    MAX_REVISIONS = 2  # 最大リビジョン回数
    
    async def process(self, request):
        for revision in range(self.MAX_REVISIONS + 1):
            result = await self._run_pipeline(request)
            review_result = result["agent_results"].get("ReviewAgent", {})
            
            verdict = review_result.get("overall_verdict", "PASS")
            
            if verdict == "PASS":
                return self._generate_report(result)
            
            if verdict == "REJECT":
                return {"status": "rejected", "findings": review_result.get("findings")}
            
            if verdict == "REVISE" and revision < self.MAX_REVISIONS:
                # 該当Agentを特定して再実行
                affected_agent = review_result.get("findings", [{}])[0].get("affected_agent")
                self._context.set("revision_target", affected_agent)
                self._context.set("revision_feedback", review_result.get("findings"))
                continue
        
        return {"status": "max_revisions_reached"}
```

**成功条件**:
- [ ] REVISE時に該当Agent再実行
- [ ] 最大2回のリビジョン制限
- [ ] リビジョン履歴記録

---

### 1.3 Skillsディレクトリ構築

**目的**: プロンプトをSKILL.md形式で構造化

**ディレクトリ構造**:

```
apps/decision_governance_engine/skills/
├── gatekeeper/
│   └── SKILL.md
├── dao/
│   └── SKILL.md
├── fa/
│   └── SKILL.md
├── shu/
│   └── SKILL.md
├── qi/
│   └── SKILL.md
└── review/
    └── SKILL.md
```

**SKILL.md例（Gatekeeper）**:

```markdown
---
name: decision-gatekeeper
description: |
  企業意思決定の入口検証。不適格な問題を門前払いする。
  一般知識質問、技術How-to、雑談、事実確認等を拒否。
version: 1.0.0
triggers:
  - 入口検証
  - 問題分類
  - 決策判定
  - gatekeeper
requirements: []
tags:
  - decision
  - validation
  - enterprise
---

# 入口検証Agent指示（GatekeeperAgent）

あなたはGatekeeperAgentです。問題の適格性を判断する門番です。

## 受理条件（全て満たす必要）
- 意思決定・判断を求めている
- 複数の選択肢や方向性が存在する
- ビジネス・組織・プロジェクトに関連する
- 回答者（決策者）が行動を起こせる
- 正解が一意に定まらない（判断が必要）

## 即座拒否パターン
以下のパターンは即座に拒否:
- 天気・時刻: (天気|気温|weather|何時|today)
- システム質問: (このシステム|このAI|どうやって作|仕組み)
- 一般知識: (とは何|what is|意味|定義)
- 計算・変換: (計算して|convert|換算)
- 雑談: (こんにちは|hello|元気|ありがとう)
- コード生成: (コード.*書いて|write.*code)
- 創作: (物語|story|poem|詩)
- 事実確認: (誰が|いつ|どこで).*\?$

## 出力形式
必ず以下のJSON形式で出力:
```json
{
    "is_acceptable": true/false,
    "category": "strategic_decision|resource_allocation|...|general_knowledge|...",
    "confidence": 0.0-1.0,
    "rejection_reason": "拒否理由（拒否時のみ）",
    "rejection_message": "ユーザー向けメッセージ",
    "suggested_rephrase": "言い換え提案（境界ケース時）"
}
```
```

**成功条件**:
- [ ] 6 Agent分のSKILL.md作成
- [ ] SkillEngine統合
- [ ] プロンプト読み込みテスト合格

---

## 🟡 Phase 2: 中優先度（2週間）

### 2.1 Gatekeeper即座拒否正規表現

**目的**: 設計書の正規表現パターンを実装

```python
# agents/gatekeeper_agent.py

import re

class GatekeeperAgent(BaseDecisionAgent):
    INSTANT_REJECT_PATTERNS = [
        r"(天気|気温|weather|何時|today)",
        r"(このシステム|このAI|どうやって作|仕組み|how.*work|how.*built)",
        r"(とは何|what is|意味|定義|explain what)",
        r"(計算して|convert|換算|translate)",
        r"(こんにちは|hello|hi|元気|調子|ありがとう|thank)",
        r"(コード.*書いて|write.*code|プログラム.*作成)",
        r"(物語|story|poem|詩|小説|作文)",
        r"(誰が|いつ|どこで|who is|when did|where is).*\?$",
    ]
    
    def _check_instant_reject(self, question: str) -> tuple[bool, str]:
        """即座拒否チェック."""
        for pattern in self.INSTANT_REJECT_PATTERNS:
            if re.search(pattern, question, re.IGNORECASE):
                return True, f"パターン一致: {pattern}"
        return False, ""
```

---

### 2.2 RAGSkill統合（Shu/Qi専用）

**目的**: 設計書で指定されたRAGソースを統合

```python
# agents/shu_agent.py

from agentflow.skills.rag import RAGSkill, RAGConfig

class ShuAgent(BaseDecisionAgent):
    USE_RAG = True
    RAG_SOURCES = ["industry_practices", "case_studies"]
    
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self._rag = RAGSkill(RAGConfig(
            sources=self.RAG_SOURCES,
            top_k=5,
        ))
    
    async def process(self, input_data):
        # RAGで関連事例を検索
        context = await self._rag.query(
            question=input_data.get("task", ""),
            topic="industry_practices"
        )
        
        # LLMプロンプトにRAG結果を含める
        prompt = self._build_prompt_with_rag(input_data, context)
        # ...
```

---

### 2.3 リトライ機構

**目的**: 各Agent最大2回リトライ

```python
# agents/base_agent.py

from agentflow.core.retry import RetryConfig, with_retry

class BaseDecisionAgent(AgentBlock):
    MAX_RETRY = 2
    RETRY_DELAY = 1.0
    
    async def run(self, input_data):
        return await self._run_with_retry(input_data)
    
    @with_retry(RetryConfig(max_attempts=2, delay=1.0))
    async def _run_with_retry(self, input_data):
        return await self.process(input_data)
```

---

### 2.4 タイムアウト制御

**目的**: 各Agent 30秒タイムアウト

```python
# agents/base_agent.py

import asyncio

class BaseDecisionAgent(AgentBlock):
    timeout_seconds: int = 30
    
    async def run(self, input_data):
        try:
            async with asyncio.timeout(self.timeout_seconds):
                return await self.process(input_data)
        except asyncio.TimeoutError:
            self._logger.error(f"{self.name} timed out after {self.timeout_seconds}s")
            raise TimeoutError(f"{self.name} exceeded {self.timeout_seconds}s timeout")
```

---

## 🟢 Phase 3: 低優先度（追加1週間）

### 3.1 Studio UI統合

**目的**: ビジュアルエディタで決策フロー可視化

```python
# 追加: studio_integration.py

from agentflow.studio.api import create_app

def register_decision_engine(app):
    """決策エンジンをStudio APIに登録."""
    
    @app.post("/api/decisions")
    async def create_decision(request: DecisionRequest):
        engine = DecisionEngine()
        result = await engine.process(request)
        return result
    
    @app.get("/api/decisions/{id}")
    async def get_decision(id: str):
        # 決策結果取得
        pass
```

---

### 3.2 WebSocket進捗通知

**目的**: 6 Agent実行進捗をリアルタイム配信

```python
# 追加: websocket_handler.py

from fastapi import WebSocket

class ProgressNotifier:
    def __init__(self, websocket: WebSocket):
        self._ws = websocket
    
    async def notify_agent_start(self, agent_name: str):
        await self._ws.send_json({
            "type": "AGENT_START",
            "agent": agent_name,
        })
    
    async def notify_agent_complete(self, agent_name: str, result: dict):
        await self._ws.send_json({
            "type": "AGENT_COMPLETE",
            "agent": agent_name,
            "result": result,
        })
```

---

### 3.3 A2UI画面生成

**目的**: 決策レポートを宣言式UIで生成

```python
# 追加: ui/components.py

from agentflow.protocols.a2ui import (
    CardComponent, TextComponent, ListComponent, ButtonComponent
)

class DecisionUIGenerator:
    def generate_executive_summary(self, report: DecisionReport):
        return CardComponent(
            title="EXECUTIVE SUMMARY",
            style={"backgroundColor": "#12121a", "padding": "20px"},
            children=[
                TextComponent(
                    content=f"💡 結論: {report.executive_summary.one_line_decision}",
                    style={"fontSize": "xl", "color": "#f8fafc"}
                ),
                TextComponent(
                    content=f"🎯 最初の一歩: {report.executive_summary.first_step}",
                    style={"fontSize": "lg", "color": "#94a3b8"}
                ),
                CardComponent(
                    title="⚠️ 主要リスク",
                    children=[
                        ListComponent(items=[
                            TextComponent(content=f"• {risk}")
                            for risk in report.executive_summary.key_risks
                        ])
                    ]
                ),
                ButtonComponent(label="📄 PDF出力", action="export_pdf"),
                ButtonComponent(label="🔄 再分析", action="reanalyze"),
            ]
        )
```

---

## 📅 スケジュール

| Phase | 期間 | 内容 |
|-------|------|------|
| Phase 1 | Week 1 | AgentCoordinator / REVISE回退 / Skills |
| Phase 2 | Week 2-3 | 正規表現 / RAG / リトライ / タイムアウト |
| Phase 3 | Week 4 | Studio UI / WebSocket / A2UI |

---

## ✅ Definition of Done

- [x] 全テスト合格
- [x] Lint/Formatter警告なし
- [x] 設計書要件100%実装
- [x] AgentFlow最佳実践準拠
- [x] ドキュメント更新完了

---

## 📝 実装完了サマリー

### 実装ファイル一覧

| ファイル | 変更内容 |
|---------|---------|
| `agents/base_agent.py` | リトライ機構、タイムアウト制御、SKILL.md読み込み追加 |
| `workflow.py` | REVISE回退ループ、RAG初期化、SharedContext強化 |
| `api.py` | WebSocketエンドポイント、個別Agent出力API追加 |
| `agent.yaml` | Studio UI用React Flowノード・エッジ定義 |
| `skills/*/SKILL.md` | 7つのSkill定義ファイル新規作成 |

### 新規作成ファイル

```
skills/
├── gatekeeper/SKILL.md  # 入口検証Agent
├── dao/SKILL.md         # 本質分析Agent
├── fa/SKILL.md          # 戦略選定Agent
├── shu/SKILL.md         # 実行計画Agent（RAG対応）
├── qi/SKILL.md          # 技術実装Agent（RAG対応）
├── review/SKILL.md      # 検証Agent
└── utils/SKILL.md       # ユーティリティ（PDF出力等）
```

### APIエンドポイント

| エンドポイント | 説明 |
|--------------|------|
| `POST /api/decision` | 同期処理 |
| `GET /api/decision/stream` | SSEストリーム |
| `WebSocket /ws/decision` | WebSocket進捗通知 |
| `GET /api/report/{id}/agent/{agent_id}` | 個別Agent出力 |
| `GET /api/workflow/config` | Studio UI設定 |

**作成日**: 2024年
**完了日**: 2024年
**ステータス**: ✅ 完了

