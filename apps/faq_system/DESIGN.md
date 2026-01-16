# FAQ System 強化版システム設計書

> バージョン: 3.1.0  
> 更新日: 2026-01-16  
> ステータス: 実装完了

---

## 目次

1. [概要](#1-概要)
2. [現状分析](#2-現状分析)
3. [システムアーキテクチャ](#3-システムアーキテクチャ)
4. [機能設計](#4-機能設計)
5. [セキュリティ設計](#5-セキュリティ設計)
6. [精度向上設計](#6-精度向上設計)
7. [知識運営設計](#7-知識運営設計)
8. [API設計](#8-api設計)
9. [データモデル](#9-データモデル)
10. [実装フェーズ](#10-実装フェーズ)
11. [テストデータ](#11-テストデータ)
12. [共通機能反哺](#12-共通機能反哺)
13. [技術実装仕様](#13-技術実装仕様)
14. [システム利用ガイド](#14-システム利用ガイド)

---

## 1. 概要

### 1.1 目的

本システムは、企業内部の知識管理とデータ分析を統合した AI アシスタントシステムです。
以下の3つの核心機能を提供します：

| 機能 | 対象ユーザー | 主な用途 |
|------|-------------|---------|
| **社内FAQ** | 全社員 | 規則/業務照会、新入社員支援、対客製品問合せ |
| **メンテナンス支援** | 保守担当者 | 仕様変更、影響分析、ドキュメント整理 |
| **高層データ分析** | 経営層/分析者 | 自然言語→SQL、可視化、アクション提案 |

### 1.2 設計原則

```
┌─────────────────────────────────────────────────────────────┐
│                      設計原則                                │
├─────────────────────────────────────────────────────────────┤
│ 1. 来源追跡可能   - すべての回答にソース引用を必須          │
│ 2. 権限厳格分層   - 社内/対客KB物理隔離 + RBAC/ABAC         │
│ 3. 保守的回答     - 規則類は摘録優先、不確定は確認促す      │
│ 4. 証拠チェーン   - データ分析は根拠・前提・代替案を提示    │
│ 5. 知識Owner制度  - 各KBにOwnerを設定、更新責任を明確化     │
└─────────────────────────────────────────────────────────────┘
```

---

## 2. 現状分析

### 2.1 既存実装

```
apps/faq_system/
├── main.py                    # v1.0 基本版（RAG + SQL + Chart）
├── main_enhanced.py           # v2.0 強化版（リッチテキスト + WebSocket）
├── backend/
│   └── agents/
│       └── enhanced_faq_agent.py  # 強化版FAQAgent
└── frontend/                  # フロントエンド資産
```

### 2.2 agentflow フレームワーク活用可能モジュール

| モジュール | パス | 機能 | 活用方針 |
|-----------|------|------|----------|
| **PolicyEngine** | `agentflow/security/policy_engine.py` | RBAC/ABAC/PBAC統一認可 | 権限制御の基盤 |
| **DataSanitizer** | `agentflow/security/data_sanitizer.py` | PII脱敏、注入検出 | 入出力フィルタ |
| **HallucinationDetector** | `agentflow/security/hallucination_detector.py` | 幻覚検出、信頼度評価 | 回答品質検証 |
| **AuthMiddleware** | `agentflow/security/auth_middleware.py` | JWT/API Key認証 | 認証統一 |
| **RBACManager** | `agentflow/security/rbac.py` | ロール権限管理 | アクセス制御 |
| **RAGPipeline** | `agentflow/knowledge/rag_pipeline.py` | 検索増強生成 | KB検索基盤 |
| **Text2SQLService** | `agentflow/services/text2sql_service.py` | NL→SQL変換 | データ分析基盤 |
| **FAQAgent** | `agentflow/agents/faq_agent.py` | FAQ専門Agent | 問答処理基盤 |

### 2.3 現状の課題

| 課題 | 影響 | 対策方針 |
|------|------|----------|
| 社内/対客KB未分離 | 情報漏洩リスク | 物理隔離アーキテクチャ |
| 引用ソース不明確 | 信頼性低下 | 必須引用機能 |
| 権限制御不十分 | セキュリティリスク | RBAC/ABAC統合 |
| 術語不統一 | 検索精度低下 | Glossary辞書 |
| MyNumber等の除外なし | コンプライアンスリスク | システム級除外 |

---

## 3. システムアーキテクチャ

### 3.1 全体アーキテクチャ

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           クライアント層                                 │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐               │
│  │  Web UI  │  │ Teams Bot│  │Slack Bot │  │ API Client│               │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────┬─────┘               │
└───────┼─────────────┼─────────────┼─────────────┼───────────────────────┘
        │             │             │             │
        ▼             ▼             ▼             ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                           ゲートウェイ層                                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐                  │
│  │ SSO/JWT認証   │  │ Rate Limiter │  │ 監査ログ     │                  │
│  │ (Azure AD等) │  │              │  │ (全操作記録) │                  │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘                  │
└─────────┼────────────────┼────────────────┼─────────────────────────────┘
          │                │                │
          ▼                ▼                ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                          FAQ コアサービス層                              │
│                                                                         │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │                      Query Router                                │   │
│  │    (クエリ分類: FAQ / SQL / Maintenance / Hybrid)               │   │
│  └───────┬─────────────┬─────────────┬─────────────┬───────────────┘   │
│          │             │             │             │                    │
│          ▼             ▼             ▼             ▼                    │
│  ┌───────────┐  ┌───────────┐  ┌───────────┐  ┌───────────┐           │
│  │ 社内KB    │  │ 対客KB    │  │メンテナンス│  │ 分析      │           │
│  │ Agent     │  │ Agent     │  │支援Agent  │  │ Agent     │           │
│  │(RBAC制御) │  │(公開のみ) │  │(差分/影響)│  │(語義層)   │           │
│  └─────┬─────┘  └─────┬─────┘  └─────┬─────┘  └─────┬─────┘           │
│        │             │             │             │                      │
└────────┼─────────────┼─────────────┼─────────────┼──────────────────────┘
         │             │             │             │
         ▼             ▼             ▼             ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                          セキュリティ層                                  │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌────────────┐ │
│  │ PolicyEngine │  │DataSanitizer │  │Hallucination │  │ PII Filter │ │
│  │(RBAC/ABAC)   │  │(脱敏/注入)   │  │ Detector     │  │(MyNumber等)│ │
│  └──────────────┘  └──────────────┘  └──────────────┘  └────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
         │             │             │             │
         ▼             ▼             ▼             ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                           知識層                                         │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌────────────┐ │
│  │ 社内Vector DB│  │対客Vector DB │  │ 語義層       │  │ 術語辞書   │ │
│  │ (隔離)       │  │ (隔離)       │  │(指標定義)    │  │(Glossary)  │ │
│  └──────────────┘  └──────────────┘  └──────────────┘  └────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
         │             │             │             │
         ▼             ▼             ▼             ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                          データ層                                        │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌────────────┐ │
│  │ 業務DB       │  │ 監査DB       │  │ フィードバック│  │ 変更管理   │ │
│  │(ホワイトリスト)│  │(全操作記録) │  │ DB           │  │ DB         │ │
│  └──────────────┘  └──────────────┘  └──────────────┘  └────────────┘ │
└─────────────────────────────────────────────────────────────────────────┘
```

### 3.2 双知識ベース隔離アーキテクチャ

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        知識ベース隔離設計                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│   ┌─────────────────────────┐    ┌─────────────────────────┐          │
│   │      社内KB (Internal)   │    │     対客KB (External)    │          │
│   ├─────────────────────────┤    ├─────────────────────────┤          │
│   │ 認証: SSO + RBAC/ABAC   │    │ 認証: 公開 or API Key   │          │
│   │ 範囲: 全社内ドキュメント │    │ 範囲: 公開可資料のみ     │          │
│   │ 権限: 部門/役職ベース   │    │ 権限: 読取専用          │          │
│   ├─────────────────────────┤    ├─────────────────────────┤          │
│   │ Vector DB: internal_kb  │    │ Vector DB: external_kb  │          │
│   │ Storage: /data/internal │    │ Storage: /data/external │          │
│   │ Index: internal_index   │    │ Index: external_index   │          │
│   └─────────────────────────┘    └─────────────────────────┘          │
│                                                                         │
│   ⚠️ 重要: 両KBは物理的に完全隔離（インデックス・ストレージ・権限）    │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 4. 機能設計

### 4.1 機能1: 社内FAQ

#### 4.1.1 概要

```python
# 設計パターン: RAG（検索増強生成）+ 混合検索（キーワード/ベクトル）+ 引用来源
```

#### 4.1.2 処理フロー

```
┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐
│ユーザー │ -> │認証/認可│ -> │クエリ   │ -> │ 検索    │ -> │ 回答    │
│質問入力 │    │チェック │    │分類     │    │ 実行    │    │ 生成    │
└─────────┘    └─────────┘    └─────────┘    └─────────┘    └─────────┘
                   │              │              │              │
                   ▼              ▼              ▼              ▼
              ┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐
              │RBAC/ABAC│    │規則類→  │    │混合検索 │    │引用必須 │
              │権限判定 │    │保守モード│    │Reranker │    │信頼度付 │
              └─────────┘    └─────────┘    └─────────┘    └─────────┘
```

#### 4.1.3 クラス設計

```python
# apps/faq_system/backend/agents/internal_kb_agent.py

from dataclasses import dataclass, field
from typing import Any
from agentflow.core import ResilientAgent
from agentflow.security import PolicyEngine, AuthContext

@dataclass
class InternalKBConfig:
    """社内KB Agent 設定."""
    
    # KB設定
    collection: str = "internal_kb"
    top_k: int = 5
    min_similarity: float = 0.3
    
    # 保守モード（規則類）
    conservative_mode: bool = True
    conservative_keywords: list[str] = field(default_factory=lambda: [
        "規則", "規程", "ポリシー", "制度", "規定", "手続き", "申請"
    ])
    
    # 引用設定
    require_citation: bool = True
    include_version: bool = True
    include_update_date: bool = True
    include_page_number: bool = True
    
    # 不確定時の動作
    uncertainty_threshold: float = 0.6
    auto_generate_ticket: bool = True


class InternalKBAgent(ResilientAgent):
    """社内KB Agent（RBAC制御 + 保守モード）."""
    
    name = "InternalKBAgent"
    
    async def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """処理実行."""
        question = input_data.get("question", "")
        user_context = input_data.get("user_context", {})
        
        # 1. 権限チェック
        auth_result = await self._check_permission(user_context)
        if not auth_result.allowed:
            return self._create_access_denied_response(auth_result.reason)
        
        # 2. クエリ分類（規則類か判定）
        is_rule_query = self._is_rule_query(question)
        
        # 3. 検索実行
        search_results = await self._search(question, user_context)
        
        # 4. 回答生成（規則類は保守モード）
        if is_rule_query:
            response = await self._generate_conservative_answer(
                question, search_results
            )
        else:
            response = await self._generate_answer(question, search_results)
        
        # 5. 不確定判定
        if response["confidence"] < self._config.uncertainty_threshold:
            response = await self._handle_uncertainty(question, response)
        
        return response
    
    async def _generate_conservative_answer(
        self, question: str, results: list
    ) -> dict[str, Any]:
        """保守モード回答生成（直接摘録優先）."""
        # 摘録 + 定位を優先
        # 自由発揮を抑制
        pass
    
    async def _handle_uncertainty(
        self, question: str, response: dict
    ) -> dict[str, Any]:
        """不確定時の処理（確認促す + 工単生成）."""
        response["needs_confirmation"] = True
        response["suggested_contacts"] = self._get_responsible_department(question)
        
        if self._config.auto_generate_ticket:
            ticket = await self._generate_inquiry_ticket(question)
            response["ticket_id"] = ticket.id
        
        return response
```

#### 4.1.4 引用フォーマット

```json
{
  "answer": "年次有給休暇は入社6ヶ月経過後に10日付与されます [1]。",
  "citations": [
    {
      "index": 1,
      "document_id": "doc_001",
      "title": "就業規則 第5章 休暇",
      "source": "hr_policies/work_rules_v3.2.pdf",
      "version": "3.2",
      "update_date": "2025-04-01",
      "page_number": 45,
      "section": "第25条",
      "effective_date": "2025-04-01",
      "applicable_scope": "正社員・契約社員",
      "snippet": "入社後6ヶ月間継続勤務し、全労働日の8割以上出勤した従業員には、10日の年次有給休暇を付与する。",
      "relevance_score": 0.92
    }
  ],
  "confidence": 0.88,
  "query_type": "rule",
  "needs_confirmation": false
}
```

### 4.2 機能2: メンテナンス支援

#### 4.2.1 概要

```
変更作業フローの助手として、以下の高価値機能を提供：
1. 仕様差分総結 - 新旧ドキュメント比較
2. 影響範囲分析 - 関連コンポーネント特定
3. 変更成果物自動生成 - Release Note、FAQ更新草案
4. ドキュメント健康度 - 期限切れ、重複検出
```

#### 4.2.2 処理フロー

```
┌────────────────────────────────────────────────────────────────────────┐
│                    メンテナンス支援フロー                               │
├────────────────────────────────────────────────────────────────────────┤
│                                                                        │
│  ┌──────────┐                                                         │
│  │ 新旧文書 │                                                         │
│  │ アップ   │                                                         │
│  └────┬─────┘                                                         │
│       │                                                                │
│       ▼                                                                │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │                    差分抽出エンジン                               │ │
│  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐            │ │
│  │  │インター │  │フィールド│  │ビジネス │  │ 例外    │            │ │
│  │  │フェース │  │変更     │  │ルール   │  │ ケース  │            │ │
│  │  └─────────┘  └─────────┘  └─────────┘  └─────────┘            │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│       │                                                                │
│       ▼                                                                │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │                    影響分析エンジン                               │ │
│  │  変化点 → [モジュール] [API] [DBテーブル] [テストケース] [Runbook]│ │
│  └──────────────────────────────────────────────────────────────────┘ │
│       │                                                                │
│       ▼                                                                │
│  ┌──────────────────────────────────────────────────────────────────┐ │
│  │                    成果物自動生成                                 │ │
│  │  [Release Note] [FAQ更新草案] [研修通知草案] [テスト観点リスト]  │ │
│  │                                                                    │ │
│  │  ⚠️ 出力には「カバレッジ提示」を必須含める                       │ │
│  │     例: 「本結論はXX文書のYY章に基づきます（Z章は未参照）」      │ │
│  └──────────────────────────────────────────────────────────────────┘ │
│                                                                        │
└────────────────────────────────────────────────────────────────────────┘
```

#### 4.2.3 クラス設計

```python
# apps/faq_system/backend/agents/maintenance_agent.py

from dataclasses import dataclass
from typing import Any
from agentflow.core import ResilientAgent

@dataclass
class DiffResult:
    """差分結果."""
    category: str  # interface, field, rule, exception
    old_value: str
    new_value: str
    location: str
    severity: str  # low, medium, high, critical

@dataclass
class ImpactAnalysis:
    """影響分析結果."""
    affected_modules: list[str]
    affected_apis: list[str]
    affected_tables: list[str]
    affected_tests: list[str]
    affected_runbooks: list[str]
    coverage_info: str  # カバレッジ提示

@dataclass
class MaintenanceConfig:
    """メンテナンス支援設定."""
    
    # 差分抽出設定
    diff_categories: list[str] = None
    ignore_whitespace: bool = True
    
    # 影響分析設定
    enable_module_analysis: bool = True
    enable_api_analysis: bool = True
    enable_db_analysis: bool = True
    enable_test_analysis: bool = True
    
    # 成果物生成設定
    generate_release_note: bool = True
    generate_faq_draft: bool = True
    generate_training_notice: bool = True
    generate_test_checklist: bool = True
    
    # カバレッジ提示必須
    require_coverage_disclosure: bool = True


class MaintenanceAgent(ResilientAgent):
    """メンテナンス支援Agent."""
    
    name = "MaintenanceAgent"
    
    async def analyze_diff(
        self, old_doc: str, new_doc: str
    ) -> list[DiffResult]:
        """仕様差分分析."""
        pass
    
    async def analyze_impact(
        self, diffs: list[DiffResult]
    ) -> ImpactAnalysis:
        """影響範囲分析."""
        pass
    
    async def generate_deliverables(
        self, diffs: list[DiffResult], impact: ImpactAnalysis
    ) -> dict[str, str]:
        """変更成果物自動生成."""
        deliverables = {}
        
        if self._config.generate_release_note:
            deliverables["release_note"] = await self._generate_release_note(
                diffs, impact
            )
        
        if self._config.generate_faq_draft:
            deliverables["faq_draft"] = await self._generate_faq_draft(
                diffs, impact
            )
        
        # カバレッジ提示を追加
        if self._config.require_coverage_disclosure:
            for key, value in deliverables.items():
                deliverables[key] = self._add_coverage_disclosure(
                    value, impact.coverage_info
                )
        
        return deliverables
    
    async def check_document_health(
        self, documents: list[str]
    ) -> dict[str, Any]:
        """ドキュメント健康度チェック."""
        return {
            "expired": [],      # 期限切れドキュメント
            "duplicates": [],   # 重複ドキュメント
            "missing_sections": [],  # 欠落セクション
            "recommendations": [],   # 改善提案
        }
```

### 4.3 機能3: 高層データ分析

#### 4.3.1 概要

```
⚠️ 重要: NL→SQL の最大のリスクは「能否生成SQL」ではなく：
1. 口径不一致（GMV/売上/粗利の定義が異なる）
2. 敏感データ誤照会（個人情報、給与、MyNumber等）
3. SQL暴走（全表スキャン、超大クエリ、コスト爆発）
4. 高層が「モデル提案」を決策依拠とする（説明と証拠チェーン必須）

解決策: 語義層（Semantic Layer）+ 受限SQL生成
```

#### 4.3.2 語義層アーキテクチャ

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         語義層（Semantic Layer）                         │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │                      指標辞典（Metrics Dictionary）               │ │
│  │                                                                   │ │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │ │
│  │  │ 売上        │  │ GMV         │  │ 粗利        │              │ │
│  │  ├─────────────┤  ├─────────────┤  ├─────────────┤              │ │
│  │  │定義:税抜金額│  │定義:総取引額│  │定義:売上-原価│              │ │
│  │  │口径:確定のみ│  │口径:キャンセル│ │口径:配送完了後│              │ │
│  │  │テーブル:sales│  │含む          │ │テーブル:profit│              │ │
│  │  │カラム:amount│  │テーブル:orders│ │カラム:margin │              │ │
│  │  │単位:円      │  │カラム:total  │  │単位:円      │              │ │
│  │  └─────────────┘  └─────────────┘  └─────────────┘              │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                                                         │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │                      ディメンション辞典                           │ │
│  │  [時間] [地域] [商品カテゴリ] [顧客セグメント] [販売チャネル]    │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                                                         │
│  ┌───────────────────────────────────────────────────────────────────┐ │
│  │                      アクセス制御                                 │ │
│  │  ホワイトリストデータセット: [sales, products, regions]          │ │
│  │  ブラックリスト: [employees.salary, customers.mynumber, ...]     │ │
│  └───────────────────────────────────────────────────────────────────┘ │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

#### 4.3.3 SQL護欄設計

```python
# apps/faq_system/backend/services/secure_text2sql.py

from dataclasses import dataclass, field
from typing import Any

@dataclass
class SQLGuardrails:
    """SQL護欄設定."""
    
    # 許可されるSQL操作
    allowed_operations: list[str] = field(default_factory=lambda: ["SELECT"])
    
    # 禁止キーワード
    forbidden_keywords: list[str] = field(default_factory=lambda: [
        "INSERT", "UPDATE", "DELETE", "DROP", "TRUNCATE", 
        "ALTER", "CREATE", "GRANT", "REVOKE"
    ])
    
    # ホワイトリストテーブル
    whitelist_tables: list[str] = field(default_factory=list)
    
    # ブラックリストカラム（個人情報等）
    blacklist_columns: list[str] = field(default_factory=lambda: [
        "*.password", "*.mynumber", "*.salary", "*.ssn",
        "employees.salary", "customers.credit_card",
        "customers.phone", "customers.address"
    ])
    
    # 自動制限
    auto_limit: int = 1000
    query_timeout_seconds: int = 30
    max_cost_threshold: float = 100.0  # クエリコスト上限
    
    # 監査
    audit_all_queries: bool = True


@dataclass
class AnalyticsResponse:
    """分析レスポンス（証拠チェーン付き）."""
    
    answer: str
    sql: str
    data: list[dict[str, Any]]
    chart: dict[str, Any] | None
    
    # 証拠チェーン（必須）
    evidence_chain: dict[str, Any] = field(default_factory=lambda: {
        "data_sources": [],       # データソース
        "query_conditions": [],   # クエリ条件
        "assumptions": [],        # 前提条件
        "limitations": [],        # 制限事項
        "alternatives": [],       # 代替オプション
    })
    
    # 不確定性
    uncertainty_level: str = "low"  # low, medium, high
    needs_verification: bool = False


class SecureText2SQLService:
    """セキュアText2SQLサービス."""
    
    def __init__(
        self,
        semantic_layer: "SemanticLayer",
        guardrails: SQLGuardrails,
    ) -> None:
        self._semantic_layer = semantic_layer
        self._guardrails = guardrails
    
    async def query(self, question: str, user_context: dict) -> AnalyticsResponse:
        """セキュアクエリ実行."""
        
        # 1. 権限チェック
        allowed_tables = self._get_allowed_tables(user_context)
        
        # 2. 語義層で指標/ディメンションを解決
        resolved = await self._semantic_layer.resolve(question)
        
        # 3. SQL生成（護欄適用）
        sql = await self._generate_sql(resolved, allowed_tables)
        
        # 4. SQL検証
        self._validate_sql(sql)
        
        # 5. 実行（タイムアウト + コスト制限）
        result = await self._execute_with_limits(sql)
        
        # 6. 証拠チェーン構築
        evidence = self._build_evidence_chain(
            question, sql, resolved, result
        )
        
        # 7. 回答生成
        answer = await self._generate_answer(question, result)
        
        return AnalyticsResponse(
            answer=answer,
            sql=sql,
            data=result.data,
            chart=self._generate_chart(result),
            evidence_chain=evidence,
        )
    
    def _validate_sql(self, sql: str) -> None:
        """SQL検証（護欄チェック）."""
        sql_upper = sql.upper()
        
        # 禁止キーワードチェック
        for keyword in self._guardrails.forbidden_keywords:
            if keyword in sql_upper:
                raise SecurityError(f"禁止されたSQL操作: {keyword}")
        
        # ブラックリストカラムチェック
        for column in self._guardrails.blacklist_columns:
            if self._column_in_sql(column, sql):
                raise SecurityError(f"アクセス禁止カラム: {column}")
        
        # LIMIT確認
        if "LIMIT" not in sql_upper:
            # 自動追加
            pass
```

#### 4.3.4 レスポンス例

```json
{
  "answer": "今月の売上TOP10は以下の通りです。1位は商品Aで1,500万円、2位は商品Bで1,200万円...",
  "sql": "SELECT product_name, SUM(amount) as total FROM sales WHERE ...",
  "data": [...],
  "chart": {
    "type": "bar",
    "title": "今月売上TOP10",
    "data": {...}
  },
  "evidence_chain": {
    "data_sources": [
      {"table": "sales", "description": "売上トランザクション", "last_updated": "2026-01-16"}
    ],
    "query_conditions": [
      "期間: 2026-01-01 〜 2026-01-16",
      "対象: 確定済み注文のみ（キャンセル除外）"
    ],
    "assumptions": [
      "売上定義: 税抜金額（tax_excluded_amount）",
      "商品集計: SKU単位"
    ],
    "limitations": [
      "返品処理中の注文は含まれていません",
      "本日分は15時時点のデータです"
    ],
    "alternatives": [
      "GMVベースで見る場合は「GMVでTOP10を見せて」と質問してください",
      "カテゴリ別内訳は「カテゴリ別売上」で確認できます"
    ]
  },
  "uncertainty_level": "low",
  "needs_verification": false
}
```

---

## 5. セキュリティ設計

### 5.1 認証統一

```python
# agentflow/security モジュールを活用

from agentflow.security import (
    AuthMiddleware,
    JWTConfig,
    PolicyEngine,
    AuthContext,
    AuthMode,
)

# SSO統合設定
sso_config = {
    "provider": "azure_ad",  # or "okta", "google"
    "tenant_id": "${AZURE_TENANT_ID}",
    "client_id": "${AZURE_CLIENT_ID}",
    "redirect_uri": "https://faq.example.com/auth/callback",
}

# JWT設定
jwt_config = JWTConfig(
    secret_key="${JWT_SECRET_KEY}",
    algorithm="RS256",
    expire_minutes=60,
    issuer="faq-system",
    audience="faq-system",
)
```

### 5.2 RBAC/ABAC 統合

```python
# apps/faq_system/backend/security/permission_config.py

from agentflow.security import PolicyEngine, Policy, AuthDecision

# ポリシーエンジン初期化
policy_engine = PolicyEngine()

# ロール定義
policy_engine.add_role("admin", permissions={"*"})
policy_engine.add_role("manager", permissions={
    "faq:read", "faq:write", "analytics:read", "analytics:execute"
})
policy_engine.add_role("employee", permissions={
    "faq:read", "analytics:read"
})
policy_engine.add_role("external", permissions={
    "external_kb:read"
})

# ABAC ポリシー（部門ベース）
department_policy = Policy(
    policy_id="department_access",
    name="部門ベースアクセス制御",
    subject_conditions={"department": ["HR", "Finance", "IT"]},
    resource_conditions={"sensitivity": ["low", "medium"]},
    action_conditions=["read"],
    effect=AuthDecision.ALLOW,
)
policy_engine.add_policy(department_policy)

# 高機密ポリシー
sensitive_policy = Policy(
    policy_id="sensitive_access",
    name="高機密アクセス制御",
    subject_conditions={"role": ["admin", "manager"]},
    resource_conditions={"sensitivity": ["high"]},
    action_conditions=["read"],
    effect=AuthDecision.ALLOW,
)
policy_engine.add_policy(sensitive_policy)
```

### 5.3 データ脱敏・PII保護

```python
# agentflow/security/data_sanitizer.py を活用

from agentflow.security import DataSanitizer, SanitizerConfig

# 日本向けPIIパターン追加
sanitizer_config = SanitizerConfig(
    detect_prompt_injection=True,
    detect_pii=True,
    detect_api_keys=True,
    mask_pii=True,
    block_injection=True,
    strict_mode=True,
)

sanitizer = DataSanitizer(config=sanitizer_config)

# MyNumber パターン追加（日本固有）
sanitizer.PII_PATTERNS["mynumber"] = r"\d{12}"  # 12桁数字

# 追加の敏感パターン
sanitizer.add_sensitive_word("マイナンバー")
sanitizer.add_sensitive_word("個人番号")
```

### 5.4 監査ログ

```python
# apps/faq_system/backend/security/audit_logger.py

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

@dataclass
class AuditLog:
    """監査ログエントリ."""
    
    timestamp: datetime
    user_id: str
    action: str  # query, view, export, etc.
    resource_type: str  # faq, analytics, document
    resource_id: str
    query_text: str
    response_summary: str
    data_sources_accessed: list[str]
    ip_address: str
    user_agent: str
    session_id: str
    
    # 結果
    success: bool
    error_message: str | None = None
    
    # セキュリティ関連
    pii_detected: bool = False
    pii_masked: bool = False
    injection_blocked: bool = False


class AuditLogger:
    """監査ログマネージャー."""
    
    async def log(self, entry: AuditLog) -> None:
        """ログ記録."""
        # DB保存 + 必要に応じてアラート
        pass
    
    async def query_logs(
        self,
        user_id: str | None = None,
        start_date: datetime | None = None,
        end_date: datetime | None = None,
        action: str | None = None,
    ) -> list[AuditLog]:
        """ログ検索."""
        pass
    
    async def detect_anomaly(self) -> list[dict]:
        """異常検知（大量アクセス、時間外アクセス等）."""
        pass
```

### 5.5 APPI対応（日本個人情報保護法）

```python
# apps/faq_system/backend/security/appi_compliance.py

@dataclass
class APPIConfig:
    """APPI（日本個人情報保護法）対応設定."""
    
    # 特定個人情報の完全除外
    exclude_mynumber: bool = True
    exclude_medical_info: bool = True
    exclude_criminal_record: bool = True
    
    # 漏洩時対応
    breach_notification_enabled: bool = True
    breach_notification_threshold: int = 1  # 1件でも通知
    
    # 保持期間
    data_retention_days: int = 365
    audit_log_retention_days: int = 1095  # 3年
    
    # 越権アラート
    unauthorized_access_alert: bool = True
    alert_channels: list[str] = field(default_factory=lambda: [
        "email:security@example.com",
        "slack:#security-alerts",
    ])


class APPIComplianceChecker:
    """APPI準拠チェッカー."""
    
    async def check_data_access(
        self, user_context: dict, data: dict
    ) -> tuple[bool, str]:
        """データアクセスの適法性チェック."""
        pass
    
    async def report_breach(
        self, breach_info: dict
    ) -> None:
        """漏洩報告（当局・本人通知）."""
        pass
```

---

## 6. 精度向上設計

### 6.1 引用来源システム

```python
# apps/faq_system/backend/services/citation_service.py

from dataclasses import dataclass
from typing import Any

@dataclass
class Citation:
    """引用情報."""
    
    # 必須フィールド
    document_id: str
    title: str
    source_path: str
    snippet: str
    relevance_score: float
    
    # バージョン管理
    version: str
    update_date: str
    effective_date: str | None = None
    expiry_date: str | None = None
    
    # 位置情報
    page_number: int | None = None
    section: str | None = None
    paragraph: int | None = None
    
    # 適用範囲
    applicable_scope: str | None = None  # 例: "正社員・契約社員"
    
    # Owner情報
    owner_department: str | None = None
    owner_contact: str | None = None


class CitationService:
    """引用管理サービス."""
    
    async def extract_citations(
        self, search_results: list[dict]
    ) -> list[Citation]:
        """検索結果から引用情報を抽出."""
        pass
    
    async def validate_citations(
        self, citations: list[Citation]
    ) -> list[Citation]:
        """引用の有効性を検証（期限切れ等チェック）."""
        valid = []
        for c in citations:
            if c.expiry_date and c.expiry_date < today():
                c.warning = "このドキュメントは期限切れの可能性があります"
            valid.append(c)
        return valid
    
    def format_citations(
        self, citations: list[Citation], format: str = "inline"
    ) -> str:
        """引用をフォーマット."""
        # inline: [1], [2] 形式
        # footnote: 脚注形式
        # full: 完全引用形式
        pass
```

### 6.2 信頼度評価

```python
# agentflow/security/hallucination_detector.py を活用

from agentflow.security import HallucinationDetector, DetectionConfig

detector_config = DetectionConfig(
    confidence_threshold=0.7,
    check_dates=True,
    check_numbers=True,
    check_citations=True,
    check_consistency=True,
    human_review_threshold=0.5,
    strict_mode=True,
)

detector = HallucinationDetector(config=detector_config)

# 回答検証
async def verify_answer(answer: str, context: str) -> dict:
    result = await detector.check(
        output=answer,
        context=context,
    )
    
    return {
        "confidence_score": result.confidence_score,
        "is_reliable": result.is_reliable,
        "needs_human_review": result.needs_human_review,
        "issues": [i.to_dict() for i in result.issues],
    }
```

### 6.3 不確定時の優雅な降格

```python
# apps/faq_system/backend/services/graceful_degradation.py

@dataclass
class DegradationConfig:
    """降格設定."""
    
    confidence_thresholds: dict[str, float] = field(default_factory=lambda: {
        "high_confidence": 0.8,    # 自信を持って回答
        "medium_confidence": 0.6,  # 注意書き付きで回答
        "low_confidence": 0.4,     # 確認を促す
        "no_confidence": 0.0,      # 回答を控える
    })
    
    # 降格時の動作
    suggest_contacts: bool = True
    auto_generate_ticket: bool = True
    show_alternatives: bool = True


class GracefulDegradationService:
    """優雅な降格サービス."""
    
    async def handle_low_confidence(
        self, question: str, confidence: float, partial_answer: str
    ) -> dict[str, Any]:
        """低信頼度時の処理."""
        
        response = {
            "answer": None,
            "confidence": confidence,
            "status": "needs_confirmation",
        }
        
        if confidence >= self._config.confidence_thresholds["medium_confidence"]:
            # 注意書き付きで回答
            response["answer"] = partial_answer
            response["warning"] = "この回答は一部の情報に基づいています。正確性を確認してください。"
        
        if confidence < self._config.confidence_thresholds["low_confidence"]:
            # 確認を促す
            response["message"] = "申し訳ありません。この質問に対する十分な情報が見つかりませんでした。"
            
            if self._config.suggest_contacts:
                response["suggested_contacts"] = await self._get_contacts(question)
            
            if self._config.auto_generate_ticket:
                ticket = await self._generate_ticket(question)
                response["ticket_id"] = ticket.id
                response["message"] += f"\n問合せチケット（{ticket.id}）を作成しました。"
        
        return response
```

### 6.4 術語辞書（Glossary）

```python
# apps/faq_system/backend/services/glossary_service.py

from dataclasses import dataclass
from typing import Any

@dataclass
class GlossaryEntry:
    """術語エントリ."""
    
    term: str                    # 正式名称
    aliases: list[str]           # 別名、略称、片仮名表記
    definition: str              # 定義
    category: str                # カテゴリ
    department: str              # 所管部門
    related_terms: list[str]     # 関連術語
    
    # 日本語特有
    reading_hiragana: str        # ひらがな読み
    reading_katakana: str        # カタカナ読み


class GlossaryService:
    """術語辞書サービス."""
    
    async def expand_query(self, query: str) -> str:
        """クエリを同義語で拡張."""
        # 例: "年休" → "年休 OR 年次有給休暇 OR 有給"
        pass
    
    async def normalize_term(self, term: str) -> str:
        """術語を正式名称に正規化."""
        pass
    
    async def suggest_terms(self, partial: str) -> list[GlossaryEntry]:
        """術語候補を提案."""
        pass


# 術語例
SAMPLE_GLOSSARY = [
    GlossaryEntry(
        term="年次有給休暇",
        aliases=["年休", "有給", "有給休暇", "年次休暇", "ネンキュウ"],
        definition="労働基準法に基づき付与される有給の休暇",
        category="人事制度",
        department="人事部",
        related_terms=["特別休暇", "振替休日", "代休"],
        reading_hiragana="ねんじゆうきゅうきゅうか",
        reading_katakana="ネンジユウキュウキュウカ",
    ),
    GlossaryEntry(
        term="稟議",
        aliases=["リンギ", "りんぎ", "決裁", "承認申請"],
        definition="組織内で意思決定の承認を得るための手続き",
        category="業務プロセス",
        department="総務部",
        related_terms=["決裁権限", "承認フロー"],
        reading_hiragana="りんぎ",
        reading_katakana="リンギ",
    ),
]
```

---

## 7. 知識運営設計

### 7.1 コンテンツ審批フロー

```
┌─────────────────────────────────────────────────────────────────────────┐
│                      コンテンツ審批フロー                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌──────────┐    ┌──────────┐    ┌──────────┐    ┌──────────┐         │
│  │ドキュメント│ -> │ AI自動   │ -> │ Owner    │ -> │ 専門部門 │         │
│  │アップロード│    │ 分類     │    │ レビュー │    │ 承認     │         │
│  └──────────┘    └──────────┘    └──────────┘    └──────────┘         │
│                                                                         │
│  承認フロー例:                                                          │
│  ┌─────────────────────────────────────────────────────────────────┐   │
│  │ カテゴリ        │ 承認者                                        │   │
│  ├─────────────────────────────────────────────────────────────────┤   │
│  │ 人事規則        │ 人事部 → 法務部                               │   │
│  │ セキュリティ規程│ 情報セキュリティ部 → CISO                     │   │
│  │ 製品情報        │ 製品企画部 → マーケティング部                 │   │
│  │ 技術ドキュメント│ 開発部門リーダー                              │   │
│  │ 対客公開資料    │ 広報部 → 法務部                               │   │
│  └─────────────────────────────────────────────────────────────────┘   │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### 7.2 フィードバック閉ループ

```python
# apps/faq_system/backend/services/feedback_service.py

from dataclasses import dataclass
from datetime import datetime
from typing import Any

@dataclass
class Feedback:
    """ユーザーフィードバック."""
    
    feedback_id: str
    question_id: str
    user_id: str
    timestamp: datetime
    
    # 評価
    helpful: bool
    accuracy_score: int | None = None  # 1-5
    completeness_score: int | None = None  # 1-5
    
    # コメント
    comment: str | None = None
    missing_info: str | None = None  # 不足情報
    incorrect_info: str | None = None  # 誤情報
    
    # 改善提案
    suggested_answer: str | None = None
    suggested_source: str | None = None


class FeedbackService:
    """フィードバック管理サービス."""
    
    async def submit_feedback(self, feedback: Feedback) -> None:
        """フィードバック登録."""
        # DB保存
        # 自動分析トリガー
        pass
    
    async def analyze_feedback(self) -> dict[str, Any]:
        """フィードバック分析."""
        return {
            "total_feedback": 0,
            "helpful_rate": 0.0,
            "common_issues": [],
            "knowledge_gaps": [],  # 待補充点
            "action_items": [],
        }
    
    async def generate_improvement_tasks(self) -> list[dict]:
        """改善タスク自動生成."""
        # 低評価の回答を分析
        # 不足情報を特定
        # 改善タスクを生成
        pass
```

### 7.3 期限切れ提醒

```python
# apps/faq_system/backend/services/expiry_monitor.py

from dataclasses import dataclass
from datetime import datetime, timedelta

@dataclass
class ExpiryConfig:
    """期限切れ監視設定."""
    
    # 事前通知
    warning_days: list[int] = None  # デフォルト: [30, 14, 7, 1]
    
    # 通知先
    notify_owner: bool = True
    notify_admin: bool = True
    
    # 自動処理
    auto_archive_after_days: int = 90  # 期限切れ後90日でアーカイブ
    auto_unpublish: bool = False  # 期限切れで自動非公開


class ExpiryMonitor:
    """期限切れ監視サービス."""
    
    async def check_expiring_documents(self) -> list[dict]:
        """期限切れ間近のドキュメントを検出."""
        pass
    
    async def send_expiry_notifications(self) -> None:
        """期限切れ通知を送信."""
        # Owner へメール/Slack通知
        pass
    
    async def generate_renewal_tasks(self) -> list[dict]:
        """更新タスクを生成."""
        pass
```

### 7.4 カバレッジダッシュボード

```python
# apps/faq_system/backend/services/coverage_dashboard.py

@dataclass
class CoverageMetrics:
    """カバレッジ指標."""
    
    # 部門別
    department_coverage: dict[str, float]  # 部門名 → カバレッジ率
    
    # 主題別
    topic_coverage: dict[str, float]  # 主題 → カバレッジ率
    
    # 質問統計
    total_questions: int
    answered_questions: int
    unanswered_questions: int
    low_confidence_answers: int
    
    # 命中率
    hit_rate: float  # 関連文書が見つかった割合
    satisfaction_rate: float  # 「役に立った」評価の割合


class CoverageDashboard:
    """カバレッジダッシュボード."""
    
    async def calculate_metrics(
        self, period_days: int = 30
    ) -> CoverageMetrics:
        """指標を計算."""
        pass
    
    async def identify_gaps(self) -> list[dict]:
        """ギャップを特定."""
        # 質問が多いが命中率が低い主題
        pass
    
    async def generate_recommendations(self) -> list[dict]:
        """改善提案を生成."""
        pass
```

---

## 8. API設計

### 8.1 エンドポイント一覧

```yaml
# API v3.0

# 認証
POST   /api/v3/auth/login          # ログイン（SSO）
POST   /api/v3/auth/token          # トークン更新
POST   /api/v3/auth/logout         # ログアウト

# チャット
POST   /api/v3/chat                # 同期チャット
POST   /api/v3/chat/stream         # ストリームチャット（SSE）
WS     /api/v3/ws/{session_id}     # WebSocket

# 社内FAQ
POST   /api/v3/internal/query      # 社内KB検索
POST   /api/v3/internal/feedback   # フィードバック送信
GET    /api/v3/internal/history    # 質問履歴

# 対客FAQ
POST   /api/v3/external/query      # 対客KB検索（権限制限）

# メンテナンス支援
POST   /api/v3/maintenance/diff    # 仕様差分分析
POST   /api/v3/maintenance/impact  # 影響分析
POST   /api/v3/maintenance/generate # 成果物生成
GET    /api/v3/maintenance/health  # ドキュメント健康度

# データ分析
POST   /api/v3/analytics/query     # NL→SQL分析
GET    /api/v3/analytics/metrics   # 指標辞書
GET    /api/v3/analytics/history   # クエリ履歴

# 管理
GET    /api/v3/admin/audit         # 監査ログ
GET    /api/v3/admin/coverage      # カバレッジ指標
POST   /api/v3/admin/documents     # ドキュメント管理
GET    /api/v3/admin/expiry        # 期限切れ確認
```

### 8.2 リクエスト/レスポンス例

```python
# POST /api/v3/internal/query

# リクエスト
{
    "question": "年次有給休暇の付与日数を教えてください",
    "options": {
        "require_citations": true,
        "max_citations": 5,
        "confidence_threshold": 0.6,
        "include_alternatives": true
    }
}

# レスポンス
{
    "answer": "年次有給休暇は、入社後6ヶ月間継続勤務し、全労働日の8割以上出勤した場合に10日付与されます [1]。その後は勤続年数に応じて増加し、6年6ヶ月以上で最大20日となります [1]。",
    "confidence": 0.92,
    "query_type": "rule",
    "citations": [
        {
            "index": 1,
            "document_id": "doc_hr_001",
            "title": "就業規則 第5章 休暇",
            "source": "hr_policies/work_rules_v3.2.pdf",
            "version": "3.2",
            "update_date": "2025-04-01",
            "effective_date": "2025-04-01",
            "page_number": 45,
            "section": "第25条",
            "snippet": "入社後6ヶ月間継続勤務し、全労働日の8割以上出勤した従業員には、10日の年次有給休暇を付与する。",
            "relevance_score": 0.95,
            "owner_department": "人事部",
            "applicable_scope": "正社員・契約社員"
        }
    ],
    "needs_confirmation": false,
    "suggestions": [
        {"text": "有給休暇の申請方法は？", "type": "followup"},
        {"text": "特別休暇の種類は？", "type": "related"}
    ],
    "metadata": {
        "execution_time_ms": 1250,
        "search_coverage": "hr_policies, employee_handbook",
        "model": "gpt-4-turbo"
    }
}
```

---

## 9. データモデル

### 9.1 ドキュメントモデル

```python
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

@dataclass
class Document:
    """ドキュメント."""
    
    # 識別
    document_id: str
    title: str
    
    # 分類
    category: str  # policy, manual, faq, product, etc.
    department: str
    tags: list[str] = field(default_factory=list)
    
    # コンテンツ
    content: str
    content_type: str  # text, pdf, html, docx
    source_path: str
    
    # バージョン管理
    version: str
    created_at: datetime
    updated_at: datetime
    created_by: str
    updated_by: str
    
    # 有効期限
    effective_date: datetime | None = None
    expiry_date: datetime | None = None
    
    # アクセス制御
    visibility: str = "internal"  # internal, external, confidential
    allowed_roles: list[str] = field(default_factory=list)
    allowed_departments: list[str] = field(default_factory=list)
    
    # Owner
    owner_id: str = ""
    owner_department: str = ""
    
    # 承認状態
    approval_status: str = "draft"  # draft, pending, approved, rejected
    approved_by: str | None = None
    approved_at: datetime | None = None
    
    # メタデータ
    metadata: dict[str, Any] = field(default_factory=dict)
```

### 9.2 質問履歴モデル

```python
@dataclass
class QuestionHistory:
    """質問履歴."""
    
    question_id: str
    session_id: str
    user_id: str
    timestamp: datetime
    
    # 質問内容
    question: str
    query_type: str  # faq, sql, maintenance
    
    # 回答
    answer: str
    confidence: float
    citations: list[dict] = field(default_factory=list)
    
    # 分析結果（SQLの場合）
    sql: str | None = None
    data: list[dict] | None = None
    
    # フィードバック
    feedback: dict | None = None
    
    # セキュリティ
    pii_detected: bool = False
    pii_masked: bool = False
```

### 9.3 監査ログモデル

```python
@dataclass
class AuditLog:
    """監査ログ."""
    
    log_id: str
    timestamp: datetime
    
    # ユーザー情報
    user_id: str
    user_role: str
    user_department: str
    
    # アクション
    action: str
    resource_type: str
    resource_id: str
    
    # 詳細
    request_body: dict
    response_summary: str
    data_sources_accessed: list[str]
    
    # 環境
    ip_address: str
    user_agent: str
    session_id: str
    
    # 結果
    success: bool
    error_message: str | None = None
    
    # セキュリティフラグ
    security_flags: dict = field(default_factory=dict)
```

---

## 10. 実装フェーズ

### Phase 1: 可信問答基盤（2-3ヶ月）

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Phase 1: 可信問答基盤                                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ 目標:                                                                   │
│   - 社内KB検索問答（引用付き）                                          │
│   - 権限制御（RBAC）                                                    │
│   - 基礎フィードバック閉ループ                                          │
│                                                                         │
│ データソース:                                                           │
│   1. 規則・制度文書（人事/総務/情報セキュリティ）                       │
│   2. 新入社員FAQ                                                        │
│   3. 製品公開資料（対客KB）                                             │
│                                                                         │
│ 成果物:                                                                 │
│   - InternalKBAgent                                                     │
│   - ExternalKBAgent                                                     │
│   - 認証・権限基盤                                                      │
│   - 監査ログ基盤                                                        │
│   - 基本UI                                                              │
│                                                                         │
│ KPI:                                                                    │
│   - 命中率 > 80%                                                        │
│   - 満足度 > 4.0/5.0                                                    │
│   - 平均応答時間 < 3秒                                                  │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Phase 2: メンテナンス支援（2-3ヶ月）

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Phase 2: メンテナンス支援                                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ 目標:                                                                   │
│   - 仕様差分総結                                                        │
│   - 影響範囲分析                                                        │
│   - 変更成果物自動生成                                                  │
│                                                                         │
│ 連携:                                                                   │
│   - プロジェクトドキュメント庫                                          │
│   - 変更管理システム（Jira/ServiceNow）                                 │
│   - Git リポジトリ                                                      │
│                                                                         │
│ 成果物:                                                                 │
│   - MaintenanceAgent                                                    │
│   - 差分抽出エンジン                                                    │
│   - 影響分析エンジン                                                    │
│   - 成果物テンプレート                                                  │
│                                                                         │
│ KPI:                                                                    │
│   - 差分抽出精度 > 90%                                                  │
│   - 影響分析カバレッジ > 85%                                            │
│   - 成果物生成時間削減 > 50%                                            │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Phase 3: 高層分析（3-4ヶ月）

```
┌─────────────────────────────────────────────────────────────────────────┐
│ Phase 3: 高層データ分析                                                  │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│ 目標:                                                                   │
│   - 語義層構築（指標口径統一）                                          │
│   - セキュアNL→SQL                                                      │
│   - 可視化・アクション提案                                              │
│                                                                         │
│ 前提条件:                                                               │
│   - ホワイトリストデータセット定義完了                                  │
│   - 指標辞書承認完了                                                    │
│   - SQL護欄テスト完了                                                   │
│                                                                         │
│ 成果物:                                                                 │
│   - SemanticLayer                                                       │
│   - SecureText2SQLService                                               │
│   - AnalyticsAgent                                                      │
│   - 証拠チェーン出力機能                                                │
│                                                                         │
│ KPI:                                                                    │
│   - SQL生成精度 > 85%                                                   │
│   - セキュリティ違反 = 0                                                │
│   - ユーザー満足度 > 4.0/5.0                                            │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 11. テストデータ

### 11.1 社内FAQ テストケース

```yaml
# tests/data/internal_faq_test_cases.yaml

test_cases:
  # 規則類（保守モード）
  - id: FAQ_001
    question: "年次有給休暇は何日もらえますか？"
    expected_query_type: "rule"
    expected_keywords: ["年次有給休暇", "付与日数"]
    expected_citation_required: true
    expected_answer_contains:
      - "6ヶ月"
      - "10日"
      - "8割以上出勤"
    expected_source: "就業規則"
    min_confidence: 0.8

  - id: FAQ_002
    question: "リモートワークの申請方法を教えてください"
    expected_query_type: "rule"
    expected_keywords: ["リモートワーク", "在宅勤務", "申請"]
    expected_citation_required: true
    expected_answer_contains:
      - "申請"
      - "上長承認"
    min_confidence: 0.7

  - id: FAQ_003
    question: "経費精算の締め日はいつですか？"
    expected_query_type: "rule"
    expected_keywords: ["経費精算", "締め日"]
    expected_citation_required: true
    expected_answer_contains:
      - "毎月"
    min_confidence: 0.7

  # 新入社員FAQ
  - id: FAQ_004
    question: "入社時に必要な書類は何ですか？"
    expected_query_type: "faq"
    expected_keywords: ["入社", "書類", "提出"]
    expected_answer_contains:
      - "マイナンバー"
      - "口座情報"
      - "年金手帳"
    min_confidence: 0.8

  - id: FAQ_005
    question: "社内システムのパスワードを忘れました"
    expected_query_type: "faq"
    expected_keywords: ["パスワード", "リセット"]
    expected_answer_contains:
      - "IT部門"
      - "リセット"
    min_confidence: 0.7

  # 不確定ケース（確認を促すべき）
  - id: FAQ_006
    question: "来年の昇給率はどのくらいですか？"
    expected_query_type: "faq"
    expected_needs_confirmation: true
    expected_message_contains:
      - "確認"
      - "人事部"

  # 対客公開情報
  - id: FAQ_007
    question: "製品Aの保証期間は？"
    expected_query_type: "faq"
    expected_visibility: "external"
    expected_answer_contains:
      - "保証"
      - "年"
    min_confidence: 0.8
```

### 11.2 メンテナンス支援テストケース

```yaml
# tests/data/maintenance_test_cases.yaml

test_cases:
  - id: MAINT_001
    name: "API仕様変更差分"
    old_document: |
      ## API: /api/users
      - Method: GET
      - Parameters:
        - user_id: required, string
      - Response:
        - name: string
        - email: string
    new_document: |
      ## API: /api/users
      - Method: GET
      - Parameters:
        - user_id: required, string
        - include_profile: optional, boolean
      - Response:
        - name: string
        - email: string
        - profile: object (when include_profile=true)
    expected_diffs:
      - category: "parameter"
        description: "新パラメータ追加: include_profile"
        severity: "low"
      - category: "response"
        description: "新フィールド追加: profile"
        severity: "low"
    expected_impact:
      affected_apis: ["/api/users"]
      affected_modules: ["UserService"]

  - id: MAINT_002
    name: "業務ルール変更"
    old_document: |
      ## 承認フロー
      - 10万円未満: 課長承認
      - 10万円以上: 部長承認
    new_document: |
      ## 承認フロー
      - 5万円未満: 課長承認
      - 5万円以上50万円未満: 部長承認
      - 50万円以上: 本部長承認
    expected_diffs:
      - category: "rule"
        description: "承認閾値変更: 10万円→5万円"
        severity: "high"
      - category: "rule"
        description: "新承認レベル追加: 本部長"
        severity: "high"
```

### 11.3 データ分析テストケース

```yaml
# tests/data/analytics_test_cases.yaml

test_cases:
  # 正常ケース
  - id: ANALYTICS_001
    question: "今月の売上TOP10を教えて"
    expected_sql_contains:
      - "SELECT"
      - "ORDER BY"
      - "LIMIT 10"
    expected_tables: ["sales"]
    expected_chart_type: "bar"
    security_check:
      should_pass: true

  - id: ANALYTICS_002
    question: "前年同期比で売上はどうなっていますか？"
    expected_sql_contains:
      - "SELECT"
      - "比較"
    expected_evidence_chain:
      data_sources: ["sales"]
      assumptions: ["売上定義"]
    security_check:
      should_pass: true

  # セキュリティ違反ケース（ブロックされるべき）
  - id: ANALYTICS_003
    question: "従業員の給与一覧を見せて"
    security_check:
      should_pass: false
      expected_block_reason: "アクセス禁止カラム: employees.salary"

  - id: ANALYTICS_004
    question: "顧客のマイナンバーを取得して"
    security_check:
      should_pass: false
      expected_block_reason: "アクセス禁止カラム: customers.mynumber"

  # 護欄テスト
  - id: ANALYTICS_005
    question: "全データを削除して"
    security_check:
      should_pass: false
      expected_block_reason: "禁止されたSQL操作: DELETE"
```

### 11.4 期待回答サンプル

```json
// tests/data/expected_answers.json

{
  "FAQ_001": {
    "question": "年次有給休暇は何日もらえますか？",
    "expected_answer": "年次有給休暇は、入社後6ヶ月間継続勤務し、全労働日の8割以上出勤した場合に10日付与されます [1]。その後は勤続年数に応じて以下のように増加します [1]:\n\n| 勤続年数 | 付与日数 |\n|---------|--------|\n| 6ヶ月 | 10日 |\n| 1年6ヶ月 | 11日 |\n| 2年6ヶ月 | 12日 |\n| 3年6ヶ月 | 14日 |\n| 4年6ヶ月 | 16日 |\n| 5年6ヶ月 | 18日 |\n| 6年6ヶ月以上 | 20日 |\n\n詳細は就業規則第25条をご確認ください。",
    "expected_citations": [
      {
        "index": 1,
        "title": "就業規則 第5章 休暇",
        "source": "hr_policies/work_rules_v3.2.pdf",
        "page_number": 45,
        "section": "第25条"
      }
    ],
    "expected_confidence_min": 0.85
  },
  
  "FAQ_006": {
    "question": "来年の昇給率はどのくらいですか？",
    "expected_answer": "申し訳ありません。来年度の昇給率については、現時点では正式な情報が確認できませんでした。\n\n確認方法:\n- 人事部（内線: 1234）にお問い合わせください\n- 年度末に発表される「給与改定通知」をご確認ください\n\n問合せチケット（#TKT-20260116-001）を作成しましたので、担当者から追って回答いたします。",
    "expected_needs_confirmation": true,
    "expected_confidence_max": 0.5
  },

  "ANALYTICS_001": {
    "question": "今月の売上TOP10を教えて",
    "expected_answer": "今月（2026年1月1日〜16日）の売上TOP10は以下の通りです:\n\n| 順位 | 商品名 | 売上金額 |\n|-----|--------|----------|\n| 1 | 商品A | ¥15,000,000 |\n| 2 | 商品B | ¥12,300,000 |\n| ... | ... | ... |\n\n※ 本データは2026年1月16日15:00時点の確定済み注文に基づいています。\n※ 売上定義: 税抜金額（キャンセル・返品除外）",
    "expected_evidence_chain": {
      "data_sources": [
        {"table": "sales", "description": "売上トランザクション"}
      ],
      "query_conditions": [
        "期間: 2026-01-01 〜 2026-01-16",
        "対象: 確定済み注文のみ"
      ],
      "assumptions": [
        "売上定義: 税抜金額"
      ],
      "limitations": [
        "本日分は15時時点のデータ"
      ]
    }
  }
}
```

---

## 12. 共通機能反哺

### 12.1 agentflow フレームワークへの貢献

本プロジェクトで開発された以下のコンポーネントは、agentflow フレームワークに反哺します：

| コンポーネント | 対象パス | 機能 |
|---------------|---------|------|
| **IsolatedKBManager** | `agentflow/knowledge/isolated_kb.py` | 複数KB物理隔離管理 |
| **SemanticLayer** | `agentflow/services/semantic_layer.py` | 指標/ディメンション辞書管理 |
| **TicketGenerator** | `agentflow/integrations/ticket_generator.py` | 工単自動生成（Jira/ServiceNow連携） |
| **DocHealthChecker** | `agentflow/knowledge/doc_health_checker.py` | ドキュメント健康度チェック |
| **CoverageAnalyzer** | `agentflow/analysis/coverage_analyzer.py` | KBカバレッジ分析 |
| **GlossaryService** | `agentflow/knowledge/glossary.py` | 術語辞書・同義語展開 |
| **CitationFormatter** | `agentflow/utils/citation_formatter.py` | 引用フォーマット |
| **GracefulDegradation** | `agentflow/patterns/graceful_degradation.py` | 優雅な降格パターン |

### 12.2 パターン共有

```python
# agentflow/patterns/graceful_degradation.py

"""優雅な降格パターン.

低信頼度回答の処理方法を標準化。

使用例:
    >>> from agentflow.patterns import GracefulDegradationMixin
    >>>
    >>> class MyAgent(ResilientAgent, GracefulDegradationMixin):
    ...     async def process(self, input_data):
    ...         result = await self._generate_answer(input_data)
    ...         if result.confidence < self.confidence_threshold:
    ...             return await self.handle_low_confidence(
    ...                 input_data, result
    ...             )
    ...         return result
"""
```

---

## 付録

### A. 用語集

| 用語 | 説明 |
|-----|------|
| KB | Knowledge Base（知識ベース） |
| RAG | Retrieval-Augmented Generation（検索増強生成） |
| NL2SQL | Natural Language to SQL |
| RBAC | Role-Based Access Control |
| ABAC | Attribute-Based Access Control |
| PII | Personally Identifiable Information |
| APPI | Act on the Protection of Personal Information（個人情報保護法） |
| 語義層 | Semantic Layer（指標定義の抽象化層） |

### B. 参考資料

- [agentflow 規範クイックリファレンス](../../docs/QUICK_REFERENCE.md)
- [agentflow パターンガイド](../../docs/PATTERNS_GUIDE.md)
- [agentflow セキュリティガイド](../../docs/SECURITY_GUIDE.md)

### C. 変更履歴

| バージョン | 日付 | 変更内容 |
|-----------|------|---------|
| 3.0.0 | 2026-01-16 | 初版作成 |
| 3.1.0 | 2026-01-16 | 実装完了、技術仕様・利用ガイド追加 |

---

## 13. 技術実装仕様

### 13.1 実装済みモジュール一覧

本設計に基づき、以下のモジュールが実装されています。

#### agentflow フレームワーク（共通機能）

| モジュール | パス | 説明 |
|-----------|------|------|
| **IsolatedKBManager** | `agentflow/knowledge/isolated_kb.py` | 社内/対客/機密KB の物理隔離管理 |
| **SemanticLayerService** | `agentflow/services/semantic_layer.py` | 指標・ディメンション辞書、SQL生成ヒント |
| **TicketGenerator** | `agentflow/integrations/ticket_generator.py` | Jira/ServiceNow 工単自動生成 |
| **DocHealthChecker** | `agentflow/knowledge/doc_health_checker.py` | 期限切れ/重複/品質チェック |

#### FAQ System Agents

| モジュール | パス | 説明 |
|-----------|------|------|
| **InternalKBAgent** | `backend/agents/internal_kb_agent.py` | 社内KB（RBAC + 保守モード + 必須引用） |
| **ExternalKBAgent** | `backend/agents/external_kb_agent.py` | 対客KB（公開情報のみ） |
| **MaintenanceAgent** | `backend/agents/maintenance_agent.py` | 仕様差分・影響分析・成果物生成 |
| **AnalyticsAgent** | `backend/agents/analytics_agent.py` | 語義層 + SQL護欄 + 証拠チェーン |

#### FAQ System Services

| モジュール | パス | 説明 |
|-----------|------|------|
| **GlossaryService** | `backend/services/glossary_service.py` | 術語辞書（同義語/略称/カタカナ） |
| **CitationService** | `backend/services/citation_service.py` | 引用ソース管理・フォーマット |
| **FeedbackService** | `backend/services/feedback_service.py` | フィードバック収集・分析 |
| **CoverageDashboard** | `backend/services/coverage_dashboard.py` | カバレッジ可視化 |

#### FAQ System Security

| モジュール | パス | 説明 |
|-----------|------|------|
| **PermissionConfig** | `backend/security/permission_config.py` | RBAC権限設定・フィールド制限 |
| **AuditLogger** | `backend/security/audit_logger.py` | 監査ログ・異常検知 |
| **APPIComplianceChecker** | `backend/security/appi_compliance.py` | PII検出/マスク/漏洩報告 |

### 13.2 テストデータ

| ファイル | 説明 |
|---------|------|
| `tests/data/internal_faq_test_cases.yaml` | 社内FAQテストケース（17件） |
| `tests/data/maintenance_test_cases.yaml` | メンテナンステストケース（7件） |
| `tests/data/analytics_test_cases.yaml` | 分析テストケース（14件） |
| `tests/data/expected_answers.json` | 期待回答データ |

---

## 14. システム利用ガイド

### 14.1 インストール

```bash
# 1. 依存関係のインストール
cd /path/to/serverlessAIAgents
pip install -e .

# 2. 環境変数の設定
export OPENAI_API_KEY="your-api-key"
export FAQ_DB_URL="postgresql://..."
```

### 14.2 基本的な使い方

#### 社内FAQ検索

```python
from apps.faq_system.backend.agents import InternalKBAgent, InternalKBConfig

# Agent 初期化
config = InternalKBConfig(
    conservative_mode=True,  # 規則類は保守モード
    require_citation=True,   # 引用必須
)
agent = InternalKBAgent(config=config)

# 質問実行
result = await agent.run({
    "question": "年次有給休暇は何日もらえますか？",
    "user_context": {
        "user_id": "user123",
        "role": "employee",
        "department": "営業部",
    },
})

# 結果確認
print(f"回答: {result['answer']}")
print(f"信頼度: {result['confidence']}")
print(f"引用: {result['citations']}")
```

#### 対客FAQ検索

```python
from apps.faq_system.backend.agents import ExternalKBAgent

# 対客KB（認証不要、公開情報のみ）
agent = ExternalKBAgent()

result = await agent.run({
    "question": "製品Aの保証期間は何年ですか？",
})
```

#### メンテナンス支援（仕様差分分析）

```python
from apps.faq_system.backend.agents import MaintenanceAgent

agent = MaintenanceAgent()

# 完全分析（差分 → 影響 → 成果物）
result = await agent.run({
    "action": "full",
    "old_document": old_spec,
    "new_document": new_spec,
})

# 結果
print(f"差分: {result['diffs']}")
print(f"影響: {result['impact']}")
print(f"Release Note: {result['deliverables']['release_note']}")
```

#### データ分析

```python
from apps.faq_system.backend.agents import AnalyticsAgent

agent = AnalyticsAgent()

result = await agent.run({
    "question": "今月の売上TOP10を教えてください",
    "user_context": {
        "user_id": "analyst001",
        "role": "analyst",
    },
})

# 結果（証拠チェーン付き）
print(f"回答: {result['answer']}")
print(f"SQL: {result['sql']}")
print(f"証拠チェーン: {result['evidence_chain']}")
```

### 14.3 サービス利用

#### 術語辞書

```python
from apps.faq_system.backend.services import GlossaryService

glossary = GlossaryService()

# クエリ拡張（同義語展開）
expanded = glossary.expand_query("有休申請")
# ["有休申請", "年次有給休暇申請", "休暇申請", ...]

# 正式名称取得
canonical = glossary.get_canonical("年休")
# "年次有給休暇"
```

#### 引用管理

```python
from apps.faq_system.backend.services import CitationService, SourceInfo

citation_service = CitationService()

# 引用作成
source = SourceInfo(
    source_id="doc-001",
    title="就業規則",
    version="3.2",
    updated_at=datetime.now(),
)
citation = citation_service.create_citation(
    source=source,
    snippet="年次有給休暇は...",
    page_number=45,
)

# フォーマット
formatted = citation_service.format_citations([citation], style="full")
```

#### フィードバック

```python
from apps.faq_system.backend.services import FeedbackService

feedback_service = FeedbackService()

# フィードバック送信
await feedback_service.submit_feedback(
    query_id="q-001",
    helpful=True,
    comment="わかりやすかった",
    topic="人事",
)

# 統計取得
stats = feedback_service.get_stats()
print(f"有用率: {stats.helpful_rate:.0%}")
```

### 14.4 セキュリティ機能

#### 権限チェック

```python
from apps.faq_system.backend.security import PermissionConfig

permission = PermissionConfig()

# アクセス権限チェック
result = permission.check_access(
    user_role="employee",
    kb_type="internal",
    action="read",
)
print(f"許可: {result['allowed']}")

# フィールド制限チェック
restricted = permission.is_field_restricted(
    field_name="employees.salary",
    user_role="employee",
)
print(f"制限: {restricted['restricted']}")
# True（給与情報は一般社員アクセス不可）
```

#### 監査ログ

```python
from apps.faq_system.backend.security import AuditLogger

audit = AuditLogger()

# アクセス記録
await audit.log_access(
    user_id="user123",
    action="search",
    resource="internal_kb",
    query="年休の付与日数",
)

# ログ検索
logs = await audit.search(
    user_id="user123",
    since=datetime.now() - timedelta(days=7),
)
```

#### APPI準拠（PII検出）

```python
from apps.faq_system.backend.security import APPIComplianceChecker

checker = APPIComplianceChecker()

# PII検出
text = "山田太郎のマイナンバーは123456789012です"
detections = checker.detect_pii(text)
for d in detections:
    print(f"タイプ: {d.pii_type}, 深刻度: {d.severity}")

# マスク処理
masked = checker.mask_pii(text)
# "山田太郎のマイナンバーは************です"
```

### 14.5 WebSocket / SSE 対応

```python
from fastapi import FastAPI, WebSocket
from apps.faq_system.backend.agents import InternalKBAgent

app = FastAPI()

@app.websocket("/ws/faq")
async def websocket_faq(websocket: WebSocket):
    await websocket.accept()
    agent = InternalKBAgent()
    
    while True:
        data = await websocket.receive_json()
        
        # ストリーム実行
        async for event in agent.run_stream(data):
            await websocket.send_json(event)
```

### 14.6 トラブルシューティング

#### よくある問題

| 問題 | 原因 | 対処法 |
|-----|------|--------|
| `PermissionError` | 権限不足 | ユーザーロールと KB タイプを確認 |
| 低信頼度回答 | KB にコンテンツ不足 | ドキュメント追加、術語辞書拡充 |
| SQL生成エラー | 指標未定義 | 語義層に指標を追加 |
| PII検出誤検知 | パターン過敏 | APPIConfig で閾値調整 |

#### ログ確認

```python
import logging

# デバッグログ有効化
logging.basicConfig(level=logging.DEBUG)
logging.getLogger("agentflow").setLevel(logging.DEBUG)
logging.getLogger("apps.faq_system").setLevel(logging.DEBUG)
```

### 14.7 ベストプラクティス

1. **KB設計**
   - 社内/対客KBは必ず物理隔離
   - 各ドキュメントにOwnerを設定
   - 有効期限を必ず設定

2. **セキュリティ**
   - 本番環境では `conservative_mode=True`
   - 機密データは `blacklist_columns` に追加
   - 監査ログを定期的にレビュー

3. **精度向上**
   - 術語辞書を定期的に更新
   - フィードバックを分析して改善
   - テストケースでリグレッションチェック

4. **運用**
   - カバレッジダッシュボードで定期モニタリング
   - 期限切れドキュメントを早期対応
   - 低パフォーマンストピックを優先改善

---

**Document Owner**: AI Platform Team  
**Last Updated**: 2026-01-16  
**Status**: Implementation Complete - Ready for Review
