# Legacy-to-Agent™ Enterprise Modernization Platform - 総体設計書

## 1. 製品ポジショニング

**一句話要約:**
> AI Agent がレガシーシステムを「理解 → 運用 → 改修」する新プラットフォーム

**ブランド名:** Legacy-to-Agent™ Enterprise Modernization Platform

---

## 2. 5層アーキテクチャ

```mermaid
graph TB
    subgraph "P Layer: プレゼンテーション"
        P1["AG-UI Web Console"]
        P2["CLI Interface"]
        P3["API Gateway"]
    end

    subgraph "M3 Layer: Agent 編排"
        M3A["CodeMigrationOrchestrator"]
        M3B["ApprovalFlow (HITL)"]
        M3C["Kill Switch"]
    end

    subgraph "M1 Layer: 旧システム摂取"
        M1A["legacy-ingestion Skill"]
        M1B["Source Adapters"]
        M1C["code-analysis Skill"]
    end

    subgraph "M2 Layer: 業務語義"
        M2A["business-semantics Skill"]
        M2B["業務フロー / イベント / ルール"]
    end

    subgraph "M4 Layer: 現代化生成"
        M4A["modernization-generator Skill"]
        M4B["cobol-migration Skill"]
        M4C["Target Adapters"]
    end

    subgraph "M5 Layer: 治理 & コンプライアンス"
        M5A["GovernanceEngine"]
        M5B["compliance-reporter Skill"]
        M5C["AuditLogger"]
    end

    P1 --> M3A
    P2 --> M3A
    P3 --> M3A
    M3A --> M1A
    M3A --> M3B
    M3A --> M3C
    M1A --> M1B
    M1A --> M1C
    M1A --> M2A
    M2A --> M2B
    M2A --> M4A
    M4A --> M4B
    M4A --> M4C
    M3A --> M5A
    M5A --> M5B
    M5A --> M5C
```

---

## 3. Skill 依存グラフ

```mermaid
graph LR
    CA["code-analysis<br/>(builtin)"] --> LI["legacy-ingestion<br/>(M1)"]
    LI --> BS["business-semantics<br/>(M2)"]
    BS --> MG["modernization-generator<br/>(M4)"]
    CM["cobol-migration<br/>(既存)"] --> MG
    BS --> CR["compliance-reporter<br/>(M5)"]
```

**自動依存解決:** `SkillEngine._resolve_dependencies()` が `depends_on` を解析し、
未登録の依存 Skill を再帰的に解決（最大深度 3）。

---

## 4. Agent 編排設計

### 7工程固定パイプライン

```
分析 → 設計 → [HITL承認] → 変換 → テスト → 差分検証 → 品質裁定 → 限定修正
```

### HITL 承認ポイント

| チェックポイント | リスクレベル | 承認対象 |
|----------------|-----------|---------|
| 設計工程後 | HIGH | 移行設計全体（変換ルール、マッピング） |

### Kill Switch

- `CodeMigrationEngine.kill()` でフラグ設定
- 各工程開始前にチェック → `killed` なら即停止
- 停止理由を返却

---

## 5. 製品パッケージ

| パッケージ | 入口メソッド | 実行範囲 | 対象顧客 |
|-----------|-----------|---------|---------|
| **A. Assessment** | `orchestrator.assess()` | 分析 + 業務モデリング + 報告書 | 初期診断 |
| **B. Modernization** | `orchestrator.modernize()` | 全7工程 + HITL + テスト | 完全移行 |
| **C. Agent Platform** | `orchestrator.platform_mode()` | 持続運用 Agent | 運用保守 |

---

## 6. GovernanceEngine 統合

```python
# engine.py での統合
self._governance = GovernanceEngine()

# 各工程の監査ログ記録
# → AuditLogger が操作履歴を保存
# → compliance-reporter Skill が集約して報告書生成
```

### 監査データフロー

```
Agent 操作 → GovernanceEngine.evaluate_tool()
  → AuditEvent 記録
  → compliance-reporter Skill が集約
  → 日本語の監査報告書
```

---

## 7. データフロー

```mermaid
sequenceDiagram
    participant User
    participant Orch as Orchestrator
    participant M1 as legacy-ingestion
    participant M2 as business-semantics
    participant HITL as ApprovalFlow
    participant M4 as modernization-generator
    participant M5 as compliance-reporter

    User->>Orch: source_code
    Orch->>M1: 解析要求
    M1-->>Orch: IngestionArtifact
    Orch->>M2: 業務モデリング
    M2-->>Orch: BusinessSemanticsArtifact
    Orch->>HITL: 設計承認要求
    HITL-->>Orch: 承認/却下
    alt 承認
        Orch->>M4: コード生成
        M4-->>Orch: ModernizationArtifact
        Orch->>M5: 報告書生成
        M5-->>Orch: ComplianceReport
    end
    Orch-->>User: 結果
```
