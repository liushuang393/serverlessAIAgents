以下は、ここ数回の議論を **「他のAI助手へ指示するための要約（指示書）」** として整理したものです（日本語）。
※目的は **旧システム移行（COBOL→Java / Struts→Spring Boot / JDKアップグレード等）を、工程化・検証中心・設定差し替えで汎用化** すること。

---

# 1) 現状からの変更点・新規ルール（重要度順）

## P0：新規の最重要方針（“賢いAI”より“工程装置”）

* **意味等価（semantic equivalence）を最優先**：速度より正確性・再現性・検証性
* **推測・創作・勝手な最適化は禁止**（設計にないことはしない）
* **検証が主役**：テスト生成・差分比較・責任工程の特定を第一級概念にする

## P0：工程責任の切り分け（新規）

* 失敗時に「どの工程の問題か」を即特定するため、**QualityGateAgent（裁定者）を新設**

  * 修正はしない／責任工程を判定し、次に動かすAgentを指示する

## P1：会話中心→成果物中心（新規）

* Agent間の“会話”は禁止、**ファイル（JSON）で通信**
* 中間成果物は **artifacts/** に固定保存、会話コンテキストに溶かさない
* 重要判断は **DECISIONS.md**、失敗履歴は **FAILURES.md** に蓄積（外部記憶）

## P1：コンテキスト劣化対策（新規）

* 長いログは会話に流さない（ファイルへ退避）
* ERRORは機械可読（1行にERROR+理由+場所）
* デフォルト `--fast`（サブサンプル/スモーク）で短時間検証→OKなら拡大

## P2：汎用化（新規）

* “言語変換”ではなく **工程・契約（Spec）・Adapter差し替え** を軸に汎用化
  例：COBOL→Java、RPG→Java、Struts→Spring、Java8→21 は設定変更で対応

---

# 2) Agent設計（名称・目的・提示語・I/O JSON）

## 2.1 全体の共通提示語（System Prompt：全Agent共通）

```text
本アプリの目的は、旧システムを新技術基盤へ意味等価に移行し、
検証可能・再現可能・回帰可能な成果物を生成することである。

原則：
- 正確性は速度より優先
- 推測・創作・自己判断の最適化は禁止
- すべての工程は証拠（ログ/テスト/差分）を残す
- 失敗時は責任工程（設計/変換/テスト/環境）を特定する

あなたは「賢いAI」ではなく「工程に従う移行装置の一部」として振る舞え。
```

## 2.2 個別提示語の運用ルール（外部参照方式）

* 各Agentは **プロンプト本文を短く**し、詳細ルールは外部ファイル参照に統一

  * `prompts/common_system.md`
  * `prompts/agents/<agent>.md`
  * `specs/*.yaml`
* Agentへの指示は基本こうする：

```text
参照: prompts/common_system.md
参照: prompts/agents/LegacyAnalysisAgent.md
参照: specs/TaskSpec.yaml, specs/MigrationProfile.yaml
出力は schemas/legacy_analysis.schema.json に従う
```

---

## 2.3 Agent一覧（工程ベース、言語非依存）

### A) SupervisorAgent（工程制御）

* **目的**：工程遷移のみ／判断はSpecとQualityGate結果で行う
* **入力**：TaskSpec、各工程の出力JSON
* **出力**：次工程の起動指示（run plan）
* **個別Prompt要点**：生成・設計・修正をしない

### B) LegacyAnalysisAgent（新設・最重要：事実抽出）

* **目的**：旧コードから“事実”を抽出し固定（推測ゼロ）
* **入力**：ソース、MigrationProfile（source_language等）
* **出力JSON（例）**：

```json
{
  "meta": {"source_language":"cobol","module":"ABC001","trace_id":"..."},
  "programs": [],
  "entry_points": [],
  "io_contracts": [],
  "data_structures": [],
  "control_flow": [],
  "db_access": [],
  "external_calls": [],
  "unknowns": [],
  "extensions": {}
}
```

### C) MigrationDesignAgent（等価移行設計）

* **目的**：意味等価のための設計決定を明文化（理由付き）
* **入力**：LegacyAnalysis.json、MigrationProfile（target_framework等）
* **出力JSON（例）**：

```json
{
  "meta": {"target":"java","framework":"spring-boot","trace_id":"..."},
  "package_mapping": {},
  "class_mapping": {},
  "transaction_policy": {},
  "state_model": {},
  "framework_mapping": {},
  "rationale": {},
  "extensions": {}
}
```

### D) CodeTransformationAgent（機械的変換）

* **目的**：Designどおりに変換（自己判断禁止）
* **入力**：Legacy code + MigrationDesign.json
* **出力**：生成コード＋生成レポートJSON（どの規則を適用したか）

```json
{"generated_files":[], "rule_hits":[], "warnings":[], "extensions":{}}
```

### E) TestSynthesisAgent（超重要：Golden Master生成）

* **目的**：旧システム基準のテスト/データ/期待値を作る
* **入力**：LegacyAnalysis.json、必要なら旧実行ログ/DBスキーマ
* **出力**：テストコード＋テストデータ＋期待値（Golden Master）

### F) DifferentialVerificationAgent（差分検証：事実報告）

* **目的**：新旧の差分を分類して報告（修正案は禁止）
* **入力**：旧結果、 新結果、期待値
* **出力JSON（例）**：

```json
{
  "equivalence": false,
  "diffs": [
    {"type":"value","location":"response.amount","legacy":"100","new":"99"}
  ],
  "classification": "logic|data|timing|environment",
  "confidence": 0.9,
  "extensions": {}
}
```

### G) QualityGateAgent（新設：責任工程の裁定）

* **目的**：差分から“どの工程が悪いか”を判定し、次に動かすAgentを指定
* **入力**：Diff結果、Design、Test内容
* **出力JSON（例）**：

```json
{
  "decision": "DESIGN_ISSUE|TRANSFORM_ISSUE|TEST_ISSUE|ENV_ISSUE|KNOWN_LEGACY",
  "target_agent": "MigrationDesignAgent",
  "reason": "Transaction boundary mismatch",
  "severity": "HIGH"
}
```

### H) FixerAgent（限定修正）

* **目的**：QualityGateが指定した範囲だけ修正（影響拡大禁止）
* **入力**：decision + 対象ファイル + diff根拠
* **出力**：パッチ＋再テスト結果

### I) E2ETestAgent（必要時）

* **目的**：画面/ API / バッチのE2E
* **入力**：シナリオ定義（YAML）
* **出力**：実行ログ＋差分（DOM/API/DB）

---

## 2.4 通信（Agent間の約束）

* **通信媒体**：Repo内のファイルのみ（JSON / 生成物 / ログ）
* **推奨ディレクトリ**

```
artifacts/
  analysis/
  design/
  code/
  tests/
  diff/
logs/
current_tasks/   (lockファイル)
DECISIONS.md
FAILURES.md
EVALS/
```

* **ロック**：`current_tasks/<task_id>.lock`（二重着手防止）

---

# 3) 健壮・拡張・柔軟（多言語/多FW）設計の要点

## 3.1 高度抽象：工程は共通、差分は設定とAdapter

* **共通**：分析→設計→変換→テスト→差分→裁定→修正
* **差分**：言語/フレームワーク固有部は以下に隔離

  * `SourceAdapter`（COBOL/RPG/Struts…）
  * `TargetAdapter`（Java/Spring/.NET…）
  * `MappingRules`（framework_mapping等）
  * `Parser（MCP/ツール）`

## 3.2 低結合・高凝集：I/Oスキーマで結合する

* Agent同士は「文章」ではなく**Schema**で結合
* JSONは必ず `meta / unknowns / extensions` を持つ

  * **unknowns**：不明点を正直に固定（推測させない）
  * **extensions**：将来の互換性枠

## 3.3 強いデバッグ性：責任工程を一撃で絞る

* DifferentialVerification：差分の“分類”
* QualityGate：責任工程の“裁定”
* Fixer：指定範囲のみ修正（暴走防止）

---

# 4) 主要スキル設計・実装リマインド（精度/効率/非会話）

## 4.1 コアスキル（再利用単位）

* `extract_legacy_structure`（事実抽出：AST/依存/IO/DB）
* `derive_migration_design`（等価設計：取引境界/状態/例外/入出力）
* `transform_with_constraints`（設計拘束下の変換：規則ヒット記録）
* `generate_golden_master`（テスト+データ+期待値の生成）
* `run_differential_verification`（差分分類）
* `assign_responsibility`（QualityGate裁定）
* `repair_with_scope_limit`（限定修正）

## 4.2 精度向上の鉄則（COBOL→Javaで特に効く）

* **“言語翻訳”禁止**：必ず「IO契約」「状態」「DB副作用」を基準にする
* **変換結果は必ず“テストで締める”**：生成物の正しさはテストでしか担保できない
* **既知良品対照**（必要時）：切替で原因範囲を狭める（設計/変換/環境）
* **delta debugging前提**：組み合わせで壊れるのを想定し、切り分けを工程に入れる

## 4.3 効率（コスト）を落とす実務テク

* `--fast` スモーク→拡大（時間盲対策）
* ログ最小化＋集計先出し（Agentに同じ推論をさせない）
* 変更単位を小さく（マージ衝突を減らす）

---

## 4.4 Agent Lightning 着想の改善（2026-02-12 追記）

### 実施内容（本リポジトリ反映済み）

* `EngineConfig` に学習連携フックを追加

  * `lightning`: `enabled` / `backend(auto|builtin|microsoft)` / `enable_training` などを統一設定
  * `lightning_store`: 実行イベント/報酬を標準化保存（外部I/F固定）
  * `reward_evaluator`: 実行結果→報酬変換
* `BaseEngine` の `run()` / `run_stream()` で実行ライフサイクルを保存
* `BaseEngine.train_lightning()` で run 履歴から学習投入を実行
* `TrajectoryAdapter` で `node_complete` 系イベントから `(s,a,r,s')` を生成
* `agentflow/run/lightning_backend.py` を追加し、Microsoft Agent Lightning 利用時は内部で `Trainer` / `trace_context` を利用
* `CodeMigrationEngine` は既定で収集/学習を無効化（opt-in）

### 期待効果

* 既存 Agent 実装を壊さずに改善ループへ接続可能（外部I/F不変）
* diff/quality 判定を報酬として再利用し、変換品質の継続改善が可能
* stream 実行も含めて学習データ欠落を減らせる
* Microsoft ライブラリ未導入時も builtin fallback で機能継続できる

---

# 5) その他の補足（見落としがちな重要点）

## 5.1 セキュリティ/事故対策（P0）

* Tool実行は **最小権限・allowlist**
* `rm -rf`/`pkill` 等は原則禁止（必要なら人間承認）
* 監査ログ（誰が何を実行したか）

## 5.2 “旧システム自身の不具合”を扱う枠

* `known_legacy_issues.json` を用意
  → 不一致が「移行の不具合」か「旧側の既知問題」かを切り分ける

## 5.3 回帰（EVALS）を最初から持つ

* 小さな回帰セット（20〜100ケース）を常設
* モデル/ルール更新で必ず回す（壊れたら即分かる）

---


あなたは旧システム移行支援AIである。
目的は “意味等価移行” と “検証可能・再現可能な成果物” の生成であり、速度より正確性を優先する。

ルール：
- 推測・創作・勝手な最適化は禁止。Specに無い判断はしない。
- 工程は「分析→設計→変換→テスト生成→差分検証→品質裁定→限定修正」で固定。
- Agent間通信は会話ではなく artifacts/ 配下の JSON と生成物で行う。
- 失敗時は QualityGate により責任工程（設計/変換/テスト/環境/既知旧不具合）を特定し、次に動かすAgentを決める。
- すべての出力は指定Schemaに従い、unknowns に不明点を明示し、証拠（ログ/テスト/差分）を残す。
