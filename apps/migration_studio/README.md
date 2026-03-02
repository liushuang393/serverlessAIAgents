# Migration Studio

COBOLレガシーシステム → Java Spring Boot REST API 自動移行ツール。

Migration Studio は UI / API / 進捗表示 / 成果物配布に専任し、
実行系は `code_migration_assistant` CLI（`run` 契約）へ委譲します。
これにより、Studio と実行エンジンの責務を分離し、単一の実行面に統一しています。

---

## 実行バックエンド（重要）

- 既定: `MIGRATION_EXECUTION_BACKEND=cma_cli`
- 互換経路: `MIGRATION_EXECUTION_BACKEND=legacy_engine`

```bash
# 既定（推奨）
export MIGRATION_EXECUTION_BACKEND=cma_cli

# 旧実装へフォールバック（互換検証用）
export MIGRATION_EXECUTION_BACKEND=legacy_engine
```

`cma_cli` では `POST /api/migrate/{task_id}/hitl` は Phase 1 で未サポート（501）です。
HITL 連携は Phase 2 で `code_migration_assistant` の ApprovalFlow と接続します。

---

## ⚠️ AI移行の制限事項（必読）

本ツールはAIを活用しますが、以下のパターンは**自動移行が困難**です。
担当者が事前に確認し、適切な設計方針を指示してください。

### 1. OS/環境依存機能

| COBOL機能 | 課題 | 対処方針 |
|-----------|------|----------|
| VSAM / ESDS / KSDS ファイル | Javaに直接対応物なし | JPA + RDB or S3/MinIO へ置換 |
| JCL（Job Control Language） | バッチ制御はJavaにない | Spring Batch へ移行（別設計必要） |
| SORT / MERGE 句 | JCLのSORT処理 | Stream API or DB ORDER BY |
| CALL 外部プログラム | DLL/LINK的な呼び出し | Microservice API or Jar分割 |
| QSAM シーケンシャルI/O | ファイル順次処理 | ファイル一括読み込み or Streaming |
| DB2 埋め込みSQL (EXEC SQL) | COBOLホスト変数付きSQL | Spring JDBC / JPA へ変換 |
| CICS コマンド | トランザクション管理の仕組みが異なる | Spring Transaction Manager へ移行 |
| MQ / COBOL-IT 等ミドル連携 | ベンダー固有API | Spring Integration / JMS へ置換 |

### 2. トランザクション設計

COBOLとJavaではトランザクション境界の考え方が異なります。

```
COBOL:  1処理 = 1COMMIT が多い（暗黙の境界）
Java:   @Transactional アノテーションで明示的に宣言
```

**移行時のデフォルト設計**（本ツールが適用する基準）:
- `EXEC SQL COMMIT` → `@Transactional(propagation=REQUIRED)` のメソッド境界
- `EXEC SQL ROLLBACK` → 例外スロー → Spring が自動ロールバック
- 複数テーブル更新 → 1つの `@Transactional` メソッドに包む
- 分離レベル: `READ_COMMITTED`（COBOLのデフォルトに合わせる）

### 3. 排他制御（Locking）

| COBOLパターン | Java移行パターン |
|--------------|----------------|
| `LOCK TABLE` | `@Lock(LockModeType.PESSIMISTIC_WRITE)` |
| `WITH HOLD` カーソル | `@Transactional` + ループ内一貫読み取り |
| `SELECT ... FOR UPDATE` | JPA `PESSIMISTIC_WRITE` or JDBC FOR UPDATE |
| ファイルロック（LOCK RECORD）| 楽観ロック（`@Version`）推奨 |

### 4. データ型精度

- `PIC S9(9)V9(2)` のような小数 → 必ず `BigDecimal`（`double`/`float` 禁止）
- `COMP-3`（パックドデシマル） → `BigDecimal` で精度保持
- `PIC X(n)` の末尾スペース → Java側で `String.trim()` 注意
- `COMP-5` / `BINARY` → `int` / `long` で表現可（符号有無を確認）

### 5. 一貫性保証（標準化ルール）

毎回の変換が一貫した結果になるよう、以下を固定します:

```yaml
命名規則:
  パッケージ: com.{company}.{domain}.{module}
  クラス: {ProgramId}Controller / Service / Repository
  DTO: {GroupName}Request / Response / Dto
  テーブル: snake_case（COBOLの WORKING-STORAGE グループ名から変換）

Springバージョン: 3.2.x (固定)
Javaバージョン: 17 (固定)
ビルドツール: Maven (固定)

エラーレスポンス形式:
  {"error": {"code": "...", "message": "...", "details": []}}

トランザクション境界:
  サービスメソッド = トランザクション境界（コントローラー層には付与しない）
```

---

## 前提条件・インストール

### 1. Python環境

```bash
# conda 環境を使用（本プロジェクト標準）
conda activate agentflow
python --version  # 3.13+ 確認
```

### 2. 追加依存（任意）

```bash
# legacy_engine を使う場合のみ必要
pip install claude-agent-sdk
```

### 3. Java開発環境（任意）

静的チェックや実行比較テストを行う場合のみ必要です。
`--fast` フラグを使う場合は不要です。

```bash
# Java JDK 17+
java --version  # 17+ 確認
javac --version

# Maven
mvn --version  # 3.8+ 確認
```

### 4. gnucobol（任意）

COBOL実行比較テスト（実際に実行して出力を比較）する場合のみ必要です。
`--fast` フラグを使う場合は不要です。

```bash
# Ubuntu/Debian
sudo apt-get install gnucobol

# macOS
brew install gnucobol

# 確認
cobc --version
```

---

## クイックスタート（Phase 1: CLI）

```bash
# conda環境を有効化
conda activate agentflow

# 単一COBOLファイルを変換（高速モード：実行比較スキップ）
python -m apps.migration_studio.cli migrate \
  apps/migration_studio/tests/fixtures/sample.cbl \
  --output /tmp/migration_test \
  --fast

# 出力先確認
ls /tmp/migration_test/SAMPLE/v1/
# 01_analysis/ 02_design/ 03_transform/ 04_verification/ 05_report/

# 完全モード（javac + gnucobol が必要）
python -m apps.migration_studio.cli migrate \
  path/to/your/program.cbl \
  --output /tmp/migration_output
```

### 期待される出力

```
[Stage 1] Analyzer        ... ✓ (LegacyAnalysisArtifact)
[Stage 2] Designer        ... ✓ (MigrationDesignArtifact)
[Stage 3] Transformer     ... ✓ (TransformationArtifact)
[Stage 4] TestGenerator   ... ✓ (TestSynthesisArtifact)
[Stage 5] Verifier        ... ✓ (DifferentialVerificationArtifact)
[Stage 6] QualityGate     ... ✓ PASSED

出力: /tmp/migration_test/SAMPLE/v1/
  03_transform/src/main/java/com/company/migration/controller/SampleController.java
  03_transform/src/main/java/com/company/migration/service/SampleService.java
  03_transform/pom.xml
  04_verification/ast_comparison.json
  05_report/report.md
```

---

## パイプラインアーキテクチャ

```
入力: COBOL ファイル (.cbl / .cob)
       ↓
┌─────────────────────────────────────────────────────────┐
│  Stage 1: Analyzer                                       │
│  └─ COBOL AST抽出・リスク評価・I/O契約特定               │
│  ⚡ HITL: unknowns≥5 / DB操作 / 外部連携 / 複雑ルール   │
│       ↓                                                  │
│  Stage 2: Designer                                       │
│  └─ REST API設計・クラス構造・トランザクション設計       │
│       ↓                                                  │
│  Stage 3: Transformer                                    │
│  └─ Java Spring Boot コード生成（フルプロジェクト）      │
│       ↓                                                  │
│  Stage 4: TestGenerator                                  │
│  └─ JUnit 5 + MockMvc テストケース生成                   │
│       ↓                                                  │
│  Stage 5: Verifier                                       │
│  └─ COBOL AST vs Java AST 差分検証                       │
│       ↓                                                  │
│  Stage 6: QualityGate                                    │
│  └─ PASSED / DESIGN_ISSUE / TRANSFORM_ISSUE / ENV_ISSUE │
└─────────────────────────────────────────────────────────┘
       ↓ PASSED                    ↓ ISSUE → Evolution Loop
  成果物zip生成              該当ステージを再実行（最大3回）
```

### Evolution Loop（自己適応進化）

品質ゲートで問題が検出された場合、自動的にプロンプトを改善して再実行します:

1. QualityGate が `DESIGN_ISSUE` → `designer.md` を更新 → Stage 2 から再実行
2. QualityGate が `TRANSFORM_ISSUE` → `transformer.md` を更新 → Stage 3 から再実行
3. QualityGate が `TEST_ISSUE` → `test_generator.md` を更新 → Stage 4 から再実行
4. 最大3イテレーション後に手動介入を要求

---

## 出力ディレクトリ構造

```
{output_dir}/{program_name}/
  v1/                              # 第1回実行
    01_analysis/
      analyzer.json                # LegacyAnalysisArtifact
    02_design/
      designer.json                # MigrationDesignArtifact
    03_transform/
      src/main/java/com/.../
        {Name}Controller.java
        {Name}Service.java
        {Name}Repository.java      # DBアクセスある場合
      src/test/java/com/.../
        {Name}ControllerTest.java
      pom.xml
    04_verification/
      verifier.json                # DifferentialVerificationArtifact
      ast_comparison.json          # 構造比較詳細
    05_report/
      report.md
  v2/                              # Evolution後（問題発生時）
  evolution.json                   # 進化履歴
final_report.md                    # 全体サマリー
download_*.zip                     # ダウンロード用zip
```

---

## よくあるエラーと対処

### `ModuleNotFoundError: No module named 'claude_agent_sdk'`
```bash
pip install claude-agent-sdk
```

### `CLINotFoundError: Claude Code CLI not found`
```bash
# Claude CLIが必要
pip install claude-agent-sdk  # CLIを含む
```

### `ENV_ISSUE: Java/gnucobol未インストール`
```bash
# 高速モードで実行（実行比較スキップ）
python -m apps.migration_studio.cli migrate file.cbl --output /tmp/out --fast
```

### `DESIGN_ISSUE: DTOフィールド数が不一致`
- Evolution Loop が自動的にプロンプトを改善して再実行します
- 3回失敗した場合、`evolution.json` を確認して手動で `agents/prompts/designer.md` を修正

---

## 設定

### 環境変数

| 変数 | 説明 | デフォルト |
|------|------|----------|
| `ANTHROPIC_API_KEY` | Anthropic APIキー | 必須 |
| `MIGRATION_MODEL` | 使用するClaudeモデル | `claude-opus-4-6` |
| `MIGRATION_MAX_ITERATIONS` | Evolution最大反復数 | `3` |
| `MIGRATION_OUTPUT_ROOT` | 出力ルートディレクトリ | `migration_output` |

### app_config.json

`apps/migration_studio/app_config.json` でポートやエージェント設定を管理します。

---

## 開発者向け

### テスト実行

```bash
conda activate agentflow

# E2Eテスト（サンプルCOBLファイル使用）
pytest apps/migration_studio/tests/ -v

# 高速モード（実行比較スキップ）
pytest apps/migration_studio/tests/ -v -k "fast"
```

### プロンプトの手動改善

各ステージのプロンプトは `agents/prompts/` 配下のMarkdownファイルです。
Evolutionログ（`evolution.json`）を参考に改善できます。

```
apps/migration_studio/agents/prompts/
  analyzer.md        # COBOL解析ルール
  designer.md        # Spring Boot設計ルール
  transformer.md     # コード変換ルール
  test_generator.md  # テスト生成ルール
  verifier.md        # 差分検証ルール
  quality_gate.md    # 品質判定ルール
```
