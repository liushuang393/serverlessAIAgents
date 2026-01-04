# Decision Governance Engine

企業級意思決定支援システム - 「道・法・術・器」フレームワークによるMulti-Agentシステム

---

## 1. システム概要

Decision Governance Engineは、企業の重要な意思決定を支援するマルチエージェントシステムです。
利用者は企業のCEO、CFO、CTOなどの責任者を対象としており。

### アーキテクチャ概要

```
入力 → GatekeeperAgent → ClarificationAgent → DaoAgent → FaAgent → ShuAgent → QiAgent → ReviewAgent → レポート出力
         (門番)           (診断)             (道)       (法)       (術)       (器)       (検証)
```

### コア概念

| 層 | Agent | 役割 |
|---|-------|------|
| 🚪 門番 | GatekeeperAgent | 不適格な質問を門前払い |
| 🔍 診断 | ClarificationAgent | 論理的穴・暗黙の仮定・認知バイアス特定 |
| 🎯 道 | DaoAgent | 問題の本質抽出、因果齿轮分析 |
| ⚖️ 法 | FaAgent | 戦略パス評価（稳健型 vs 激进型） |
| 📋 術 | ShuAgent | 具体的実行計画策定 |
| 🔧 器 | QiAgent | 技術実装方針策定 |
| ✅ 検証 | ReviewAgent | 全層検証・最終判定（PASS/REVISE/REJECT） |

### 技術スタック

**バックエンド:**
- Python 3.13+
- FastAPI（Web API）
- Pydantic v2（バリデーション）
- asyncio（非同期処理）

**フロントエンド:**
- React 18 + TypeScript
- Vite（ビルドツール）
- Tailwind CSS
- Zustand（状態管理）

---

## 2. 機能一覧

### 2.1 API エンドポイント

| エンドポイント | メソッド | 説明 |
|--------------|---------|------|
| `/api/health` | GET | ヘルスチェック |
| `/api/agents` | GET | Agent定義取得 |
| `/api/decision` | POST | 同期的意思決定処理 |
| `/api/decision/stream` | GET | SSEストリーム付き処理 |
| `/ws/decision` | WebSocket | リアルタイム進捗通知 |
| `/api/report/{id}/pdf` | GET | PDFエクスポート |
| `/api/report/{id}/components` | GET | A2UIコンポーネント取得 |
| `/api/report/{id}/agent/{agent_id}` | GET | 個別Agent出力取得 |
| `/api/workflow/config` | GET | Studio UI設定取得 |

### 2.2 入力スキーマ

```python
# DecisionRequest（必須フィールド）
{
    "question": "意思決定の質問（10〜2000字）",
    "constraints": {
        "budget": {"amount": 500, "currency": "JPY"},
        "timeline": {"months": 6},
        "technical": ["Python", "AWS"],
        "regulatory": ["GDPR"]
    }
}
```

### 2.3 出力レポート

- エグゼクティブサマリー（30字以内の一文結論）
- 道（本質）分析結果
- 法（戦略）パス評価
- 術（計画）フェーズ定義
- 器（技術）実装方針
- 検証結果・署名欄

---

## 3. 使用方法

本システムは **Web UI（推奨）** と **CLI** の2つの利用方法を提供します。

| 方法 | 用途 | 対象ユーザー |
|-----|------|------------|
| 🖥️ Web UI | メイン利用方法、リアルタイム進捗表示 | CEO/CFO/CTO等の経営層 |
| ⌨️ CLI | バッチ処理、スクリプト連携 | 開発者・自動化用途 |

---

## 4. 環境構築（新規セットアップ）

### 4.1 前提条件

| 項目 | 要件 |
|-----|------|
| Python | 3.13以上 |
| Node.js | 20以上 |
| パッケージマネージャ | pip, npm |
| API Key | OpenAI / Anthropic / Gemini（いずれか1つ以上） |

### 4.2 リポジトリ取得

```bash
git clone https://github.com/liushuang393/serverlessAIAgents.git
cd serverlessAIAgents
```

### 4.3 バックエンド環境構築

```bash
# conda環境を使用する場合
conda activate agentflow

# または venv を使用する場合
python -m venv .venv
source .venv/bin/activate  # Linux/Mac
# .venv\Scripts\activate   # Windows

# 依存関係インストール
pip install -e ".[dev,studio]"
```

### 4.4 フロントエンド環境構築

```bash
cd apps/decision_governance_engine/frontend
npm install
```

### 4.5 環境変数設定

```bash
# ~/.bashrc に追加（WSL/Linux）
export OPENAI_API_KEY="your_openai_key"
export ANTHROPIC_API_KEY="your_anthropic_key"
export GEMINI_API_KEY="your_gemini_key"

# 反映
source ~/.bashrc
```

---

## 5. 起動手順

### 5.1 Web UI モード（推奨）

**ターミナル1: バックエンドAPI起動**
```bash
cd /mnt/d/pythonPJ/serverlessAIAgents
conda activate agentflow

# APIサーバー起動（ポート8000）
uvicorn apps.decision_governance_engine.api:app --reload --host 0.0.0.0 --port 8000
```

**ターミナル2: フロントエンド起動**
```bash
cd /mnt/d/pythonPJ/serverlessAIAgents/apps/decision_governance_engine/frontend

# 開発サーバー起動（ポート5173）
npm run dev
```

**ブラウザでアクセス**
```
http://localhost:5173
```

### 5.2 動作確認

```bash
# APIヘルスチェック
curl http://localhost:8000/api/health
# 期待出力: {"status":"ok","version":"1.0.0"}

# Swagger UI（API仕様書）
open http://localhost:8000/docs
```

### 5.3 CLI モード（開発者向け）

```bash
# 基本実行
python -m apps.decision_governance_engine.main "新規事業AとBのどちらに投資すべきか判断したい"

# 制約条件付き（予算500万円、期間6ヶ月）
python -m apps.decision_governance_engine.main "新規事業への投資判断をしたい" --budget 500 --timeline 6

# インタラクティブモード
python -m apps.decision_governance_engine.main --interactive
```

---

## 6. テスト手順

### 6.1 ユニットテスト実行

```bash
# Decision Governance Engine専用テスト
pytest tests/unit/test_decision_governance_engine.py -v

# カバレッジ付き
pytest tests/unit/test_decision_governance_engine.py -v \
  --cov=apps.decision_governance_engine \
  --cov-report=term-missing

# 特定テストクラスのみ
pytest tests/unit/test_decision_governance_engine.py::TestDecisionEngine -v
```

### 6.2 統合テスト

```bash
# APIサーバー起動後
pytest tests/integration/ -v -m "decision"
```

### 6.3 フロントエンドテスト

```bash
cd apps/decision_governance_engine/frontend
npm run test
npm run lint
npm run type-check
```

### 6.4 Lint/型チェック

```bash
# Ruff（リンター）
ruff check apps/decision_governance_engine/

# Mypy（型チェック）
mypy apps/decision_governance_engine/
```

---

## 7. 本番デプロイ手順

### 7.1 ビルド

```bash
# フロントエンドビルド
cd apps/decision_governance_engine/frontend
npm run build

# 静的ファイルは dist/ に出力される
```

### 7.2 本番起動

```bash
# Gunicorn + Uvicorn（推奨）
gunicorn apps.decision_governance_engine.api:app \
  -w 4 \
  -k uvicorn.workers.UvicornWorker \
  --bind 0.0.0.0:8000

# または Uvicorn 単体
uvicorn apps.decision_governance_engine.api:app \
  --host 0.0.0.0 \
  --port 8000 \
  --workers 4
```

### 7.3 Docker（推奨）

```bash
# イメージビルド
docker build -t decision-engine:latest -f Dockerfile.decision .

# コンテナ起動
docker run -d \
  -p 8000:8000 \
  -e OPENAI_API_KEY=${OPENAI_API_KEY} \
  --name decision-engine \
  decision-engine:latest
```

### 7.4 本番チェックリスト

- [ ] 環境変数（APIキー等）が正しく設定されている
- [ ] CORS設定が本番ドメインに限定されている
- [ ] ログレベルがINFO以上に設定されている
- [ ] ヘルスチェックエンドポイントが応答する
- [ ] SSL/TLS証明書が設定されている（HTTPS）

---

## 8. ディレクトリ構成

```
apps/decision_governance_engine/
├── __init__.py          # パッケージ初期化
├── main.py              # CLIエントリーポイント
├── api.py               # FastAPI REST API
├── workflow.py          # DecisionEngine本体
├── agent.yaml           # Agent/Workflow設定
├── agents/              # 各Agentの実装
│   ├── base_agent.py
│   ├── gatekeeper_agent.py
│   ├── clarification_agent.py
│   ├── dao_agent.py
│   ├── fa_agent.py
│   ├── shu_agent.py
│   ├── qi_agent.py
│   └── review_agent.py
├── schemas/             # Pydanticスキーマ
│   ├── input_schemas.py
│   ├── output_schemas.py
│   └── agent_schemas.py
├── services/            # ビジネスロジック
│   ├── pdf_generator.py
│   └── ui_components.py
├── skills/              # SKILL.md定義
├── frontend/            # React フロントエンド
└── design/              # 設計ドキュメント
```

---

## 9. トラブルシューティング

### よくある問題

**Q: `ModuleNotFoundError: No module named 'apps'`**
```bash
# プロジェクトルートから実行するか、PYTHONPATHを設定
export PYTHONPATH="${PYTHONPATH}:$(pwd)"
```

**Q: APIキーエラー**
```bash
# 環境変数を確認
echo $OPENAI_API_KEY
# .envファイルを確認
cat .env
```

**Q: ポート8000が使用中**
```bash
# 別ポートを指定
uvicorn apps.decision_governance_engine.api:app --port 8001
```

---

## 10. 関連ドキュメント

- [設計仕様書](design/decision-agent-spec.md)
- [実装計画](IMPLEMENTATION_PLAN.md)
- [変更履歴](CHANGELOG_v2.0.md)
