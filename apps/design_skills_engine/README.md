# Design Skills Engine（デザインスキルエンジン）

Lovart風「Design Skills」思想をAgentFlowフレームワークで実装したマルチ画像生成パイプライン。

自然言語のデザインブリーフから、統一スタイルの画像セットをComfyUI経由で生成する。

---

## 使用手順

### 1. 前提条件

#### 1.1 AgentFlow フレームワークのインストール

```bash
# リポジトリルートで実行
pip install -e ".[dev]"
```

#### 1.2 ComfyUI サーバーの起動

**方法A: Docker で起動（推奨）**

```bash
# ステップ1: モデルをダウンロード (~6.9GB、初回のみ)
./apps/design_skills_engine/scripts/setup_comfyui.sh

# ステップ2: apps/design_skills_engine ディレクトリに移動
cd apps/design_skills_engine

# ステップ3: Docker Compose で起動
# GPU モード（NVIDIA GPU がある場合）
COMFYUI_IMAGE=yanwk/comfyui-boot:cu128-slim COMFYUI_MODELS_DIR=../../models docker compose up -d

# CPU モード（GPU がない場合、生成速度は遅い）
COMFYUI_MODELS_DIR=../../models docker compose \
  -f docker-compose.yml -f docker-compose.cpu.yml up -d

# ステップ4: ヘルスチェック待機（起動完了まで1-2分）
until curl -sf http://localhost:8188/prompt > /dev/null; do
  echo "ComfyUI 起動待機中..."
  sleep 5
done
echo "✓ ComfyUI サーバー起動完了"

# 停止する場合
docker compose down
```

**方法B: ローカルインストールで起動**

```bash
# ステップ1: ComfyUI をクローン（初回のみ）
git clone https://github.com/comfyanonymous/ComfyUI.git ~/ComfyUI
cd ~/ComfyUI

# ステップ2: 依存関係をインストール（初回のみ）
pip install -r requirements.txt

# ステップ3: モデルをダウンロード（初回のみ、~6.9GB）
# models/checkpoints/ ディレクトリに配置
mkdir -p models/checkpoints
cd models/checkpoints
wget https://huggingface.co/stabilityai/stable-diffusion-xl-base-1.0/resolve/main/sd_xl_base_1.0.safetensors
cd ~/ComfyUI

# ステップ4: サーバーを起動（別ターミナルで実行）
python main.py --listen 0.0.0.0 --port 8188

# 起動確認（別ターミナルで実行）
curl -sf http://localhost:8188/prompt && echo "✓ ComfyUI 起動成功"
```

#### 1.3 環境変数の設定（任意）

```bash
# ComfyUI URL（デフォルト: http://localhost:8188）
export COMFYUI_URL=http://localhost:8188

# LLM強化モード使用時（IntentAnalyzer, PromptPlanner で使用）
export OPENAI_API_KEY=
```

### 2. 基本使用（Pythonコード）

```python
from agentflow.skills.builtin.design_skills.engine import DesignSkillsEngine

# エンジンを初期化
engine = DesignSkillsEngine()

# 自然言語ブリーフで画像セットを生成
result = await engine.run({
    "brief": "アウトドアBluetoothスピーカーの商品画像、テクノ風、黒系、防水・音質を強調",
    "num_images": 8,
    "target_platform": "amazon",
    "style_preferences": ["tech", "dark", "minimal"],
    "brand_colors": ["#000000", "#1E90FF"],
    "aspect_ratio": "1:1",
})
```

### 3. SSEストリーム（リアルタイム進捗表示）

```python
async for event in engine.run_stream({
    "brief": "コーヒーブランドのInstagram投稿画像",
    "num_images": 6,
    "target_platform": "instagram",
    "aspect_ratio": "4:5",
}):
    print(event)  # AG-UI イベント形式
```

### 4. 個別Agent使用

```python
from agentflow.skills.builtin.design_skills.agents.intent_analyzer_agent import IntentAnalyzerAgent
from agentflow.skills.builtin.design_skills.agents.prompt_planner_agent import PromptPlannerAgent
from agentflow.skills.builtin.design_skills.schemas.design_schemas import (
    DesignBriefInput,
    PromptPlanInput,
)

# Step 1: 意図解析
analyzer = IntentAnalyzerAgent()
intent = await analyzer.process(DesignBriefInput(
    brief="ラップトップスタンドの商品画像",
    num_images=4,
))

# Step 2: プロンプト計画生成
planner = PromptPlannerAgent()
plan = await planner.process(PromptPlanInput(
    intent=intent,
    brand_colors=["#333333"],
    aspect_ratio="1:1",
))

# Step 3: 計画の確認・手動調整
for img in plan.images:
    print(f"{img.image_id} ({img.role.value}): {img.prompt}")
```

### 5. AgentFlowスキルとして使用

```python
from agentflow.skills.builtin.design_skills.skills.design_skills import run

result = await run({
    "brief": "商品画像を生成",
    "num_images": 4,
})
```

---

## アーキテクチャ

```
[ユーザー自然言語ブリーフ]
          │
          ▼
┌─────────────────────────┐
│ IntentAnalyzerAgent     │  ← デザイン意図を理解
│  - カテゴリ分類          │
│  - 被写体抽出            │
│  - 特徴抽出              │
│  - 画像役割配分          │
└─────────────────────────┘
          │（IntentAnalysis）
          ▼
┌─────────────────────────┐
│ PromptPlannerAgent      │  ← プロンプト計画を生成
│  - グローバルスタイル     │
│  - 個別画像プロンプト     │
│  - 一貫性シード          │
└─────────────────────────┘
          │（PromptPlanOutput）
          ▼
┌─────────────────────────┐
│ WorkflowExecutorAgent   │  ← ComfyUI経由で実行
│  - ワークフローJSON構築   │
│  - キューイング + ポーリング│
│  - 画像収集 + 保存       │
└─────────────────────────┘
          │
          ▼
[統一スタイルの画像セット]
```

---

## ディレクトリ構造

```
apps/design_skills_engine/
├── __init__.py              # パブリックAPI
├── engine.py                # DesignSkillsEngine（PipelineEngine継承）
├── agents/
│   ├── __init__.py
│   ├── agent_definitions.yaml   # YAML Agent定義（前後端共有）
│   ├── intent_analyzer_agent.py # デザイン意図解析
│   ├── prompt_planner_agent.py  # プロンプト計画生成
│   └── workflow_executor_agent.py # ComfyUIワークフロー実行
├── schemas/
│   ├── __init__.py
│   └── design_schemas.py       # 全Pydanticスキーマ
├── services/
│   ├── __init__.py
│   └── agent_registry.py       # Agent管理レジストリ
├── tools/
│   ├── __init__.py
│   └── comfyui_client.py       # ComfyUI非同期HTTPクライアント
└── prompts/                     # 将来のカスタムプロンプト用
```

---

## 画像役割（ImageRole）

| 役割 | 説明 | 推奨枚数 |
|------|------|----------|
| HERO | メインビジュアル・キービジュアル | 1枚 |
| FEATURE | 特徴ハイライト（防水、音質等） | 2-4枚 |
| DETAIL | クローズアップ・ディテール | 1-2枚 |
| LIFESTYLE | 使用シーン・ライフスタイル | 1-2枚 |
| COMPARISON | サイズ比較 | 0-1枚 |
| INFOGRAPHIC | テキストオーバーレイ・スペック | 0-1枚 |

---

## 最佳実践（ベストプラクティス）

### 1. ブリーフの書き方

**良い例:**
```
アウトドアBluetoothスピーカーの商品画像、
テクノ風、黒系、
防水と高音質を強調、
Amazon出品用
```

**改善が必要な例:**
```
スピーカーの写真
```

**ポイント:**
- 被写体を具体的に記述する
- スタイルキーワードを含める（テクノ、ミニマル、暖色系等）
- 強調すべき特徴を明記する
- ターゲットプラットフォームを指定する

### 2. 一貫性制御

- **共有シード**: 全画像が同じseedを使用し、スタイルの一貫性を確保
- **グローバルスタイル**: カラーパレット、ライティング、ネガティブプロンプトを統一
- **LoRA**: `GlobalStyle.lora_models` でスタイル一貫性をさらに強化

### 3. プラットフォーム別推奨設定

| プラットフォーム | アスペクト比 | 枚数 | 注意点 |
|----------------|-------------|------|--------|
| Amazon | 1:1 | 7-9枚 | 白背景推奨、1000x1000以上 |
| Instagram | 1:1 or 4:5 | 4-6枚 | 外側10%にテキスト配置を避ける |
| Website | 16:9 | 3-5枚 | ヒーロー画像を必ず含める |
| 楽天 | 1:1 | 7-12枚 | テキスト埋め込み画像あり |

### 4. ComfyUI の設定

```bash
# 推奨モデル（SDXL）
# models/checkpoints/ に以下を配置:
# - sd_xl_base_1.0.safetensors

# IPアダプター（スタイル一貫性強化用、任意）
# models/ipadapter/ に配置

# ControlNet（構図制御用、任意）
# models/controlnet/ に配置
```

### 5. 拡張ポイント

**新しい画像役割の追加:**
```python
# design_schemas.py の ImageRole に追加
class ImageRole(str, Enum):
    ...
    PACKAGING_FRONT = "PACKAGING_FRONT"  # パッケージ正面
    PACKAGING_BACK = "PACKAGING_BACK"    # パッケージ背面

# prompt_planner_agent.py の ROLE_TEMPLATES に追加
ROLE_TEMPLATES[ImageRole.PACKAGING_FRONT] = (
    "{subject}, package front view, flat lay, ..."
)
```

**カスタムワークフロー:**
```python
# comfyui_client.py の build_workflow_payload を拡張
# - IP-Adapter ノードの追加
# - ControlNet ノードの追加
# - Inpainting ワークフロー
```

### 6. エラーハンドリング

- 個別画像の生成失敗は他の画像に影響しない（グレースフルデグレード）
- `WorkflowResult.errors` でエラー内容を確認可能
- ComfyUIサーバー未起動時は全画像がエラーになるが、プロンプト計画は確認可能

### 7. テストの実行

```bash
# 全DSEユニットテスト実行
pytest tests/unit/test_design_skills_schemas.py \
       tests/unit/test_comfyui_client.py \
       tests/unit/test_intent_analyzer_agent.py \
       tests/unit/test_prompt_planner_agent.py \
       tests/unit/test_workflow_executor_agent.py \
       tests/unit/test_dse_agent_registry.py \
       tests/unit/test_design_skills_engine.py -v --no-cov

# パターンマッチで実行
pytest -k "design_skills or comfyui or intent_analyzer or prompt_planner or workflow_executor or dse" --no-cov
```

### 8. E2Eテスト (ComfyUI実サーバー)

実際のComfyUIサーバーに対してAPIを通しで検証する。ComfyUI未起動時は自動スキップされる。

#### セットアップ

```bash
# 1. モデルをダウンロード (~6.9GB)
./apps/design_skills_engine/scripts/setup_comfyui.sh

# 2. ComfyUIサーバーを起動
cd apps/design_skills_engine

# GPU モード
COMFYUI_MODELS_DIR=../../models docker compose up -d

# CPU モード (GPU がない場合)
COMFYUI_MODELS_DIR=../../models docker compose \
  -f docker-compose.yml -f docker-compose.cpu.yml up -d

# 3. ヘルスチェックを待機
until curl -sf http://localhost:8188/prompt > /dev/null; do sleep 5; done
```

#### テスト実行

```bash
# E2Eテスト実行 (ComfyUI未起動時は自動スキップ)
pytest tests/e2e/test_design_skills_e2e.py -v --no-cov

# slowマーカーを除外 (バッチ生成テストをスキップ)
pytest tests/e2e/test_design_skills_e2e.py -v --no-cov -m "e2e and not slow"
```

#### テストケース一覧

| クラス | テスト | マーカー | 内容 |
|--------|--------|----------|------|
| TestComfyUIConnection | test_health_check | e2e | サーバー到達確認 |
| TestSingleImageGeneration | test_generate_hero_image | e2e | HERO画像1枚生成 + PNG検証 |
| TestFullPipeline | test_brief_to_images | e2e | IntentAnalyzer -> PromptPlanner -> ComfyUI |
| TestBatchGeneration | test_generate_three_images | e2e, slow | 3役割の画像生成 |
| TestErrorHandling | test_invalid_model_name | e2e | 存在しないモデルのエラー確認 |
| TestErrorHandling | test_timeout_handling | e2e | タイムアウトエラー確認 |

#### 停止

```bash
cd apps/design_skills_engine && docker compose down
```

#### 環境変数

| 変数 | デフォルト | 説明 |
|------|-----------|------|
| `COMFYUI_URL` | `http://localhost:8188` | ComfyUIサーバーURL |
| `COMFYUI_PORT` | `8188` | Docker公開ポート |
| `COMFYUI_MODELS_DIR` | `./models` | モデルディレクトリパス |
| `COMFYUI_IMAGE` | `yanwk/comfyui-boot:cu128-slim` | GPU用ComfyUIイメージ（任意） |
| `COMFYUI_IMAGE_CPU` | `yanwk/comfyui-boot:cpu` | CPU用ComfyUIイメージ（任意） |

---

## DGEパターンとの対応関係

| DecisionGovernanceEngine | DesignSkillsEngine | 説明 |
|--------------------------|-------------------|------|
| CognitiveGate → Gatekeeper | IntentAnalyzer | 入力理解 |
| Dao → Fa → Shu → Qi | PromptPlanner | 計画生成 |
| ReviewAgent | WorkflowExecutor | 実行・検証 |
| DecisionReportBuilder | WorkflowResult | 結果構造化 |
| AgentRegistry | DesignAgentRegistry | Agent管理 |
| agent_definitions.yaml | agent_definitions.yaml | YAML定義 |

---

## 将来のロードマップ

1. **Inpainting対応** - 「この画像のスピーカーだけ赤に変更」
2. **ControlNet統合** - 構図制御（ポーズ、深度、線画）
3. **IP-Adapter** - 参照画像からスタイル転写
4. **バッチ並列生成** - 複数画像の同時生成
5. **Studio UI統合** - リアルタイムプレビュー + AG-UIイベント
6. **A2A対応** - 他のエージェントからの画像生成リクエスト受付
