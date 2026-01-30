#!/bin/bash
# AgentFlow 開発環境セットアップスクリプト
# 使用方法: bash setup_dev.sh または ./setup_dev.sh
#
# 依存関係は pyproject.toml で一元管理しています

set -e  # エラー時に即座に終了

echo "========================================"
echo "AgentFlow 開発環境セットアップ"
echo "========================================"
echo ""

# ステップ数を環境状況に応じて調整
TOTAL_STEPS=5

# 1. Conda 環境のチェック/作成
echo "[1/$TOTAL_STEPS] Conda 環境を確認中..."

if [ "$CONDA_DEFAULT_ENV" = "agentflow" ]; then
    echo "✓ Conda 環境 'agentflow' はアクティブです"
elif conda env list | grep -q "^agentflow "; then
    echo "Conda 環境 'agentflow' が存在します。アクティベートしてください:"
    echo "  conda activate agentflow"
    echo "  bash setup_dev.sh"
    exit 1
else
    echo "Conda 環境 'agentflow' を作成中..."
    conda create -n agentflow python=3.13 -y
    echo ""
    echo "✓ Conda 環境作成完了"
    echo ""
    echo "次のコマンドを実行してください:"
    echo "  conda activate agentflow"
    echo "  bash setup_dev.sh"
    exit 0
fi
echo ""

# 2. Python 依存関係をインストール
echo "[2/$TOTAL_STEPS] Python 依存関係をインストール中..."
pip install -e ".[dev]"
echo "✓ Python 依存関係インストール完了"
echo ""

# 3. Pre-commit フックをインストール
echo "[3/$TOTAL_STEPS] Pre-commit フックをインストール中..."
pip install pre-commit
pre-commit install
echo "✓ Pre-commit フックインストール完了"
echo ""

# 4. フロントエンド依存関係をインストール
echo "[4/$TOTAL_STEPS] フロントエンド依存関係をインストール中..."
if [ -d "studio" ]; then
    cd studio
    npm install
    cd ..
    echo "✓ フロントエンド依存関係インストール完了"
else
    echo "⚠ studio/ ディレクトリが見つかりません（スキップ）"
fi
echo ""

# 5. インストール確認
echo "[5/$TOTAL_STEPS] インストール確認中..."
agentflow --version
echo "✓ インストール確認完了"
echo ""

echo "========================================"
echo "セットアップ完了！"
echo "========================================"
echo ""
echo "使用方法:"
echo "  conda activate agentflow    # 環境アクティベート"
echo "  agentflow --help            # CLI ヘルプ"
echo ""
echo "開発コマンド:"
echo "  make check-all              # すべてのチェック"
echo "  make test                   # テスト実行"
echo "  make dev-backend            # バックエンド起動"
echo "  make dev-frontend           # フロントエンド起動"
echo ""
