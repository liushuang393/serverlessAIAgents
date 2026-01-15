#!/bin/bash
# AgentFlow Conda 環境セットアップスクリプト
# 使用方法: bash setup_conda.sh または ./setup_conda.sh
#
# 依存関係は pyproject.toml で一元管理しています

set -e  # エラー時に即座に終了

echo "========================================"
echo "AgentFlow Conda 環境セットアップ"
echo "========================================"
echo ""

# Conda 環境を作成（Python 3.13）
echo "[1/4] Conda 環境を作成中..."
conda create -n agentflow python=3.13 -y
if [ $? -ne 0 ]; then
    echo "エラー: Conda 環境の作成に失敗しました"
    exit 1
fi
echo "✓ Conda 環境作成完了"
echo ""

# 環境をアクティベート
echo "[2/4] 環境をアクティベート中..."
# Condaのアクティベーションを現在のシェルで実行
eval "$(conda shell.bash hook)"
conda activate agentflow
if [ $? -ne 0 ]; then
    echo "エラー: 環境のアクティベートに失敗しました"
    exit 1
fi
echo "✓ 環境アクティベート完了"
echo ""

# AgentFlow を開発モードでインストール（pyproject.toml から依存関係を取得）
echo "[3/4] AgentFlow を開発モードでインストール中..."
pip install -e ".[dev]"
if [ $? -ne 0 ]; then
    echo "エラー: AgentFlow のインストールに失敗しました"
    exit 1
fi
echo "✓ AgentFlow インストール完了"
echo ""

# インストール確認
echo "[4/4] インストール確認中..."
agentflow --version
if [ $? -ne 0 ]; then
    echo "エラー: AgentFlow の確認に失敗しました"
    exit 1
fi
echo "✓ インストール確認完了"
echo ""

echo "========================================"
echo "セットアップ完了！"
echo "========================================"
echo ""
echo "次のコマンドで環境をアクティベートできます:"
echo "  conda activate agentflow"
echo ""
echo "AgentFlow を使用開始:"
echo "  agentflow --help"
echo ""

