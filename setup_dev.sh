#!/bin/bash
# AgentFlow 開発環境セットアップスクリプト (Linux/WSL)
#
# 使用方法:
#   bash setup_dev.sh または ./setup_dev.sh

echo "========================================"
echo "AgentFlow 開発環境セットアップ"
echo "========================================"
echo ""

# Conda 環境がアクティブか確認
if [ -z "$CONDA_DEFAULT_ENV" ]; then
    echo "[エラー] Conda 環境がアクティブではありません"
    echo ""
    echo "以下のコマンドを実行してください:"
    echo "  conda activate agentflow"
    echo ""
    echo "または、Conda 環境を作成してください:"
    echo "  conda env create -f environment.yml"
    echo "  conda activate agentflow"
    echo ""
    read -p "Press Enter to exit..."
    exit 1
fi

echo "[1/4] 開発依存関係をインストール中..."
pip install -e ".[dev]"
if [ $? -ne 0 ]; then
    echo "[エラー] 開発依存関係のインストールに失敗しました"
    read -p "Press Enter to exit..."
    exit 1
fi
echo ""

echo "[2/4] Pre-commit フックをインストール中..."
pip install pre-commit
if [ $? -ne 0 ]; then
    echo "[エラー] Pre-commit のインストールに失敗しました"
    read -p "Press Enter to exit..."
    exit 1
fi

pre-commit install
if [ $? -ne 0 ]; then
    echo "[エラー] Pre-commit フックのインストールに失敗しました"
    read -p "Press Enter to exit..."
    exit 1
fi
echo ""

echo "[3/4] フロントエンド依存関係をインストール中..."
cd studio
npm install
if [ $? -ne 0 ]; then
    echo "[エラー] フロントエンド依存関係のインストールに失敗しました"
    cd ..
    read -p "Press Enter to exit..."
    exit 1
fi
cd ..
echo ""

echo "[4/4] インストール確認中..."
agentflow --version
if [ $? -ne 0 ]; then
    echo "[警告] AgentFlow CLI が正しくインストールされていない可能性があります"
fi
echo ""

echo "========================================"
echo "セットアップ完了！"
echo "========================================"
echo ""
echo "次のステップ:"
echo "  1. コードをフォーマット: ./check.sh format"
echo "  2. リントチェック: ./check.sh lint"
echo "  3. 型チェック: ./check.sh type-check"
echo "  4. テスト実行: ./check.sh test"
echo "  5. すべてのチェック: ./check.sh all"
echo ""
read -p "Press Enter to exit..."

