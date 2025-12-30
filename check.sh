#!/bin/bash
# AgentFlow コード品質チェックスクリプト (Linux/WSL)
#
# 使用方法:
#   ./check.sh [command]
#
# コマンド:
#   help          - ヘルプを表示
#   format        - コードを自動フォーマット
#   lint          - リントチェック
#   type-check    - 型チェック
#   test          - テストを実行
#   test-cov      - カバレッジ付きテスト
#   all           - すべてのチェックを実行
#   pre-commit    - Pre-commit を全ファイルに実行
#   clean         - 一時ファイルを削除

# 関数定義
show_help() {
    echo "========================================"
    echo "AgentFlow - 利用可能なコマンド"
    echo "========================================"
    echo ""
    echo "  ./check.sh format        - コードを自動フォーマット (Python + JS/TS)"
    echo "  ./check.sh lint          - リントチェック (Python + JS/TS)"
    echo "  ./check.sh type-check    - 型チェック (Python + TypeScript)"
    echo "  ./check.sh test          - テストを実行"
    echo "  ./check.sh test-cov      - カバレッジ付きでテストを実行"
    echo ""
    echo "  ./check.sh all           - すべてのチェックを実行"
    echo "  ./check.sh pre-commit    - Pre-commit を全ファイルに実行"
    echo "  ./check.sh clean         - 一時ファイルとキャッシュを削除"
    echo ""
}

do_format() {
    echo "========================================"
    echo "コードを自動フォーマット中..."
    echo "========================================"
    echo ""
    echo "[Python] Ruff フォーマット中..."
    ruff format .
    if [ $? -ne 0 ]; then
        echo "[エラー] Ruff フォーマットに失敗しました"
        return 1
    fi
    ruff check --fix .
    if [ $? -ne 0 ]; then
        echo "[エラー] Ruff リントに失敗しました"
        return 1
    fi
    echo ""
    echo "[JS/TS] Prettier フォーマット中..."
    cd studio
    npx prettier --write "src/**/*.{ts,tsx,js,jsx,json,css}"
    if [ $? -ne 0 ]; then
        echo "[エラー] Prettier フォーマットに失敗しました"
        cd ..
        return 1
    fi
    cd ..
    echo ""
    echo "✅ すべてのコードがフォーマットされました"
    return 0
}

do_lint() {
    echo "========================================"
    echo "リントチェック中..."
    echo "========================================"
    echo ""
    echo "[Python] Ruff リントチェック中..."
    ruff check .
    if [ $? -ne 0 ]; then
        echo "[エラー] Ruff リントチェックに失敗しました"
        return 1
    fi
    echo ""
    echo "[JS/TS] ESLint チェック中..."
    cd studio
    npx eslint "src/**/*.{ts,tsx,js,jsx}" --max-warnings=0
    if [ $? -ne 0 ]; then
        echo "[エラー] ESLint チェックに失敗しました"
        cd ..
        return 1
    fi
    cd ..
    echo ""
    echo "✅ すべてのリントチェックが完了しました"
    return 0
}

do_type_check() {
    echo "========================================"
    echo "型チェック中..."
    echo "========================================"
    echo ""
    echo "[Python] MyPy 型チェック中..."
    mypy agentflow --strict --ignore-missing-imports
    if [ $? -ne 0 ]; then
        echo "[エラー] MyPy 型チェックに失敗しました"
        return 1
    fi
    echo ""
    echo "[TypeScript] tsc 型チェック中..."
    cd studio
    npx tsc --noEmit
    if [ $? -ne 0 ]; then
        echo "[エラー] TypeScript 型チェックに失敗しました"
        cd ..
        return 1
    fi
    cd ..
    echo ""
    echo "✅ すべての型チェックが完了しました"
    return 0
}

do_test() {
    echo "========================================"
    echo "テストを実行中..."
    echo "========================================"
    echo ""
    pytest -v
    if [ $? -ne 0 ]; then
        echo "[エラー] テストに失敗しました"
        return 1
    fi
    echo ""
    echo "✅ すべてのテストが完了しました"
    return 0
}

do_test_cov() {
    echo "========================================"
    echo "カバレッジ付きでテストを実行中..."
    echo "========================================"
    echo ""
    pytest --cov=agentflow --cov-report=html --cov-report=term-missing -v
    if [ $? -ne 0 ]; then
        echo "[エラー] テストに失敗しました"
        return 1
    fi
    echo ""
    echo "📊 カバレッジレポート: htmlcov/index.html"
    echo "✅ すべてのテストが完了しました"
    return 0
}

do_all() {
    echo "========================================"
    echo "すべてのチェックを実行中..."
    echo "========================================"
    echo ""
    do_format
    if [ $? -ne 0 ]; then return 1; fi
    echo ""
    do_lint
    if [ $? -ne 0 ]; then return 1; fi
    echo ""
    do_type_check
    if [ $? -ne 0 ]; then return 1; fi
    echo ""
    do_test
    if [ $? -ne 0 ]; then return 1; fi
    echo ""
    echo "========================================"
    echo "✅ すべてのチェックが完了しました！"
    echo "========================================"
    return 0
}

do_pre_commit() {
    echo "========================================"
    echo "Pre-commit を全ファイルに実行中..."
    echo "========================================"
    echo ""
    pre-commit run --all-files
    if [ $? -ne 0 ]; then
        echo "[警告] Pre-commit で問題が見つかりました"
        echo "自動修正された場合は、変更を確認してください"
    fi
    echo ""
    echo "✅ Pre-commit チェックが完了しました"
    return 0
}

do_clean() {
    echo "========================================"
    echo "一時ファイルとキャッシュを削除中..."
    echo "========================================"
    echo ""
    echo "[Python] キャッシュを削除中..."
    find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null
    find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null
    find . -type d -name ".mypy_cache" -exec rm -rf {} + 2>/dev/null
    find . -type d -name ".ruff_cache" -exec rm -rf {} + 2>/dev/null
    find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null
    rm -rf htmlcov 2>/dev/null
    rm -f .coverage 2>/dev/null
    rm -f coverage.xml 2>/dev/null
    rm -rf dist 2>/dev/null
    rm -rf build 2>/dev/null
    echo ""
    echo "[JS/TS] キャッシュを削除中..."
    rm -rf studio/dist 2>/dev/null
    rm -rf studio/node_modules/.cache 2>/dev/null
    echo ""
    echo "✅ クリーンアップ完了"
    return 0
}

# メイン処理
case "${1:-help}" in
    help)
        show_help
        ;;
    format)
        do_format
        ;;
    lint)
        do_lint
        ;;
    type-check)
        do_type_check
        ;;
    test)
        do_test
        ;;
    test-cov)
        do_test_cov
        ;;
    all)
        do_all
        ;;
    pre-commit)
        do_pre_commit
        ;;
    clean)
        do_clean
        ;;
    *)
        echo "[エラー] 不明なコマンド: $1"
        echo ""
        show_help
        exit 1
        ;;
esac

