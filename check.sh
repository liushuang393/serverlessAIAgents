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
#   build         - フロントエンド(studio)ビルド確認
#   audit         - npm 脆弱性チェック (studio)
#   all           - すべてのチェックを実行（format/lint/type-check/test/build）
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
    echo "  ./check.sh build         - フロントエンド(studio)ビルド確認"
    echo "  ./check.sh audit         - npm 脆弱性チェック (studio)"
    echo ""
    echo "  ./check.sh all           - すべてのチェックを実行 (format/lint/type-check/test/build)"
    echo "  ./check.sh all --no-type-check  - 型チェックをスキップして実行"
    echo "  ./check.sh pre-commit    - Pre-commit を全ファイルに実行"
    echo "  ./check.sh clean         - 一時ファイルとキャッシュを削除"
    echo ""
}

do_format() {
    echo "========================================"
    echo "コードを自動フォーマット中..."
    echo "========================================"
    echo ""
    echo "[Python] 不要な type: ignore コメントを削除中..."
    python scripts/fix_mypy_safe.py
    if [ $? -ne 0 ]; then
        echo "[警告] fix_mypy_safe.py に問題があります（続行します）"
    fi
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

do_build() {
    echo "========================================"
    echo "フロントエンド(studio)ビルド確認中..."
    echo "========================================"
    echo ""
    if [ ! -d "studio" ]; then
        echo "[スキップ] studio ディレクトリがありません"
        return 0
    fi
    cd studio
    npm run build
    if [ $? -ne 0 ]; then
        echo "[エラー] studio ビルドに失敗しました"
        cd ..
        return 1
    fi
    cd ..
    echo ""
    echo "✅ studio ビルドが完了しました"
    return 0
}

do_audit() {
    echo "========================================"
    echo "npm 脆弱性チェック中 (studio)..."
    echo "========================================"
    echo ""
    if [ ! -d "studio" ]; then
        echo "[スキップ] studio ディレクトリがありません"
        return 0
    fi
    cd studio
    npm audit --audit-level=high
    local ret=$?
    cd ..
    if [ $ret -ne 0 ]; then
        echo "[警告] 高以上の脆弱性が検出されました。対応するか code-rules の方針に従って記録してください"
    fi
    echo ""
    echo "✅ npm audit が完了しました"
    return 0
}

do_all() {
    local skip_type_check=0
    if [ "${1:-}" = "--no-type-check" ]; then
        skip_type_check=1
        echo "[オプション] 型チェックをスキップします"
        echo ""
    fi
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
    if [ $skip_type_check -eq 0 ]; then
        do_type_check
        if [ $? -ne 0 ]; then return 1; fi
        echo ""
    fi
    do_test
    if [ $? -ne 0 ]; then return 1; fi
    echo ""
    do_build
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
    build)
        do_build
        ;;
    audit)
        do_audit
        ;;
    all)
        do_all "$2"
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

