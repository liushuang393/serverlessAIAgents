#!/usr/bin/env bash
# BizCore コード品質チェックスクリプト (Linux/WSL)
#
# 利用例:
#   ./check.sh format
#   ./check.sh all
#   sh check.sh all     # sh 経由でも内部で bash に切り替え

if [ -z "${BASH_VERSION:-}" ]; then
    exec bash "$0" "$@"
fi

set -o pipefail

# カラー定義
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$REPO_ROOT" || exit 1

declare -a JS_PROJECTS=()
CONDA_ENV_NAME="${CHECK_CONDA_ENV:-agentflow}"
PYTHON_CHECK_TARGETS="${CHECK_PY_TARGETS:-contracts infrastructure shared kernel harness control_plane domain apps}"
CHECK_INCLUDE_TESTS="${CHECK_INCLUDE_TESTS:-0}"
MYPY_EXCLUDE_REGEX_DEFAULT='(^|/)(tests|test)(/|$)|(^|/)test_.*\.py$|(^|/).*_test\.py$'
MYPY_EXCLUDE_REGEX="${MYPY_EXCLUDE_REGEX:-$MYPY_EXCLUDE_REGEX_DEFAULT}"
BANDIT_EXCLUDE_PATHS="${BANDIT_EXCLUDE_PATHS:-tests/*,*/tests/*}"

declare -a RUFF_EXCLUDE_ARGS=()
if [ "$CHECK_INCLUDE_TESTS" != "1" ]; then
    RUFF_EXCLUDE_ARGS=(
        --exclude "tests/**"
        --exclude "**/tests/**"
        --exclude "**/test_*.py"
        --exclude "**/*_test.py"
    )
fi

command_exists() {
    command -v "$1" >/dev/null 2>&1
}

can_use_conda_env() {
    command_exists conda && conda env list 2>/dev/null | awk '{print $1}' | grep -Fxq "$CONDA_ENV_NAME"
}

run_py_tool() {
    local tool="$1"
    shift

    # デフォルトでは conda env を優先利用。未利用にしたい場合は CHECK_USE_CONDA=0
    if [ "${CHECK_USE_CONDA:-1}" = "1" ] && can_use_conda_env; then
        # すでに対象 env 上なら PATH 優先で直接実行（conda run ハング回避）
        if [ "${CONDA_DEFAULT_ENV:-}" = "$CONDA_ENV_NAME" ] && command_exists "$tool"; then
            "$tool" "$@"
            return $?
        fi

        # env バイナリを直接実行（conda run より安定・高速）
        local conda_base conda_tool
        conda_base="$(conda info --base 2>/dev/null)"
        conda_tool="${conda_base}/envs/${CONDA_ENV_NAME}/bin/${tool}"
        if [ -x "$conda_tool" ]; then
            "$conda_tool" "$@"
            return $?
        fi

        # 最後のフォールバック
        conda run -n "$CONDA_ENV_NAME" "$tool" "$@"
        return $?
    fi

    if command_exists "$tool"; then
        "$tool" "$@"
        return $?
    fi

    echo "[エラー] ${tool} が見つかりません (CHECK_USE_CONDA=${CHECK_USE_CONDA:-1}, env=${CONDA_ENV_NAME})"
    return 127
}

classify_mypy_output() {
    local output_file="$1"
    echo "[分類][MyPy] エラーコード上位:"
    grep -oE '\[[a-z0-9-]+\]' "$output_file" | tr -d '[]' | sort | uniq -c | sort -nr | head -n 10 || true
    echo "[分類][MyPy] ファイル上位:"
    awk -F: '/: error: / {c[$1]++} END {for (f in c) printf "%5d %s\n", c[f], f}' "$output_file" | sort -nr | head -n 10 || true
    echo "[分析][MyPy] 自動修復可否:"
    echo "  - 半自動で対応可能: import-untyped（types-* stub 追加）"
    echo "  - 自動修復困難: call-arg / arg-type / assignment / union-attr / override / no-any-return"
    echo "    -> これらは型定義・実装整合性の修正が必要"
}

classify_eslint_output() {
    local output_file="$1"
    echo "[分類][ESLint] ルール上位:"
    awk '/ error / {rule=$NF; c[rule]++} END {for (r in c) printf "%5d %s\n", c[r], r}' "$output_file" | sort -nr | head -n 10 || true
    echo "[分類][ESLint] ファイル上位:"
    awk '/^\/.*\.(ts|tsx|js|jsx)$/ {file=$0} / error / {if (file != "") c[file]++} END {for (f in c) printf "%5d %s\n", c[f], f}' "$output_file" | sort -nr | head -n 10 || true
    echo "[分析][ESLint] 自動修復可否:"
    echo "  - 自動修復可能: no-unused-vars（一部）, ban-ts-comment（一部）, stylistic rules"
    echo "  - 自動修復困難: no-explicit-any（型設計が必要）"
}

classify_tsc_output() {
    local output_file="$1"
    echo "[分類][TypeScript] エラーコード上位:"
    grep -oE 'TS[0-9]+' "$output_file" | sort | uniq -c | sort -nr | head -n 10 || true
    echo "[分析][TypeScript] 自動修復可否:"
    echo "  - 自動修復困難: TS型不整合（実装か型宣言の修正が必要）"
}

classify_pytest_output() {
    local output_file="$1"
    if grep -qE 'collected [0-9]+ items / [0-9]+ deselected / 0 selected' "$output_file"; then
        echo "[分類][pytest] 0件実行（全テスト除外）"
    fi
    echo "[分類][pytest] 失敗テスト上位:"
    grep -E '^FAILED ' "$output_file" | head -n 20 || true
    echo "[分析][pytest] 自動修復可否:"
    echo "  - 自動修復困難: テスト失敗は実装修正かfixture調整が必要"
}

classify_bandit_output() {
    local output_file="$1"
    echo "[分類][bandit] 重大度件数:"
    grep -Eo 'Severity: (LOW|MEDIUM|HIGH)' "$output_file" | awk '{c[$2]++} END {for (k in c) printf "%5d %s\n", c[k], k}' | sort -nr || true
    echo "[分類][bandit] 代表 issue:"
    grep -E '^>> Issue:' "$output_file" | head -n 10 || true
    echo "[分析][bandit] 自動修復可否:"
    echo "  - 一部自動修復可能: subprocess/check, weak hash などパターン置換"
    echo "  - 多くは要手動: セキュリティ要件確認が必要"
}

show_help() {
    echo "========================================"
    echo "BizCore - 利用可能なコマンド"
    echo "========================================"
    echo ""
    echo "  ./check.sh format        - コードを自動フォーマット (Python + JS/TS)"
    echo "  ./check.sh lint          - リントチェック (Python + JS/TS)"
    echo "  ./check.sh type-check    - 型チェック (Python + TypeScript)"
    echo "  ./check.sh test          - テストを実行"
    echo "  ./check.sh e2e-smoke     - LLM プロバイダー疎通スモーク (opt-in)"
    echo "  ./check.sh test-cov      - カバレッジ付きでテストを実行"
    echo "  ./check.sh security      - セキュリティチェック (bandit SAST)"
    echo "  ./check.sh report        - 全チェックを実行してAI向けレポートを生成"
    echo ""
    echo "  ./check.sh all           - format -> lint -> type-check -> security -> test"
    echo "  ./check.sh pre-commit    - Pre-commit を全ファイルに実行"
    echo "  ./check.sh clean         - 一時ファイルとキャッシュを削除"
    echo ""
    echo "環境変数:"
    echo "  MYPY_TARGETS             - type-check 時の mypy 対象 (デフォルト: CHECK_PY_TARGETS と同じ)"
    echo "  CHECK_PY_TARGETS         - Python 静的チェック対象 (デフォルト: contracts infrastructure shared kernel harness control_plane domain apps)"
    echo "  CHECK_INCLUDE_TESTS      - all/report でテスト実行するか (1=実行, 0=スキップ, デフォルト: 0)"
    echo "  MYPY_EXCLUDE_REGEX       - mypy 除外パス正規表現 (デフォルト: tests ディレクトリ + test ファイル)"
    echo "  BANDIT_EXCLUDE_PATHS     - bandit 除外パス (デフォルト: tests/*,*/tests/*)"
    echo "  JS_PROJECT_DIRS          - JS/TS 対象ディレクトリを空白区切りで指定"
    echo "  CHECK_CONDA_ENV          - Python ツール実行に使う conda env 名 (デフォルト: agentflow)"
    echo "  CHECK_USE_CONDA          - 1: conda優先, 0: PATH優先 (デフォルト: 1)"
    echo "  CHECK_PYTEST_FLAGS       - unit test 時の追加 pytest 引数 (デフォルト: --tb=short -ra)"
    echo "  CHECK_PYTEST_E2E_FLAGS   - e2e-smoke 時の追加 pytest 引数 (デフォルト: --tb=short -ra)"
    echo "  CHECK_E2E_PROVIDER_SMOKE - 1: all 実行時に e2e-smoke も実行 (デフォルト: 0)"
    echo "  RUN_PROVIDER_E2E         - 1: e2e-smoke で実際のプロバイダー呼び出しを有効化"
    echo ""
}

discover_js_projects() {
    JS_PROJECTS=()

    if ! command_exists npm; then
        return 0
    fi

    if [ -n "${JS_PROJECT_DIRS:-}" ]; then
        # 例: JS_PROJECT_DIRS="control_plane/frontend apps/faq_system/frontend"
        local dir
        for dir in $JS_PROJECT_DIRS; do
            if [ -f "$dir/package.json" ]; then
                JS_PROJECTS+=("$dir")
            fi
        done
        return 0
    fi

    while IFS= read -r package_file; do
        JS_PROJECTS+=("${package_file%/package.json}")
    done < <(
        find . -type f -name "package.json" \
            -not -path "*/node_modules/*" \
            -not -path "*/dist/*" \
            | sort
    )
}

project_label() {
    local dir="$1"
    local label="${dir#./}"
    if [ -z "$label" ]; then
        label="."
    fi
    echo "$label"
}

package_has_script() {
    local dir="$1"
    local script_name="$2"
    grep -Eq "\"${script_name}\"[[:space:]]*:" "$dir/package.json"
}

has_local_bin() {
    local dir="$1"
    local bin_name="$2"
    [ -x "$dir/node_modules/.bin/$bin_name" ]
}

run_npm_script_if_exists() {
    local dir="$1"
    local script_name="$2"
    local stage="$3"
    local label
    label="$(project_label "$dir")"

    if ! package_has_script "$dir" "$script_name"; then
        return 2
    fi

    if [ ! -d "$dir/node_modules" ]; then
        echo "[スキップ][$stage] $label (node_modules が見つかりません)"
        return 0
    fi

    local output_file
    output_file="$(mktemp)"

    echo "[$stage] $label -> npm run $script_name"
    (
        cd "$dir" || exit 1
        npm run -s "$script_name"
    ) 2>&1 | tee "$output_file"
    local rc=${PIPESTATUS[0]}

    if [ $rc -ne 0 ]; then
        echo "[エラー][$stage] $label で失敗しました"
        if [[ "$stage" == *"lint"* ]]; then
            classify_eslint_output "$output_file"
        elif [[ "$stage" == *"TypeScript"* ]]; then
            classify_tsc_output "$output_file"
        fi
        rm -f "$output_file"
        return 1
    fi
    rm -f "$output_file"
    return 0
}

run_js_lint() {
    if ! command_exists npm; then
        echo "[スキップ] npm が見つかりません。JS/TS lint をスキップします。"
        return 0
    fi

    discover_js_projects
    local checked=0
    local failed=0
    local dir

    for dir in "${JS_PROJECTS[@]}"; do
        if ! package_has_script "$dir" "lint"; then
            continue
        fi
        if [ ! -d "$dir/node_modules" ]; then
            echo "[スキップ][JS/TS lint] $(project_label "$dir") (node_modules が見つかりません)"
            checked=1
            continue
        fi
        if ! has_local_bin "$dir" "eslint"; then
            echo "[スキップ][JS/TS lint] $(project_label "$dir") (eslint が見つかりません)"
            checked=1
            continue
        fi

        if run_npm_script_if_exists "$dir" "lint" "JS/TS lint"; then
            checked=1
        else
            local rc=$?
            if [ $rc -eq 1 ]; then
                failed=1
                checked=1
            fi
        fi
    done

    if [ $checked -eq 0 ]; then
        echo "[スキップ] lint スクリプトを持つ JS/TS プロジェクトが見つかりません。"
    fi

    return $failed
}

run_js_type_check() {
    if ! command_exists npm; then
        echo "[スキップ] npm が見つかりません。TypeScript チェックをスキップします。"
        return 0
    fi

    discover_js_projects
    local checked=0
    local failed=0
    local dir

    for dir in "${JS_PROJECTS[@]}"; do
        local label
        label="$(project_label "$dir")"

        if package_has_script "$dir" "type-check"; then
            if [ ! -d "$dir/node_modules" ]; then
                echo "[スキップ][TypeScript] $label (node_modules が見つかりません)"
                checked=1
                continue
            fi
            if ! has_local_bin "$dir" "tsc"; then
                echo "[スキップ][TypeScript] $label (tsc が見つかりません)"
                checked=1
                continue
            fi

            if run_npm_script_if_exists "$dir" "type-check" "TypeScript"; then
                checked=1
                continue
            fi

            local rc=$?
            if [ $rc -eq 1 ]; then
                failed=1
                checked=1
                continue
            fi
        fi

        # type-check スクリプトがない場合は tsconfig + tsc があれば fallback 実行
        if [ -f "$dir/tsconfig.json" ] && has_local_bin "$dir" "tsc"; then
            echo "[TypeScript] $label -> npx tsc --noEmit (fallback)"
            (
                cd "$dir" || exit 1
                npx tsc --noEmit
            )
            if [ $? -ne 0 ]; then
                echo "[エラー][TypeScript] $label で失敗しました"
                failed=1
            fi
            checked=1
        fi
    done

    if [ $checked -eq 0 ]; then
        echo "[スキップ] TypeScript 対象プロジェクトが見つかりません。"
    fi

    return $failed
}

run_js_format() {
    if ! command_exists npm; then
        echo "[スキップ] npm が見つかりません。JS/TS format をスキップします。"
        return 0
    fi

    discover_js_projects
    local checked=0
    local failed=0
    local dir

    for dir in "${JS_PROJECTS[@]}"; do
        if run_npm_script_if_exists "$dir" "format" "JS/TS format"; then
            checked=1
            continue
        fi

        local rc=$?
        if [ $rc -eq 1 ]; then
            failed=1
            checked=1
            continue
        fi

        # format スクリプトがない場合、lint --fix を fallback として試行（失敗しても継続）
        if package_has_script "$dir" "lint" && [ -d "$dir/node_modules" ] && has_local_bin "$dir" "eslint"; then
            local label
            label="$(project_label "$dir")"
            echo "[JS/TS format] $label -> npm run lint -- --fix (fallback)"
            (
                cd "$dir" || exit 1
                npm run -s lint -- --fix
            ) || echo "[警告][JS/TS format] $label fallback 自動修正で未解決エラーがあります"
            checked=1
        fi
    done

    if [ $checked -eq 0 ]; then
        echo "[スキップ] format/lint スクリプトを持つ JS/TS プロジェクトが見つかりません。"
    fi

    return $failed
}

do_format() {
    echo "========================================"
    echo "コードを自動フォーマット中..."
    echo "========================================"
    echo ""

    local -a py_targets=()
    # shellcheck disable=SC2206
    py_targets=($PYTHON_CHECK_TARGETS)

    echo "[Python] Ruff フォーマット中..."
    run_py_tool ruff format "${RUFF_EXCLUDE_ARGS[@]}" "${py_targets[@]}"
    if [ $? -ne 0 ]; then
        echo "[エラー] Ruff フォーマットに失敗しました"
        return 1
    fi

    echo "[Python] Ruff 自動修正中 (safe fixes)..."
    run_py_tool ruff check --fix "${RUFF_EXCLUDE_ARGS[@]}" "${py_targets[@]}" || echo "[警告] safe fixes 後に未修正エラーが残っています (lint で確認されます)"

    echo "[Python] Ruff 自動修正中 (unsafe fixes)..."
    run_py_tool ruff check --unsafe-fixes --fix "${RUFF_EXCLUDE_ARGS[@]}" "${py_targets[@]}" || true

    echo ""
    run_js_format
    if [ $? -ne 0 ]; then
        return 1
    fi

    echo ""
    echo "✅ フォーマット処理が完了しました"
    return 0
}

do_lint() {
    echo "========================================"
    echo "リントチェック中..."
    echo "========================================"
    echo ""

    local -a py_targets=()
    # shellcheck disable=SC2206
    py_targets=($PYTHON_CHECK_TARGETS)

    echo "[Python] Ruff リントチェック中..."
    local ruff_output
    ruff_output="$(mktemp)"
    run_py_tool ruff check "${RUFF_EXCLUDE_ARGS[@]}" "${py_targets[@]}" 2>&1 | tee "$ruff_output"
    local ruff_rc=${PIPESTATUS[0]}
    if [ $ruff_rc -ne 0 ]; then
        echo "[エラー] Ruff リントチェックに失敗しました"
        echo "[分類][Ruff] ルール上位:"
        grep -Eo '[A-Z]{1,4}[0-9]{3,4}' "$ruff_output" | sort | uniq -c | sort -nr | head -n 10 || true
        rm -f "$ruff_output"
        return 1
    fi
    rm -f "$ruff_output"

    echo ""
    echo "[Python] Layer boundary チェック中..."
    run_py_tool python scripts/check_layer_boundaries.py
    if [ $? -ne 0 ]; then
        echo "[エラー] Layer boundary チェックに失敗しました"
        return 1
    fi

    echo ""
    echo "[Python] Provider 直呼びチェック中..."
    run_py_tool python scripts/check_no_direct_provider_calls.py
    if [ $? -ne 0 ]; then
        echo "[エラー] Provider 直呼びチェックに失敗しました"
        return 1
    fi

    # App コンプライアンスチェック
    printf "${BLUE}[App Compliance]${NC} Checking app framework usage...\\n"
    run_py_tool python scripts/check_app_compliance.py
    local app_rc=$?
    if [ $app_rc -ne 0 ]; then
        printf "${YELLOW}[App Compliance] Some apps have compliance issues (non-blocking)${NC}\\n"
    fi

    echo ""
    echo "--- Encoding Check ---"
    run_py_tool python scripts/check_encoding.py
    if [ $? -ne 0 ]; then
        echo "[エラー] エンコーディングチェックに失敗しました"
        return 1
    fi

    echo ""
    run_js_lint
    if [ $? -ne 0 ]; then
        return 1
    fi

    echo ""
    echo "✅ すべてのリントチェックが完了しました"
    return 0
}

do_type_check() {
    echo "========================================"
    echo "型チェック中..."
    echo "========================================"
    echo ""

    local mypy_targets="${MYPY_TARGETS:-$PYTHON_CHECK_TARGETS}"
    local -a mypy_target_args=()
    # shellcheck disable=SC2206
    mypy_target_args=($mypy_targets)
    local -a mypy_exclude_args=()
    if [ "$CHECK_INCLUDE_TESTS" != "1" ]; then
        mypy_exclude_args=(--exclude "$MYPY_EXCLUDE_REGEX")
    fi
    echo "[Python] MyPy 型チェック中... (targets: $mypy_targets)"
    local mypy_output
    mypy_output="$(mktemp)"
    run_py_tool mypy "${mypy_target_args[@]}" "${mypy_exclude_args[@]}" --ignore-missing-imports 2>&1 | tee "$mypy_output"
    local mypy_rc=${PIPESTATUS[0]}
    if [ $mypy_rc -ne 0 ]; then
        echo "[エラー] MyPy 型チェックに失敗しました"
        classify_mypy_output "$mypy_output"
        rm -f "$mypy_output"
        return 1
    fi
    rm -f "$mypy_output"

    echo ""
    run_js_type_check
    if [ $? -ne 0 ]; then
        return 1
    fi

    echo ""
    echo "✅ すべての型チェックが完了しました"
    return 0
}

do_test() {
    echo "========================================"
    echo "テストを実行中 (unit tests)..."
    echo "========================================"
    echo ""

    # --no-cov でカバレッジ閾値をバイパス（unit tests のみ実行）
    local test_output
    local pytest_flags="${CHECK_PYTEST_FLAGS:---tb=short -ra}"
    test_output="$(mktemp)"
    # shellcheck disable=SC2086
    LLM_PROVIDER=mock run_py_tool pytest tests/unit/ --no-cov $pytest_flags 2>&1 | tee "$test_output"
    local test_rc=${PIPESTATUS[0]}
    if [ $test_rc -ne 0 ]; then
        echo "[エラー] テストに失敗しました"
        classify_pytest_output "$test_output"
        rm -f "$test_output"
        return 1
    fi
    rm -f "$test_output"

    echo ""
    echo "✅ すべてのテストが完了しました"
    return 0
}

do_e2e_smoke() {
    echo "========================================"
    echo "E2E スモークチェック中 (LLM providers)..."
    echo "========================================"
    echo ""

    if [ "${RUN_PROVIDER_E2E:-0}" != "1" ]; then
        echo "[スキップ] RUN_PROVIDER_E2E=1 で実行されます"
        return 0
    fi

    local test_output
    local pytest_e2e_flags="${CHECK_PYTEST_E2E_FLAGS:---tb=short -ra}"
    test_output="$(mktemp)"
    # shellcheck disable=SC2086
    run_py_tool pytest tests/e2e/test_llm_provider_smoke.py --no-cov $pytest_e2e_flags 2>&1 | tee "$test_output"
    local test_rc=${PIPESTATUS[0]}
    if [ $test_rc -ne 0 ]; then
        echo "[エラー] E2E スモークテストに失敗しました"
        classify_pytest_output "$test_output"
        rm -f "$test_output"
        return 1
    fi
    rm -f "$test_output"

    echo ""
    echo "✅ E2E スモークチェックが完了しました"
    return 0
}

do_test_cov() {
    echo "========================================"
    echo "カバレッジ付きでテストを実行中..."
    echo "========================================"
    echo ""

    local test_output
    test_output="$(mktemp)"
    run_py_tool pytest \
        --cov=contracts \
        --cov=infrastructure \
        --cov=shared \
        --cov=kernel \
        --cov=harness \
        --cov=control_plane \
        --cov=domain \
        --cov=apps \
        --cov-report=html \
        --cov-report=term-missing \
        -v 2>&1 | tee "$test_output"
    local test_rc=${PIPESTATUS[0]}
    if [ $test_rc -ne 0 ]; then
        echo "[エラー] テストに失敗しました"
        classify_pytest_output "$test_output"
        rm -f "$test_output"
        return 1
    fi
    rm -f "$test_output"

    echo ""
    echo "📊 カバレッジレポート: htmlcov/index.html"
    echo "✅ すべてのテストが完了しました"
    return 0
}

do_security() {
    echo "========================================"
    echo "セキュリティチェック中 (bandit)..."
    echo "========================================"
    echo ""

    echo "[Python] bandit SAST チェック中..."
    local bandit_output
    bandit_output="$(mktemp)"
    local -a bandit_exclude_args=()
    if [ "$CHECK_INCLUDE_TESTS" != "1" ]; then
        bandit_exclude_args=(-x "$BANDIT_EXCLUDE_PATHS")
    fi
    run_py_tool bandit -r contracts infrastructure shared kernel harness control_plane domain apps "${bandit_exclude_args[@]}" -lll -iii --format txt 2>&1 | tee "$bandit_output"
    local bandit_exit=${PIPESTATUS[0]}

    echo ""
    if [ $bandit_exit -ne 0 ]; then
        echo "[エラー] bandit セキュリティチェックに失敗しました (HIGH severity)"
        classify_bandit_output "$bandit_output"
        rm -f "$bandit_output"
        return 1
    fi
    rm -f "$bandit_output"

    echo "✅ セキュリティチェックが完了しました"
    return 0
}

do_report() {
    echo "========================================"
    echo "コード品質レポートを生成中..."
    echo "========================================"

    local report_file="check-report.md"
    local timestamp
    timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    local mypy_targets="${MYPY_TARGETS:-$PYTHON_CHECK_TARGETS}"
    local -a py_targets=()
    # shellcheck disable=SC2206
    py_targets=($PYTHON_CHECK_TARGETS)
    local -a mypy_target_args=()
    # shellcheck disable=SC2206
    mypy_target_args=($mypy_targets)
    local -a mypy_exclude_args=()
    if [ "$CHECK_INCLUDE_TESTS" != "1" ]; then
        mypy_exclude_args=(--exclude "$MYPY_EXCLUDE_REGEX")
    fi
    local -a bandit_exclude_args=()
    if [ "$CHECK_INCLUDE_TESTS" != "1" ]; then
        bandit_exclude_args=(-x "$BANDIT_EXCLUDE_PATHS")
    fi

    local lint_status="✅" type_status="✅" test_status="✅" security_status="✅"
    local lint_output type_output security_output test_output

    lint_output=$(run_py_tool ruff check "${RUFF_EXCLUDE_ARGS[@]}" "${py_targets[@]}" 2>&1)
    [ $? -ne 0 ] && lint_status="❌"

    type_output=$(run_py_tool mypy "${mypy_target_args[@]}" "${mypy_exclude_args[@]}" --ignore-missing-imports 2>&1)
    [ $? -ne 0 ] && type_status="❌"

    security_output=$(run_py_tool bandit -r contracts infrastructure shared kernel harness control_plane domain apps "${bandit_exclude_args[@]}" -lll -iii --format txt 2>&1)
    [ $? -ne 0 ] && security_status="❌"

    if [ "$CHECK_INCLUDE_TESTS" = "1" ]; then
        test_output=$(run_py_tool pytest -v --tb=short 2>&1)
        [ $? -ne 0 ] && test_status="❌"
    else
        test_output="skipped (CHECK_INCLUDE_TESTS=0)"
        test_status="⏭️"
    fi

    # ルールコンプライアンスレポート
    printf "${BLUE}[Rules Compliance]${NC} Generating compliance report...\\n"
    run_py_tool python scripts/check_rules_compliance.py

    cat >"$report_file" <<EOF
# BizCore Code Quality Report
Generated: $timestamp

## Summary
- $lint_status lint (Ruff)
- $type_status type-check (MyPy)
- $security_status security (bandit HIGH)
- $test_status test (pytest)

## lint エラー (Ruff)
\`\`\`
$lint_output
\`\`\`

## type-check エラー (MyPy)
\`\`\`
$type_output
\`\`\`

## security エラー (bandit)
\`\`\`
$security_output
\`\`\`

## test 結果 (pytest)
\`\`\`
$test_output
\`\`\`

## 参照: CLAUDE.md ルール
- 型安全性・async-first・safe_enum: \`code-rules/CLAUDE.md\`
EOF

    echo ""
    echo "📋 レポートを生成しました: $report_file"
    echo "   AIアシスタントに貼り付けて修正を依頼できます"
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

    local -a failed_steps=()

    if ! do_format; then
        failed_steps+=("format")
    fi
    echo ""

    if ! do_lint; then
        failed_steps+=("lint")
    fi
    echo ""

    if [ $skip_type_check -eq 0 ]; then
        if ! do_type_check; then
            failed_steps+=("type-check")
        fi
    else
        echo "[スキップ] --no-type-check 指定のため type-check ステップをスキップします"
    fi
    echo ""

    if ! do_security; then
        failed_steps+=("security")
    fi
    echo ""

    if [ "$CHECK_INCLUDE_TESTS" = "1" ]; then
        if ! do_test; then
            failed_steps+=("test")
        fi
    else
        echo "[スキップ] CHECK_INCLUDE_TESTS=0 のため test ステップをスキップします"
    fi
    echo ""

    if [ "${CHECK_E2E_PROVIDER_SMOKE:-0}" = "1" ]; then
        if ! do_e2e_smoke; then
            failed_steps+=("e2e-smoke")
        fi
        echo ""
    fi

    echo "========================================"
    if [ ${#failed_steps[@]} -eq 0 ]; then
        echo "✅ すべてのチェックが完了しました！"
        echo "========================================"
        return 0
    fi

    echo "❌ 一部チェックで失敗しました"
    echo "失敗ステップ: ${failed_steps[*]}"
    echo "========================================"
    return 1
}

do_pre_commit() {
    echo "========================================"
    echo "Pre-commit を全ファイルに実行中..."
    echo "========================================"
    echo ""

    run_py_tool pre-commit run --all-files
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
    rm -rf control_plane/frontend/dist 2>/dev/null
    rm -rf control_plane/frontend/node_modules/.cache 2>/dev/null
    echo ""
    echo "✅ クリーンアップ完了"
    return 0
}

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
e2e-smoke)
    do_e2e_smoke
    ;;
security)
    do_security
    ;;
report)
    do_report
    ;;
all)
    do_all "${2:-}"
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
