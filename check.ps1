# BizCore コード品質チェックスクリプト (PowerShell)
#
# 使用方法:
#   .\check.ps1 [command]
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

param(
    [Parameter(Position=0)]
    [string]$Command = "help"
)

function Show-Help {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "BizCore - 利用可能なコマンド" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  .\check.ps1 format        - コードを自動フォーマット (Python + JS/TS)" -ForegroundColor Green
    Write-Host "  .\check.ps1 lint          - リントチェック (Python + JS/TS)" -ForegroundColor Green
    Write-Host "  .\check.ps1 type-check    - 型チェック (Python + TypeScript)" -ForegroundColor Green
    Write-Host "  .\check.ps1 test          - テストを実行" -ForegroundColor Green
    Write-Host "  .\check.ps1 test-cov      - カバレッジ付きでテストを実行" -ForegroundColor Green
    Write-Host ""
    Write-Host "  .\check.ps1 all           - すべてのチェックを実行" -ForegroundColor Yellow
    Write-Host "  .\check.ps1 pre-commit    - Pre-commit を全ファイルに実行" -ForegroundColor Yellow
    Write-Host "  .\check.ps1 clean         - 一時ファイルとキャッシュを削除" -ForegroundColor Yellow
    Write-Host ""
}

function Format-Code {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "コードを自動フォーマット中..." -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    Write-Host "[Python] Ruff フォーマット中..." -ForegroundColor Yellow
    ruff format .
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] Ruff フォーマットに失敗しました" -ForegroundColor Red
        exit 1
    }

    ruff check --fix .
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] Ruff リントに失敗しました" -ForegroundColor Red
        exit 1
    }
    Write-Host ""

    Write-Host "[JS/TS] ESLint auto-fix 実行中..." -ForegroundColor Yellow
    Push-Location control_plane/frontend
    npm run lint -- --fix
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] JS/TS 自動修正に失敗しました" -ForegroundColor Red
        Pop-Location
        exit 1
    }
    Pop-Location
    Write-Host ""

    Write-Host "✅ すべてのコードがフォーマットされました" -ForegroundColor Green
}

function Run-Lint {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "リントチェック中..." -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    Write-Host "[Python] Ruff リントチェック中..." -ForegroundColor Yellow
    ruff check .
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] Ruff リントチェックに失敗しました" -ForegroundColor Red
        exit 1
    }
    Write-Host ""

    Write-Host "[JS/TS] ESLint チェック中..." -ForegroundColor Yellow
    Push-Location control_plane/frontend
    npm run lint
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] ESLint チェックに失敗しました" -ForegroundColor Red
        Pop-Location
        exit 1
    }
    Pop-Location
    Write-Host ""

    Write-Host "✅ すべてのリントチェックが完了しました" -ForegroundColor Green
}

function Run-TypeCheck {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "型チェック中..." -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    Write-Host "[Python] MyPy 型チェック中..." -ForegroundColor Yellow
    mypy contracts infrastructure shared kernel harness control_plane domain apps tests --strict --ignore-missing-imports
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] MyPy 型チェックに失敗しました" -ForegroundColor Red
        exit 1
    }
    Write-Host ""

    Write-Host "[TypeScript] tsc 型チェック中..." -ForegroundColor Yellow
    Push-Location control_plane/frontend
    npm run type-check
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] TypeScript 型チェックに失敗しました" -ForegroundColor Red
        Pop-Location
        exit 1
    }
    Pop-Location
    Write-Host ""

    Write-Host "✅ すべての型チェックが完了しました" -ForegroundColor Green
}

function Run-Test {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "テストを実行中..." -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    pytest -v
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] テストに失敗しました" -ForegroundColor Red
        exit 1
    }
    Write-Host ""

    Write-Host "✅ すべてのテストが完了しました" -ForegroundColor Green
}

function Run-TestCov {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "カバレッジ付きでテストを実行中..." -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    pytest --cov=contracts --cov=infrastructure --cov=shared --cov=kernel --cov=harness --cov=control_plane --cov=domain --cov=apps --cov-report=html --cov-report=term-missing -v
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[エラー] テストに失敗しました" -ForegroundColor Red
        exit 1
    }
    Write-Host ""

    Write-Host "📊 カバレッジレポート: htmlcov\index.html" -ForegroundColor Cyan
    Write-Host "✅ すべてのテストが完了しました" -ForegroundColor Green
}

function Run-All {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "すべてのチェックを実行中..." -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    Format-Code
    if ($LASTEXITCODE -ne 0) { exit 1 }
    Write-Host ""

    Run-Lint
    if ($LASTEXITCODE -ne 0) { exit 1 }
    Write-Host ""

    Run-TypeCheck
    if ($LASTEXITCODE -ne 0) { exit 1 }
    Write-Host ""

    Run-Test
    if ($LASTEXITCODE -ne 0) { exit 1 }
    Write-Host ""

    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "✅ すべてのチェックが完了しました！" -ForegroundColor Green
    Write-Host "========================================" -ForegroundColor Cyan
}

function Run-PreCommit {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "Pre-commit を全ファイルに実行中..." -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    pre-commit run --all-files
    if ($LASTEXITCODE -ne 0) {
        Write-Host "[警告] Pre-commit で問題が見つかりました" -ForegroundColor Yellow
        Write-Host "自動修正された場合は、変更を確認してください" -ForegroundColor Yellow
    }
    Write-Host ""

    Write-Host "✅ Pre-commit チェックが完了しました" -ForegroundColor Green
}

function Clean-Files {
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "一時ファイルとキャッシュを削除中..." -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""

    Write-Host "[Python] キャッシュを削除中..." -ForegroundColor Yellow
    Get-ChildItem -Path . -Recurse -Directory -Filter "__pycache__" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
    Get-ChildItem -Path . -Recurse -Directory -Filter ".pytest_cache" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
    Get-ChildItem -Path . -Recurse -Directory -Filter ".mypy_cache" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
    Get-ChildItem -Path . -Recurse -Directory -Filter ".ruff_cache" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue
    Get-ChildItem -Path . -Recurse -Directory -Filter "*.egg-info" | Remove-Item -Recurse -Force -ErrorAction SilentlyContinue

    if (Test-Path "htmlcov") { Remove-Item -Path "htmlcov" -Recurse -Force }
    if (Test-Path ".coverage") { Remove-Item -Path ".coverage" -Force }
    if (Test-Path "coverage.xml") { Remove-Item -Path "coverage.xml" -Force }
    if (Test-Path "dist") { Remove-Item -Path "dist" -Recurse -Force }
    if (Test-Path "build") { Remove-Item -Path "build" -Recurse -Force }
    Write-Host ""

    Write-Host "[JS/TS] キャッシュを削除中..." -ForegroundColor Yellow
    if (Test-Path "control_plane\frontend\dist") { Remove-Item -Path "control_plane\frontend\dist" -Recurse -Force }
    if (Test-Path "control_plane\frontend\node_modules\.cache") { Remove-Item -Path "control_plane\frontend\node_modules\.cache" -Recurse -Force }
    Write-Host ""

    Write-Host "✅ クリーンアップ完了" -ForegroundColor Green
}

# メインロジック
switch ($Command.ToLower()) {
    "help" { Show-Help }
    "format" { Format-Code }
    "lint" { Run-Lint }
    "type-check" { Run-TypeCheck }
    "test" { Run-Test }
    "test-cov" { Run-TestCov }
    "all" { Run-All }
    "pre-commit" { Run-PreCommit }
    "clean" { Clean-Files }
    default {
        Write-Host "[エラー] 不明なコマンド: $Command" -ForegroundColor Red
        Write-Host ""
        Show-Help
        exit 1
    }
}
