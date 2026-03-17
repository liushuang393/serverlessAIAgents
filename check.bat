@echo off
REM BizCore コード品質チェックスクリプト (Windows)
REM
REM 使用方法:
REM   check.bat [command]
REM
REM コマンド:
REM   help          - ヘルプを表示
REM   format        - コードを自動フォーマット
REM   lint          - リントチェック
REM   type-check    - 型チェック
REM   test          - テストを実行
REM   test-cov      - カバレッジ付きテスト
REM   all           - すべてのチェックを実行
REM   pre-commit    - Pre-commit を全ファイルに実行
REM   clean         - 一時ファイルを削除

setlocal enabledelayedexpansion

if "%1"=="" goto :help
if "%1"=="help" goto :help
if "%1"=="format" goto :format
if "%1"=="lint" goto :lint
if "%1"=="type-check" goto :type_check
if "%1"=="test" goto :test
if "%1"=="test-cov" goto :test_cov
if "%1"=="all" goto :all
if "%1"=="pre-commit" goto :pre_commit
if "%1"=="clean" goto :clean

echo [エラー] 不明なコマンド: %1
echo.
goto :help

:help
echo ========================================
echo BizCore - 利用可能なコマンド
echo ========================================
echo.
echo   check.bat format        - コードを自動フォーマット (Python + JS/TS)
echo   check.bat lint          - リントチェック (Python + JS/TS)
echo   check.bat type-check    - 型チェック (Python + TypeScript)
echo   check.bat test          - テストを実行
echo   check.bat test-cov      - カバレッジ付きでテストを実行
echo.
echo   check.bat all           - すべてのチェックを実行
echo   check.bat pre-commit    - Pre-commit を全ファイルに実行
echo   check.bat clean         - 一時ファイルとキャッシュを削除
echo.
goto :eof

:format
echo ========================================
echo コードを自動フォーマット中...
echo ========================================
echo.
echo [Python] Ruff フォーマット中...
ruff format .
if errorlevel 1 (
    echo [エラー] Ruff フォーマットに失敗しました
    exit /b 1
)
ruff check --fix .
if errorlevel 1 (
    echo [エラー] Ruff リントに失敗しました
    exit /b 1
)
echo.
echo [JS/TS] ESLint auto-fix 実行中...
cd control_plane\frontend
call npm run lint -- --fix
if errorlevel 1 (
    echo [エラー] JS/TS 自動修正に失敗しました
    cd ..\..
    exit /b 1
)
cd ..\..
echo.
echo ✅ すべてのコードがフォーマットされました
goto :eof

:lint
echo ========================================
echo リントチェック中...
echo ========================================
echo.
echo [Python] Ruff リントチェック中...
ruff check .
if errorlevel 1 (
    echo [エラー] Ruff リントチェックに失敗しました
    exit /b 1
)
echo.
echo [JS/TS] ESLint チェック中...
cd control_plane\frontend
call npm run lint
if errorlevel 1 (
    echo [エラー] ESLint チェックに失敗しました
    cd ..\..
    exit /b 1
)
cd ..\..
echo.
echo ✅ すべてのリントチェックが完了しました
goto :eof

:type_check
echo ========================================
echo 型チェック中...
echo ========================================
echo.
echo [Python] MyPy 型チェック中...
mypy contracts infrastructure shared kernel harness control_plane domain apps tests --strict --ignore-missing-imports
if errorlevel 1 (
    echo [エラー] MyPy 型チェックに失敗しました
    exit /b 1
)
echo.
echo [TypeScript] tsc 型チェック中...
cd control_plane\frontend
call npm run type-check
if errorlevel 1 (
    echo [エラー] TypeScript 型チェックに失敗しました
    cd ..\..
    exit /b 1
)
cd ..\..
echo.
echo ✅ すべての型チェックが完了しました
goto :eof

:test
echo ========================================
echo テストを実行中...
echo ========================================
echo.
pytest -v
if errorlevel 1 (
    echo [エラー] テストに失敗しました
    exit /b 1
)
echo.
echo ✅ すべてのテストが完了しました
goto :eof

:test_cov
echo ========================================
echo カバレッジ付きでテストを実行中...
echo ========================================
echo.
pytest --cov=contracts --cov=infrastructure --cov=shared --cov=kernel --cov=harness --cov=control_plane --cov=domain --cov=apps --cov-report=html --cov-report=term-missing -v
if errorlevel 1 (
    echo [エラー] テストに失敗しました
    exit /b 1
)
echo.
echo 📊 カバレッジレポート: htmlcov\index.html
echo ✅ すべてのテストが完了しました
goto :eof

:all
echo ========================================
echo すべてのチェックを実行中...
echo ========================================
echo.
call :format
if errorlevel 1 exit /b 1
echo.
call :lint
if errorlevel 1 exit /b 1
echo.
call :type_check
if errorlevel 1 exit /b 1
echo.
call :test
if errorlevel 1 exit /b 1
echo.
echo ========================================
echo ✅ すべてのチェックが完了しました！
echo ========================================
goto :eof

:pre_commit
echo ========================================
echo Pre-commit を全ファイルに実行中...
echo ========================================
echo.
pre-commit run --all-files
if errorlevel 1 (
    echo [警告] Pre-commit で問題が見つかりました
    echo 自動修正された場合は、変更を確認してください
)
echo.
echo ✅ Pre-commit チェックが完了しました
goto :eof

:clean
echo ========================================
echo 一時ファイルとキャッシュを削除中...
echo ========================================
echo.
echo [Python] キャッシュを削除中...
for /d /r . %%d in (__pycache__) do @if exist "%%d" rd /s /q "%%d"
for /d /r . %%d in (.pytest_cache) do @if exist "%%d" rd /s /q "%%d"
for /d /r . %%d in (.mypy_cache) do @if exist "%%d" rd /s /q "%%d"
for /d /r . %%d in (.ruff_cache) do @if exist "%%d" rd /s /q "%%d"
for /d /r . %%d in (*.egg-info) do @if exist "%%d" rd /s /q "%%d"
if exist htmlcov rd /s /q htmlcov
if exist .coverage del /q .coverage
if exist coverage.xml del /q coverage.xml
if exist dist rd /s /q dist
if exist build rd /s /q build
echo.
echo [JS/TS] キャッシュを削除中...
if exist control_plane\frontend\dist rd /s /q control_plane\frontend\dist
if exist control_plane\frontend\node_modules\.cache rd /s /q control_plane\frontend\node_modules\.cache
echo.
echo ✅ クリーンアップ完了
goto :eof

endlocal
