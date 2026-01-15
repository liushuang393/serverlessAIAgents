@echo off
REM AgentFlow 開発環境セットアップスクリプト (Windows)
REM
REM 使用方法:
REM   setup_dev.bat

echo ========================================
echo AgentFlow 開発環境セットアップ
echo ========================================
echo.

REM Conda 環境がアクティブか確認
if "%CONDA_DEFAULT_ENV%"=="" (
    echo [エラー] Conda 環境がアクティブではありません
    echo.
    echo 以下のコマンドを実行してください:
    echo   conda activate agentflow
    echo.
    echo または、Conda 環境を作成してください:
    echo   conda create -n agentflow python=3.13 -y
    echo   conda activate agentflow
    echo.
    pause
    exit /b 1
)

echo [1/4] 開発依存関係をインストール中...
pip install -e ".[dev]"
if errorlevel 1 (
    echo [エラー] 開発依存関係のインストールに失敗しました
    pause
    exit /b 1
)
echo.

echo [2/4] Pre-commit フックをインストール中...
pip install pre-commit
if errorlevel 1 (
    echo [エラー] Pre-commit のインストールに失敗しました
    pause
    exit /b 1
)

pre-commit install
if errorlevel 1 (
    echo [エラー] Pre-commit フックのインストールに失敗しました
    pause
    exit /b 1
)
echo.

echo [3/4] フロントエンド依存関係をインストール中...
cd studio
call npm install
if errorlevel 1 (
    echo [エラー] フロントエンド依存関係のインストールに失敗しました
    cd ..
    pause
    exit /b 1
)
cd ..
echo.

echo [4/4] インストール確認中...
agentflow --version
if errorlevel 1 (
    echo [警告] AgentFlow CLI が正しくインストールされていない可能性があります
)
echo.

echo ========================================
echo セットアップ完了！
echo ========================================
echo.
echo 次のステップ:
echo   1. コードをフォーマット: check.bat format
echo   2. リントチェック: check.bat lint
echo   3. 型チェック: check.bat type-check
echo   4. テスト実行: check.bat test
echo   5. すべてのチェック: check.bat all
echo.
pause
