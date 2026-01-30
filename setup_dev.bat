@echo off
REM AgentFlow 開発環境セットアップスクリプト (Windows)
REM 使用方法: Anaconda Prompt で setup_dev.bat を実行
REM
REM 依存関係は pyproject.toml で一元管理しています

echo ========================================
echo AgentFlow 開発環境セットアップ
echo ========================================
echo.

REM 1. Conda 環境のチェック/作成
echo [1/5] Conda 環境を確認中...

if "%CONDA_DEFAULT_ENV%"=="agentflow" (
    echo ✓ Conda 環境 'agentflow' はアクティブです
    goto :install_deps
)

REM 環境が存在するかチェック
conda env list | findstr /B "agentflow " >nul 2>&1
if %errorlevel%==0 (
    echo Conda 環境 'agentflow' が存在します。アクティベートしてください:
    echo   conda activate agentflow
    echo   setup_dev.bat
    pause
    exit /b 1
)

REM 環境を新規作成
echo Conda 環境 'agentflow' を作成中...
call conda create -n agentflow python=3.13 -y
if errorlevel 1 (
    echo エラー: Conda 環境の作成に失敗しました
    pause
    exit /b 1
)
echo.
echo ✓ Conda 環境作成完了
echo.
echo 次のコマンドを実行してください:
echo   conda activate agentflow
echo   setup_dev.bat
pause
exit /b 0

:install_deps
echo.

REM 2. Python 依存関係をインストール
echo [2/5] Python 依存関係をインストール中...
pip install -e ".[dev]"
if errorlevel 1 (
    echo エラー: Python 依存関係のインストールに失敗しました
    pause
    exit /b 1
)
echo ✓ Python 依存関係インストール完了
echo.

REM 3. Pre-commit フックをインストール
echo [3/5] Pre-commit フックをインストール中...
pip install pre-commit
if errorlevel 1 (
    echo エラー: Pre-commit のインストールに失敗しました
    pause
    exit /b 1
)
pre-commit install
if errorlevel 1 (
    echo エラー: Pre-commit フックのインストールに失敗しました
    pause
    exit /b 1
)
echo ✓ Pre-commit フックインストール完了
echo.

REM 4. フロントエンド依存関係をインストール
echo [4/5] フロントエンド依存関係をインストール中...
if exist "studio" (
    cd studio
    call npm install
    if errorlevel 1 (
        echo エラー: フロントエンド依存関係のインストールに失敗しました
        cd ..
        pause
        exit /b 1
    )
    cd ..
    echo ✓ フロントエンド依存関係インストール完了
) else (
    echo ⚠ studio/ ディレクトリが見つかりません（スキップ）
)
echo.

REM 5. インストール確認
echo [5/5] インストール確認中...
agentflow --version
if errorlevel 1 (
    echo 警告: AgentFlow CLI が正しくインストールされていない可能性があります
)
echo ✓ インストール確認完了
echo.

echo ========================================
echo セットアップ完了！
echo ========================================
echo.
echo 使用方法:
echo   conda activate agentflow    # 環境アクティベート
echo   agentflow --help            # CLI ヘルプ
echo.
echo 開発コマンド:
echo   check.bat all               # すべてのチェック
echo   check.bat test              # テスト実行
echo.
pause
