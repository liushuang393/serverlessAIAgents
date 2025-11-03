@echo off
REM AgentFlow Conda 環境セットアップスクリプト
REM 使用方法: Anaconda Prompt で setup_conda.bat を実行

echo ========================================
echo AgentFlow Conda 環境セットアップ
echo ========================================
echo.

REM Conda 環境を作成
echo [1/4] Conda 環境を作成中...
call conda env create -f environment.yml -y
if %errorlevel% neq 0 (
    echo エラー: Conda 環境の作成に失敗しました
    exit /b 1
)
echo ✓ Conda 環境作成完了
echo.

REM 環境をアクティベート
echo [2/4] 環境をアクティベート中...
call conda activate agentflow
if %errorlevel% neq 0 (
    echo エラー: 環境のアクティベートに失敗しました
    exit /b 1
)
echo ✓ 環境アクティベート完了
echo.

REM AgentFlow を開発モードでインストール
echo [3/4] AgentFlow を開発モードでインストール中...
pip install -e ".[dev]"
if %errorlevel% neq 0 (
    echo エラー: AgentFlow のインストールに失敗しました
    exit /b 1
)
echo ✓ AgentFlow インストール完了
echo.

REM インストール確認
echo [4/4] インストール確認中...
agentflow --version
if %errorlevel% neq 0 (
    echo エラー: AgentFlow の確認に失敗しました
    exit /b 1
)
echo ✓ インストール確認完了
echo.

echo ========================================
echo セットアップ完了！
echo ========================================
echo.
echo 次のコマンドで環境をアクティベートできます:
echo   conda activate agentflow
echo.
echo AgentFlow を使用開始:
echo   agentflow --help
echo.

