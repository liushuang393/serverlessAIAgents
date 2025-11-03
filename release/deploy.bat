@echo off
setlocal enabledelayedexpansion

REM =============================================================================
REM AI-Blocks 本番リリーススクリプト (Windows)
REM =============================================================================

REM 設定変数
set PROJECT_NAME=ai-blocks
set DOCKER_IMAGE_NAME=aiblocks/ai-blocks
set DOCKER_COMPOSE_FILE=docker-compose.prod.yml

REM カラー設定（Windows 10以降）
set RED=[91m
set GREEN=[92m
set YELLOW=[93m
set BLUE=[94m
set NC=[0m

REM ログ関数
:log_info
echo %BLUE%[INFO]%NC% %~1
goto :eof

:log_success
echo %GREEN%[SUCCESS]%NC% %~1
goto :eof

:log_warning
echo %YELLOW%[WARNING]%NC% %~1
goto :eof

:log_error
echo %RED%[ERROR]%NC% %~1
goto :eof

REM ヘルプ表示
:show_help
echo AI-Blocks 本番リリーススクリプト
echo.
echo 使用方法:
echo     %~nx0 [オプション] [コマンド]
echo.
echo コマンド:
echo     build           パッケージをビルド
echo     test            全テストを実行
echo     docker-build    Dockerイメージをビルド
echo     docker-deploy   Docker本番デプロイ
echo     pypi-upload     PyPIにアップロード
echo     full-release    完全リリース（全工程実行）
echo     health-check    ヘルスチェック実行
echo     rollback        前バージョンにロールバック
echo.
echo オプション:
echo     /h, /help       このヘルプを表示
echo     /v VERSION      新しいバージョンを指定
echo     /test-only      テスト環境のみ
echo     /skip-tests     テストをスキップ
echo     /force          強制実行
echo.
echo 例:
echo     %~nx0 build
echo     %~nx0 /v 0.2.0 full-release
echo     %~nx0 docker-deploy
goto :eof

REM 現在のバージョン取得
:get_current_version
for /f "tokens=3 delims= " %%a in ('findstr /r "^version" pyproject.toml') do (
    set CURRENT_VERSION=%%a
    set CURRENT_VERSION=!CURRENT_VERSION:"=!
)
goto :eof

REM 前提条件チェック
:check_prerequisites
call :log_info "前提条件をチェック中..."

REM 必要なコマンドの存在確認
python --version >nul 2>&1
if errorlevel 1 (
    call :log_error "Python が見つかりません。インストールしてください。"
    exit /b 1
)

pip --version >nul 2>&1
if errorlevel 1 (
    call :log_error "pip が見つかりません。インストールしてください。"
    exit /b 1
)

docker --version >nul 2>&1
if errorlevel 1 (
    call :log_error "Docker が見つかりません。インストールしてください。"
    exit /b 1
)

docker-compose --version >nul 2>&1
if errorlevel 1 (
    call :log_error "docker-compose が見つかりません。インストールしてください。"
    exit /b 1
)

git --version >nul 2>&1
if errorlevel 1 (
    call :log_error "Git が見つかりません。インストールしてください。"
    exit /b 1
)

REM Pythonバージョン表示
for /f "tokens=2" %%a in ('python --version') do (
    call :log_info "Python バージョン: %%a"
)

REM Gitの状態チェック
git status --porcelain >nul 2>&1
if not errorlevel 1 (
    for /f %%a in ('git status --porcelain') do (
        call :log_warning "未コミットの変更があります。"
        if not "%FORCE%"=="true" (
            set /p REPLY="続行しますか？ (y/N): "
            if /i not "!REPLY!"=="y" exit /b 1
        )
        goto :check_complete
    )
)

:check_complete
call :log_success "前提条件チェック完了"
goto :eof

REM バージョン更新
:update_version
set NEW_VERSION=%~1
call :log_info "バージョンを %CURRENT_VERSION% から %NEW_VERSION% に更新中..."

REM pyproject.tomlのバージョン更新
powershell -Command "(Get-Content pyproject.toml) -replace 'version = \"%CURRENT_VERSION%\"', 'version = \"%NEW_VERSION%\"' | Set-Content pyproject.toml"

REM __init__.pyのバージョン更新（存在する場合）
if exist "ai_blocks\__init__.py" (
    powershell -Command "(Get-Content ai_blocks\__init__.py) -replace '__version__ = \"%CURRENT_VERSION%\"', '__version__ = \"%NEW_VERSION%\"' | Set-Content ai_blocks\__init__.py"
)

call :log_success "バージョン更新完了: %NEW_VERSION%"
set CURRENT_VERSION=%NEW_VERSION%
goto :eof

REM テスト実行
:run_tests
if "%SKIP_TESTS%"=="true" (
    call :log_warning "テストをスキップしました"
    goto :eof
)

call :log_info "テストを実行中..."

REM 開発依存関係のインストール
pip install -e ".[dev]" >nul 2>&1

REM 単体テスト
call :log_info "単体テストを実行中..."
pytest tests/unit/ -v --tb=short
if errorlevel 1 (
    call :log_error "単体テストに失敗しました"
    exit /b 1
)

REM 統合テスト
call :log_info "統合テストを実行中..."
pytest tests/integration/ -v --tb=short
if errorlevel 1 (
    call :log_error "統合テストに失敗しました"
    exit /b 1
)

REM 型チェック
call :log_info "型チェックを実行中..."
mypy ai_blocks/
if errorlevel 1 (
    call :log_error "型チェックに失敗しました"
    exit /b 1
)

REM コードフォーマットチェック
call :log_info "コードフォーマットをチェック中..."
black --check ai_blocks/ tests/
if errorlevel 1 (
    call :log_error "コードフォーマットチェックに失敗しました"
    exit /b 1
)

isort --check-only ai_blocks/ tests/
if errorlevel 1 (
    call :log_error "importソートチェックに失敗しました"
    exit /b 1
)

call :log_success "全テスト完了"
goto :eof

REM パッケージビルド
:build_package
call :log_info "パッケージをビルド中..."

REM クリーンアップ
make clean

REM ビルド
python -m build
if errorlevel 1 (
    call :log_error "パッケージビルドに失敗しました"
    exit /b 1
)

REM ビルド結果確認
if exist "dist" (
    call :log_success "パッケージビルド完了"
    dir dist
) else (
    call :log_error "パッケージビルドに失敗しました"
    exit /b 1
)
goto :eof

REM Dockerイメージビルド
:build_docker_image
call :log_info "Dockerイメージをビルド中..."

set IMAGE_TAG=%DOCKER_IMAGE_NAME%:%CURRENT_VERSION%
set LATEST_TAG=%DOCKER_IMAGE_NAME%:latest

REM Dockerイメージビルド
docker build -t "%IMAGE_TAG%" -t "%LATEST_TAG%" .
if errorlevel 1 (
    call :log_error "Dockerイメージビルドに失敗しました"
    exit /b 1
)

call :log_success "Dockerイメージビルド完了: %IMAGE_TAG%"
goto :eof

REM Docker本番デプロイ
:deploy_docker
call :log_info "Docker本番デプロイを開始中..."

REM 環境変数ファイルの存在確認
if not exist ".env.production" (
    call :log_error ".env.production ファイルが見つかりません"
    exit /b 1
)

REM 既存コンテナの停止
docker-compose -f "%DOCKER_COMPOSE_FILE%" ps -q >nul 2>&1
if not errorlevel 1 (
    call :log_info "既存コンテナを停止中..."
    docker-compose -f "%DOCKER_COMPOSE_FILE%" down
)

REM 新しいコンテナの起動
call :log_info "新しいコンテナを起動中..."
docker-compose -f "%DOCKER_COMPOSE_FILE%" up -d
if errorlevel 1 (
    call :log_error "Docker本番デプロイに失敗しました"
    exit /b 1
)

REM 起動確認
timeout /t 10 /nobreak >nul
docker-compose -f "%DOCKER_COMPOSE_FILE%" ps | findstr "Up" >nul
if errorlevel 1 (
    call :log_error "Docker本番デプロイに失敗しました"
    docker-compose -f "%DOCKER_COMPOSE_FILE%" logs
    exit /b 1
)

call :log_success "Docker本番デプロイ完了"
goto :eof

REM PyPIアップロード
:upload_to_pypi
set REPOSITORY=%~1
if "%REPOSITORY%"=="" set REPOSITORY=pypi

call :log_info "PyPI (%REPOSITORY%) にアップロード中..."

REM twineの存在確認
twine --version >nul 2>&1
if errorlevel 1 (
    call :log_info "twineをインストール中..."
    pip install twine
)

REM アップロード
if "%REPOSITORY%"=="testpypi" (
    twine upload --repository testpypi dist/*
) else (
    twine upload dist/*
)

if errorlevel 1 (
    call :log_error "PyPI (%REPOSITORY%) アップロードに失敗しました"
    exit /b 1
)

call :log_success "PyPI (%REPOSITORY%) アップロード完了"
goto :eof

REM ヘルスチェック
:health_check
call :log_info "ヘルスチェックを実行中..."

set HEALTH_URL=http://localhost:8000/health
set MAX_ATTEMPTS=30
set ATTEMPT=1

:health_loop
curl -s "%HEALTH_URL%" >nul 2>&1
if not errorlevel 1 (
    for /f "delims=" %%a in ('curl -s "%HEALTH_URL%"') do (
        call :log_success "ヘルスチェック成功: %%a"
        goto :eof
    )
)

call :log_info "ヘルスチェック試行 %ATTEMPT%/%MAX_ATTEMPTS%..."
timeout /t 2 /nobreak >nul
set /a ATTEMPT+=1
if %ATTEMPT% leq %MAX_ATTEMPTS% goto :health_loop

call :log_error "ヘルスチェックに失敗しました"
exit /b 1

REM 完全リリース
:full_release
call :log_info "完全リリースを開始します..."

call :check_prerequisites
if errorlevel 1 exit /b 1

call :run_tests
if errorlevel 1 exit /b 1

call :build_package
if errorlevel 1 exit /b 1

if not "%TEST_ONLY%"=="true" (
    call :build_docker_image
    if errorlevel 1 exit /b 1

    call :upload_to_pypi
    if errorlevel 1 exit /b 1

    call :deploy_docker
    if errorlevel 1 exit /b 1

    call :health_check
    if errorlevel 1 exit /b 1

    REM Gitタグ作成
    set TAG_NAME=v%CURRENT_VERSION%
    git tag "!TAG_NAME!"
    git push origin "!TAG_NAME!"

    call :log_success "完全リリース完了: !TAG_NAME!"
) else (
    call :log_success "テスト環境リリース完了"
)
goto :eof

REM メイン処理
:main
call :get_current_version

set COMMAND=
set NEW_VERSION=
set TEST_ONLY=
set SKIP_TESTS=
set FORCE=

REM 引数解析
:parse_args
if "%~1"=="" goto :execute_command
if "%~1"=="/h" goto :show_help_and_exit
if "%~1"=="/help" goto :show_help_and_exit
if "%~1"=="/v" (
    set NEW_VERSION=%~2
    shift
    shift
    goto :parse_args
)
if "%~1"=="/test-only" (
    set TEST_ONLY=true
    shift
    goto :parse_args
)
if "%~1"=="/skip-tests" (
    set SKIP_TESTS=true
    shift
    goto :parse_args
)
if "%~1"=="/force" (
    set FORCE=true
    shift
    goto :parse_args
)
if "%~1"=="build" set COMMAND=build
if "%~1"=="test" set COMMAND=test
if "%~1"=="docker-build" set COMMAND=docker-build
if "%~1"=="docker-deploy" set COMMAND=docker-deploy
if "%~1"=="pypi-upload" set COMMAND=pypi-upload
if "%~1"=="full-release" set COMMAND=full-release
if "%~1"=="health-check" set COMMAND=health-check
if "%~1"=="rollback" set COMMAND=rollback
if not "%COMMAND%"=="" (
    shift
    goto :parse_args
)

call :log_error "不明なオプション: %~1"
goto :show_help_and_exit

:show_help_and_exit
call :show_help
exit /b 0

:execute_command
REM バージョン更新
if not "%NEW_VERSION%"=="" (
    call :update_version "%NEW_VERSION%"
)

REM コマンド実行
if "%COMMAND%"=="build" (
    call :check_prerequisites
    call :build_package
) else if "%COMMAND%"=="test" (
    call :run_tests
) else if "%COMMAND%"=="docker-build" (
    call :check_prerequisites
    call :build_docker_image
) else if "%COMMAND%"=="docker-deploy" (
    call :check_prerequisites
    call :deploy_docker
) else if "%COMMAND%"=="pypi-upload" (
    call :check_prerequisites
    call :build_package
    call :upload_to_pypi
) else if "%COMMAND%"=="full-release" (
    call :full_release
) else if "%COMMAND%"=="health-check" (
    call :health_check
) else if "%COMMAND%"=="" (
    call :log_error "コマンドが指定されていません"
    call :show_help
    exit /b 1
) else (
    call :log_error "不明なコマンド: %COMMAND%"
    call :show_help
    exit /b 1
)

goto :eof

REM スクリプト実行
call :main %*
