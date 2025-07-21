#!/bin/bash

# =============================================================================
# AI-Blocks 本番リリーススクリプト (Linux/macOS)
# =============================================================================

set -euo pipefail

# カラー定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ログ関数
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# 設定変数
PROJECT_NAME="ai-blocks"
CURRENT_VERSION=$(grep -E '^version = ' pyproject.toml | sed 's/version = "\(.*\)"/\1/')
DOCKER_IMAGE_NAME="aiblocks/ai-blocks"
DOCKER_COMPOSE_FILE="docker-compose.prod.yml"

# ヘルプ表示
show_help() {
    cat << EOF
AI-Blocks 本番リリーススクリプト

使用方法:
    $0 [オプション] [コマンド]

コマンド:
    build           パッケージをビルド
    test            全テストを実行
    docker-build    Dockerイメージをビルド
    docker-deploy   Docker本番デプロイ
    pypi-upload     PyPIにアップロード
    full-release    完全リリース（全工程実行）
    health-check    ヘルスチェック実行
    rollback        前バージョンにロールバック

オプション:
    -h, --help      このヘルプを表示
    -v, --version   新しいバージョンを指定
    -t, --test-only テスト環境のみ
    --skip-tests    テストをスキップ
    --force         強制実行

例:
    $0 build
    $0 -v 0.2.0 full-release
    $0 docker-deploy
EOF
}

# 前提条件チェック
check_prerequisites() {
    log_info "前提条件をチェック中..."

    # 必要なコマンドの存在確認
    local required_commands=("python" "pip" "docker" "docker-compose" "git")
    for cmd in "${required_commands[@]}"; do
        if ! command -v "$cmd" &> /dev/null; then
            log_error "$cmd が見つかりません。インストールしてください。"
            exit 1
        fi
    done

    # Pythonバージョンチェック
    local python_version=$(python --version | cut -d' ' -f2)
    log_info "Python バージョン: $python_version"

    # Gitの状態チェック
    if [[ -n $(git status --porcelain) ]]; then
        log_warning "未コミットの変更があります。"
        if [[ "${FORCE:-false}" != "true" ]]; then
            read -p "続行しますか？ (y/N): " -n 1 -r
            echo
            if [[ ! $REPLY =~ ^[Yy]$ ]]; then
                exit 1
            fi
        fi
    fi

    log_success "前提条件チェック完了"
}

# バージョン更新
update_version() {
    local new_version="$1"
    log_info "バージョンを $CURRENT_VERSION から $new_version に更新中..."

    # pyproject.tomlのバージョン更新
    sed -i.bak "s/version = \"$CURRENT_VERSION\"/version = \"$new_version\"/" pyproject.toml

    # __init__.pyのバージョン更新（存在する場合）
    if [[ -f "ai_blocks/__init__.py" ]]; then
        sed -i.bak "s/__version__ = \"$CURRENT_VERSION\"/__version__ = \"$new_version\"/" ai_blocks/__init__.py
    fi

    log_success "バージョン更新完了: $new_version"
    CURRENT_VERSION="$new_version"
}

# テスト実行
run_tests() {
    if [[ "${SKIP_TESTS:-false}" == "true" ]]; then
        log_warning "テストをスキップしました"
        return 0
    fi

    log_info "テストを実行中..."

    # 開発依存関係のインストール
    pip install -e ".[dev]" > /dev/null 2>&1

    # 単体テスト
    log_info "単体テストを実行中..."
    pytest tests/unit/ -v --tb=short

    # 統合テスト
    log_info "統合テストを実行中..."
    pytest tests/integration/ -v --tb=short

    # 型チェック
    log_info "型チェックを実行中..."
    mypy ai_blocks/

    # コードフォーマットチェック
    log_info "コードフォーマットをチェック中..."
    black --check ai_blocks/ tests/
    isort --check-only ai_blocks/ tests/

    log_success "全テスト完了"
}

# パッケージビルド
build_package() {
    log_info "パッケージをビルド中..."

    # クリーンアップ
    make clean

    # ビルド
    python -m build

    # ビルド結果確認
    if [[ -d "dist" && $(ls -1 dist/ | wc -l) -gt 0 ]]; then
        log_success "パッケージビルド完了"
        ls -la dist/
    else
        log_error "パッケージビルドに失敗しました"
        exit 1
    fi
}

# Dockerイメージビルド
build_docker_image() {
    log_info "Dockerイメージをビルド中..."

    local image_tag="${DOCKER_IMAGE_NAME}:${CURRENT_VERSION}"
    local latest_tag="${DOCKER_IMAGE_NAME}:latest"

    # Dockerイメージビルド
    docker build -t "$image_tag" -t "$latest_tag" .

    # イメージサイズ確認
    local image_size=$(docker images "$image_tag" --format "table {{.Size}}" | tail -n 1)
    log_success "Dockerイメージビルド完了: $image_tag (サイズ: $image_size)"
}

# Docker本番デプロイ
deploy_docker() {
    log_info "Docker本番デプロイを開始中..."

    # 環境変数ファイルの存在確認
    if [[ ! -f ".env.production" ]]; then
        log_error ".env.production ファイルが見つかりません"
        exit 1
    fi

    # 既存コンテナの停止
    if docker-compose -f "$DOCKER_COMPOSE_FILE" ps -q | grep -q .; then
        log_info "既存コンテナを停止中..."
        docker-compose -f "$DOCKER_COMPOSE_FILE" down
    fi

    # 新しいコンテナの起動
    log_info "新しいコンテナを起動中..."
    docker-compose -f "$DOCKER_COMPOSE_FILE" up -d

    # 起動確認
    sleep 10
    if docker-compose -f "$DOCKER_COMPOSE_FILE" ps | grep -q "Up"; then
        log_success "Docker本番デプロイ完了"
    else
        log_error "Docker本番デプロイに失敗しました"
        docker-compose -f "$DOCKER_COMPOSE_FILE" logs
        exit 1
    fi
}

# PyPIアップロード
upload_to_pypi() {
    local repository="${1:-pypi}"

    log_info "PyPI ($repository) にアップロード中..."

    # twineの存在確認
    if ! command -v twine &> /dev/null; then
        log_info "twineをインストール中..."
        pip install twine
    fi

    # アップロード
    if [[ "$repository" == "testpypi" ]]; then
        twine upload --repository testpypi dist/*
    else
        twine upload dist/*
    fi

    log_success "PyPI ($repository) アップロード完了"
}

# ヘルスチェック
health_check() {
    log_info "ヘルスチェックを実行中..."

    local health_url="http://localhost:8000/health"
    local max_attempts=30
    local attempt=1

    while [[ $attempt -le $max_attempts ]]; do
        if curl -s "$health_url" > /dev/null 2>&1; then
            local response=$(curl -s "$health_url")
            log_success "ヘルスチェック成功: $response"
            return 0
        fi

        log_info "ヘルスチェック試行 $attempt/$max_attempts..."
        sleep 2
        ((attempt++))
    done

    log_error "ヘルスチェックに失敗しました"
    return 1
}

# ロールバック
rollback() {
    log_warning "ロールバックを実行中..."

    # 前のバージョンを取得
    local previous_version=$(git tag --sort=-version:refname | head -n 2 | tail -n 1)

    if [[ -z "$previous_version" ]]; then
        log_error "ロールバック可能なバージョンが見つかりません"
        exit 1
    fi

    log_info "バージョン $previous_version にロールバック中..."

    # Dockerコンテナの停止
    docker-compose -f "$DOCKER_COMPOSE_FILE" down

    # 前のイメージでデプロイ
    local previous_image="${DOCKER_IMAGE_NAME}:${previous_version#v}"
    docker tag "$previous_image" "${DOCKER_IMAGE_NAME}:latest"
    docker-compose -f "$DOCKER_COMPOSE_FILE" up -d

    log_success "ロールバック完了: $previous_version"
}

# 完全リリース
full_release() {
    log_info "完全リリースを開始します..."

    check_prerequisites
    run_tests
    build_package

    if [[ "${TEST_ONLY:-false}" != "true" ]]; then
        build_docker_image
        upload_to_pypi
        deploy_docker
        health_check

        # Gitタグ作成
        local tag_name="v${CURRENT_VERSION}"
        git tag "$tag_name"
        git push origin "$tag_name"

        log_success "完全リリース完了: $tag_name"
    else
        log_success "テスト環境リリース完了"
    fi
}

# メイン処理
main() {
    local command=""
    local new_version=""

    # 引数解析
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                show_help
                exit 0
                ;;
            -v|--version)
                new_version="$2"
                shift 2
                ;;
            -t|--test-only)
                TEST_ONLY="true"
                shift
                ;;
            --skip-tests)
                SKIP_TESTS="true"
                shift
                ;;
            --force)
                FORCE="true"
                shift
                ;;
            build|test|docker-build|docker-deploy|pypi-upload|full-release|health-check|rollback)
                command="$1"
                shift
                ;;
            *)
                log_error "不明なオプション: $1"
                show_help
                exit 1
                ;;
        esac
    done

    # バージョン更新
    if [[ -n "$new_version" ]]; then
        update_version "$new_version"
    fi

    # コマンド実行
    case "$command" in
        build)
            check_prerequisites
            build_package
            ;;
        test)
            run_tests
            ;;
        docker-build)
            check_prerequisites
            build_docker_image
            ;;
        docker-deploy)
            check_prerequisites
            deploy_docker
            ;;
        pypi-upload)
            check_prerequisites
            build_package
            upload_to_pypi
            ;;
        full-release)
            full_release
            ;;
        health-check)
            health_check
            ;;
        rollback)
            rollback
            ;;
        "")
            log_error "コマンドが指定されていません"
            show_help
            exit 1
            ;;
        *)
            log_error "不明なコマンド: $command"
            show_help
            exit 1
            ;;
    esac
}

# スクリプト実行
main "$@"
