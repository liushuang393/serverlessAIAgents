# AgentFlow Makefile
# コード品質チェックと自動修正のためのコマンド集
# 使用する Python: conda 環境の場合は conda activate agentflow 後に make を実行すること。
# 未指定時は python3 を使用（上書き: make PYTHON=python check-all）
PYTHON ?= python3

.PHONY: help install install-dev install-hooks format lint type-check test test-cov clean check-all pre-commit

# デフォルトターゲット
help:
	@echo "AgentFlow - 利用可能なコマンド:"
	@echo ""
	@echo "  make install          - 本番環境用の依存関係をインストール"
	@echo "  make install-dev      - 開発環境用の依存関係をインストール"
	@echo "  make install-hooks    - pre-commit フックをインストール"
	@echo ""
	@echo "  make format           - コードを自動フォーマット (Python + JS/TS)"
	@echo "  make lint             - リントチェック (Python + JS/TS)"
	@echo "  make type-check       - 型チェック (Python + TypeScript)"
	@echo "  make test             - テストを実行"
	@echo "  make test-cov         - カバレッジ付きでテストを実行"
	@echo ""
	@echo "  make check-all        - すべてのチェックを実行 (format + lint + type + test)"
	@echo "  make check-nomypy     - 型チェックをスキップして実行 (format + lint + test)"
	@echo "  make pre-commit       - pre-commit を全ファイルに実行"
	@echo "  make clean            - 一時ファイルとキャッシュを削除"

# ========================================
# インストール
# ========================================

install:
	@echo "📦 本番環境用の依存関係をインストール中..."
	$(PYTHON) -m pip install -e .

install-dev:
	@echo "📦 開発環境用の依存関係をインストール中..."
	$(PYTHON) -m pip install -e ".[dev]"
	@echo "📦 フロントエンド依存関係をインストール中..."
	cd studio && npm install

install-hooks:
	@echo "🪝 pre-commit フックをインストール中..."
	$(PYTHON) -m pip install pre-commit
	$(PYTHON) -m pre_commit install
	@echo "✅ pre-commit フックがインストールされました"

# ========================================
# Python: フォーマットとリント
# ========================================

format-python:
	@echo "🎨 Python コードをフォーマット中..."
	@echo "  → 不要な type: ignore を削除..."
	-$(PYTHON) scripts/fix_mypy_safe.py
	$(PYTHON) -m ruff format .
	$(PYTHON) -m ruff check --fix .

lint-python:
	@echo "🔍 Python コードをリントチェック中..."
	$(PYTHON) -m ruff check .

type-check-python:
	@echo "🔍 Python 型チェック中..."
	$(PYTHON) -m mypy agentflow --strict --ignore-missing-imports

# ========================================
# JavaScript/TypeScript: フォーマットとリント
# ========================================

format-js:
	@echo "🎨 JS/TS コードをフォーマット中..."
	cd studio && npx prettier --write "src/**/*.{ts,tsx,js,jsx,json,css}"

lint-js:
	@echo "🔍 JS/TS コードをリントチェック中..."
	cd studio && npx eslint "src/**/*.{ts,tsx,js,jsx}" --fix --max-warnings=0

type-check-js:
	@echo "🔍 TypeScript 型チェック中..."
	cd studio && npx tsc --noEmit

# ========================================
# 統合コマンド
# ========================================

format: format-python format-js
	@echo "✅ すべてのコードがフォーマットされました"

lint: lint-python lint-js
	@echo "✅ すべてのリントチェックが完了しました"

type-check: type-check-python type-check-js
	@echo "✅ すべての型チェックが完了しました"

# ========================================
# テスト
# ========================================

test:
	@echo "🧪 テストを実行中..."
	$(PYTHON) -m pytest -v

test-cov:
	@echo "🧪 カバレッジ付きでテストを実行中..."
	$(PYTHON) -m pytest --cov=agentflow --cov-report=html --cov-report=term-missing -v
	@echo "📊 カバレッジレポート: htmlcov/index.html"

test-watch:
	@echo "🧪 テストを監視モードで実行中..."
	$(PYTHON) -m ptw

# ========================================
# すべてのチェック
# ========================================

check-all: format lint type-check test
	@echo "✅ すべてのチェックが完了しました！"

# 型チェックをスキップ（型エラー解消中に format/lint/test のみ確認する用）
check-nomypy: format lint test
	@echo "✅ フォーマット・リント・テストが完了しました（型チェックはスキップ）"

# ========================================
# Pre-commit
# ========================================

pre-commit:
	@echo "🪝 pre-commit を全ファイルに実行中..."
	$(PYTHON) -m pre_commit run --all-files

pre-commit-update:
	@echo "🔄 pre-commit フックを更新中..."
	pre-commit autoupdate

# ========================================
# クリーンアップ
# ========================================

clean:
	@echo "🧹 一時ファイルとキャッシュを削除中..."
	find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".mypy_cache" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name ".ruff_cache" -exec rm -rf {} + 2>/dev/null || true
	find . -type d -name "*.egg-info" -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete 2>/dev/null || true
	find . -type f -name "*.pyo" -delete 2>/dev/null || true
	find . -type f -name ".coverage" -delete 2>/dev/null || true
	rm -rf htmlcov/ 2>/dev/null || true
	rm -rf dist/ 2>/dev/null || true
	rm -rf build/ 2>/dev/null || true
	rm -rf studio/dist/ 2>/dev/null || true
	rm -rf studio/node_modules/.cache/ 2>/dev/null || true
	@echo "✅ クリーンアップ完了"

clean-all: clean
	@echo "🧹 すべての依存関係を削除中..."
	rm -rf venv/ 2>/dev/null || true
	rm -rf .venv/ 2>/dev/null || true
	rm -rf studio/node_modules/ 2>/dev/null || true
	@echo "✅ すべてのクリーンアップ完了"

# ========================================
# ビルド
# ========================================

build:
	@echo "📦 パッケージをビルド中..."
	$(PYTHON) -m build

build-frontend:
	@echo "📦 フロントエンドをビルド中..."
	cd studio && npm run build

# ========================================
# 開発サーバー
# ========================================

dev-backend:
	@echo "🚀 バックエンドサーバーを起動中..."
	uvicorn agentflow.studio.server:app --reload --host 0.0.0.0 --port 8000

dev-frontend:
	@echo "🚀 フロントエンドサーバーを起動中..."
	cd studio && npm run dev

# ========================================
# CI/CD
# ========================================

ci: install-dev check-all
	@echo "✅ CI チェックが完了しました！"
