.PHONY: help install install-dev test test-unit test-integration test-performance lint format type-check clean build docs

# デフォルトターゲット
help:
	@echo "利用可能なコマンド:"
	@echo "  install          - 本番用依存関係をインストール"
	@echo "  install-dev      - 開発用依存関係をインストール"
	@echo "  test             - 全てのテストを実行"
	@echo "  test-unit        - 単体テストを実行"
	@echo "  test-integration - 統合テストを実行"
	@echo "  test-performance - パフォーマンステストを実行"
	@echo "  lint             - コードの静的解析を実行"
	@echo "  format           - コードフォーマットを実行"
	@echo "  type-check       - 型チェックを実行"
	@echo "  clean            - 一時ファイルを削除"
	@echo "  build            - パッケージをビルド"
	@echo "  docs             - ドキュメントを生成"

# インストール
install:
	pip install -e .

install-dev:
	pip install -e ".[dev]"
	pre-commit install

# テスト
test:
	pytest tests/ -v

test-unit:
	pytest tests/unit/ -v

test-integration:
	pytest tests/integration/ -v

test-performance:
	pytest tests/performance/ -v -m slow

# コード品質
lint:
	flake8 ai_blocks/ tests/
	mypy ai_blocks/

format:
	black ai_blocks/ tests/
	isort ai_blocks/ tests/

type-check:
	mypy ai_blocks/

# クリーンアップ
clean:
	rm -rf build/ dist/ *.egg-info/
	find . -type f -name "*.pyc" -delete
	find . -type d -name "__pycache__" -delete
	find . -type d -name "*.egg-info" -exec rm -rf {} +
	rm -rf build/
	rm -rf dist/
	rm -rf .pytest_cache/
	rm -rf .mypy_cache/
	rm -rf __temp_tests__/

# パッケージビルド
build:
	python -m build

# PyPIへのアップロード（テスト環境）
upload-test:
	python -m twine upload --repository testpypi dist/*

# PyPIへのアップロード（本番環境）
upload:
	python -m twine upload dist/*

# ドキュメント
docs:
	cd docs && make html

# 開発環境セットアップ
setup-dev: install-dev
	@echo "開発環境のセットアップが完了しました"
	@echo "以下のコマンドでテストを実行できます:"
	@echo "  make test"
