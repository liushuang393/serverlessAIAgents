# コード品質チェックガイド

AgentFlow プロジェクトでは、コード品質を保証するために複数の自動チェックツールを使用しています。

---

## 📋 目次

1. [セットアップ](#セットアップ)
2. [使用ツール](#使用ツール)
3. [コマンド一覧](#コマンド一覧)
4. [Pre-commit フック](#pre-commit-フック)
5. [CI/CD 統合](#cicd-統合)
6. [トラブルシューティング](#トラブルシューティング)

---

## 🚀 セットアップ

### 1. 開発環境のインストール

```bash
# Conda 環境を使用する場合
conda env create -f environment.yml
conda activate agentflow
pip install -e ".[dev]"

# または Python venv を使用する場合
python -m venv venv
source venv/bin/activate  # Linux/Mac
.\venv\Scripts\Activate.ps1  # Windows
pip install -e ".[dev]"
```

### 2. Pre-commit フックのインストール

```bash
# Pre-commit をインストール
pip install pre-commit

# フックをインストール
pre-commit install

# 動作確認
pre-commit run --all-files
```

### 3. フロントエンド依存関係のインストール

```bash
cd studio
npm install
```

---

## 🛠️ 使用ツール

### Python ツール

| ツール | 用途 | 自動修正 |
|--------|------|---------|
| **Ruff** | リント + フォーマット | ✅ |
| **MyPy** | 型チェック | ❌ |
| **pytest** | テスト実行 | ❌ |
| **pytest-cov** | カバレッジ測定 | ❌ |
| **detect-secrets** | 機密情報検出 | ❌ |

### JavaScript/TypeScript ツール

| ツール | 用途 | 自動修正 |
|--------|------|---------|
| **ESLint** | リント | ✅ |
| **Prettier** | フォーマット | ✅ |
| **TypeScript** | 型チェック | ❌ |

### その他のツール

| ツール | 用途 | 自動修正 |
|--------|------|---------|
| **markdownlint** | Markdown リント | ✅ |
| **yamllint** | YAML 検証 | ❌ |

---

## 📝 コマンド一覧

### Makefile コマンド

```bash
# ヘルプを表示
make help

# すべてのチェックを実行
make check-all

# フォーマット（Python + JS/TS）
make format

# リント（Python + JS/TS）
make lint

# 型チェック（Python + TypeScript）
make type-check

# テスト実行
make test

# カバレッジ付きテスト
make test-cov

# Pre-commit を全ファイルに実行
make pre-commit

# クリーンアップ
make clean
```

### Python コマンド

```bash
# Ruff フォーマット
ruff format .

# Ruff リント（自動修正）
ruff check --fix .

# Ruff リント（チェックのみ）
ruff check .

# MyPy 型チェック
mypy agentflow --strict --ignore-missing-imports

# pytest テスト
pytest -v

# pytest カバレッジ
pytest --cov=agentflow --cov-report=html --cov-report=term-missing -v
```

### JavaScript/TypeScript コマンド

```bash
cd studio

# ESLint リント（自動修正）
npm run lint:fix

# ESLint リント（チェックのみ）
npm run lint

# Prettier フォーマット
npm run format

# Prettier チェック
npm run format:check

# TypeScript 型チェック
npm run type-check
```

---

## 🪝 Pre-commit フック

### 自動実行されるチェック

Git コミット時に以下のチェックが自動実行されます：

1. **Python**
   - Ruff Linter（自動修正）
   - Ruff Formatter
   - MyPy 型チェック（tests/ と examples/ を除く）

2. **JavaScript/TypeScript**
   - ESLint（自動修正）
   - Prettier フォーマット
   - TypeScript 型チェック

3. **YAML/JSON/TOML**
   - 構文チェック

4. **一般**
   - ファイル末尾の改行
   - 行末の空白削除
   - 大きなファイルのチェック
   - マージコンフリクトマーカーのチェック
   - デバッグステートメントのチェック

5. **セキュリティ**
   - 機密情報検出（detect-secrets）

6. **Markdown**
   - Markdown リント（自動修正）

### Pre-commit の使用方法

```bash
# すべてのファイルに対して実行
pre-commit run --all-files

# 特定のフックのみ実行
pre-commit run ruff --all-files
pre-commit run mypy --all-files

# フックを更新
pre-commit autoupdate

# フックをスキップしてコミット（非推奨）
git commit --no-verify -m "message"
```

---

## 🔄 CI/CD 統合

### GitHub Actions ワークフロー

プロジェクトには 3 つの CI/CD ワークフローがあります：

#### 1. Test Workflow (`.github/workflows/test.yml`)

- **トリガー**: Push, Pull Request
- **実行内容**:
  - Python 3.13 でテスト実行
  - カバレッジ測定
  - カバレッジレポートのアップロード

#### 2. Lint Workflow (`.github/workflows/lint.yml`)

- **トリガー**: Push, Pull Request
- **実行内容**:
  - Ruff リント
  - MyPy 型チェック
  - ESLint（フロントエンド）
  - Prettier チェック（フロントエンド）

#### 3. Publish Workflow (`.github/workflows/publish.yml`)

- **トリガー**: Tag push (`v*`)
- **実行内容**:
  - パッケージビルド
  - PyPI に公開

### ローカルで CI を再現

```bash
# CI と同じチェックを実行
make ci

# または個別に実行
make install-dev
make check-all
```

---

## 🐛 トラブルシューティング

### 問題 1: Pre-commit が遅い

**原因**: 初回実行時に依存関係をダウンロードするため

**解決策**:
```bash
# キャッシュをクリア
pre-commit clean

# 再インストール
pre-commit install --install-hooks
```

### 問題 2: MyPy エラーが多すぎる

**原因**: 厳格な型チェック設定

**解決策**:
```bash
# 特定のファイルのみチェック
mypy agentflow/core/

# または --ignore-missing-imports を使用
mypy agentflow --ignore-missing-imports
```

### 問題 3: ESLint エラーが修正されない

**原因**: 自動修正できないエラー

**解決策**:
```bash
# エラー詳細を確認
cd studio
npm run lint

# 手動で修正
# または ESLint ルールを調整（.eslintrc.json）
```

### 問題 4: Ruff と MyPy の競合

**原因**: Ruff が型アノテーションを削除する場合がある

**解決策**:
```bash
# Ruff の設定を調整（pyproject.toml）
# または MyPy の設定を調整
```

### 問題 5: detect-secrets が誤検出

**原因**: 機密情報ではない文字列を検出

**解決策**:
```bash
# ベースラインを更新
detect-secrets scan > .secrets.baseline

# または特定のファイルを除外（.pre-commit-config.yaml）
```

---

## 📚 参考資料

- [Ruff Documentation](https://docs.astral.sh/ruff/)
- [MyPy Documentation](https://mypy.readthedocs.io/)
- [ESLint Documentation](https://eslint.org/docs/)
- [Prettier Documentation](https://prettier.io/docs/)
- [Pre-commit Documentation](https://pre-commit.com/)
- [detect-secrets Documentation](https://github.com/Yelp/detect-secrets)

---

## ✅ ベストプラクティス

1. **コミット前に必ずチェック**
   ```bash
   make check-all
   ```

2. **定期的に Pre-commit を更新**
   ```bash
   pre-commit autoupdate
   ```

3. **カバレッジを 80% 以上に保つ**
   ```bash
   make test-cov
   ```

4. **型アノテーションを 100% にする**
   ```bash
   mypy agentflow --strict
   ```

5. **機密情報をコミットしない**
   ```bash
   # .gitignore に追加
   # detect-secrets でチェック
   ```

---

**🎊 コード品質を保ちながら開発を楽しみましょう！**

