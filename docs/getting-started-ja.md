# AgentFlow 初心者ガイド

このガイドでは、AgentFlow の**インストール**から**日常的な使い方**まで、初心者でも分かるように説明します。

---

## 📋 目次

1. [事前準備](#事前準備)
2. [インストール](#インストール)
3. [初期設定](#初期設定)
4. [日常的な使い方](#日常的な使い方)
5. [よくある質問](#よくある質問)

---

## 🔧 事前準備

### 必要なソフトウェア

以下のソフトウェアをインストールしてください：

1. **Anaconda** または **Miniconda**
   - ダウンロード: https://www.anaconda.com/download
   - Python 環境を管理するツール

2. **Git**
   - ダウンロード: https://git-scm.com/download/win
   - コードをダウンロードするツール

3. **Node.js** (v18 以上)
   - ダウンロード: https://nodejs.org/
   - フロントエンド開発に必要

### インストール確認

**Anaconda Prompt** を開いて、以下のコマンドを実行してください：

```bash
# Conda のバージョン確認
conda --version
# 例: conda 24.1.0

# Git のバージョン確認
git --version
# 例: git version 2.43.0

# Node.js のバージョン確認
node --version
# 例: v20.11.0
```

すべて表示されれば OK です！

---

## 📦 インストール

### ステップ 1: プロジェクトをダウンロード

**Anaconda Prompt** を開いて、以下を実行：

```bash
# プロジェクトをダウンロード
git clone https://github.com/liushuang393/serverlessAIAgents.git

# プロジェクトフォルダに移動
cd serverlessAIAgents
```

### ステップ 2: Conda 環境を作成

```bash
# Conda 環境を作成（初回のみ）
conda env create -f environment.yml

# 環境をアクティベート
conda activate agentflow
```

**重要**: 今後、AgentFlow を使うときは必ず `conda activate agentflow` を実行してください！

### ステップ 3: 開発環境をセットアップ

```bash
# 自動セットアップスクリプトを実行
setup_dev.bat
```

このスクリプトは以下を自動で行います：
- ✅ Python パッケージのインストール
- ✅ Pre-commit フックのインストール
- ✅ フロントエンド依存関係のインストール
- ✅ インストール確認

### ステップ 4: インストール確認

```bash
# AgentFlow のバージョン確認
agentflow --version
# 表示例: agentflow, version 1.0.0
```

バージョンが表示されれば、インストール成功です！🎉

---

## ⚙️ 初期設定

### Pre-commit フックとは？

Git でコミットする前に、自動的にコードをチェック・修正してくれる便利な機能です。

### 設定方法

すでに `setup_dev.bat` で設定済みですが、手動で設定する場合：

```bash
# Pre-commit をインストール
pip install pre-commit

# フックを有効化
pre-commit install

# 動作確認
pre-commit run --all-files
```

---

## 💻 日常的な使い方

### 毎日の作業フロー

#### 1. Conda 環境をアクティベート

**Anaconda Prompt** を開いて：

```bash
# プロジェクトフォルダに移動
cd d:\pythonPJ\serverlessAIAgents

# Conda 環境をアクティベート
conda activate agentflow
```

#### 2. コードを編集

お好きなエディタ（VS Code など）でコードを編集します。

#### 3. コードをチェック

**PowerShell** の場合：

```powershell
# すべてのチェックを実行
.\check.ps1 all
```

**コマンドプロンプト (CMD)** の場合：

```cmd
# すべてのチェックを実行
check.bat all
```

#### 4. Git にコミット

```bash
# 変更をステージング
git add .

# コミット（自動チェックが実行される）
git commit -m "feat: 新機能を追加"

# GitHub にプッシュ
git push origin main
```

---

## 📝 コマンド一覧

### check.ps1 / check.bat コマンド

| コマンド | 説明 | 実行時間 |
|---------|------|---------|
| `.\check.ps1 help` | ヘルプを表示 | 即座 |
| `.\check.ps1 format` | コードを自動フォーマット | 10秒 |
| `.\check.ps1 lint` | リントチェック | 10秒 |
| `.\check.ps1 type-check` | 型チェック | 20秒 |
| `.\check.ps1 test` | テストを実行 | 30秒 |
| `.\check.ps1 test-cov` | カバレッジ付きテスト | 40秒 |
| `.\check.ps1 all` | **すべてのチェック** | 1分 |
| `.\check.ps1 pre-commit` | Pre-commit を実行 | 30秒 |
| `.\check.ps1 clean` | 一時ファイルを削除 | 5秒 |

### 推奨される使い方

#### コミット前（必須）

```powershell
# すべてのチェックを実行
.\check.ps1 all
```

#### コードを書いている途中

```powershell
# フォーマットだけ実行
.\check.ps1 format
```

#### エラーが出たとき

```powershell
# リントチェックで詳細を確認
.\check.ps1 lint

# 型チェックで詳細を確認
.\check.ps1 type-check
```

---

## 🎯 実践例

### 例 1: 新機能を追加する

```bash
# 1. Conda 環境をアクティベート
conda activate agentflow

# 2. 新しいブランチを作成
git checkout -b feature/new-feature

# 3. コードを編集
# （VS Code などでコードを編集）

# 4. すべてのチェックを実行
.\check.ps1 all

# 5. コミット
git add .
git commit -m "feat: 新機能を追加"

# 6. プッシュ
git push origin feature/new-feature
```

### 例 2: バグを修正する

```bash
# 1. Conda 環境をアクティベート
conda activate agentflow

# 2. バグ修正ブランチを作成
git checkout -b fix/bug-fix

# 3. コードを修正
# （VS Code などでコードを修正）

# 4. テストを実行
.\check.ps1 test

# 5. すべてのチェックを実行
.\check.ps1 all

# 6. コミット
git add .
git commit -m "fix: バグを修正"

# 7. プッシュ
git push origin fix/bug-fix
```

### 例 3: コードをクリーンアップする

```bash
# 1. フォーマットを実行
.\check.ps1 format

# 2. 一時ファイルを削除
.\check.ps1 clean

# 3. コミット
git add .
git commit -m "style: コードをフォーマット"
```

---

## ❓ よくある質問

### Q1: `conda activate agentflow` を忘れたらどうなる？

**A**: コマンドが見つからないエラーが出ます。必ず `conda activate agentflow` を実行してください。

```bash
# エラー例
agentflow: The term 'agentflow' is not recognized...

# 解決方法
conda activate agentflow
```

### Q2: `.\check.ps1 all` でエラーが出た

**A**: エラーメッセージを読んで、該当箇所を修正してください。

```powershell
# エラー詳細を確認
.\check.ps1 lint
.\check.ps1 type-check

# 自動修正を試す
.\check.ps1 format
```

### Q3: Pre-commit が遅い

**A**: 初回実行時は依存関係をダウンロードするため遅いです。2回目以降は速くなります。

```bash
# キャッシュをクリア
pre-commit clean

# 再インストール
pre-commit install --install-hooks
```

### Q4: Git コミット時に自動チェックをスキップしたい

**A**: 緊急時のみ `--no-verify` を使用してください（非推奨）。

```bash
# 自動チェックをスキップ（非推奨）
git commit --no-verify -m "message"
```

### Q5: `make` コマンドが使えない

**A**: Windows では `make` が標準でインストールされていません。代わりに `check.ps1` または `check.bat` を使用してください。

```powershell
# PowerShell の場合
.\check.ps1 all

# CMD の場合
check.bat all
```

---

## 📚 次のステップ

1. **詳細なドキュメントを読む**
   - `docs/quality-checks.md` - コード品質チェックの詳細
   - `docs/development.md` - 開発ガイド
   - `docs/implementation-guide.md` - 実装ガイド

2. **サンプルエージェントを試す**
   ```bash
   # サンプルエージェントを実行
   cd examples/text_processor_agent
   agentflow run .
   ```

3. **自分のエージェントを作成**
   ```bash
   # 新しいエージェントを作成
   agentflow create my-agent --template basic
   cd my-agent
   ```

---

## 🎊 まとめ

### 覚えておくべき 3 つのコマンド

1. **環境をアクティベート**
   ```bash
   conda activate agentflow
   ```

2. **すべてのチェックを実行**
   ```powershell
   .\check.ps1 all
   ```

3. **コミット**
   ```bash
   git add .
   git commit -m "feat: 変更内容"
   git push origin main
   ```

これだけ覚えれば、AgentFlow の開発ができます！🚀

---

**困ったときは**: `docs/quality-checks.md` を参照するか、GitHub Issues で質問してください。

