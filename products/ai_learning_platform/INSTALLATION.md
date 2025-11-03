# AI学習プラットフォーム インストールガイド

このドキュメントでは、AI学習プラットフォームの詳細なインストール手順を説明します。

## 📋 システム要件

### 最小要件
- **OS**: Windows 10/11, macOS 10.15+, Ubuntu 18.04+
- **CPU**: 2コア以上
- **メモリ**: 4GB以上
- **ストレージ**: 10GB以上の空き容量
- **ネットワーク**: インターネット接続（OpenAI API使用のため）

### 推奨要件
- **OS**: Windows 11, macOS 12+, Ubuntu 20.04+
- **CPU**: 4コア以上
- **メモリ**: 8GB以上
- **ストレージ**: 20GB以上の空き容量（SSD推奨）
- **ネットワーク**: 高速インターネット接続

## 🛠️ 前提ソフトウェアのインストール

### 1. Python 3.8+ のインストール

#### Windows
```bash
# Microsoft Store からインストール、または
# https://www.python.org/downloads/ からダウンロード

# インストール確認
python --version
pip --version
```

#### macOS
```bash
# Homebrewを使用
brew install python@3.9

# または公式サイトからダウンロード
# https://www.python.org/downloads/

# インストール確認
python3 --version
pip3 --version
```

#### Ubuntu/Debian
```bash
sudo apt update
sudo apt install python3 python3-pip python3-venv

# インストール確認
python3 --version
pip3 --version
```

### 2. Node.js 16+ のインストール

#### Windows
```bash
# https://nodejs.org/ からLTS版をダウンロードしてインストール

# インストール確認
node --version
npm --version
```

#### macOS
```bash
# Homebrewを使用
brew install node

# または公式サイトからダウンロード
# https://nodejs.org/

# インストール確認
node --version
npm --version
```

#### Ubuntu/Debian
```bash
# NodeSourceリポジトリを使用
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
sudo apt-get install -y nodejs

# インストール確認
node --version
npm --version
```

### 3. MySQL 8.0+ のインストール

#### Windows
```bash
# https://dev.mysql.com/downloads/mysql/ からダウンロード
# MySQL Installerを使用してインストール

# サービス開始
net start mysql80
```

#### macOS
```bash
# Homebrewを使用
brew install mysql

# サービス開始
brew services start mysql

# 初期設定
mysql_secure_installation
```

#### Ubuntu/Debian
```bash
sudo apt update
sudo apt install mysql-server

# サービス開始
sudo systemctl start mysql
sudo systemctl enable mysql

# 初期設定
sudo mysql_secure_installation
```

### 4. Git のインストール

#### Windows
```bash
# https://git-scm.com/download/win からダウンロード
# または
winget install Git.Git
```

#### macOS
```bash
# Xcodeコマンドラインツール
xcode-select --install

# またはHomebrew
brew install git
```

#### Ubuntu/Debian
```bash
sudo apt install git
```

## 📥 プロジェクトのセットアップ

### 1. リポジトリのクローン
```bash
git clone https://github.com/your-username/ai-learning-platform.git
cd ai-learning-platform
```

### 2. データベースの準備

#### データベースの作成
```bash
# MySQLにログイン
mysql -u root -p

# データベース作成
CREATE DATABASE ai_learning_platform CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

# ユーザー作成（オプション）
CREATE USER 'ai_platform'@'localhost' IDENTIFIED BY 'your_password';
GRANT ALL PRIVILEGES ON ai_learning_platform.* TO 'ai_platform'@'localhost';
FLUSH PRIVILEGES;

EXIT;
```

#### データベース接続テスト
```bash
mysql -u ai_platform -p ai_learning_platform
SHOW TABLES;
EXIT;
```

## 🔧 バックエンドのセットアップ

### 1. 仮想環境の作成
```bash
cd backend

# 仮想環境作成
python -m venv venv

# 仮想環境有効化
# Windows
venv\Scripts\activate

# macOS/Linux
source venv/bin/activate

# 仮想環境確認
which python
```

### 2. 依存関係のインストール
```bash
# 依存関係インストール
pip install --upgrade pip
pip install -r requirements.txt

# インストール確認
pip list
```

### 3. 環境変数の設定
```bash
# 設定ファイルをコピー
cp .env.example .env

# 設定ファイルを編集
# Windows
notepad .env

# macOS/Linux
nano .env
```

#### .env ファイルの設定内容
```env
# データベース設定
DATABASE_URL=mysql+pymysql://ai_platform:your_password@localhost:3306/ai_learning_platform

# セキュリティ設定（本番環境では必ず変更）
SECRET_KEY=your-super-secret-key-change-in-production
ACCESS_TOKEN_EXPIRE_MINUTES=30

# OpenAI API設定
OPENAI_API_KEY=sk-your-openai-api-key-here

# ログ設定
LOG_LEVEL=INFO
DEBUG=true
TESTING=false

# CORS設定
BACKEND_CORS_ORIGINS=http://localhost:3000,http://127.0.0.1:3000

# ファイル設定
MAX_UPLOAD_SIZE=10485760
UPLOAD_DIR=uploads
CONTENT_DIR=learning_contents
```

### 4. データベーステーブルの作成
```bash
# テストスクリプト実行（テーブル作成含む）
python test_main.py

# または直接テーブル作成
python -c "
from app.database import create_tables
create_tables()
print('テーブル作成完了')
"
```

### 5. バックエンドサーバーの起動
```bash
# 開発サーバー起動
uvicorn app.main:app --reload --host 0.0.0.0 --port 8000

# 起動確認
# ブラウザで http://localhost:8000 にアクセス
# API文書は http://localhost:8000/docs で確認
```

## 🎨 フロントエンドのセットアップ

### 1. 新しいターミナルを開く
```bash
# プロジェクトルートから
cd frontend
```

### 2. 依存関係のインストール
```bash
# パッケージインストール
npm install

# インストール確認
npm list --depth=0
```

### 3. 環境変数の設定（オプション）
```bash
# 環境変数ファイル作成
echo "REACT_APP_API_URL=http://localhost:8000/api/v1" > .env

# Windows
echo REACT_APP_API_URL=http://localhost:8000/api/v1 > .env
```

### 4. フロントエンドサーバーの起動
```bash
# 開発サーバー起動
npm start

# 起動確認
# ブラウザで http://localhost:3000 にアクセス
```

## ✅ インストール確認

### 1. サービス起動確認
```bash
# バックエンド確認
curl http://localhost:8000/health

# フロントエンド確認
curl http://localhost:3000
```

### 2. データベース接続確認
```bash
# バックエンドディレクトリで実行
python -c "
from app.database import SessionLocal
try:
    db = SessionLocal()
    print('データベース接続成功')
    db.close()
except Exception as e:
    print(f'データベース接続エラー: {e}')
"
```

### 3. API動作確認
```bash
# ヘルスチェック
curl -X GET http://localhost:8000/health

# API文書確認
# ブラウザで http://localhost:8000/docs にアクセス
```

### 4. フロントエンド動作確認
- ブラウザで http://localhost:3000 にアクセス
- ホームページが表示されることを確認
- ナビゲーションが正常に動作することを確認

## 🔧 トラブルシューティング

### よくある問題

#### 1. Python仮想環境の問題
```bash
# 仮想環境が有効化されていない
source venv/bin/activate  # macOS/Linux
venv\Scripts\activate     # Windows

# 仮想環境の再作成
rm -rf venv
python -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

#### 2. データベース接続エラー
```bash
# MySQLサービス確認
# Windows
net start mysql80

# macOS
brew services start mysql

# Linux
sudo systemctl start mysql

# 接続テスト
mysql -u root -p -e "SELECT 1"
```

#### 3. ポート競合
```bash
# 使用中ポート確認
# Windows
netstat -ano | findstr :8000

# macOS/Linux
lsof -i :8000

# プロセス終了
kill -9 <PID>
```

#### 4. Node.js依存関係の問題
```bash
# node_modulesの再インストール
rm -rf node_modules package-lock.json
npm install

# キャッシュクリア
npm cache clean --force
```

### ログの確認方法

#### バックエンドログ
```bash
# コンソール出力を確認
# uvicornの出力を確認

# ログファイル確認（設定されている場合）
tail -f logs/app.log
```

#### フロントエンドログ
- ブラウザの開発者ツール（F12）を開く
- Consoleタブでエラーメッセージを確認
- NetworkタブでAPI通信を確認

## 🚀 次のステップ

インストールが完了したら：

1. **初期データの投入**: サンプルユーザーと問題データの作成
2. **機能テスト**: 各機能が正常に動作することを確認
3. **カスタマイズ**: 必要に応じて設定を調整
4. **本番環境準備**: セキュリティ設定と最適化

詳細は [README.md](README.md) を参照してください。
