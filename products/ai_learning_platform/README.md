
# AI学習プラットフォーム

個人化されたAI・機械学習学習体験を提供するWebアプリケーション

## 📋 製品概要

AI学習プラットフォームは、ユーザーの技能レベルに応じて個人化された学習体験を提供するWebアプリケーションです。
技能診断から始まり、最適な学習パスを提案し、インタラクティブな学習コンテンツと実時間フィードバックを通じて効果的な学習をサポートします。

## 🎯 主要機能

### 1. ユーザー認証システム
- **ユーザー登録・ログイン**: JWT認証による安全なアカウント管理
- **プロフィール管理**: ユーザー情報の編集と学習統計の表示
- **セッション管理**: 自動ログアウトとトークン更新

### 2. 技能診断システム
- **多段階評価**: カテゴリ別の技能評価問題
- **AI分析**: OpenAI APIを活用した詳細な技能分析
- **結果可視化**: 強み・弱み・推奨学習内容の表示
- **進捗追跡**: 診断結果の履歴管理

### 3. 個人化学習パス
- **適応型カリキュラム**: 診断結果に基づく学習コンテンツ推奨
- **段階的学習**: 前提知識に基づく順次解放システム
- **進捗管理**: リアルタイムの学習進捗追跡
- **学習統計**: 完了率、学習時間、連続学習日数の可視化

### 4. インタラクティブ学習コンテンツ
- **Markdownコンテンツ**: 構造化された理論学習
- **動画学習**: 視覚的な説明とデモンストレーション
- **クイズシステム**: 理解度確認のための選択問題
- **実践演習**: シミュレーション型の実習課題

### 5. AI フィードバックシステム
- **提出物評価**: メール、レポート、プレゼンテーション資料の評価
- **建設的フィードバック**: 改善点と具体的な提案の提供
- **学習支援**: 個別指導に近い詳細なアドバイス

### 6. 学習効果可視化
- **ダッシュボード**: 学習状況の総合的な表示
- **進捗グラフ**: 時系列での学習進捗の可視化
- **実績システム**: 学習マイルストーンの達成管理
- **レポート機能**: 学習成果の詳細分析

## 🏗️ システム構成

### バックエンド (FastAPI)
- **フレームワーク**: FastAPI 0.104.1
- **データベース**: MySQL 8.0 + SQLAlchemy 2.0.23
- **認証**: JWT (python-jose)
- **AI統合**: OpenAI API 1.3.7
- **API文書**: 自動生成されたSwagger UI

### フロントエンド (React)
- **フレームワーク**: React 18.2.0
- **ルーティング**: React Router DOM 6.8.1
- **UI ライブラリ**: Material-UI 5.15.0
- **HTTP クライアント**: Axios 1.6.2
- **Markdown レンダリング**: React Markdown 9.0.1
- **チャート**: Chart.js 4.4.0 + React Chart.js 2

### データベース設計
- **users**: ユーザー情報管理
- **chapters**: 学習チャプター管理
- **skill_questions**: 技能評価問題
- **user_skill_profiles**: ユーザー技能プロフィール
- **learning_contents**: 学習コンテンツ
- **user_learning_progress**: 学習進捗追跡
- **user_submissions**: 提出物管理
- **ai_feedbacks**: AIフィードバック

## 📚 学習コンテンツ

### チャプター構成
1. **Introduction to Language Models** - 言語モデルの基本概念
2. **Tokens and Embeddings** - トークン化とベクトル表現
3. **Looking Inside Transformer LLMs** - Transformerアーキテクチャ
4. **Text Classification** - テキスト分類タスク
5. **Text Clustering and Topic Modeling** - クラスタリングとトピックモデリング
6. **Prompt Engineering** - 効果的なプロンプト設計
7. **Advanced Text Generation Techniques** - 高度なテキスト生成
8. **Semantic Search and RAG** - セマンティック検索とRAG
9. **Multimodal Large Language Models** - マルチモーダルモデル
10. **Creating Text Embedding Models** - テキスト埋め込みモデル作成
11. **Fine-tuning Representation Models** - 表現モデルの微調整
12. **Fine-tuning Generation Models** - 生成モデルの微調整

### コンテンツタイプ
- **理論学習**: Markdown形式の構造化コンテンツ
- **動画学習**: 概念説明とデモンストレーション
- **理解度チェック**: 選択式クイズ
- **実践演習**: シミュレーション型課題

## 🔧 技術仕様

### API エンドポイント
```
POST /api/v1/auth/register     - ユーザー登録
POST /api/v1/auth/login        - ユーザーログイン
GET  /api/v1/auth/me           - 現在のユーザー情報取得
GET  /api/v1/skill-assessment/categories - 技能カテゴリ一覧
GET  /api/v1/skill-assessment/questions/{category} - カテゴリ別問題取得
POST /api/v1/skill-assessment/submit - 回答提出
GET  /api/v1/learning/chapters - チャプター一覧
GET  /api/v1/learning/chapters/{chapter_id} - チャプター詳細
GET  /api/v1/learning/progress - 学習進捗取得
PUT  /api/v1/learning/progress/{content_id} - 進捗更新
POST /api/v1/learning/submissions - 提出物作成
GET  /api/v1/learning/submissions/{submission_id}/feedback - AIフィードバック取得
```

### セキュリティ機能
- **JWT認証**: アクセストークンによる認証
- **CORS設定**: クロスオリジンリクエスト制御
- **入力検証**: Pydanticによるデータ検証
- **SQLインジェクション対策**: SQLAlchemy ORMの使用
- **パスワードハッシュ化**: bcryptによる安全なパスワード保存

## 🚀 インストールと起動手順

### 前提条件
- **Python**: 3.8以上
- **Node.js**: 16.0以上
- **MySQL**: 8.0以上
- **Git**: 最新版

### 1. リポジトリのクローン
```bash
git clone https://github.com/liushuang393/ai-learning-platform.git
cd ai-learning-platform
```

### 2. データベースの準備
```bash
# MySQLサーバーを起動
sudo systemctl start mysql

# データベースを作成
mysql -u root -p
CREATE DATABASE ai_learning_platform CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
EXIT;
```

### 3. バックエンドの設定と起動

#### 3.1 仮想環境の作成と有効化
```bash
cd backend
python -m venv venv

# Windows
venv\Scripts\activate

# macOS/Linux
source venv/bin/activate
```

#### 3.2 依存関係のインストール
```bash
pip install -r requirements.txt
```

#### 3.3 環境変数の設定
```bash
# .env.exampleを.envにコピー
cp .env.example .env

# .envファイルを編集
nano .env
```

**.envファイルの設定例:**
```env
# データベース設定
DATABASE_URL=mysql+pymysql://root:your_password@localhost:3306/ai_learning_platform

# セキュリティ設定
SECRET_KEY=your-super-secret-key-here
ACCESS_TOKEN_EXPIRE_MINUTES=30

# OpenAI API設定
OPENAI_API_KEY=your-openai-api-key-here

# ログ設定
LOG_LEVEL=INFO
DEBUG=true

# CORS設定
BACKEND_CORS_ORIGINS=http://localhost:3000,http://127.0.0.1:3000
```

#### 3.4 データベーステーブルの作成
```bash
# テストスクリプトを実行してテーブルを作成
python test_main.py
```

#### 3.5 バックエンドサーバーの起動
```bash
uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
```

### 4. フロントエンドの設定と起動

#### 4.1 新しいターミナルでフロントエンドディレクトリに移動
```bash
cd frontend
```

#### 4.2 依存関係のインストール
```bash
npm install
```

#### 4.3 環境変数の設定（オプション）
```bash
# .envファイルを作成（必要に応じて）
echo "REACT_APP_API_URL=http://localhost:8000/api/v1" > .env
```

#### 4.4 フロントエンドサーバーの起動
```bash
npm start
```

### 5. アプリケーションへのアクセス

- **フロントエンド**: http://localhost:3000
- **バックエンドAPI**: http://localhost:8000
- **API文書**: http://localhost:8000/docs
- **ReDoc**: http://localhost:8000/redoc

### 6. 初期データの投入（オプション）

#### 6.1 管理者ユーザーの作成
```bash
# バックエンドディレクトリで実行
python -c "
from app.core.auth import get_password_hash
from app.database import SessionLocal
from app.models.user import User

db = SessionLocal()
admin_user = User(
    username='admin',
    email='admin@example.com',
    hashed_password=get_password_hash('admin123'),
    full_name='管理者',
    is_admin=True
)
db.add(admin_user)
db.commit()
print('管理者ユーザーを作成しました')
"
```

#### 6.2 サンプル技能評価問題の投入
```bash
python -c "
from app.database import SessionLocal
from app.models.skill_question import SkillQuestion
import json

db = SessionLocal()

questions = [
    {
        'category': 'Prompt Engineering',
        'question': '効果的なプロンプトの特徴として最も重要なのはどれですか？',
        'options': json.dumps({
            'A': '明確で具体的な指示',
            'B': '曖昧で抽象的な表現',
            'C': '可能な限り短い文章',
            'D': '専門用語を多用する'
        }),
        'correct_answer': 'A',
        'explanation': '効果的なプロンプトは明確で具体的である必要があります。'
    },
    {
        'category': 'Text Classification',
        'question': 'テキスト分類において、特徴量エンジニアリングの目的は何ですか？',
        'options': json.dumps({
            'A': 'データ量を増やすため',
            'B': 'モデルの性能を向上させるため',
            'C': '処理時間を長くするため',
            'D': 'メモリ使用量を増やすため'
        }),
        'correct_answer': 'B',
        'explanation': '特徴量エンジニアリングはモデルの性能向上が主な目的です。'
    }
]

for q_data in questions:
    question = SkillQuestion(**q_data)
    db.add(question)

db.commit()
print('サンプル問題を投入しました')
"
```

## 🔧 トラブルシューティング

### よくある問題と解決方法

#### 1. データベース接続エラー
```
sqlalchemy.exc.OperationalError: (pymysql.err.OperationalError)
```
**解決方法:**
- MySQLサーバーが起動していることを確認
- データベース認証情報が正しいことを確認
- データベースが存在することを確認

#### 2. OpenAI API エラー
```
openai.error.AuthenticationError: Incorrect API key provided
```
**解決方法:**
- OpenAI APIキーが正しく設定されていることを確認
- APIキーに十分なクレジットがあることを確認
- 環境変数が正しく読み込まれていることを確認

#### 3. フロントエンド起動エラー
```
npm ERR! Missing script: "start"
```
**解決方法:**
- `npm install`が正常に完了していることを確認
- `package.json`ファイルが存在することを確認
- Node.jsのバージョンが16.0以上であることを確認

#### 4. CORS エラー
```
Access to fetch at 'http://localhost:8000' from origin 'http://localhost:3000' has been blocked by CORS policy
```
**解決方法:**
- バックエンドの`.env`ファイルでCORS設定を確認
- `BACKEND_CORS_ORIGINS`にフロントエンドのURLが含まれていることを確認

#### 5. ポート競合エラー
```
Error: listen EADDRINUSE: address already in use :::8000
```
**解決方法:**
```bash
# 使用中のポートを確認
lsof -i :8000

# プロセスを終了
kill -9 <PID>

# または別のポートを使用
uvicorn app.main:app --reload --port 8001
```

### ログの確認方法

#### バックエンドログ
```bash
# アプリケーションログ
tail -f logs/app.log

# エラーログ
tail -f logs/error.log
```

#### フロントエンドログ
- ブラウザの開発者ツール（F12）でコンソールを確認
- ネットワークタブでAPI通信を確認

## 🧪 テスト実行

### バックエンドテスト
```bash
cd backend

# 単体テスト実行
pytest tests/ -v

# カバレッジ付きテスト
pytest tests/ --cov=app --cov-report=html

# 特定のテストファイル実行
pytest tests/test_auth.py -v
```

### フロントエンドテスト
```bash
cd frontend

# 単体テスト実行
npm test

# カバレッジ付きテスト
npm test -- --coverage

# E2Eテスト実行
npm run test:e2e
```

### 統合テスト
```bash
# バックエンドとフロントエンドの統合テスト
cd backend
python test_main.py
```

## 📦 本番環境デプロイ

### Docker を使用したデプロイ

#### 1. Dockerfileの作成
**backend/Dockerfile:**
```dockerfile
FROM python:3.9-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 8000

CMD ["uvicorn", "app.main:app", "--host", "0.0.0.0", "--port", "8000"]
```

**frontend/Dockerfile:**
```dockerfile
FROM node:16-alpine as build

WORKDIR /app

COPY package*.json ./
RUN npm ci --only=production

COPY . .
RUN npm run build

FROM nginx:alpine
COPY --from=build /app/build /usr/share/nginx/html
COPY nginx.conf /etc/nginx/nginx.conf

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
```

#### 2. Docker Compose設定
**docker-compose.yml:**
```yaml
version: '3.8'

services:
  mysql:
    image: mysql:8.0
    environment:
      MYSQL_ROOT_PASSWORD: rootpassword
      MYSQL_DATABASE: ai_learning_platform
    ports:
      - "3306:3306"
    volumes:
      - mysql_data:/var/lib/mysql

  backend:
    build: ./backend
    ports:
      - "8000:8000"
    environment:
      - DATABASE_URL=mysql+pymysql://root:rootpassword@mysql:3306/ai_learning_platform
      - OPENAI_API_KEY=${OPENAI_API_KEY}
    depends_on:
      - mysql

  frontend:
    build: ./frontend
    ports:
      - "80:80"
    depends_on:
      - backend

volumes:
  mysql_data:
```

#### 3. デプロイ実行
```bash
# 1. 環境変数設定（オプション）
export OPENAI_API_KEY=your-openai-api-key-here

# 2. コンテナビルドと起動
docker compose up --build -d

# 3. アクセス確認
# - フロントエンド: http://localhost:3000
# - バックエンドAPI: http://localhost:8000
# - Nginx: http://localhost:80
```

### クラウドデプロイ（AWS例）

#### 1. EC2インスタンスの準備
```bash
# EC2インスタンスにSSH接続
ssh -i your-key.pem ubuntu@your-ec2-ip

# 必要なソフトウェアをインストール
sudo apt update
sudo apt install -y python3 python3-pip nodejs npm mysql-server nginx
```

#### 2. アプリケーションのデプロイ
```bash
# リポジトリをクローン
git clone https://github.com/your-username/ai-learning-platform.git
cd ai-learning-platform

# バックエンドの設定
cd backend
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# フロントエンドのビルド
cd ../frontend
npm install
npm run build

# Nginxの設定
sudo cp nginx.conf /etc/nginx/sites-available/ai-learning-platform
sudo ln -s /etc/nginx/sites-available/ai-learning-platform /etc/nginx/sites-enabled/
sudo systemctl restart nginx
```

## 🔒 セキュリティ考慮事項

### 本番環境での設定

#### 1. 環境変数の管理
- 本番環境では強力なSECRET_KEYを使用
- データベースパスワードを複雑にする
- OpenAI APIキーを安全に管理

#### 2. HTTPS の設定
```bash
# Let's Encryptを使用したSSL証明書の取得
sudo apt install certbot python3-certbot-nginx
sudo certbot --nginx -d your-domain.com
```

#### 3. ファイアウォールの設定
```bash
# UFWを使用したファイアウォール設定
sudo ufw allow ssh
sudo ufw allow 'Nginx Full'
sudo ufw enable
```

#### 4. データベースセキュリティ
```sql
-- MySQLのセキュリティ設定
mysql_secure_installation

-- 専用ユーザーの作成
CREATE USER 'ai_platform'@'localhost' IDENTIFIED BY 'strong_password';
GRANT ALL PRIVILEGES ON ai_learning_platform.* TO 'ai_platform'@'localhost';
FLUSH PRIVILEGES;
```

## 📊 監視とメンテナンス

### ログ監視
```bash
# システムログの監視
sudo journalctl -u ai-learning-platform -f

# アプリケーションログの監視
tail -f /var/log/ai-learning-platform/app.log
```

### パフォーマンス監視
- **CPU使用率**: `htop`コマンドで確認
- **メモリ使用量**: `free -h`コマンドで確認
- **ディスク使用量**: `df -h`コマンドで確認
- **データベース性能**: MySQLのスロークエリログを確認

### バックアップ
```bash
# データベースバックアップ
mysqldump -u root -p ai_learning_platform > backup_$(date +%Y%m%d).sql

# アプリケーションファイルのバックアップ
tar -czf app_backup_$(date +%Y%m%d).tar.gz /path/to/ai-learning-platform
```

## 🤝 開発への参加

### 開発環境の準備
1. リポジトリをフォーク
2. 開発ブランチを作成
3. 変更を実装
4. テストを実行
5. プルリクエストを作成

### コーディング規約
- **Python**: PEP 8に準拠
- **JavaScript**: ESLint + Prettierを使用
- **コミットメッセージ**: Conventional Commitsに準拠

### 貢献ガイドライン
- バグ報告はIssueで作成
- 新機能提案はDiscussionで議論
- セキュリティ問題は非公開で報告

---

## 📞 サポート

### 技術サポート
- **Email**: support@ai-learning-platform.com
- **GitHub Issues**: バグ報告と機能要求
- **Discord**: リアルタイムサポート

### ドキュメント
- **API文書**: http://localhost:8000/docs
- **開発者ガイド**: `/docs/developer-guide.md`
- **ユーザーマニュアル**: `/docs/user-manual.md`

このAI学習プラットフォームは、最新のWeb技術とAI技術を組み合わせて、効果的で魅力的な学習体験を提供します。個人化された学習パス、インタラクティブなコンテンツ、そして実時間フィードバックにより、ユーザーのAI・機械学習スキル向上を強力にサポートします。
