# AI学習プラットフォーム API仕様書

## 📋 概要

AI学習プラットフォームのREST API仕様書です。すべてのAPIエンドポイントはJSON形式でデータを送受信します。

## 🔗 ベースURL

```
開発環境: http://localhost:8000/api/v1
本番環境: https://your-domain.com/api/v1
```

## 🔐 認証

### JWT認証
ほとんどのAPIエンドポイントはJWT（JSON Web Token）による認証が必要です。

#### 認証ヘッダー
```http
Authorization: Bearer <access_token>
```

#### トークンの取得
```http
POST /auth/login
```

## 📚 エンドポイント一覧

### 🔑 認証関連 (Authentication)

#### ユーザー登録
```http
POST /auth/register
```

**リクエストボディ:**
```json
{
  "username": "string",
  "email": "string",
  "password": "string",
  "full_name": "string" // オプション
}
```

**レスポンス (201 Created):**
```json
{
  "id": 1,
  "username": "test_user",
  "email": "test@example.com",
  "full_name": "テストユーザー",
  "is_active": true,
  "is_admin": false,
  "created_at": "2024-01-01T00:00:00Z",
  "updated_at": "2024-01-01T00:00:00Z",
  "last_login": null
}
```

#### ユーザーログイン
```http
POST /auth/login
```

**リクエストボディ:**
```json
{
  "username": "string",
  "password": "string"
}
```

**レスポンス (200 OK):**
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "token_type": "bearer",
  "expires_in": 1800,
  "user": {
    "id": 1,
    "username": "test_user",
    "email": "test@example.com",
    "full_name": "テストユーザー",
    "is_active": true,
    "is_admin": false,
    "created_at": "2024-01-01T00:00:00Z",
    "updated_at": "2024-01-01T00:00:00Z",
    "last_login": "2024-01-01T12:00:00Z"
  }
}
```

#### 現在のユーザー情報取得
```http
GET /auth/me
```
**認証:** 必須

**レスポンス (200 OK):**
```json
{
  "id": 1,
  "username": "test_user",
  "email": "test@example.com",
  "full_name": "テストユーザー",
  "bio": "AI学習中です",
  "is_active": true,
  "is_admin": false,
  "created_at": "2024-01-01T00:00:00Z",
  "updated_at": "2024-01-01T00:00:00Z",
  "last_login": "2024-01-01T12:00:00Z"
}
```

#### ログアウト
```http
POST /auth/logout
```
**認証:** 必須

**レスポンス (200 OK):**
```json
{
  "message": "正常にログアウトしました"
}
```

#### トークン更新
```http
POST /auth/refresh
```
**認証:** 必須

**レスポンス (200 OK):**
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "token_type": "bearer",
  "expires_in": 1800,
  "user": {
    "id": 1,
    "username": "test_user",
    "email": "test@example.com"
  }
}
```

### 🧠 技能評価関連 (Skill Assessment)

#### 技能カテゴリ一覧取得
```http
GET /skill-assessment/categories
```
**認証:** 必須

**レスポンス (200 OK):**
```json
[
  "Prompt Engineering",
  "Text Classification",
  "Language Models",
  "Fine-tuning",
  "Multimodal"
]
```

#### カテゴリ別問題取得
```http
GET /skill-assessment/questions/{category}
```
**認証:** 必須

**パスパラメータ:**
- `category`: 技能カテゴリ名

**レスポンス (200 OK):**
```json
[
  {
    "id": 1,
    "category": "Prompt Engineering",
    "question": "効果的なプロンプトの特徴として最も重要なのはどれですか？",
    "options": {
      "A": "明確で具体的な指示",
      "B": "曖昧で抽象的な表現",
      "C": "可能な限り短い文章",
      "D": "専門用語を多用する"
    }
  }
]
```

#### 全問題取得
```http
GET /skill-assessment/questions?limit=20
```
**認証:** 必須

**クエリパラメータ:**
- `limit`: 取得する問題数の上限（デフォルト: 20）

#### 回答提出
```http
POST /skill-assessment/submit
```
**認証:** 必須

**リクエストボディ:**
```json
{
  "answers": [
    {
      "question_id": 1,
      "selected_option": "A"
    },
    {
      "question_id": 2,
      "selected_option": "B"
    }
  ]
}
```

**レスポンス (200 OK):**
```json
{
  "overall_score": 78.5,
  "strengths": [
    "プロンプトエンジニアリング",
    "言語モデル"
  ],
  "weaknesses": [
    "ファインチューニング",
    "マルチモーダルモデル"
  ],
  "recommendations": [
    {
      "chapter_id": 11,
      "title": "Fine-tuning Representation Models",
      "reason": "ファインチューニング技術の理解を深める必要があります"
    }
  ],
  "detailed_analysis": "あなたは基本的なAI概念をよく理解していますが..."
}
```

### 📚 学習関連 (Learning)

#### チャプター一覧取得
```http
GET /learning/chapters
```

**レスポンス (200 OK):**
```json
[
  {
    "id": 1,
    "title": "Introduction to Language Models",
    "description": "言語モデルの基本概念について学習します",
    "order_index": 1,
    "created_at": "2024-01-01T00:00:00Z"
  }
]
```

#### チャプター詳細取得
```http
GET /learning/chapters/{chapter_id}
```
**認証:** 必須

**パスパラメータ:**
- `chapter_id`: チャプターID

**レスポンス (200 OK):**
```json
{
  "id": 1,
  "title": "Introduction to Language Models",
  "description": "言語モデルの基本概念について学習します",
  "order_index": 1,
  "created_at": "2024-01-01T00:00:00Z",
  "contents": [
    {
      "id": 1,
      "title": "言語モデルとは",
      "content_type": "markdown",
      "content_path": "/contents/chapter1/intro.md",
      "order_index": 1,
      "created_at": "2024-01-01T00:00:00Z"
    }
  ]
}
```

#### 学習コンテンツ取得
```http
GET /learning/content/{content_id}
```
**認証:** 必須

**パスパラメータ:**
- `content_id`: コンテンツID

**レスポンス (200 OK):**
```json
{
  "id": 1,
  "chapter_id": 1,
  "title": "言語モデルとは",
  "content_type": "markdown",
  "content_path": "/contents/chapter1/intro.md",
  "order_index": 1,
  "created_at": "2024-01-01T00:00:00Z"
}
```

#### 学習進捗取得
```http
GET /learning/progress
```
**認証:** 必須

**レスポンス (200 OK):**
```json
[
  {
    "id": 1,
    "user_id": 1,
    "content_id": 1,
    "status": "completed",
    "started_at": "2024-01-01T10:00:00Z",
    "completed_at": "2024-01-01T11:00:00Z",
    "last_accessed": "2024-01-01T11:00:00Z"
  }
]
```

#### 学習進捗更新
```http
PUT /learning/progress/{content_id}
```
**認証:** 必須

**パスパラメータ:**
- `content_id`: コンテンツID

**リクエストボディ:**
```json
{
  "status": "in_progress"
}
```

**レスポンス (200 OK):**
```json
{
  "id": 1,
  "user_id": 1,
  "content_id": 1,
  "status": "in_progress",
  "started_at": "2024-01-01T10:00:00Z",
  "completed_at": null,
  "last_accessed": "2024-01-01T12:00:00Z"
}
```

#### 提出物作成
```http
POST /learning/submissions
```
**認証:** 必須

**リクエストボディ:**
```json
{
  "content_type": "email",
  "title": "プロジェクト進捗報告",
  "content": "件名: プロジェクト進捗報告\n\nお疲れ様です。..."
}
```

**レスポンス (201 Created):**
```json
{
  "id": 1,
  "user_id": 1,
  "content_type": "email",
  "title": "プロジェクト進捗報告",
  "content": "件名: プロジェクト進捗報告\n\nお疲れ様です。...",
  "submitted_at": "2024-01-01T12:00:00Z"
}
```

#### AIフィードバック取得
```http
GET /learning/submissions/{submission_id}/feedback
```
**認証:** 必須

**パスパラメータ:**
- `submission_id`: 提出物ID

**レスポンス (200 OK):**
```json
{
  "id": 1,
  "submission_id": 1,
  "feedback_text": "ビジネスメールとしての評価：\n\n【良い点】\n- 件名が明確...",
  "generated_at": "2024-01-01T12:05:00Z"
}
```

#### 学習推奨取得
```http
GET /learning/recommendations
```
**認証:** 必須

**レスポンス (200 OK):**
```json
{
  "recommended_chapters": [
    {
      "id": 4,
      "title": "Text Classification",
      "description": "テキスト分類タスクとその応用を習得する",
      "order_index": 4,
      "created_at": "2024-01-01T00:00:00Z"
    }
  ],
  "personalized_message": "AI・機械学習の基礎から始めることをお勧めします。",
  "estimated_duration": "約6週間"
}
```

## 🚨 エラーレスポンス

### エラー形式
```json
{
  "detail": "エラーメッセージ"
}
```

### HTTPステータスコード

| コード | 説明 |
|--------|------|
| 200 | OK - 成功 |
| 201 | Created - 作成成功 |
| 400 | Bad Request - リクエストエラー |
| 401 | Unauthorized - 認証エラー |
| 403 | Forbidden - 権限エラー |
| 404 | Not Found - リソースが見つからない |
| 422 | Unprocessable Entity - バリデーションエラー |
| 500 | Internal Server Error - サーバーエラー |

### エラー例

#### 認証エラー
```json
{
  "detail": "認証情報が無効です"
}
```

#### バリデーションエラー
```json
{
  "detail": [
    {
      "loc": ["body", "email"],
      "msg": "field required",
      "type": "value_error.missing"
    }
  ]
}
```

## 📊 レート制限

- **一般API**: 1分間に60リクエスト
- **認証API**: 1分間に10リクエスト
- **AI分析API**: 1分間に5リクエスト

制限に達した場合、HTTP 429 Too Many Requestsが返されます。

## 🔧 開発者向け情報

### API文書
- **Swagger UI**: http://localhost:8000/docs
- **ReDoc**: http://localhost:8000/redoc

### テスト用データ
```bash
# テストユーザー
username: demo_user
password: demo_password

# テスト用OpenAI APIキー
OPENAI_API_KEY=test-api-key-placeholder
```

### SDKとライブラリ
- **JavaScript**: Axios推奨
- **Python**: requests推奨
- **cURL**: コマンドライン操作

### 例：JavaScript (Axios)
```javascript
import axios from 'axios';

const apiClient = axios.create({
  baseURL: 'http://localhost:8000/api/v1',
  headers: {
    'Content-Type': 'application/json',
  },
});

// 認証トークンを設定
apiClient.defaults.headers.common['Authorization'] = `Bearer ${token}`;

// ユーザー情報取得
const user = await apiClient.get('/auth/me');
```
