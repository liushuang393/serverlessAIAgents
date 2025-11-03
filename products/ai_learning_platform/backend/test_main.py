# backend/test_main.py
# テストスクリプト - 各機能の動作確認
import asyncio
import sys
import os
from pathlib import Path

# プロジェクトルートをパスに追加
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

from app.core.openai_client import analyze_user_answers, generate_feedback, generate_learning_content
from app.core.auth import get_password_hash, verify_password, create_access_token
from app.database import create_tables, get_db
from app.models.user import User
from app.models.skill_question import SkillQuestion
from app.models.chapter import Chapter

def test_password_hashing():
    """パスワードハッシュ化のテスト"""
    print("=== パスワードハッシュ化テスト ===")
    
    password = "test_password123"
    hashed = get_password_hash(password)
    
    print(f"元のパスワード: {password}")
    print(f"ハッシュ化後: {hashed}")
    print(f"検証結果: {verify_password(password, hashed)}")
    print(f"間違ったパスワードの検証: {verify_password('wrong_password', hashed)}")
    print()

def test_jwt_token():
    """JWTトークン生成のテスト"""
    print("=== JWTトークン生成テスト ===")
    
    user_data = {"sub": "test_user"}
    token = create_access_token(user_data)
    
    print(f"ユーザーデータ: {user_data}")
    print(f"生成されたトークン: {token}")
    print()

async def test_openai_functions():
    """OpenAI API機能のテスト"""
    print("=== OpenAI API機能テスト ===")

    # OpenAI APIキーが設定されているかチェック
    import os
    api_key = os.getenv("OPENAI_API_KEY")
    if not api_key or api_key == "test-api-key-placeholder":
        print("OpenAI APIキーが設定されていません。モック結果を表示します。")
        print()

        # モック結果を表示
        print("1. 技能分析テスト（モック）")
        mock_analysis = {
            "overall_score": 75,
            "strengths": ["プロンプトエンジニアリング", "テキスト分類"],
            "weaknesses": ["ファインチューニング", "マルチモーダルモデル"],
            "recommendations": [
                {
                    "chapter_id": 11,
                    "title": "Fine-tuning Representation Models",
                    "reason": "分類モデルの微調整技術を強化する必要があります"
                }
            ],
            "detailed_analysis": "基本的なAI概念を理解していますが、より高度な技術の学習が推奨されます。"
        }
        print(f"分析結果（モック）: {mock_analysis}")
        print()

        print("2. フィードバック生成テスト（モック）")
        mock_feedback = """
        ビジネスメールとしての評価：

        【良い点】
        - 件名が明確で内容が分かりやすい
        - 進捗状況が整理されて記載されている
        - 今後の予定が明記されている

        【改善提案】
        - 各機能の詳細な進捗率があるとより良い
        - 課題や懸念事項があれば記載することを推奨
        """
        print(f"フィードバック（モック）: {mock_feedback}")
        print()

        print("3. 学習コンテンツ生成テスト（モック）")
        mock_content = """
        # 言語モデルの基本概念

        ## 概要
        言語モデルは自然言語の確率分布を学習するモデルです。

        ## 重要な概念
        - トークン化
        - 確率分布
        - 文脈理解

        ## 具体例
        GPT、BERT、T5などが代表的な言語モデルです。
        """
        print(f"生成されたコンテンツ（モック）: {mock_content[:200]}...")
        print()
        return

    # 実際のOpenAI API機能のテスト
    print("実際のOpenAI APIを使用してテストします...")

    # 技能分析のテスト
    print("1. 技能分析テスト")
    sample_answers = {
        "user_id": 1,
        "username": "test_user",
        "basic_score": 75,
        "correct_count": 15,
        "total_count": 20,
        "answers": {
            "1": {
                "category": "Prompt Engineering",
                "question": "効果的なプロンプトの特徴は？",
                "selected_option": "A",
                "correct_answer": "A",
                "is_correct": True
            },
            "2": {
                "category": "Text Classification",
                "question": "テキスト分類の手法は？",
                "selected_option": "B",
                "correct_answer": "C",
                "is_correct": False
            }
        }
    }

    try:
        analysis_result = analyze_user_answers(sample_answers)
        print(f"分析結果: {analysis_result}")
    except Exception as e:
        print(f"技能分析エラー: {e}")

    print()

    # フィードバック生成のテスト
    print("2. フィードバック生成テスト")
    sample_content = """
    件名: プロジェクト進捗報告

    お疲れ様です。
    今週のプロジェクト進捗をご報告いたします。

    ・機能A: 実装完了
    ・機能B: テスト中
    ・機能C: 設計段階

    来週までに機能Bのテストを完了予定です。
    """

    try:
        feedback = generate_feedback(sample_content, "email")
        print(f"フィードバック: {feedback}")
    except Exception as e:
        print(f"フィードバック生成エラー: {e}")

    print()

    # 学習コンテンツ生成のテスト
    print("3. 学習コンテンツ生成テスト")
    try:
        content = generate_learning_content(
            "Introduction to Language Models",
            "言語モデルの基本概念"
        )
        print(f"生成されたコンテンツ: {content[:200]}...")
    except Exception as e:
        print(f"コンテンツ生成エラー: {e}")

    print()

def test_database_models():
    """データベースモデルのテスト"""
    print("=== データベースモデルテスト ===")
    
    try:
        # テーブル作成
        create_tables()
        print("データベーステーブルの作成が完了しました")
        
        # モデルのインスタンス化テスト
        user = User(
            username="test_user",
            email="test@example.com",
            hashed_password=get_password_hash("password123"),
            full_name="テストユーザー"
        )
        print(f"ユーザーモデル: {user}")
        
        question = SkillQuestion(
            category="Prompt Engineering",
            question="効果的なプロンプトの特徴は何ですか？",
            options='{"A": "明確で具体的", "B": "曖昧で抽象的", "C": "短すぎる", "D": "長すぎる"}',
            correct_answer="A",
            explanation="効果的なプロンプトは明確で具体的である必要があります。"
        )
        print(f"技能質問モデル: {question}")
        
        chapter = Chapter(
            title="Introduction to Language Models",
            description="言語モデルの基本概念について学習します",
            order_index=1
        )
        print(f"チャプターモデル: {chapter}")
        
    except Exception as e:
        print(f"データベースモデルテストエラー: {e}")
    
    print()

def test_api_endpoints():
    """APIエンドポイントのテスト（モック）"""
    print("=== APIエンドポイントテスト ===")
    
    # 実際のAPIテストは別途FastAPIのTestClientを使用
    print("APIエンドポイントのテストは別途実装が必要です")
    print("以下のエンドポイントが実装されています:")
    
    endpoints = [
        "POST /api/v1/auth/register - ユーザー登録",
        "POST /api/v1/auth/login - ユーザーログイン",
        "GET /api/v1/auth/me - 現在のユーザー情報取得",
        "GET /api/v1/skill-assessment/categories - 技能カテゴリ一覧",
        "GET /api/v1/skill-assessment/questions/{category} - カテゴリ別問題取得",
        "POST /api/v1/skill-assessment/submit - 回答提出",
        "GET /api/v1/learning/chapters - チャプター一覧",
        "GET /api/v1/learning/chapters/{chapter_id} - チャプター詳細",
        "GET /api/v1/learning/progress - 学習進捗取得",
        "PUT /api/v1/learning/progress/{content_id} - 進捗更新",
    ]
    
    for endpoint in endpoints:
        print(f"  - {endpoint}")
    
    print()

async def main():
    """メインテスト実行"""
    print("AI学習プラットフォーム - 機能テスト開始")
    print("=" * 50)
    
    # 各テストを実行
    test_password_hashing()
    test_jwt_token()
    await test_openai_functions()
    test_database_models()
    test_api_endpoints()
    
    print("=" * 50)
    print("全てのテストが完了しました")
    print()
    print("次のステップ:")
    print("1. 環境変数(.env)ファイルを設定してください")
    print("2. MySQLデータベースを起動してください")
    print("3. OpenAI API Keyを設定してください")
    print("4. `uvicorn app.main:app --reload` でサーバーを起動してください")
    print("5. フロントエンドは `npm start` で起動してください")

if __name__ == "__main__":
    asyncio.run(main())
