# backend/app/core/openai_client.py
# OpenAI APIクライアント - ユーザーの技能評価とフィードバック生成
import os
from typing import Dict, List, Any
from openai import OpenAI
from dotenv import load_dotenv
import json
import logging

# 環境変数を読み込み
load_dotenv()

# ログ設定
logger = logging.getLogger(__name__)

# OpenAIクライアントを初期化
client = OpenAI(
    api_key=os.getenv("OPENAI_API_KEY")
)

def analyze_user_answers(answers: Dict[str, Any]) -> Dict[str, Any]:
    """
    ユーザーの回答を分析して技能評価レポートを生成

    Args:
        answers: ユーザーの回答データ

    Returns:
        技能評価レポート（スコア、強み、弱み、推奨学習内容）
    """
    try:
        prompt = f"""
        以下のユーザーの回答を分析し、AI・機械学習分野での技能評価を行ってください。

        ユーザー回答データ:
        {json.dumps(answers, ensure_ascii=False, indent=2)}

        以下のJSON形式で回答してください：
        {{
            "overall_score": 85,
            "strengths": ["プロンプトエンジニアリング", "テキスト分類"],
            "weaknesses": ["ファインチューニング", "マルチモーダルモデル"],
            "recommendations": [
                {{
                    "chapter_id": 11,
                    "title": "Fine-tuning Representation Models",
                    "reason": "分類モデルの微調整技術を強化する必要があります"
                }},
                {{
                    "chapter_id": 9,
                    "title": "Multimodal Large Language Models",
                    "reason": "マルチモーダルモデルの理解を深めることを推奨します"
                }}
            ],
            "detailed_analysis": "ユーザーは基本的なAI概念を理解していますが..."
        }}
        """

        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "あなたはAI・機械学習分野の専門教育者です。ユーザーの技能を正確に評価し、適切な学習指導を提供してください。"},
                {"role": "user", "content": prompt}
            ],
            max_tokens=1000,
            temperature=0.3
        )

        # レスポンスをJSONとして解析
        result_text = response.choices[0].message.content.strip()

        # JSONの開始と終了を見つけて抽出
        start_idx = result_text.find('{')
        end_idx = result_text.rfind('}') + 1

        if start_idx != -1 and end_idx != -1:
            json_str = result_text[start_idx:end_idx]
            return json.loads(json_str)
        else:
            # JSONが見つからない場合のフォールバック
            return {
                "overall_score": 70,
                "strengths": ["基本概念理解"],
                "weaknesses": ["実践経験不足"],
                "recommendations": [],
                "detailed_analysis": result_text
            }

    except Exception as e:
        logger.error(f"技能分析中にエラーが発生: {e}")
        return {
            "overall_score": 50,
            "strengths": [],
            "weaknesses": ["評価エラー"],
            "recommendations": [],
            "detailed_analysis": "分析中にエラーが発生しました。後ほど再試行してください。"
        }

def generate_feedback(submission_content: str, content_type: str) -> str:
    """
    ユーザーの提出内容に対してAIフィードバックを生成

    Args:
        submission_content: ユーザーが提出した内容
        content_type: コンテンツタイプ（email, report, presentation）

    Returns:
        AIが生成したフィードバック
    """
    try:
        content_type_prompts = {
            "email": "ビジネスメールとして",
            "report": "技術レポートとして",
            "presentation": "プレゼンテーション資料として"
        }

        type_prompt = content_type_prompts.get(content_type, "文書として")

        prompt = f"""
        以下の{type_prompt}提出された内容を評価し、建設的なフィードバックを提供してください。

        提出内容:
        {submission_content}

        以下の観点から評価してください：
        1. 内容の明確性と論理性
        2. 構成と流れ
        3. 専門用語の適切な使用
        4. 改善提案

        日本語で丁寧にフィードバックを提供してください。
        """

        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "あなたは経験豊富な教育者です。学習者の成長を促す建設的で具体的なフィードバックを提供してください。"},
                {"role": "user", "content": prompt}
            ],
            max_tokens=800,
            temperature=0.5
        )

        return response.choices[0].message.content.strip()

    except Exception as e:
        logger.error(f"フィードバック生成中にエラーが発生: {e}")
        return "申し訳ございませんが、フィードバックの生成中にエラーが発生しました。後ほど再試行してください。"

def generate_learning_content(chapter_title: str, topic: str) -> str:
    """
    指定されたトピックの学習コンテンツを生成

    Args:
        chapter_title: チャプタータイトル
        topic: 具体的なトピック

    Returns:
        生成された学習コンテンツ（Markdown形式）
    """
    try:
        prompt = f"""
        「{chapter_title}」の章で「{topic}」について、初学者にも理解しやすい学習コンテンツを作成してください。

        以下の構成でMarkdown形式で作成してください：
        1. 概要説明
        2. 重要な概念の解説
        3. 具体例
        4. 実践的な応用例
        5. まとめ

        技術的に正確で、実践的な内容にしてください。
        """

        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "あなたはAI・機械学習分野の専門教育者です。初学者にも分かりやすく、実践的な学習コンテンツを作成してください。"},
                {"role": "user", "content": prompt}
            ],
            max_tokens=1500,
            temperature=0.4
        )

        return response.choices[0].message.content.strip()

    except Exception as e:
        logger.error(f"学習コンテンツ生成中にエラーが発生: {e}")
        return f"# {topic}\n\n申し訳ございませんが、コンテンツの生成中にエラーが発生しました。"