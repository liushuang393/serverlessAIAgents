import os

from openai import OpenAI


def call_llm(prompt):
    """
    LLMを呼び出す関数

    環境変数の優先順位:
    1. OPENAI_API_KEY (直接指定)
    2. AI_BLOCKS_OPENAI_API_KEY (プロジェクト標準)
    3. dummy_key (デフォルト)
    """
    # BASE_URLの取得（複数の環境変数をチェック）
    base_url = (
        os.getenv("BASE_URL")
        or os.getenv("AI_BLOCKS_BASE_URL")
        or "http://localhost:11434/v1"
    )

    # API キーの取得（複数の環境変数をチェック）
    api_key = (
        os.getenv("OPENAI_API_KEY")
        or os.getenv("AI_BLOCKS_OPENAI_API_KEY")
        or "dummy_key"
    )

    # デバッグ情報を出力
    print(f"DEBUG: base_url = {base_url}")
    print(
        f"DEBUG: api_key = {'***' + api_key[-4:] if len(api_key) > 4 else 'dummy_key'}"
    )

    # モック応答の条件をチェック
    if api_key in ["YOUR_API_KEY_HERE", "dummy_key"]:
        # モック応答を返す
        return f"モック応答: '{prompt}' に対する回答です。宇宙の終わりは熱的死と考えられています。"

    # 実際のAPIを呼び出す
    client = OpenAI(base_url=base_url, api_key=api_key)
    r = client.chat.completions.create(
        model="gemma3n:latest",  # "gpt-4o",
        messages=[{"role": "user", "content": prompt}],
    )
    return r.choices[0].message.content


if __name__ == "__main__":
    prompt = "What is the meaning of life?"
    print(call_llm(prompt))
