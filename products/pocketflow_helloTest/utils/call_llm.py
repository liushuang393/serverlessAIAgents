import os

from openai import OpenAI


def call_llm(prompt):
    # テスト用のモック応答（APIキーが設定されていない場合）
    base_url = os.getenv(
        "BASE_URL", "http://localhost:11434/v1"
    )  # "https://api.openai.com/v1"
    api_key = os.getenv("OPENAI_API_KEY", "dummy_key")
    if api_key == "YOUR_API_KEY_HERE":
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
