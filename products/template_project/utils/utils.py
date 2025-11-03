import os
from openai import OpenAI

def call_llm(prompt):
    client = OpenAI(base_url=os.environ.get("BASE_URL", "http://localhost:11434/v1"), api_key=os.environ.get("OPENAI_API_KEY", "your-api-key"))
    r = client.chat.completions.create(
        model="gemma3n",
        messages=[{"role": "user", "content": prompt}]
    )
    return r.choices[0].message.content

# Example usage
if __name__ == "__main__":
    print(call_llm("Tell me a short joke")) 