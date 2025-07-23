import logging
import os
from typing import Optional

from openai import OpenAI

# ログ設定
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def generate(
    prompt: str, base_url: Optional[str] = None, model_name: Optional[str] = None
) -> str:
    """
    1. OpenAIのLLMを呼び出す関数、OpenAI APIを使用します。

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        base_url (Optional[str]): APIのベースURL（指定されない場合は環境変数から取得）
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        APIError: API呼び出しでExceptionエラーが発生した場合
    """

    if not prompt:
        raise ValueError("プロンプトは必須です")

    # 文字列をメッセージリストに変換
    from openai.types.chat import ChatCompletionUserMessageParam

    formatted_messages = [ChatCompletionUserMessageParam(role="user", content=prompt)]

    # パラメータの優先順位: 関数引数 > 環境変数 > デフォルト値
    base_url = base_url or os.getenv("BASE_URL") or "http://localhost:11434/v1"

    api_key = os.getenv("OPENAI_API_KEY") or "dummy_key"

    model_name = model_name or os.getenv("LLM_MODEL_NAME") or "gemma3n:latest"

    # デバッグ情報を出力
    logger.info(f"DEBUG: base_url = {base_url}")
    logger.info(f"DEBUG: model_name = {model_name}")
    logger.info(
        f"DEBUG: api_key = {'***' + api_key[-4:] if len(api_key) > 4 else 'dummy_key'}"
    )

    try:
        # 実際のAPIを呼び出す
        client = OpenAI(base_url=base_url, api_key=api_key)

        logger.info(f"API呼び出し中: {model_name}")
        response = client.chat.completions.create(
            model=model_name,
            messages=formatted_messages,
            temperature=0.3,  # 創造性のバランス
            max_tokens=5000,  # 最大トークン数
        )

        content = response.choices[0].message.content
        if content is None:
            content = ""

        logger.info(f"API呼び出し完了: {content}")
        return str(content)

    except Exception as e:
        logger.error(f"予期しないエラー: {e}")
        raise Exception(f"予期しないエラーが発生しました: {e}")


def generate_anthropic(prompt: str, model_name: Optional[str] = None) -> str:
    """
    2. AnthropicのLLMを呼び出す関数、Anthropic APIを使用します。

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        Exception: API呼び出しでExceptionエラーが発生した場合
    """
    from anthropic import Anthropic

    if not prompt:
        raise ValueError("メッセージは必須です")

    api_key = os.getenv("ANTHROPIC_API_KEY")
    if not api_key:
        raise ValueError("ANTHROPIC_API_KEY 環境変数が設定されていません。")

    model_name = (
        model_name or os.getenv("LLM_MODEL_NAME") or "claude-3-7-sonnet-20250219"
    )

    # デバッグ情報を出力
    logger.info(f"DEBUG: Anthropic model_name = {model_name}")
    logger.info(
        f"DEBUG: Anthropic api_key = {'***' + api_key[-4:] if len(api_key) > 4 else 'dummy_key'}"
    )

    try:
        client = Anthropic(api_key=api_key)
        logger.info(f"Anthropic API呼び出し中: {model_name}")
        r = client.messages.create(
            model=model_name,
            max_tokens=3000,
            messages=[{"role": "user", "content": prompt}],
        )
        if r.content and len(r.content) > 0 and hasattr(r.content[0], "text"):
            logger.info(f"Anthropic API呼び出し完了: {r.content[0].text}")
            return str(r.content[0].text)
        else:
            logger.info("Anthropic API呼び出し完了: 空の応答")
            return ""

    except Exception as e:
        logger.error(f"Anthropicで予期しないエラー: {e}")
        raise Exception(f"Anthropicで予期しないエラーが発生しました: {e}")


def generate_google(prompt: str, model_name: Optional[str] = None) -> str:
    """
    3. googleのLLMを呼び出す関数、google APIを使用します。

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        Exception: API呼び出しでExceptionエラーが発生した場合
    """
    from google import genai

    if not prompt:
        raise ValueError("メッセージは必須です")

    api_key = os.getenv("GOOGLE_API_KEY")
    if not api_key:
        raise ValueError("GOOGLE_API_KEY 環境変数が設定されていません。")

    model_name = model_name or os.getenv("LLM_MODEL_NAME") or "gemini-2.0-flash-001"

    # デバッグ情報を出力
    logger.info(f"DEBUG: google model_name = {model_name}")
    logger.info(
        f"DEBUG: google api_key = {'***' + api_key[-4:] if len(api_key) > 4 else 'dummy_key'}"
    )

    try:
        logger.info(f"google API呼び出し中: {model_name}")

        client = genai.Client(api_key="GEMINI_API_KEY")
        response = client.models.generate_content(model=model_name, contents=prompt)

        if response.text and len(response.text) > 0:
            logger.info(f"google API呼び出し完了: {response.text}")
            return str(response.text)
        else:
            logger.info("google API呼び出し完了: 空の応答")
            return ""

    except Exception as e:
        logger.error(f"googleで予期しないエラー: {e}")
        raise Exception(f"googleで予期しないエラーが発生しました: {e}")


def _check_model_config(model_name) -> int:
    """モデル設定を事前確認"""
    from transformers import AutoConfig

    try:
        config = AutoConfig.from_pretrained(model_name)
        max_pos = getattr(config, "max_position_embeddings", None)
        model_max_len = getattr(config, "max_length", None)
        print(f"Model: {model_name}")
        print(f"  max_position_embeddings: {max_pos}")
        print(f"  model_max_length: {model_max_len}")
        return int(max_pos) if max_pos is not None else 1024
    except Exception as e:
        print(f"設定確認エラー: {e}")
        return 1024


def generate_huggingface(prompt: str, model_name: Optional[str] = None) -> str:
    """
    4. HuggingFaceのLLMを呼び出す関数、HuggingFace Inference APIを使用します。

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        Exception: API呼び出しでExceptionエラーが発生した場合
    """
    import torch
    from vllm import LLM, SamplingParams

    if not prompt:
        raise ValueError("メッセージは必須です")

    model_name = (
        model_name or os.getenv("LLM_MODEL_NAME") or "microsoft/DialoGPT-medium"
    )

    # デバッグ情報を出力
    logger.info(f"DEBUG: huggingface model_name = {model_name}")
    logger.info(f"huggingface API呼び出し中: {model_name}")

    # モデル設定確認
    max_pos = _check_model_config(model_name)
    max_len = max(max_pos, 1024)

    try:
        # vLLMモデル初期化
        llm = LLM(
            model=model_name,
            trust_remote_code=True,
            max_model_len=max_len,
            max_num_seqs=1,
            gpu_memory_utilization=1024,  # GPU使用率を下げる
            enforce_eager=True,  # メモリ効率を改善
        )
        print(f"✅ {model_name} ロード成功")

        # テキスト生成テスト
        prompts = ["Hello, how are you?"]
        sampling_params = SamplingParams(temperature=0.2, max_tokens=32, top_p=0.9)

        outputs = llm.generate(prompts, sampling_params)
        result = outputs[0].outputs[0].text
        print(f"✅ テキスト生成成功: {result.strip()}")

        # GPU情報確認
        if torch.cuda.is_available():
            gpu_memory = torch.cuda.get_device_properties(0).total_memory / 1024**3
            print(f"GPU Memory: {gpu_memory:.1f} GB")
        else:
            print("CUDA not available")
        # メモリクリーンアップ
        del llm
        torch.cuda.empty_cache()

        print(f"\n結果: {result} ")
        return str(result)

    except Exception as e:
        logger.error(f"huggingfaceで予期しないエラー: {e}")
        raise Exception(f"huggingfaceで予期しないエラーが発生しました: {e}")


if __name__ == "__main__":
    # テスト用のサンプル実行
    test_prompt = "What is the meaning of life?"
    try:
        result = generate(test_prompt)
        print(f"結果: {result}")
    except Exception as e:
        print(f"エラー: {e}")
