"""
LLMProvider - 改善されたLLM呼び出しライブラリ

官方の改善点を統合:
1. チャット履歴を処理する機能 - call_llm_with_messages()
2. メモリ内キャッシュ機能 - @lru_cache デコレータ使用、再試行対応
3. 詳細ログ機能 - プロンプト、応答、エラーの詳細ログ

主な機能:
- generate(): 基本的なLLM呼び出し（キャッシュ、ログ強化）
- call_llm_with_messages(): チャット履歴処理
- generate_anthropic(): Anthropic API対応（ログ強化）
- generate_google(): Google API対応
- generate_huggingface(): HuggingFace API対応

キャッシュ機能:
- 再試行時はキャッシュを使用しない設計
- use_cache=False または cur_retry>0 の場合はキャッシュをバイパス
"""

import logging
import os
from functools import lru_cache
from typing import Dict, List, Optional

from openai import OpenAI

# ログ設定
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# キャッシュ機能（再試行対応）
@lru_cache(maxsize=1000)
def _cached_call_llm(prompt: str, model_name: str, base_url: str, api_key: str) -> str:
    """
    キャッシュされたLLM呼び出し（内部使用）

    Args:
        prompt: プロンプト文字列
        model_name: モデル名
        base_url: APIベースURL
        api_key: APIキー

    Returns:
        str: LLMからの応答
    """
    logger.info(f"キャッシュされた呼び出し: プロンプト={prompt[:50]}...")

    # 実際のAPI呼び出し（エラーハンドリング強化）

    # 方法1: 基本的な初期化
    client = OpenAI(base_url=base_url, api_key=api_key)
    logger.info("OpenAI初期化成功")

    response = client.chat.completions.create(
        model=model_name,
        messages=[{"role": "user", "content": prompt}],
        temperature=0.3,
        max_tokens=5000,
    )

    content = response.choices[0].message.content
    return str(content) if content else ""


# Google API用キャッシュ機能
@lru_cache(maxsize=1000)
def _cached_call_google(prompt: str, model_name: str, api_key: str) -> str:
    """
    キャッシュされたGoogle LLM呼び出し（内部使用）

    Args:
        prompt: プロンプト文字列
        model_name: モデル名
        api_key: APIキー

    Returns:
        str: LLMからの応答
    """
    logger.info(f"Google キャッシュされた呼び出し: プロンプト={prompt[:50]}...")

    from google import genai

    # 新しいGoogle Gen AI SDKを使用してクライアントを初期化
    client = genai.Client(api_key=api_key)
    response = client.models.generate_content(model=model_name, contents=prompt)

    if response.text and len(response.text) > 0:
        return str(response.text)
    else:
        return ""


# HuggingFace用キャッシュ機能
@lru_cache(maxsize=1000)
def _cached_call_huggingface(prompt: str, model_name: str) -> str:
    """
    キャッシュされたHuggingFace LLM呼び出し（内部使用）

    Args:
        prompt: プロンプト文字列
        model_name: モデル名

    Returns:
        str: LLMからの応答
    """
    logger.info(f"HuggingFace キャッシュされた呼び出し: プロンプト={prompt[:50]}...")

    # vLLMライブラリの互換性問題を回避するため、直接Transformersを使用
    try:
        logger.info("HuggingFace Transformersライブラリを使用")
        import torch
        from transformers import AutoModelForCausalLM, AutoTokenizer

        # モデルとトークナイザーの初期化
        tokenizer = AutoTokenizer.from_pretrained(model_name)

        # パディングトークンが設定されていない場合は設定
        if tokenizer.pad_token is None:
            tokenizer.pad_token = tokenizer.eos_token

        # モデルの初期化（デバイス設定を統一）
        device = "cuda" if torch.cuda.is_available() else "cpu"
        model = AutoModelForCausalLM.from_pretrained(
            model_name,
            torch_dtype=torch.float16 if device == "cuda" else torch.float32,
            device_map="auto" if device == "cuda" else None,
        )

        logger.info(f"モデル {model_name} の初期化成功")

        # テキスト生成（デバイス統一）
        inputs = tokenizer.encode(prompt, return_tensors="pt")
        if device == "cuda":
            inputs = inputs.to(device)

        # attention_maskを明示的に設定
        attention_mask = torch.ones_like(inputs)

        with torch.no_grad():
            outputs = model.generate(
                inputs,
                attention_mask=attention_mask,
                max_length=min(inputs.shape[1] + 50, 100),
                num_return_sequences=1,
                temperature=0.7,
                do_sample=True,
                pad_token_id=tokenizer.eos_token_id,
            )

        # 結果のデコード（入力部分を除去）
        generated_text = tokenizer.decode(
            outputs[0][inputs.shape[1] :], skip_special_tokens=True
        )

        logger.info(f"HuggingFace生成完了: {len(generated_text)}文字")

        # メモリクリーンアップ
        del model, tokenizer
        if torch.cuda.is_available():
            torch.cuda.empty_cache()

        return str(generated_text.strip())

    except Exception as e:
        logger.error(f"HuggingFace Transformers処理エラー: {e}")
        # エラーを上位に投げる（外部呼び出し者が適切に処理できるように）
        raise Exception(f"HuggingFace Transformers処理エラー: {e}")


def call_llm_with_messages(
    messages: List[Dict[str, str]],
    base_url: Optional[str] = None,
    model_name: Optional[str] = None,
    use_cache: bool = True,
) -> str:
    """
    チャット履歴を処理するLLM呼び出し関数

    Args:
        messages: チャット履歴のメッセージリスト
                 例: [{"role": "user", "content": "Hello"}, {"role": "assistant", "content": "Hi!"}]
        base_url: APIのベースURL
        model_name: 使用するモデル名
        use_cache: キャッシュを使用するかどうか

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        Exception: API呼び出しでエラーが発生した場合
    """
    if not messages:
        raise ValueError("メッセージリストは必須です")

    # パラメータの設定
    base_url = base_url or os.getenv("BASE_URL") or "http://localhost:11434/v1"
    api_key = os.getenv("OPENAI_API_KEY") or "dummy_key"
    model_name = model_name or os.getenv("LLM_MODEL_NAME") or "gemma3n:latest"

    # ログ出力
    logger.info(f"チャット履歴処理開始: メッセージ数={len(messages)}")
    logger.info(f"使用モデル: {model_name}")
    logger.info(f"キャッシュ使用: {use_cache}")

    try:
        # メッセージリストを文字列に変換（キャッシュキー用）
        messages_str = str(messages)

        if use_cache:
            # キャッシュを使用
            result = _cached_call_llm(messages_str, model_name, base_url, api_key)
        else:
            # キャッシュを使用せず直接呼び出し
            result = _cached_call_llm.__wrapped__(
                messages_str, model_name, base_url, api_key
            )

        logger.info(f"チャット履歴処理完了: 応答長={len(result)}")
        return result

    except Exception as e:
        logger.error(f"チャット履歴処理エラー: {e}")
        raise Exception(f"チャット履歴処理でエラーが発生しました: {e}")


def generate(
    prompt: str,
    base_url: Optional[str] = None,
    model_name: Optional[str] = None,
    use_cache: bool = True,
    cur_retry: int = 0,
) -> str:
    """
    1. OpenAIのLLMを呼び出す関数、OpenAI APIを使用します。
    改善点：キャッシュ機能、詳細ログ、再試行対応を追加

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        base_url (Optional[str]): APIのベースURL（指定されない場合は環境変数から取得）
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）
        use_cache (bool): キャッシュを使用するかどうか（デフォルト: True）
        cur_retry (int): 現在の再試行回数（デフォルト: 0）

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        Exception: API呼び出しでExceptionエラーが発生した場合
    """

    if not prompt:
        raise ValueError("プロンプトは必須です")

    # パラメータの優先順位: 関数引数 > 環境変数 > デフォルト値
    base_url = base_url or os.getenv("BASE_URL") or "http://localhost:11434/v1"
    api_key = os.getenv("OPENAI_API_KEY") or "dummy_key"
    model_name = model_name or os.getenv("LLM_MODEL_NAME") or "gemma3n:latest"

    # 詳細ログ出力
    logger.info("=== LLM呼び出し開始 ===")
    logger.info(f"プロンプト: {prompt[:100]}{'...' if len(prompt) > 100 else ''}")
    logger.info(f"プロンプト長: {len(prompt)} 文字")
    logger.info(f"base_url: {base_url}")
    logger.info(f"model_name: {model_name}")
    logger.info(f"api_key: {'***' + api_key[-4:] if len(api_key) > 4 else 'dummy_key'}")
    logger.info(f"use_cache: {use_cache}")
    logger.info(f"cur_retry: {cur_retry}")

    try:
        # キャッシュ使用判定（再試行時はキャッシュを使用しない）
        should_use_cache = use_cache and cur_retry == 0

        if should_use_cache:
            logger.info("キャッシュを使用してLLM呼び出し")
            result = _cached_call_llm(prompt, model_name, base_url, api_key)
        else:
            logger.info("キャッシュを使用せずLLM呼び出し")
            result = _cached_call_llm.__wrapped__(prompt, model_name, base_url, api_key)

        # 応答ログ
        logger.info(f"応答: {result[:200]}{'...' if len(result) > 200 else ''}")
        logger.info(f"応答長: {len(result)} 文字")
        logger.info("=== LLM呼び出し完了 ===")

        return result

    except Exception as e:
        logger.error("=== LLM呼び出しエラー ===")
        logger.error(f"エラー詳細: {e}")
        logger.error(f"エラータイプ: {type(e).__name__}")
        raise Exception(f"予期しないエラーが発生しました: {e}")


def generate_anthropic(
    prompt: str,
    model_name: Optional[str] = None,
    use_cache: bool = True,
    cur_retry: int = 0,
) -> str:
    """
    2. AnthropicのLLMを呼び出す関数、Anthropic APIを使用します。
    改善点：詳細ログ、再試行対応を追加

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）
        use_cache (bool): キャッシュを使用するかどうか（デフォルト: True）
        cur_retry (int): 現在の再試行回数（デフォルト: 0）

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

    # 詳細ログ出力
    logger.info("=== Anthropic LLM呼び出し開始 ===")
    logger.info(f"プロンプト: {prompt[:100]}{'...' if len(prompt) > 100 else ''}")
    logger.info(f"プロンプト長: {len(prompt)} 文字")
    logger.info(f"model_name: {model_name}")
    logger.info(f"api_key: {'***' + api_key[-4:] if len(api_key) > 4 else 'dummy_key'}")
    logger.info(f"use_cache: {use_cache}")
    logger.info(f"cur_retry: {cur_retry}")

    try:
        client = Anthropic(api_key=api_key)
        logger.info(f"Anthropic API呼び出し中: {model_name}")

        r = client.messages.create(
            model=model_name,
            max_tokens=3000,
            messages=[{"role": "user", "content": prompt}],
        )

        if r.content and len(r.content) > 0 and hasattr(r.content[0], "text"):
            result = str(r.content[0].text)
            logger.info(f"応答: {result[:200]}{'...' if len(result) > 200 else ''}")
            logger.info(f"応答長: {len(result)} 文字")
            logger.info("=== Anthropic LLM呼び出し完了 ===")
            return result
        else:
            logger.info("Anthropic API呼び出し完了: 空の応答")
            logger.info("=== Anthropic LLM呼び出し完了 ===")
            return ""

    except Exception as e:
        logger.error("=== Anthropic LLM呼び出しエラー ===")
        logger.error(f"エラー詳細: {e}")
        logger.error(f"エラータイプ: {type(e).__name__}")
        raise Exception(f"Anthropicで予期しないエラーが発生しました: {e}")


def generate_google(
    prompt: str,
    model_name: Optional[str] = None,
    use_cache: bool = True,
    cur_retry: int = 0,
) -> str:
    """
    3. GoogleのLLMを呼び出す関数、Google APIを使用します。
    改善点：キャッシュ機能、詳細ログ、再試行対応を追加

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）
        use_cache (bool): キャッシュを使用するかどうか（デフォルト: True）
        cur_retry (int): 現在の再試行回数（デフォルト: 0）

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        Exception: API呼び出しでExceptionエラーが発生した場合
    """
    if not prompt:
        raise ValueError("メッセージは必須です")

    api_key = os.getenv("GOOGLE_API_KEY")
    if not api_key:
        raise ValueError("GOOGLE_API_KEY 環境変数が設定されていません。")

    model_name = model_name or os.getenv("LLM_MODEL_NAME") or "gemini-2.0-flash-001"

    # 詳細ログ出力
    logger.info("=== Google LLM呼び出し開始 ===")
    logger.info(f"プロンプト: {prompt[:100]}{'...' if len(prompt) > 100 else ''}")
    logger.info(f"プロンプト長: {len(prompt)} 文字")
    logger.info(f"model_name: {model_name}")
    logger.info(f"api_key: {'***' + api_key[-4:] if len(api_key) > 4 else 'dummy_key'}")
    logger.info(f"use_cache: {use_cache}")
    logger.info(f"cur_retry: {cur_retry}")

    try:
        # キャッシュ使用判定（再試行時はキャッシュを使用しない）
        should_use_cache = use_cache and cur_retry == 0

        if should_use_cache:
            logger.info("キャッシュを使用してGoogle LLM呼び出し")
            result = _cached_call_google(prompt, model_name, api_key)
        else:
            logger.info("キャッシュを使用せずGoogle LLM呼び出し")
            result = _cached_call_google.__wrapped__(prompt, model_name, api_key)

        # 応答ログ
        logger.info(f"応答: {result[:200]}{'...' if len(result) > 200 else ''}")
        logger.info(f"応答長: {len(result)} 文字")
        logger.info("=== Google LLM呼び出し完了 ===")

        return result

    except Exception as e:
        logger.error("=== Google LLM呼び出しエラー ===")
        logger.error(f"エラー詳細: {e}")
        logger.error(f"エラータイプ: {type(e).__name__}")
        raise Exception(f"Googleで予期しないエラーが発生しました: {e}")


def _check_model_config(model_name) -> int:
    """モデル設定を事前確認（改善版）"""
    try:
        from transformers import AutoConfig

        logger.info(f"モデル設定確認中: {model_name}")
        config = AutoConfig.from_pretrained(model_name)

        max_pos = getattr(config, "max_position_embeddings", None)
        model_max_len = getattr(config, "max_length", None)
        vocab_size = getattr(config, "vocab_size", None)

        logger.info(f"Model: {model_name}")
        logger.info(f"  max_position_embeddings: {max_pos}")
        logger.info(f"  model_max_length: {model_max_len}")
        logger.info(f"  vocab_size: {vocab_size}")

        # より安全なデフォルト値を返す
        if max_pos is not None and max_pos > 0:
            return min(int(max_pos), 512)  # 512に制限
        else:
            logger.warning("max_position_embeddingsが取得できません。デフォルト値512を使用")
            return 512

    except Exception as e:
        logger.error(f"モデル設定確認エラー: {e}")
        logger.info("デフォルト値512を使用")
        return 512


def generate_huggingface(
    prompt: str,
    model_name: Optional[str] = None,
    use_cache: bool = True,
    cur_retry: int = 0,
) -> str:
    """
    4. HuggingFaceのLLMを呼び出す関数、HuggingFace Inference APIを使用します。
    改善点：キャッシュ機能、詳細ログ、再試行対応を追加

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）
        use_cache (bool): キャッシュを使用するかどうか（デフォルト: True）
        cur_retry (int): 現在の再試行回数（デフォルト: 0）

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        Exception: API呼び出しでExceptionエラーが発生した場合
    """
    if not prompt:
        raise ValueError("メッセージは必須です")

    model_name = (
        model_name
        or os.getenv("LLM_MODEL_NAME")
        or "microsoft/DialoGPT-small"  # 対話型モデル（テスト用）
    )

    # DialoGPTは対話専用なので、単発質問の場合は対話形式に変換
    if "DialoGPT" in model_name and not prompt.startswith("User:"):
        prompt = f"User: {prompt}\nBot:"

    # 詳細ログ出力
    logger.info("=== HuggingFace LLM呼び出し開始 ===")
    logger.info(f"プロンプト: {prompt[:100]}{'...' if len(prompt) > 100 else ''}")
    logger.info(f"プロンプト長: {len(prompt)} 文字")
    logger.info(f"model_name: {model_name}")
    logger.info(f"use_cache: {use_cache}")
    logger.info(f"cur_retry: {cur_retry}")

    # モデル設定確認
    max_pos = _check_model_config(model_name)
    max_len = max(max_pos, 1024)
    logger.info(f"max_model_len: {max_len}")

    try:
        # キャッシュ使用判定（再試行時はキャッシュを使用しない）
        should_use_cache = use_cache and cur_retry == 0

        if should_use_cache:
            logger.info("キャッシュを使用してHuggingFace LLM呼び出し")
            result = _cached_call_huggingface(prompt, model_name)
        else:
            logger.info("キャッシュを使用せずHuggingFace LLM呼び出し")
            result = _cached_call_huggingface.__wrapped__(prompt, model_name)

        # 応答ログ
        logger.info(f"応答: {result[:200]}{'...' if len(result) > 200 else ''}")
        logger.info(f"応答長: {len(result)} 文字")
        logger.info("=== HuggingFace LLM呼び出し完了 ===")

        return result

    except Exception as e:
        logger.error("=== HuggingFace LLM呼び出しエラー ===")
        logger.error(f"エラー詳細: {e}")
        logger.error(f"エラータイプ: {type(e).__name__}")
        raise Exception(f"HuggingFaceで予期しないエラーが発生しました: {e}")


# ============================================================
# Simple Transformers 互換: Hugging Face pipeline 版
# ------------------------------------------------------------
# ● PROMPT と Model Name を渡すと生成テキストを返す
# ● 生成済みは LRU キャッシュで再利用
# ============================================================
@lru_cache(maxsize=1_000)
def cached_call_hf(prompt: str, model_name: str = "rinna/japanese-gpt2-small") -> str:
    """
    キャッシュ付き Hugging Face 生成パイプライン呼び出し

    Args:
        prompt (str): 生成用プロンプト
        model_name (str): HF Hub かローカルのモデル名/パス
    Returns:
        str: 生成テキスト（プロンプト部分は除去済み）
    Raises:
        RuntimeError: 生成失敗時の詳細
    """
    import os
    import traceback

    import torch
    from transformers import AutoModelForCausalLM, AutoTokenizer
    from transformers.pipelines import pipeline

    logger.info(f"[HF] プロンプト先頭: {prompt[:30]}... | model={model_name}")

    # ----- 1) デバイス選択 -----
    force_cpu = os.getenv("FORCE_CPU", "false").lower() == "true"
    cuda_available = torch.cuda.is_available() and not force_cpu
    device_str = "cuda:0" if cuda_available else "cpu"

    # ----- 2) パイプライン読み込み -----
    def _load_pipeline(on_device: str):
        logger.info(f"[HF] パイプラインを {on_device} で初期化...")

        # CUDAエラーを回避するため、CPUモードでは完全にCUDAを無効化
        original_cuda_visible = None
        if on_device == "cpu":
            # CPUモードでは環境変数を設定してCUDAを無効化
            original_cuda_visible = os.environ.get("CUDA_VISIBLE_DEVICES", None)
            os.environ["CUDA_VISIBLE_DEVICES"] = ""

        try:
            tok = AutoTokenizer.from_pretrained(model_name)
            if tok.pad_token_id is None:
                tok.pad_token_id = tok.eos_token_id

            # GPT‑2 系は pad_token なし → eos_token を兼用する
            if tok.pad_token_id is None:
                tok.pad_token_id = tok.eos_token_id

            # モデル読み込み時のデバイス指定を明確化
            if on_device == "cpu":
                mdl = AutoModelForCausalLM.from_pretrained(
                    model_name,
                    torch_dtype=torch.float32,  # CPUでは float32 を使用
                    device_map=None,  # CPUでは device_map を使用しない
                )
                # CPUに明示的に移動
                mdl = mdl.to("cpu")
                device_id = -1  # CPUを指定
            else:
                mdl = AutoModelForCausalLM.from_pretrained(
                    model_name,
                    torch_dtype=torch.float16,
                )
                mdl = mdl.to(on_device)
                device_id = 0  # GPUを指定

            return pipeline(
                "text-generation",
                model=mdl,
                tokenizer=tok,
                device=device_id,
                max_new_tokens=1024,
                pad_token_id=tok.pad_token_id,
            )
        except Exception as e:
            logger.error(f"[HF] パイプライン初期化エラー: {e}")
            # CPUモードの場合、環境変数を復元
            if on_device == "cpu" and original_cuda_visible is not None:
                if original_cuda_visible:
                    os.environ["CUDA_VISIBLE_DEVICES"] = original_cuda_visible
                elif "CUDA_VISIBLE_DEVICES" in os.environ:
                    del os.environ["CUDA_VISIBLE_DEVICES"]
            raise

    try:
        gen_pipe = _load_pipeline(device_str)
    except Exception:
        # GPU エラー時はワーニングを出して CPU で再ロード
        logger.warning(f"[HF] GPU 初期化失敗 → CPU フォールバック\n{traceback.format_exc(limit=1)}")
        # CUDA関連の環境変数をクリア
        os.environ["CUDA_VISIBLE_DEVICES"] = ""
        # キャッシュをクリア
        cached_call_hf.cache_clear()
        gen_pipe = _load_pipeline("cpu")

    # ----- 3) テキスト生成 -----
    try:
        # パイプラインの戻り値は List[Dict[str, Any]] 型
        # tokenizerがNoneでないことを確認してからpad_token_idにアクセス
        tokenizer = gen_pipe.tokenizer
        if tokenizer is None:
            raise RuntimeError("[HF] トークナイザーが初期化されていません")

        pad_token_id = tokenizer.pad_token_id
        if pad_token_id is None:
            pad_token_id = tokenizer.eos_token_id

        results = gen_pipe(
            prompt,
            max_new_tokens=50,
            do_sample=True,
            temperature=0.7,
            pad_token_id=pad_token_id,
        )

        # 結果がNoneでないことを確認
        if results is None:
            raise RuntimeError("[HF] 生成結果がNoneです")

        # Generatorの場合はリストに変換
        if hasattr(results, "__iter__") and not isinstance(results, (list, tuple)):
            try:
                results = list(results)
            except Exception as e:
                raise RuntimeError(f"[HF] 結果のリスト変換に失敗: {e}") from e

        # 結果の型と内容を確認
        if not isinstance(results, (list, tuple)):
            raise RuntimeError(f"[HF] 生成結果が配列型ではありません: {type(results)}")

        if len(results) == 0:
            raise RuntimeError("[HF] 生成結果が空です")

        # 最初の結果から生成テキストを取得（安全にアクセス）
        first_result = results[0]

        if not isinstance(first_result, dict) or "generated_text" not in first_result:
            raise RuntimeError("[HF] 生成結果の形式が不正です")

        # 型チェッカーのために明示的に型をアサート
        assert isinstance(first_result, dict), "[HF] 結果が辞書型ではありません"
        assert "generated_text" in first_result, "[HF] generated_textキーが存在しません"

        out: str = first_result["generated_text"]
    except Exception as e:
        raise RuntimeError(f"[HF] 生成失敗: {e}") from e

    # ----- 4) プロンプトを除去して返却 -----
    generated = out[len(prompt) :].strip()
    return generated or out.strip()


def generate_simple_transformers(
    prompt: str,
    model_name: Optional[str] = None,
    use_cache: bool = True,
    cur_retry: int = 0,
) -> str:
    """
    5. Simple TransformersのLLMを呼び出す関数、軽量なTransformers pipelineを使用します。
    改善点：キャッシュ機能、詳細ログ、再試行対応を追加

    Args:
        prompt (str): LLMに送信するプロンプト文字列
        model_name (Optional[str]): 使用するモデル名（指定されない場合はデフォルト値を使用）
        use_cache (bool): キャッシュを使用するかどうか（デフォルト: True）
        cur_retry (int): 現在の再試行回数（デフォルト: 0）

    Returns:
        str: LLMからの応答

    Raises:
        ValueError: 必須パラメータが不足している場合
        Exception: API呼び出しでExceptionエラーが発生した場合
    """
    if not prompt:
        raise ValueError("プロンプトは必須です")

    model_name = model_name or os.getenv("LLM_MODEL_NAME")

    # 詳細ログ出力
    logger.info("=== Simple Transformers LLM呼び出し開始 ===")
    logger.info(f"プロンプト: {prompt[:100]}{'...' if len(prompt) > 100 else ''}")
    logger.info(f"プロンプト長: {len(prompt)} 文字")
    logger.info(f"model_name: {model_name}")
    logger.info(f"use_cache: {use_cache}")
    logger.info(f"cur_retry: {cur_retry}")

    try:
        # キャッシュ使用判定（再試行時はキャッシュを使用しない）
        should_use_cache = use_cache and cur_retry == 0

        if should_use_cache:
            logger.info("キャッシュを使用してSimple Transformers LLM呼び出し")
            result = cached_call_hf(prompt, model_name)
        else:
            logger.info("キャッシュを使用せずSimple Transformers LLM呼び出し")
            result = cached_call_hf.__wrapped__(prompt, model_name)

        # 応答ログ
        logger.info(f"応答: {result[:200]}{'...' if len(result) > 200 else ''}")
        logger.info(f"応答長: {len(result)} 文字")
        logger.info("=== Simple Transformers LLM呼び出し完了 ===")

        return result

    except Exception as e:
        logger.error("=== Simple Transformers LLM呼び出しエラー ===")
        logger.error(f"エラー詳細: {e}")
        raise Exception(f"Simple Transformersで予期しないエラーが発生しました: {e}")


if __name__ == "__main__":
    # テスト用のサンプル実行
    test_prompt = "What is the meaning of life?"

    print("=== 基本的なLLM呼び出しテスト ===")
    try:
        result = generate(test_prompt)
        print(f"結果: {result}")
    except Exception as e:
        print(f"エラー: {e}")

    print("\n=== チャット履歴処理テスト ===")
    try:
        test_messages = [
            {"role": "user", "content": "Hello, how are you?"},
            {"role": "assistant", "content": "I'm doing well, thank you!"},
            {"role": "user", "content": "What's the weather like?"},
        ]
        result = call_llm_with_messages(test_messages)
        print(f"チャット履歴結果: {result}")
    except Exception as e:
        print(f"チャット履歴エラー: {e}")

    print("\n=== キャッシュテスト ===")
    try:
        # 同じプロンプトを2回呼び出してキャッシュ効果を確認
        print("1回目の呼び出し（キャッシュなし）:")
        result1 = generate(test_prompt, use_cache=True)
        print(f"結果1: {result1[:100]}...")

        print("2回目の呼び出し（キャッシュあり）:")
        result2 = generate(test_prompt, use_cache=True)
        print(f"結果2: {result2[:100]}...")

        print(f"結果が同じ: {result1 == result2}")
    except Exception as e:
        print(f"キャッシュテストエラー: {e}")

    print("\n=== 再試行テスト ===")
    try:
        # 再試行時はキャッシュを使用しない
        result3 = generate(test_prompt, use_cache=True, cur_retry=1)
        print(f"再試行結果: {result3[:100]}...")
    except Exception as e:
        print(f"再試行テストエラー: {e}")

    print("\n=== Google LLMテスト ===")
    try:
        result_google = generate_google(test_prompt, use_cache=True)
        print(f"Google結果: {result_google[:100]}...")
    except Exception as e:
        print(f"Google LLMテストエラー: {e}")

    print("\n=== HuggingFace LLMテスト ===")
    try:
        result_hf = generate_huggingface(test_prompt, use_cache=True)
        print(f"HuggingFace結果: {result_hf[:100]}...")
    except Exception as e:
        print(f"HuggingFace LLMテストエラー: {e}")

    print("\n=== Anthropic LLMテスト ===")
    try:
        result_anthropic = generate_anthropic(test_prompt, use_cache=True)
        print(f"Anthropic結果: {result_anthropic[:100]}...")
    except Exception as e:
        print(f"Anthropic LLMテストエラー: {e}")

    print("\n=== Simple Transformers LLMテスト ===")
    try:
        result_simple = generate_simple_transformers(test_prompt, use_cache=True)
        print(f"Simple Transformers結果: {result_simple[:100]}...")
    except Exception as e:
        print(f"Simple Transformers LLMテストエラー: {e}")

    print("\n=== 全LLMテスト完了 ===")
