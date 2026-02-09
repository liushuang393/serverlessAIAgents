"""JSON ユーティリティ.

LLM レスポンスから JSON を安全に抽出するためのユーティリティ。
"""

from __future__ import annotations

import json
import re
from typing import Any


def extract_json(text: str) -> dict[str, Any] | None:
    """テキストから JSON を抽出.

    LLM レスポンスから JSON を安全に抽出します。
    以下のフォーマットに対応:
    - 純粋な JSON
    - Markdown コードブロック (```json ... ```)
    - テキストに埋め込まれた JSON

    Args:
        text: LLM レスポンスまたは JSON を含むテキスト

    Returns:
        パース済み dict、抽出失敗時は None

    Examples:
        >>> extract_json('{"key": "value"}')
        {'key': 'value'}
        >>> extract_json('```json\\n{"key": "value"}\\n```')
        {'key': 'value'}
        >>> extract_json('Here is the result: {"key": "value"} Thanks!')
        {'key': 'value'}
    """
    if not text or not text.strip():
        return None

    # 1. Markdown コードブロックから抽出を試みる
    code_block_pattern = r"```(?:json)?\s*([\s\S]*?)```"
    code_matches = re.findall(code_block_pattern, text)
    for match in code_matches:
        result = _try_parse_json(match.strip())
        if result is not None:
            return result

    # 2. 純粋な JSON としてパースを試みる
    result = _try_parse_json(text.strip())
    if result is not None:
        return result

    # 3. テキストから {} を見つけて抽出
    # 最も外側の {} を見つける（ネストに対応）
    json_str = _extract_outermost_json(text)
    if json_str:
        result = _try_parse_json(json_str)
        if result is not None:
            return result

    # 4. 単純な find/rfind での抽出（レガシー互換）
    start = text.find("{")
    end = text.rfind("}")
    if start != -1 and end != -1 and end > start:
        json_str = text[start : end + 1]
        result = _try_parse_json(json_str)
        if result is not None:
            return result

    return None


def _try_parse_json(text: str) -> dict[str, Any] | None:
    """JSON パースを試みる.

    不完全な JSON の場合は修復を試みる（max_tokens 不足対策）。
    """
    try:
        data = json.loads(text)
        if isinstance(data, dict):
            return data
        return None
    except json.JSONDecodeError:
        # 修復を試みる（トークン不足で途中で切れた場合）
        repaired = _try_repair_json(text)
        if repaired:
            try:
                data = json.loads(repaired)
                if isinstance(data, dict):
                    return data
            except json.JSONDecodeError:
                pass
        return None


def _try_repair_json(text: str) -> str | None:
    """不完全な JSON を修復.

    max_tokens 不足で JSON が途中で切れた場合の対策。
    - 閉じられていない文字列を閉じる
    - 閉じられていない括弧/配列を閉じる
    - 末尾のカンマを削除

    Args:
        text: 不完全な JSON テキスト

    Returns:
        修復済み JSON、修復不可能な場合は None
    """
    if not text or not text.strip():
        return None

    text = text.strip()

    # 末尾の不完全なキーやコンマを削除
    # 例: {"key": "value", "incomplete   →  {"key": "value"
    text = re.sub(r',\s*"[^"]*$', "", text)
    text = re.sub(r",\s*$", "", text)

    # 閉じられていない文字列を閉じる
    # 奇数個の " がある場合は最後に " を追加
    quote_count = 0
    i = 0
    while i < len(text):
        if text[i] == "\\" and i + 1 < len(text):
            i += 2
            continue
        if text[i] == '"':
            quote_count += 1
        i += 1

    if quote_count % 2 == 1:
        text += '"'

    # 括弧のバランスを確認
    open_braces = text.count("{") - text.count("}")
    open_brackets = text.count("[") - text.count("]")

    # 閉じ括弧を追加
    text += "]" * open_brackets
    text += "}" * open_braces

    return text if text else None


def _extract_outermost_json(text: str) -> str | None:
    """最も外側の JSON オブジェクトを抽出.

    ネストした {} に対応するため、括弧のバランスを追跡。
    """
    start = text.find("{")
    if start == -1:
        return None

    depth = 0
    in_string = False
    escape_next = False

    for i in range(start, len(text)):
        char = text[i]

        if escape_next:
            escape_next = False
            continue

        if char == "\\":
            escape_next = True
            continue

        if char == '"' and not escape_next:
            in_string = not in_string
            continue

        if in_string:
            continue

        if char == "{":
            depth += 1
        elif char == "}":
            depth -= 1
            if depth == 0:
                return text[start : i + 1]

    return None


__all__ = ["extract_json"]
