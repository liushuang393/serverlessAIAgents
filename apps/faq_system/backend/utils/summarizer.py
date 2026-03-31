"""会話タイトルの要約ユーティリティ."""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from infrastructure.llm.providers import LLMProvider

logger = logging.getLogger(__name__)


async def summarize_to_title(text: str, llm: LLMProvider | None) -> str:
    """ユーザーの最初のメッセージを 20 文字程度のタイトルに要約する.

    Args:
        text: ユーザーメッセージ全文
        llm: LLM プロバイダーインスタンス

    Returns:
        要約されたタイトル
    """
    if not text.strip():
        return "新しいチャット"

    if llm is None:
        # LLM が提供されていない場合は単純な切り出し
        return _truncate_title(text)

    prompt = f"""以下のユーザーの質問を、チャット履歴のサイドバーに表示するのに適した、具体的で簡潔なタイトル（20文字以内）に要約してください。
タイトルのみを出力し、余計な説明や引用符は含めないでください。

質問: {text}
タイトル:"""

    try:
        response = await llm.generate(
            role="cheap",
            messages=[{"role": "user", "content": prompt}],
            temperature=0.3,
        )
        summary = str(response.get("content", "")).strip()
        # 引用符が入る場合があるので除去
        summary = summary.strip('"').strip("'").strip("「").strip("」")
        
        if not summary:
            return _truncate_title(text)
            
        # 万が一長すぎる場合は切り詰め
        if len(summary) > 22:
            return summary[:20] + "…"
            
        return summary
    except Exception as e:
        logger.warning(f"Failed to summarize title with LLM, falling back to truncation: {e}")
        return _truncate_title(text)


def _truncate_title(text: str, max_chars: int = 20) -> str:
    """テキストを単純に切り詰めるフォールバック."""
    normalized = text.strip().replace("\n", " ")
    if len(normalized) <= max_chars:
        return normalized
    return normalized[:max_chars] + "…"
