"""ドキュメント操作スキル.

ドキュメントの作成、要約、書き換え、キーポイント抽出、翻訳を提供。

使用例:
    >>> skill = DocumentSkill()
    >>> result = await skill.create("週次レポート", "営業成績をまとめて", format="md")
    >>> summary = await skill.summarize("long_document.txt", style="executive")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any

from agentflow.providers import get_llm


class DocumentFormat(str, Enum):
    """ドキュメント形式."""

    MARKDOWN = "md"
    PLAIN_TEXT = "txt"
    HTML = "html"


class SummaryStyle(str, Enum):
    """要約スタイル."""

    EXECUTIVE = "executive"  # 経営層向け簡潔サマリー
    DETAILED = "detailed"  # 詳細な要約
    BULLET_POINTS = "bullet_points"  # 箇条書き
    ABSTRACT = "abstract"  # 論文風アブストラクト


@dataclass
class DocumentResult:
    """ドキュメント作成結果.

    Attributes:
        title: タイトル
        content: コンテンツ
        format: 形式
        word_count: 単語数
        created_at: 作成日時
        metadata: メタデータ
    """

    title: str
    content: str
    format: DocumentFormat
    word_count: int
    created_at: datetime
    metadata: dict[str, Any]

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "title": self.title,
            "content": self.content,
            "format": self.format.value,
            "word_count": self.word_count,
            "created_at": self.created_at.isoformat(),
            "metadata": self.metadata,
        }


@dataclass
class SummaryResult:
    """要約結果.

    Attributes:
        summary: 要約テキスト
        style: 要約スタイル
        original_length: 元のテキスト長
        summary_length: 要約テキスト長
        compression_ratio: 圧縮率
        key_points: キーポイント
    """

    summary: str
    style: SummaryStyle
    original_length: int
    summary_length: int
    compression_ratio: float
    key_points: list[str]

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "summary": self.summary,
            "style": self.style.value,
            "original_length": self.original_length,
            "summary_length": self.summary_length,
            "compression_ratio": round(self.compression_ratio, 2),
            "key_points": self.key_points,
        }


@dataclass
class RewriteResult:
    """書き換え結果.

    Attributes:
        original: 元のテキスト
        rewritten: 書き換え後テキスト
        instruction: 指示内容
        changes_made: 変更点の説明
    """

    original: str
    rewritten: str
    instruction: str
    changes_made: list[str]

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "original": self.original[:200] + "..." if len(self.original) > 200 else self.original,
            "rewritten": self.rewritten,
            "instruction": self.instruction,
            "changes_made": self.changes_made,
        }


@dataclass
class TranslationResult:
    """翻訳結果.

    Attributes:
        original: 元のテキスト
        translated: 翻訳後テキスト
        source_language: 元の言語
        target_language: 翻訳先言語
    """

    original: str
    translated: str
    source_language: str
    target_language: str

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "original": self.original[:200] + "..." if len(self.original) > 200 else self.original,
            "translated": self.translated,
            "source_language": self.source_language,
            "target_language": self.target_language,
        }


class DocumentSkill:
    """ドキュメント操作スキル.

    LLMを使用してドキュメントの作成、要約、書き換え、翻訳を行う。
    """

    def __init__(self, temperature: float = 0.7) -> None:
        """初期化.

        Args:
            temperature: LLM温度パラメータ
        """
        self._temperature = temperature
        self._logger = logging.getLogger(__name__)

    async def create(
        self,
        title: str,
        content_prompt: str,
        format: str = "md",
        context: str | None = None,
        max_length: int | None = None,
    ) -> DocumentResult:
        """ドキュメントを作成.

        Args:
            title: タイトル
            content_prompt: コンテンツ生成の指示
            format: 出力形式（md/txt/html）
            context: 追加コンテキスト
            max_length: 最大文字数

        Returns:
            ドキュメント作成結果
        """
        doc_format = (
            DocumentFormat(format)
            if format in [f.value for f in DocumentFormat]
            else DocumentFormat.MARKDOWN
        )

        system_prompt = f"""あなたは優秀なドキュメント作成者です。
指示に従って{doc_format.value}形式のドキュメントを作成してください。

出力形式の要件:
- markdown: 適切な見出し、箇条書き、強調を使用
- txt: プレーンテキストで読みやすく
- html: 適切なHTMLタグを使用

{"最大文字数: " + str(max_length) + "文字以内" if max_length else ""}
"""

        user_prompt = f"""タイトル: {title}

指示: {content_prompt}

{"参考情報: " + context if context else ""}

上記に基づいてドキュメントを作成してください。"""

        llm = get_llm(temperature=self._temperature)
        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": user_prompt},
            ]
        )

        content = response.get("content", "")

        self._logger.info("ドキュメント作成: title=%s, format=%s", title, doc_format.value)

        return DocumentResult(
            title=title,
            content=content,
            format=doc_format,
            word_count=len(content.split()),
            created_at=datetime.now(),
            metadata={"prompt": content_prompt},
        )

    async def summarize(
        self,
        source: str,
        style: str = "executive",
        max_length: int | None = None,
    ) -> SummaryResult:
        """テキストを要約.

        Args:
            source: 要約対象テキスト（またはファイルパス）
            style: 要約スタイル
            max_length: 最大文字数

        Returns:
            要約結果
        """
        summary_style = (
            SummaryStyle(style)
            if style in [s.value for s in SummaryStyle]
            else SummaryStyle.EXECUTIVE
        )

        # ファイルパスの場合は読み込み
        text = source
        if len(source) < 500 and Path(source).exists():
            text = Path(source).read_text(encoding="utf-8")

        style_instructions = {
            SummaryStyle.EXECUTIVE: "経営層向けの簡潔な要約を作成。重要な数値や意思決定に必要な情報を優先。",
            SummaryStyle.DETAILED: "詳細な要約を作成。主要なポイントを網羅的に含める。",
            SummaryStyle.BULLET_POINTS: "箇条書き形式で要点をまとめる。",
            SummaryStyle.ABSTRACT: "論文のアブストラクト形式で要約。目的、方法、結果、結論を含める。",
        }

        system_prompt = f"""あなたは優秀な要約エキスパートです。
{style_instructions[summary_style]}

{"最大文字数: " + str(max_length) + "文字以内" if max_length else ""}

要約の後に、JSON形式でキーポイントも出力してください:
```json
{{"key_points": ["ポイント1", "ポイント2", ...]}}
```"""

        llm = get_llm(temperature=0.3)  # 要約は低温度で
        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": f"以下を要約してください:\n\n{text}"},
            ]
        )

        content = response.get("content", "")

        # キーポイントを抽出
        key_points = []
        import json

        if "```json" in content:
            try:
                json_part = content.split("```json")[1].split("```")[0]
                data = json.loads(json_part)
                key_points = data.get("key_points", [])
                content = content.split("```json")[0].strip()
            except (json.JSONDecodeError, IndexError):
                pass

        self._logger.info("要約完了: style=%s, original=%d chars", style, len(text))

        return SummaryResult(
            summary=content,
            style=summary_style,
            original_length=len(text),
            summary_length=len(content),
            compression_ratio=len(content) / max(len(text), 1),
            key_points=key_points,
        )

    async def rewrite(
        self,
        source: str,
        instruction: str,
    ) -> RewriteResult:
        """テキストを書き換え.

        Args:
            source: 書き換え対象テキスト
            instruction: 書き換えの指示

        Returns:
            書き換え結果
        """
        system_prompt = """あなたは優秀な編集者です。
指示に従ってテキストを書き換えてください。

書き換え後、変更点を箇条書きで説明してください。
形式:
---
[書き換え後のテキスト]
---
変更点:
- 変更1
- 変更2
"""

        llm = get_llm(temperature=self._temperature)
        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": f"指示: {instruction}\n\n元のテキスト:\n{source}"},
            ]
        )

        content = response.get("content", "")

        # 書き換え結果と変更点を分離
        rewritten = content
        changes = []

        if "変更点:" in content:
            parts = content.split("変更点:")
            rewritten = parts[0].strip().strip("-").strip()
            changes_text = parts[1] if len(parts) > 1 else ""
            changes = [
                c.strip().lstrip("-").strip()
                for c in changes_text.split("\n")
                if c.strip().lstrip("-").strip()
            ]

        self._logger.info("書き換え完了: instruction=%s", instruction[:50])

        return RewriteResult(
            original=source,
            rewritten=rewritten,
            instruction=instruction,
            changes_made=changes,
        )

    async def extract_key_points(
        self,
        source: str,
        max_points: int = 10,
    ) -> list[str]:
        """キーポイントを抽出.

        Args:
            source: 抽出対象テキスト
            max_points: 最大ポイント数

        Returns:
            キーポイントリスト
        """
        system_prompt = f"""テキストから最も重要なポイントを{max_points}個以内で抽出してください。
各ポイントは1行で簡潔に記述してください。
番号付きリストで出力してください（例: 1. ポイント）"""

        llm = get_llm(temperature=0.3)
        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": source},
            ]
        )

        content = response.get("content", "")

        # 番号付きリストをパース
        points = []
        for line in content.split("\n"):
            line = line.strip()
            if line and (line[0].isdigit() or line.startswith("-")):
                # 番号や記号を除去
                point = line.lstrip("0123456789.-) ").strip()
                if point:
                    points.append(point)

        self._logger.info("キーポイント抽出: %d points", len(points))

        return points[:max_points]

    async def translate(
        self,
        source: str,
        target_language: str,
        source_language: str = "auto",
    ) -> TranslationResult:
        """テキストを翻訳.

        Args:
            source: 翻訳対象テキスト
            target_language: 翻訳先言語
            source_language: 元の言語（autoで自動検出）

        Returns:
            翻訳結果
        """
        language_names = {
            "ja": "日本語",
            "en": "英語",
            "zh": "中国語",
            "ko": "韓国語",
            "fr": "フランス語",
            "de": "ドイツ語",
            "es": "スペイン語",
            "auto": "自動検出",
        }

        target_name = language_names.get(target_language, target_language)
        source_name = language_names.get(source_language, source_language)

        system_prompt = f"""あなたは優秀な翻訳者です。
{"元の言語を自動検出し、" if source_language == "auto" else f"{source_name}から"}{target_name}に翻訳してください。

以下の点に注意:
- 意味を正確に伝える
- 自然な表現を使用
- 専門用語は適切に翻訳
- 文化的なニュアンスを考慮

翻訳結果のみを出力してください。"""

        llm = get_llm(temperature=0.3)
        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": source},
            ]
        )

        translated = response.get("content", "")

        self._logger.info("翻訳完了: %s -> %s", source_language, target_language)

        return TranslationResult(
            original=source,
            translated=translated,
            source_language=source_language,
            target_language=target_language,
        )

    async def compare(
        self,
        text1: str,
        text2: str,
    ) -> dict[str, Any]:
        """2つのテキストを比較.

        Args:
            text1: テキスト1
            text2: テキスト2

        Returns:
            比較結果
        """
        system_prompt = """2つのテキストを比較し、以下を分析してください:
1. 主な相違点
2. 共通点
3. どちらがより適切か（該当する場合）

JSON形式で出力:
{
    "differences": ["相違点1", "相違点2"],
    "similarities": ["共通点1", "共通点2"],
    "recommendation": "推奨テキストと理由"
}"""

        llm = get_llm(temperature=0.3)
        response = await llm.chat(
            [
                {"role": "system", "content": system_prompt},
                {"role": "user", "content": f"テキスト1:\n{text1}\n\nテキスト2:\n{text2}"},
            ]
        )

        content = response.get("content", "")

        # JSONを抽出
        import json

        try:
            if "```json" in content:
                content = content.split("```json")[1].split("```")[0]
            result = json.loads(content.strip())
        except json.JSONDecodeError:
            result = {
                "differences": [],
                "similarities": [],
                "recommendation": content,
            }

        self._logger.info("テキスト比較完了")

        return result
