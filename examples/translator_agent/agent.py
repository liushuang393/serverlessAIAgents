"""Translator Agent - テキストを翻訳するエージェント."""

from __future__ import annotations

import random
from typing import Any, ClassVar

from agentflow.core.agent_block import AgentBlock


class TranslatorAgent(AgentBlock):
    """テキストを翻訳するエージェント.

    このエージェントは指定された言語にテキストを翻訳します。
    実際の翻訳 API の代わりに、デモ用のダミー翻訳を返します。
    """

    # サンプル翻訳辞書
    TRANSLATIONS: ClassVar[dict[tuple[str, str], dict[str, str]]] = {
        ("ja", "en"): {
            "こんにちは": "Hello",
            "ありがとう": "Thank you",
            "さようなら": "Goodbye",
        },
        ("en", "ja"): {
            "Hello": "こんにちは",
            "Thank you": "ありがとう",
            "Goodbye": "さようなら",
        },
        ("ja", "zh"): {
            "こんにちは": "你好",
            "ありがとう": "谢谢",
            "さようなら": "再见",
        },
    }

    async def initialize(self) -> None:
        """初期化処理."""
        await super().initialize()
        print("🌐 Translator Agent を初期化しました")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """テキストを翻訳.

        Args:
            input_data: 入力データ
                - text (str): 翻訳するテキスト
                - source_lang (str, optional): 元の言語コード
                - target_lang (str): 翻訳先の言語コード

        Returns:
            翻訳結果
                - translated_text (str): 翻訳されたテキスト
                - detected_lang (str): 検出された元の言語
                - confidence (float): 翻訳の信頼度
                - source_lang (str): 元の言語
                - target_lang (str): 翻訳先の言語

        Raises:
            ValueError: text または target_lang が指定されていない場合
        """
        text = input_data.get("text")
        if not text:
            msg = "text は必須です"
            raise ValueError(msg)

        target_lang = input_data.get("target_lang")
        if not target_lang:
            msg = "target_lang は必須です"
            raise ValueError(msg)

        source_lang = input_data.get("source_lang", "auto")

        # 言語を検出
        detected_lang = self._detect_language(text) if source_lang == "auto" else source_lang

        # 翻訳を実行
        translated_text = self._translate(text, detected_lang, target_lang)

        # 信頼度を生成
        confidence = round(random.uniform(0.85, 0.99), 2)

        return {
            "translated_text": translated_text,
            "detected_lang": detected_lang,
            "confidence": confidence,
            "source_lang": detected_lang,
            "target_lang": target_lang,
        }

    def _detect_language(self, text: str) -> str:
        """言語を検出.

        Args:
            text: テキスト

        Returns:
            言語コード
        """
        # Unicode範囲定数
        hiragana_start = 0x3040
        hiragana_end = 0x309F
        kanji_start = 0x4E00
        kanji_end = 0x9FFF

        # 簡易的な言語検出
        # 実際の実装では、langdetect などのライブラリを使用します
        if any(ord(c) >= hiragana_start and ord(c) <= hiragana_end for c in text):
            return "ja"  # ひらがな
        if any(ord(c) >= kanji_start and ord(c) <= kanji_end for c in text):
            return "zh"  # 漢字
        return "en"  # デフォルトは英語

    def _translate(self, text: str, source_lang: str, target_lang: str) -> str:
        """テキストを翻訳.

        Args:
            text: 翻訳するテキスト
            source_lang: 元の言語コード
            target_lang: 翻訳先の言語コード

        Returns:
            翻訳されたテキスト
        """
        # 辞書から翻訳を検索
        key = (source_lang, target_lang)
        if key in self.TRANSLATIONS and text in self.TRANSLATIONS[key]:
            return self.TRANSLATIONS[key][text]

        # 辞書にない場合は、ダミー翻訳を返す
        # 実際の実装では、Google Translate API などを使用します
        return f"[{target_lang}] {text}"

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        print("🧹 Translator Agent をクリーンアップしました")
        await super().cleanup()


# エージェントのエントリーポイント
if __name__ == "__main__":
    import asyncio

    async def main() -> None:
        """メイン関数."""
        async with TranslatorAgent(metadata_path="agent.yaml") as agent:
            # 日本語から英語に翻訳
            result1 = await agent.run(
                {
                    "text": "こんにちは",
                    "target_lang": "en",
                }
            )

            print("\n📝 元のテキスト: こんにちは")
            print(f"🌐 翻訳: {result1['translated_text']}")
            print(f"🔍 検出言語: {result1['detected_lang']}")
            print(f"📊 信頼度: {result1['confidence']}")

            # 英語から日本語に翻訳
            result2 = await agent.run(
                {
                    "text": "Thank you",
                    "source_lang": "en",
                    "target_lang": "ja",
                }
            )

            print("\n📝 元のテキスト: Thank you")
            print(f"🌐 翻訳: {result2['translated_text']}")
            print(f"📊 信頼度: {result2['confidence']}")

    asyncio.run(main())
