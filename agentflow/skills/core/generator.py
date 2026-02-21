"""Skill 生成器 - 自動進化の核心コンポーネント.

このモジュールは新しい Skill を自動生成する機能を提供します：
- LLM を使用した Skill 生成
- SKILL.md フォーマット出力
- 品質検証とリトライ

これにより、システムが「知らない」要求に対して自動的に学習できます。

設計原則：
- 松耦合：LLM プロバイダーを意識しない
"""

import logging
import re
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any

from agentflow.providers import get_llm
from agentflow.skills.core.base import Skill, SkillMetadata


if TYPE_CHECKING:
    from agentflow.providers.llm_provider import LLMProvider


@dataclass
class GenerationResult:
    """生成結果.

    Attributes:
        skill: 生成された Skill（失敗時 None）
        success: 成功したか
        error: エラーメッセージ
        attempts: 試行回数
    """

    skill: Skill | None
    success: bool
    error: str = ""
    attempts: int = 1


# Skill 生成用プロンプトテンプレート
SKILL_GENERATION_PROMPT = """あなたは Skill 生成の専門家です。
ユーザーの要求に基づいて、Claude Code Skills 形式の SKILL.md を生成してください。

## ユーザー要求
{user_request}

## 出力形式
以下の SKILL.md 形式で出力してください：

```markdown
---
name: skill-name-kebab-case
description: 具体的な説明。何ができるか、いつ使うべきかを明記。
version: 1.0.0
triggers:
  - トリガーワード1
  - トリガーワード2
requirements:
  - 必要なパッケージ（あれば）
tags:
  - カテゴリタグ
---

# 指示内容

ここに具体的な実行手順を書く。
コード例があればコードブロックで示す。
```

## 注意事項
- name は kebab-case で簡潔に
- description は「何ができるか」+「いつ使うか」を含める
- triggers はユーザーが使いそうなキーワード
- 指示内容は具体的で実行可能なものに

SKILL.md のみを出力してください。説明文は不要です。
"""


class SkillGenerator:
    """Skill 生成器 - LLM を使用して新しい Skill を自動生成（松耦合設計）.

    LLM プロバイダー/モデルは環境変数から自動検出されます。

    Example:
        >>> generator = SkillGenerator()
        >>> result = await generator.generate("PDFからテキストを抽出する方法")
        >>> if result.success:
        ...     print(f"Generated: {result.skill.name}")
    """

    # SKILL.md 抽出用パターン
    _MD_PATTERN = re.compile(r"```markdown\s*\n(---[\s\S]*?)```", re.MULTILINE)
    _FRONTMATTER_PATTERN = re.compile(r"^---\s*\n(.*?)\n---\s*\n", re.DOTALL)

    def __init__(
        self,
        llm_provider: Any = None,
        max_retries: int = 3,
    ) -> None:
        """初期化.

        Note:
            LLM プロバイダーは環境変数から自動検出されます（松耦合設計）。

        Args:
            llm_provider: LLM プロバイダー（省略時は自動検出）
            max_retries: 最大リトライ回数
        """
        self._llm: LLMProvider = llm_provider or get_llm()
        self._max_retries = max_retries
        self._logger = logging.getLogger(__name__)

    async def generate(self, user_request: str) -> GenerationResult:
        """ユーザー要求から Skill を生成.

        Args:
            user_request: ユーザーの要求（自然言語）

        Returns:
            生成結果
        """
        prompt = SKILL_GENERATION_PROMPT.format(user_request=user_request)

        for attempt in range(1, self._max_retries + 1):
            try:
                # LLM で生成（松耦合：プロバイダー不明）
                result = await self._llm.complete(prompt)
                response = result["content"]

                # SKILL.md を抽出
                skill = self._parse_response(response, user_request)
                if skill:
                    self._logger.info(f"Generated skill: {skill.name} (attempt {attempt})")
                    return GenerationResult(skill=skill, success=True, attempts=attempt)

                self._logger.warning(f"Parse failed, attempt {attempt}/{self._max_retries}")

            except Exception as e:
                self._logger.exception(f"Generation error: {e}")
                if attempt == self._max_retries:
                    return GenerationResult(skill=None, success=False, error=str(e), attempts=attempt)

        return GenerationResult(skill=None, success=False, error="Failed to parse response", attempts=self._max_retries)

    def _parse_response(self, response: str, original_request: str) -> Skill | None:
        """LLM レスポンスから Skill を抽出.

        Args:
            response: LLM の出力
            original_request: 元の要求

        Returns:
            Skill インスタンス、抽出失敗時 None
        """
        import yaml  # 遅延インポート

        # markdown コードブロックから抽出
        match = self._MD_PATTERN.search(response)
        content = match.group(1) if match else response

        # frontmatter を解析
        fm_match = self._FRONTMATTER_PATTERN.match(content.strip())
        if not fm_match:
            return None

        try:
            yaml_content = fm_match.group(1)
            meta_dict = yaml.safe_load(yaml_content) or {}
            instructions = content[fm_match.end() :].strip()

            # 自動学習フラグを設定
            meta_dict["learned"] = True
            meta_dict["confidence"] = 0.8  # 初回生成は 0.8

            metadata = SkillMetadata.from_dict(meta_dict)
            return Skill(metadata=metadata, instructions=instructions)

        except yaml.YAMLError as e:
            self._logger.warning(f"YAML parse error: {e}")
            return None
