"""汎用スキルパックローダー.

SKILL.md フロントマター形式のスキルファイルを自動検出・パースし、
LLM アドバイザリーハンドラとして SkillGateway に登録する。

スキルパック構造（自動検出）:
    skills/<pack-name>/
    ├── <skill-a>/SKILL.md    ← フロントマター: name, description
    ├── <skill-b>/SKILL.md
    └── ...

利用方法:
    # 単一スキルをロード
    >>> load_skill(gateway, Path("skills/pack/find-community/SKILL.md"))

    # スキルパック一括ロード
    >>> load_skill_pack(gateway, Path("skills/minimalist-entrepreneur-skills"))

    # messaging_hub のスキルディレクトリ全体をスキャン
    >>> discover_and_load_all_packs(gateway)

    # 特定タスク向けに一連のスキルをロード
    >>> load_skills_for_task(gateway, task="product_launch")
"""

from __future__ import annotations

import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING, Any

from infrastructure.llm.providers import get_llm
from kernel.skills.gateway import RiskLevel, SkillCategory, SkillDefinition


if TYPE_CHECKING:
    from collections.abc import Awaitable, Callable

    from kernel.skills.gateway import SkillGateway

logger = logging.getLogger(__name__)

# スキルディレクトリのルート
_SKILLS_ROOT = Path(__file__).parent.parent  # apps/messaging_hub/skills/

# タスク別スキルセット定義
TASK_SKILL_SETS: dict[str, list[str]] = {
    # 製品確定: コミュニティ発見 → アイデア検証 → プロセス化
    "product_discovery": [
        "find-community",
        "validate-idea",
        "processize",
        "mvp",
    ],
    # 製品発売: MVP → 価格 → 顧客獲得 → マーケティング
    "product_launch": [
        "mvp",
        "pricing",
        "first-customers",
        "marketing-plan",
    ],
    # 収益性維持: 価格見直し → 持続可能成長 → レビュー
    "profitability": [
        "pricing",
        "grow-sustainably",
        "minimalist-review",
    ],
    # 製品改善: レビュー → 再検証 → 次期MVP
    "product_improvement": [
        "minimalist-review",
        "validate-idea",
        "mvp",
    ],
}


def _parse_frontmatter(content: str) -> tuple[dict[str, str], str]:
    """SKILL.md のフロントマターとボディを分離する.

    Args:
        content: SKILL.md の全文

    Returns:
        (フロントマター辞書, ボディテキスト)
    """
    frontmatter: dict[str, str] = {}
    body = content

    match = re.match(r"^---\s*\n(.*?)\n---\s*\n(.*)$", content, re.DOTALL)
    if match:
        fm_text = match.group(1)
        body = match.group(2)
        for line in fm_text.strip().splitlines():
            if ":" in line:
                key, _, value = line.partition(":")
                frontmatter[key.strip()] = value.strip()

    return frontmatter, body.strip()


def _create_advisory_handler(
    prompt_content: str,
    skill_name: str,
) -> Callable[..., Awaitable[Any]]:
    """SKILL.md プロンプトを LLM アドバイザリーハンドラに変換する.

    Args:
        prompt_content: スキルのシステムプロンプト
        skill_name: スキル名（ログ用）

    Returns:
        非同期ハンドラ関数
    """

    async def handler(
        user_input: str = "",
        context: str = "",
    ) -> dict[str, Any]:
        """アドバイザリースキルを実行する.

        Args:
            user_input: ユーザーの質問・入力
            context: 追加コンテキスト（前ステップの結果など）

        Returns:
            アドバイス結果
        """
        llm = get_llm(temperature=0.4)

        user_message = user_input
        if context:
            user_message = f"{user_input}\n\n## 追加コンテキスト\n{context}"

        messages: list[dict[str, str]] = [
            {"role": "system", "content": prompt_content},
            {"role": "user", "content": user_message},
        ]

        response = await llm.generate(role="reasoning", messages=messages)
        content = str(response.get("content", ""))

        return {
            "advice": content,
            "skill_name": skill_name,
            "skill_type": "advisory",
        }

    return handler


def _to_skill_id(name: str, pack_prefix: str = "") -> str:
    """スキル名を SkillGateway 用の ID に変換する.

    例: "find-community" → "biz_find_community"
        "find-community" (prefix="me") → "me_find_community"

    Args:
        name: フロントマターの name フィールド
        pack_prefix: スキルパックのプレフィックス

    Returns:
        スキルID
    """
    normalized = name.replace("-", "_").replace(" ", "_").lower()
    prefix = pack_prefix or "biz"
    return f"{prefix}_{normalized}"


def load_skill(
    gateway: SkillGateway,
    skill_path: Path,
    pack_prefix: str = "",
) -> str | None:
    """単一の SKILL.md をロードして SkillGateway に登録する.

    Args:
        gateway: スキルゲートウェイ
        skill_path: SKILL.md ファイルのパス
        pack_prefix: スキルID のプレフィックス

    Returns:
        登録されたスキルID。失敗時は None
    """
    if not skill_path.exists():
        logger.warning("スキルファイルが見つかりません: %s", skill_path)
        return None

    try:
        raw = skill_path.read_text(encoding="utf-8")
    except OSError:
        logger.warning("スキルファイルの読み込みに失敗: %s", skill_path)
        return None

    frontmatter, body = _parse_frontmatter(raw)
    name = frontmatter.get("name", skill_path.parent.name)
    description = frontmatter.get("description", f"{name} スキル")

    skill_id = _to_skill_id(name, pack_prefix)
    handler = _create_advisory_handler(body, skill_id)

    skill = SkillDefinition(
        name=skill_id,
        description=description,
        category=SkillCategory.ADVISORY,
        risk_level=RiskLevel.LOW,
        handler=handler,
        parameters={
            "user_input": {
                "type": "string",
                "description": "ユーザーの質問・入力",
                "required": True,
            },
            "context": {
                "type": "string",
                "description": "追加コンテキスト（前ステップの結果など）",
                "required": False,
            },
        },
        requires_confirmation=False,
        allowed_in_isolated=True,
        allowed_in_real_machine=True,
    )

    gateway.register_skill(skill)
    logger.debug("スキル登録: %s (%s)", skill_id, name)
    return skill_id


def load_skill_pack(
    gateway: SkillGateway,
    pack_dir: Path,
    pack_prefix: str = "",
) -> list[str]:
    """スキルパックディレクトリを一括ロードする.

    pack_dir 直下の各サブディレクトリから SKILL.md を探して登録する。

    Args:
        gateway: スキルゲートウェイ
        pack_dir: スキルパックのルートディレクトリ
        pack_prefix: スキルID のプレフィックス

    Returns:
        登録されたスキルID のリスト
    """
    if not pack_dir.is_dir():
        logger.warning("スキルパックディレクトリが見つかりません: %s", pack_dir)
        return []

    registered: list[str] = []
    for sub in sorted(pack_dir.iterdir()):
        skill_md = sub / "SKILL.md"
        if sub.is_dir() and skill_md.exists():
            skill_id = load_skill(gateway, skill_md, pack_prefix)
            if skill_id is not None:
                registered.append(skill_id)

    logger.info(
        "スキルパック '%s' から %d 件登録: %s",
        pack_dir.name,
        len(registered),
        registered,
    )
    return registered


def load_skills_for_task(
    gateway: SkillGateway,
    task: str,
    pack_dir: Path | None = None,
    pack_prefix: str = "",
) -> list[str]:
    """特定タスク向けのスキルセットをロードする.

    TASK_SKILL_SETS に定義されたタスク名に対応するスキルのみを選択的にロードする。

    Args:
        gateway: スキルゲートウェイ
        task: タスク名（product_discovery, product_launch, profitability, product_improvement）
        pack_dir: スキルパックディレクトリ（None の場合はデフォルト）
        pack_prefix: スキルID のプレフィックス

    Returns:
        登録されたスキルID のリスト
    """
    skill_names = TASK_SKILL_SETS.get(task)
    if skill_names is None:
        available = ", ".join(TASK_SKILL_SETS.keys())
        logger.warning("未知のタスク '%s'。利用可能: %s", task, available)
        return []

    if pack_dir is None:
        pack_dir = _SKILLS_ROOT / "minimalist-entrepreneur-skills"

    registered: list[str] = []
    for skill_name in skill_names:
        skill_md = pack_dir / skill_name / "SKILL.md"
        skill_id = load_skill(gateway, skill_md, pack_prefix)
        if skill_id is not None:
            registered.append(skill_id)

    logger.info(
        "タスク '%s' 向けスキル %d/%d 件登録: %s",
        task,
        len(registered),
        len(skill_names),
        registered,
    )
    return registered


def discover_and_load_all_packs(
    gateway: SkillGateway,
    skills_root: Path | None = None,
) -> dict[str, list[str]]:
    """スキルディレクトリ内の全パックを自動検出・ロードする.

    skills_root 直下のサブディレクトリを走査し、
    SKILL.md を含むサブディレクトリを持つものをスキルパックと見なす。

    Args:
        gateway: スキルゲートウェイ
        skills_root: スキルのルートディレクトリ（None の場合はデフォルト）

    Returns:
        パック名 → 登録スキルID リストの辞書
    """
    root = skills_root or _SKILLS_ROOT
    if not root.is_dir():
        logger.warning("スキルルートが見つかりません: %s", root)
        return {}

    results: dict[str, list[str]] = {}

    for candidate in sorted(root.iterdir()):
        if not candidate.is_dir():
            continue
        # __pycache__ や entrepreneurship（ローダー自体）はスキップ
        if candidate.name.startswith(("__", ".")):
            continue
        # Python パッケージ（__init__.py あり）はスキップ
        if (candidate / "__init__.py").exists():
            continue

        # SKILL.md を含むサブディレクトリがあるかチェック
        has_skills = any((sub / "SKILL.md").exists() for sub in candidate.iterdir() if sub.is_dir())
        if has_skills:
            pack_name = candidate.name
            # パック名からプレフィックスを生成（例: minimalist-entrepreneur-skills → biz）
            prefix = _derive_prefix(pack_name)
            registered = load_skill_pack(gateway, candidate, prefix)
            results[pack_name] = registered

    total = sum(len(v) for v in results.values())
    logger.info("全パック自動検出完了: %d パック、計 %d スキル", len(results), total)
    return results


def _derive_prefix(pack_name: str) -> str:
    """パック名からスキルID プレフィックスを導出する.

    Args:
        pack_name: スキルパックのディレクトリ名

    Returns:
        プレフィックス文字列
    """
    # 既知のマッピング
    known: dict[str, str] = {
        "minimalist-entrepreneur-skills": "biz",
    }
    if pack_name in known:
        return known[pack_name]

    # 汎用: 最初の単語の先頭3文字
    first_word = pack_name.split("-")[0]
    return first_word[:3].lower()


# 後方互換: 旧 API
def register_entrepreneurship_skills(gateway: SkillGateway) -> int:
    """起業フレームワークスキルを一括登録する（後方互換）.

    Args:
        gateway: スキルゲートウェイ

    Returns:
        登録されたスキル数
    """
    pack_dir = _SKILLS_ROOT / "minimalist-entrepreneur-skills"
    registered = load_skill_pack(gateway, pack_dir, pack_prefix="biz")
    return len(registered)
