"""Skill 基類 - Claude Code Skills 互換の能力パッケージ.

このモジュールは Skill の基本クラスを定義します：
- SKILL.md ファイルの読み込み
- YAML frontmatter の解析
- 指示内容の取得
- 自動進化対応（学習・固化）

Claude Code Skills フォーマット：
```markdown
---
name: my-skill
description: Extract text and tables from PDF files. Use when working with PDFs.
version: 1.0.0
triggers:
  - pdf
  - document extraction
  - parse pdf
requirements:
  - pypdf
  - pdfplumber
---

# 指示内容
ここに指示を書く
```

参考: https://code.claude.com/docs/en/skills
"""

import re
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Any

import yaml


@dataclass
class SkillMetadata:
    """Skill メタデータ - Claude Code Skills 規範準拠.

    Claude Code の SKILL.md フォーマットと完全互換。
    自動進化システム用の追加フィールドも含む。

    Attributes:
        name: Skill 名（一意識別子）
        description: 説明（Claude が使用判断に使う重要フィールド）
        version: バージョン（semver 形式推奨）
        author: 作成者
        tags: タグリスト（分類用）
        triggers: トリガーワード（この単語が入力にあれば発火）
        requirements: 必要パッケージ（pip install 形式）
        dependencies: 依存 Skill 名
        depends_on: 依存する他の Skill 名（自動解決用）
        provides: 本 Skill が提供する能力タグ
        phase: 所属する工程段階（例: ingestion, analysis, generation）
        examples: 使用例リスト
        created_at: 作成日時
        learned: 自動学習で生成されたか
        confidence: 自動生成時の信頼度 (0.0-1.0)
        usage_count: 使用回数（人気度追跡用）
        extra: その他のメタデータ
    """

    # 必須フィールド
    name: str

    # Claude Code Skills 標準フィールド
    description: str = ""
    version: str = "1.0.0"
    author: str = ""
    tags: list[str] = field(default_factory=list)
    triggers: list[str] = field(default_factory=list)
    requirements: list[str] = field(default_factory=list)
    dependencies: list[str] = field(default_factory=list)
    depends_on: list[str] = field(default_factory=list)
    provides: list[str] = field(default_factory=list)
    phase: str = ""
    examples: list[str] = field(default_factory=list)

    # Claude Code CLI 互換フィールド
    allowed_tools: list[str] = field(default_factory=list)
    context: str = ""  # "fork" | "" (empty = default)
    agent: bool = False  # agent-requested skill
    user_invocable: bool = False
    disable_model_invocation: bool = False
    argument_hint: str = ""  # e.g., "<file-path>"
    hooks: dict[str, Any] = field(default_factory=dict)

    # 自動進化システム用フィールド
    created_at: str = ""
    learned: bool = False
    confidence: float = 1.0
    usage_count: int = 0

    # その他
    extra: dict[str, Any] = field(default_factory=dict)

    def __post_init__(self) -> None:
        """初期化後処理."""
        if not self.created_at:
            self.created_at = datetime.now().isoformat()

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "SkillMetadata":
        """辞書から SkillMetadata を作成.

        Args:
            data: メタデータ辞書

        Returns:
            SkillMetadata インスタンス

        Raises:
            TypeError: data が辞書でない場合
        """
        known_fields = {
            "name",
            "description",
            "version",
            "author",
            "tags",
            "triggers",
            "requirements",
            "dependencies",
            "depends_on",
            "depends-on",
            "provides",
            "phase",
            "examples",
            "created_at",
            "learned",
            "confidence",
            "usage_count",
            # Claude Code CLI fields (kebab-case and snake_case variants)
            "allowed-tools",
            "allowed_tools",
            "context",
            "agent",
            "user-invocable",
            "user_invocable",
            "disable-model-invocation",
            "disable_model_invocation",
            "argument-hint",
            "argument_hint",
            "hooks",
        }
        extra = {k: v for k, v in data.items() if k not in known_fields}

        def _ensure_list(value: Any) -> list[str]:
            """値をリストに変換."""
            if value is None:
                return []
            if isinstance(value, list):
                return [str(v) for v in value]
            return [str(value)]

        hooks_raw = data.get("hooks")

        return cls(
            name=str(data.get("name", "unknown")),
            description=str(data.get("description", "")),
            version=str(data.get("version", "1.0.0")),
            author=str(data.get("author", "")),
            tags=_ensure_list(data.get("tags")),
            triggers=_ensure_list(data.get("triggers")),
            requirements=_ensure_list(data.get("requirements")),
            dependencies=_ensure_list(data.get("dependencies")),
            depends_on=_ensure_list(data.get("depends_on", data.get("depends-on"))),
            provides=_ensure_list(data.get("provides")),
            phase=str(data.get("phase", "")),
            examples=_ensure_list(data.get("examples")),
            # Claude Code CLI fields (kebab-case → snake_case)
            allowed_tools=_ensure_list(data.get("allowed-tools", data.get("allowed_tools"))),
            context=str(data.get("context", "")),
            agent=bool(data.get("agent", False)),
            user_invocable=bool(data.get("user-invocable", data.get("user_invocable", False))),
            disable_model_invocation=bool(
                data.get(
                    "disable-model-invocation",
                    data.get("disable_model_invocation", False),
                )
            ),
            argument_hint=str(data.get("argument-hint", data.get("argument_hint", ""))),
            hooks=hooks_raw if isinstance(hooks_raw, dict) else {},
            # Auto-evolution fields
            created_at=str(data.get("created_at", "")),
            learned=bool(data.get("learned", False)),
            confidence=float(data.get("confidence", 1.0)),
            usage_count=int(data.get("usage_count", 0)),
            extra=extra,
        )

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換（YAML 出力用）.

        Returns:
            メタデータ辞書
        """
        result: dict[str, Any] = {
            "name": self.name,
            "description": self.description,
            "version": self.version,
        }
        # オプションフィールドは値がある場合のみ出力
        if self.author:
            result["author"] = self.author
        if self.tags:
            result["tags"] = self.tags
        if self.triggers:
            result["triggers"] = self.triggers
        if self.requirements:
            result["requirements"] = self.requirements
        if self.dependencies:
            result["dependencies"] = self.dependencies
        if self.depends_on:
            result["depends_on"] = self.depends_on
        if self.provides:
            result["provides"] = self.provides
        if self.phase:
            result["phase"] = self.phase
        if self.examples:
            result["examples"] = self.examples
        # Claude Code CLI fields (kebab-case output)
        _cli_fields: dict[str, Any] = {
            "allowed-tools": self.allowed_tools,
            "context": self.context,
            "agent": self.agent,
            "user-invocable": self.user_invocable,
            "disable-model-invocation": self.disable_model_invocation,
            "argument-hint": self.argument_hint,
            "hooks": self.hooks,
        }
        result.update({k: v for k, v in _cli_fields.items() if v})
        # Auto-evolution fields
        if self.learned:
            result["learned"] = self.learned
            result["confidence"] = self.confidence
            result["created_at"] = self.created_at
        if self.usage_count > 0:
            result["usage_count"] = self.usage_count
        # extra フィールドをマージ
        result.update(self.extra)
        return result

    def matches_query(self, query: str) -> float:
        """クエリとのマッチスコアを計算.

        Args:
            query: ユーザー入力

        Returns:
            マッチスコア (0.0-1.0)
        """
        query_lower = query.lower()
        score = 0.0

        # triggers との完全一致（最高優先度）
        for trigger in self.triggers:
            if trigger.lower() in query_lower:
                score = max(score, 0.9)

        # description との部分一致
        if self.description:
            desc_words = self.description.lower().split()
            query_words = set(query_lower.split())
            matches = sum(1 for w in desc_words if w in query_words)
            if desc_words:
                score = max(score, matches / len(desc_words) * 0.7)

        # tags との一致
        for tag in self.tags:
            if tag.lower() in query_lower:
                score = max(score, 0.6)

        # name との一致
        if self.name.lower() in query_lower:
            score = max(score, 0.8)

        return min(score, 1.0)


class Skill:
    """Skill 基類 - Claude Code Skills 完全互換.

    Claude Code Skills 規範に準拠した能力パッケージ。
    自動進化システムによる学習・固化に対応。

    Example:
        >>> skill = Skill.load(Path("./my-skill"))
        >>> print(skill.metadata.name)
        'my-skill'
        >>> print(skill.instructions)
        '# 指示内容...'
        >>> # 新規作成
        >>> skill = Skill.create("pdf-parser", "Parse PDF files", "Use pdfplumber...")
        >>> skill.save(Path("./skills/pdf-parser"))
    """

    # YAML frontmatter パターン
    _FRONTMATTER_PATTERN = re.compile(r"^---\s*\n(.*?)\n---\s*\n", re.DOTALL)

    def __init__(
        self,
        metadata: SkillMetadata,
        instructions: str,
        path: Path | None = None,
    ) -> None:
        """初期化.

        Args:
            metadata: メタデータ
            instructions: 指示内容
            path: Skill ファイルのパス
        """
        self._metadata = metadata
        self._instructions = instructions
        self._path = path

    @property
    def metadata(self) -> SkillMetadata:
        """メタデータを取得."""
        return self._metadata

    @property
    def instructions(self) -> str:
        """指示内容を取得."""
        return self._instructions

    @property
    def name(self) -> str:
        """Skill 名を取得."""
        return self._metadata.name

    @property
    def path(self) -> Path | None:
        """Skill パスを取得."""
        return self._path

    @classmethod
    def create(
        cls,
        name: str,
        description: str,
        instructions: str,
        *,
        triggers: list[str] | None = None,
        requirements: list[str] | None = None,
        tags: list[str] | None = None,
        learned: bool = False,
        confidence: float = 1.0,
    ) -> "Skill":
        """新しい Skill を作成.

        Args:
            name: Skill 名
            description: 説明
            instructions: 指示内容
            triggers: トリガーワード
            requirements: 必要パッケージ
            tags: タグ
            learned: 自動学習フラグ
            confidence: 信頼度

        Returns:
            Skill インスタンス
        """
        metadata = SkillMetadata(
            name=name,
            description=description,
            triggers=triggers or [],
            requirements=requirements or [],
            tags=tags or [],
            learned=learned,
            confidence=confidence,
        )
        return cls(metadata=metadata, instructions=instructions)

    @classmethod
    def load(cls, skill_path: Path) -> "Skill":
        """Skill を読み込み.

        Args:
            skill_path: Skill ディレクトリまたは SKILL.md ファイルのパス

        Returns:
            Skill インスタンス

        Raises:
            FileNotFoundError: SKILL.md が見つからない場合
            ValueError: フォーマットが不正な場合
        """
        # パスの正規化
        if skill_path.is_dir():
            skill_file = skill_path / "SKILL.md"
        else:
            skill_file = skill_path
            skill_path = skill_file.parent

        if not skill_file.exists():
            msg = f"SKILL.md not found: {skill_file}"
            raise FileNotFoundError(msg)

        # ファイル読み込み
        content = skill_file.read_text(encoding="utf-8")

        # frontmatter 解析
        match = cls._FRONTMATTER_PATTERN.match(content)
        if match:
            yaml_content = match.group(1)
            instructions = content[match.end() :].strip()
            try:
                meta_dict = yaml.safe_load(yaml_content) or {}
            except yaml.YAMLError as e:
                msg = f"Invalid YAML frontmatter: {e}"
                raise ValueError(msg) from e
        else:
            # frontmatter なしの場合
            meta_dict = {"name": skill_path.name}
            instructions = content.strip()

        metadata = SkillMetadata.from_dict(meta_dict)
        return cls(metadata=metadata, instructions=instructions, path=skill_path)

    def save(self, save_path: Path) -> Path:
        """Skill をファイルに保存.

        Args:
            save_path: 保存先ディレクトリ

        Returns:
            保存された SKILL.md のパス
        """
        save_path.mkdir(parents=True, exist_ok=True)
        skill_file = save_path / "SKILL.md"

        # YAML frontmatter 生成
        yaml_content = yaml.dump(
            self._metadata.to_dict(),
            default_flow_style=False,
            allow_unicode=True,
            sort_keys=False,
        )

        # SKILL.md 内容生成
        content = f"---\n{yaml_content}---\n\n{self._instructions}\n"
        skill_file.write_text(content, encoding="utf-8")

        self._path = save_path
        return skill_file

    def matches(self, query: str) -> float:
        """クエリとのマッチスコアを計算.

        Args:
            query: ユーザー入力

        Returns:
            マッチスコア (0.0-1.0)
        """
        return self._metadata.matches_query(query)

    def increment_usage(self) -> None:
        """使用回数をインクリメント."""
        self._metadata.usage_count += 1

    def to_prompt(self) -> str:
        """プロンプト用文字列に変換.

        Returns:
            システムプロンプトに挿入可能な形式
        """
        parts = [
            f"## Skill: {self.name}",
            f"Description: {self._metadata.description}",
            "",
            self._instructions,
        ]
        return "\n".join(parts)

    def __repr__(self) -> str:
        """文字列表現."""
        learned_mark = " [learned]" if self._metadata.learned else ""
        return f"Skill(name={self.name!r}, v={self._metadata.version!r}{learned_mark})"
