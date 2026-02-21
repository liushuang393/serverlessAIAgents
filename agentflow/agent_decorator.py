"""Agent デコレータ - 簡易Agent定義.

このモジュールは、@agent デコレータによる簡易Agent定義を提供します。
AgentBlockを継承せずに、シンプルなクラスをAgentとして使用できます。
Claude Code Skills 互換の Skills 自動読み込みをサポート。

v0.3.1: Pydantic 入出力スキーマ対応
- input_schema / output_schema クラス属性で型安全を実現
- 自動バリデーション & シリアライゼーション

使用例:
    >>> # 基本的な使用法
    >>> @agent
    ... class MyAgent:
    ...     '''簡単な質問応答Agent'''
    ...
    ...     system_prompt = "あなたは親切なアシスタントです"
    ...     skills = ["chatbot", "rag"]  # 使用する Skills
    ...
    ...     @tool
    ...     def search(self, query: str) -> list:
    ...         '''検索機能'''
    ...         return []
    ...
    >>> # Agentを呼び出し
    >>> result = await AgentClient.get("MyAgent").invoke({"question": "..."})
    >>>
    >>> # Pydantic スキーマ付き（型安全）
    >>> from pydantic import BaseModel
    >>> class QAInput(BaseModel):
    ...     question: str
    ...     context: str | None = None
    >>>
    >>> class QAOutput(BaseModel):
    ...     answer: str
    ...     confidence: float
    >>>
    >>> @agent
    ... class TypedAgent:
    ...     input_schema = QAInput      # 入力を自動バリデーション
    ...     output_schema = QAOutput    # 出力を自動シリアライズ
    ...
    ...     async def process(self, input_data: QAInput) -> QAOutput:
    ...         return QAOutput(answer="...", confidence=0.9)
"""

import logging
from collections.abc import AsyncIterator, Callable
from pathlib import Path
from typing import TYPE_CHECKING, Any, TypeVar, cast

from pydantic import BaseModel

from agentflow.providers.llm_provider import LLMProvider
from agentflow.providers.tool_provider import ToolProvider


if TYPE_CHECKING:
    from agentflow.providers.llm_provider import LLMProvider


# グローバルロガー
_logger = logging.getLogger(__name__)

# Agentレジストリ（グローバル）
_agent_registry: dict[str, "RegisteredAgent"] = {}

# Skillsレジストリ（グローバル・遅延初期化）
_skills_registry: dict[str, Any] | None = None


def _get_skills_registry() -> dict[str, Any]:
    """Skills レジストリを取得（遅延初期化）.

    Returns:
        Skill名 → Skillインスタンスのマップ
    """
    global _skills_registry
    if _skills_registry is None:
        _skills_registry = {}
        _load_builtin_skills()
    return _skills_registry


def _load_builtin_skills() -> None:
    """ビルトイン Skills を読み込み.

    agentflow/skills/builtin/ から SKILL.md を持つ Skills を自動検出。
    """
    from agentflow.skills.loader import SkillLoader

    logger = logging.getLogger(__name__)

    # ビルトイン Skills ディレクトリ
    builtin_dir = Path(__file__).parent / "skills" / "builtin"

    if builtin_dir.exists():
        loader = SkillLoader()
        skills = loader.load_directory(builtin_dir, recursive=True)
        registry = _get_skills_registry()

        for skill in skills:
            registry[skill.name] = skill
            logger.debug(f"Loaded builtin skill: {skill.name}")

        logger.info(f"Loaded {len(skills)} builtin skills")


def get_skill(name: str) -> Any:
    """Skill を名前で取得.

    Args:
        name: Skill 名

    Returns:
        Skill インスタンス

    Raises:
        KeyError: Skill が見つからない場合
    """
    registry = _get_skills_registry()
    if name not in registry:
        available = list(registry.keys())
        msg = f"Skill not found: {name}. Available: {available}"
        raise KeyError(msg)
    return registry[name]


def list_skills() -> list[str]:
    """利用可能な Skills 一覧を取得.

    Returns:
        Skill 名のリスト
    """
    return list(_get_skills_registry().keys())


class RegisteredAgent:
    """登録されたAgent情報.

    Claude Code Skills 互換の Skills 自動読み込みをサポート。
    v0.3.1: Pydantic 入出力スキーマ対応。
    """

    def __init__(
        self,
        cls: type,
        name: str,
        llm: str | None = None,
        temperature: float = 0.7,
        max_tokens: int = 2000,
        system_prompt: str | None = None,
        tools: list[str] | None = None,
        skills: list[str] | None = None,
        input_schema: type[BaseModel] | None = None,
        output_schema: type[BaseModel] | None = None,
    ) -> None:
        """初期化.

        Args:
            cls: Agentクラス
            name: Agent名
            llm: LLMモデル名
            temperature: 温度パラメータ
            max_tokens: 最大トークン数
            system_prompt: システムプロンプト
            tools: ツールリスト
            skills: 使用する Skills リスト（Claude Code Skills 互換）
            input_schema: 入力 Pydantic スキーマ（自動バリデーション）
            output_schema: 出力 Pydantic スキーマ（自動シリアライゼーション）
        """
        self.cls = cls
        self.name = name
        self.llm = llm
        self.temperature = temperature
        self.max_tokens = max_tokens
        self.system_prompt = system_prompt or getattr(cls, "system_prompt", None)
        self.tools = tools or []
        self.skills = skills or getattr(cls, "skills", [])
        # Pydantic スキーマ（クラス属性から取得）
        self.input_schema = input_schema or getattr(cls, "input_schema", None)
        self.output_schema = output_schema or getattr(cls, "output_schema", None)
        self._instance: Any = None
        self._llm_provider: LLMProvider | None = None
        self._loaded_skills: dict[str, Any] = {}
        self._logger = logging.getLogger(__name__)

    def get_instance(self) -> Any:
        """インスタンスを取得（遅延初期化）."""
        if self._instance is None:
            self._instance = self.cls()
            # LLMProviderを注入（get_llm() 松耦合API を使用）
            try:
                from agentflow.providers import get_llm

                self._llm_provider = get_llm(
                    temperature=self.temperature,
                    max_tokens=self.max_tokens,
                )
            except Exception as e:
                self._logger.warning(f"LLMProvider initialization failed: {e}")
                self._llm_provider = None
            self._instance._llm = self._llm_provider
            # ToolProviderを注入
            self._instance._tools = ToolProvider.discover()
            # Skillsを注入
            self._load_skills()
            self._instance._skills = self._loaded_skills
        return self._instance

    def _load_skills(self) -> None:
        """Skills を読み込み、システムプロンプトに追加.

        Claude Code Skills 規範に従い、Skill の instructions をプロンプトに追加。
        """
        if not self.skills:
            return

        skill_prompts = []

        for skill_name in self.skills:
            try:
                skill = get_skill(skill_name)
                self._loaded_skills[skill_name] = skill

                # Skill の instructions をプロンプトに追加
                if hasattr(skill, "to_prompt"):
                    skill_prompts.append(skill.to_prompt())
                elif hasattr(skill, "instructions"):
                    skill_prompts.append(f"## Skill: {skill_name}\n{skill.instructions}")

                self._logger.debug(f"Loaded skill: {skill_name}")
            except KeyError as e:
                self._logger.warning(f"Skill not found: {skill_name} - {e}")

        # システムプロンプトに Skills を追加
        if skill_prompts and self.system_prompt:
            skills_section = "\n\n---\n# Available Skills\n\n" + "\n\n".join(skill_prompts)
            self.system_prompt = self.system_prompt + skills_section
        elif skill_prompts:
            self.system_prompt = "# Available Skills\n\n" + "\n\n".join(skill_prompts)

    def get_skill(self, name: str) -> Any:
        """読み込まれた Skill を取得.

        Args:
            name: Skill 名

        Returns:
            Skill インスタンス
        """
        return self._loaded_skills.get(name)

    def validate_input(self, input_data: dict[str, Any]) -> Any:
        """入力データをバリデーション.

        input_schema が設定されている場合、Pydantic でバリデーションを実行。

        Args:
            input_data: 入力データ（辞書）

        Returns:
            バリデーション済みデータ（Pydantic モデルまたは元の辞書）

        Raises:
            ValidationError: バリデーション失敗時
        """
        if self.input_schema is None:
            return input_data

        # Pydantic v2 でバリデーション
        try:
            return self.input_schema.model_validate(input_data)
        except Exception as e:
            self._logger.exception(f"Input validation failed: {e}")
            raise

    def serialize_output(self, output: Any) -> dict[str, Any]:
        """出力をシリアライズ.

        output_schema が設定されている場合、Pydantic モデルを辞書に変換。

        Args:
            output: 出力データ（Pydantic モデルまたは辞書）

        Returns:
            シリアライズされた辞書
        """
        if output is None:
            return {}

        # Pydantic モデルの場合は model_dump()
        if hasattr(output, "model_dump"):
            return cast("dict[str, Any]", output.model_dump())

        # 辞書の場合はそのまま
        if isinstance(output, dict):
            return output

        # その他はそのまま辞書でラップ
        return {"result": output}

    @property
    def has_typed_schema(self) -> bool:
        """型付きスキーマがあるか."""
        return self.input_schema is not None or self.output_schema is not None


T = TypeVar("T", bound=type)


def agent[T: type](
    cls: T | None = None,
    *,
    name: str | None = None,
    llm: str | None = None,
    temperature: float = 0.7,
    max_tokens: int = 2000,
    tools: list[str] | None = None,
    skills: list[str] | None = None,
) -> T | Callable[[T], T]:
    """Agentデコレータ.

    クラスをAgentとして登録します。
    AgentBlockを継承する必要がなく、シンプルなクラスで定義可能。
    Claude Code Skills 互換の Skills 自動読み込みをサポート。

    Args:
        cls: Agentクラス
        name: Agent名（省略時はクラス名）
        llm: LLMモデル名（省略時はデフォルト）
        temperature: 温度パラメータ
        max_tokens: 最大トークン数
        tools: 有効にするツールリスト
        skills: 使用する Skills リスト（Claude Code Skills 互換）

    Returns:
        デコレートされたクラス

    使用例:
        >>> @agent
        ... class QAAgent:
        ...     system_prompt = "質問に答えます"
        ...
        >>> @agent(name="カスタム", llm="gpt-4", skills=["chatbot", "rag"])
        ... class AdvancedAgent:
        ...     pass

        >>> # クラス属性で Skills を指定
        >>> @agent
        ... class DocumentAgent:
        ...     skills = ["rag", "database-manager"]  # Claude Code Skills 互換
    """

    def decorator(cls: T) -> T:
        agent_name = name or cls.__name__

        # システムプロンプトを取得
        system_prompt = getattr(cls, "system_prompt", None)
        if system_prompt is None and cls.__doc__:
            system_prompt = cls.__doc__.strip()

        # クラス属性から Skills を取得（デコレータ引数が優先）
        agent_skills = skills or getattr(cls, "skills", None)

        # Agentをレジストリに登録
        registered = RegisteredAgent(
            cls=cls,
            name=agent_name,
            llm=llm,
            temperature=temperature,
            max_tokens=max_tokens,
            system_prompt=system_prompt,
            tools=tools,
            skills=agent_skills,
        )
        _agent_registry[agent_name] = registered

        # NEW: グローバル AgentRegistry にも登録
        try:
            from agentflow.core.agent_registry import get_global_agent_registry
            from agentflow.core.capability_spec import AgentCapabilitySpec

            global_registry = get_global_agent_registry()

            # AgentCapabilitySpec を作成
            capability = AgentCapabilitySpec(
                id=f"{agent_name}_capability",
                name=agent_name,
                description=system_prompt or cls.__doc__ or f"{agent_name} Agent",
                tags=list(agent_skills) if agent_skills else [],
                required_tools=[f"tool://skill/{s}" for s in (agent_skills or [])],
            )

            # AgentRegistry に登録
            def _factory() -> Any:
                return registered.get_instance()

            global_registry.register(
                agent_id=agent_name,
                capability=capability,
                factory=_factory,
            )

            _logger.debug(f"Agent '{agent_name}' を AgentRegistry に登録")
        except Exception as e:
            _logger.warning(f"AgentRegistry 登録エラー: {e}")

        # 元のクラスにメタデータを追加
        cls._agent_name = agent_name  # type: ignore
        cls._agent_registered = registered  # type: ignore

        # process メソッドがない場合、デフォルト実装を追加
        if not hasattr(cls, "process") and not hasattr(cls, "run"):

            async def default_process(self: Any, input_data: dict[str, Any]) -> dict[str, Any]:
                """デフォルトの処理メソッド.

                LLMを使ってメッセージを処理。
                """
                messages = []

                # システムプロンプト
                if hasattr(self, "system_prompt") and self.system_prompt:
                    messages.append({"role": "system", "content": self.system_prompt})

                # ユーザー入力
                user_content = input_data.get("question") or input_data.get("message") or str(input_data)
                messages.append({"role": "user", "content": user_content})

                # LLM呼び出し
                if hasattr(self, "_llm") and self._llm:
                    response = await self._llm.chat(messages)
                    return {"content": response.get("content", ""), "raw_response": response}

                return {"content": "[LLM not configured]", "input": input_data}

            cls.process = default_process  # type: ignore

        # run メソッドがない場合、process を呼び出すラッパーを追加
        if not hasattr(cls, "run"):

            async def run(self: Any, input_data: dict[str, Any]) -> dict[str, Any]:
                """実行メソッド."""
                if hasattr(self, "process"):
                    return cast("dict[str, Any]", await self.process(input_data))
                return {"error": "No process method defined"}

            cls.run = run  # type: ignore

        return cls

    # @agent と @agent() 両方に対応
    if cls is not None:
        return decorator(cls)
    return decorator


class AgentClient:
    """Agent呼び出しクライアント.

    登録されたAgentを名前で取得し、invoke/stream で呼び出し。

    使用例:
        >>> client = AgentClient.get("MyAgent")
        >>> result = await client.invoke({"question": "..."})
        >>> async for chunk in client.stream({"question": "..."}):
        ...     print(chunk)
    """

    def __init__(self, registered: RegisteredAgent) -> None:
        """初期化.

        Args:
            registered: 登録されたAgent情報
        """
        self._registered = registered
        self._logger = logging.getLogger(__name__)

    @classmethod
    def get(cls, name: str) -> "AgentClient":
        """Agent名でクライアントを取得.

        Args:
            name: Agent名

        Returns:
            AgentClient

        Raises:
            ValueError: Agentが見つからない場合
        """
        registered = _agent_registry.get(name)
        if registered is None:
            available = list(_agent_registry.keys())
            msg = f"Agent not found: {name}. Available: {available}"
            raise ValueError(msg)
        return cls(registered)

    @classmethod
    def list_agents(cls) -> list[str]:
        """登録されたAgent一覧を取得.

        Returns:
            Agent名のリスト
        """
        return list(_agent_registry.keys())

    async def invoke(
        self,
        input_data: dict[str, Any],
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """Agentを呼び出し（同期的に結果を取得）.

        入力スキーマが設定されている場合は自動バリデーション。
        出力スキーマが設定されている場合は自動シリアライゼーション。

        Args:
            input_data: 入力データ
            context: コンテキスト情報

        Returns:
            Agent の出力（辞書形式）

        Raises:
            ValidationError: 入力バリデーション失敗時
        """
        instance = self._registered.get_instance()

        # 入力バリデーション（Pydantic スキーマがある場合）
        validated_input = self._registered.validate_input(input_data)

        # コンテキストを設定
        if context:
            for key, value in context.items():
                setattr(instance, f"_ctx_{key}", value)

        # run メソッドを呼び出し
        # Pydantic モデルを渡す場合と辞書を渡す場合を区別
        call_input = validated_input if self._registered.has_typed_schema else input_data

        if hasattr(instance, "run"):
            result = await instance.run(call_input)
        elif hasattr(instance, "process"):
            result = await instance.process(call_input)
        else:
            msg = f"Agent {self._registered.name} has no run/process method"
            raise ValueError(msg)

        # 出力シリアライゼーション
        return self._registered.serialize_output(result)

    async def stream(
        self,
        input_data: dict[str, Any],
        context: dict[str, Any] | None = None,
    ) -> AsyncIterator[dict[str, Any]]:
        """Agentをストリームモードで呼び出し.

        Args:
            input_data: 入力データ
            context: コンテキスト情報

        Yields:
            生成されたチャンク
        """
        instance = self._registered.get_instance()

        # run_stream メソッドがあれば使用
        if hasattr(instance, "run_stream"):
            async for chunk in instance.run_stream(input_data):
                yield chunk
        else:
            # 通常の呼び出しを1チャンクとして返す
            result = await self.invoke(input_data, context)
            yield result

    async def invoke_async(
        self,
        input_data: dict[str, Any],
        callback_url: str | None = None,
    ) -> str:
        """非同期呼び出し（バックグラウンド実行）.

        Args:
            input_data: 入力データ
            callback_url: 完了時のコールバックURL

        Returns:
            タスクID
        """
        import uuid

        task_id = str(uuid.uuid4())
        # TODO: 実際のバックグラウンドタスク実装
        self._logger.info(f"Async task created: {task_id}")
        return task_id

    @property
    def name(self) -> str:
        """Agent名."""
        return self._registered.name
