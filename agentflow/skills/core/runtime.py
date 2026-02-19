"""Skill実行ランタイム - Anthropic Skills体系準拠.

Skill内のスクリプトを安全に実行するためのランタイム環境を提供。
agentflow/sandbox/を統合し、隔離されたPython実行環境を実現。

設計原則（Anthropic Skills体系より）:
- 確定性処理: スクリプトはLLM推論なしで決定論的に実行
- 隔離実行: サンドボックスで安全に実行
- 依存管理: requirements自動インストール

Example:
    >>> from agentflow.skills import SkillRuntime, SkillRouter
    >>>
    >>> router = SkillRouter()
    >>> await router.initialize()
    >>> runtime = SkillRuntime()
    >>>
    >>> # Skillスクリプト実行
    >>> result = router.route("extract keywords")
    >>> if result.matched:
    ...     script_result = await runtime.execute_script(
    ...         result.skill,
    ...         "extract_keywords",
    ...         {"articles": [...]}
    ...     )
    ...     print(script_result.output)
"""

from __future__ import annotations

import asyncio
import importlib.util
import json
import logging
import sys
import time
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from pathlib import Path

    from agentflow.sandbox.base import ExecutionResult
    from agentflow.skills.base import Skill


logger = logging.getLogger(__name__)


@dataclass
class ScriptResult:
    """スクリプト実行結果.

    Attributes:
        success: 成功したか
        output: 出力データ（JSON解析済み）
        stdout: 標準出力
        stderr: 標準エラー出力
        duration_ms: 実行時間（ミリ秒）
        error: エラーメッセージ
    """

    success: bool = True
    output: dict[str, Any] = field(default_factory=dict)
    stdout: str = ""
    stderr: str = ""
    duration_ms: float = 0.0
    error: str | None = None

    @classmethod
    def from_execution_result(
        cls, result: ExecutionResult, parse_json: bool = True
    ) -> ScriptResult:
        """ExecutionResultから変換.

        Args:
            result: サンドボックス実行結果
            parse_json: stdoutをJSONとして解析するか

        Returns:
            ScriptResult
        """
        output: dict[str, Any] = {}
        if parse_json and result.stdout:
            try:
                output = json.loads(result.stdout.strip())
            except json.JSONDecodeError:
                # JSONでない場合はそのまま
                output = {"raw": result.stdout}

        return cls(
            success=result.success,
            output=output,
            stdout=result.stdout,
            stderr=result.stderr,
            duration_ms=result.duration_ms,
            error=result.error,
        )


class SkillRuntime:
    """Skill実行ランタイム - 確定性スクリプトの実行環境.

    Skillディレクトリ内のscripts/配下のPythonスクリプトを実行。
    サンドボックス（microsandbox/docker/e2b）または直接実行を選択可能。

    Attributes:
        sandbox_provider: サンドボックスプロバイダ名（None=直接実行）
    """

    def __init__(
        self,
        sandbox_provider: str | None = None,
        timeout: float = 60.0,
    ) -> None:
        """初期化.

        Args:
            sandbox_provider: サンドボックスプロバイダ（microsandbox/docker/e2b/None）
            timeout: デフォルトタイムアウト秒
        """
        self._sandbox_provider = sandbox_provider
        self._timeout = timeout
        self._logger = logging.getLogger(__name__)

    async def execute_script(
        self,
        skill: Skill,
        script_name: str,
        input_data: dict[str, Any],
        *,
        timeout: float | None = None,
        use_sandbox: bool | None = None,
    ) -> ScriptResult:
        """Skillスクリプトを実行.

        Args:
            skill: 対象Skill
            script_name: スクリプト名（拡張子なし）
            input_data: 入力データ（JSON変換可能）
            timeout: タイムアウト秒
            use_sandbox: サンドボックス使用（None=自動判断）

        Returns:
            ScriptResult
        """
        script_path = self._resolve_script_path(skill, script_name)
        if not script_path:
            return ScriptResult(
                success=False,
                error=f"Script not found: {script_name} in {skill.name}",
            )

        effective_timeout = timeout or self._timeout

        # Route by file extension
        if script_path.suffix == ".sh":
            return await self._execute_shell_script(skill, script_path, input_data)

        should_use_sandbox = (
            use_sandbox if use_sandbox is not None else bool(self._sandbox_provider)
        )

        if should_use_sandbox and self._sandbox_provider:
            return await self._execute_in_sandbox(skill, script_path, input_data, effective_timeout)
        return await self._execute_directly(skill, script_path, input_data)

    def _resolve_script_path(self, skill: Skill, script_name: str) -> Path | None:
        """スクリプトパスを解決.

        Args:
            skill: 対象Skill
            script_name: スクリプト名

        Returns:
            スクリプトファイルパス（存在しない場合はNone）
        """
        if not skill.path:
            return None

        # scripts/ ディレクトリ内を検索
        scripts_dir = skill.path / "scripts"
        if not scripts_dir.exists():
            return None

        # Check exact match first (includes extension)
        script_path = scripts_dir / script_name
        if script_path.exists():
            return script_path

        # Try with extensions: .py, .sh
        for ext in (".py", ".sh"):
            if not script_name.endswith(ext):
                candidate = scripts_dir / f"{script_name}{ext}"
                if candidate.exists():
                    return candidate

        return None

    def list_scripts(self, skill: Skill) -> list[str]:
        """Skill内のスクリプト一覧を取得.

        Args:
            skill: 対象Skill

        Returns:
            スクリプト名リスト（拡張子なし）
        """
        if not skill.path:
            return []

        scripts_dir = skill.path / "scripts"
        if not scripts_dir.exists():
            return []

        return [
            p.stem
            for p in scripts_dir.glob("*")
            if p.suffix in (".py", ".sh") and not p.name.startswith("_")
        ]

    async def _execute_directly(
        self,
        skill: Skill,
        script_path: Path,
        input_data: dict[str, Any],
    ) -> ScriptResult:
        """スクリプトを直接実行（サンドボックスなし）.

        スクリプト内の関数を動的インポートして呼び出し。
        同じプロセス内で実行するため高速だが隔離性なし。

        Args:
            skill: 対象Skill
            script_path: スクリプトパス
            input_data: 入力データ

        Returns:
            ScriptResult
        """
        import time

        start_time = time.time()

        try:
            # 動的インポート
            spec = importlib.util.spec_from_file_location(
                f"skill_script_{skill.name}_{script_path.stem}", script_path
            )
            if not spec or not spec.loader:
                return ScriptResult(
                    success=False,
                    error=f"Cannot load script: {script_path}",
                )

            module = importlib.util.module_from_spec(spec)
            # Python 3.13+ではdataclassが__module__を参照するため、事前登録が必須
            sys.modules[spec.name] = module
            module.__dict__["__builtins__"] = __builtins__
            spec.loader.exec_module(module)

            # メイン関数を検索
            # スクリプトファイル名から関数名を推測（snake_caseの関数）
            expected_func_name = script_path.stem

            # スクリプト内で定義された関数のみを対象
            # dataclass、クラス、importされたものは除外
            import inspect

            def is_valid_function(name: str) -> bool:
                """有効な関数かどうかを判定."""
                if name.startswith("_"):
                    return False
                obj = getattr(module, name, None)
                if obj is None or not callable(obj):
                    return False
                # クラス（dataclass含む）を除外
                if inspect.isclass(obj):
                    return False
                # このモジュールで定義されたものか確認
                obj_module = getattr(obj, "__module__", None)
                return obj_module == spec.name

            defined_funcs = [name for name in dir(module) if is_valid_function(name)]

            # スクリプトファイル名に基づく関数名マッピング
            # 例: validate_input.py -> validate_articles_input, validate_input

            # スクリプト名の一部を含む関数を検索
            matching_funcs = [
                name
                for name in defined_funcs
                if expected_func_name.replace("_", "") in name.replace("_", "").lower()
            ]

            # 優先順位: マッチする関数 > main > 定義された最初の関数
            if matching_funcs:
                func_name = matching_funcs[0]
            elif "main" in defined_funcs:
                func_name = "main"
            elif defined_funcs:
                func_name = defined_funcs[0]
            else:
                return ScriptResult(
                    success=False,
                    error=f"No callable function in {script_path}",
                )

            func = getattr(module, func_name)
            self._logger.debug(f"Calling function: {func_name}")

            # 関数実行
            result = func(input_data)

            # 結果を辞書に変換
            output = self._convert_result_to_dict(result)

            duration_ms = (time.time() - start_time) * 1000
            return ScriptResult(
                success=True,
                output=output,
                duration_ms=duration_ms,
            )

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            self._logger.exception(f"Script execution failed: {e}")
            return ScriptResult(
                success=False,
                error=str(e),
                duration_ms=duration_ms,
            )

    async def _execute_shell_script(
        self,
        skill: Skill,
        script_path: Path,
        input_data: dict[str, Any],
    ) -> ScriptResult:
        """シェルスクリプトを実行.

        JSON入力をstdinで渡し、stdoutからJSON出力を取得。

        Args:
            skill: 対象Skill
            script_path: スクリプトパス
            input_data: 入力データ

        Returns:
            ScriptResult
        """
        start_time = time.time()
        try:
            input_json = json.dumps(input_data, ensure_ascii=False)
            proc = await asyncio.create_subprocess_exec(
                "bash",
                str(script_path),
                stdin=asyncio.subprocess.PIPE,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd=str(skill.path),
            )
            stdout_bytes, stderr_bytes = await asyncio.wait_for(
                proc.communicate(input=input_json.encode()),
                timeout=self._timeout,
            )
            duration_ms = (time.time() - start_time) * 1000
            stdout = stdout_bytes.decode("utf-8", errors="replace")
            stderr = stderr_bytes.decode("utf-8", errors="replace")

            output: dict[str, Any] = {}
            if stdout.strip():
                try:
                    output = json.loads(stdout.strip())
                except json.JSONDecodeError:
                    output = {"raw": stdout}

            return ScriptResult(
                success=proc.returncode == 0,
                output=output,
                stdout=stdout,
                stderr=stderr,
                duration_ms=duration_ms,
                error=stderr if proc.returncode != 0 else None,
            )
        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            return ScriptResult(success=False, error=str(e), duration_ms=duration_ms)

    def _convert_result_to_dict(self, result: Any) -> dict[str, Any]:
        """実行結果を辞書に変換.

        dataclass、Pydanticモデル、namedtuple等に対応。

        Args:
            result: 関数の戻り値

        Returns:
            辞書形式の結果
        """
        import dataclasses

        # None の場合
        if result is None:
            return {"result": None}

        # 既に辞書の場合
        if isinstance(result, dict):
            return result

        # dataclass の場合
        if dataclasses.is_dataclass(result) and not isinstance(result, type):
            return dataclasses.asdict(result)

        # to_dict() メソッドがある場合（Pydantic等）
        if hasattr(result, "to_dict") and callable(result.to_dict):
            return result.to_dict()

        # model_dump() メソッドがある場合（Pydantic v2）
        if hasattr(result, "model_dump") and callable(result.model_dump):
            return result.model_dump()

        # dict() メソッドがある場合（Pydantic v1）
        if hasattr(result, "dict") and callable(result.dict):
            return result.dict()

        # _asdict() メソッドがある場合（namedtuple）
        if hasattr(result, "_asdict") and callable(result._asdict):
            return result._asdict()

        # プリミティブ型の場合
        if isinstance(result, (str, int, float, bool)):
            return {"result": result}

        # リストの場合
        if isinstance(result, list):
            return {"result": result}

        # その他: __dict__ を使用
        if hasattr(result, "__dict__"):
            return dict(result.__dict__)

        # フォールバック
        return {"result": str(result)}

    async def _execute_in_sandbox(
        self,
        skill: Skill,
        script_path: Path,
        input_data: dict[str, Any],
        timeout: float,
    ) -> ScriptResult:
        """サンドボックス内でスクリプトを実行.

        隔離された環境で安全に実行。
        依存パッケージを自動インストール。

        Args:
            skill: 対象Skill
            script_path: スクリプトパス
            input_data: 入力データ
            timeout: タイムアウト秒

        Returns:
            ScriptResult
        """
        from agentflow.sandbox import get_sandbox

        try:
            sandbox = get_sandbox(provider=self._sandbox_provider or "docker")

            # スクリプトコード読み込み
            script_code = script_path.read_text(encoding="utf-8")

            # ラッパーコード生成（入力データ注入 + 関数呼び出し + JSON出力）
            wrapper_code = self._generate_wrapper_code(script_code, script_path.stem, input_data)

            # 依存パッケージ取得
            packages = skill.metadata.requirements.copy() if skill.metadata.requirements else []

            # サンドボックスで実行
            result = await sandbox.execute(
                code=wrapper_code,
                timeout=timeout,
                packages=packages if packages else None,
            )

            return ScriptResult.from_execution_result(result)

        except Exception as e:
            self._logger.exception(f"Sandbox execution failed: {e}")
            return ScriptResult(
                success=False,
                error=str(e),
            )

    def _generate_wrapper_code(
        self,
        script_code: str,
        func_name: str,
        input_data: dict[str, Any],
    ) -> str:
        """サンドボックス用ラッパーコード生成.

        Args:
            script_code: 元のスクリプトコード
            func_name: 呼び出す関数名
            input_data: 入力データ

        Returns:
            ラッパーコード
        """
        input_json = json.dumps(input_data, ensure_ascii=False)
        return f"""
import json
import sys

# --- Original Script ---
{script_code}
# --- End of Original Script ---

# --- Wrapper ---
if __name__ == "__main__":
    input_data = json.loads({input_json!r})

    # Find and call the main function
    func = None
    if "{func_name}" in dir():
        func = {func_name}
    elif "main" in dir():
        func = main
    else:
        # Find first public function
        for name in dir():
            if not name.startswith("_") and callable(eval(name)):
                try:
                    func = eval(name)
                    break
                except:
                    pass

    if func is None:
        print(json.dumps({{"error": "No callable function found"}}))
        sys.exit(1)

    try:
        result = func(input_data)
        # Convert result to JSON
        if hasattr(result, "__dict__"):
            output = result.__dict__
        elif hasattr(result, "to_dict"):
            output = result.to_dict()
        elif isinstance(result, dict):
            output = result
        else:
            output = {{"result": result}}
        print(json.dumps(output, ensure_ascii=False, default=str))
    except Exception as e:
        print(json.dumps({{"error": str(e)}}))
        sys.exit(1)
"""
