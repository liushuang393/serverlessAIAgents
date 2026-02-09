"""CodeAct执行器 - 增强的代码执行能力.

Manus分析中提到的CodeAct能力：
- 将工具调用转换为代码执行
- 支持复杂的多步骤操作
- 自动错误恢复和重试
- 执行结果的结构化解析

设计原则:
- 代码即工具：任何操作都可以通过代码表达
- 安全隔离：所有代码在沙盒中执行
- 结果追踪：完整的执行历史和结果

使用例:
    >>> from agentflow.sandbox.codeact_executor import CodeActExecutor
    >>>
    >>> executor = CodeActExecutor(sandbox_provider="microsandbox")
    >>> result = await executor.execute_action(
    ...     action_type="data_analysis",
    ...     params={"data": df, "operation": "describe"},
    ... )
"""

from __future__ import annotations

import asyncio
import json
import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field

from agentflow.sandbox.base import SandboxConfig, SandboxProvider


logger = logging.getLogger(__name__)


class ActionType(str, Enum):
    """操作类型."""

    CODE_EXECUTE = "code_execute"      # 直接代码执行
    DATA_ANALYSIS = "data_analysis"    # 数据分析
    FILE_OPERATION = "file_operation"  # 文件操作
    API_CALL = "api_call"              # API调用
    SHELL_COMMAND = "shell_command"    # Shell命令
    CUSTOM = "custom"                  # 自定义操作


class ExecutionStatus(str, Enum):
    """执行状态."""

    PENDING = "pending"
    RUNNING = "running"
    SUCCESS = "success"
    FAILED = "failed"
    TIMEOUT = "timeout"
    CANCELLED = "cancelled"


@dataclass
class ActionResult:
    """操作结果.

    Attributes:
        action_id: 操作ID
        action_type: 操作类型
        status: 执行状态
        output: 输出结果
        error: 错误信息
        duration_ms: 执行时间
        metadata: 元数据
    """

    action_id: str
    action_type: ActionType
    status: ExecutionStatus = ExecutionStatus.PENDING
    output: Any = None
    error: str | None = None
    duration_ms: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.now)
    completed_at: datetime | None = None

    @property
    def success(self) -> bool:
        """是否成功."""
        return self.status == ExecutionStatus.SUCCESS

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "action_id": self.action_id,
            "action_type": self.action_type.value,
            "status": self.status.value,
            "output": self.output,
            "error": self.error,
            "duration_ms": self.duration_ms,
            "success": self.success,
            "created_at": self.created_at.isoformat(),
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
        }


class ActionTemplate(BaseModel):
    """操作模板.

    预定义的操作模板，简化常见操作。

    Attributes:
        name: 模板名称
        action_type: 操作类型
        code_template: 代码模板
        required_params: 必需参数
        optional_params: 可选参数
    """

    name: str = Field(default="", description="模板名称")
    action_type: ActionType = Field(default=ActionType.CODE_EXECUTE)
    code_template: str = Field(default="", description="代码模板")
    required_params: list[str] = Field(default_factory=list)
    optional_params: list[str] = Field(default_factory=list)
    packages: list[str] = Field(default_factory=list, description="需要的包")
    timeout: float = Field(default=60.0, ge=1.0)


# 预定义模板
BUILTIN_TEMPLATES: dict[str, ActionTemplate] = {
    "data_describe": ActionTemplate(
        name="data_describe",
        action_type=ActionType.DATA_ANALYSIS,
        code_template="""
import pandas as pd
import json

data = {data}
df = pd.DataFrame(data)
result = df.describe().to_dict()
print(json.dumps(result))
""",
        required_params=["data"],
        packages=["pandas"],
    ),
    "file_read": ActionTemplate(
        name="file_read",
        action_type=ActionType.FILE_OPERATION,
        code_template="""
with open("{filepath}", "r") as f:
    content = f.read()
print(content)
""",
        required_params=["filepath"],
    ),
    "json_parse": ActionTemplate(
        name="json_parse",
        action_type=ActionType.DATA_ANALYSIS,
        code_template="""
import json
data = '''{json_string}'''
result = json.loads(data)
print(json.dumps(result, indent=2))
""",
        required_params=["json_string"],
    ),
}


class CodeActExecutor:
    """CodeAct执行器.

    将工具调用转换为代码执行，支持复杂的多步骤操作。

    核心功能:
    - 模板化操作：预定义常见操作模板
    - 动态代码生成：根据参数生成执行代码
    - 错误恢复：自动重试和错误处理
    - 结果解析：结构化解析执行结果

    Example:
        >>> executor = CodeActExecutor(sandbox_provider="microsandbox")
        >>>
        >>> # 使用模板
        >>> result = await executor.execute_template(
        ...     "data_describe",
        ...     data={"a": [1, 2, 3], "b": [4, 5, 6]},
        ... )
        >>>
        >>> # 直接执行代码
        >>> result = await executor.execute_code("print(1 + 1)")
    """

    def __init__(
        self,
        sandbox_provider: str = "microsandbox",
        config: SandboxConfig | None = None,
        max_retries: int = 3,
        retry_delay: float = 1.0,
    ) -> None:
        """初始化.

        Args:
            sandbox_provider: 沙盒提供者类型
            config: 沙盒配置
            max_retries: 最大重试次数
            retry_delay: 重试延迟秒数
        """
        self._provider_name = sandbox_provider
        self._config = config or SandboxConfig()
        self._max_retries = max_retries
        self._retry_delay = retry_delay
        self._sandbox: SandboxProvider | None = None
        self._templates = dict(BUILTIN_TEMPLATES)
        self._history: list[ActionResult] = []
        self._logger = logging.getLogger(__name__)

    async def _get_sandbox(self) -> SandboxProvider:
        """获取沙盒实例（懒加载）."""
        if self._sandbox is None:
            if self._provider_name == "microsandbox":
                from agentflow.sandbox.microsandbox_provider import MicrosandboxProvider
                self._sandbox = MicrosandboxProvider(self._config)
            elif self._provider_name == "docker":
                from agentflow.sandbox.docker_provider import DockerProvider
                self._sandbox = DockerProvider(self._config)
            elif self._provider_name == "e2b":
                from agentflow.sandbox.e2b_provider import E2BProvider
                self._sandbox = E2BProvider(self._config)
            else:
                msg = f"Unknown provider: {self._provider_name}"
                raise ValueError(msg)
        return self._sandbox

    def register_template(self, template: ActionTemplate) -> None:
        """注册操作模板.

        Args:
            template: 操作模板
        """
        self._templates[template.name] = template
        self._logger.info(f"注册模板: {template.name}")

    async def execute_code(
        self,
        code: str,
        *,
        packages: list[str] | None = None,
        timeout: float | None = None,
        env: dict[str, str] | None = None,
        files: dict[str, bytes] | None = None,
    ) -> ActionResult:
        """执行代码.

        Args:
            code: Python代码
            packages: 需要安装的包
            timeout: 超时秒数
            env: 环境变量
            files: 文件

        Returns:
            ActionResult
        """
        action_id = f"act-{uuid.uuid4().hex[:8]}"
        result = ActionResult(
            action_id=action_id,
            action_type=ActionType.CODE_EXECUTE,
            status=ExecutionStatus.RUNNING,
        )

        start_time = datetime.now()

        try:
            sandbox = await self._get_sandbox()
            exec_result = await sandbox.execute(
                code=code,
                packages=packages,
                timeout=timeout or self._config.timeout,
                env=env,
                files=files,
            )

            result.duration_ms = (datetime.now() - start_time).total_seconds() * 1000
            result.completed_at = datetime.now()

            if exec_result.success:
                result.status = ExecutionStatus.SUCCESS
                result.output = self._parse_output(exec_result.stdout)
            else:
                result.status = ExecutionStatus.FAILED
                result.error = exec_result.stderr or exec_result.error
                result.output = exec_result.stdout

        except TimeoutError:
            result.status = ExecutionStatus.TIMEOUT
            result.error = "执行超时"
            result.completed_at = datetime.now()
        except Exception as e:
            result.status = ExecutionStatus.FAILED
            result.error = str(e)
            result.completed_at = datetime.now()

        self._history.append(result)
        return result

    async def execute_template(
        self,
        template_name: str,
        **params: Any,
    ) -> ActionResult:
        """执行模板操作.

        Args:
            template_name: 模板名称
            **params: 模板参数

        Returns:
            ActionResult
        """
        if template_name not in self._templates:
            return ActionResult(
                action_id=f"act-{uuid.uuid4().hex[:8]}",
                action_type=ActionType.CUSTOM,
                status=ExecutionStatus.FAILED,
                error=f"模板不存在: {template_name}",
            )

        template = self._templates[template_name]

        # 检查必需参数
        missing = [p for p in template.required_params if p not in params]
        if missing:
            return ActionResult(
                action_id=f"act-{uuid.uuid4().hex[:8]}",
                action_type=template.action_type,
                status=ExecutionStatus.FAILED,
                error=f"缺少必需参数: {missing}",
            )

        # 生成代码
        code = template.code_template.format(**params)

        return await self.execute_code(
            code=code,
            packages=template.packages,
            timeout=template.timeout,
        )

    async def execute_action(
        self,
        action_type: ActionType | str,
        params: dict[str, Any],
        *,
        code: str | None = None,
        retry_on_failure: bool = True,
    ) -> ActionResult:
        """执行操作.

        Args:
            action_type: 操作类型
            params: 参数
            code: 自定义代码（可选）
            retry_on_failure: 失败时是否重试

        Returns:
            ActionResult
        """
        action_type = ActionType(action_type) if isinstance(action_type, str) else action_type

        # 如果提供了代码，直接执行
        if code:
            return await self._execute_with_retry(
                code=code,
                packages=params.get("packages"),
                retry_on_failure=retry_on_failure,
            )

        # 根据操作类型选择模板
        template_map = {
            ActionType.DATA_ANALYSIS: "data_describe",
            ActionType.FILE_OPERATION: "file_read",
        }

        template_name = template_map.get(action_type)
        if template_name and template_name in self._templates:
            return await self.execute_template(template_name, **params)

        # 无匹配模板，返回错误
        return ActionResult(
            action_id=f"act-{uuid.uuid4().hex[:8]}",
            action_type=action_type,
            status=ExecutionStatus.FAILED,
            error=f"无法处理操作类型: {action_type.value}",
        )

    async def _execute_with_retry(
        self,
        code: str,
        packages: list[str] | None = None,
        retry_on_failure: bool = True,
    ) -> ActionResult:
        """带重试的执行.

        Args:
            code: 代码
            packages: 包
            retry_on_failure: 是否重试

        Returns:
            ActionResult
        """
        last_result: ActionResult | None = None

        for attempt in range(self._max_retries if retry_on_failure else 1):
            result = await self.execute_code(code=code, packages=packages)

            if result.success:
                return result

            last_result = result

            if attempt < self._max_retries - 1:
                self._logger.warning(
                    f"执行失败 (尝试 {attempt + 1}/{self._max_retries}): {result.error}"
                )
                await asyncio.sleep(self._retry_delay * (attempt + 1))

        return last_result or ActionResult(
            action_id=f"act-{uuid.uuid4().hex[:8]}",
            action_type=ActionType.CODE_EXECUTE,
            status=ExecutionStatus.FAILED,
            error="所有重试都失败",
        )

    def _parse_output(self, output: str) -> Any:
        """解析输出.

        尝试将输出解析为JSON，失败则返回原始字符串。

        Args:
            output: 输出字符串

        Returns:
            解析后的结果
        """
        if not output:
            return None

        output = output.strip()

        # 尝试JSON解析
        try:
            return json.loads(output)
        except json.JSONDecodeError:
            pass

        # 尝试多行JSON
        lines = output.split("\n")
        for line in reversed(lines):
            try:
                return json.loads(line.strip())
            except json.JSONDecodeError:
                continue

        return output

    def get_history(self, limit: int = 100) -> list[ActionResult]:
        """获取执行历史.

        Args:
            limit: 最大返回数量

        Returns:
            ActionResult列表
        """
        return self._history[-limit:]

    def clear_history(self) -> None:
        """清空执行历史."""
        self._history.clear()

    async def close(self) -> None:
        """关闭资源."""
        if self._sandbox:
            await self._sandbox.close()
            self._sandbox = None

    async def __aenter__(self) -> CodeActExecutor:
        """async with支持."""
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """async with支持."""
        await self.close()

    def get_stats(self) -> dict[str, Any]:
        """获取统计信息."""
        total = len(self._history)
        success = sum(1 for r in self._history if r.success)
        failed = total - success

        return {
            "total_executions": total,
            "success_count": success,
            "failed_count": failed,
            "success_rate": success / total if total > 0 else 0.0,
            "template_count": len(self._templates),
            "provider": self._provider_name,
        }

