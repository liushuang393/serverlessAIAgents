# -*- coding: utf-8 -*-
"""Language Adapter Base Classes.

语言适配器的抽象基类，定义源语言和目标语言的统一接口。
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any


@dataclass
class ExecutionResult:
    """代码执行结果."""

    success: bool
    outputs: dict[str, Any] = field(default_factory=dict)
    stdout: str = ""
    stderr: str = ""
    return_code: int = 0
    execution_time_ms: float = 0.0
    error: str | None = None


@dataclass
class AST:
    """抽象语法树."""

    program_id: str
    divisions: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)
    variables: list[dict[str, Any]] = field(default_factory=list)
    procedures: list[dict[str, Any]] = field(default_factory=list)


class SourceLanguageAdapter(ABC):
    """源语言适配器接口.

    定义如何解析和执行源语言代码。
    """

    @property
    @abstractmethod
    def language_name(self) -> str:
        """语言名称."""

    @abstractmethod
    def parse(self, source_code: str) -> AST:
        """解析源代码为 AST.

        Args:
            source_code: 源代码

        Returns:
            抽象语法树
        """

    @abstractmethod
    def extract_variables(self, ast: AST) -> list[dict[str, Any]]:
        """提取变量定义.

        Args:
            ast: 抽象语法树

        Returns:
            变量列表
        """

    @abstractmethod
    def identify_external_calls(self, ast: AST) -> list[dict[str, Any]]:
        """识别外部调用（文件、DB、API 等）.

        Args:
            ast: 抽象语法树

        Returns:
            外部调用列表
        """

    def execute(self, source_code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """执行源代码（可选，用于差分测试）.

        Args:
            source_code: 源代码
            inputs: 输入参数

        Returns:
            执行结果
        """
        return ExecutionResult(
            success=False,
            error="Execution not supported for this language",
        )


class TargetLanguageAdapter(ABC):
    """目标语言适配器接口.

    定义如何生成和执行目标语言代码。
    """

    @property
    @abstractmethod
    def language_name(self) -> str:
        """语言名称."""

    @abstractmethod
    def generate_skeleton(self, ast: AST, class_name: str) -> str:
        """生成代码骨架（类定义、字段声明）.

        Args:
            ast: 源代码 AST
            class_name: 类名

        Returns:
            代码骨架
        """

    @abstractmethod
    def generate_test_skeleton(self, class_name: str, test_cases: list[dict]) -> str:
        """生成测试代码骨架.

        Args:
            class_name: 被测类名
            test_cases: 测试用例

        Returns:
            测试代码骨架
        """

    @abstractmethod
    def compile(self, code: str) -> tuple[bool, list[str]]:
        """编译代码.

        Args:
            code: 源代码

        Returns:
            (成功, 错误列表)
        """

    @abstractmethod
    def execute(self, code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """执行代码.

        Args:
            code: 源代码
            inputs: 输入参数

        Returns:
            执行结果
        """

    def get_type_mapping(self, source_type: str, pic_clause: str = "") -> str:
        """获取类型映射.

        Args:
            source_type: 源类型
            pic_clause: PIC 子句（COBOL 特有）

        Returns:
            目标语言类型
        """
        # 默认实现，子类可覆盖
        mapping = {
            "numeric": "int",
            "decimal": "BigDecimal",
            "string": "String",
        }
        return mapping.get(source_type, "Object")

