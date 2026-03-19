"""層間インターフェース Protocol 定義.

上位層が下位層の具象クラスに直接依存しないよう、
Protocol ベースのインターフェースを contracts 層に集約する。
"""

from contracts.interfaces.publish import (
    ICodeGenerator,
    IConfigManager,
    IDeployExecutor,
)


__all__ = [
    "ICodeGenerator",
    "IConfigManager",
    "IDeployExecutor",
]
