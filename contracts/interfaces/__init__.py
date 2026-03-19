"""層間インターフェース Protocol 定義.

上位層が下位層の具象クラスに直接依存しないよう、
Protocol ベースのインターフェースを contracts 層に集約する。
"""

from contracts.interfaces.code_generator import ICodeGenerator
from contracts.interfaces.config_manager import IConfigManager
from contracts.interfaces.deploy_executor import IDeployExecutor


__all__ = [
    "ICodeGenerator",
    "IConfigManager",
    "IDeployExecutor",
]
