"""Deployment Manager 异常定义."""


class DeploymentError(Exception):
    """部署基础异常."""

    def __init__(self, message: str, code: str | None = None) -> None:
        """初始化异常."""
        super().__init__(message)
        self.message = message
        self.code = code


class BuildError(DeploymentError):
    """构建错误."""



class ConfigError(DeploymentError):
    """配置错误."""



class DomainError(DeploymentError):
    """域名错误."""



class RollbackError(DeploymentError):
    """回滚错误."""



class ProviderError(DeploymentError):
    """提供商 API 错误."""



class TimeoutError(DeploymentError):
    """超时错误."""


