"""Platform App shell.

実装本体は top-level ``platform.api.app`` に集約し、
このモジュールは互換入口だけを保持する。
"""

from platform.api.app import (
    DEFAULT_API_HOST,
    DEFAULT_API_PORT,
    app,
    cli_components_list,
    cli_dashboard,
    cli_publish,
    cli_search,
    create_app,
    main,
    setup_logging,
)


__all__ = [
    "DEFAULT_API_HOST",
    "DEFAULT_API_PORT",
    "app",
    "cli_components_list",
    "cli_dashboard",
    "cli_publish",
    "cli_search",
    "create_app",
    "main",
    "setup_logging",
]


if __name__ == "__main__":
    main()
