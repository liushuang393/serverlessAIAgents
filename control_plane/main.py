"""BizCore control plane entrypoint."""

from control_plane.api.app import (
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
