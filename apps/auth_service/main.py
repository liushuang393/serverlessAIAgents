"""auth_service shell.

正規実装は top-level ``platform.api.auth_app`` に集約し、
このモジュールは互換入口だけを保持する。
"""

from platform.api.auth_app import app, create_app, lifespan, main


__all__ = ["app", "create_app", "lifespan", "main"]


if __name__ == "__main__":
    main()
