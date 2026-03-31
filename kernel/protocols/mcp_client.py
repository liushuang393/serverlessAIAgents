"""後方互換ブリッジ — 実装モジュールをそのまま公開する."""

from __future__ import annotations

import sys

from kernel.protocols.mcp import client as _client_impl


sys.modules[__name__] = _client_impl
