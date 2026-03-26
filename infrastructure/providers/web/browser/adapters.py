"""Web browser operation provider 実装."""

from __future__ import annotations

import importlib
import json
import os
import shlex
from typing import Any

from infrastructure.browser.browser_skill import BrowserSkill
from infrastructure.browser.config import BrowserSkillConfig
from infrastructure.providers.web.browser.base import (
    BrowserOperationRequest,
    BrowserOperationResult,
    BrowserOperator,
)


class LegacyPlaywrightBrowserOperator(BrowserOperator):
    """既存 BrowserSkill を使う互換 operator."""

    name = "legacy_playwright_browser_operator"

    async def run(self, request: BrowserOperationRequest) -> BrowserOperationResult:
        config = BrowserSkillConfig(domain_whitelist=request.allowed_domains or [])
        try:
            async with BrowserSkill(config) as browser:
                await browser.navigate(request.url)
                for step in request.steps:
                    action = step.action
                    selector = step.selector or ""
                    text = step.text or ""
                    if action == "click" and selector:
                        await browser.click(selector)
                    elif action in {"type", "fill"} and selector:
                        await browser.type_text(selector, text)
                    elif action == "wait" and step.timeout_ms:
                        await browser.get_text(step.selector or "body")
                markdown = None
                if request.return_markdown:
                    content = await browser.get_text("body")
                    markdown = content.strip()
                return BrowserOperationResult(
                    ok=True,
                    markdown=markdown,
                    metadata={"provider": self.name},
                )
        except Exception as exc:
            return BrowserOperationResult(
                ok=False,
                markdown=None,
                metadata={"provider": self.name, "error": str(exc)},
            )


class MCPBrowserOperator(BrowserOperator):
    """Playwright MCP 直結 operator."""

    name = "mcp_browser_operator"

    def __init__(self) -> None:
        self._server_name = os.getenv("PLAYWRIGHT_MCP_SERVER_NAME", "playwright")

    async def run(self, request: BrowserOperationRequest) -> BrowserOperationResult:
        try:
            async with self._build_client() as client:
                tools = client.list_tools()
                if not tools:
                    return BrowserOperationResult(
                        ok=False,
                        markdown=None,
                        metadata={"provider": self.name, "reason": "no_mcp_tools"},
                    )

                artifacts: dict[str, Any] = {}
                await self._call_first_available(
                    client=client,
                    aliases=["browser_navigate", "navigate", "open_page", "goto"],
                    arguments={"url": request.url},
                )
                for step in request.steps:
                    step_result = await self._execute_step(client=client, step=step)
                    if step.download_name and step_result is not None:
                        artifacts[step.download_name] = step_result

                markdown: str | None = None
                if request.return_markdown:
                    markdown = await self._read_markdown(client)

                return BrowserOperationResult(
                    ok=True,
                    markdown=markdown,
                    artifacts=artifacts,
                    metadata={"provider": self.name, "tool_count": len(tools), "server_name": self._server_name},
                )
        except Exception as exc:
            return BrowserOperationResult(
                ok=False,
                markdown=None,
                metadata={"provider": self.name, "error": str(exc)},
            )

    def _build_client(self) -> Any:
        mcp_client_module = importlib.import_module("kernel.protocols.mcp_client")
        mcp_config_module = importlib.import_module("kernel.protocols.mcp_config")
        mcp_client_cls = mcp_client_module.MCPClient
        mcp_config_cls = mcp_config_module.MCPConfig
        mcp_server_config_cls = mcp_config_module.MCPServerConfig
        command = os.getenv("PLAYWRIGHT_MCP_COMMAND", "npx").strip() or "npx"
        raw_args = os.getenv("PLAYWRIGHT_MCP_ARGS", "@playwright/mcp@latest")
        args = shlex.split(raw_args) if raw_args.strip() else ["@playwright/mcp@latest"]
        raw_env = os.getenv("PLAYWRIGHT_MCP_ENV", "")
        env = json.loads(raw_env) if raw_env.strip() else {}
        config = mcp_config_cls(
            servers=[
                mcp_server_config_cls(
                    name=self._server_name,
                    command=command,
                    args=args,
                    env=env if isinstance(env, dict) else {},
                    enabled=True,
                    description="Playwright MCP",
                )
            ]
        )
        return mcp_client_cls(config, enable_security=False, timeout=60.0)

    async def _execute_step(self, *, client: Any, step: Any) -> Any:
        if step.action == "click" and step.selector:
            return await self._call_first_available(
                client=client,
                aliases=["browser_click", "click"],
                arguments={"selector": step.selector},
            )
        if step.action in {"type", "fill"} and step.selector:
            return await self._call_first_available(
                client=client,
                aliases=["browser_type", "browser_fill_form", "fill", "type"],
                arguments={
                    "selector": step.selector,
                    "text": step.text or "",
                    "fields": {step.selector: step.text or ""},
                },
            )
        if step.action == "wait":
            wait_text = step.wait_for or step.selector or ""
            return await self._call_first_available(
                client=client,
                aliases=["browser_wait_for", "wait_for"],
                arguments={"text": wait_text, "timeout_ms": step.timeout_ms or 30000},
                allow_missing=True,
            )
        if step.action == "download" and step.selector:
            return await self._call_first_available(
                client=client,
                aliases=["browser_click", "click"],
                arguments={"selector": step.selector},
            )
        if step.action in {"expand", "paginate"} and step.selector:
            return await self._call_first_available(
                client=client,
                aliases=["browser_click", "click"],
                arguments={"selector": step.selector},
            )
        if step.action == "navigate" and step.wait_for:
            return await self._call_first_available(
                client=client,
                aliases=["browser_navigate", "navigate", "open_page", "goto"],
                arguments={"url": step.wait_for},
            )
        return None

    async def _read_markdown(self, client: Any) -> str | None:
        result = await self._call_first_available(
            client=client,
            aliases=["browser_snapshot", "browser_get_text", "snapshot", "get_text"],
            arguments={"selector": "body"},
        )
        if isinstance(result, str):
            return result.strip()
        if isinstance(result, list):
            return "\n".join(str(item) for item in result).strip()
        if isinstance(result, dict):
            for key in ("markdown", "text", "content", "snapshot"):
                value = result.get(key)
                if isinstance(value, str):
                    return value.strip()
        return None

    async def _call_first_available(
        self,
        *,
        client: Any,
        aliases: list[str],
        arguments: dict[str, Any],
        allow_missing: bool = False,
    ) -> Any:
        for alias in aliases:
            tool_uri = f"mcp://{self._server_name}/{alias}"
            if tool_uri not in client.list_tools():
                continue
            result = await client.call_tool(tool_uri, arguments)
            if result.get("success"):
                return result.get("result")
        if allow_missing:
            return None
        msg = f"Playwright MCP tool not found for aliases: {aliases}"
        raise RuntimeError(msg)
