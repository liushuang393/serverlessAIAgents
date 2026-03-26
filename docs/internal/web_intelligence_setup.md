# Web Intelligence Setup

Web Intelligence 7章のローカル導入手順と検証手順。

## 必須環境変数

`.env` で管理する。コードへのハードコードは禁止。

### Search API

- `BING_SEARCH_API_KEY`
- `BING_SEARCH_ENDPOINT`
- `SERPAPI_API_KEY`
- `SERPAPI_ENDPOINT`
- `TAVILY_API_KEY`
- `TAVILY_SEARCH_ENDPOINT`

いずれかの key がある場合、default search provider は external を使う。

### Cloudflare Browser Rendering

- `CLOUDFLARE_ACCOUNT_ID`
- `CLOUDFLARE_API_TOKEN`

未設定時、`rendered_markdown` provider は失敗理由を metadata に返す。

### Playwright MCP

- `PLAYWRIGHT_MCP_SERVER_NAME`
- `PLAYWRIGHT_MCP_COMMAND`
- `PLAYWRIGHT_MCP_ARGS`
- `PLAYWRIGHT_MCP_ENV`

既定値:

- command: `npx`
- args: `@playwright/mcp@latest`

## Skill Entry Points

- `kernel/skills/builtin/web-intelligence-router`
- `kernel/skills/builtin/web-search`
- `kernel/skills/builtin/web-read`
- `kernel/skills/builtin/web-extract`
- `kernel/skills/builtin/web-operate`
- `kernel/skills/builtin/web-crawl`
- `kernel/skills/builtin/web-content-fetcher`

`web-operate` は `steps` を必須で受け取り、typed `BrowserActionStep` に変換する。

## テスト手順

Web 関連の最小確認:

```bash
conda run -n agentflow pytest \
  tests/contracts/test_web_contracts.py \
  tests/unit/test_web_provider_chain_and_utils.py \
  tests/unit/test_web_intelligence_router.py \
  tests/integration/boundary/test_boundary_scripts.py \
  tests/integration/test_web_intelligence_pipeline.py \
  tests/integration/test_web_content_fetcher_compatibility.py \
  --no-cov -q
```

総合確認:

```bash
conda run -n agentflow ./check.sh all
```

## 補足

- integration/e2e は live 外部依存を避け、mock または local fixture を優先する
- browser backend の default は `mcp`
- 互換目的で `legacy_playwright` 実装を残している
