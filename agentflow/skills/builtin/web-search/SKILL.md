---
name: web-search
description: 网络检索 Skill - 实时获取互联网信息
version: 1.0.0
author: AgentFlow
tags:
  - search
  - web
  - retrieval
  - real-time
triggers:
  - 検索
  - search
  - 調べて
  - 最新
  - ニュース
  - 情報取得
requirements:
  - httpx
  - beautifulsoup4
dependencies: []
---

# Web Search Skill

実時間でインターネットから情報を検索・取得する能力。

## 機能

1. **マルチエンジン検索**: Google, Bing, DuckDuckGo 対応
2. **コンテンツ抽出**: 検索結果からテキスト抽出
3. **要約生成**: LLM による検索結果要約

## 使用方法

```python
from agentflow.skills.builtin.web_search import WebSearchSkill, SearchConfig

# 初期化
search = WebSearchSkill(SearchConfig(
    engine="duckduckgo",  # google, bing, duckduckgo
    max_results=5,
    language="ja",
))

# 検索実行
results = await search.search("AgentFlow AI フレームワーク")

for r in results:
    print(f"タイトル: {r.title}")
    print(f"URL: {r.url}")
    print(f"スニペット: {r.snippet}")

# コンテンツ取得
content = await search.fetch_content(results[0].url)
print(content.text)

# 検索 + 要約（LLM 使用）
summary = await search.search_and_summarize(
    query="最新のAI技術動向",
    question="2024年のAI技術で最も注目すべきトレンドは？"
)
print(summary.answer)
```

## 設定オプション

| パラメータ | 説明 | デフォルト |
|-----------|------|-----------|
| engine | 検索エンジン | duckduckgo |
| max_results | 最大結果数 | 5 |
| language | 言語コード | ja |
| timeout | タイムアウト秒 | 10 |
| safe_search | セーフサーチ | true |

## Decision Engine での使用例

```python
class CognitiveGateAgent:
    """認知門 - 情報不足時に自動検索"""
    
    def __init__(self):
        self.search = WebSearchSkill()
    
    async def enrich_context(self, question: str, missing_info: list[str]) -> dict:
        """不足情報を検索で補完"""
        enriched = {}
        
        for info in missing_info:
            results = await self.search.search_and_summarize(
                query=f"{question} {info}",
                question=f"{info}について教えてください"
            )
            enriched[info] = results.answer
        
        return enriched
```

## エラーハンドリング

```python
from agentflow.skills.builtin.web_search import SearchError, RateLimitError

try:
    results = await search.search(query)
except RateLimitError:
    # レートリミット - 待機して再試行
    await asyncio.sleep(60)
    results = await search.search(query)
except SearchError as e:
    # 検索失敗 - フォールバック
    logger.warning(f"Search failed: {e}")
    results = []
```

## プライバシー考慮

- DuckDuckGo をデフォルトで使用（プライバシー重視）
- 検索クエリはログに記録しない
- ユーザーの同意なく個人情報を検索しない

