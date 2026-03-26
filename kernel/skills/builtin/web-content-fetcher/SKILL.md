---
name: web-content-fetcher
description: Webページ本文を Markdown へ整理して取得する互換スキル
version: "1.1.0"
author: AgentFlow
tags:
  - web
  - fetch
  - markdown
  - retrieval
triggers:
  - Webページの正文を取得
  - URLの記事を読み取り
  - 网页内容を抽出
  - 提取链接正文
requirements:
  - requests
  - html2text
---

# Web Content Fetcher

既存互換を維持しつつ、`web-read` 共通パイプラインへ段階移行するスキルです。

## フォールバック戦略

1. Jina / Markdown 直接取得
2. Scrapling 相当の HTML 読取（互換）
3. requests + HTML 変換

## 実行スクリプト

- `fetch.py`
