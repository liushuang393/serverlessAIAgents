# AgentFlow

**3 Studio 製品線 + Kernel Plugin 基盤**

- Migration Studio
- Enterprise FAQ Studio
- Computer Assistant Studio

言語: [English](README_EN.md) | [简体中文](README_ZH.md) | 日本語

## 製品方針

1. 顧客導線を単純化する（テンプレート選択 → 設定 → 実行 → 成果物）
2. 機能拡張は Plugin First で行う
3. Kernel は安定境界のみを保持する

## アーキテクチャ

```mermaid
flowchart LR
    S["3 Studio"] --> K["Kernel"]
    K --> P["Plugin Blocks"]
    K --> G["Governance"]
```

## API

- Studio: `/api/studios/*`
- Framework: `/api/studios/framework/*`

## クイックスタート

1. `conda activate agentflow`
2. `pip install -e "[apps,dev]"`
3. `python -m apps.platform.main serve --port 8000`
4. `cd apps/platform/frontend && npm install && npm run dev`

## ドキュメント

- 目次: `docs/index.md`
- 外部向け: `docs/external/README.md`
- 内部向け: `docs/internal/README.md`
- 技術構成: `docs/architecture.md`
