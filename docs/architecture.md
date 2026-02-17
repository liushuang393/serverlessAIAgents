# アーキテクチャ概要

## 1. 全体方針

- 対外提供は 3 Studio を主線にする。
- Kernel は安定境界のみを保持する。
- 機能拡張は Plugin First で行う。

## 2. 外部向けビュー（成果導線）

```mermaid
flowchart LR
    C["顧客"] --> S["3 Studio"]
    S --> K["Kernel"]
    K --> O["成果物"]
```

## 3. 内部向けビュー（境界/ガバナンス）

```mermaid
flowchart LR
    subgraph Product
      ST["Studio Surface"]
      FW["Framework Surface"]
    end

    subgraph Kernel
      L["Loader"]
      R["Runner"]
      P["Policy Hook"]
      A["Audit"]
      C["Runtime Context"]
    end

    subgraph Plugins
      T["Tool"]
      S["SkillPack"]
      X["Protocol/Channel"]
      D["Deploy/Runner"]
    end

    ST --> R
    FW --> R
    R --> T
    R --> S
    R --> X
    R --> D
    R --> P
    R --> A
    C --> R
```

## 4. 正規 API 面

- Studio: `/api/studios/*`
- Framework: `/api/studios/framework/*`

旧 `/api/agents` などの経路は提供しない。
