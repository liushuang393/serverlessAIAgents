# レイヤー設計

## 1. 論理レイヤー

1. Product Surface（3 Studio）
2. Platform API Surface（`/api/studios/*`）
3. Kernel（Loader / Runner / Policy Hook / Audit / Runtime Context）
4. Plugin Blocks（Tool / SkillPack / Protocol Adapter / Channel / Deploy / Runner）
5. Governance（Risk / Permission / Signature / Audit）

## 2. 設計原則

1. 製品固有ロジックを Kernel に入れない。
2. プロトコルの具体実装を Kernel に入れない。
3. 副作用操作は必ずガバナンス連鎖を通す。
4. Core 追加前に公式 Plugin 化を検討する。

## 3. 内部向けレイヤー図（境界重視）

```mermaid
flowchart TB
    classDef surface fill:#E8F3FF,stroke:#0B5CAD,color:#0B3A66,stroke-width:1.5px
    classDef kernel fill:#EAF9EC,stroke:#2E8B57,color:#1A5A38,stroke-width:1.5px
    classDef plugin fill:#FFF7E8,stroke:#C56A00,color:#7A3C00,stroke-width:1.5px
    classDef gov fill:#F4EDFF,stroke:#6A48C8,color:#3A297A,stroke-width:1.5px

    S["Studio/API Surface"] --> K["Kernel"]
    K --> P["Plugin Blocks"]
    K --> G["Governance"]
    P --> G

    class S surface
    class K kernel
    class P plugin
    class G gov
```
