# 製品概要（3 Studio）

## 提供価値

- `Migration Studio`: 移行前評価、リスク可視化、提案作成
- `Enterprise FAQ Studio`: 引用付き回答、索引運用、運用レポート
- `Computer Assistant Studio`: 制御付き実行、監査ログ、記憶管理

## 外部向けアーキテクチャ（成果導線重視）

```mermaid
flowchart LR
    classDef studio fill:#E8F3FF,stroke:#0B5CAD,color:#0B3A66,stroke-width:1.5px
    classDef kernel fill:#EAF9EC,stroke:#2E8B57,color:#1A5A38,stroke-width:1.5px
    classDef govern fill:#FFF3E8,stroke:#C56A00,color:#7A3C00,stroke-width:1.5px
    classDef output fill:#F3F0FF,stroke:#5E43B5,color:#35246B,stroke-width:1.5px

    U["顧客ユーザー"] --> S["Studio 選択"]
    S --> M["Migration Studio"]
    S --> F["Enterprise FAQ Studio"]
    S --> A["Computer Assistant Studio"]

    M --> K["Kernel\n実行/制御"]
    F --> K
    A --> K

    K --> G["ガバナンス\nポリシー/監査/承認"]

    K --> O1["評価レポート"]
    K --> O2["FAQ サービス成果"]
    K --> O3["実行ログ/監査成果"]

    class M,F,A studio
    class K kernel
    class G govern
    class O1,O2,O3 output
```

## 顧客への説明定型

1. 3 Studio から目的に合う製品線を選択
2. テンプレートを選び、データ/権限を設定
3. 実行
4. 成果物を確認
