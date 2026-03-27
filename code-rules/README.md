# 📋 AgentFlow コードルール体系 - AI 開発者ガイド

> **バージョン**: 1.0.0
> **最終更新**: 2026-01-19
> **対象読者**: AI 開発者、プロンプトエンジニア、コードレビュアー

## 🎯 このガイドについて

このドキュメントは、**AgentFlow プロジェクトのコードルール体系**を効果的に活用して AI（Claude、GPT、Gemini など）を開発・コード生成に導くための実践ガイドです。

**AI の弱点を補強し、人間らしい洞察を提供する**ための体系的なアプローチを学びましょう。

---

## 📖 目次

1. [ルール体系の概要](#ルール体系の概要)
2. [AI 開発時の基本フロー](#ai-開発時の基本フロー)
3. [グローバルルールの適用](#グローバルルールの適用)
4. [言語別・プロジェクト別ルールの選択](#言語別・プロジェクト別ルールの選択)
5. [AI が気づきにくい問題への対処](#ai-が気づきにくい問題への対処)
6. [プロンプトテンプレート](#プロンプトテンプレート)
7. [実践例](#実践例)
8. [トラブルシューティング](#トラブルシューティング)

---

## 🏗️ ルール体系の概要

### 階層構造

```
📁 code-rules/
├── 📄 CLAUDE.md                    # 🔍 インデックス（最初に読む）
├── 📁 global/                      # 🌍 普遍的な原則
├── 📁 languages/                   # 🐍 言語固有ルール
├── 📁 project/                     # 🏢 AgentFlow 固有（例: Agent Lightning 統合規約）
├── 📁 company-specific/            # 🏭 企業固有指針
└── 📁 tools/                       # 🛠️ 自動化ツール
```

### 適用優先順位

1. **🔴 グローバルルール**（必須）- 品質の基盤
2. **🟡 言語別ルール**（推奨）- Python の特徴
3. **🟢 プロジェクトルール**（状況次第）- AgentFlow の文脈
4. **🔵 企業固有ルール**（任意）- 組織の慣習

学習連携（Agent Lightning backend）を扱う場合は、`project/agent-lightning-integration.md` を追加で必ず参照してください。

---

## 🚀 AI 開発時の基本フロー

### Step 1: 事前準備（5分）

```bash
# 1. インデックスを読む
cat code-rules/CLAUDE.md

# 2. グローバルルールを確認
head -20 code-rules/global/principles.md
head -20 code-rules/global/naming-guidelines.md

# 3. 言語ルールを確認
head -20 code-rules/languages/python-rules.md

# 4. 学習連携を扱う場合のみ確認
head -40 code-rules/project/agent-lightning-integration.md
```

### Step 2: AI への指示作成（10分）

```markdown
あなたは Python エキスパート開発者です。以下のルール体系に従って AgentFlow プロジェクトのコードを生成してください：

## 必須遵守ルール

- [global/principles.md](global/principles.md) の開発原則
- [global/naming-guidelines.md](global/naming-guidelines.md) の命名規則
- [languages/python-rules.md](languages/python-rules.md) の Python ルール

## 品質チェック

- 型アノテーション 100%
- エラーハンドリング完全
- 非同期 I/O のみ使用
```

### Step 3: コード生成（開発時間）

```markdown
## タスク定義

[具体的な実装要求]

## AI への追加指示

- BizCore の 7コア層アーキテクチャを遵守
- @agent デコレータを優先使用
- 統一 Provider API（get_llm()）を使用
```

### Step 4: レビュー・修正（10分）

- 生成されたコードをルール体系で検証
- 不足している点を AI にフィードバック

---

## 🌍 グローバルルールの適用

### 品質優先の原則

**🎯 AI 指示例:**

```markdown
## 品質要件（最優先）

- 型安全: すべての関数/パラメータに型アノテーション
- リントクリーン: Ruff check が通るコード
- テスト可能: 各関数が独立してテスト可能
- エラーハンドリング: 想定される例外をすべて処理

## なぜ重要か

AI はしばしば「動けばいい」コードを書きますが、人間らしい保守性が必要です。
```

### 命名規則の徹底

**🎯 AI 指示例:**

```markdown
## 命名規則（厳守）

- クラス: PascalCase（CodeGenerator）
- 関数: snake_case（generate_code）
- 定数: UPPER_SNAKE_CASE（MAX_RETRIES）
- 非公開: アンダースコア接頭語（\_internal_method）

## 命名意図の明確化

- 関数名から何をするかがわかるようにする
- 変数名から何を表すかがわかるようにする
```

### エラーハンドリングの完全性

**🎯 AI 指示例:**

```markdown
## エラー処理（完全網羅）

- bare except 禁止
- 具体的な例外クラスを使用
- エラーメッセージは具体的で実用的
- リソース解放は確実に行う

## AI の弱点補強

AI は正常系しか考えないので、異常系を明示的に要求する。
```

---

## 🎯 言語別・プロジェクト別ルールの選択

### Python 開発時の選択

| 状況               | 適用ルール                                                  | 理由                     |
| ------------------ | ----------------------------------------------------------- | ------------------------ |
| **新規クラス作成** | `global/naming-guidelines.md` + `languages/python-rules.md` | 命名 + Python イディオム |
| **非同期処理**     | `languages/python-rules.md` の非同期章                      | AgentFlow は完全非同期   |
| **Agent 実装**     | `company-specific/agentflow-python.md`                      | AgentFlow 固有パターン   |

### AgentFlow 固有開発時の選択

| コンポーネント    | 適用ルール                             | 追加指示                 |
| ----------------- | -------------------------------------- | ------------------------ |
| **Agent クラス**  | `company-specific/agentflow-python.md` | @agent デコレータ使用    |
| **ワークフロー**  | `project/architecture.md`              | 7コア層 + Apps外層遵守   |
| **Provider 統合** | `company-specific/agentflow-python.md` | 松耦合 Provider パターン |

### 機能別のルール適用

#### 新規機能開発

```markdown
## 適用ルール

1. global/principles.md - 開発原則
2. global/naming-guidelines.md - 命名規則
3. languages/python-rules.md - Python 標準
4. company-specific/agentflow-python.md - AgentFlow パターン

## 追加チェック

- 7コア層 + Apps外層適合性
- Skills 統合可能性
- テスト容易性
```

#### リファクタリング

```markdown
## 適用ルール

1. global/principles.md - 改善原則
2. project/architecture.md - アーキテクチャ整合性
3. global/style-formatting.md - フォーマット統一

## 注意点

- 後方互換性の維持
- パフォーマンス影響の評価
```

#### デバッグ・修正

```markdown
## 適用ルール

1. global/error-handling.md - エラーハンドリング強化
2. global/security-standards.md - セキュリティ確認
3. tools/testing.md - テスト戦略

## 追加指示

- 根本原因の特定
- 再発防止策の実装
```

---

## ⚠️ AI が気づきにくい問題への対処

### 1. アーキテクチャ境界の無視

**🤖 AI の問題:**

```python
# AI が書いてしまうコード
class AgentBlock:
    def __init__(self, db_connection):  # 下位層への直接依存
        self.db = db_connection
```

**🧠 人間の修正:**

```python
# 正しいアーキテクチャ
class AgentBlock:
    def __init__(self, data_provider: IDataProvider):  # インターフェース経由
        self.data_provider = data_provider
```

**🎯 対処法:**

```markdown
## アーキテクチャ境界の厳守

- 上位層は下位層を知らない
- 依存はインターフェース経由のみ
- 循環依存を避ける

AI指示: "7コア層 + Apps外層の層間依存を厳守せよ"
```

### 2. セキュリティ考慮の欠如

**🤖 AI の問題:**

```python
# AI が書いてしまうコード
def get_user_data(user_id):
    query = f"SELECT * FROM users WHERE id = {user_id}"  # SQL インジェクション危険
    return db.execute(query)
```

**🧠 人間の修正:**

```python
def get_user_data(self, user_id: str) -> dict[str, Any] | None:
    if not user_id or not isinstance(user_id, str):
        raise ValueError("有効な user_id が必要です")

    query = "SELECT id, name, email FROM users WHERE id = ?"
    result = await self.db.execute(query, (user_id,))
    return result.fetchone() if result else None
```

**🎯 対処法:**

```markdown
## セキュリティ強化指示

- 入力検証を最初に行う
- パラメータ化クエリを使用
- 最小権限の原則
- エラーメッセージに情報漏洩なし

AI指示: "セキュリティを最優先で考慮せよ"
```

### 3. パフォーマンスの考慮不足

**🤖 AI の問題:**

```python
# AI が書いてしまうコード
def process_large_data(data):
    result = []
    for item in data:  # 非効率なループ
        if item['status'] == 'active':
            result.append(process_item(item))  # 同期処理
    return result
```

**🧠 人間の修正:**

```python
async def process_large_data(self, data: list[dict[str, Any]]) -> list[dict[str, Any]]:
    """大規模データを並行処理."""
    # フィルタリング
    active_items = [item for item in data if item.get('status') == 'active']

    # 並行処理（セマフォで制御）
    semaphore = asyncio.Semaphore(10)  # 最大10並行

    async def process_with_limit(item):
        async with semaphore:
            return await self.process_item(item)

    tasks = [process_with_limit(item) for item in active_items]
    return await asyncio.gather(*tasks)
```

**🎯 対処法:**

```markdown
## パフォーマンス最適化指示

- 非同期処理を活用
- メモリ使用量を考慮
- アルゴリズムの時間計算量を評価
- 並行処理の限界を制御

AI指示: "パフォーマンスを考慮した実装をせよ"
```

### 4. ドキュメントの不足

**🤖 AI の問題:**

```python
# AI が書いてしまうコード
def complex_calculation(a, b, c):
    # 複雑な計算だが説明なし
    return (a + b) * c / (a - b) if a != b else 0
```

**🧠 人間の修正:**

```python
def complex_calculation(
    self,
    principal: float,
    rate: float,
    time_periods: int,
) -> float:
    """複利計算を行う.

    Args:
        principal: 元本
        rate: 利率（例: 0.05 = 5%）
        time_periods: 期間数

    Returns:
        計算結果の金額

    Raises:
        ValueError: principal が0以下の場合

    Example:
        >>> calc = FinanceCalculator()
        >>> calc.complex_calculation(1000, 0.05, 12)
        1617.07
    """
    if principal <= 0:
        raise ValueError("元本は正の値である必要があります")
    if rate < 0:
        raise ValueError("利率は0以上である必要があります")

    return principal * (1 + rate) ** time_periods
```

**🎯 対処法:**

```markdown
## ドキュメント充実指示

- Google スタイル Docstring
- Args/Returns/Raises の完全記述
- 実用的使用例の記載
- 複雑なロジックの説明

AI指示: "完全なドキュメントを書け"
```

---

## 📝 プロンプトテンプレート

### 基本テンプレート

```markdown
# AgentFlow コード生成プロンプト

あなたは **Senior Python Developer** であり、**AgentFlow フレームワークのエキスパート**です。

## 必須遵守ルール

### グローバルルール（最優先）

- **[品質優先](global/principles.md)**: 型安全、リントクリーン、テスト可能
- **[命名規則](global/naming-guidelines.md)**: 意図が明確な命名
- **[エラーハンドリング](global/error-handling.md)**: 完全な例外処理

### 言語ルール

- **[Python 標準](languages/python-rules.md)**: 非同期優先、型アノテーション 100%

### プロジェクトルール

- **[アーキテクチャ](project/architecture.md)**: 7コア層構造遵守
- **[AgentFlow 特有](company-specific/agentflow-python.md)**: @agent デコレータ、統一 Provider

## タスク

[具体的な実装要求をここに書く]

## 品質チェック

- [ ] 型アノテーション 100%
- [ ] エラーハンドリング完全
- [ ] 非同期 I/O のみ
- [ ] 7コア層 + Apps外層遵守
- [ ] テスト容易性確保

## 追加指示

- 保守性を最優先
- パフォーマンスを考慮
- セキュリティを強化
- ドキュメントを充実
```

### 機能追加テンプレート

```markdown
# 新機能追加プロンプト

## 機能概要

[新機能の説明]

## 実装要件

- グローバルルール遵守
- AgentFlow パターン適用
- テストコード同梱

## AI への特別指示

- アーキテクチャ適合性を確認
- 既存コードとの統合性を確保
- 後方互換性を維持

## レビュー観点

- セキュリティホールなし
- パフォーマンス影響最小
- ドキュメント更新完了
```

### リファクタリングテンプレート

```markdown
# コードリファクタリングプロンプト

## リファクタリング対象

[対象コードの場所と内容]

## 改善目標

- 可読性向上
- 保守性強化
- パフォーマンス改善

## 制約条件

- 外部インターフェース変更なし
- 後方互換性維持
- テストカバレッジ維持

## AI への指示

- グローバルルールの再適用
- アーキテクチャ整合性の確認
- ドキュメントの更新
```

---

## 💡 実践例

### 例1: 新しい Agent 作成

**🎯 プロンプト:**

```markdown
以下のルール体系に従って、FAQ Agent を作成してください：

## 必須ルール

- global/principles.md の原則
- company-specific/agentflow-python.md の Agent パターン
- languages/python-rules.md の Python 標準

## 機能要件

- 質問を受け付けて回答
- データベースから関連情報を検索
- 構造化された回答を返す

## 品質要件

- 型アノテーション 100%
- エラーハンドリング完全
- 非同期処理のみ
- テストコード同梱
```

### 例2: Provider 統合

**🎯 プロンプト:**

```markdown
## タスク

OpenAI Provider を統合せよ

## 適用ルール

- company-specific/agentflow-python.md の統一 Provider パターン
- global/error-handling.md のエラー処理
- languages/python-rules.md の非同期処理

## 実装要件

- 松耦合設計
- 環境変数自動検出
- タイムアウト処理
- リトライロジック
```

### 例3: テストコード生成

**🎯 プロンプト:**

```markdown
## テスト生成依頼

[対象コード] の包括的なテストを作成せよ

## 適用ルール

- tools/testing.md のテスト戦略
- global/principles.md のテストファースト原則
- languages/python-rules.md の Python テストパターン

## カバレッジ要件

- ユニットテスト 100%
- 境界値テスト網羅
- エラーケーステスト
```

---

## 🔧 トラブルシューティング

### AI がルールを無視する場合

**対処法:**

1. **明確な参照**: "このルールファイルの内容を厳守せよ"
2. **優先順位指定**: "グローバルルールを最優先"
3. **具体例提示**: 良い例と悪い例を示す
4. **段階的指示**: 一度に一つのルールだけ指示

### 生成コードが動かない場合

**対処法:**

1. **依存関係確認**: 必要なモジュールがインポートされているか
2. **環境変数**: 必要な設定が揃っているか
3. **型エラー**: mypy で型チェック
4. **実行時エラー**: スタックトレースを AI にフィードバック

### 品質が低い場合

**対処法:**

1. **具体的なフィードバック**: "この部分がルール違反"
2. **改善例提示**: 正しいコード例を示す
3. **段階的改善**: 一度に一つの問題を修正
4. **チェックリスト使用**: 品質チェック項目を明示

### プロンプトが長すぎる場合

**対処法:**

1. **ファイル参照**: "このファイルの内容を参照せよ"
2. **段階的適用**: 基本ルール → 詳細ルールの順
3. **テンプレート使用**: 上記のプロンプトテンプレート活用
4. **優先順位付け**: 重要なルールから適用

---

## 🎯 効果的な AI 活用のコツ

### 1. 明確な期待値設定

- **何を期待しているか** を具体的に伝える
- **品質基準** を数値化して伝える
- **使用例** を具体的に示す

### 2. 段階的アプローチ

- **基本構造** から始めて徐々に詳細化
- **フィードバックループ** を活用
- **改善提案** を積極的に取り入れる

### 3. 品質ゲートの活用

- **自動チェック** を活用（lint, type check, test）
- **レビュー観点** を明確に伝える
- **継続的改善** を意識

### 4. ドメイン知識の共有

- **AgentFlow の文脈** を説明
- **既存コード例** を参照
- **設計意図** を共有

---

## 📚 関連リソース

- **[CLAUDE.md](CLAUDE.md)**: ルール体系インデックス
- **[ai-extract.py](ai-extract.py)**: AI ルール自動抽出スクリプト
- **[generate-zip.py](generate-zip.py)**: ルールパッケージ生成スクリプト
- **[プロジェクトルート](../../../README.md)**: AgentFlow 概要

---

## 🤝 貢献・改善

このガイドは継続的に改善されています。効果的な AI 活用法を見つけたら、ぜひ共有してください！

**あなたの経験が他の開発者を助けます！** 🚀

_最終更新: 2026-01-19 | AgentFlow AI 開発ガイド_
