# 依赖安全管理指南 / Dependency Security Guide

## 概要

本ドキュメントは、プロジェクトの依存関係のセキュリティ管理について説明します。
依存関係の脆弱性を早期に発見し、適切に対応するためのガイドラインを提供します。

---

## 1. なぜ依存関係のセキュリティ問題が発生するか

### 1.1 原因

| 原因 | 説明 |
|------|------|
| **サードパーティライブラリの脆弱性** | lodash、vite等のライブラリに新しい脆弱性が発見される |
| **依存関係の連鎖** | 直接依存していなくても、間接依存で脆弱なパッケージが含まれる |
| **バージョン固定の欠如** | `^` や `~` でバージョン指定すると、意図しないバージョンがインストールされる可能性 |
| **更新の遅延** | 定期的な依存関係の更新を怠ると、脆弱性が蓄積する |

### 1.2 今回発生した問題

2026年1月に Dependabot が8件のセキュリティ/依存関係更新PRを作成:

- **lodash 4.17.21 → 4.17.23**: セキュリティ脆弱性修正（重要）
- **lucide-react**: lodash の間接依存更新含む
- **vite 6.x → 7.x**: メジャーバージョン更新
- **tailwindcss 3.x → 4.x**: メジャーバージョン更新
- **GitHub Actions**: codeql-action, setup-gcloud, codecov-action のバージョン更新

### 1.3 Dependabot PRを直接マージしなかった理由

1. **テスト未実施**: Dependabot は依存関係を更新するだけで、プロジェクト固有のテストを実行しない
2. **Breaking Changes**: メジャーバージョン更新には互換性の問題がある可能性
3. **ESLint 9 移行**: `eslint-plugin-react-hooks` v7 は ESLint 9 の flat config が必要

---

## 2. 予防策：セキュリティ問題を未然に防ぐ

### 2.1 定期的な依存関係監査

```bash
# フロントエンド (studio/)
cd studio && npm audit

# Python バックエンド
pip-audit  # pip install pip-audit

# または
safety check  # pip install safety
```

### 2.2 自動化ツールの設定

#### GitHub Dependabot（現在有効）
`.github/dependabot.yml` でセキュリティアラートを受信:

```yaml
version: 2
updates:
  - package-ecosystem: "npm"
    directory: "/studio"
    schedule:
      interval: "weekly"
  - package-ecosystem: "pip"
    directory: "/"
    schedule:
      interval: "weekly"
  - package-ecosystem: "github-actions"
    directory: "/"
    schedule:
      interval: "weekly"
```

#### npm audit の CI 統合
```yaml
# .github/workflows/security.yml
- name: Security Audit
  run: |
    cd studio && npm audit --audit-level=high
```

### 2.3 バージョン固定戦略

| パッケージタイプ | 推奨戦略 |
|------------------|----------|
| 本番依存 | マイナーバージョンまで固定 `"react": "~18.3.0"` |
| 開発依存 | パッチバージョン自動更新 `"vite": "^6.0.0"` |
| セキュリティ重要 | 完全固定 `"lodash": "4.17.23"` |

---

## 3. 検出方法：問題を早期発見する

### 3.1 GitHub Security Alerts

1. リポジトリの **Security** タブを確認
2. **Dependabot alerts** でセキュリティ脆弱性を確認
3. **Code scanning alerts** でコードの脆弱性を確認

### 3.2 ローカルでの確認コマンド

```bash
# フロントエンド脆弱性チェック
cd studio && npm audit

# 詳細レポート
cd studio && npm audit --json > audit-report.json

# 自動修正（安全な更新のみ）
cd studio && npm audit fix

# Python 依存関係チェック
pip-audit
```

### 3.3 CI/CD での自動チェック

PRおよびmainブランチへのプッシュ時に自動でセキュリティチェックを実行。

---

## 4. 対応方法：問題発生時の手順

### 4.1 緊急度の判断

| レベル | 条件 | 対応期限 |
|--------|------|----------|
| **Critical** | リモートコード実行、認証バイパス | 24時間以内 |
| **High** | 情報漏洩、DoS | 1週間以内 |
| **Medium** | 限定的な影響 | 1ヶ月以内 |
| **Low** | 理論的なリスク | 次回リリース時 |

### 4.2 更新手順

#### Step 1: 影響範囲の確認
```bash
# どのパッケージが影響を受けるか確認
npm ls <vulnerable-package>
```

#### Step 2: 更新の実施
```bash
# 特定パッケージの更新
cd studio && npm update <package-name>

# または package.json を編集して
cd studio && npm install
```

#### Step 3: テストの実行
```bash
# フロントエンド
cd studio && npm run lint && npm run type-check && npm run build

# バックエンド
pytest tests/ -v
```

#### Step 4: コミットとPR作成
```bash
git add -A
git commit -m "chore(deps): セキュリティ更新 - <package-name> を v<version> に更新"
git push origin <branch-name>
```

---

## 5. 今回の対応履歴

### 更新内容（2026-02-04）

#### フロントエンド (studio/package.json)
| パッケージ | 旧バージョン | 新バージョン | 理由 |
|------------|--------------|--------------|------|
| lucide-react | ^0.460.0 | ^0.563.0 | lodash セキュリティ修正含む |
| @typescript-eslint/* | ^8.15.0 | ^8.54.0 | バグ修正 |
| autoprefixer | ^10.4.20 | ^10.4.24 | バグ修正 |

#### ESLint 設定
- `.eslintrc.json` → `eslint.config.js` (ESLint 9 flat config 移行)
- `globals` パッケージ追加

#### 見送った更新（Breaking Changes のため）
| パッケージ | 理由 |
|------------|------|
| vite 6.x → 7.x | メジャーバージョン、要動作確認 |
| tailwindcss 3.x → 4.x | メジャーバージョン、CSS 構文変更あり |
| react-dom 18.x → 19.x | メジャーバージョン、要互換性確認 |

---

## 6. 関連ドキュメント

- [CONTRIBUTING.md](../CONTRIBUTING.md) - コントリビューションガイド
- [DEVELOPMENT_STANDARDS_JA.md](./DEVELOPMENT_STANDARDS_JA.md) - 開発標準
- [GitHub Dependabot Docs](https://docs.github.com/en/code-security/dependabot)

---

**最終更新**: 2026-02-04
**担当**: AgentFlow Team

