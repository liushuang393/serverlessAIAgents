# Kernel Skills

スキルシステムの概要と構成。

## ディレクトリ構成

- `base.py` - Skill 基底クラス（SKILL.md 読み込み・メタデータ管理）
- `loader.py` - Skill ローダー（ビルトイン・カスタムスキルの検出）
- `runtime.py` - Skill ランタイム（スクリプト実行エンジン）
- `builtin/` - ビルトインスキル群

## SKILL.md フォーマット

各スキルは `SKILL.md` ファイルで定義されます。
Claude Code Skills フォーマットと互換性があります。
