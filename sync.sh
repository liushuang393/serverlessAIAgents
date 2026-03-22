#!/bin/bash
# sync.sh - 2台PC間の簡単同期スクリプト
# 使い方: ./sync.sh
# または: ./sync.sh "コミットメッセージ"

set -e

MSG=${1:-"auto: sync $(date '+%Y-%m-%d %H:%M')"}

echo "🔄 同期開始..."

# 変更があればコミット
if ! git diff --quiet || ! git diff --cached --quiet || git ls-files --others --exclude-standard | grep -q .; then
    git add -A
    git commit -m "$MSG"
    echo "✅ ローカル変更をコミット"
else
    echo "ℹ️  コミットなし（変更なし）"
fi

# リモートの変更を取り込む（rebase）
echo "⬇️  リモートから取得中..."
git pull

# リモートへプッシュ
echo "⬆️  リモートへプッシュ中..."
git push

echo "🎉 同期完了！"

