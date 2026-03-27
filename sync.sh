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

<<<<<<< HEAD
# リモートの変更を取り込む
echo "⬇️  リモートから取得中..."
BRANCH=$(git rev-parse --abbrev-ref HEAD)
git fetch origin

# ローカルとリモートの分岐を確認
LOCAL=$(git rev-parse HEAD)
REMOTE=$(git rev-parse "origin/$BRANCH")
BASE=$(git merge-base HEAD "origin/$BRANCH")

if [ "$LOCAL" = "$REMOTE" ]; then
    echo "ℹ️  既に最新"
elif [ "$LOCAL" = "$BASE" ]; then
    # ローカルが遅れている → fast-forward で安全に取り込む
    echo "⬇️  fast-forward で取り込み..."
    git merge --ff-only "origin/$BRANCH"
elif [ "$REMOTE" = "$BASE" ]; then
    # リモートが遅れている → push するだけ
    echo "ℹ️  リモートが遅れている（push で解決）"
else
    # 分岐している → マージで統合（ローカルの変更を失わない）
    echo "⚠️  ブランチが分岐しています。マージで統合します..."
    git merge "origin/$BRANCH" -m "merge: sync remote changes $(date '+%Y-%m-%d %H:%M')"
=======
# リモートの変更を取り込む（rebase で分岐を解決）
echo "⬇️  リモートから取得中..."
if ! git pull --rebase; then
    echo "⚠️  コンフリクトが発生しました"
    echo "   手動で解決後、以下を実行してください:"
    echo "     git add <解決したファイル>"
    echo "     git rebase --continue"
    echo "     git push"
    exit 1
>>>>>>> 5fc1624f (auto: sync 2026-03-27 17:02)
fi

# リモートへプッシュ
echo "⬆️  リモートへプッシュ中..."
git push

echo "🎉 同期完了！"
