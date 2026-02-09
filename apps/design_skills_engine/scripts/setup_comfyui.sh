#!/usr/bin/env bash
# ==============================================================================
# ComfyUI モデルセットアップスクリプト
# ==============================================================================
# SDXL Base 1.0 モデルを HuggingFace からダウンロードする。
#
# 使用方法:
#   ./scripts/setup_comfyui.sh
#   MODELS_DIR=/custom/path ./scripts/setup_comfyui.sh
# ==============================================================================

set -euo pipefail

# ---------------------------------------------------------------------------
# 設定
# ---------------------------------------------------------------------------
MODELS_DIR="${MODELS_DIR:-$(dirname "$0")/../models}"
CHECKPOINTS_DIR="${MODELS_DIR}/checkpoints"

MODEL_NAME="sd_xl_base_1.0.safetensors"
MODEL_URL="https://huggingface.co/stabilityai/stable-diffusion-xl-base-1.0/resolve/main/${MODEL_NAME}"
MODEL_PATH="${CHECKPOINTS_DIR}/${MODEL_NAME}"

# ---------------------------------------------------------------------------
# メイン処理
# ---------------------------------------------------------------------------
echo "=== ComfyUI モデルセットアップ ==="
echo "モデルディレクトリ: ${CHECKPOINTS_DIR}"

# ディレクトリ作成
mkdir -p "${CHECKPOINTS_DIR}"

# モデルが既に存在する場合はスキップ
if [ -f "${MODEL_PATH}" ]; then
    FILE_SIZE=$(stat -c%s "${MODEL_PATH}" 2>/dev/null || stat -f%z "${MODEL_PATH}" 2>/dev/null || echo 0)
    echo "モデルは既にダウンロード済みです: ${MODEL_PATH} (${FILE_SIZE} bytes)"
    echo "再ダウンロードする場合はファイルを削除してください。"
    exit 0
fi

echo "SDXL Base 1.0 をダウンロード中... (~6.9GB)"
echo "URL: ${MODEL_URL}"

# wget または curl でダウンロード (レジューム対応)
if command -v wget &>/dev/null; then
    wget -c -O "${MODEL_PATH}" "${MODEL_URL}"
elif command -v curl &>/dev/null; then
    curl -C - -L -o "${MODEL_PATH}" "${MODEL_URL}"
else
    echo "エラー: wget または curl が必要です。"
    exit 1
fi

echo "ダウンロード完了: ${MODEL_PATH}"
echo "=== セットアップ完了 ==="
