#!/usr/bin/env bash
set -euo pipefail

# 目的: ローカル開発環境の auth_service DB 上で FAQ デモユーザーを復旧する。
# 入力: 第1引数=ユーザー名（既定: admin）、第2引数=新パスワード（既定: admin123）。
# 注意: Docker の auth-db コンテナと Python 実行環境（agentflow 推奨）が必要。

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
AUTH_DB_CONTAINER="${AUTH_DB_CONTAINER:-auth-db}"
AUTH_DB_NAME="${AUTH_DB_NAME:-auth_service}"
AUTH_DB_USER="${AUTH_DB_USER:-postgres}"
TARGET_USERNAME="${1:-admin}"
TARGET_PASSWORD="${2:-admin123}"

if [[ ! "${TARGET_USERNAME}" =~ ^[A-Za-z0-9_.-]+$ ]]; then
  echo "[reset_admin] エラー: ユーザー名は英数字・._- のみ指定できます: ${TARGET_USERNAME}" >&2
  exit 1
fi

cd "${REPO_ROOT}"

if command -v python >/dev/null 2>&1; then
  PYTHON_CMD=(python)
else
  PYTHON_CMD=(python3)
fi

if ! command -v docker >/dev/null 2>&1; then
  echo "[reset_admin] エラー: docker コマンドが見つかりません。" >&2
  exit 1
fi

if ! docker ps --format '{{.Names}}' | grep -Fxq "${AUTH_DB_CONTAINER}"; then
  echo "[reset_admin] auth-db コンテナが未起動のため起動します..."
  docker compose -f "${REPO_ROOT}/apps/auth_service/docker-compose.yml" up -d auth-db >/dev/null
fi

for _ in $(seq 1 30); do
  if docker exec "${AUTH_DB_CONTAINER}" pg_isready -U "${AUTH_DB_USER}" -d "${AUTH_DB_NAME}" >/dev/null 2>&1; then
    break
  fi
  sleep 1
done

if ! docker exec "${AUTH_DB_CONTAINER}" pg_isready -U "${AUTH_DB_USER}" -d "${AUTH_DB_NAME}" >/dev/null 2>&1; then
  echo "[reset_admin] エラー: auth-db の起動確認に失敗しました。" >&2
  exit 1
fi

PASSWORD_ROW="$(TARGET_PASSWORD="${TARGET_PASSWORD}" "${PYTHON_CMD[@]}" -c '
import os
from apps.auth_service.core.password import PasswordManager

target_password = os.environ["TARGET_PASSWORD"]
manager = PasswordManager(iterations=200000)
salt = manager.generate_salt()
password_hash = manager.hash_password(target_password, salt)
print(f"{salt}\t{password_hash}")
')"
IFS=$'\t' read -r PASSWORD_SALT PASSWORD_HASH <<<"${PASSWORD_ROW}"

if [[ -z "${PASSWORD_SALT}" || -z "${PASSWORD_HASH}" ]]; then
  echo "[reset_admin] エラー: パスワードハッシュ生成結果が空です。" >&2
  exit 1
fi

echo "[reset_admin] ${TARGET_USERNAME} を復旧します..."
docker exec \
  -e TARGET_USERNAME="${TARGET_USERNAME}" \
  -e PASSWORD_SALT="${PASSWORD_SALT}" \
  -e PASSWORD_HASH="${PASSWORD_HASH}" \
  "${AUTH_DB_CONTAINER}" \
  psql -v ON_ERROR_STOP=1 -U "${AUTH_DB_USER}" -d "${AUTH_DB_NAME}" -c "
UPDATE user_accounts
SET password_salt = '${PASSWORD_SALT}',
    password_hash = '${PASSWORD_HASH}',
    login_attempts = 0,
    locked_until = NULL,
    updated_at = NOW()
WHERE username = '${TARGET_USERNAME}';

SELECT username, login_attempts, locked_until, is_active, auth_source
FROM user_accounts
WHERE username = '${TARGET_USERNAME}';
"

echo "[reset_admin] 完了: username=${TARGET_USERNAME} password=${TARGET_PASSWORD}"
echo "[reset_admin] 確認コマンド: curl -X POST http://127.0.0.1:8005/api/auth/login -H 'Content-Type: application/json' -d '{\"username\":\"${TARGET_USERNAME}\",\"password\":\"${TARGET_PASSWORD}\"}'"