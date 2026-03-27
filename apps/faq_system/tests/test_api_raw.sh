#!/bin/bash
# FAQ API raw test script
set -e

echo "=== Login ==="
curl -s -X POST http://localhost:8005/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"admin123"}' \
  -o /tmp/faq_login.json
python3 -c "import json; d=json.load(open('/tmp/faq_login.json')); print('success:', d.get('success')); open('/tmp/faq_token.txt','w').write(d.get('access_token',''))"

TOKEN=$(cat /tmp/faq_token.txt)
echo "Token: ${TOKEN:0:20}..."

echo ""
echo "=== Chat POST (greeting) ==="
curl -v -X POST http://localhost:8005/api/chat \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"message":"こんにちは"}' \
  2>&1 | grep -E "^[<>*]|HTTP|{" | head -30

echo ""
echo "=== Chat STREAM (FAQ) ==="
curl -s -X POST http://localhost:8005/api/chat/stream \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{"message":"返品ポリシーを教えてください"}' \
  --max-time 30 2>&1 | head -30
