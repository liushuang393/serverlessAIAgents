#!/usr/bin/env python3
"""FAQ APIテスト（直接実行用）."""

import json
import urllib.error
import urllib.request


BASE = "http://localhost:8005"


def chat(msg: str, token: str) -> dict:
    """チャットAPIを呼び出す."""
    req = urllib.request.Request(
        f"{BASE}/api/chat",
        data=json.dumps({"message": msg}).encode(),
        headers={"Content-Type": "application/json", "Authorization": f"Bearer {token}"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=120) as r:
            return json.loads(r.read())
    except urllib.error.HTTPError as e:
        return {"error": e.code, "body": e.read().decode()[:300]}
    except Exception as ex:
        return {"error": str(ex)}


def main() -> None:
    """テスト実行."""
    # Login
    login_req = urllib.request.Request(
        f"{BASE}/api/auth/login",
        data=json.dumps({"username": "admin", "password": "admin123"}).encode(),
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    with urllib.request.urlopen(login_req, timeout=10) as r:
        login_data = json.loads(r.read())
    token = login_data["access_token"]
    print(f"Login OK: {token[:20]}...")

    tests = [
        ("Greeting", "こんにちは"),
        ("FAQ", "有給休暇の取得方法を教えてください"),
        ("SQL", "月別売上データを表示してください"),
    ]

    for name, msg in tests:
        print(f"\n=== {name}: {msg} ===")
        r = chat(msg, token)
        print(f"  query_type: {r.get('query_type')}")
        print(f"  answer[:200]: {str(r.get('answer', ''))[:200]}")
        if r.get("error"):
            print(f"  error: {r.get('error')}")
        if r.get("chart"):
            print(f"  chart_type: {r['chart'].get('chart_type')}")
        if r.get("data"):
            print(f"  data rows: {len(r['data'])}")
        if r.get("suggestions"):
            print(f"  suggestions: {[s.get('text', '')[:30] for s in r['suggestions']]}")


if __name__ == "__main__":
    main()
