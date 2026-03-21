#!/usr/bin/env python3
"""FAQ システム API ライブテストスクリプト（起動済みサーバー向け）."""
import json
import urllib.request
import urllib.error

BASE = "http://localhost:8005"


def req(method: str, path: str, data: dict | None = None, token: str | None = None) -> dict:
    """HTTPリクエストを送信してJSONを返す."""
    url = BASE + path
    body = json.dumps(data).encode() if data else None
    headers: dict[str, str] = {"Content-Type": "application/json"}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    request = urllib.request.Request(url, data=body, headers=headers, method=method)
    try:
        with urllib.request.urlopen(request, timeout=20) as resp:
            return json.loads(resp.read())  # type: ignore[no-any-return]
    except urllib.error.HTTPError as e:
        return {"error": e.code, "body": e.read().decode()}


def stream_test(message: str, token: str) -> None:
    """SSEストリームをテストする."""
    url = BASE + "/api/chat/stream"
    body = json.dumps({"message": message}).encode()
    headers = {"Content-Type": "application/json", "Authorization": f"Bearer {token}"}
    request = urllib.request.Request(url, data=body, headers=headers, method="POST")
    events: list[str] = []
    try:
        with urllib.request.urlopen(request, timeout=25) as resp:
            for raw_line in resp:
                line = raw_line.decode().strip()
                if not line.startswith("data: "):
                    continue
                raw = line[6:]
                if raw == "[DONE]":
                    break
                try:
                    ev = json.loads(raw)
                    ev_type = ev.get("type", "unknown")
                    events.append(ev_type)
                    if ev_type == "result":
                        d = ev.get("data", {})
                        print(f"  [result] query_type={d.get('query_type')}, "
                              f"chart={'yes' if d.get('chart') else 'no'}, "
                              f"suggestions={len(d.get('suggestions', []))}, "
                              f"data_rows={len(d.get('data', []))}")
                        print(f"  answer[:150]: {str(d.get('answer',''))[:150]}")
                        break
                except Exception:
                    pass
    except Exception as exc:
        print(f"  stream error: {exc}")
    print(f"  events seen: {events}")


def main() -> None:
    """メインテスト実行."""
    # 1. Login
    print("=== 1. Login ===")
    login = req("POST", "/api/auth/login", {"username": "admin", "password": "admin123"})
    token = login.get("access_token", "")
    print(f"Login: success={login.get('success')}, token={token[:20]}...")

    if not token:
        print("LOGIN FAILED - stopping")
        return

    # 2. Greeting
    print("\n=== 2. Greeting ===")
    r = req("POST", "/api/chat", {"message": "こんにちは"}, token)
    print(f"query_type: {r.get('query_type')}")
    print(f"answer: {str(r.get('answer',''))[:150]}")
    print(f"chart: {r.get('chart')}")
    print(f"suggestions: {r.get('suggestions')}")

    # 3. FAQ
    print("\n=== 3. FAQ (有給休暇) ===")
    r = req("POST", "/api/chat", {"message": "有給休暇の取得方法を教えてください"}, token)
    print(f"query_type: {r.get('query_type')}")
    print(f"answer: {str(r.get('answer',''))[:200]}")
    print(f"chart: {r.get('chart')}")
    print(f"docs: {len(r.get('documents', []))}")
    print(f"verification: {r.get('verification')}")

    # 4. SQL / analytics
    print("\n=== 4. SQL (売上) ===")
    r = req("POST", "/api/chat", {"message": "月別売上データを表示してください"}, token)
    print(f"query_type: {r.get('query_type')}")
    print(f"answer: {str(r.get('answer',''))[:200]}")
    print(f"chart type: {r.get('chart', {}).get('chart_type') if r.get('chart') else 'none'}")
    print(f"sql: {r.get('sql','')[:100]}")
    print(f"data rows: {len(r.get('data', []))}")
    print(f"columns: {r.get('columns')}")

    # 5. Stream test
    print("\n=== 5. SSE Stream (返品ポリシー) ===")
    stream_test("返品ポリシーを教えてください", token)

    # 6. Markdown/table/chart test
    print("\n=== 6. Markdown/chart test ===")
    r = req("POST", "/api/chat", {"message": "顧客満足度データをグラフで見せてください"}, token)
    print(f"query_type: {r.get('query_type')}")
    print(f"answer: {str(r.get('answer',''))[:200]}")
    print(f"chart: {r.get('chart')}")
    print(f"rich_response keys: {list((r.get('rich_response') or {}).keys())}")


if __name__ == "__main__":
    main()

