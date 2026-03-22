"""FAQ System API テスト."""
import json
import pathlib
import urllib.request

BASE = "http://localhost:8005"
OUT = pathlib.Path("api_test_results.json")

# ログイン
req = urllib.request.Request(
    f"{BASE}/api/auth/login",
    method="POST",
    headers={"Content-Type": "application/json"},
    data=json.dumps({"username": "admin", "password": "admin123"}).encode(),
)
with urllib.request.urlopen(req, timeout=10) as r:
    token = json.loads(r.read())["access_token"]

headers = {"Content-Type": "application/json", "Authorization": f"Bearer {token}"}
results: dict[str, dict] = {"login": {"status": "ok"}}


def chat(msg: str, label: str) -> None:
    """チャットAPIを呼び出し結果を記録."""
    req = urllib.request.Request(
        f"{BASE}/api/chat",
        method="POST",
        headers=headers,
        data=json.dumps({"message": msg}).encode(),
    )
    try:
        with urllib.request.urlopen(req, timeout=120) as r:
            d = json.loads(r.read())
            results[label] = {
                "status": d.get("status"),
                "query_type": d.get("query_type"),
                "answer": d.get("answer", "")[:300],
                "data_rows": len(d.get("data", [])),
                "has_chart": d.get("chart") is not None,
                "suggestions": len(d.get("suggestions", [])),
                "error": d.get("error", ""),
            }
    except Exception as e:
        results[label] = {"error": str(e)}


# テスト実行
chat("こんにちは", "chat_general")
chat("有給休暇の申請方法を教えてください", "faq_rag")
chat("売上トップ5の商品を教えて", "sql_analytics")
chat("先月の売上が低い理由と改善策を教えて", "hybrid")

OUT.write_text(json.dumps(results, ensure_ascii=False, indent=2) + "\n")
print("Results written to", OUT)

