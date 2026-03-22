"""Playwright UI 証跡テスト.

auth_service の Swagger UI を使って認可エンドポイントの動作を証跡として記録する。
スクリーンショットは tests/evidence/auth_authorization/ に保存。

前提条件:
- auth_service が http://localhost:8010 で稼働中
- playwright がインストール済み

実行方法:
    pytest tests/apps/auth_service/test_authorization_e2e_playwright.py -v --headed
"""

from __future__ import annotations

import os
from pathlib import Path

import pytest


# Playwright がインストールされていない場合はスキップ
pytest.importorskip("playwright")

EVIDENCE_DIR = Path("tests/evidence/auth_authorization")
BASE_URL = os.environ.get("AUTH_SERVICE_URL", "http://localhost:8010")


@pytest.fixture(autouse=True)
def _ensure_evidence_dir() -> None:
    """証跡ディレクトリを作成."""
    EVIDENCE_DIR.mkdir(parents=True, exist_ok=True)


@pytest.fixture
def _skip_if_no_server() -> None:
    """auth_service が起動していない場合はスキップ."""
    try:
        import httpx

        resp = httpx.get(f"{BASE_URL}/health", timeout=3)
        if resp.status_code != 200:
            pytest.skip("auth_service が応答しません")
    except Exception:
        pytest.skip("auth_service に接続できません")


@pytest.mark.skipif(
    os.environ.get("CI") == "true",
    reason="CI 環境ではブラウザテストをスキップ",
)
class TestPlaywrightEvidence:
    """Playwright UI 証跡テスト."""

    @pytest.mark.usefixtures("_skip_if_no_server")
    async def test_swagger_authorization_endpoints(self) -> None:
        """Swagger UI の認可エンドポイント一覧スクリーンショット."""
        from playwright.async_api import async_playwright

        async with async_playwright() as p:
            browser = await p.chromium.launch(headless=True)
            page = await browser.new_page()

            await page.goto(f"{BASE_URL}/docs")
            await page.wait_for_load_state("networkidle")
            await page.wait_for_timeout(2000)

            await page.screenshot(
                path=str(EVIDENCE_DIR / "01_swagger_authorization_endpoints.png"),
                full_page=True,
            )
            await browser.close()

    @pytest.mark.usefixtures("_skip_if_no_server")
    async def test_admin_login_and_roles(self) -> None:
        """admin ログイン → ロール一覧 API 実行."""
        import httpx
        from playwright.async_api import async_playwright

        # まず API でトークンを取得
        async with httpx.AsyncClient() as client:
            login_resp = await client.post(
                f"{BASE_URL}/auth/login",
                json={
                    "username": "admin",
                    "password": "admin123",
                },
            )
            token = login_resp.json().get("access_token", "")

            # ロール一覧取得
            roles_resp = await client.get(
                f"{BASE_URL}/auth/authorization/roles",
                headers={"Authorization": f"Bearer {token}"},
            )

        async with async_playwright() as p:
            browser = await p.chromium.launch(headless=True)
            page = await browser.new_page()

            # ロール一覧結果をページに表示
            roles_json = roles_resp.json() if roles_resp.status_code == 200 else {"error": "failed"}
            import json

            await page.set_content(f"""
                <html><body style="font-family:monospace;padding:20px">
                <h2>認可ロール一覧 (admin)</h2>
                <pre>{json.dumps(roles_json, indent=2, ensure_ascii=False)}</pre>
                </body></html>
            """)
            await page.screenshot(
                path=str(EVIDENCE_DIR / "02_admin_roles_list.png"),
            )
            await browser.close()

    @pytest.mark.usefixtures("_skip_if_no_server")
    async def test_authorization_check_evidence(self) -> None:
        """認可チェック API 実行（allowed/denied）."""
        import httpx
        from playwright.async_api import async_playwright

        results: list[dict[str, object]] = []

        async with httpx.AsyncClient() as client:
            # admin ログイン
            admin_login = await client.post(
                f"{BASE_URL}/auth/login",
                json={
                    "username": "admin",
                    "password": "admin123",
                },
            )
            admin_token = admin_login.json().get("access_token", "")

            # employee ログイン
            emp_login = await client.post(
                f"{BASE_URL}/auth/login",
                json={
                    "username": "suzuki",
                    "password": "suzuki123",
                },
            )
            emp_token = emp_login.json().get("access_token", "")

            # admin チェック → allowed
            resp1 = await client.post(
                f"{BASE_URL}/auth/authorization/check",
                headers={"Authorization": f"Bearer {admin_token}"},
                json={"permission": "faq:write"},
            )
            results.append({"user": "admin", "permission": "faq:write", "result": resp1.json()})

            # employee チェック → denied
            resp2 = await client.post(
                f"{BASE_URL}/auth/authorization/check",
                headers={"Authorization": f"Bearer {emp_token}"},
                json={"permission": "faq:write"},
            )
            results.append({"user": "employee", "permission": "faq:write", "result": resp2.json()})

        async with async_playwright() as p:
            browser = await p.chromium.launch(headless=True)
            page = await browser.new_page()
            import json

            await page.set_content(f"""
                <html><body style="font-family:monospace;padding:20px">
                <h2>認可チェック結果</h2>
                <pre>{json.dumps(results, indent=2, ensure_ascii=False)}</pre>
                </body></html>
            """)
            await page.screenshot(
                path=str(EVIDENCE_DIR / "03_authorization_check_results.png"),
            )
            await browser.close()

    @pytest.mark.usefixtures("_skip_if_no_server")
    async def test_resource_permission_evidence(self) -> None:
        """リソースパーミッション設定証跡."""
        import httpx
        from playwright.async_api import async_playwright

        async with httpx.AsyncClient() as client:
            login_resp = await client.post(
                f"{BASE_URL}/auth/login",
                json={
                    "username": "admin",
                    "password": "admin123",
                },
            )
            token = login_resp.json().get("access_token", "")
            headers = {"Authorization": f"Bearer {token}"}

            # リソースパーミッション一覧取得
            rp_resp = await client.get(
                f"{BASE_URL}/auth/authorization/resource-permissions",
                headers=headers,
            )

        async with async_playwright() as p:
            browser = await p.chromium.launch(headless=True)
            page = await browser.new_page()
            import json

            rp_data = rp_resp.json() if rp_resp.status_code == 200 else {"error": "failed"}
            await page.set_content(f"""
                <html><body style="font-family:monospace;padding:20px">
                <h2>リソースパーミッション一覧</h2>
                <pre>{json.dumps(rp_data, indent=2, ensure_ascii=False)}</pre>
                </body></html>
            """)
            await page.screenshot(
                path=str(EVIDENCE_DIR / "04_resource_permissions.png"),
            )
            await browser.close()
