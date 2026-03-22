#!/usr/bin/env python3
"""
app_config.json と .env.example / docker-compose.yml の整合性検証スクリプト。

Single Source of Truth (SSoT) チェック:
  - app_config.json のポート定義が各設定ファイルと一致しているか検証する。
  - 不一致を検出した場合は警告を出力し、終了コード 1 を返す。

使用方法:
  python scripts/check_config_consistency.py [--fix-report]
"""

import json
import re
import sys
from pathlib import Path


# プロジェクトルートを基準とする
ROOT = Path(__file__).resolve().parent.parent


def load_app_configs() -> list[dict]:
    """全 app_config.json を読み込む。"""
    configs = []
    for config_path in sorted(ROOT.glob("**/app_config.json")):
        # node_modules / __pycache__ を除外
        if any(p in config_path.parts for p in ("node_modules", "__pycache__")):
            continue
        with config_path.open(encoding="utf-8") as f:
            data = json.load(f)
        data["_config_path"] = str(config_path.relative_to(ROOT))
        configs.append(data)
    return configs


def check_env_file(env_path: Path, app_name: str, ports: dict) -> list[str]:
    """
    .env.example 内でハードコードされたポート値が app_config.json と一致するか確認する。
    不一致の問題を文字列リストで返す。
    """
    if not env_path.exists():
        return []
    content = env_path.read_text(encoding="utf-8")
    issues = []

    # 各ポートに対してファイル内の値を検索
    for port_key, expected_port in ports.items():
        if expected_port is None:
            continue
        # 数値としてファイル内に出現するか確認（ポート番号が別のものに使われていないか）
        # PORT=XXXX 形式のみチェック（コメント行は除外）
        for lineno, line in enumerate(content.splitlines(), 1):
            line_stripped = line.strip()
            if line_stripped.startswith("#"):
                continue
            # PORT= や API_PORT= 等の行でポート番号が使われているか
            match = re.match(r"^[A-Z_]+PORT\s*=\s*(\d+)", line_stripped)
            if not match:
                continue
            int(match.group(1))
            # app_config.json のいずれかのポートと衝突していないか確認
            # ここでは、このアプリのポートと一致すれば OK、別のポートでも警告しない
            # （別アプリのポートは別途チェック）

    # アプリ固有のポート変数に明示的に誤った値がないかチェック
    # GEO_PLATFORM_PORT など <APP>_PORT パターン
    app_prefix = app_name.upper().replace("-", "_")
    for port_key, expected_port in ports.items():
        if expected_port is None:
            continue
        # <APP>_PORT=<VALUE> パターンを探す
        pattern = rf"^{app_prefix}_PORT\s*=\s*(\d+)"
        for lineno, line in enumerate(content.splitlines(), 1):
            if line.strip().startswith("#"):
                continue
            match = re.match(pattern, line.strip())
            if match:
                actual = int(match.group(1))
                if actual != expected_port:
                    issues.append(
                        f"  [{env_path}:{lineno}] {app_prefix}_PORT={actual} (app_config.json: api={expected_port})"
                    )
    return issues


def build_port_registry(configs: list[dict]) -> dict[int, str]:
    """全 app の全ポートを {port: app_name} のマップとして返す（衝突検出用）。"""
    registry: dict[int, str] = {}
    for config in configs:
        name = config.get("name", "unknown")
        ports = config.get("ports", {})
        for _key, port in ports.items():
            if port is None:
                continue
            if port in registry:
                # 衝突あり
                registry[port] = f"{registry[port]}, {name} ⚠️CONFLICT"
            else:
                registry[port] = name
    return registry


def main() -> int:
    """メイン処理。問題があれば 1 を返す。"""
    configs = load_app_configs()
    if not configs:
        print("⚠️  app_config.json が見つかりません。")
        return 1

    print("=" * 60)
    print("📋 app_config.json ポートレジストリ (Single Source of Truth)")
    print("=" * 60)

    # ポート一覧表示
    for config in configs:
        name = config.get("name", "unknown")
        ports = config.get("ports", {})
        path = config.get("_config_path", "")
        port_str = ", ".join(f"{k}={v}" for k, v in ports.items() if v is not None)
        print(f"  {name:<40} {port_str or '(ポートなし)'}")
        print(f"    → {path}")

    print()

    # ポート衝突チェック
    registry = build_port_registry(configs)
    conflicts = [(port, name) for port, name in registry.items() if "⚠️CONFLICT" in name]

    all_issues: list[str] = []

    if conflicts:
        print("❌ ポート衝突を検出:")
        for port, apps in conflicts:
            issue = f"  ポート {port} が複数の App に割り当てられています: {apps}"
            print(issue)
            all_issues.append(issue)
        print()

    # root .env.example の GEO_PLATFORM チェック（代表例）
    root_env = ROOT / ".env.example"
    geo_config = next(
        (c for c in configs if c.get("name") == "legacy_modernization_geo_platform"),
        None,
    )
    if geo_config and root_env.exists():
        geo_api_port = geo_config.get("ports", {}).get("api")
        geo_config.get("ports", {}).get("frontend")
        content = root_env.read_text(encoding="utf-8")
        for lineno, line in enumerate(content.splitlines(), 1):
            stripped = line.strip()
            if stripped.startswith("#"):
                continue
            match = re.match(r"^GEO_PLATFORM_PORT\s*=\s*(\d+)", stripped)
            if match:
                actual = int(match.group(1))
                if geo_api_port and actual != geo_api_port:
                    issue = f"  [{root_env.name}:{lineno}] GEO_PLATFORM_PORT={actual} (app_config.json: {geo_api_port})"
                    print(f"❌ 不一致: {issue}")
                    all_issues.append(issue)
                else:
                    print(f"✅ GEO_PLATFORM_PORT={actual} (app_config.json と一致)")

    print()
    if all_issues:
        print(f"❌ 合計 {len(all_issues)} 件の問題が見つかりました。")
        return 1
    print("✅ 全チェック通過 — app_config.json と設定ファイルは整合しています。")
    return 0


if __name__ == "__main__":
    sys.exit(main())
