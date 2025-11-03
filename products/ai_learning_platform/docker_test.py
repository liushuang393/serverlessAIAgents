#!/usr/bin/env python3
"""
Docker設定の妥当性をチェックするテストスクリプト
"""

import os
import yaml
import json
from pathlib import Path

def check_dockerfile_exists():
    """Dockerfileの存在確認"""
    backend_dockerfile = Path("backend/Dockerfile")
    frontend_dockerfile = Path("frontend/Dockerfile")
    
    print("=== Dockerfileの存在確認 ===")
    print(f"Backend Dockerfile: {'✓' if backend_dockerfile.exists() else '✗'}")
    print(f"Frontend Dockerfile: {'✓' if frontend_dockerfile.exists() else '✗'}")
    
    return backend_dockerfile.exists() and frontend_dockerfile.exists()

def check_docker_compose():
    """docker-compose.ymlの妥当性確認"""
    compose_file = Path("docker-compose.yml")
    
    print("\n=== docker-compose.yml確認 ===")
    if not compose_file.exists():
        print("✗ docker-compose.ymlが存在しません")
        return False
    
    try:
        with open(compose_file, 'r', encoding='utf-8') as f:
            compose_config = yaml.safe_load(f)
        
        # 必要なサービスの確認
        required_services = ['backend', 'frontend', 'db']
        services = compose_config.get('services', {})
        
        print("サービス確認:")
        for service in required_services:
            exists = service in services
            print(f"  {service}: {'✓' if exists else '✗'}")
        
        # ポート設定の確認
        print("\nポート設定:")
        for service_name, service_config in services.items():
            ports = service_config.get('ports', [])
            if ports:
                print(f"  {service_name}: {ports}")
        
        # 環境変数の確認
        print("\n環境変数設定:")
        backend_env = services.get('backend', {}).get('environment', [])
        for env in backend_env:
            print(f"  Backend: {env}")
        
        return True
        
    except yaml.YAMLError as e:
        print(f"✗ YAML解析エラー: {e}")
        return False

def check_requirements():
    """依存関係ファイルの確認"""
    print("\n=== 依存関係ファイル確認 ===")
    
    # Backend requirements.txt
    backend_req = Path("backend/requirements.txt")
    print(f"Backend requirements.txt: {'✓' if backend_req.exists() else '✗'}")
    
    if backend_req.exists():
        with open(backend_req, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        print(f"  依存関係数: {len([l for l in lines if l.strip() and not l.startswith('#')])}")
    
    # Frontend package.json
    frontend_pkg = Path("frontend/package.json")
    print(f"Frontend package.json: {'✓' if frontend_pkg.exists() else '✗'}")
    
    if frontend_pkg.exists():
        with open(frontend_pkg, 'r', encoding='utf-8') as f:
            pkg_data = json.load(f)
        deps = pkg_data.get('dependencies', {})
        print(f"  依存関係数: {len(deps)}")

def check_config_files():
    """設定ファイルの確認"""
    print("\n=== 設定ファイル確認 ===")
    
    config_files = [
        "nginx.conf",
        "mysql_init_db.sql"
    ]
    
    for config_file in config_files:
        path = Path(config_file)
        exists = path.exists()
        print(f"{config_file}: {'✓' if exists else '✗'}")
        
        if exists and path.stat().st_size > 0:
            print(f"  サイズ: {path.stat().st_size} bytes")
        elif exists:
            print("  ⚠️ ファイルが空です")

def generate_deployment_instructions():
    """デプロイ手順の生成"""
    print("\n=== デプロイ手順 ===")
    instructions = """
Docker展開手順:

1. 環境変数の設定:
   export OPENAI_API_KEY=your-openai-api-key-here

2. コンテナのビルドと起動:
   docker compose up --build -d

3. ログの確認:
   docker compose logs -f

4. アクセス確認:
   - フロントエンド: http://localhost:3000
   - バックエンドAPI: http://localhost:8000
   - Nginx (リバースプロキシ): http://localhost:80

5. 停止:
   docker compose down

6. データベースデータも削除する場合:
   docker compose down -v
"""
    print(instructions)

def main():
    """メイン実行関数"""
    print("AI学習プラットフォーム Docker設定チェック")
    print("=" * 50)
    
    # 各チェックを実行
    dockerfile_ok = check_dockerfile_exists()
    compose_ok = check_docker_compose()
    check_requirements()
    check_config_files()
    
    # 総合判定
    print("\n=== 総合判定 ===")
    if dockerfile_ok and compose_ok:
        print("✓ Docker設定は正常です")
        generate_deployment_instructions()
    else:
        print("✗ Docker設定に問題があります")
        print("上記のエラーを修正してから再度実行してください")

if __name__ == "__main__":
    main()
