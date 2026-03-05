---
category: code_development
name: deployment-manager
description: |
  デプロイ管理を統一的に扱うインターフェース。
  Vercel、Cloudflare Pages/Workers、AWS Lambda などに対応し、
  ワンクリックデプロイ、プレビュー環境、ロールバック、環境変数管理、ドメイン設定を提供。
version: 1.0.0
author: AgentFlow Team
triggers:
  - deploy
  - デプロイ
  - vercel
  - cloudflare
  - aws
  - lambda
  - serverless
  - hosting
  - preview
  - rollback
  - domain
requirements:
  - httpx>=0.25.0
tags:
  - deployment
  - infrastructure
  - serverless
  - production-ready
examples:
  - "Vercel へデプロイ"
  - "プレビュー環境を作成"
  - "前のバージョンへロールバック"
  - "カスタムドメインを設定"
allowed-tools:
  - Bash
  - Read
  - Write
user-invocable: true
---

# Deployment Manager Skill

## 概要

统一的部署管理接口，支持主流 Serverless 平台，实现一键部署和环境管理。

## 対応プラットフォーム

| 平台 | 类型 | 免费额度 | 特点 |
|------|------|----------|------|
| **Vercel** | Serverless | 100GB 带宽/月 | Next.js 优化、Preview 部署 |
| **Cloudflare Pages** | Edge | 无限请求 | 全球 CDN、边缘计算 |
| **Cloudflare Workers** | Edge Function | 100k 请求/天 | 超低延迟、KV 存储 |
| **AWS Lambda** | Serverless | 1M 请求/月 | AWS 生态、高度可配置 |

## クイックスタート

### 1. Vercel 部署

```python
from agentflow.skills.builtin.deployment_manager import (
    DeploymentManager,
    VercelConfig,
)

# 配置
config = VercelConfig(
    token="your_vercel_token",
    team_id="team_xxx",  # 可选
)

# 初始化
deployer = DeploymentManager(provider="vercel", config=config)

# 部署项目
deployment = await deployer.deploy(
    project_name="my-agent-app",
    source_path="./dist",  # 构建产物目录
    environment="production",
    env_vars={
        "DATABASE_URL": "postgres://...",
        "API_KEY": "xxx",
    },
)

print(f"部署 URL: {deployment.url}")
print(f"状态: {deployment.status}")
```

### 2. Cloudflare Pages 部署

```python
from agentflow.skills.builtin.deployment_manager import (
    DeploymentManager,
    CloudflareConfig,
)

config = CloudflareConfig(
    api_token="your_cf_token",
    account_id="your_account_id",
)

deployer = DeploymentManager(provider="cloudflare_pages", config=config)

deployment = await deployer.deploy(
    project_name="my-agent-app",
    source_path="./dist",
    branch="main",  # 分支名
)
```

### 3. Cloudflare Workers 部署

```python
deployer = DeploymentManager(provider="cloudflare_workers", config=config)

# 部署 Worker
deployment = await deployer.deploy_worker(
    name="my-api-worker",
    script_path="./worker.js",
    routes=["api.example.com/*"],
    kv_namespaces=["MY_KV"],
)
```

## 预览环境

### 自动 Preview 部署

```python
# 为 PR 创建预览环境
preview = await deployer.create_preview(
    project_name="my-agent-app",
    source_path="./dist",
    branch="feature/new-ui",
    pr_number=123,
)

print(f"预览 URL: {preview.url}")
# 例如: https://my-agent-app-pr-123.vercel.app

# PR 合并后自动清理
await deployer.delete_preview(preview.id)
```

### GitHub Actions 集成

```yaml
# .github/workflows/preview.yml
name: Preview Deployment

on:
  pull_request:
    types: [opened, synchronize]

jobs:
  preview:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Build
        run: npm run build
      
      - name: Deploy Preview
        uses: agentflow/deploy-action@v1
        with:
          provider: vercel
          token: ${{ secrets.VERCEL_TOKEN }}
          project: my-agent-app
          source: ./dist
          environment: preview
          
      - name: Comment PR
        uses: actions/github-script@v7
        with:
          script: |
            github.rest.issues.createComment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              issue_number: context.issue.number,
              body: '🚀 Preview: ${{ steps.deploy.outputs.url }}'
            })
```

## 回滚

### 快速回滚

```python
# 获取部署历史
deployments = await deployer.list_deployments(
    project_name="my-agent-app",
    limit=10,
)

for d in deployments:
    print(f"{d.id}: {d.created_at} - {d.status}")

# 回滚到指定版本
await deployer.rollback(
    project_name="my-agent-app",
    deployment_id="dpl_xxx",  # 或 version_number
)

# 回滚到上一个版本
await deployer.rollback_to_previous("my-agent-app")
```

### 蓝绿部署

```python
# 创建新部署但不激活
deployment = await deployer.deploy(
    project_name="my-agent-app",
    source_path="./dist",
    auto_assign_domains=False,  # 不自动绑定域名
)

# 测试新部署
# ...

# 切换流量
await deployer.promote_deployment(
    project_name="my-agent-app",
    deployment_id=deployment.id,
)
```

## 环境变量管理

### 设置环境变量

```python
# 单个变量
await deployer.set_env_var(
    project_name="my-agent-app",
    key="API_KEY",
    value="xxx",
    environment="production",  # production/preview/development
    encrypted=True,  # 加密存储
)

# 批量设置
await deployer.set_env_vars(
    project_name="my-agent-app",
    env_vars={
        "DATABASE_URL": "postgres://...",
        "REDIS_URL": "redis://...",
        "API_KEY": "xxx",
    },
    environment="production",
)

# 从 .env 文件导入
await deployer.import_env_file(
    project_name="my-agent-app",
    file_path=".env.production",
    environment="production",
)
```

### 获取环境变量

```python
# 列出所有变量
env_vars = await deployer.list_env_vars(
    project_name="my-agent-app",
    environment="production",
)

for var in env_vars:
    print(f"{var.key}: {'*****' if var.encrypted else var.value}")
```

## 域名管理

### 绑定自定义域名

```python
# 添加域名
domain = await deployer.add_domain(
    project_name="my-agent-app",
    domain="app.example.com",
)

print(f"DNS 记录: {domain.dns_records}")
# [
#   {"type": "CNAME", "name": "app", "value": "cname.vercel-dns.com"},
# ]

# 验证域名
verified = await deployer.verify_domain(
    project_name="my-agent-app",
    domain="app.example.com",
)

# SSL 证书自动配置
```

### 域名重定向

```python
# 添加重定向
await deployer.add_redirect(
    project_name="my-agent-app",
    source="www.example.com",
    destination="example.com",
    permanent=True,  # 301 重定向
)
```

## 监控与日志

### 部署状态监控

```python
# 获取部署状态
status = await deployer.get_deployment_status(
    project_name="my-agent-app",
    deployment_id="dpl_xxx",
)

print(f"状态: {status.state}")  # building/ready/error
print(f"构建时间: {status.build_time}s")
print(f"URL: {status.url}")

# 等待部署完成
deployment = await deployer.wait_for_deployment(
    project_name="my-agent-app",
    deployment_id="dpl_xxx",
    timeout=300,  # 5分钟超时
)
```

### 实时日志

```python
# 获取构建日志
async for log in deployer.stream_build_logs(
    project_name="my-agent-app",
    deployment_id="dpl_xxx",
):
    print(log)

# 获取运行时日志
async for log in deployer.stream_runtime_logs(
    project_name="my-agent-app",
    functions=["api/hello"],  # 特定函数
):
    print(f"[{log.timestamp}] {log.message}")
```

## CI/CD 集成

### GitHub Actions 完整示例

```yaml
# .github/workflows/deploy.yml
name: Deploy

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
          cache: 'npm'
      
      - run: npm ci
      - run: npm run build
      - run: npm test
      
      - uses: actions/upload-artifact@v4
        with:
          name: dist
          path: dist/

  deploy-preview:
    needs: build
    if: github.event_name == 'pull_request'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: dist
          path: dist/
      
      - name: Deploy to Preview
        id: deploy
        run: |
          python -c "
          import asyncio
          from agentflow.skills.builtin.deployment_manager import DeploymentManager, VercelConfig
          
          async def main():
              deployer = DeploymentManager(
                  provider='vercel',
                  config=VercelConfig(token='${{ secrets.VERCEL_TOKEN }}')
              )
              deployment = await deployer.create_preview(
                  project_name='my-agent-app',
                  source_path='./dist',
                  pr_number=${{ github.event.pull_request.number }}
              )
              print(f'::set-output name=url::{deployment.url}')
          
          asyncio.run(main())
          "

  deploy-production:
    needs: build
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest
    environment: production
    steps:
      - uses: actions/download-artifact@v4
        with:
          name: dist
          path: dist/
      
      - name: Deploy to Production
        run: |
          python -c "
          import asyncio
          from agentflow.skills.builtin.deployment_manager import DeploymentManager, VercelConfig
          
          async def main():
              deployer = DeploymentManager(
                  provider='vercel',
                  config=VercelConfig(token='${{ secrets.VERCEL_TOKEN }}')
              )
              deployment = await deployer.deploy(
                  project_name='my-agent-app',
                  source_path='./dist',
                  environment='production'
              )
              print(f'Deployed: {deployment.url}')
          
          asyncio.run(main())
          "
```

## Agent 集成示例

```python
from agentflow.skills import SkillEngine

engine = SkillEngine()

@engine.tool("deploy_app")
async def deploy_app(
    project: str,
    environment: str = "preview",
) -> dict:
    """部署应用"""
    deployment = await deployer.deploy(
        project_name=project,
        source_path="./dist",
        environment=environment,
    )
    return {
        "url": deployment.url,
        "status": deployment.status,
        "deployment_id": deployment.id,
    }

@engine.tool("rollback_app")
async def rollback_app(project: str) -> dict:
    """回滚到上一个版本"""
    result = await deployer.rollback_to_previous(project)
    return {"status": "rolled_back", "deployment_id": result.id}

@engine.tool("get_deployment_status")
async def get_deployment_status(project: str) -> dict:
    """获取部署状态"""
    deployments = await deployer.list_deployments(project, limit=1)
    if deployments:
        d = deployments[0]
        return {
            "url": d.url,
            "status": d.status,
            "created_at": d.created_at.isoformat(),
        }
    return {"status": "no_deployments"}
```

## 最佳实践

### 1. 环境分离

```python
# 开发环境
await deployer.deploy(
    project_name="my-app",
    source_path="./dist",
    environment="development",
    env_vars={"DEBUG": "true"},
)

# 预览环境（PR）
await deployer.create_preview(
    project_name="my-app",
    source_path="./dist",
    branch="feature/xxx",
)

# 生产环境
await deployer.deploy(
    project_name="my-app",
    source_path="./dist",
    environment="production",
    env_vars={"DEBUG": "false"},
)
```

### 2. 渐进式发布

```python
# Canary 部署（10%流量）
deployment = await deployer.deploy(
    project_name="my-app",
    source_path="./dist",
    traffic_percentage=10,
)

# 监控 10 分钟
await asyncio.sleep(600)

# 检查错误率
metrics = await deployer.get_deployment_metrics(deployment.id)
if metrics.error_rate < 0.01:  # < 1%
    # 提升到 100%
    await deployer.promote_deployment(
        project_name="my-app",
        deployment_id=deployment.id,
    )
else:
    # 回滚
    await deployer.rollback_to_previous("my-app")
```

### 3. 安全配置

```python
# 敏感变量加密存储
await deployer.set_env_var(
    project_name="my-app",
    key="DATABASE_URL",
    value="postgres://...",
    encrypted=True,
)

# 使用 Secret 引用
await deployer.set_env_var(
    project_name="my-app",
    key="API_KEY",
    value="@secret/api-key",  # 引用 Secret 存储
)
```

## 平台选择指南

| 场景 | 推荐平台 | 理由 |
|------|----------|------|
| Next.js 应用 | Vercel | 原生支持、最佳优化 |
| 静态站点 | Cloudflare Pages | 无限带宽、全球 CDN |
| API/函数 | Cloudflare Workers | 超低延迟、免费额度大 |
| 复杂后端 | AWS Lambda | 完整生态、高度可配置 |
| Python/AI | Modal.com | GPU 支持、按需扩展 |
