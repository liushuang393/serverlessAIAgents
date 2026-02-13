# FAQ 系统改造总结（认证 + 知识库配置）

## 本次完成

1. 认证能力补齐  
- 新增改密/忘记密码/重置密码/个人资料更新接口。  
- JWT 登录链路保持兼容。  

2. 企业认证接入能力增强  
- `agentflow/security/auth_middleware.py` 新增 `external_authenticator` 扩展点。  
- `apps/faq_system/backend/auth/dependencies.py` 支持可选的认证代理头模式（`FAQ_TRUST_PROXY_AUTH`）。  

3. 安全闭环  
- WebSocket 增加认证校验（token 或 cookie）。  
- SSE/同步链路统一传递用户上下文。  

4. 知识库配置可视化  
- 新增 `apps/faq_system/backend/config/knowledge_base.py`。  
- 新增 `GET/PATCH /api/kb/settings`。  
- `POST /api/rag/query` 和 `POST /api/rag/add` 支持 `kb_type + collection` 双模式。  

## 关键接口

### 认证

- `POST /api/auth/login`
- `POST /api/auth/logout`
- `GET /api/auth/me`
- `POST /api/auth/token`
- `POST /api/auth/password/change`
- `POST /api/auth/password/forgot`
- `POST /api/auth/password/reset`
- `PATCH /api/auth/profile`

### 知识库

- `GET /api/kb/settings`
- `PATCH /api/kb/settings`
- `POST /api/rag/query` (`kb_type` / `collection`)
- `POST /api/rag/add` (`kb_type` / `collection`)

## 配置项

- 认证：
  - `JWT_SECRET_KEY`
  - `JWT_EXPIRE_MINUTES`
  - `FAQ_AUTH_DEV_MODE`
  - `FAQ_TRUST_PROXY_AUTH`
- 知识库：
  - `FAQ_KB_INTERNAL_COLLECTION`
  - `FAQ_KB_EXTERNAL_COLLECTION`
  - `FAQ_KB_CONFIDENTIAL_COLLECTION`
  - `FAQ_KB_DEFAULT_TYPE`

## 测试结果

- `PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 pytest apps/faq_system/tests/test_auth.py -q -p pytest_asyncio.plugin -o addopts=''`
  - 结果：`47 passed`
- `PYTEST_DISABLE_PLUGIN_AUTOLOAD=1 pytest tests/unit/test_security_module.py -q -p pytest_asyncio.plugin -o addopts='' -k "AuthMiddleware or CreateAuthMiddleware"`
  - 结果：`5 passed`

