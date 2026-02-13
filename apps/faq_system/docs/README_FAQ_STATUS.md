# FAQ System 当前状态（2026-02-13）

## 结论

### Q1: 现在是用 agentflow 的认证体系吗？
是。`apps/faq_system/backend/auth/service.py` 使用了 `agentflow/security/auth_middleware.py` 的 `AuthMiddleware` 来做 JWT 发行与校验。  
另外，`AuthMiddleware` 现已支持 `external_authenticator`，可用于接客户 SSO/OIDC/SAML 网关。

### Q2: 登录、改密、忘记密码、个人属性都支持了吗？
已支持，接口如下：

- `POST /api/auth/login`
- `POST /api/auth/logout`
- `GET /api/auth/me`
- `POST /api/auth/token`
- `POST /api/auth/password/change`
- `POST /api/auth/password/forgot`
- `POST /api/auth/password/reset`
- `PATCH /api/auth/profile`

## 知识库配置在哪里

### 配置入口

- 环境变量：
  - `FAQ_KB_INTERNAL_COLLECTION`
  - `FAQ_KB_EXTERNAL_COLLECTION`
  - `FAQ_KB_CONFIDENTIAL_COLLECTION`
  - `FAQ_KB_DEFAULT_TYPE`
- 运行时 API：
  - `GET /api/kb/settings`
  - `PATCH /api/kb/settings`（`admin/manager`）

### 查询如何指定知识库

- `POST /api/rag/query` 中传 `kb_type`（`internal/external/confidential`）
- 可选传 `collection` 覆盖默认映射

示例：

```json
{
  "question": "请给我社内采购制度",
  "kb_type": "internal",
  "top_k": 5
}
```

## 安全状态

- API 路由已要求认证（`require_auth`）。
- WebSocket 已要求 token（`/ws/{client_id}?access_token=...`）或会话 cookie。
- 流式接口已统一传递用户上下文。

## 企业认证系统快速接入建议

1. 若有认证网关（OAuth2 Proxy / APIM / IAP）：配置 `FAQ_TRUST_PROXY_AUTH=true`，注入 `X-Auth-*` 头。  
2. 若需定制验签/票据交换：在 `AuthMiddleware(external_authenticator=...)` 中接企业身份系统。  
3. 最终生产建议替换 demo 用户存储为 DB/LDAP/IdP。  

