# FAQ System 清单（更新后）

## 认证

- [x] `POST /api/auth/login`
- [x] `POST /api/auth/logout`
- [x] `GET /api/auth/me`
- [x] `POST /api/auth/token`
- [x] `POST /api/auth/password/change`
- [x] `POST /api/auth/password/forgot`
- [x] `POST /api/auth/password/reset`
- [x] `PATCH /api/auth/profile`

## 安全

- [x] API 端点认证保护
- [x] WebSocket 认证保护
- [x] SSE 链路用户上下文透传
- [x] 角色限制（`/api/kb/settings` 写操作）

## 知识库

- [x] 环境变量配置（internal/external/confidential）
- [x] 运行时查看配置：`GET /api/kb/settings`
- [x] 运行时更新配置：`PATCH /api/kb/settings`
- [x] 查询指定 `kb_type`
- [x] 查询可覆盖 `collection`

## AgentFlow 反哺

- [x] `AuthMiddleware` 支持 `external_authenticator`
- [x] 认证上下文存储改为 `ContextVar`（替代全局变量）

## 测试

- [x] FAQ 认证与权限测试（47）
- [x] AgentFlow 认证扩展测试（5）

