# FAQ System 快速登录实现指南

## 🚀 最小可行实现（MVP）- 2小时

### 步骤1：创建认证模块

```bash
mkdir -p apps/faq_system/backend/auth
touch apps/faq_system/backend/auth/__init__.py
touch apps/faq_system/backend/auth/models.py
touch apps/faq_system/backend/auth/service.py
```

### 步骤2：实现用户模型（models.py）

```python
from pydantic import BaseModel
from datetime import datetime

class User(BaseModel):
    user_id: str
    email: str
    username: str
    role: str  # admin, manager, employee, external
    department: str
    created_at: datetime

class LoginRequest(BaseModel):
    username: str
    password: str

class AuthResponse(BaseModel):
    success: bool
    token: str | None = None
    user: User | None = None
    message: str = ""
```

### 步骤3：实现认证服务（service.py）

```python
import secrets
from datetime import datetime, timedelta
import jwt

class AuthService:
    def __init__(self, secret_key: str = "your-secret-key"):
        self.secret_key = secret_key
        self.users = {}  # 临时存储，生产用DB

    def create_token(self, user_id: str, role: str) -> str:
        payload = {
            "sub": user_id,
            "role": role,
            "exp": datetime.utcnow() + timedelta(hours=24)
        }
        return jwt.encode(payload, self.secret_key, algorithm="HS256")

    def verify_token(self, token: str) -> dict | None:
        try:
            return jwt.decode(token, self.secret_key, algorithms=["HS256"])
        except:
            return None

    async def login(self, username: str, password: str) -> tuple[bool, str]:
        # 验证用户（示例）
        if username == "admin" and password == "admin123":
            token = self.create_token("admin", "admin")
            return True, token
        return False, "Invalid credentials"
```

### 步骤4：在main.py中添加认证端点

```python
from fastapi import Depends, HTTPException
from apps.faq_system.backend.auth.service import AuthService

auth_service = AuthService()

@app.post("/api/auth/login")
async def login(request: LoginRequest):
    success, token = await auth_service.login(request.username, request.password)
    if not success:
        raise HTTPException(status_code=401, detail="Invalid credentials")
    return {"token": token, "success": True}

@app.post("/api/auth/logout")
async def logout():
    return {"success": True, "message": "Logged out"}

# 依赖：获取当前用户
async def get_current_user(authorization: str = Header(None)):
    if not authorization or not authorization.startswith("Bearer "):
        raise HTTPException(status_code=401, detail="Missing token")

    token = authorization[7:]
    payload = auth_service.verify_token(token)
    if not payload:
        raise HTTPException(status_code=401, detail="Invalid token")

    return payload
```

### 步骤5：保护API端点

```python
@app.post("/api/chat")
async def chat(request: ChatRequest, current_user = Depends(get_current_user)):
    # 现有逻辑
    agent = _get_faq_agent()
    result = await agent.run({"question": request.message})
    return _register_artifacts(result)
```

## 📋 检查清单

- [ ] 创建认证模块目录
- [ ] 实现用户模型
- [ ] 实现认证服务
- [ ] 添加登录端点
- [ ] 添加登出端点
- [ ] 实现令牌验证
- [ ] 保护API端点
- [ ] 测试登录流程
- [ ] 添加错误处理
- [ ] 文档更新

## 🧪 测试命令

```bash
# 登录
curl -X POST http://localhost:8001/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"admin","password":"admin123"}'

# 使用令牌调用API
curl -X POST http://localhost:8001/api/chat \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"message":"年次有給休暇は何日もらえますか？"}'
```

## 🔐 生产环保建议

1. **使用数据库**：替换内存存储
2. **密码哈希**：使用 bcrypt
3. **环境变量**：存储密钥
4. **HTTPS**：生产环境必须
5. **刷新令牌**：实现令牌刷新机制
6. **速率限制**：防止暴力破解
7. **审计日志**：记录所有认证事件

## 📚 参考资源

- `agentflow/security/auth_middleware.py` - 认证中间件
- `apps/decision_governance_engine/routers/auth.py` - 参考实现
- FastAPI 文档：https://fastapi.tiangolo.com/tutorial/security/
