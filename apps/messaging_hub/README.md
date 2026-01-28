# Messaging Hub - Multi-Platform AI Chatbot

统一消息平台网关，类似 [moltbot](https://github.com/moltbot/moltbot) 的实现，支持 Telegram、Slack、Discord 等多平台集成。

## 🚀 Features

- ✅ **多平台支持**: Telegram, Slack, Discord
- ✅ **统一会话管理**: 跨平台用户会话追踪
- ✅ **实时同步**: WebSocket 双向通信
- ✅ **AI Agent 集成**: 复用 AgentFlow 多代理能力
- ✅ **富文本支持**: Markdown, Embeds, Block Kit
- ✅ **松耦合设计**: 自动检测 LLM provider

## 📋 Architecture

```
Message Platforms (Telegram/Slack/Discord)
           ↓
    Message Gateway (核心路由)
           ↓
    ChatBot Skill (会话管理)
           ↓
    Agent/Coordinator (AI 处理)
           ↓
    WebSocket Hub → Frontend (Live Canvas)
```

## 🛠️ Quick Start

### 1. 安装依赖

```bash
# 基础依赖
pip install -e ".[dev]"

# 平台依赖（按需安装）
pip install python-telegram-bot>=20.0  # Telegram
pip install slack-sdk>=3.0             # Slack
pip install discord.py>=2.0            # Discord
```

### 2. 配置环境变量

```bash
# 复制配置文件
cp apps/messaging_hub/.env.example apps/messaging_hub/.env

# 编辑配置（至少配置一个 LLM + 一个平台）
vim apps/messaging_hub/.env
```

必需配置：
- **LLM Provider**: `OPENAI_API_KEY` 或 `ANTHROPIC_API_KEY`
- **至少一个平台**: `TELEGRAM_BOT_TOKEN` 或 `SLACK_BOT_TOKEN` 或 `DISCORD_BOT_TOKEN`

### 3. 运行服务

```bash
# 开发模式
python apps/messaging_hub/main.py

# 或使用 uvicorn
uvicorn apps.messaging_hub.main:app --reload --port 8000
```

启动后访问：
- **API 文档**: http://localhost:8000/docs
- **健康检查**: http://localhost:8000/health
- **WebSocket**: ws://localhost:8000/ws

## 🤖 Platform Setup

### Telegram

1. **创建 Bot**:
   - 访问 [@BotFather](https://t.me/BotFather)
   - 发送 `/newbot` 创建新 bot
   - 获取 Token: `1234567890:ABCdef...`

2. **配置 Webhook**（可选，推荐用于生产环境）:
   ```bash
   curl -X POST https://api.telegram.org/bot<TOKEN>/setWebhook \
     -d url=https://your-domain.com/webhook/telegram
   ```

3. **或使用轮询模式**（开发环境）:
   - 代码已支持自动轮询，无需额外配置

### Slack

1. **创建 Slack App**:
   - 访问 https://api.slack.com/apps
   - 点击 "Create New App" → "From scratch"

2. **配置 OAuth & Permissions**:
   - 添加 Bot Token Scopes:
     - `chat:write`
     - `channels:read`
     - `im:read`
     - `users:read`
   - 安装 App 到 workspace
   - 复制 Bot User OAuth Token: `xoxb-...`

3. **配置 Event Subscriptions**:
   - 启用 Events
   - Request URL: `https://your-domain.com/webhook/slack`
   - Subscribe to bot events:
     - `message.channels`
     - `message.im`

### Discord

1. **创建 Discord Bot**:
   - 访问 https://discord.com/developers/applications
   - 点击 "New Application"
   - 进入 "Bot" 标签，点击 "Add Bot"

2. **配置 Intents**:
   - 启用 "Message Content Intent"
   - 启用 "Server Members Intent"

3. **获取 Token**:
   - 复制 Bot Token

4. **邀请 Bot 到服务器**:
   - OAuth2 → URL Generator
   - Scopes: `bot`
   - Permissions: `Send Messages`, `Read Messages/View Channels`
   - 复制生成的 URL 并访问

## 📡 API Endpoints

### HTTP APIs

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/` | GET | 服务信息 |
| `/health` | GET | 健康检查 + 统计 |
| `/platforms` | GET | 已注册平台列表 |
| `/sessions` | GET | 活跃会话列表 |
| `/send` | POST | 直接发送消息（管理） |
| `/webhook/telegram` | POST | Telegram webhook |
| `/webhook/slack` | POST | Slack webhook |

### WebSocket

```javascript
// 连接 WebSocket
const ws = new WebSocket('ws://localhost:8000/ws?client_id=user123');

// 接收实时消息
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  console.log('Received:', data);
  // { type: 'assistant_message', session_id: '...', data: {...} }
};
```

## 🧪 Testing

### 手动测试

1. **Telegram**: 向你的 bot 发送消息
   ```
   /start
   Hello, bot!
   ```

2. **Slack**: 在频道或 DM 中 @提及 bot
   ```
   @YourBot hello
   ```

3. **Discord**: 在服务器频道发送消息
   ```
   !hello
   你好，bot
   ```

### 使用 API 测试

```bash
# 发送消息到 Telegram
curl -X POST http://localhost:8000/send \
  -H "Content-Type: application/json" \
  -d '{
    "platform": "telegram",
    "channel_id": "123456789",
    "text": "Hello from API!"
  }'

# 查看活跃会话
curl http://localhost:8000/sessions

# 查看平台状态
curl http://localhost:8000/platforms
```

## 🎯 Advanced Usage

### 添加自定义 Agent

```python
from agentflow import ChatBotSkill
from agentflow.patterns.coordinator import AdaptiveCoordinator

# 创建多代理协调器
coordinator = AdaptiveCoordinator(agents=[agent1, agent2])

# 集成到 ChatBot
chatbot = ChatBotSkill(coordinator=coordinator)

# 使用到网关
gateway = MessageGateway(hub, chatbot)
```

### 添加 RAG 能力

```python
from agentflow.skills.rag import RAGSkill

# 创建 RAG skill
rag = RAGSkill(knowledge_base_path="./data")

# 集成到 ChatBot
chatbot = ChatBotSkill(rag_skill=rag)
```

### 持久化会话

```python
# 扩展 ChatBotSkill 实现数据库存储
from agentflow import get_db

class PersistentChatBot(ChatBotSkill):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.db = get_db()

    async def create_session(self, metadata=None):
        # 从数据库加载
        # ...
```

## 📊 Monitoring

查看统计信息：

```bash
curl http://localhost:8000/health
```

响应：
```json
{
  "status": "healthy",
  "statistics": {
    "registered_channels": 3,
    "active_sessions": 15,
    "message_queues": 8,
    "platforms": ["telegram", "slack", "discord"]
  }
}
```

## 🔒 Security

1. **Webhook 验证**:
   - Slack: 自动验证签名（需要 `SLACK_SIGNING_SECRET`）
   - Telegram: 建议使用 HTTPS + secret token

2. **环境变量**:
   - 切勿提交 `.env` 文件到版本控制
   - 生产环境使用密钥管理服务

3. **Rate Limiting**:
   - 考虑添加速率限制（可用 FastAPI middleware）

## 🚢 Deployment

### Docker

```dockerfile
FROM python:3.13-slim

WORKDIR /app
COPY . .

RUN pip install -e ".[dev]" && \
    pip install python-telegram-bot slack-sdk discord.py

CMD ["uvicorn", "apps.messaging_hub.main:app", "--host", "0.0.0.0", "--port", "8000"]
```

### 环境变量（生产）

```bash
# 使用 secrets management
export OPENAI_API_KEY=$(aws secretsmanager get-secret-value ...)
export TELEGRAM_BOT_TOKEN=$(...)
```

## 🆚 Comparison with Moltbot

| Feature | Moltbot | Messaging Hub |
|---------|---------|---------------|
| Platforms | 12+ (WhatsApp, iMessage, etc.) | 3 (可扩展) |
| Architecture | Gateway-centric | 8-layer clean arch |
| Multi-Agent | Basic routing | 4 patterns + 5 engines |
| Memory | Unknown | 3-tier system |
| UI | Live Canvas | A2UI + React Studio |
| Protocols | A2UI | MCP/A2A/AG-UI/A2UI/UCP |
| Voice | ✅ (ElevenLabs) | 🔜 (planned) |
| Device Tools | ✅ (Camera, Location) | 🔜 (planned) |
| Browser Control | ✅ (Playwright) | 🔜 (planned) |

## 📝 License

MIT License - see AgentFlow main README

## 🤝 Contributing

欢迎贡献！请遵循 AgentFlow 的贡献指南。

可以添加的功能：
- [ ] WhatsApp 适配器
- [ ] Microsoft Teams 适配器
- [ ] Signal 适配器
- [ ] 语音消息支持
- [ ] 图片识别（Vision）
- [ ] 会话导出
- [ ] 管理后台 UI
