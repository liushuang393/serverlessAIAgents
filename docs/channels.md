# Channels - 消息平台集成

AgentFlow Channels 模块提供统一的消息平台适配器接口，支持 Telegram、Slack、Discord 等多平台集成，实现类似 [moltbot](https://github.com/moltbot/moltbot) 的多平台 AI 聊天机器人功能。

## 架构概览

```
┌─────────────────────────────────────────────────────────┐
│         Message Platforms (多平台)                      │
│  Telegram │ Slack │ Discord │ WhatsApp │ Teams │ ...   │
└─────────────┬───────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────────────────────┐
│         MessageGateway (统一路由网关)                   │
│  • 会话管理   • 消息路由   • 错误处理   • 队列管理     │
└─────────────┬───────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────────────────────┐
│         ChatBotSkill (会话管理)                         │
│  • 多轮对话   • 上下文维护   • Agent 集成   • RAG      │
└─────────────┬───────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────────────────────┐
│         Agent/Coordinator (AI 处理)                     │
│  • 多代理协调   • 工具调用   • 知识库查询              │
└─────────────┬───────────────────────────────────────────┘
              │
              ▼
┌─────────────────────────────────────────────────────────┐
│         WebSocket Hub (实时同步)                        │
│  • 前端推送   • Live Canvas   • 实时状态               │
└─────────────────────────────────────────────────────────┘
```

## 核心组件

### 1. MessageChannelAdapter（基类）

所有平台适配器的抽象基类，定义统一接口。

```python
from agentflow.channels.base import MessageChannelAdapter, UserInfo

class MyPlatformAdapter(MessageChannelAdapter):
    @property
    def platform_name(self) -> str:
        return "myplatform"

    async def send_message(self, channel_id: str, text: str, **kwargs) -> str:
        # 实现发送逻辑
        return "message_id"

    async def send_typing_indicator(self, channel_id: str) -> None:
        # 实现输入指示器
        pass

    async def get_user_info(self, user_id: str) -> UserInfo:
        # 获取用户信息
        return UserInfo(user_id=user_id, display_name="User")
```

### 2. MessageGateway（核心路由）

消息路由网关，连接平台与 Agent。

```python
from agentflow.channels import MessageGateway, TelegramAdapter
from agentflow import ChatBotSkill, WebSocketHub

# 创建核心组件
hub = WebSocketHub()
chatbot = ChatBotSkill()
gateway = MessageGateway(hub, chatbot)

# 注册平台
telegram = TelegramAdapter(token=TELEGRAM_TOKEN)
gateway.register_channel("telegram", telegram)

# 路由消息
response = await gateway.route_message(
    platform="telegram",
    user_id="123456",
    text="Hello, bot!"
)
```

### 3. 平台适配器

#### Telegram 适配器

```python
from agentflow.channels import TelegramAdapter

adapter = TelegramAdapter(token=TELEGRAM_BOT_TOKEN)

# 发送消息
message_id = await adapter.send_message("chat_id", "Hello")

# 发送图片
await adapter.send_image("chat_id", "https://example.com/image.jpg", caption="Photo")

# Webhook 模式
@app.post("/webhook/telegram")
async def telegram_webhook(update: dict):
    await adapter.handle_webhook(update, gateway)
    return {"ok": True}

# 轮询模式
await adapter.start_polling(gateway)
```

#### Slack 适配器

```python
from agentflow.channels import SlackAdapter

adapter = SlackAdapter(
    token=SLACK_BOT_TOKEN,
    signing_secret=SLACK_SIGNING_SECRET
)

# 发送消息
ts = await adapter.send_message("channel_id", "Hello")

# 线程回复
await adapter.send_message(
    "channel_id",
    "Reply",
    thread_ts=ts
)

# Block Kit 支持
blocks = [
    {"type": "section", "text": {"type": "mrkdwn", "text": "*Bold* text"}}
]
await adapter.send_message("channel_id", "Fallback", blocks=blocks)

# Webhook 处理
@app.post("/webhook/slack")
async def slack_webhook(request: Request):
    body = await request.body()
    headers = dict(request.headers)
    response = await adapter.handle_webhook(body, headers, gateway)
    return response
```

#### Discord 适配器

```python
from agentflow.channels import DiscordAdapter

adapter = DiscordAdapter(token=DISCORD_BOT_TOKEN)

# 发送消息
message_id = await adapter.send_message("channel_id", "Hello")

# 发送 Embed
embed = discord.Embed(title="Title", description="Description")
await adapter.send_message("channel_id", "Check this:", embed=embed)

# 启动 Bot（长连接）
await adapter.start_bot(gateway)
```

## 完整示例

### 基础聊天机器人

```python
from fastapi import FastAPI
from agentflow import ChatBotSkill, WebSocketHub
from agentflow.channels import MessageGateway, TelegramAdapter

app = FastAPI()

# 初始化组件
hub = WebSocketHub()
chatbot = ChatBotSkill()
gateway = MessageGateway(hub, chatbot)

# 注册平台
telegram = TelegramAdapter(token=TELEGRAM_BOT_TOKEN)
gateway.register_channel("telegram", telegram)

# Webhook 端点
@app.post("/webhook/telegram")
async def telegram_webhook(update: dict):
    await telegram.handle_webhook(update, gateway)
    return {"ok": True}

# WebSocket 端点
@app.websocket("/ws")
async def websocket_endpoint(websocket: WebSocket):
    await hub.connect(websocket, client_id="user_123")
    # ... 保持连接
```

### 多平台支持

```python
from agentflow.channels import TelegramAdapter, SlackAdapter, DiscordAdapter

# 注册多个平台
platforms = {
    "telegram": TelegramAdapter(token=TELEGRAM_TOKEN),
    "slack": SlackAdapter(token=SLACK_TOKEN, signing_secret=SLACK_SECRET),
    "discord": DiscordAdapter(token=DISCORD_TOKEN),
}

for name, adapter in platforms.items():
    gateway.register_channel(name, adapter)

print(f"Registered: {gateway.list_channels()}")
# ['telegram', 'slack', 'discord']
```

### Agent 集成

```python
from agentflow import ChatBotSkill
from agentflow.patterns.coordinator import AdaptiveCoordinator

# 创建多代理系统
coordinator = AdaptiveCoordinator(agents=[
    research_agent,
    writing_agent,
    review_agent,
])

# 集成到聊天机器人
chatbot = ChatBotSkill(
    coordinator=coordinator,
    rag_skill=rag,  # 可选 RAG
)

gateway = MessageGateway(hub, chatbot)
```

### RAG 增强

```python
from agentflow.skills.rag import RAGSkill
from agentflow import ChatBotSkill

# 创建 RAG skill
rag = RAGSkill(knowledge_base_path="./data")

# 集成到聊天机器人
chatbot = ChatBotSkill(
    rag_skill=rag,
    config=ChatBotConfig(enable_rag=True),
)
```

## 会话管理

### 跨平台会话追踪

```python
# 会话自动创建和管理
# 用户第一次发消息时自动创建会话
await gateway.route_message("telegram", "user_123", "Hello")

# 获取用户会话
session = gateway.get_session_by_user("telegram", "user_123")
print(f"Messages: {len(session.messages)}")

# 清除会话
gateway.clear_user_session("telegram", "user_123")
```

### 会话持久化

```python
from agentflow import get_db

class PersistentChatBot(ChatBotSkill):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.db = get_db()

    async def create_session(self, metadata=None):
        user_id = metadata.get("user_id")
        # 从数据库加载
        session_data = await self.db.get(f"session:{user_id}")
        if session_data:
            return ChatSession(**session_data)
        # 创建新会话
        session = await super().create_session(metadata)
        await self.save_session(session)
        return session

    async def save_session(self, session: ChatSession):
        await self.db.set(f"session:{session.id}", session.model_dump())
```

## WebSocket 实时同步

```python
# 前端 JavaScript
const ws = new WebSocket('ws://localhost:8000/ws?client_id=user_123');

ws.onmessage = (event) => {
    const data = JSON.parse(event.data);
    if (data.type === 'assistant_message') {
        // 显示 AI 响应
        displayMessage(data.data.text);
    }
};

// 消息自动同步到 WebSocket
// gateway.route_message() 会自动广播事件
```

## API 端点设计

### Webhook 端点

```python
# Telegram
@app.post("/webhook/telegram")
async def telegram_webhook(update: dict):
    adapter = gateway.get_channel("telegram")
    await adapter.handle_webhook(update, gateway)
    return {"ok": True}

# Slack
@app.post("/webhook/slack")
async def slack_webhook(request: Request):
    body = await request.body()
    headers = dict(request.headers)
    adapter = gateway.get_channel("slack")
    response = await adapter.handle_webhook(body, headers, gateway)
    return response
```

### 管理 API

```python
# 列出平台
@app.get("/platforms")
async def list_platforms():
    return {"platforms": gateway.list_channels()}

# 查看统计
@app.get("/health")
async def health():
    stats = gateway.get_statistics()
    return {"status": "healthy", "statistics": stats}

# 列出会话
@app.get("/sessions")
async def list_sessions():
    sessions = chatbot.list_sessions()
    return {"sessions": sessions}

# 直接发送消息
@app.post("/send")
async def send_message(platform: str, channel_id: str, text: str):
    message_id = await gateway.send_message_to_platform(
        platform, channel_id, text
    )
    return {"message_id": message_id}
```

## 高级特性

### 异步消息处理

```python
# 使用消息队列避免并发问题
await gateway.route_message_async(
    platform="telegram",
    user_id="123",
    text="Hello"
)
# 立即返回，消息在后台处理
```

### 输入指示器

```python
# 自动发送 "正在输入" 指示器
gateway = MessageGateway(
    hub,
    chatbot,
    enable_typing_indicator=True  # 默认启用
)

# 或单次禁用
await gateway.route_message(
    "telegram",
    "user_123",
    "Hello",
    send_typing=False
)
```

### 错误处理

```python
try:
    await gateway.route_message("telegram", "user_123", "Hello")
except ValueError as e:
    # 平台未注册
    print(f"Platform error: {e}")
except Exception as e:
    # 其他错误（自动发送错误消息到用户）
    print(f"Processing error: {e}")
```

## 平台配置

### 环境变量

```bash
# LLM Provider
OPENAI_API_KEY=

# Telegram
TELEGRAM_BOT_TOKEN=

# Slack
SLACK_BOT_TOKEN=
SLACK_SIGNING_SECRET=

# Discord
DISCORD_BOT_TOKEN=
```

### 平台设置

#### Telegram

1. 创建 Bot: [@BotFather](https://t.me/BotFather)
2. 设置 Webhook:
   ```bash
   curl -X POST https://api.telegram.org/bot<TOKEN>/setWebhook \
     -d url=https://your-domain.com/webhook/telegram
   ```

#### Slack

1. 创建 App: https://api.slack.com/apps
2. 配置 Scopes: `chat:write`, `channels:read`, `im:read`, `users:read`
3. 设置 Event Subscriptions URL
4. 订阅事件: `message.channels`, `message.im`

#### Discord

1. 创建 Application: https://discord.com/developers/applications
2. 添加 Bot
3. 启用 Intents: "Message Content Intent"
4. 生成 OAuth2 URL 并邀请 Bot

## 部署建议

### 开发环境

```bash
# 使用轮询模式（Telegram/Discord）
python apps/messaging_hub/main.py
```

### 生产环境

```bash
# 使用 Webhook + HTTPS
uvicorn apps.messaging_hub.main:app \
  --host 0.0.0.0 \
  --port 8000 \
  --workers 4
```

### Docker

```dockerfile
FROM python:3.13-slim
WORKDIR /app
COPY . .
RUN pip install -e ".[dev]" && \
    pip install python-telegram-bot slack-sdk discord.py
CMD ["uvicorn", "apps.messaging_hub.main:app", "--host", "0.0.0.0"]
```

## 性能优化

1. **消息队列**: 使用 `route_message_async()` 避免阻塞
2. **连接池**: WebSocket Hub 自动管理连接
3. **会话缓存**: 会话存储在内存，考虑 Redis 持久化
4. **限流**: 添加 FastAPI 中间件限制请求速率

## 扩展开发

### 自定义适配器

```python
from agentflow.channels.base import MessageChannelAdapter

class WhatsAppAdapter(MessageChannelAdapter):
    @property
    def platform_name(self) -> str:
        return "whatsapp"

    async def send_message(self, channel_id: str, text: str, **kwargs) -> str:
        # 实现 WhatsApp Business API 调用
        response = await self.client.send_text(channel_id, text)
        return response["id"]

    # 实现其他必需方法...
```

### 自定义 Hook

```python
def pre_process(text: str, session: ChatSession) -> str:
    """消息预处理."""
    # 敏感词过滤
    return filter_sensitive_words(text)

def post_process(response: str, session: ChatSession) -> str:
    """响应后处理."""
    # 添加签名
    return response + "\n\n-- Powered by AgentFlow"

chatbot = ChatBotSkill(
    pre_hook=pre_process,
    post_hook=post_process,
)
```

## 示例应用

完整示例见 [`apps/messaging_hub/`](../apps/messaging_hub/)：

- 多平台支持（Telegram/Slack/Discord）
- WebSocket 实时同步
- FastAPI 完整实现
- 生产级配置

## 对比 Moltbot

| Feature | Moltbot | AgentFlow Channels |
|---------|---------|-------------------|
| Platforms | 12+ | 3+ (可扩展) |
| Architecture | Gateway | 8-layer + Gateway |
| Multi-Agent | Basic | 4 patterns + 5 engines |
| Memory | Unknown | 3-tier system |
| Protocols | A2UI | MCP/A2A/AG-UI/A2UI/UCP |
| Testing | ? | 16+ unit tests |
| Documentation | Good | Comprehensive |

## 参考资源

- [Messaging Hub 示例](../apps/messaging_hub/README.md)
- [ChatBot Skill](../agentflow/skills/chatbot.py)
- [WebSocket Hub](../agentflow/api/websocket_hub.py)
- [Moltbot](https://github.com/moltbot/moltbot)
