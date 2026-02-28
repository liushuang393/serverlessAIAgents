# sr_chat API 仕様（凍結版）

## 方針

- 既存 API は維持
- `/api/sr_chat/*` を非破壊追加
- Slack 風 API と実行イベント購読を提供

## エンドポイント

- `POST /api/sr_chat/auth.test`
- `GET /api/sr_chat/conversations.list`
- `GET /api/sr_chat/conversations.history`
- `POST /api/sr_chat/chat.postMessage`
- `POST /api/sr_chat/chat.update`
- `POST /api/sr_chat/files.upload`
- `POST /api/sr_chat/events.subscribe`

## 実行イベント

- `RunStarted`
- `StepStarted`
- `ToolApprovalRequested`
- `ToolExecuted`
- `EvidenceAdded`
- `RunFinished`

## events.subscribe 応答

```json
{
  "ok": true,
  "subscription_id": "uuid",
  "events": [
    "RunStarted",
    "StepStarted",
    "ToolApprovalRequested",
    "ToolExecuted",
    "EvidenceAdded",
    "RunFinished"
  ],
  "ws_url": "ws://localhost:8004/ws?client_id=<client_id>"
}
```

## chat.postMessage 応答

```json
{
  "ok": true,
  "conversation_id": "chat:default",
  "message": {
    "id": "msg_xxx",
    "role": "assistant",
    "text": "..."
  },
  "run_id": "run_xxx"
}
```
