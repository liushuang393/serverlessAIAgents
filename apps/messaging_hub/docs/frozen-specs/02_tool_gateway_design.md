# Tool Gateway 内部設計（凍結版）

## 実行順序

1. 入力検証
2. ポリシー評価
3. サンドボックス実行
4. 監査記録
5. イベント通知

## 標準戻り値契約

すべての Tool/Skill 実行は以下のキーを持つ。

```json
{
  "result": {},
  "evidence": [],
  "artifacts": [],
  "rollback_handle": null,
  "cost": {
    "duration_ms": 0,
    "token_estimate": 0
  },
  "risk_flags": []
}
```

## 責務

- Tool Gateway: 実行入口を一本化し、直接実行を禁止
- PolicyEngine/Governance: ALLOW / DENY / APPROVAL_REQUIRED を返す
- ApprovalManager: 高リスク操作の承認要求・決裁・期限管理
- ExecutionTracker: 実行開始/完了・統計・タイムライン

## 実行イベント標準

- `RunStarted`
- `StepStarted`
- `ToolApprovalRequested`
- `ToolExecuted`
- `EvidenceAdded`
- `RunFinished`

## 互換方針

移行期間は旧イベント名も併送し、UI/外部連携を破壊しない。
