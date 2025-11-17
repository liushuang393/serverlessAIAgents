# Phase 1: Tool Use Pattern Enhancement - 詳細設計書

## 📋 概要

**目標**: AgentFlow の Tool Use パターンを業界最佳実践に基づいて 60% → 95% に改善

**期間**: 1-2 週間

**優先度**: ⭐⭐⭐ 高優先級

---

## 🎯 設計原則（基於 Anthropic 最佳実践）

### 1. Agent-Computer Interface (ACI) 設計原則

> "Think about how much effort goes into human-computer interfaces (HCI), and plan to invest just as much effort in creating good agent-computer interfaces (ACI)."
> — Anthropic, Building Effective Agents

#### 核心原則:

1. **給模型足夠的思考空間**
   - 避免讓模型在寫代碼前就需要精確計數
   - 使用自然的格式，接近互聯網上的文本

2. **防錯設計 (Poka-yoke)**
   - 使用絕對路徑而非相對路徑
   - 參數設計讓錯誤難以發生
   - 清晰的邊界和約束

3. **清晰的工具文檔**
   - 像寫給初級開發者的文檔一樣詳細
   - 包含使用示例和邊界情況
   - 明確的輸入格式要求

---

## 🏗️ 架構設計

### 1. 增強的 MCP Client 架構

```
┌─────────────────────────────────────────────────────────┐
│                    Enhanced MCP Client                   │
├─────────────────────────────────────────────────────────┤
│                                                           │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ ToolWhitelist│  │ AuditLogger  │  │  Parameter   │  │
│  │              │  │              │  │  Validator   │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
│                                                           │
│  ┌─────────────────────────────────────────────────┐    │
│  │         Retry & Timeout Controller              │    │
│  │  - Exponential Backoff                          │    │
│  │  - Configurable Timeout                         │    │
│  │  - Error Classification                         │    │
│  └─────────────────────────────────────────────────┘    │
│                                                           │
│  ┌─────────────────────────────────────────────────┐    │
│  │         Tool Call Executor                      │    │
│  │  - Pre-call Validation                          │    │
│  │  - Post-call Audit                              │    │
│  │  - Result Caching                               │    │
│  └─────────────────────────────────────────────────┘    │
│                                                           │
└─────────────────────────────────────────────────────────┘
```

---

## 📝 詳細実装設計

### 1. Enhanced MCP Client

#### 1.1 クラス設計

```python
# agentflow/protocols/mcp_client.py

from typing import Any, Optional
import asyncio
from datetime import UTC, datetime

from agentflow.core.security import (
    ToolWhitelist,
    AuditLogger,
    ParameterValidator,
)

class EnhancedMCPClient:
    """
    強化された MCP Client。
    
    業界最佳実践に基づいた以下の機能を提供:
    - ツールホワイトリスト検証
    - 審計ログ記録
    - パラメータ検証
    - リトライとタイムアウト制御
    """
    
    def __init__(
        self,
        server_url: str,
        whitelist: Optional[ToolWhitelist] = None,
        audit_logger: Optional[AuditLogger] = None,
        validator: Optional[ParameterValidator] = None,
        max_retries: int = 3,
        timeout: float = 30.0,
        enable_cache: bool = True,
    ):
        """
        初期化。
        
        Args:
            server_url: MCP サーバーの URL
            whitelist: ツールホワイトリスト（None の場合はデフォルト）
            audit_logger: 審計ログ記録器（None の場合はデフォルト）
            validator: パラメータ検証器（None の場合はデフォルト）
            max_retries: 最大リトライ回数
            timeout: タイムアウト時間（秒）
            enable_cache: 結果キャッシュを有効にするか
        """
        self._server_url = server_url
        self._whitelist = whitelist or ToolWhitelist()
        self._audit_logger = audit_logger or AuditLogger()
        self._validator = validator or ParameterValidator()
        self._max_retries = max_retries
        self._timeout = timeout
        self._enable_cache = enable_cache
        self._cache: dict[str, Any] = {}
```

#### 1.2 核心メソッド設計

```python
    async def call_tool(
        self,
        tool_uri: str,
        parameters: dict,
        user_id: str = "system",
    ) -> Any:
        """
        ツールを呼び出す（完全な安全チェック付き）。
        
        実行フロー:
        1. ホワイトリストチェック
        2. パラメータ検証
        3. キャッシュチェック（有効な場合）
        4. リトライ付き実行
        5. 審計ログ記録
        
        Args:
            tool_uri: ツール URI（例: "mcp://filesystem/read_file"）
            parameters: ツールパラメータ
            user_id: ユーザー ID（審計ログ用）
            
        Returns:
            ツール実行結果
            
        Raises:
            ToolNotAllowedError: ツールがホワイトリストにない
            ValidationError: パラメータ検証失敗
            ToolExecutionError: ツール実行失敗
            TimeoutError: タイムアウト
        """
```

---

## 🔧 実装タスク

### Task 1.1: MCP Client への安全機能統合

**ファイル**: `agentflow/protocols/mcp_client.py`

**変更内容**:
1. `EnhancedMCPClient` クラスを追加
2. 既存の `MCPClient` を `LegacyMCPClient` にリネーム
3. `MCPClient` を `EnhancedMCPClient` のエイリアスに

**実装詳細**:
- ホワイトリストチェック
- パラメータ検証
- 審計ログ記録
- リトライメカニズム
- タイムアウト制御

---

## 📊 成功指標

### 1. 機能完成度

- ✅ ホワイトリスト統合: 100%
- ✅ 審計ログ統合: 100%
- ✅ パラメータ検証統合: 100%
- ✅ リトライメカニズム: 100%
- ✅ タイムアウト制御: 100%

### 2. テストカバレッジ

- 目標: **90%+**
- 単体テスト: 全機能カバー
- 統合テスト: エンドツーエンドシナリオ

### 3. パフォーマンス

- ツール呼び出しオーバーヘッド: < 50ms
- リトライ成功率: > 95%
- キャッシュヒット率: > 80%（適用可能な場合）

---

## 🧪 テスト戦略

### 1. 単体テスト

**ファイル**: `tests/unit/test_enhanced_mcp_client.py`

**テストケース**:
1. ホワイトリストチェック
2. パラメータ検証
3. リトライメカニズム
4. タイムアウト処理
5. 審計ログ記録
6. キャッシュ機能

### 2. 統合テスト

**ファイル**: `tests/integration/test_mcp_security_integration.py`

**テストシナリオ**:
1. 正常なツール呼び出しフロー
2. ホワイトリスト違反の処理
3. パラメータ検証エラーの処理
4. ネットワークエラーのリトライ
5. タイムアウトの処理

---

## 📚 参考資料

1. **Anthropic - Building Effective Agents**
   - https://www.anthropic.com/research/building-effective-agents
   - 重点: Appendix 2 "Prompt Engineering your Tools"

2. **LangChain - Tool Calling Best Practices**
   - Retry mechanisms
   - Error handling

3. **AgentFlow 既存実装**
   - `agentflow/protocols/a2a_client.py` - リトライメカニズム参考
   - `agentflow/core/security.py` - 安全機能

---

## 🚀 次のステップ

Phase 1 完成後:
1. Phase 2: Plan-then-Execute パターン実装
2. 既存の AgentFlowEngine との統合
3. パフォーマンス最適化
4. ドキュメント更新

