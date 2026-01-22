-- ==============================================================================
-- Decision Governance Engine - 履歴DB スキーマ
-- ==============================================================================
-- 目的: 長期保存・分析用の決策履歴アーカイブ
-- 文字コード: UTF-8
-- ==============================================================================

-- 履歴決策記録（メインDBからの同期/アーカイブ用）
CREATE TABLE IF NOT EXISTS decision_history (
    id UUID PRIMARY KEY,
    request_id UUID NOT NULL,
    
    -- 入力情報
    question TEXT NOT NULL,
    mode VARCHAR(20) NOT NULL,
    
    -- 決策結果
    decision_role VARCHAR(20) NOT NULL,
    confidence DECIMAL(5,4),
    
    -- 完全な結果（分析用に全データ保持）
    full_response JSONB NOT NULL,
    
    -- メタ情報
    requester_role VARCHAR(50),
    organization_size VARCHAR(20),
    
    -- 統計情報
    processing_time_ms INTEGER,
    llm_tokens_used INTEGER,
    evidence_count INTEGER,
    claim_count INTEGER,
    
    -- 時間情報
    original_created_at TIMESTAMPTZ NOT NULL,
    archived_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- 統計サマリテーブル（日次集計）
CREATE TABLE IF NOT EXISTS decision_stats_daily (
    stat_date DATE PRIMARY KEY,
    
    -- 決策件数
    total_decisions INTEGER NOT NULL DEFAULT 0,
    go_count INTEGER NOT NULL DEFAULT 0,
    no_go_count INTEGER NOT NULL DEFAULT 0,
    delay_count INTEGER NOT NULL DEFAULT 0,
    pilot_count INTEGER NOT NULL DEFAULT 0,
    
    -- モード別
    fast_count INTEGER NOT NULL DEFAULT 0,
    standard_count INTEGER NOT NULL DEFAULT 0,
    audit_count INTEGER NOT NULL DEFAULT 0,
    
    -- 平均処理時間
    avg_processing_time_ms INTEGER,
    avg_confidence DECIMAL(5,4),
    
    -- コスト
    total_tokens_used INTEGER NOT NULL DEFAULT 0,
    
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- インデックス
CREATE INDEX IF NOT EXISTS idx_decision_history_created ON decision_history(original_created_at DESC);
CREATE INDEX IF NOT EXISTS idx_decision_history_role ON decision_history(decision_role);
CREATE INDEX IF NOT EXISTS idx_decision_history_archived ON decision_history(archived_at DESC);

