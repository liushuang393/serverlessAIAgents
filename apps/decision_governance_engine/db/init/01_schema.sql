-- ==============================================================================
-- Decision Governance Engine - メインDB スキーマ
-- ==============================================================================
-- 目的: 決策履歴・証拠・クレームの永続化
-- 文字コード: UTF-8
-- ==============================================================================

-- 決策記録メインテーブル
CREATE TABLE IF NOT EXISTS decision_records (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    request_id UUID NOT NULL UNIQUE,
    
    -- 入力情報
    question TEXT NOT NULL,
    mode VARCHAR(20) NOT NULL DEFAULT 'STANDARD',  -- FAST/STANDARD/AUDIT
    
    -- 決策結果
    decision_role VARCHAR(20) NOT NULL,  -- GO/NO_GO/DELAY/PILOT
    confidence DECIMAL(5,4),  -- 0.0000 ~ 1.0000
    
    -- 各セクション結果（JSONB で柔軟に格納）
    cognitive_gate_result JSONB,
    gatekeeper_result JSONB,
    dao_result JSONB,
    fa_result JSONB,
    shu_result JSONB,
    qi_result JSONB,
    review_result JSONB,
    
    -- サマリ
    summary_bullets JSONB,  -- string[]
    warnings JSONB,         -- string[]
    
    -- メタ情報
    requester_role VARCHAR(50),
    organization_size VARCHAR(20),
    
    -- 監査情報
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    processing_time_ms INTEGER,
    llm_tokens_used INTEGER,
    
    -- ソフト削除
    deleted_at TIMESTAMPTZ
);

-- 証拠テーブル（外部情報源）
CREATE TABLE IF NOT EXISTS evidence_items (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    decision_record_id UUID NOT NULL REFERENCES decision_records(id) ON DELETE CASCADE,
    
    -- 証拠情報
    url TEXT,
    title TEXT,
    publisher TEXT,
    snippet TEXT,           -- 原文片段
    summary TEXT,           -- 要約
    
    -- 信頼性
    reliability VARCHAR(10) NOT NULL DEFAULT 'MEDIUM',  -- LOW/MEDIUM/HIGH
    
    -- 分類タグ
    tags JSONB,  -- string[]
    
    -- 時間情報
    retrieved_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    published_at TIMESTAMPTZ,
    
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- クレームテーブル（主張・断言）
CREATE TABLE IF NOT EXISTS claims (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    decision_record_id UUID NOT NULL REFERENCES decision_records(id) ON DELETE CASCADE,
    
    -- クレーム内容
    claim_type VARCHAR(20) NOT NULL,  -- FACT/INFERENCE/ASSUMPTION/RECOMMENDATION
    statement TEXT NOT NULL,
    source_section VARCHAR(20),  -- fa/shu/qi/dao/review
    confidence DECIMAL(5,4),
    
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- クレーム-証拠の関連テーブル
CREATE TABLE IF NOT EXISTS claim_evidence_refs (
    claim_id UUID NOT NULL REFERENCES claims(id) ON DELETE CASCADE,
    evidence_id UUID NOT NULL REFERENCES evidence_items(id) ON DELETE CASCADE,
    PRIMARY KEY (claim_id, evidence_id)
);

-- インデックス
CREATE INDEX IF NOT EXISTS idx_decision_records_created_at ON decision_records(created_at DESC);
CREATE INDEX IF NOT EXISTS idx_decision_records_decision_role ON decision_records(decision_role);
CREATE INDEX IF NOT EXISTS idx_decision_records_mode ON decision_records(mode);
CREATE INDEX IF NOT EXISTS idx_decision_records_requester ON decision_records(requester_role);
CREATE INDEX IF NOT EXISTS idx_evidence_items_decision ON evidence_items(decision_record_id);
CREATE INDEX IF NOT EXISTS idx_evidence_items_reliability ON evidence_items(reliability);
CREATE INDEX IF NOT EXISTS idx_claims_decision ON claims(decision_record_id);
CREATE INDEX IF NOT EXISTS idx_claims_type ON claims(claim_type);

-- 全文検索用インデックス（PostgreSQL）
CREATE INDEX IF NOT EXISTS idx_decision_records_question_gin ON decision_records USING gin(to_tsvector('simple', question));

-- 更新トリガー
CREATE OR REPLACE FUNCTION update_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER tr_decision_records_updated_at
    BEFORE UPDATE ON decision_records
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at();

