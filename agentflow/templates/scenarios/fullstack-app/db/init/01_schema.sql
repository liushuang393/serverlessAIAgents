-- -*- coding: utf-8 -*-
-- {{ app_name }} 初期スキーマ
-- Docker 初回起動時に自動実行

-- UUID 拡張を有効化
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- pg_trgm 拡張（あいまい検索用）
CREATE EXTENSION IF NOT EXISTS pg_trgm;

-- ===========================================
-- サンプルテーブル（実際のスキーマに置き換えること）
-- ===========================================

-- example_entities テーブル
CREATE TABLE IF NOT EXISTS example_entities (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name VARCHAR(255) NOT NULL,
    status VARCHAR(50) NOT NULL DEFAULT 'active',
    description VARCHAR(1000),
    metadata JSONB,
    created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    deleted_at TIMESTAMPTZ
);

-- インデックス
CREATE INDEX IF NOT EXISTS idx_example_entities_name ON example_entities(name);
CREATE INDEX IF NOT EXISTS idx_example_entities_status ON example_entities(status);
CREATE INDEX IF NOT EXISTS idx_example_entities_created_at ON example_entities(created_at);
CREATE INDEX IF NOT EXISTS idx_example_entities_deleted_at ON example_entities(deleted_at);

-- JSONB インデックス（GIN）
CREATE INDEX IF NOT EXISTS idx_example_entities_metadata ON example_entities USING gin(metadata);

-- ===========================================
-- updated_at 自動更新トリガー
-- ===========================================
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_example_entities_updated_at
    BEFORE UPDATE ON example_entities
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- ===========================================
-- 初期データ（オプション）
-- ===========================================
-- INSERT INTO example_entities (name, status, description)
-- VALUES ('Sample', 'active', 'Initial sample data');

COMMENT ON TABLE example_entities IS 'サンプルエンティティ - 実際のエンティティに置き換えること';

