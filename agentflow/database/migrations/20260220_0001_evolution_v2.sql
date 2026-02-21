-- Evolution V2 schema bootstrap
-- Target: PostgreSQL primary, SQLite compatible fallback

CREATE TABLE IF NOT EXISTS evolution_strategy (
    id VARCHAR(64) PRIMARY KEY,
    tenant_id VARCHAR(120),
    app_id VARCHAR(120),
    product_line VARCHAR(64),
    scope_level VARCHAR(40) NOT NULL,
    intent_hash VARCHAR(64) NOT NULL,
    capsule_json JSON NOT NULL,
    status VARCHAR(32) NOT NULL DEFAULT 'candidate',
    version INTEGER NOT NULL DEFAULT 1,
    parent_strategy_id VARCHAR(64),
    valid_from TIMESTAMP,
    valid_to TIMESTAMP,
    last_verified_at TIMESTAMP,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL,
    CONSTRAINT uq_evolution_strategy_intent_version_scope
        UNIQUE (tenant_id, app_id, product_line, scope_level, intent_hash, version),
    CONSTRAINT fk_evolution_strategy_parent
        FOREIGN KEY (parent_strategy_id) REFERENCES evolution_strategy(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS ix_evolution_strategy_scope
    ON evolution_strategy (scope_level, tenant_id, app_id, product_line, status);

CREATE TABLE IF NOT EXISTS evolution_strategy_keyword (
    id BIGINT PRIMARY KEY,
    strategy_id VARCHAR(64) NOT NULL,
    keyword VARCHAR(120) NOT NULL,
    weight DOUBLE PRECISION NOT NULL DEFAULT 1.0,
    CONSTRAINT fk_evolution_strategy_keyword_strategy
        FOREIGN KEY (strategy_id) REFERENCES evolution_strategy(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS ix_evolution_strategy_keyword_keyword
    ON evolution_strategy_keyword (keyword);

CREATE TABLE IF NOT EXISTS evolution_strategy_score (
    strategy_id VARCHAR(64) PRIMARY KEY,
    success_rate_7d DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    success_rate_30d DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    reuse_count_30d INTEGER NOT NULL DEFAULT 0,
    avg_latency_ms DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    freshness_decay DOUBLE PRECISION NOT NULL DEFAULT 1.0,
    suspicion_level DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    final_score DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    sample_count INTEGER NOT NULL DEFAULT 0,
    condition_match DOUBLE PRECISION NOT NULL DEFAULT 1.0,
    cost_efficiency DOUBLE PRECISION NOT NULL DEFAULT 0.5,
    updated_at TIMESTAMP NOT NULL,
    CONSTRAINT fk_evolution_strategy_score_strategy
        FOREIGN KEY (strategy_id) REFERENCES evolution_strategy(id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS evolution_execution_event (
    id BIGINT PRIMARY KEY,
    run_id VARCHAR(80) NOT NULL,
    step_id VARCHAR(80),
    event_type VARCHAR(64) NOT NULL,
    payload_json JSON NOT NULL,
    created_at TIMESTAMP NOT NULL
);

CREATE INDEX IF NOT EXISTS ix_evolution_execution_event_run_id_created_at
    ON evolution_execution_event (run_id, created_at);

CREATE TABLE IF NOT EXISTS evolution_outcome (
    id BIGINT PRIMARY KEY,
    run_id VARCHAR(80) NOT NULL,
    strategy_id VARCHAR(64),
    task_signature_hash VARCHAR(64),
    difficulty DOUBLE PRECISION NOT NULL DEFAULT 0.5,
    self_confidence DOUBLE PRECISION NOT NULL DEFAULT 0.5,
    used_retrieval BOOLEAN NOT NULL DEFAULT FALSE,
    success BOOLEAN NOT NULL DEFAULT TRUE,
    failure_reason TEXT,
    latency_ms DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    token_in INTEGER NOT NULL DEFAULT 0,
    token_out INTEGER NOT NULL DEFAULT 0,
    cost_usd DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    created_at TIMESTAMP NOT NULL,
    CONSTRAINT fk_evolution_outcome_strategy
        FOREIGN KEY (strategy_id) REFERENCES evolution_strategy(id) ON DELETE SET NULL
);

CREATE INDEX IF NOT EXISTS ix_evolution_outcome_strategy_id_created_at
    ON evolution_outcome (strategy_id, created_at);

CREATE TABLE IF NOT EXISTS evolution_validation_result (
    id BIGINT PRIMARY KEY,
    strategy_id VARCHAR(64) NOT NULL,
    job_id VARCHAR(80) NOT NULL,
    status VARCHAR(32) NOT NULL,
    checks_json JSON NOT NULL,
    score_delta DOUBLE PRECISION NOT NULL DEFAULT 0.0,
    created_at TIMESTAMP NOT NULL,
    CONSTRAINT fk_evolution_validation_result_strategy
        FOREIGN KEY (strategy_id) REFERENCES evolution_strategy(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS ix_evolution_validation_result_job_id
    ON evolution_validation_result (job_id);

CREATE INDEX IF NOT EXISTS ix_evolution_validation_result_status
    ON evolution_validation_result (status);
