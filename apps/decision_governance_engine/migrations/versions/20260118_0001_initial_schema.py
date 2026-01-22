# -*- coding: utf-8 -*-
"""
初期スキーマ作成.

Revision ID: 0001
Revises:
Create Date: 2026-01-18

目的:
    - decision_records テーブル作成
    - evidence_items テーブル作成
    - claims テーブル作成
    - claim_evidence_refs テーブル作成
    - インデックス・トリガー作成
"""
from typing import Sequence, Union

from alembic import op
import sqlalchemy as sa
from sqlalchemy.dialects import postgresql

# revision identifiers, used by Alembic.
revision: str = '0001'
down_revision: Union[str, None] = None
branch_labels: Union[str, Sequence[str], None] = None
depends_on: Union[str, Sequence[str], None] = None


def upgrade() -> None:
    """スキーマ作成."""
    # decision_records テーブル
    op.create_table(
        'decision_records',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True,
                  server_default=sa.text('gen_random_uuid()')),
        sa.Column('request_id', postgresql.UUID(as_uuid=True), nullable=False, unique=True),
        sa.Column('question', sa.Text(), nullable=False),
        sa.Column('decision_role', sa.String(20), nullable=False),
        sa.Column('confidence', sa.Numeric(5, 4), nullable=True),
        sa.Column('mode', sa.String(20), nullable=False, server_default='STANDARD'),
        sa.Column('fa_result', postgresql.JSONB(), nullable=True),
        sa.Column('shu_result', postgresql.JSONB(), nullable=True),
        sa.Column('qi_result', postgresql.JSONB(), nullable=True),
        sa.Column('review_result', postgresql.JSONB(), nullable=True),
        sa.Column('summary_bullets', postgresql.JSONB(), nullable=True),
        sa.Column('warnings', postgresql.JSONB(), nullable=True),
        sa.Column('processing_time_ms', sa.Integer(), nullable=True),
        sa.Column('created_at', sa.DateTime(timezone=True),
                  server_default=sa.text('CURRENT_TIMESTAMP'), nullable=False),
        sa.Column('updated_at', sa.DateTime(timezone=True),
                  server_default=sa.text('CURRENT_TIMESTAMP'), nullable=False),
        sa.Column('deleted_at', sa.DateTime(timezone=True), nullable=True),
    )

    # evidence_items テーブル
    op.create_table(
        'evidence_items',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True,
                  server_default=sa.text('gen_random_uuid()')),
        sa.Column('decision_id', postgresql.UUID(as_uuid=True),
                  sa.ForeignKey('decision_records.id', ondelete='CASCADE'), nullable=False),
        sa.Column('evidence_id', sa.String(100), nullable=False),
        sa.Column('source_type', sa.String(50), nullable=False),
        sa.Column('source_url', sa.Text(), nullable=True),
        sa.Column('title', sa.Text(), nullable=True),
        sa.Column('snippet', sa.Text(), nullable=True),
        sa.Column('reliability', sa.String(20), nullable=False, server_default='MEDIUM'),
        sa.Column('fetched_at', sa.DateTime(timezone=True), nullable=True),
        sa.Column('created_at', sa.DateTime(timezone=True),
                  server_default=sa.text('CURRENT_TIMESTAMP'), nullable=False),
    )

    # claims テーブル
    op.create_table(
        'claims',
        sa.Column('id', postgresql.UUID(as_uuid=True), primary_key=True,
                  server_default=sa.text('gen_random_uuid()')),
        sa.Column('decision_id', postgresql.UUID(as_uuid=True),
                  sa.ForeignKey('decision_records.id', ondelete='CASCADE'), nullable=False),
        sa.Column('claim_id', sa.String(100), nullable=False),
        sa.Column('claim_type', sa.String(30), nullable=False),
        sa.Column('statement', sa.Text(), nullable=False),
        sa.Column('confidence', sa.Numeric(5, 4), nullable=True),
        sa.Column('created_at', sa.DateTime(timezone=True),
                  server_default=sa.text('CURRENT_TIMESTAMP'), nullable=False),
    )

    # claim_evidence_refs テーブル
    op.create_table(
        'claim_evidence_refs',
        sa.Column('claim_id', postgresql.UUID(as_uuid=True),
                  sa.ForeignKey('claims.id', ondelete='CASCADE'), primary_key=True),
        sa.Column('evidence_id', postgresql.UUID(as_uuid=True),
                  sa.ForeignKey('evidence_items.id', ondelete='CASCADE'), primary_key=True),
    )

    # インデックス作成
    op.create_index('idx_decision_records_request_id', 'decision_records', ['request_id'])
    op.create_index('idx_decision_records_decision_role', 'decision_records', ['decision_role'])
    op.create_index('idx_decision_records_created_at', 'decision_records', ['created_at'])
    op.create_index('idx_decision_records_deleted_at', 'decision_records', ['deleted_at'])
    op.create_index('idx_evidence_items_decision_id', 'evidence_items', ['decision_id'])
    op.create_index('idx_claims_decision_id', 'claims', ['decision_id'])

    # 全文検索インデックス（PostgreSQL）
    op.execute("""
        CREATE INDEX idx_decision_records_question_fts
        ON decision_records
        USING gin(to_tsvector('english', question))
    """)


def downgrade() -> None:
    """スキーマ削除."""
    op.drop_index('idx_decision_records_question_fts', 'decision_records')
    op.drop_index('idx_claims_decision_id', 'claims')
    op.drop_index('idx_evidence_items_decision_id', 'evidence_items')
    op.drop_index('idx_decision_records_deleted_at', 'decision_records')
    op.drop_index('idx_decision_records_created_at', 'decision_records')
    op.drop_index('idx_decision_records_decision_role', 'decision_records')
    op.drop_index('idx_decision_records_request_id', 'decision_records')

    op.drop_table('claim_evidence_refs')
    op.drop_table('claims')
    op.drop_table('evidence_items')
    op.drop_table('decision_records')

