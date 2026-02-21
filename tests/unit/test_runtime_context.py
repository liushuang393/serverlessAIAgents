"""RuntimeContext platform tests."""

import pytest

from agentflow.config import AgentFlowSettings
from agentflow.core.result_store import MemoryResultStore, ResultStoreManager
from agentflow.providers import get_db, get_llm, reset_db, reset_llm
from agentflow.runtime import RuntimeContext, get_runtime_context, use_runtime_context


@pytest.fixture(autouse=True)
def reset_singletons():
    """Reset provider singletons between tests."""
    reset_llm()
    reset_db()
    yield
    reset_llm()
    reset_db()


def test_runtime_context_scope():
    ctx = RuntimeContext(tenant_id="tenant-1")
    assert get_runtime_context() is None
    with use_runtime_context(ctx):
        assert get_runtime_context() == ctx
    assert get_runtime_context() is None


def test_llm_context_settings_override():
    # llm_provider="auto" を指定してクレデンシャルからの自動検出を強制する
    # (.env の LLM_PROVIDER=mock が上書きされないように)
    settings = AgentFlowSettings(
        openai_api_key="sk-test",
        openai_model="gpt-test",
        llm_provider="auto",
    )
    ctx = RuntimeContext(tenant_id="tenant-1", settings=settings)
    llm = get_llm(context=ctx, _new_instance=True)
    assert llm._provider_info[0] == "openai"
    assert llm._provider_info[1] == "gpt-test"


def test_db_context_settings_override():
    settings = AgentFlowSettings(
        supabase_url="https://example.supabase.co",
        supabase_key="test-key",
    )
    ctx = RuntimeContext(tenant_id="tenant-1", settings=settings)
    db = get_db(context=ctx, _new_instance=True)
    assert db.get_provider_name() == "supabase"


@pytest.mark.asyncio
async def test_result_store_tenant_id_from_context():
    ResultStoreManager.set_store(MemoryResultStore())
    ctx = RuntimeContext(tenant_id="tenant-42")
    with use_runtime_context(ctx):
        result = await ResultStoreManager.save(
            result_id="result-1",
            data={"ok": True},
            flow_id="flow-1",
        )
    assert result.tenant_id == "tenant-42"
