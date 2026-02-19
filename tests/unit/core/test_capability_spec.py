"""AgentCapabilitySpecモデルのテスト.

Agent能力仕様モデルのユニットテスト。
"""


def test_capability_spec_creation():
    """基本的なAgentCapabilitySpec作成のテスト."""
    from agentflow.core.capability_spec import AgentCapabilitySpec

    cap = AgentCapabilitySpec(
        id="pdf_analysis_v1",
        name="PDF Analysis",
        description="PDF文書を分析して情報を抽出",
        input_schema={"type": "object", "properties": {"pdf_path": {"type": "string"}}},
        output_schema={"type": "object", "properties": {"text": {"type": "string"}}},
        required_tools=["tool://mcp/filesystem/read_file"],
        tags=["pdf", "analysis", "extraction"],
    )

    assert cap.id == "pdf_analysis_v1"
    assert cap.name == "PDF Analysis"
    assert len(cap.required_tools) == 1
    assert "pdf" in cap.tags


def test_capability_spec_default_confidence():
    """デフォルト信頼度が1.0のテスト."""
    from agentflow.core.capability_spec import AgentCapabilitySpec

    cap = AgentCapabilitySpec(
        id="test",
        name="Test",
        description="テスト用能力",
    )

    assert cap.confidence == 1.0


def test_capability_spec_llm_config():
    """LLM設定のテスト."""
    from agentflow.core.capability_spec import AgentCapabilitySpec, LLMRequirements

    cap = AgentCapabilitySpec(
        id="creative_writing",
        name="Creative Writing",
        description="創造的なコンテンツを生成",
        llm_requirements=LLMRequirements(
            model="claude-3-opus",
            temperature=0.9,
            max_tokens=4000,
        ),
    )

    assert cap.llm_requirements.temperature == 0.9
    assert cap.llm_requirements.model == "claude-3-opus"


def test_capability_spec_matches_requirements():
    """要件マッチングのテスト."""
    from agentflow.core.capability_spec import AgentCapabilitySpec, CapabilityRequirement

    cap = AgentCapabilitySpec(
        id="pdf_analysis",
        name="PDF Analysis",
        description="PDF文書を分析",
        tags=["pdf", "analysis"],
        required_tools=["tool://mcp/filesystem/read_file"],
    )

    req = CapabilityRequirement(
        description="PDFレポートを分析",
        required_tags=["pdf"],
        required_tools=["tool://mcp/filesystem/read_file"],
    )

    score = cap.matches(req)

    assert score > 0.5  # 良いマッチ


def test_capability_spec_from_agent_config():
    """Agent設定からCapabilitySpec作成のテスト."""
    from agentflow.core.capability_spec import AgentCapabilitySpec

    config = {
        "name": "SummarizerAgent",
        "description": "テキストドキュメントを要約",
        "skills": ["summarization", "text_analysis"],
        "tools": ["tool://builtin/tokenizer"],
    }

    cap = AgentCapabilitySpec.from_agent_config(config)

    assert cap.id == "summarizeragent"
    assert cap.name == "SummarizerAgent"
    assert "summarization" in cap.tags


def test_capability_requirement_creation():
    """CapabilityRequirement作成のテスト."""
    from agentflow.core.capability_spec import CapabilityRequirement

    req = CapabilityRequirement(
        description="PDFを分析して要約を作成",
        required_tags=["pdf", "summarization"],
        required_tools=["tool://mcp/filesystem/read_file"],
        min_confidence=0.7,
    )

    assert "pdf" in req.required_tags
    assert req.min_confidence == 0.7


def test_llm_requirements_defaults():
    """LLMRequirementsデフォルト値のテスト."""
    from agentflow.core.capability_spec import LLMRequirements

    llm = LLMRequirements()

    assert llm.model is None  # デフォルトはNone（自動選択）
    assert llm.temperature == 0.7
    assert llm.max_tokens == 2000


def test_capability_spec_no_match():
    """マッチしない要件のテスト."""
    from agentflow.core.capability_spec import AgentCapabilitySpec, CapabilityRequirement

    cap = AgentCapabilitySpec(
        id="pdf_analysis",
        name="PDF Analysis",
        description="PDF文書を分析",
        tags=["pdf"],
    )

    req = CapabilityRequirement(
        description="画像認識",
        required_tags=["image", "vision"],
    )

    score = cap.matches(req)

    assert score < 0.3  # 低いマッチ
