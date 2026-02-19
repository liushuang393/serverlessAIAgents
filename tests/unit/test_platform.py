"""Unit tests for the Platform module.

Tests cover:
- Component schemas (ComponentType, ComponentVisibility, etc.)
- Gallery schemas (GalleryItemType, GalleryFilter, etc.)
- Publish schemas (PublishTarget, PublishStatus, etc.)
- Pydantic model validation
"""

from datetime import UTC, datetime

from apps.platform.schemas.component_schemas import (
    ComponentCreateRequest,
    ComponentListResponse,
    ComponentResponse,
    ComponentType,
    ComponentVisibility,
)
from apps.platform.schemas.gallery_schemas import (
    FeaturedItem,
    GalleryFilter,
    GalleryItem,
    GalleryItemType,
    GallerySearchRequest,
    GallerySearchResponse,
)
from apps.platform.schemas.publish_schemas import (
    PublishMode,
    PublishPhase,
    PublishRequest,
    PublishResponse,
    PublishStatus,
    PublishTarget,
)


class TestComponentType:
    """Tests for ComponentType enum."""

    def test_component_types(self) -> None:
        """Test all component types exist."""
        assert ComponentType.AGENT.value == "agent"
        assert ComponentType.FLOW.value == "flow"
        assert ComponentType.TOOL.value == "tool"
        assert ComponentType.SKILL.value == "skill"
        assert ComponentType.ENGINE.value == "engine"
        assert ComponentType.TEMPLATE.value == "template"


class TestComponentVisibility:
    """Tests for ComponentVisibility enum."""

    def test_visibility_levels(self) -> None:
        """Test all visibility levels exist."""
        assert ComponentVisibility.PRIVATE.value == "private"
        assert ComponentVisibility.TENANT.value == "tenant"
        assert ComponentVisibility.PUBLIC.value == "public"


class TestComponentCreateRequest:
    """Tests for ComponentCreateRequest schema."""

    def test_minimal_create_request(self) -> None:
        """Test creating a component with minimal fields."""
        request = ComponentCreateRequest(
            name="TestComponent",
            type=ComponentType.AGENT,
        )
        assert request.name == "TestComponent"
        assert request.type == ComponentType.AGENT
        assert request.version == "1.0.0"
        assert request.visibility == ComponentVisibility.PRIVATE

    def test_full_create_request(self) -> None:
        """Test creating a component with all fields."""
        request = ComponentCreateRequest(
            name="FullComponent",
            type=ComponentType.SKILL,
            version="2.0.0",
            description="A full component",
            category="utility",
            tags=["test", "unit"],
            visibility=ComponentVisibility.PUBLIC,
            source_code="def run(): pass",
            config={"key": "value"},
            dependencies=["dep1", "dep2"],
            protocols=["mcp", "a2a"],
            metadata={"author": "test"},
        )
        assert request.name == "FullComponent"
        assert request.version == "2.0.0"
        assert len(request.tags) == 2
        assert request.visibility == ComponentVisibility.PUBLIC


class TestComponentResponse:
    """Tests for ComponentResponse schema."""

    def test_component_response(self) -> None:
        """Test ComponentResponse creation."""
        now = datetime.now(UTC)
        response = ComponentResponse(
            id="comp-123",
            name="TestComp",
            type=ComponentType.FLOW,
            version="1.0.0",
            visibility=ComponentVisibility.TENANT,
            created_at=now,
            updated_at=now,
        )
        assert response.id == "comp-123"
        assert response.type == ComponentType.FLOW
        assert response.usage_count == 0


class TestComponentListResponse:
    """Tests for ComponentListResponse schema."""

    def test_empty_list(self) -> None:
        """Test empty component list."""
        response = ComponentListResponse()
        assert len(response.items) == 0
        assert response.total == 0

    def test_list_with_items(self) -> None:
        """Test component list with items."""
        now = datetime.now(UTC)
        items = [
            ComponentResponse(
                id=f"comp-{i}",
                name=f"Component{i}",
                type=ComponentType.AGENT,
                version="1.0.0",
                visibility=ComponentVisibility.PRIVATE,
                created_at=now,
                updated_at=now,
            )
            for i in range(3)
        ]
        response = ComponentListResponse(items=items, total=3, limit=20, offset=0)
        assert len(response.items) == 3
        assert response.total == 3


class TestGalleryItemType:
    """Tests for GalleryItemType enum."""

    def test_item_types(self) -> None:
        """Test all gallery item types exist."""
        assert GalleryItemType.AGENT.value == "agent"
        assert GalleryItemType.FLOW.value == "flow"
        assert GalleryItemType.TOOL.value == "tool"
        assert GalleryItemType.SKILL.value == "skill"
        assert GalleryItemType.ENGINE.value == "engine"
        assert GalleryItemType.TEMPLATE.value == "template"
        assert GalleryItemType.APP.value == "app"


class TestGalleryFilter:
    """Tests for GalleryFilter schema."""

    def test_default_filter(self) -> None:
        """Test default filter values."""
        filter_obj = GalleryFilter()
        assert filter_obj.types is None
        assert filter_obj.verified_only is False
        assert filter_obj.include_local is True
        assert filter_obj.include_marketplace is True

    def test_custom_filter(self) -> None:
        """Test custom filter values."""
        filter_obj = GalleryFilter(
            types=[GalleryItemType.AGENT, GalleryItemType.SKILL],
            categories=["utility", "ai"],
            protocols=["mcp"],
            min_rating=3.5,
            verified_only=True,
        )
        assert len(filter_obj.types) == 2
        assert filter_obj.min_rating == 3.5
        assert filter_obj.verified_only is True


class TestGallerySearchRequest:
    """Tests for GallerySearchRequest schema."""

    def test_search_request(self) -> None:
        """Test search request creation."""
        request = GallerySearchRequest(query="PDF parser")
        assert request.query == "PDF parser"
        assert request.limit == 20
        assert request.offset == 0
        assert request.sort_by == "relevance"

    def test_search_request_with_filter(self) -> None:
        """Test search request with filter."""
        request = GallerySearchRequest(
            query="agent",
            filter=GalleryFilter(types=[GalleryItemType.AGENT]),
            limit=50,
            sort_by="downloads",
            sort_order="desc",
        )
        assert request.query == "agent"
        assert request.limit == 50
        assert len(request.filter.types) == 1


class TestGalleryItem:
    """Tests for GalleryItem schema."""

    def test_gallery_item(self) -> None:
        """Test GalleryItem creation."""
        item = GalleryItem(
            id="gallery-123",
            name="PDF Analyzer",
            type=GalleryItemType.AGENT,
            version="1.0.0",
            description="Analyzes PDF documents",
            author="test_author",
            tags=["pdf", "analysis"],
            rating=4.5,
            downloads=1000,
            verified=True,
        )
        assert item.id == "gallery-123"
        assert item.rating == 4.5
        assert item.verified is True
        assert item.icon == "📦"  # default


class TestGallerySearchResponse:
    """Tests for GallerySearchResponse schema."""

    def test_search_response(self) -> None:
        """Test search response creation."""
        items = [
            GalleryItem(
                id=f"item-{i}",
                name=f"Item{i}",
                type=GalleryItemType.SKILL,
                version="1.0.0",
            )
            for i in range(5)
        ]
        response = GallerySearchResponse(
            items=items,
            total=100,
            limit=5,
            offset=0,
            query="test",
            has_more=True,
        )
        assert len(response.items) == 5
        assert response.total == 100
        assert response.has_more is True


class TestFeaturedItem:
    """Tests for FeaturedItem schema."""

    def test_featured_item(self) -> None:
        """Test FeaturedItem creation."""
        item = GalleryItem(
            id="featured-1",
            name="Best Agent",
            type=GalleryItemType.AGENT,
            version="2.0.0",
        )
        featured = FeaturedItem(
            item=item,
            featured_reason="Most downloaded this month",
            priority=1,
        )
        assert featured.item.name == "Best Agent"
        assert featured.priority == 1


class TestPublishTarget:
    """Tests for PublishTarget enum."""

    def test_publish_targets(self) -> None:
        """Test all publish targets exist."""
        assert PublishTarget.DOCKER.value == "docker"
        assert PublishTarget.VERCEL.value == "vercel"
        assert PublishTarget.AWS_LAMBDA.value == "aws_lambda"
        assert PublishTarget.GITHUB_ACTIONS.value == "github_actions"
        assert PublishTarget.LOCAL.value == "local"
        assert PublishTarget.GALLERY.value == "gallery"


class TestPublishStatus:
    """Tests for PublishStatus enum."""

    def test_publish_statuses(self) -> None:
        """Test all publish statuses exist."""
        assert PublishStatus.PENDING.value == "pending"
        assert PublishStatus.VALIDATING.value == "validating"
        assert PublishStatus.GENERATING.value == "generating"
        assert PublishStatus.DEPLOYING.value == "deploying"
        assert PublishStatus.REGISTERING.value == "registering"
        assert PublishStatus.COMPLETED.value == "completed"
        assert PublishStatus.FAILED.value == "failed"
        assert PublishStatus.CANCELLED.value == "cancelled"


class TestPublishMode:
    """Tests for PublishMode enum."""

    def test_publish_modes(self) -> None:
        """Test all publish modes exist."""
        assert PublishMode.STUDIO.value == "studio"
        assert PublishMode.CLI.value == "cli"
        assert PublishMode.API.value == "api"
        assert PublishMode.AI_ASSISTANT.value == "ai_assistant"


class TestPublishRequest:
    """Tests for PublishRequest schema."""

    def test_minimal_request(self) -> None:
        """Test minimal publish request."""
        request = PublishRequest(
            component_id="comp-123",
            target=PublishTarget.DOCKER,
        )
        assert request.component_id == "comp-123"
        assert request.target == PublishTarget.DOCKER
        assert request.mode == PublishMode.API

    def test_full_request(self) -> None:
        """Test full publish request."""
        request = PublishRequest(
            source_path="/path/to/agent",
            target=PublishTarget.VERCEL,
            version="2.0.0",
            name="my-agent",
            description="My agent deployment",
            mode=PublishMode.CLI,
            config={"region": "us-east-1"},
            publish_to_gallery=True,
            gallery_visibility="public",
            env_vars={"API_KEY": "secret"},
        )
        assert request.source_path == "/path/to/agent"
        assert request.version == "2.0.0"
        assert request.publish_to_gallery is True


class TestPublishPhase:
    """Tests for PublishPhase schema."""

    def test_phase_creation(self) -> None:
        """Test phase creation."""
        now = datetime.now(UTC)
        phase = PublishPhase(
            name="validation",
            status=PublishStatus.COMPLETED,
            message="Validation passed",
            progress=100.0,
            started_at=now,
            completed_at=now,
        )
        assert phase.name == "validation"
        assert phase.status == PublishStatus.COMPLETED
        assert phase.progress == 100.0


class TestPublishResponse:
    """Tests for PublishResponse schema."""

    def test_pending_response(self) -> None:
        """Test pending publish response."""
        now = datetime.now(UTC)
        response = PublishResponse(
            publish_id="pub-123",
            status=PublishStatus.PENDING,
            target=PublishTarget.DOCKER,
            started_at=now,
        )
        assert response.publish_id == "pub-123"
        assert response.status == PublishStatus.PENDING
        assert response.completed_at is None

    def test_completed_response(self) -> None:
        """Test completed publish response."""
        now = datetime.now(UTC)
        phases = [
            PublishPhase(name="validate", status=PublishStatus.COMPLETED, progress=100.0),
            PublishPhase(name="generate", status=PublishStatus.COMPLETED, progress=100.0),
            PublishPhase(name="deploy", status=PublishStatus.COMPLETED, progress=100.0),
        ]
        response = PublishResponse(
            publish_id="pub-456",
            status=PublishStatus.COMPLETED,
            target=PublishTarget.VERCEL,
            phases=phases,
            current_phase="deploy",
            progress=100.0,
            started_at=now,
            completed_at=now,
            deployment_id="dep-789",
            deployment_url="https://my-agent.vercel.app",
        )
        assert response.status == PublishStatus.COMPLETED
        assert len(response.phases) == 3
        assert response.deployment_url is not None
