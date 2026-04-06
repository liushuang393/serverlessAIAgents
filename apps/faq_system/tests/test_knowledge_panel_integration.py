"""ナレッジベース管理の統合テスト。

コレクション作成→ドキュメントアップロード→インデックス、
設定変更→キャッシュ無効化、アクセスマトリクス→RBAC の
各フローをモックレベルで検証する。
"""

from __future__ import annotations

from typing import Any
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from apps.faq_system.backend.security.permission_config import (
    KBPermission,
    PermissionConfig,
    PermissionConfigData,
    RolePermissions,
)
from apps.faq_system.routers.access_control import (
    _build_matrix,
    get_access_matrix,
)
from apps.faq_system.routers.collections import (
    CreateCollectionRequest,
    TestQueryRequest,
    UpdateCollectionRequest,
    create_collection,
    index_document,
    test_query,
    update_collection,
    upload_document,
)


def _fake_user(role: str = "admin") -> MagicMock:
    """テスト用ユーザー情報を生成."""
    user = MagicMock()
    user.role = role
    user.user_id = "test-user-001"
    return user


def _rag_enabled_patches() -> dict[str, Any]:
    """RAG 有効化用の共通パッチターゲットを返す."""
    return {
        "apps.faq_system.routers.collections.is_rag_enabled": True,
    }


class TestKnowledgeManagementIntegration:
    """ナレッジベース管理のフローテスト。"""

    @pytest.mark.asyncio
    async def test_collection_create_upload_index_flow(self) -> None:
        """コレクション作成→ドキュメントアップロード→インデックスのフルフロー。

        各ステップで正しいサービスメソッドが呼ばれることを検証。
        """
        # --- Step 1: コレクション作成 ---
        fake_collection_model = MagicMock()
        fake_collection_model.to_dict.return_value = {
            "collection_name": "test_kb",
            "display_name": "テスト KB",
            "top_k": 5,
        }

        fake_col_mgr = MagicMock()
        fake_col_mgr.create_collection = AsyncMock(return_value=fake_collection_model)

        create_req = CreateCollectionRequest(
            collection_name="test_kb",
            display_name="テスト KB",
            description="統合テスト用コレクション",
        )

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_col_mgr",
                return_value=fake_col_mgr,
            ),
        ):
            result = await create_collection(
                request=create_req,
                user=_fake_user("admin"),
            )

        assert result == {"collection": fake_collection_model.to_dict.return_value}
        fake_col_mgr.create_collection.assert_awaited_once()
        call_kwargs = fake_col_mgr.create_collection.call_args.kwargs
        assert call_kwargs["collection_name"] == "test_kb"
        assert call_kwargs["display_name"] == "テスト KB"

        # --- Step 2: ドキュメントアップロード ---
        fake_doc_record = MagicMock()
        fake_doc_record.document_id = "doc-001"
        fake_doc_record.to_dict.return_value = {
            "document_id": "doc-001",
            "collection_name": "test_kb",
            "filename": "manual.pdf",
            "status": "uploaded",
        }

        fake_doc_mgr = MagicMock()
        fake_doc_mgr.upload_document = AsyncMock(return_value=fake_doc_record)

        fake_upload_file = MagicMock()
        fake_upload_file.read = AsyncMock(return_value=b"dummy pdf content")
        fake_upload_file.filename = "manual.pdf"

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_doc_mgr",
                return_value=fake_doc_mgr,
            ),
        ):
            upload_result = await upload_document(
                name="test_kb",
                file=fake_upload_file,
                auto_index=False,
                user=_fake_user("admin"),
            )

        assert upload_result["document"]["document_id"] == "doc-001"
        assert upload_result["document"]["filename"] == "manual.pdf"
        fake_doc_mgr.upload_document.assert_awaited_once()
        upload_kwargs = fake_doc_mgr.upload_document.call_args.kwargs
        assert upload_kwargs["collection_name"] == "test_kb"
        assert upload_kwargs["filename"] == "manual.pdf"

        # --- Step 3: インデックス登録 ---
        indexed_record = MagicMock()
        indexed_record.to_dict.return_value = {
            "document_id": "doc-001",
            "status": "indexed",
        }

        fake_doc_mgr_for_index = MagicMock()
        fake_doc_mgr_for_index.index_document = AsyncMock(return_value=indexed_record)

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_doc_mgr",
                return_value=fake_doc_mgr_for_index,
            ),
        ):
            index_result = await index_document(
                name="test_kb",
                doc_id="doc-001",
                _user=_fake_user("admin"),
            )

        assert index_result["document"]["status"] == "indexed"
        fake_doc_mgr_for_index.index_document.assert_awaited_once_with("doc-001")

    @pytest.mark.asyncio
    async def test_settings_change_invalidates_cache(self) -> None:
        """設定変更（top_k）が RAGService キャッシュを無効化することを検証。"""
        fake_model = MagicMock()
        fake_model.to_dict.return_value = {
            "collection_name": "my_col",
            "top_k": 20,
        }

        fake_mgr = MagicMock()
        fake_mgr.update_collection = AsyncMock(return_value=fake_model)

        request = UpdateCollectionRequest(top_k=20)

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_col_mgr",
                return_value=fake_mgr,
            ),
            patch(
                "apps.faq_system.routers.collections.invalidate_service_cache",
            ) as mock_invalidate,
        ):
            result = await update_collection(
                name="my_col",
                request=request,
                _user=_fake_user("admin"),
            )

        # top_k=20 へ変更後、キャッシュ無効化が発火する
        mock_invalidate.assert_called_once_with("rag:my_col", "faq_agent")

        # 更新結果が正しく返る
        assert result["collection"]["top_k"] == 20

        # CollectionManager.update_collection が正しい引数で呼ばれた
        fake_mgr.update_collection.assert_awaited_once_with(
            "my_col",
            {"top_k": 20},
        )

    @pytest.mark.asyncio
    async def test_test_query_route_delegates_to_collection_test_query_service(self) -> None:
        """test-query route が route 独自補完ではなく service へ委譲することを検証。"""
        from shared.services.rag_service import ChunkStrategy, RAGConfig, RerankerType

        rag_config = RAGConfig(
            collection="faq_main",
            chunk_strategy=ChunkStrategy.SENTENCE,
            chunk_size=500,
            chunk_overlap=80,
            retrieval_method="hybrid",
            reranker=RerankerType.BM25,
            top_k=8,
            min_similarity=0.15,
        )

        fake_col_mgr = MagicMock()
        fake_col_mgr.build_rag_config = AsyncMock(return_value=rag_config)
        fake_doc_mgr = MagicMock()

        fake_service = MagicMock()
        fake_service.run_test_query = AsyncMock(
            return_value={
                "answer": "Follow the latest update notice.",
                "documents": [{"source": "travel_policy_official_2025.pdf"}],
                "related_documents": [{"source": "travel_faq_exceptions.docx"}],
                "query": "policy conflict",
                "total": 2,
                "diagnostics": {
                    "retrieval_method": "hybrid",
                    "used_lexical_fallback": True,
                    "expanded_related_count": 1,
                    "answer_mode": "policy_conflict_guided",
                },
            }
        )
        fake_service.cleanup = AsyncMock(return_value=None)

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_col_mgr",
                return_value=fake_col_mgr,
            ),
            patch(
                "apps.faq_system.routers.collections._get_doc_mgr",
                return_value=fake_doc_mgr,
            ),
            patch(
                "apps.faq_system.routers.collections.CollectionTestQueryService",
                return_value=fake_service,
            ) as mock_query_service,
        ):
            result = await test_query(
                name="faq_main",
                request=TestQueryRequest(query="policy conflict", top_k=8),
                _user=_fake_user("admin"),
            )

        assert result["diagnostics"]["retrieval_method"] == "hybrid"
        mock_query_service.assert_called_once_with(
            collection_name="faq_main",
            rag_config=rag_config,
            document_manager=fake_doc_mgr,
        )
        fake_service.run_test_query.assert_awaited_once_with(
            query="policy conflict",
            top_k=8,
            expand_related=True,
        )
        fake_service.cleanup.assert_awaited_once()

    @pytest.mark.asyncio
    async def test_access_matrix_reflects_rbac(self) -> None:
        """アクセスマトリクスが RBAC 設定を正しく反映することを検証。"""
        # カスタムロール定義: operator は internal:read と external:read のみ
        custom_roles = {
            "admin": RolePermissions(
                role="admin",
                description="管理者",
                kb_permissions=list(KBPermission),
            ),
            "operator": RolePermissions(
                role="operator",
                description="オペレーター",
                kb_permissions=[
                    KBPermission.INTERNAL_READ,
                    KBPermission.EXTERNAL_READ,
                ],
            ),
            "viewer": RolePermissions(
                role="viewer",
                description="閲覧者",
                kb_permissions=[
                    KBPermission.EXTERNAL_READ,
                ],
            ),
        }
        config_data = PermissionConfigData(roles=custom_roles)
        config = PermissionConfig(config=config_data)

        matrix = _build_matrix(config)

        # admin: 全 KB タイプにアクセス可
        assert matrix["admin"]["internal"] is True
        assert matrix["admin"]["external"] is True
        assert matrix["admin"]["confidential"] is True

        # operator: internal と external のみ
        assert matrix["operator"]["internal"] is True
        assert matrix["operator"]["external"] is True
        assert matrix["operator"]["confidential"] is False

        # viewer: external のみ
        assert matrix["viewer"]["internal"] is False
        assert matrix["viewer"]["external"] is True
        assert matrix["viewer"]["confidential"] is False

    @pytest.mark.asyncio
    async def test_access_matrix_endpoint_uses_permission_config(self) -> None:
        """GET /api/access/matrix がシングルトン PermissionConfig を使用する。"""
        fake_config = MagicMock(spec=PermissionConfig)
        fake_config.list_roles.return_value = ["admin"]
        fake_role_perms = MagicMock()
        fake_role_perms.kb_permissions = [
            KBPermission.INTERNAL_READ,
            KBPermission.EXTERNAL_READ,
        ]
        fake_config.get_role_permissions.return_value = fake_role_perms

        with patch(
            "apps.faq_system.routers.access_control._get_permission_config",
            return_value=fake_config,
        ):
            result = await get_access_matrix(_user=_fake_user("admin"))

        assert "matrix" in result
        # admin は internal:read と external:read を持つ
        assert result["matrix"]["admin"]["internal"] is True
        assert result["matrix"]["admin"]["external"] is True
        assert result["matrix"]["admin"]["confidential"] is False

    @pytest.mark.asyncio
    async def test_upload_with_auto_index_triggers_indexing(self) -> None:
        """auto_index=True でアップロード時にインデックスが自動実行される。"""
        uploaded_record = MagicMock()
        uploaded_record.document_id = "doc-auto"
        uploaded_record.to_dict.return_value = {
            "document_id": "doc-auto",
            "status": "uploaded",
        }

        indexed_record = MagicMock()
        indexed_record.to_dict.return_value = {
            "document_id": "doc-auto",
            "status": "indexed",
        }

        fake_doc_mgr = MagicMock()
        fake_doc_mgr.upload_document = AsyncMock(return_value=uploaded_record)
        fake_doc_mgr.index_document = AsyncMock(return_value=indexed_record)

        fake_file = MagicMock()
        fake_file.read = AsyncMock(return_value=b"content")
        fake_file.filename = "auto.txt"

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_doc_mgr",
                return_value=fake_doc_mgr,
            ),
        ):
            result = await upload_document(
                name="test_kb",
                file=fake_file,
                auto_index=True,
                user=_fake_user("admin"),
            )

        # upload と index の両方が呼ばれた
        fake_doc_mgr.upload_document.assert_awaited_once()
        fake_doc_mgr.index_document.assert_awaited_once_with("doc-auto")

        # auto_index 後は indexed レコードが返る
        assert result["document"]["status"] == "indexed"

    @pytest.mark.asyncio
    async def test_upload_forwards_group_tags_and_scenario_metadata(self) -> None:
        """アップロード API が group/tags/scenario を正規化して転送する。"""
        uploaded_record = MagicMock()
        uploaded_record.document_id = "doc-meta"
        uploaded_record.to_dict.return_value = {
            "document_id": "doc-meta",
            "status": "uploaded",
            "document_group_id": "group-hr-travel",
            "tags": ["policy", "travel"],
        }

        fake_doc_mgr = MagicMock()
        fake_doc_mgr.upload_document = AsyncMock(return_value=uploaded_record)

        fake_file = MagicMock()
        fake_file.read = AsyncMock(return_value=b"content")
        fake_file.filename = "policy.txt"

        with (
            patch(
                "apps.faq_system.routers.collections.is_rag_enabled",
                return_value=True,
            ),
            patch(
                "apps.faq_system.routers.collections._get_doc_mgr",
                return_value=fake_doc_mgr,
            ),
        ):
            await upload_document(
                name="test_kb",
                file=fake_file,
                auto_index=False,
                document_group_id="group-hr-travel",
                tags="policy, travel",
                scenario_id="hr-travel",
                user=_fake_user("admin"),
            )

        fake_doc_mgr.upload_document.assert_awaited_once()
        upload_kwargs = fake_doc_mgr.upload_document.call_args.kwargs
        assert upload_kwargs["metadata"] == {
            "document_group_id": "group-hr-travel",
            "tags": ["policy", "travel"],
            "scenario_id": "hr-travel",
        }
