import pytest
import io
import os
from unittest.mock import MagicMock, patch
from apps.faq_system.backend.rag.parsers import FileParser

class TestEnterpriseFeatures:
    """Enterprise 拡張機能（P2）の検証テスト."""

    def test_pdf_parser_smoke(self):
        """PDF パーサーのモックテスト."""
        mock_pdf_content = b"%PDF-1.4\n1 0 obj\n<< /Length 10 >>\nstream\nHello PDF\nendstream\nendobj\ntrailer\n<< /Root 1 0 R >>\n%%EOF"
        stream = io.BytesIO(mock_pdf_content)
        
        # pypdf が入っていない環境でも動作するように ImportError をキャッチするかパッチする
        with patch("apps.faq_system.backend.rag.parsers.PdfReader") as mock_reader:
            if mock_reader:
                mock_page = MagicMock()
                mock_page.extract_text.return_value = "Extracted PDF Text"
                mock_reader.return_value.pages = [mock_page]
                
                text = FileParser.parse_pdf(stream)
                assert text == "Extracted PDF Text"

    def test_docx_parser_smoke(self):
        """Word パーサーのモックテスト."""
        stream = io.BytesIO(b"dummy docx")
        with patch("apps.faq_system.backend.rag.parsers.Document") as mock_doc:
            if mock_doc:
                mock_para = MagicMock()
                mock_para.text = "Extracted Docx Text"
                mock_doc.return_value.paragraphs = [mock_para]
                
                text = FileParser.parse_docx(stream)
                assert text == "Extracted Docx Text"

    def test_csv_parser(self):
        """CSV パーサーの検証."""
        csv_data = "header1,header2\nval1,val2\nval3,val4"
        stream = io.StringIO(csv_data)
        text = FileParser.parse_csv(stream)
        
        assert "header1: val1" in text
        assert "header2: val2" in text
        assert "---" in text
        assert "header1: val3" in text

    @patch("ldap3.Connection")
    @patch("ldap3.Server")
    def test_ldap_auth_logic(self, mock_server, mock_conn):
        """LDAP 認証ロジックの検証."""
        from apps.faq_system.backend.auth.service import AuthService
        
        # モック設定
        mock_conn.return_value.bind.return_value = True
        mock_entry = MagicMock()
        mock_entry.displayName = "LDAP User"
        mock_entry.mail = "ldap@example.com"
        mock_entry.memberOf = ["cn=admins,dc=example,dc=com"]
        mock_conn.return_value.entries = [mock_entry]
        
        os.environ["FAQ_LDAP_BASE_DN"] = "dc=example,dc=com"
        os.environ["FAQ_LDAP_ROLE_MAPPING"] = '{"cn=admins,dc=example,dc=com": "admin"}'
        
        service = AuthService()
        # 私有メソッドを直接テスト（または authenticate 経由）
        result = service._authenticate_ldap_sync(
            "ldap://localhost",
            "uid={username},dc=example,dc=com",
            "testuser",
            "password"
        )
        
        assert result is not None
        assert result.display_name == "LDAP User"
        assert result.role == "admin"
        assert result.email == "ldap@example.com"

    @patch("apps.faq_system.backend.auth.service.get_db_session")
    async def test_account_lockout_logic(self, mock_get_session):
        """アカウントロックアウトロジックの検証."""
        from apps.faq_system.backend.auth.service import AuthService
        from apps.faq_system.backend.db.models import UserAccount
        from unittest.mock import AsyncMock
        
        # モック設定
        mock_session = AsyncMock()
        mock_get_session.return_value.__aenter__.return_value = mock_session
        
        account = UserAccount(
            username="locked_user",
            password_hash="...hashed...",
            password_salt="salt",
            is_active=True,
            auth_source="local_db",
            login_attempts=0
        )
        mock_session.scalar.return_value = account
        
        service = AuthService()
        with patch.object(service, "_verify_password", return_value=False):
            # 5回連続で失敗させる
            for i in range(5):
                success, msg, _ = await service._authenticate_local_db_with_mfa("locked_user", "wrong", None)
                assert success is False
            
            assert account.login_attempts == 5
            assert account.locked_until is not None
            assert "ロックしました" in msg
            
            # ロック中
            success, msg, _ = await service._authenticate_local_db_with_mfa("locked_user", "wrong", None)
            assert success is False
            assert "ロックされています" in msg
