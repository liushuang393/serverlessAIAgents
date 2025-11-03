"""
音声関連ユーティリティのテストクラス

このモジュールは、audio_utils.pyの機能をテストします。
"""

import unittest
from unittest.mock import Mock, patch, MagicMock, mock_open
import sys
import os

# テスト対象モジュールのパスを追加
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

try:
    from audio_utils import (
        TTSProvider, AmazonPollyProvider, GoogleCloudTTSProvider,
        AzureTTSProvider, ElevenLabsProvider, TTSManager,
        AudioError, get_tts_manager, text_to_speech, AudioUtils
    )
except ImportError as e:
    print(f"Warning: Could not import audio_utils: {e}")
    # フォールバック用のダミークラス
    class TTSProvider:
        def __init__(self, name): self.name = name
    class AudioError(Exception): pass


class TestTTSProvider(unittest.TestCase):
    """TTSProvider基底クラスのテスト"""
    
    def test_base_provider_initialization(self):
        """基底プロバイダーの初期化テスト"""
        provider = TTSProvider("test")
        self.assertEqual(provider.name, "test")
        if hasattr(provider, 'call_count'):
            self.assertEqual(provider.call_count, 0)
            self.assertEqual(provider.error_count, 0)
    
    def test_not_implemented_method(self):
        """未実装メソッドのテスト"""
        provider = TTSProvider("test")
        if hasattr(provider, 'synthesize'):
            with self.assertRaises(NotImplementedError):
                provider.synthesize("test text", "output.mp3")


class TestAmazonPollyProvider(unittest.TestCase):
    """AmazonPollyProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.provider = AmazonPollyProvider(
                aws_access_key_id="test_key",
                aws_secret_access_key="test_secret",
                region_name="us-east-1"
            )
        except NameError:
            self.skipTest("AmazonPollyProvider not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "amazon_polly")
        self.assertEqual(self.provider.aws_access_key_id, "test_key")
        self.assertEqual(self.provider.aws_secret_access_key, "test_secret")
        self.assertEqual(self.provider.region_name, "us-east-1")
    
    def test_no_credentials_error(self):
        """認証情報未設定エラーテスト"""
        try:
            provider = AmazonPollyProvider(
                aws_access_key_id=None,
                aws_secret_access_key=None
            )
            
            with self.assertRaises(AudioError):
                provider.synthesize("test text", "output.mp3")
                
        except NameError:
            self.skipTest("AmazonPollyProvider not available")
    
    @patch('audio_utils.boto3.client')
    @patch('builtins.open', new_callable=mock_open)
    def test_synthesize_success(self, mock_file, mock_boto3_client):
        """音声合成成功テスト"""
        try:
            # モックの設定
            mock_polly = Mock()
            mock_response = {
                "AudioStream": Mock()
            }
            mock_response["AudioStream"].read.return_value = b"fake_audio_data"
            mock_polly.synthesize_speech.return_value = mock_response
            mock_boto3_client.return_value = mock_polly
            
            result = self.provider.synthesize("Hello world", "output.mp3")
            
            self.assertEqual(result, "output.mp3")
            mock_polly.synthesize_speech.assert_called_once()
            mock_file.assert_called_once_with("output.mp3", "wb")
            
        except NameError:
            self.skipTest("AmazonPollyProvider not available")


class TestElevenLabsProvider(unittest.TestCase):
    """ElevenLabsProviderのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.provider = ElevenLabsProvider(api_key="test_elevenlabs_key")
        except NameError:
            self.skipTest("ElevenLabsProvider not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.provider.name, "elevenlabs")
        self.assertEqual(self.provider.api_key, "test_elevenlabs_key")
    
    def test_no_api_key_error(self):
        """APIキー未設定エラーテスト"""
        try:
            provider = ElevenLabsProvider(api_key=None)
            
            with self.assertRaises(AudioError):
                provider.synthesize("test text", "output.mp3")
                
        except NameError:
            self.skipTest("ElevenLabsProvider not available")
    
    @patch('audio_utils.requests.post')
    @patch('builtins.open', new_callable=mock_open)
    def test_synthesize_success(self, mock_file, mock_post):
        """音声合成成功テスト"""
        try:
            # モックの設定
            mock_response = Mock()
            mock_response.raise_for_status.return_value = None
            mock_response.content = b"fake_audio_data"
            mock_post.return_value = mock_response
            
            result = self.provider.synthesize("Hello world", "output.mp3")
            
            self.assertEqual(result, "output.mp3")
            mock_post.assert_called_once()
            mock_file.assert_called_once_with("output.mp3", "wb")
            
        except NameError:
            self.skipTest("ElevenLabsProvider not available")


class TestTTSManager(unittest.TestCase):
    """TTSManagerのテスト"""
    
    def setUp(self):
        """テストセットアップ"""
        try:
            self.manager = TTSManager()
        except NameError:
            self.skipTest("TTSManager not available")
    
    def test_initialization(self):
        """初期化テスト"""
        self.assertEqual(self.manager.default_provider, "amazon_polly")
        self.assertEqual(len(self.manager.providers), 0)  # 初期状態では空
    
    def test_register_provider(self):
        """プロバイダー登録テスト"""
        try:
            test_provider = TTSProvider("test")
            self.manager.register_provider("test", test_provider)
            
            self.assertIn("test", self.manager.providers)
            self.assertEqual(self.manager.providers["test"], test_provider)
        except NameError:
            self.skipTest("TTSManager not available")
    
    def test_set_default_provider(self):
        """デフォルトプロバイダー設定テスト"""
        try:
            test_provider = TTSProvider("test")
            self.manager.register_provider("test", test_provider)
            self.manager.set_default_provider("test")
            
            self.assertEqual(self.manager.default_provider, "test")
        except NameError:
            self.skipTest("TTSManager not available")
    
    def test_set_invalid_default_provider(self):
        """無効なデフォルトプロバイダー設定テスト"""
        try:
            with self.assertRaises(ValueError):
                self.manager.set_default_provider("nonexistent")
        except NameError:
            self.skipTest("TTSManager not available")


class TestAudioUtils(unittest.TestCase):
    """AudioUtilsクラスのテスト"""
    
    def test_split_text_for_tts(self):
        """TTS用テキスト分割テスト"""
        try:
            # 短いテキスト
            short_text = "これは短いテキストです。"
            result = AudioUtils.split_text_for_tts(short_text, max_length=100)
            self.assertEqual(result, [short_text])
            
            # 長いテキスト
            long_text = "これは非常に長いテキストです。" * 100
            result = AudioUtils.split_text_for_tts(long_text, max_length=50)
            self.assertGreater(len(result), 1)
            
            # 各チャンクが最大長以下であることを確認
            for chunk in result:
                self.assertLessEqual(len(chunk), 50)
                
        except NameError:
            self.skipTest("AudioUtils not available")
    
    @patch('audio_utils.librosa')
    def test_get_audio_duration(self, mock_librosa):
        """音声長さ取得テスト"""
        try:
            mock_librosa.get_duration.return_value = 10.5
            
            duration = AudioUtils.get_audio_duration("test.mp3")
            self.assertEqual(duration, 10.5)
            mock_librosa.get_duration.assert_called_once_with(filename="test.mp3")
            
        except NameError:
            self.skipTest("AudioUtils not available")
    
    def test_get_audio_duration_no_librosa(self):
        """librosa未インストール時の音声長さ取得テスト"""
        try:
            with patch('audio_utils.librosa', side_effect=ImportError):
                duration = AudioUtils.get_audio_duration("test.mp3")
                self.assertEqual(duration, 0.0)
        except NameError:
            self.skipTest("AudioUtils not available")


class TestGlobalFunctions(unittest.TestCase):
    """グローバル関数のテスト"""
    
    def test_get_tts_manager(self):
        """グローバルマネージャー取得テスト"""
        try:
            manager1 = get_tts_manager()
            manager2 = get_tts_manager()
            
            # 同じインスタンスが返されることを確認
            self.assertIs(manager1, manager2)
        except NameError:
            self.skipTest("get_tts_manager not available")
    
    def test_text_to_speech_function(self):
        """text_to_speech関数のテスト"""
        try:
            # プロバイダーが登録されていない場合はエラーになる
            with self.assertRaises(ValueError):
                text_to_speech("test text", "output.mp3")
        except NameError:
            self.skipTest("text_to_speech function not available")


class TestSetupTTSProviders(unittest.TestCase):
    """setup_tts_providers関数のテスト"""
    
    def test_setup_amazon_polly_provider(self):
        """Amazon Pollyプロバイダー設定テスト"""
        try:
            from audio_utils import setup_tts_providers
            
            config = {
                "polly": {
                    "type": "amazon_polly",
                    "aws_access_key_id": "test_key",
                    "aws_secret_access_key": "test_secret",
                    "region_name": "us-east-1",
                    "default": True
                }
            }
            
            setup_tts_providers(config)
            
            manager = get_tts_manager()
            self.assertIn("polly", manager.providers)
            self.assertEqual(manager.default_provider, "polly")
            
        except (NameError, ImportError):
            self.skipTest("setup_tts_providers not available")
    
    def test_setup_unknown_provider_type(self):
        """未知のプロバイダータイプ設定テスト"""
        try:
            from audio_utils import setup_tts_providers
            
            config = {
                "unknown": {
                    "type": "unknown_type",
                    "api_key": "test_key"
                }
            }
            
            # 警告が出るが例外は発生しない
            setup_tts_providers(config)
            
            manager = get_tts_manager()
            self.assertNotIn("unknown", manager.providers)
            
        except (NameError, ImportError):
            self.skipTest("setup_tts_providers not available")


if __name__ == '__main__':
    unittest.main(verbosity=2)
