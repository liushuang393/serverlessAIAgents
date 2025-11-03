"""
音声関連ユーティリティ

このモジュールは、Text-to-Speech（Amazon Polly、Google Cloud TTS、Azure TTS、
IBM Watson、ElevenLabs）への統一されたインターフェースを提供します。
"""

import os
from typing import Optional, Dict, Any, Union, List
import logging

logger = logging.getLogger(__name__)


class AudioError(Exception):
    """音声関連のエラー"""
    pass


class TTSProvider:
    """Text-to-Speechプロバイダーの基底クラス"""
    
    def __init__(self, name: str):
        self.name = name
        self.call_count = 0
        self.error_count = 0
    
    def synthesize(
        self,
        text: str,
        output_path: str,
        voice: Optional[str] = None,
        **kwargs
    ) -> str:
        """テキストを音声に変換（サブクラスで実装）"""
        raise NotImplementedError
    
    def _record_call(self, success: bool = True):
        """呼び出し統計を記録"""
        self.call_count += 1
        if not success:
            self.error_count += 1


class AmazonPollyProvider(TTSProvider):
    """Amazon Polly TTSプロバイダー"""
    
    def __init__(
        self,
        aws_access_key_id: Optional[str] = None,
        aws_secret_access_key: Optional[str] = None,
        region_name: str = "us-east-1"
    ):
        super().__init__("amazon_polly")
        self.aws_access_key_id = aws_access_key_id or os.getenv("AWS_ACCESS_KEY_ID")
        self.aws_secret_access_key = aws_secret_access_key or os.getenv("AWS_SECRET_ACCESS_KEY")
        self.region_name = region_name
        
        if not self.aws_access_key_id or not self.aws_secret_access_key:
            logger.warning("AWS認証情報が設定されていません")
    
    def synthesize(
        self,
        text: str,
        output_path: str,
        voice: Optional[str] = None,
        **kwargs
    ) -> str:
        """Amazon Pollyでテキストを音声に変換"""
        try:
            if not self.aws_access_key_id or not self.aws_secret_access_key:
                raise AudioError("AWS認証情報が設定されていません")
            
            import boto3
            
            polly = boto3.client(
                "polly",
                region_name=self.region_name,
                aws_access_key_id=self.aws_access_key_id,
                aws_secret_access_key=self.aws_secret_access_key
            )
            
            response = polly.synthesize_speech(
                Text=text,
                OutputFormat=kwargs.get("output_format", "mp3"),
                VoiceId=voice or kwargs.get("voice_id", "Joanna"),
                Engine=kwargs.get("engine", "standard")
            )
            
            with open(output_path, "wb") as f:
                f.write(response["AudioStream"].read())
            
            logger.info(f"Amazon Pollyで音声を生成しました: {output_path}")
            self._record_call(True)
            return output_path
            
        except Exception as e:
            logger.error(f"Amazon Polly音声生成エラー: {e}")
            self._record_call(False)
            raise AudioError(f"Amazon Polly音声生成に失敗しました: {e}")


class GoogleCloudTTSProvider(TTSProvider):
    """Google Cloud TTS プロバイダー"""
    
    def __init__(self, credentials_path: Optional[str] = None):
        super().__init__("google_cloud_tts")
        self.credentials_path = credentials_path or os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
        
        if not self.credentials_path:
            logger.warning("Google Cloud認証情報が設定されていません")
    
    def synthesize(
        self,
        text: str,
        output_path: str,
        voice: Optional[str] = None,
        **kwargs
    ) -> str:
        """Google Cloud TTSでテキストを音声に変換"""
        try:
            if not self.credentials_path:
                raise AudioError("Google Cloud認証情報が設定されていません")
            
            from google.cloud import texttospeech
            
            client = texttospeech.TextToSpeechClient()
            
            input_text = texttospeech.SynthesisInput(text=text)
            
            voice_params = texttospeech.VoiceSelectionParams(
                language_code=kwargs.get("language_code", "en-US"),
                name=voice,
                ssml_gender=getattr(
                    texttospeech.SsmlVoiceGender,
                    kwargs.get("gender", "NEUTRAL")
                )
            )
            
            audio_config = texttospeech.AudioConfig(
                audio_encoding=getattr(
                    texttospeech.AudioEncoding,
                    kwargs.get("audio_encoding", "MP3")
                )
            )
            
            response = client.synthesize_speech(
                input=input_text,
                voice=voice_params,
                audio_config=audio_config
            )
            
            with open(output_path, "wb") as f:
                f.write(response.audio_content)
            
            logger.info(f"Google Cloud TTSで音声を生成しました: {output_path}")
            self._record_call(True)
            return output_path
            
        except Exception as e:
            logger.error(f"Google Cloud TTS音声生成エラー: {e}")
            self._record_call(False)
            raise AudioError(f"Google Cloud TTS音声生成に失敗しました: {e}")


class AzureTTSProvider(TTSProvider):
    """Azure TTS プロバイダー"""
    
    def __init__(
        self,
        subscription_key: Optional[str] = None,
        region: Optional[str] = None
    ):
        super().__init__("azure_tts")
        self.subscription_key = subscription_key or os.getenv("AZURE_SPEECH_KEY")
        self.region = region or os.getenv("AZURE_SPEECH_REGION")
        
        if not self.subscription_key or not self.region:
            logger.warning("Azure Speech認証情報が設定されていません")
    
    def synthesize(
        self,
        text: str,
        output_path: str,
        voice: Optional[str] = None,
        **kwargs
    ) -> str:
        """Azure TTSでテキストを音声に変換"""
        try:
            if not self.subscription_key or not self.region:
                raise AudioError("Azure Speech認証情報が設定されていません")
            
            import azure.cognitiveservices.speech as speechsdk
            
            speech_config = speechsdk.SpeechConfig(
                subscription=self.subscription_key,
                region=self.region
            )
            
            if voice:
                speech_config.speech_synthesis_voice_name = voice
            
            audio_config = speechsdk.audio.AudioConfig(filename=output_path)
            
            synthesizer = speechsdk.SpeechSynthesizer(
                speech_config=speech_config,
                audio_config=audio_config
            )
            
            result = synthesizer.speak_text_async(text).get()
            
            if result.reason == speechsdk.ResultReason.SynthesizingAudioCompleted:
                logger.info(f"Azure TTSで音声を生成しました: {output_path}")
                self._record_call(True)
                return output_path
            else:
                raise AudioError(f"Azure TTS合成失敗: {result.reason}")
            
        except Exception as e:
            logger.error(f"Azure TTS音声生成エラー: {e}")
            self._record_call(False)
            raise AudioError(f"Azure TTS音声生成に失敗しました: {e}")


class IBMWatsonTTSProvider(TTSProvider):
    """IBM Watson TTS プロバイダー"""
    
    def __init__(
        self,
        api_key: Optional[str] = None,
        service_url: Optional[str] = None
    ):
        super().__init__("ibm_watson_tts")
        self.api_key = api_key or os.getenv("IBM_WATSON_TTS_API_KEY")
        self.service_url = service_url or os.getenv("IBM_WATSON_TTS_URL")
        
        if not self.api_key or not self.service_url:
            logger.warning("IBM Watson TTS認証情報が設定されていません")
    
    def synthesize(
        self,
        text: str,
        output_path: str,
        voice: Optional[str] = None,
        **kwargs
    ) -> str:
        """IBM Watson TTSでテキストを音声に変換"""
        try:
            if not self.api_key or not self.service_url:
                raise AudioError("IBM Watson TTS認証情報が設定されていません")
            
            from ibm_watson import TextToSpeechV1
            from ibm_cloud_sdk_core.authenticators import IAMAuthenticator
            
            authenticator = IAMAuthenticator(self.api_key)
            service = TextToSpeechV1(authenticator=authenticator)
            service.set_service_url(self.service_url)
            
            response = service.synthesize(
                text,
                voice=voice or kwargs.get("voice", "en-US_AllisonV3Voice"),
                accept=kwargs.get("accept", "audio/mp3")
            ).get_result()
            
            with open(output_path, "wb") as f:
                f.write(response.content)
            
            logger.info(f"IBM Watson TTSで音声を生成しました: {output_path}")
            self._record_call(True)
            return output_path
            
        except Exception as e:
            logger.error(f"IBM Watson TTS音声生成エラー: {e}")
            self._record_call(False)
            raise AudioError(f"IBM Watson TTS音声生成に失敗しました: {e}")


class ElevenLabsProvider(TTSProvider):
    """ElevenLabs TTS プロバイダー"""
    
    def __init__(self, api_key: Optional[str] = None):
        super().__init__("elevenlabs")
        self.api_key = api_key or os.getenv("ELEVENLABS_API_KEY")
        
        if not self.api_key:
            logger.warning("ElevenLabs API キーが設定されていません")
    
    def synthesize(
        self,
        text: str,
        output_path: str,
        voice: Optional[str] = None,
        **kwargs
    ) -> str:
        """ElevenLabsでテキストを音声に変換"""
        try:
            if not self.api_key:
                raise AudioError("ElevenLabs API キーが設定されていません")
            
            import requests
            
            voice_id = voice or kwargs.get("voice_id", "21m00Tcm4TlvDq8ikWAM")  # デフォルト音声
            url = f"https://api.elevenlabs.io/v1/text-to-speech/{voice_id}"
            
            headers = {
                "xi-api-key": self.api_key,
                "Content-Type": "application/json"
            }
            
            data = {
                "text": text,
                "voice_settings": {
                    "stability": kwargs.get("stability", 0.75),
                    "similarity_boost": kwargs.get("similarity_boost", 0.75)
                }
            }
            
            response = requests.post(url, headers=headers, json=data, timeout=60)
            response.raise_for_status()
            
            with open(output_path, "wb") as f:
                f.write(response.content)
            
            logger.info(f"ElevenLabsで音声を生成しました: {output_path}")
            self._record_call(True)
            return output_path
            
        except Exception as e:
            logger.error(f"ElevenLabs音声生成エラー: {e}")
            self._record_call(False)
            raise AudioError(f"ElevenLabs音声生成に失敗しました: {e}")


class TTSManager:
    """TTS プロバイダー管理クラス"""
    
    def __init__(self):
        self.providers: Dict[str, TTSProvider] = {}
        self.default_provider = "amazon_polly"
    
    def register_provider(self, name: str, provider: TTSProvider) -> None:
        """プロバイダーを登録"""
        self.providers[name] = provider
        logger.info(f"TTSプロバイダーを登録しました: {name}")
    
    def set_default_provider(self, name: str) -> None:
        """デフォルトプロバイダーを設定"""
        if name not in self.providers:
            raise ValueError(f"プロバイダー '{name}' が登録されていません")
        self.default_provider = name
        logger.info(f"デフォルトTTSプロバイダーを設定しました: {name}")
    
    def synthesize(
        self,
        text: str,
        output_path: str,
        provider: Optional[str] = None,
        voice: Optional[str] = None,
        **kwargs
    ) -> str:
        """テキストを音声に変換"""
        provider_name = provider or self.default_provider
        
        if provider_name not in self.providers:
            raise ValueError(f"プロバイダー '{provider_name}' が登録されていません")
        
        return self.providers[provider_name].synthesize(text, output_path, voice, **kwargs)
    
    def get_stats(self) -> Dict[str, Dict[str, Union[int, float]]]:
        """統計情報を取得"""
        stats = {}
        for name, provider in self.providers.items():
            stats[name] = {
                "call_count": provider.call_count,
                "error_count": provider.error_count,
                "success_rate": (
                    (provider.call_count - provider.error_count) / provider.call_count
                    if provider.call_count > 0 else 0.0
                )
            }
        return stats


# グローバルマネージャー
_tts_manager: Optional[TTSManager] = None


def get_tts_manager() -> TTSManager:
    """グローバルTTSマネージャーを取得"""
    global _tts_manager
    if _tts_manager is None:
        _tts_manager = TTSManager()
    return _tts_manager


# 便利関数
def text_to_speech(
    text: str,
    output_path: str,
    provider: Optional[str] = None,
    voice: Optional[str] = None,
    **kwargs
) -> str:
    """テキスト音声変換の便利関数"""
    manager = get_tts_manager()
    return manager.synthesize(text, output_path, provider, voice, **kwargs)


def setup_tts_providers(config: Dict[str, Dict[str, Any]]) -> None:
    """設定からTTSプロバイダーを設定"""
    manager = get_tts_manager()
    
    for name, provider_config in config.items():
        provider_type = provider_config.get("type", "amazon_polly")
        
        provider: TTSProvider
        if provider_type == "amazon_polly":
            provider = AmazonPollyProvider(
                aws_access_key_id=provider_config.get("aws_access_key_id"),
                aws_secret_access_key=provider_config.get("aws_secret_access_key"),
                region_name=provider_config.get("region_name", "us-east-1")
            )
        elif provider_type == "google_cloud_tts":
            provider = GoogleCloudTTSProvider(
                credentials_path=provider_config.get("credentials_path")
            )
        elif provider_type == "azure_tts":
            provider = AzureTTSProvider(
                subscription_key=provider_config.get("subscription_key"),
                region=provider_config.get("region")
            )
        elif provider_type == "ibm_watson_tts":
            provider = IBMWatsonTTSProvider(
                api_key=provider_config.get("api_key"),
                service_url=provider_config.get("service_url")
            )
        elif provider_type == "elevenlabs":
            provider = ElevenLabsProvider(
                api_key=provider_config.get("api_key")
            )
        else:
            logger.warning(f"未知のTTSプロバイダータイプ: {provider_type}")
            continue
        
        manager.register_provider(name, provider)
        
        if provider_config.get("default", False):
            manager.set_default_provider(name)


class AudioUtils:
    """音声ユーティリティクラス"""
    
    @staticmethod
    def get_audio_duration(file_path: str) -> float:
        """音声ファイルの長さを取得（秒）"""
        try:
            import librosa
            duration = librosa.get_duration(filename=file_path)
            return float(duration)
        except ImportError:
            logger.warning("librosaがインストールされていません。音声長さの取得をスキップします")
            return 0.0
        except Exception as e:
            logger.error(f"音声長さ取得エラー: {e}")
            return 0.0
    
    @staticmethod
    def convert_audio_format(
        input_path: str,
        output_path: str,
        output_format: str = "mp3"
    ) -> str:
        """音声ファイルの形式を変換"""
        try:
            from pydub import AudioSegment
            
            audio = AudioSegment.from_file(input_path)
            audio.export(output_path, format=output_format)
            
            logger.info(f"音声形式を変換しました: {input_path} -> {output_path}")
            return output_path
            
        except ImportError:
            logger.warning("pydubがインストールされていません。音声形式変換をスキップします")
            return input_path
        except Exception as e:
            logger.error(f"音声形式変換エラー: {e}")
            raise AudioError(f"音声形式変換に失敗しました: {e}")
    
    @staticmethod
    def split_text_for_tts(text: str, max_length: int = 5000) -> List[str]:
        """TTSの文字数制限に合わせてテキストを分割"""
        if len(text) <= max_length:
            return [text]

        # 文で分割を試行（中国語と日本語の句読点も考慮）
        import re
        sentences = re.split(r'[.。！？!?]+', text)
        sentences = [s.strip() for s in sentences if s.strip()]

        chunks = []
        current_chunk = ""

        for sentence in sentences:
            # 文を追加した場合の長さをチェック
            test_chunk = current_chunk + sentence + "。" if current_chunk else sentence + "。"

            if len(test_chunk) <= max_length:
                current_chunk = test_chunk
            else:
                # 現在のチャンクを保存
                if current_chunk:
                    chunks.append(current_chunk.strip())

                # 単一の文が最大長を超える場合は強制分割
                if len(sentence) > max_length:
                    # 文字単位で分割
                    for i in range(0, len(sentence), max_length):
                        chunk_part = sentence[i:i + max_length]
                        chunks.append(chunk_part)
                    current_chunk = ""
                else:
                    current_chunk = sentence + "。"

        # 最後のチャンクを追加
        if current_chunk:
            chunks.append(current_chunk.strip())

        return chunks if chunks else [text]
    
    @staticmethod
    def merge_audio_files(input_paths: List[str], output_path: str) -> str:
        """複数の音声ファイルを結合"""
        try:
            from pydub import AudioSegment
            
            combined = AudioSegment.empty()
            
            for path in input_paths:
                audio = AudioSegment.from_file(path)
                combined += audio
            
            combined.export(output_path, format="mp3")
            
            logger.info(f"音声ファイルを結合しました: {output_path}")
            return output_path
            
        except ImportError:
            logger.warning("pydubがインストールされていません。音声結合をスキップします")
            return input_paths[0] if input_paths else ""
        except Exception as e:
            logger.error(f"音声結合エラー: {e}")
            raise AudioError(f"音声結合に失敗しました: {e}")


# 便利関数
def synthesize_long_text(
    text: str,
    output_dir: str,
    provider: Optional[str] = None,
    voice: Optional[str] = None,
    max_length: int = 5000,
    **kwargs
) -> List[str]:
    """長いテキストを分割してTTS実行"""
    import os
    
    chunks = AudioUtils.split_text_for_tts(text, max_length)
    output_files = []
    
    for i, chunk in enumerate(chunks):
        output_path = os.path.join(output_dir, f"chunk_{i:03d}.mp3")
        text_to_speech(chunk, output_path, provider, voice, **kwargs)
        output_files.append(output_path)
    
    return output_files
