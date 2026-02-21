"""Voice Skill - 音声認識・合成機能.

OpenAI Whisper / TTS を使用した音声処理スキル。

Example:
    >>> from agentflow.skills.builtin.voice import VoiceSkill
    >>> voice = VoiceSkill()
    >>> text = await voice.transcribe(audio_path="recording.mp3")
"""

from agentflow.skills.builtin.voice.voice import (
    TTSVoice,
    VoiceConfig,
    VoiceProvider,
    VoiceSkill,
)


__all__ = [
    "TTSVoice",
    "VoiceConfig",
    "VoiceProvider",
    "VoiceSkill",
]
