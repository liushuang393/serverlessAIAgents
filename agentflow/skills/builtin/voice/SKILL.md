---
name: voice
description: 音声認識・合成機能。OpenAI Whisper / TTS を使用した音声処理スキル。
version: 1.0.0
triggers:
  - 音声認識
  - speech to text
  - transcribe
  - TTS
  - text to speech
  - 音声合成
  - voice
tags:
  - voice
  - speech
  - audio
  - tts
  - stt
requirements:
  - httpx
examples:
  - "この音声をテキストに変換して"
  - "テキストを音声で読み上げて"
  - "音声ファイルを文字起こしして"
allowed-tools:
  - Bash
  - Read
  - Write
user-invocable: true
---

# Voice Skill

音声認識・合成機能を提供するスキル。

## 機能

- **音声認識 (STT)**: 音声をテキストに変換
- **音声合成 (TTS)**: テキストを音声に変換
- **多言語対応**: 複数言語の音声認識・合成

## 使用例

```python
from agentflow.skills.builtin.voice import VoiceSkill

voice = VoiceSkill()

# 音声認識
text = await voice.transcribe(audio_path="recording.mp3")

# 音声合成
audio_data = await voice.synthesize("こんにちは、世界！")
with open("output.mp3", "wb") as f:
    f.write(audio_data)
```

## 対応プロバイダー

- OpenAI (Whisper + TTS)
- Google Cloud Speech
- Azure Cognitive Services

