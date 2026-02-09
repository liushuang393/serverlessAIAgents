---
name: vision
description: 画像認識・分析機能。GPT-4V / Claude Vision / Gemini Vision を使用した画像認識スキル。
version: 1.0.0
triggers:
  - 画像分析
  - image analysis
  - OCR
  - テキスト抽出
  - 画像認識
  - vision
tags:
  - vision
  - image
  - ocr
  - multimodal
requirements:
  - httpx
examples:
  - "この画像に何が写っていますか？"
  - "画像からテキストを抽出して"
  - "写真を分析して"
allowed-tools:
  - Bash
  - Read
  - Write
user-invocable: true
---

# Vision Skill

画像認識・分析機能を提供するスキル。

## 機能

- **画像分析**: 画像の内容を解析し、説明を生成
- **OCR**: 画像からテキストを抽出
- **オブジェクト検出**: 画像内のオブジェクトを検出

## 使用例

```python
from agentflow.skills.builtin.vision import VisionSkill

vision = VisionSkill()

# 画像分析
result = await vision.analyze_image(
    image_url="https://example.com/image.jpg",
    prompt="この画像に何が写っていますか？"
)

# OCR
text = await vision.extract_text(image_url="https://example.com/doc.png")
```

## 対応プロバイダー

- OpenAI (GPT-4V)
- Anthropic (Claude Vision)
- Google (Gemini Vision)

