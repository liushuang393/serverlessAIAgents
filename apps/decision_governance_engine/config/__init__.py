"""
決策評価パラメータ設定モジュール.

目的:
    - YAML 設定ファイルの読み込み・検証
    - プリセット切り替え機能
    - API 経由での設定取得・更新

作成日: 2026-01-18
"""
from __future__ import annotations

import os
from functools import lru_cache
from pathlib import Path
from typing import Any

import yaml
from pydantic import BaseModel, Field


# ==============================================================================
# 設定ファイルパス
# ==============================================================================
CONFIG_DIR = Path(__file__).parent
DEFAULT_CONFIG_PATH = CONFIG_DIR / "decision_weights.yaml"


# ==============================================================================
# Pydantic モデル定義
# ==============================================================================
class DimensionConfig(BaseModel):
    """評価次元設定."""

    id: str
    name_zh: str
    name_ja: str
    name_en: str
    description: str
    min_score: int = 1
    max_score: int = 5
    inverse: bool = False  # True の場合、スコアが低いほど良い


class ThresholdConfig(BaseModel):
    """決策閾値設定."""

    min_score: float
    min_confidence: float
    min_evidence_coverage: float
    hard_veto_dimensions: list[str] = Field(default_factory=list)
    hard_veto_score: int = 5


class PresetThresholds(BaseModel):
    """プリセット閾値セット."""

    go: ThresholdConfig
    pilot: ThresholdConfig
    delay: ThresholdConfig


class PresetConfig(BaseModel):
    """プリセット設定."""

    id: str
    name_zh: str
    name_ja: str
    name_en: str
    description: str
    weights: dict[str, int]  # dimension_id -> weight (合計 100)
    thresholds: PresetThresholds


class ModeSettings(BaseModel):
    """モード別タイムアウト設定."""

    timeout_seconds: int
    max_sources: int
    max_tokens: int
    parallel_agents: int


class EvidenceReliabilityConfig(BaseModel):
    """証拠信頼度スコアリング設定."""

    domain_scores: dict[str, float]
    freshness_decay: dict[str, float]


class DecisionWeightsConfig(BaseModel):
    """決策評価パラメータ設定（トップレベル）."""

    version: str
    active_preset: str
    dimensions: dict[str, DimensionConfig]
    presets: dict[str, PresetConfig]
    mode_settings: dict[str, ModeSettings]
    evidence_reliability: EvidenceReliabilityConfig

    def get_active_preset(self) -> PresetConfig:
        """現在アクティブなプリセットを取得."""
        return self.presets[self.active_preset]

    def get_weights(self) -> dict[str, int]:
        """現在アクティブなプリセットの権重を取得."""
        return self.get_active_preset().weights

    def get_thresholds(self) -> PresetThresholds:
        """現在アクティブなプリセットの閾値を取得."""
        return self.get_active_preset().thresholds


# ==============================================================================
# 設定読み込み関数
# ==============================================================================
@lru_cache(maxsize=1)
def load_config(config_path: str | None = None) -> DecisionWeightsConfig:
    """
    設定ファイルを読み込む.

    Args:
        config_path: 設定ファイルパス（省略時はデフォルト）

    Returns:
        DecisionWeightsConfig: 設定オブジェクト

    Raises:
        FileNotFoundError: 設定ファイルが見つからない場合
        ValueError: 設定ファイルの形式が不正な場合
    """
    path = Path(config_path) if config_path else DEFAULT_CONFIG_PATH

    if not path.exists():
        msg = f"設定ファイルが見つかりません: {path}"
        raise FileNotFoundError(msg)

    with open(path, encoding="utf-8") as f:
        raw_config = yaml.safe_load(f)

    return DecisionWeightsConfig(**raw_config)


def reload_config() -> DecisionWeightsConfig:
    """設定をリロード（キャッシュクリア）."""
    load_config.cache_clear()
    return load_config()


def get_config() -> DecisionWeightsConfig:
    """設定を取得（キャッシュ利用）."""
    return load_config()

