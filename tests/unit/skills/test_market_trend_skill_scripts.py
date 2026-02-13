# -*- coding: utf-8 -*-
"""Market Trend Analysis Skill - 確定性処理スクリプトテスト.

scripts/ 内の確定性処理が正しく動作することを検証。
"""

import importlib.util
import sys
from pathlib import Path

import pytest


# scripts/ パスを動的にインポート
SCRIPTS_DIR = Path(__file__).parent.parent.parent.parent / "agentflow/skills/builtin/market-trend-analysis/scripts"


def _load_script(name: str):
    """スクリプトを動的にロード."""
    script_path = SCRIPTS_DIR / f"{name}.py"
    if not script_path.exists():
        pytest.skip(f"Script not found: {script_path}")
    spec = importlib.util.spec_from_file_location(name, script_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


class TestValidateInput:
    """入力検証スクリプトテスト."""

    def test_valid_input(self) -> None:
        """正常入力テスト."""
        module = _load_script("validate_input")
        
        input_data = {
            "articles": [
                {"id": "1", "title": "Test Article", "content": "Content here"},
            ],
        }
        result = module.validate_articles_input(input_data)
        
        assert result.valid is True
        assert len(result.errors) == 0
        assert result.normalized_data is not None
        assert len(result.normalized_data["articles"]) == 1

    def test_missing_articles(self) -> None:
        """articles欠落テスト."""
        module = _load_script("validate_input")
        
        result = module.validate_articles_input({})
        
        assert result.valid is False
        assert "Missing required field: 'articles'" in result.errors

    def test_empty_articles(self) -> None:
        """空articles配列テスト（警告のみ）."""
        module = _load_script("validate_input")
        
        result = module.validate_articles_input({"articles": []})
        
        assert result.valid is True
        assert len(result.warnings) > 0
        assert "Empty articles list" in result.warnings[0]

    def test_invalid_article_type(self) -> None:
        """不正記事型テスト."""
        module = _load_script("validate_input")
        
        result = module.validate_articles_input({"articles": ["not a dict"]})
        
        assert result.valid is False
        assert any("must be an object" in e for e in result.errors)

    def test_missing_required_fields(self) -> None:
        """必須フィールド欠落テスト."""
        module = _load_script("validate_input")
        
        result = module.validate_articles_input({
            "articles": [{"content": "only content"}]
        })
        
        assert result.valid is False
        assert any("missing required field 'id'" in e for e in result.errors)

    def test_normalize_options(self) -> None:
        """オプション正規化テスト."""
        module = _load_script("validate_input")
        
        input_data = {
            "articles": [{"id": "1", "title": "Test"}],
            "analysis_options": {
                "enable_sentiment": False,
                "min_keyword_frequency": "5",  # 文字列も変換
            },
        }
        result = module.validate_articles_input(input_data)
        
        assert result.valid is True
        opts = result.normalized_data["analysis_options"]
        assert opts["enable_sentiment"] is False
        assert opts["min_keyword_frequency"] == 5


class TestExtractKeywords:
    """キーワード抽出スクリプトテスト."""

    def test_extract_basic(self) -> None:
        """基本抽出テスト."""
        module = _load_script("extract_keywords")
        
        articles = [
            {"title": "COBOL to Java Migration", "content": "Legacy COBOL systems"},
            {"title": "Java Modernization", "content": "Modern Java frameworks"},
        ]
        result = module.extract_keywords_from_articles(articles, min_frequency=1)
        
        assert result.total_words > 0
        assert "java" in [k.lower() for k in result.top_keywords]

    def test_tech_keywords_boost(self) -> None:
        """技術キーワード重み付けテスト."""
        module = _load_script("extract_keywords")
        
        articles = [
            {"title": "COBOL Migration", "content": "COBOL to modern systems"},
            {"title": "General News", "content": "Some general content here"},
        ]
        result = module.extract_keywords_from_articles(articles, min_frequency=1)
        
        # COBOLは技術キーワードとして重み付けされる
        assert "cobol" in [k.lower() for k in result.keywords.keys()]

    def test_existing_keywords_boost(self) -> None:
        """既存キーワード重み付けテスト."""
        module = _load_script("extract_keywords")
        
        articles = [
            {"title": "Test", "content": "Test", "keywords": ["AI", "ML"]},
        ]
        result = module.extract_keywords_from_articles(articles, min_frequency=1)
        
        # 既存キーワードは重み2倍
        assert "ai" in [k.lower() for k in result.keywords.keys()]

    def test_empty_articles(self) -> None:
        """空記事リストテスト."""
        module = _load_script("extract_keywords")
        
        result = module.extract_keywords_from_articles([])
        
        assert result.total_words == 0
        assert len(result.keywords) == 0


class TestGenerateReportSkeleton:
    """レポート骨格生成スクリプトテスト."""

    def test_generate_basic(self) -> None:
        """基本生成テスト."""
        module = _load_script("generate_report_skeleton")
        
        trends = [
            {"topic": "COBOL", "score": 0.8, "sentiment": "neutral", "growth_rate": 0.1},
            {"topic": "Java", "score": 0.7, "sentiment": "positive", "growth_rate": 0.2},
        ]
        result = module.generate_report_skeleton(trends)
        
        assert result.title != ""
        assert len(result.sections) >= 4
        assert result.markdown != ""
        assert "市場動向レポート" in result.title

    def test_sections_structure(self) -> None:
        """セクション構造テスト."""
        module = _load_script("generate_report_skeleton")
        
        trends = [{"topic": "Test", "score": 0.5, "sentiment": "neutral", "growth_rate": 0}]
        result = module.generate_report_skeleton(trends)
        
        section_types = [s["type"] for s in result.sections]
        assert "summary" in section_types
        assert "trends" in section_types
        assert "sentiment" in section_types
        assert "recommendations" in section_types

    def test_markdown_contains_trends(self) -> None:
        """Markdownにトレンド含まれるテスト."""
        module = _load_script("generate_report_skeleton")
        
        trends = [
            {"topic": "UniqueTopicName", "score": 0.9, "sentiment": "positive", "growth_rate": 0.3},
        ]
        result = module.generate_report_skeleton(trends)
        
        assert "UniqueTopicName" in result.markdown
        assert "0.90" in result.markdown  # スコア

    def test_empty_trends(self) -> None:
        """空トレンドテスト."""
        module = _load_script("generate_report_skeleton")
        
        result = module.generate_report_skeleton([])
        
        assert result.title != ""
        assert result.metadata["trends_analyzed"] == 0

