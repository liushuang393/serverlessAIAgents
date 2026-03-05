"""setup_qdrant.sh の内容・実行可能性を検証."""

from pathlib import Path


def test_setup_qdrant_script_exists() -> None:
    script = Path("scripts/setup_qdrant.sh")
    assert script.exists(), "scripts/setup_qdrant.sh が存在しない"
    assert script.stat().st_mode & 0o111, "スクリプトに実行権限がない"


def test_setup_qdrant_script_contains_docker_compose() -> None:
    script = Path("scripts/setup_qdrant.sh")
    content = script.read_text()
    assert "docker" in content.lower()
    assert "qdrant" in content.lower()
    assert "6333" in content  # Qdrant デフォルトポート


def test_faq_setup_rag_script_exists() -> None:
    script = Path("apps/faq_system/scripts/setup_rag.sh")
    assert script.exists()
