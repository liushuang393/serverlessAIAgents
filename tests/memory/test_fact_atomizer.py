"""FactAtomizer のテスト.

TDDアプローチ: エントロピーゲート・原子化・時間正規化・プライバシーフィルタのテスト。
"""

from __future__ import annotations

from datetime import datetime

import pytest

from agentflow.memory.fact_atomizer import AtomicFact, FactAtomizer, IngestionResult


class TestHeuristicGate:
    """ヒューリスティックゲートのテスト."""

    def test_discard_greeting(self) -> None:
        """挨拶はdiscardされること."""
        atomizer = FactAtomizer()
        keep, score = atomizer._heuristic_gate("こんにちは！")
        assert keep is False

    def test_discard_short_filler(self) -> None:
        """短いフィラーはdiscardされること."""
        atomizer = FactAtomizer()
        keep, score = atomizer._heuristic_gate("了解です")
        assert keep is False

    def test_keep_with_date(self) -> None:
        """日付キーワードがあればkeepされること."""
        atomizer = FactAtomizer()
        keep, score = atomizer._heuristic_gate("明日14時に会議があります")
        assert keep is True

    def test_keep_with_proper_noun(self) -> None:
        """固有名詞があればkeepされること."""
        atomizer = FactAtomizer()
        keep, score = atomizer._heuristic_gate("田中さんがプロジェクトAを承認しました")
        assert keep is True

    def test_h_score_range(self) -> None:
        """h_scoreは0.0-1.0の範囲であること."""
        atomizer = FactAtomizer()
        _, score = atomizer._heuristic_gate("明日の会議でプランを決定します")
        assert 0.0 <= score <= 1.0

    def test_discard_very_short_text(self) -> None:
        """非常に短いテキスト（<3語）はdiscardされること."""
        atomizer = FactAtomizer()
        keep, _ = atomizer._heuristic_gate("はい")
        assert keep is False


class TestTimeNormalization:
    """時間正規化のテスト."""

    def test_ashita_to_iso(self) -> None:
        """「明日」がISO-8601に変換されること."""
        base = datetime(2026, 3, 5)
        atomizer = FactAtomizer(base_time=base)
        result = atomizer._normalize_time("明日14時に会議")
        assert "2026-03-06" in result

    def test_raishu_to_iso(self) -> None:
        """「来週月曜」がISO-8601に変換されること."""
        base = datetime(2026, 3, 5)  # 木曜日
        atomizer = FactAtomizer(base_time=base)
        result = atomizer._normalize_time("来週月曜に打ち合わせ")
        assert "2026-03-09" in result

    def test_kyou_to_iso(self) -> None:
        """「今日」がISO-8601に変換されること."""
        base = datetime(2026, 3, 5)
        atomizer = FactAtomizer(base_time=base)
        result = atomizer._normalize_time("今日の午後に提出")
        assert "2026-03-05" in result

    def test_no_time_reference_unchanged(self) -> None:
        """時間参照がなければそのまま返ること."""
        base = datetime(2026, 3, 5)
        atomizer = FactAtomizer(base_time=base)
        text = "田中さんが承認しました"
        result = atomizer._normalize_time(text)
        assert result == text

    def test_default_base_time(self) -> None:
        """base_timeが未指定でも動作すること."""
        atomizer = FactAtomizer()
        result = atomizer._normalize_time("明日の会議")
        # 例外なく実行できること
        assert isinstance(result, str)


class TestProhibitedFilter:
    """プライバシー禁止フィルタのテスト."""

    def test_credit_card_blocked(self) -> None:
        """クレジットカード番号はブロックされること."""
        atomizer = FactAtomizer()
        assert atomizer._is_prohibited("カード番号は4111111111111111です") is True

    def test_password_blocked(self) -> None:
        """パスワードはブロックされること."""
        atomizer = FactAtomizer()
        assert atomizer._is_prohibited("パスワードはsecret123です") is True

    def test_normal_text_not_blocked(self) -> None:
        """通常のテキストはブロックされないこと."""
        atomizer = FactAtomizer()
        assert atomizer._is_prohibited("田中さんは明日の会議でQ4プランを承認します") is False

    def test_custom_prohibited_patterns(self) -> None:
        """カスタム禁止パターンが機能すること."""
        atomizer = FactAtomizer(prohibited_patterns=["秘密"])
        assert atomizer._is_prohibited("秘密の計画があります") is True
        assert atomizer._is_prohibited("通常のメッセージ") is False


class TestHeuristicFallback:
    """LLMなしのヒューリスティックフォールバックテスト."""

    async def test_no_llm_returns_result(self) -> None:
        """llm_client=None でも IngestionResult を返すこと."""
        atomizer = FactAtomizer(llm_client=None)
        result = await atomizer.process("田中さんは明日の会議でプランを承認します")
        assert isinstance(result, IngestionResult)
        assert result.decision in ("keep", "discard")

    async def test_process_discard_greeting(self) -> None:
        """挨拶はprocess()でdiscardされること."""
        atomizer = FactAtomizer(llm_client=None)
        result = await atomizer.process("こんにちは！")
        assert result.decision == "discard"

    async def test_process_keep_meaningful_text(self) -> None:
        """意味のあるテキストはprocess()でkeepされること."""
        atomizer = FactAtomizer(llm_client=None)
        result = await atomizer.process("田中部長は来週月曜にプロジェクトAの予算を承認する予定です")
        assert result.decision == "keep"
        assert len(result.atomic_facts) > 0

    async def test_process_prohibited_returns_discard(self) -> None:
        """禁止コンテンツはdiscardされること."""
        atomizer = FactAtomizer(llm_client=None)
        result = await atomizer.process("パスワードはsecret123です")
        assert result.decision == "discard"

    async def test_ingestion_result_fields(self) -> None:
        """IngestionResult のフィールドが正しく設定されること."""
        atomizer = FactAtomizer(llm_client=None)
        result = await atomizer.process("田中さんが明日の会議でプランを確認します")
        assert isinstance(result.h_score, float)
        assert isinstance(result.reasons, list)
        assert isinstance(result.atomic_facts, list)
        assert isinstance(result.follow_up_questions, list)


class TestAtomicFact:
    """AtomicFact データクラスのテスト."""

    def test_atomic_fact_creation(self) -> None:
        """AtomicFact が正しく作成されること."""
        fact = AtomicFact(
            fact_text="田中さんが承認する",
            subject="田中さん",
            predicate="承認する",
            object="プランA",
            time_start="2026-03-06",
            confidence=0.9,
            tags=["決定"],
            source_excerpt="田中さんが明日承認",
            needs_coreference=False,
        )
        assert fact.fact_text == "田中さんが承認する"
        assert fact.confidence == 0.9
        assert fact.time_start == "2026-03-06"
