"""flight/ パッケージ分割のユニットテスト.

元の flight_watch.py と同等の動作を検証する。
"""

from __future__ import annotations

import pytest
from apps.messaging_hub.flight.models import (
    DateWindow,
    FlightOffer,
    FlightSearchRequest,
    RankingWeights,
    build_clarification_questions,
    extract_request_from_message,
    merge_clarification_answers,
)
from apps.messaging_hub.flight.providers.fake import FakeFlightProvider
from apps.messaging_hub.flight.ranking import _calculate_convenience_score, rank_offers


# === モデルテスト ===


class TestFlightSearchRequest:
    """FlightSearchRequest のバリデーション."""

    def test_minimal(self) -> None:
        req = FlightSearchRequest(
            origin="NRT",
            destination="LAX",
            depart_window=DateWindow(start_date="2025-07-01", end_date="2025-07-05"),
            return_window=DateWindow(start_date="2025-07-10", end_date="2025-07-15"),
        )
        assert req.origin == "NRT"
        assert req.max_stops == 1
        assert req.ranking_weights.price == 0.5

    def test_ranking_weights_normalize(self) -> None:
        w = RankingWeights(price=1.0, duration=1.0, convenience=1.0)
        total = w.price + w.duration + w.convenience
        assert abs(total - 1.0) < 0.01


# === NLP パーサーテスト ===


class TestExtractRequestFromMessage:
    """自然言語→構造化データ抽出."""

    def test_basic_extraction(self) -> None:
        msg = "origin: NRT destination: LAX depart: 2025-07-01 return: 2025-07-10 budget: 50000"
        partial, missing = extract_request_from_message(msg)
        assert partial["origin"] == "NRT"
        assert partial["destination"] == "LAX"
        assert partial["budget"] == 50000.0
        assert missing == []

    def test_watch_keyword(self) -> None:
        msg = "origin: NRT destination: LAX depart: 2025-07-01 return: 2025-07-10 monitor"
        partial, _ = extract_request_from_message(msg)
        assert partial.get("create_watch") is True

    def test_missing_fields(self) -> None:
        msg = "origin: NRT"
        partial, missing = extract_request_from_message(msg)
        assert "destination" in missing
        assert "depart_window" in missing
        assert "return_window" in missing

    def test_chinese_keywords(self) -> None:
        msg = "出发地: NRT 目的地: LAX 出发: 2025-07-01 返程: 2025-07-10 监视"
        partial, missing = extract_request_from_message(msg)
        assert partial["origin"] == "NRT"
        assert partial["destination"] == "LAX"
        assert partial.get("create_watch") is True


class TestClarificationQuestions:
    """補足質問生成."""

    def test_all_missing(self) -> None:
        questions = build_clarification_questions({}, ["origin", "destination", "depart_window", "return_window"])
        assert len(questions) >= 4  # + create_watch

    def test_none_missing(self) -> None:
        questions = build_clarification_questions({"create_watch": True}, [])
        assert len(questions) == 0


class TestMergeClarificationAnswers:
    """回答マージ."""

    def test_date_window_merge(self) -> None:
        merged = merge_clarification_answers(
            {"origin": "NRT"},
            {"depart_window": "2025-07-01..2025-07-05"},
        )
        assert merged["depart_window"]["start_date"] == "2025-07-01"
        assert merged["depart_window"]["end_date"] == "2025-07-05"

    def test_create_watch_yes(self) -> None:
        merged = merge_clarification_answers({}, {"create_watch": "yes"})
        assert merged["create_watch"] is True


# === ランキングテスト ===


class TestRankOffers:
    """ランキングエンジン."""

    def _make_offers(self) -> list[FlightOffer]:
        return [
            FlightOffer(
                offer_id="1",
                provider="fake",
                origin="NRT",
                destination="LAX",
                depart_date="2025-07-01",
                return_date="2025-07-10",
                price=300.0,
                total_duration_minutes=700,
                stops=0,
            ),
            FlightOffer(
                offer_id="2",
                provider="fake",
                origin="NRT",
                destination="LAX",
                depart_date="2025-07-01",
                return_date="2025-07-10",
                price=200.0,
                total_duration_minutes=900,
                stops=1,
                layover_minutes=[120],
            ),
            FlightOffer(
                offer_id="3",
                provider="fake",
                origin="NRT",
                destination="LAX",
                depart_date="2025-07-01",
                return_date="2025-07-10",
                price=500.0,
                total_duration_minutes=600,
                stops=0,
            ),
        ]

    def test_ranking_order(self) -> None:
        offers = self._make_offers()
        ranked = rank_offers(offers, RankingWeights())
        assert len(ranked) == 3
        # スコアが付与されていること
        assert all(o.score > 0 for o in ranked)
        # 降順であること
        assert ranked[0].score >= ranked[1].score

    def test_budget_filter(self) -> None:
        offers = self._make_offers()
        ranked = rank_offers(offers, RankingWeights(), budget=250.0)
        assert len(ranked) == 1
        assert ranked[0].price == 200.0

    def test_max_stops_filter(self) -> None:
        offers = self._make_offers()
        ranked = rank_offers(offers, RankingWeights(), max_stops=0)
        assert all(o.stops == 0 for o in ranked)

    def test_empty_after_filter(self) -> None:
        offers = self._make_offers()
        ranked = rank_offers(offers, RankingWeights(), budget=100.0)
        assert ranked == []


class TestConvenienceScore:
    """利便性スコア."""

    def test_direct_flight(self) -> None:
        offer = FlightOffer(
            offer_id="1",
            provider="f",
            origin="A",
            destination="B",
            depart_date="d",
            return_date="r",
            price=100.0,
            total_duration_minutes=600,
            stops=0,
        )
        score = _calculate_convenience_score(offer)
        assert score == 1.0

    def test_red_eye_penalty(self) -> None:
        offer = FlightOffer(
            offer_id="1",
            provider="f",
            origin="A",
            destination="B",
            depart_date="d",
            return_date="r",
            price=100.0,
            total_duration_minutes=600,
            stops=0,
            red_eye=True,
        )
        score = _calculate_convenience_score(offer)
        assert score < 1.0


# === FakeFlightProvider テスト ===


class TestFakeFlightProvider:
    """FakeFlightProvider."""

    @pytest.mark.asyncio
    async def test_returns_offers(self) -> None:
        provider = FakeFlightProvider()
        req = FlightSearchRequest(
            origin="NRT",
            destination="LAX",
            depart_window=DateWindow(start_date="2025-07-01", end_date="2025-07-05"),
            return_window=DateWindow(start_date="2025-07-10", end_date="2025-07-15"),
        )
        offers = await provider.search(req)
        assert len(offers) == 4
        assert all(o.origin == "NRT" for o in offers)
        assert all(o.price > 0 for o in offers)

    @pytest.mark.asyncio
    async def test_deterministic(self) -> None:
        """同じ入力に対して同じ結果を返す."""
        provider = FakeFlightProvider()
        req = FlightSearchRequest(
            origin="NRT",
            destination="LAX",
            depart_window=DateWindow(start_date="2025-07-01", end_date="2025-07-05"),
            return_window=DateWindow(start_date="2025-07-10", end_date="2025-07-15"),
        )
        offers1 = await provider.search(req)
        offers2 = await provider.search(req)
        assert [o.price for o in offers1] == [o.price for o in offers2]
