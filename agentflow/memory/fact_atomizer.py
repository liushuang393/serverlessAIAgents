"""SimpleMem思想: 摂取時の原子化パイプライン.

エントロピーゲートによる低価値情報の破棄、
原子事実の抽出（SPO）、時間正規化、出典付与を担う。

参考: SimpleMem - 保存→代謝パイプライン
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from typing import Any, Literal


# 挨拶パターン（日本語）
_GREETING_PATTERNS: list[str] = [
    "こんにちは",
    "こんばんは",
    "おはよう",
    "ありがとう",
    "どうも",
    "よろしく",
    "はじめまして",
    "お世話になります",
    "失礼します",
    "お疲れ様",
]

# 短いフィラーパターン
_FILLER_PATTERNS: list[str] = [
    "了解",
    "わかりました",
    "はい",
    "なるほど",
    "そうですね",
    "いいですね",
    "そうか",
    "うん",
    "ええ",
    "そうです",
]

# 日付キーワード（相対時刻）
_DATE_KEYWORDS: list[str] = [
    "明日",
    "明後日",
    "来週",
    "今日",
    "今週",
    "来月",
    "今月",
    "昨日",
    "先週",
    "今年",
    "来年",
]

# スケジュール・予定キーワード
_SCHEDULE_KEYWORDS: list[str] = [
    "会議",
    "打ち合わせ",
    "ミーティング",
    "締め切り",
    "締切",
    "予定",
    "スケジュール",
    "deadline",
]

# 禁止パターン: クレジットカード番号（16桁連続、またはXXXX-XXXX形式）
_CC_PATTERN: re.Pattern[str] = re.compile(r"(?<!\d)(?:\d{4}[-\s]?){3}\d{4}(?!\d)")
# 禁止パターン: パスワード
_PASSWORD_PATTERN: re.Pattern[str] = re.compile(
    r"(?:パスワード|password|passwd|pwd)\s*[:はは=＝]\s*\S+",
    re.IGNORECASE,
)

# 固有名詞・人名パターン
_PROPER_NOUN_PATTERN: re.Pattern[str] = re.compile(
    r"[A-Z][a-zA-Z]+|さん|部長|社長|課長|プロジェクト[A-Za-z0-9\u30A0-\u30FF]+"
)

# 月曜〜日曜の日本語
_WEEKDAY_MAP: dict[str, int] = {
    "月曜": 0,
    "火曜": 1,
    "水曜": 2,
    "木曜": 3,
    "金曜": 4,
    "土曜": 5,
    "日曜": 6,
    "月": 0,
    "火": 1,
    "水": 2,
    "木": 3,
    "金": 4,
    "土": 5,
    "日": 6,
}


@dataclass
class AtomicFact:
    """原子事実: 自己完結の1文."""

    fact_text: str  # 自己完結の1文
    subject: str  # 主語（解決済み）
    predicate: str  # 述語
    object: str  # 目的語
    time_start: str | None  # ISO-8601 or null
    confidence: float  # 0.0-1.0
    tags: list[str]  # 予定・嗜好・制約・決定 等
    source_excerpt: str  # 原文の25語以内
    needs_coreference: bool  # 未解決指代フラグ


@dataclass
class IngestionResult:
    """摂取処理の結果."""

    decision: Literal["keep", "discard"]  # 保存するかどうか
    h_score: float  # エントロピースコア（0-1）
    reasons: list[str]  # 判断理由
    atomic_facts: list[AtomicFact] = field(default_factory=list)
    follow_up_questions: list[str] = field(default_factory=list)


class FactAtomizer:
    """摂取時の原子化パイプライン（LLM依存、heuristicフォールバック）."""

    def __init__(
        self,
        llm_client: Any | None = None,
        base_time: datetime | None = None,
        h_threshold: float = 0.35,
        prohibited_patterns: list[str] | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（None の場合はヒューリスティック処理）
            base_time: 相対時刻の基準日時（None の場合は現在日時）
            h_threshold: エントロピー閾値（この値以上であれば keep）
            prohibited_patterns: 追加の禁止パターン文字列リスト
        """
        self._llm_client = llm_client
        self._base_time = base_time or datetime.now()
        self._h_threshold = h_threshold
        self._extra_prohibited: list[str] = prohibited_patterns or []

    async def process(self, text: str, context: str = "") -> IngestionResult:
        """テキストを評価→原子事実に分解して返す.

        Args:
            text: 処理対象テキスト
            context: 会話コンテキスト（省略可）

        Returns:
            摂取処理の結果
        """
        # 1. プライバシー禁止チェック
        if self._is_prohibited(text):
            return IngestionResult(
                decision="discard",
                h_score=0.0,
                reasons=["prohibited: プライバシー禁止コンテンツが検出されました"],
            )

        # 2. ヒューリスティックゲートで評価
        keep, h_score = self._heuristic_gate(text)

        if not keep or h_score < self._h_threshold:
            return IngestionResult(
                decision="discard",
                h_score=h_score,
                reasons=["low_entropy: 情報エントロピーが閾値を下回りました"],
            )

        # 3. LLMが利用可能な場合はLLM原子化を試みる
        if self._llm_client is not None:
            return await self._llm_atomize(text)

        # 4. ヒューリスティックフォールバック
        return self._heuristic_atomize(text, h_score)

    def _heuristic_gate(self, text: str) -> tuple[bool, float]:
        """LLMなしの簡易エントロピー判定（高速フォールバック）.

        日本語テキストはスペースで分割できないため、文字数ベースで判断する。

        Args:
            text: 判定対象テキスト

        Returns:
            (keep: bool, h_score: float) のタプル
        """
        # 非スペース文字のみの文字数（実質的な文字数）
        char_len = len(text.strip())

        # 6文字未満は必ず discard（"了解" "はい" など）
        if char_len < 6:
            return False, 0.1

        # 挨拶パターンを検出
        for pattern in _GREETING_PATTERNS:
            if pattern in text:
                return False, 0.1

        # 短いフィラー（12文字未満）を検出
        if char_len < 12:
            for pattern in _FILLER_PATTERNS:
                if pattern in text:
                    return False, 0.15

        # エントロピースコアを計算
        score = 0.3  # ベーススコア

        # 日付キーワードの存在
        for kw in _DATE_KEYWORDS:
            if kw in text:
                score += 0.25
                break

        # スケジュールキーワードの存在
        for kw in _SCHEDULE_KEYWORDS:
            if kw in text:
                score += 0.15
                break

        # 数値の存在（時刻・数量・IDなど）
        if re.search(r"\d+", text):
            score += 0.15

        # 固有名詞の存在（さん/部長/英字+カタカナ等）
        if _PROPER_NOUN_PATTERN.search(text):
            score += 0.2

        # 英字の存在（技術用語、プロジェクト名）
        if re.search(r"[A-Za-z]+", text):
            score += 0.1

        # テキストが長いほど情報量が多い可能性
        if char_len >= 20:
            score += 0.1

        score = min(score, 1.0)
        return score >= self._h_threshold, score

    def _normalize_time(self, text: str) -> str:
        """相対時刻をISO-8601に変換（正規表現ベース）.

        Args:
            text: 変換対象テキスト

        Returns:
            ISO-8601形式の日付を含むテキスト
        """
        result = text

        # 来週X曜: 次の週の指定曜日
        for weekday_str, weekday_num in _WEEKDAY_MAP.items():
            pattern = f"来週{weekday_str}(?:日)?(?:曜日)?"
            if re.search(pattern, result):
                # 今週の月曜日を基準に7日進める
                current_weekday = self._base_time.weekday()  # 0=月, 6=日
                days_to_monday = -current_weekday  # 今週の月曜に戻る
                this_week_monday = self._base_time + timedelta(days=days_to_monday)
                next_week_monday = this_week_monday + timedelta(days=7)
                target_date = next_week_monday + timedelta(days=weekday_num)
                iso_str = target_date.strftime("%Y-%m-%d")
                result = re.sub(pattern, iso_str, result)
                break

        # 来週（曜日なし）: 7日後
        if "来週" in result:
            target = self._base_time + timedelta(days=7)
            result = result.replace("来週", target.strftime("%Y-%m-%d"))

        # 明後日: 2日後
        if "明後日" in result:
            target = self._base_time + timedelta(days=2)
            result = result.replace("明後日", target.strftime("%Y-%m-%d"))

        # 明日: 1日後
        if "明日" in result:
            target = self._base_time + timedelta(days=1)
            result = result.replace("明日", target.strftime("%Y-%m-%d"))

        # 今日: 当日
        if "今日" in result:
            result = result.replace("今日", self._base_time.strftime("%Y-%m-%d"))

        # 昨日: 1日前
        if "昨日" in result:
            target = self._base_time - timedelta(days=1)
            result = result.replace("昨日", target.strftime("%Y-%m-%d"))

        return result

    def _is_prohibited(self, text: str) -> bool:
        """プライバシー禁止カテゴリ判定.

        Args:
            text: チェック対象テキスト

        Returns:
            禁止コンテンツが含まれる場合True
        """
        # クレジットカード番号パターン
        if _CC_PATTERN.search(text):
            return True

        # パスワードパターン
        if _PASSWORD_PATTERN.search(text):
            return True

        # カスタム禁止パターン
        return any(pattern in text for pattern in self._extra_prohibited)

    def _heuristic_atomize(self, text: str, h_score: float) -> IngestionResult:
        """ヒューリスティックで原子事実を抽出（LLMなし）.

        Args:
            text: 原子化対象テキスト
            h_score: エントロピースコア

        Returns:
            摂取処理の結果
        """
        # 時間正規化を適用
        normalized = self._normalize_time(text)

        # 簡易SPO抽出: テキストをそのまま fact_text として使用
        # 主語: 最初の固有名詞または「が/は」の前
        subject = self._extract_subject(text)
        predicate = self._extract_predicate(text)
        obj = self._extract_object(text)

        # タグ推定
        tags: list[str] = []
        for kw in _SCHEDULE_KEYWORDS:
            if kw in text:
                tags.append("予定")
                break
        for kw in _DATE_KEYWORDS:
            if kw in text:
                tags.append("時間")
                break
        if _PROPER_NOUN_PATTERN.search(text):
            tags.append("人物")

        # 出典抜粋（25語以内）
        words = text.split()
        excerpt = " ".join(words[:25])

        # 時間情報の抽出
        time_start: str | None = None
        for kw in _DATE_KEYWORDS:
            if kw in text:
                # 時間情報が含まれていれば正規化後のテキストから抽出
                date_match = re.search(r"\d{4}-\d{2}-\d{2}", normalized)
                if date_match:
                    time_start = date_match.group(0)
                break

        fact = AtomicFact(
            fact_text=normalized,
            subject=subject,
            predicate=predicate,
            object=obj,
            time_start=time_start,
            confidence=h_score,
            tags=tags,
            source_excerpt=excerpt,
            needs_coreference="彼" in text or "彼女" in text or "それ" in text,
        )

        # フォローアップ質問の生成
        follow_ups: list[str] = []
        if fact.needs_coreference:
            follow_ups.append("代名詞の指示対象を確認する必要があります")

        return IngestionResult(
            decision="keep",
            h_score=h_score,
            reasons=["heuristic: ヒューリスティックで情報価値ありと判断"],
            atomic_facts=[fact],
            follow_up_questions=follow_ups,
        )

    def _extract_subject(self, text: str) -> str:
        """テキストから主語を簡易抽出.

        Args:
            text: 対象テキスト

        Returns:
            抽出された主語（見つからない場合は空文字）
        """
        # 「〜は」「〜が」のパターン
        match = re.search(r"(\S+?)(?:は|が)", text)
        if match:
            return match.group(1)
        # 固有名詞パターン
        pn_match = _PROPER_NOUN_PATTERN.search(text)
        if pn_match:
            return pn_match.group(0)
        return ""

    def _extract_predicate(self, text: str) -> str:
        """テキストから述語を簡易抽出.

        Args:
            text: 対象テキスト

        Returns:
            抽出された述語（見つからない場合は空文字）
        """
        # 文末の動詞（〜する、〜した、〜ます、〜ました等）
        match = re.search(r"(\S+(?:する|した|ます|ました|います|いました|ある|あった))(?:[。．]|$)", text)
        if match:
            return match.group(1)
        return ""

    def _extract_object(self, text: str) -> str:
        """テキストから目的語を簡易抽出.

        Args:
            text: 対象テキスト

        Returns:
            抽出された目的語（見つからない場合は空文字）
        """
        # 「〜を」パターン
        match = re.search(r"(\S+?)を", text)
        if match:
            return match.group(1)
        return ""

    async def _llm_atomize(self, text: str) -> IngestionResult:
        """LLMでSPO抽出・指代解決・原子化（JSON出力）.

        Args:
            text: 原子化対象テキスト

        Returns:
            摂取処理の結果

        Note:
            現在はヒューリスティックにフォールバック。
            LLMクライアントが提供された場合は実装を拡張してください。
        """
        # LLMクライアントがあってもフォールバックとして同じロジックを使用
        # 実際のLLM統合は外部から提供されるクライアントに委ねる
        keep, h_score = self._heuristic_gate(text)
        return self._heuristic_atomize(text, h_score)
