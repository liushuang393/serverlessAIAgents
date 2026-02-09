"""SelfImprovementLoop - 自己改進循環.

Monitor → Analyze → Improve → Deploy → Verify のループを実行します。

使用例:
    >>> loop = SelfImprovementLoop()
    >>> await loop.start()
    >>> # バックグラウンドで継続的に改善
    >>> await loop.stop()
"""

from __future__ import annotations

import asyncio
import contextlib
import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow.providers import get_llm
from agentflow.wizard.gap_detector import CapabilityGapDetector
from agentflow.wizard.system_synthesizer import SystemSynthesizer


if TYPE_CHECKING:
    from collections.abc import Callable

    from agentflow.wizard.models import (
        CapabilityGap,
        GapAnalysis,
        SynthesisResult,
    )


class LoopPhase(str, Enum):
    """ループフェーズ."""

    IDLE = "idle"
    MONITORING = "monitoring"
    ANALYZING = "analyzing"
    IMPROVING = "improving"
    DEPLOYING = "deploying"
    VERIFYING = "verifying"


@dataclass
class ImprovementRecord:
    """改善記録.

    Attributes:
        record_id: 記録ID
        gap: 対象の能力缺口
        synthesis: 合成結果
        deployed: デプロイされたか
        verified: 検証されたか
        timestamp: タイムスタンプ
    """

    record_id: str
    gap: CapabilityGap
    synthesis: SynthesisResult | None = None
    deployed: bool = False
    verified: bool = False
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    error: str | None = None


@dataclass
class LoopStats:
    """ループ統計.

    Attributes:
        iterations: 実行回数
        gaps_detected: 検出した缺口数
        improvements_made: 行った改善数
        successful_deployments: 成功したデプロイ数
        failed_deployments: 失敗したデプロイ数
    """

    iterations: int = 0
    gaps_detected: int = 0
    improvements_made: int = 0
    successful_deployments: int = 0
    failed_deployments: int = 0
    last_run: datetime | None = None


class SelfImprovementLoop:
    """自己改進循環.

    システムを継続的に監視し、能力缺口を検出、改善を行います。

    ループ:
    1. Monitor - システムの状態を監視
    2. Analyze - 能力缺口を分析
    3. Improve - 改善を生成
    4. Deploy - 改善をデプロイ
    5. Verify - 改善を検証

    Example:
        >>> loop = SelfImprovementLoop()
        >>> await loop.start()
        >>> # 手動で1回実行
        >>> result = await loop.run_once()
    """

    def __init__(
        self,
        llm_client: Any = None,
        gap_detector: CapabilityGapDetector | None = None,
        synthesizer: SystemSynthesizer | None = None,
        interval_seconds: int = 3600,
        auto_deploy: bool = False,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント
            gap_detector: 缺口検出器
            synthesizer: システム合成器
            interval_seconds: ループ間隔（秒）
            auto_deploy: 自動デプロイを有効にするか
        """
        self._llm = llm_client or get_llm()
        self._detector = gap_detector or CapabilityGapDetector()
        self._synthesizer = synthesizer or SystemSynthesizer(llm_client=self._llm)
        self._interval = interval_seconds
        self._auto_deploy = auto_deploy
        self._logger = logging.getLogger(__name__)

        # 状態
        self._running = False
        self._current_phase = LoopPhase.IDLE
        self._task: asyncio.Task | None = None
        self._stats = LoopStats()
        self._history: list[ImprovementRecord] = []

        # コールバック
        self._on_improvement: Callable[[ImprovementRecord], None] | None = None
        self._on_phase_change: Callable[[LoopPhase], None] | None = None

    async def start(self) -> None:
        """ループを開始."""
        if self._running:
            self._logger.warning("Loop is already running")
            return

        self._running = True
        self._task = asyncio.create_task(self._loop())
        self._logger.info("Self-improvement loop started")

    async def stop(self) -> None:
        """ループを停止."""
        self._running = False
        if self._task:
            self._task.cancel()
            with contextlib.suppress(asyncio.CancelledError):
                await self._task
            self._task = None
        self._current_phase = LoopPhase.IDLE
        self._logger.info("Self-improvement loop stopped")

    async def run_once(self) -> list[ImprovementRecord]:
        """1回のループを実行.

        Returns:
            改善記録リスト
        """
        records: list[ImprovementRecord] = []

        try:
            # 1. Monitor
            self._set_phase(LoopPhase.MONITORING)
            await self._monitor()

            # 2. Analyze
            self._set_phase(LoopPhase.ANALYZING)
            analysis = await self._analyze()

            if not analysis.gaps:
                self._logger.info("No gaps detected")
                self._stats.iterations += 1
                self._stats.last_run = datetime.now(UTC)
                return records

            self._stats.gaps_detected += len(analysis.gaps)

            # 3. Improve
            self._set_phase(LoopPhase.IMPROVING)
            for gap in analysis.priority_gaps[:5]:
                record = await self._improve(gap)
                if record:
                    records.append(record)

            # 4. Deploy
            if self._auto_deploy:
                self._set_phase(LoopPhase.DEPLOYING)
                for record in records:
                    await self._deploy(record)

            # 5. Verify
            self._set_phase(LoopPhase.VERIFYING)
            for record in records:
                await self._verify(record)

            self._stats.iterations += 1
            self._stats.last_run = datetime.now(UTC)
            self._history.extend(records)

        except Exception as e:
            self._logger.exception(f"Loop iteration failed: {e}")

        finally:
            self._set_phase(LoopPhase.IDLE)

        return records

    async def _loop(self) -> None:
        """メインループ."""
        while self._running:
            try:
                await self.run_once()
                await asyncio.sleep(self._interval)
            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.exception(f"Loop error: {e}")
                await asyncio.sleep(60)  # エラー時は短い間隔で再試行

    async def _monitor(self) -> None:
        """システムを監視.

        メトリクス収集、ログ分析などを行います。
        """
        # TODO: 実際のモニタリング実装
        self._logger.debug("Monitoring system...")
        await asyncio.sleep(0.1)

    async def _analyze(self) -> GapAnalysis:
        """能力缺口を分析.

        Returns:
            缺口分析結果
        """
        self._logger.debug("Analyzing gaps...")
        return await self._detector.analyze()

    async def _improve(self, gap: CapabilityGap) -> ImprovementRecord | None:
        """改善を生成.

        Args:
            gap: 能力缺口

        Returns:
            改善記録
        """
        self._logger.info(f"Improving gap: {gap.description[:50]}...")

        import uuid
        record_id = f"imp_{uuid.uuid4().hex[:12]}"

        try:
            # 自動修正可能な場合のみ合成
            if gap.auto_fixable:
                synthesis = await self._synthesizer.synthesize(
                    [gap],
                    generate_tests=True,
                    validate=True,
                )

                record = ImprovementRecord(
                    record_id=record_id,
                    gap=gap,
                    synthesis=synthesis,
                )

                if synthesis.success:
                    self._stats.improvements_made += 1

                    if self._on_improvement:
                        self._on_improvement(record)

                return record

            # 手動対応が必要
            return ImprovementRecord(
                record_id=record_id,
                gap=gap,
                error="Manual intervention required",
            )

        except Exception as e:
            self._logger.exception(f"Improvement failed: {e}")
            return ImprovementRecord(
                record_id=record_id,
                gap=gap,
                error=str(e),
            )

    async def _deploy(self, record: ImprovementRecord) -> None:
        """改善をデプロイ.

        Args:
            record: 改善記録
        """
        if not record.synthesis or not record.synthesis.success:
            return

        self._logger.info(f"Deploying improvement: {record.record_id}")

        try:
            # TODO: 実際のデプロイ実装
            # 現在はシミュレーション
            await asyncio.sleep(0.1)
            record.deployed = True
            self._stats.successful_deployments += 1

        except Exception as e:
            self._logger.exception(f"Deploy failed: {e}")
            record.error = str(e)
            self._stats.failed_deployments += 1

    async def _verify(self, record: ImprovementRecord) -> None:
        """改善を検証.

        Args:
            record: 改善記録
        """
        if not record.deployed:
            return

        self._logger.info(f"Verifying improvement: {record.record_id}")

        try:
            # TODO: 実際の検証実装
            # 現在はシミュレーション
            await asyncio.sleep(0.1)
            record.verified = True

        except Exception as e:
            self._logger.exception(f"Verify failed: {e}")
            record.error = str(e)

    def _set_phase(self, phase: LoopPhase) -> None:
        """フェーズを設定.

        Args:
            phase: 新しいフェーズ
        """
        self._current_phase = phase
        self._logger.debug(f"Phase: {phase.value}")

        if self._on_phase_change:
            self._on_phase_change(phase)

    def on_improvement(self, callback: Callable[[ImprovementRecord], None]) -> None:
        """改善コールバックを設定.

        Args:
            callback: コールバック関数
        """
        self._on_improvement = callback

    def on_phase_change(self, callback: Callable[[LoopPhase], None]) -> None:
        """フェーズ変更コールバックを設定.

        Args:
            callback: コールバック関数
        """
        self._on_phase_change = callback

    @property
    def is_running(self) -> bool:
        """実行中かどうか."""
        return self._running

    @property
    def current_phase(self) -> LoopPhase:
        """現在のフェーズ."""
        return self._current_phase

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        return {
            "iterations": self._stats.iterations,
            "gaps_detected": self._stats.gaps_detected,
            "improvements_made": self._stats.improvements_made,
            "successful_deployments": self._stats.successful_deployments,
            "failed_deployments": self._stats.failed_deployments,
            "last_run": self._stats.last_run.isoformat() if self._stats.last_run else None,
            "is_running": self._running,
            "current_phase": self._current_phase.value,
        }

    def get_history(self, limit: int = 50) -> list[dict[str, Any]]:
        """改善履歴を取得.

        Args:
            limit: 取得数

        Returns:
            改善記録リスト
        """
        records = self._history[-limit:]
        return [
            {
                "record_id": r.record_id,
                "gap_description": r.gap.description,
                "gap_type": r.gap.gap_type.value,
                "deployed": r.deployed,
                "verified": r.verified,
                "error": r.error,
                "timestamp": r.timestamp.isoformat(),
            }
            for r in reversed(records)
        ]


__all__ = ["ImprovementRecord", "LoopPhase", "LoopStats", "SelfImprovementLoop"]
