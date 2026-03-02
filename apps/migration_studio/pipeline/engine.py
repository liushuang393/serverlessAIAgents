"""移行パイプラインエンジン.

6ステージ（Analyzer→Designer→Transformer→TestGenerator→Verifier→QualityGate）
+ Evolution Loop を実行する。
Claude Agent SDK の query() を各ステージで使用する。
"""

from __future__ import annotations

import asyncio
import json
import logging
import re
from collections.abc import AsyncGenerator
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any

import anyio
try:
    from claude_agent_sdk import ClaudeAgentOptions, ResultMessage, query
except ModuleNotFoundError:  # pragma: no cover - SDK 未導入環境向けフォールバック
    ClaudeAgentOptions = None  # type: ignore[assignment]
    ResultMessage = object  # type: ignore[assignment]
    query = None  # type: ignore[assignment]

from apps.migration_studio.agents.definitions import get_agent_definition
from apps.migration_studio.evolution.manager import EvolutionManager
from apps.migration_studio.output.organizer import OutputOrganizer
from apps.migration_studio.pipeline.project import COBOLFile

logger = logging.getLogger(__name__)

# デフォルト Evolution 最大反復数
_DEFAULT_MAX_ITERATIONS = 3

# パイプラインステージの定義順（この順番で実行）
_PIPELINE_STAGES = [
    "analyzer",
    "designer",
    "transformer",
    "test_generator",
    "verifier",
    "quality_gate",
]

# QualityGate 判定 → 再実行開始ステージのマッピング
_RETRY_FROM_STAGE: dict[str, str] = {
    "DESIGN_ISSUE": "designer",
    "TRANSFORM_ISSUE": "transformer",
    "TEST_ISSUE": "test_generator",
}


@dataclass
class SSEEvent:
    """SSEイベントデータクラス."""

    event_type: str
    stage: str | None
    data: dict[str, Any]
    # HITL待機用イベント（JSON非シリアライズ、routerとの内部I/F）
    _hitl_event: asyncio.Event | None = field(default=None, compare=False, repr=False)


@dataclass
class PipelineResult:
    """パイプライン実行結果."""

    program_name: str
    version: int
    decision: str
    artifacts: dict[str, Any]
    output_dir: Path
    java_files: list[Path]
    success: bool
    error_message: str | None = None


class MigrationEngine:
    """COBOL→Java Spring Boot 移行パイプラインエンジン.

    6ステージパイプライン + Evolution Loop を実行し、
    各ステージの進捗をSSEイベントとして yield する。
    """

    def __init__(
        self,
        output_root: Path,
        fast_mode: bool = False,
        max_iterations: int = _DEFAULT_MAX_ITERATIONS,
        model: str = "claude-opus-4-6",
    ) -> None:
        """初期化.

        Args:
            output_root: 移行成果物の出力ルートディレクトリ
            fast_mode: True の場合は実行比較をスキップ
            max_iterations: Evolution 最大反復数
            model: 使用するClaudeモデルID
        """
        self.output_root = output_root
        self.fast_mode = fast_mode
        self.max_iterations = max_iterations
        self.model = model

        # サブコンポーネント
        prompts_dir = Path(__file__).parent.parent / "agents" / "prompts"
        self.evolution_manager = EvolutionManager(prompts_dir, max_iterations)
        self.organizer = OutputOrganizer(output_root)

    async def run_file(
        self,
        cobol_file: COBOLFile,
    ) -> AsyncGenerator[SSEEvent, None]:
        """単一COBOLファイルを移行する.

        Args:
            cobol_file: 変換対象COBOLファイル

        Yields:
            SSEEvent: 各ステージの進捗イベント
        """
        program_name = cobol_file.program_name
        version_count = self.organizer.get_version_count(program_name) + 1
        version_dir = self.organizer.create_version_dir(program_name, version_count)
        program_dir = self.organizer.get_program_dir(program_name)

        yield SSEEvent(
            event_type="stage_start",
            stage="pipeline",
            data={"message": f"移行パイプライン開始: {program_name}", "version": version_count},
        )

        artifacts: dict[str, Any] = {}
        current_iteration = 0
        decision = "UNKNOWN"  # ループ前にデフォルト初期化

        # Evolution Loop（最大 max_iterations 回）
        while current_iteration <= self.max_iterations:
            # SSEイベントキューを通じてステージ進捗をリアルタイム配信
            stage_event_queue: asyncio.Queue[SSEEvent | None] = asyncio.Queue()

            # パイプラインをバックグラウンドタスクとして実行しながらイベントをドレイン
            pipeline_task = asyncio.create_task(
                self._run_pipeline(
                    cobol_file=cobol_file,
                    version_dir=version_dir,
                    artifacts=artifacts,
                    event_queue=stage_event_queue,
                )
            )

            # パイプラインからのステージ進捗イベントをリアルタイムで yield
            while True:
                try:
                    stage_event = await asyncio.wait_for(
                        stage_event_queue.get(), timeout=300.0
                    )
                except asyncio.TimeoutError:
                    logger.warning("ステージイベント待機タイムアウト")
                    break
                if stage_event is None:  # sentinel: パイプライン完了
                    break
                yield stage_event

            decision, artifacts = await pipeline_task

            yield SSEEvent(
                event_type="stage_complete",
                stage="quality_gate",
                data={"decision": decision, "iteration": current_iteration},
            )

            if decision in ("PASSED", "KNOWN_LEGACY", "ENV_ISSUE"):
                break

            # Evolution: 問題があれば改善して再実行
            if not self.evolution_manager.can_evolve(program_name, decision):
                logger.warning("最大反復数超過: %s", program_name)
                break

            evolution_info = self.evolution_manager.evolve(
                program_name=program_name,
                decision=decision,
                quality_gate_result=artifacts.get("quality_gate", {}),
                version_dir=version_dir,
            )

            if evolution_info is None:
                break

            self.evolution_manager.record_evolution(evolution_info, program_dir, program_name)

            yield SSEEvent(
                event_type="evolution",
                stage=evolution_info["stage"],
                data={
                    **evolution_info,
                    # app.js が読むエイリアス（event.problem, event.fix）
                    "problem": evolution_info.get("decision", ""),
                    "fix": evolution_info.get("fix_summary", ""),
                },
            )

            # 再実行開始ステージの決定
            retry_stage = self.evolution_manager.get_retry_stage(decision)
            if retry_stage is None:
                break

            current_iteration += 1

            # バージョンを更新して再実行
            version_count = self.organizer.get_version_count(program_name) + 1
            version_dir = self.organizer.create_version_dir(program_name, version_count)

        # サマリーレポート生成
        all_versions = [artifacts]
        self.organizer.generate_summary_report(program_name, all_versions)

        yield SSEEvent(
            event_type="complete",
            stage="pipeline",
            data={
                "program_name": program_name,
                "decision": decision,
                "version": version_count,
                "output_dir": str(self.output_root / program_name),
            },
        )

    async def run_file_from_stage(
        self,
        cobol_file: COBOLFile,
        start_stage: str,
        existing_version: int | None = None,
    ) -> AsyncGenerator[SSEEvent, None]:
        """既存成果物を活かして特定ステージから移行を再開する.

        Args:
            cobol_file: 変換対象COBOLファイル
            start_stage: 再実行を開始するステージ名
            existing_version: 使用するバージョン番号（省略時は最新）

        Yields:
            SSEEvent: 各ステージの進捗イベント
        """
        program_name = cobol_file.program_name

        if existing_version is None:
            existing_version = self.organizer.get_version_count(program_name)

        version_dir = self.organizer.get_version_dir(program_name, existing_version)
        if not version_dir.exists():
            yield SSEEvent(
                event_type="error",
                stage="pipeline",
                data={"message": f"バージョンディレクトリが存在しません: {version_dir}"},
            )
            return

        # start_stage より前の成果物を読み込む
        artifacts = self.organizer.load_artifacts(version_dir, up_to_stage=start_stage)

        yield SSEEvent(
            event_type="stage_start",
            stage="pipeline",
            data={
                "message": f"ステージ {start_stage} から再実行: {program_name} (v{existing_version})",
                "version": existing_version,
            },
        )

        stage_event_queue: asyncio.Queue[SSEEvent | None] = asyncio.Queue()
        pipeline_task = asyncio.create_task(
            self._run_pipeline(
                cobol_file=cobol_file,
                version_dir=version_dir,
                artifacts=artifacts,
                event_queue=stage_event_queue,
                start_stage=start_stage,
            )
        )

        while True:
            try:
                stage_event = await asyncio.wait_for(
                    stage_event_queue.get(), timeout=300.0
                )
            except asyncio.TimeoutError:
                logger.warning("ステージイベント待機タイムアウト")
                break
            if stage_event is None:
                break
            yield stage_event

        decision, artifacts = await pipeline_task

        yield SSEEvent(
            event_type="complete",
            stage="pipeline",
            data={
                "program_name": program_name,
                "decision": decision,
                "version": existing_version,
                "output_dir": str(self.output_root / program_name),
            },
        )

    async def _run_pipeline(
        self,
        cobol_file: COBOLFile,
        version_dir: Path,
        artifacts: dict[str, Any],
        event_queue: asyncio.Queue,
        start_stage: str | None = None,
    ) -> tuple[str, dict[str, Any]]:
        """パイプライン全6ステージを実行する.

        各ステージの開始・完了イベントを event_queue に投入し、
        run_file がリアルタイムで yield できるようにする。
        最後に None（sentinel）を投入してストリームの終端を知らせる。

        Args:
            cobol_file: 変換対象COBOLファイル
            version_dir: 出力バージョンディレクトリ
            artifacts: 前イテレーションの成果物（再実行時に引き継ぐ）
            event_queue: ステージ進捗イベントを送る asyncio.Queue
            start_stage: このステージから開始（前のステージはスキップ）

        Returns:
            (decision, artifacts) のタプル
        """
        program_name = cobol_file.program_name
        cobol_content = cobol_file.content

        # ステージごとの作業ディレクトリ（Glob/Grep/Readのルート）
        _cwd_map: dict[str, str] = {
            "analyzer": str(cobol_file.path.parent),
            "designer": str(version_dir),
            "transformer": str(version_dir / "03_transform"),
            "test_generator": str(version_dir / "03_transform"),
            "verifier": str(version_dir),
            "quality_gate": str(version_dir),
        }

        started = start_stage is None  # start_stage 指定なし → 全ステージ実行

        try:
            for stage in _PIPELINE_STAGES:
                # start_stage が指定されている場合、そのステージまでスキップ
                if not started:
                    if stage == start_stage:
                        started = True
                    else:
                        continue

                logger.info("[%s] %s 開始", stage, program_name)

                await event_queue.put(SSEEvent(
                    event_type="stage_start",
                    stage=stage,
                    data={"message": f"{stage} 実行中...", "program": program_name},
                ))

                prompt = self._build_prompt(
                    stage=stage,
                    cobol_content=cobol_content,
                    artifacts=artifacts,
                    fast_mode=self.fast_mode,
                    cobol_path=cobol_file.path,
                    version_dir=version_dir,
                )

                agent_def = get_agent_definition(stage)
                result_text = await self._run_agent(
                    stage=stage,
                    prompt=prompt,
                    agent_def=agent_def,
                    cwd=_cwd_map.get(stage),
                )

                stage_data = self._parse_stage_result(stage, result_text)
                artifacts[stage] = stage_data

                # Java ファイルの保存（transformer ステージ）
                if stage == "transformer":
                    java_code = stage_data.get("target_code", "")
                    if java_code:
                        self.organizer.save_java_files(java_code, version_dir)

                # レポートの保存（最終ステージ後）
                if stage == "quality_gate":
                    report_md = self._build_report(program_name, artifacts)
                    self.organizer.save_report(report_md, version_dir)

                # 成果物をJSONで保存
                self.organizer.save_artifact(
                    artifact_data=stage_data,
                    stage=stage,
                    version_dir=version_dir,
                )

                # HITL検出（analyzer/designer ステージ）
                if stage in ("analyzer", "designer") and self._should_hitl(stage_data):
                    hitl_request_id = str(uuid.uuid4())
                    hitl_ev = asyncio.Event()
                    question = (
                        "分析結果に多数の不明点があります。移行設計に進む前に確認してください。"
                        if stage == "analyzer"
                        else "設計内容に外部システムへの依存があります。変換を進める前に確認してください。"
                    )
                    await event_queue.put(SSEEvent(
                        event_type="hitl_required",
                        stage=stage,
                        data={
                            "request_id": hitl_request_id,
                            "artifact": stage_data,
                            "unknowns": stage_data.get("unknowns", []),
                            "question": question,
                        },
                        _hitl_event=hitl_ev,
                    ))
                    # ルーターがHITLを受け取り、応答するまで待機
                    await hitl_ev.wait()

                logger.info("[%s] %s 完了", stage, program_name)

                # QualityGate で判定結果を確認
                if stage == "quality_gate":
                    decision = stage_data.get("decision", "ENV_ISSUE")
                    return decision, artifacts

        finally:
            await event_queue.put(None)  # sentinel: ドレイナーへ完了を通知

        return "ENV_ISSUE", artifacts

    async def _run_agent(
        self,
        stage: str,
        prompt: str,
        agent_def: Any,
        cwd: str | None = None,
    ) -> str:
        """エージェントを実行して結果テキストを返す.

        Args:
            stage: ステージ名
            prompt: エージェントへのプロンプト
            agent_def: AgentDefinition オブジェクト
            cwd: 作業ディレクトリ（Glob/Grep/Read のルート）

        Returns:
            エージェントの出力テキスト
        """
        result_text = ""

        if ClaudeAgentOptions is None or query is None:
            return json.dumps({
                "error": "claude_agent_sdk がインストールされていません",
                "stage": stage,
                "decision": "ENV_ISSUE",
                "reason": "claude_agent_sdk not installed",
            })

        options = ClaudeAgentOptions(
            allowed_tools=agent_def.tools,
            system_prompt=agent_def.prompt,
            model=self.model,
            max_turns=20,
            cwd=cwd,
        )

        try:
            async for message in query(prompt=prompt, options=options):
                if isinstance(message, ResultMessage):
                    result_text = message.result or ""
        except Exception as exc:  # noqa: BLE001
            logger.error("[%s] エージェント実行エラー: %s", stage, exc)
            result_text = json.dumps({
                "error": str(exc),
                "stage": stage,
                "decision": "ENV_ISSUE",
                "reason": f"エージェント実行エラー: {exc}",
            })

        return result_text

    @staticmethod
    def _should_hitl(stage_data: dict[str, Any]) -> bool:
        """HITLが必要かどうかを判定する.

        以下の条件のいずれかを満たす場合に HITL を発動する:
        - unknowns が 5 件以上ある
        - DB操作がある（has_database_access）
        - 外部呼び出しがある（has_external_calls）
        """
        unknowns = stage_data.get("unknowns", [])
        if len(unknowns) >= 5:
            return True
        if stage_data.get("has_database_access"):
            return True
        if stage_data.get("has_external_calls"):
            return True
        return False

    @staticmethod
    def _build_prompt(
        stage: str,
        cobol_content: str,
        artifacts: dict[str, Any],
        fast_mode: bool,
        cobol_path: Path | None = None,
        version_dir: Path | None = None,
    ) -> str:
        """ステージ別のプロンプトを組み立てる.

        Args:
            stage: ステージ名
            cobol_content: COBOLソースコード
            artifacts: 前ステージまでの成果物
            fast_mode: 高速モードフラグ
            cobol_path: COBOLファイルの絶対パス（Glob/Readでの探索用）
            version_dir: 出力バージョンディレクトリ（成果物の読み書き用）

        Returns:
            エージェントへのプロンプト文字列
        """
        # ファイルシステムコンテキスト（Glob/Grepで探索できる場所を明示）
        cobol_ctx = f"\nCOBOLファイルパス: {cobol_path}" if cobol_path else ""
        copy_ctx = (
            f"\n同ディレクトリのCOPYファイルは Glob(\"{cobol_path.parent}/**/*.cpy\") で探索できます"
            if cobol_path else ""
        )
        java_src_dir = version_dir / "03_transform" if version_dir else None
        java_ctx = f"\nJava出力先: {java_src_dir}/src/main/java/" if java_src_dir else ""
        test_ctx = f"\nテスト出力先: {java_src_dir}/src/test/java/" if java_src_dir else ""
        version_ctx = f"\n成果物ディレクトリ: {version_dir}/" if version_dir else ""

        if stage == "analyzer":
            return (
                f"以下のCOBOLコードを分析し、指定のJSON形式で出力してください。"
                f"{cobol_ctx}{copy_ctx}\n\n"
                f"```cobol\n{cobol_content}\n```"
            )

        if stage == "designer":
            analysis = json.dumps(
                artifacts.get("analyzer", {}),
                ensure_ascii=False, indent=2,
            )
            return (
                f"以下の分析結果をもとにJava Spring Boot移行設計を行い、"
                f"指定のJSON形式で出力してください。"
                f"{cobol_ctx}\n\n"
                f"## 分析結果\n```json\n{analysis}\n```\n\n"
                f"## COBOLソース参考\n```cobol\n{cobol_content}\n```"
            )

        if stage == "transformer":
            analysis = json.dumps(
                artifacts.get("analyzer", {}),
                ensure_ascii=False, indent=2,
            )
            design = json.dumps(
                artifacts.get("designer", {}),
                ensure_ascii=False, indent=2,
            )
            return (
                f"以下の分析結果と設計書に基づいて、"
                f"完全なJava Spring Bootプロジェクトを生成してください。"
                f"{java_ctx}{test_ctx}\n"
                f"（Writeツールで上記パスに直接ファイルを書き込んでください）\n\n"
                f"## 分析結果\n```json\n{analysis}\n```\n\n"
                f"## 設計書\n```json\n{design}\n```\n\n"
                f"## COBOLソース\n```cobol\n{cobol_content}\n```"
            )

        if stage == "test_generator":
            analysis = json.dumps(
                artifacts.get("analyzer", {}),
                ensure_ascii=False, indent=2,
            )
            transformer_result = artifacts.get("transformer", {})
            java_code = transformer_result.get("target_code", "")
            return (
                f"以下の分析結果とJavaコードに基づいて、"
                f"JUnit 5 + MockMvc テストケースを生成してください。"
                f"{java_ctx}{test_ctx}\n"
                f"（Glob + Readでソースを読み込み、テストをWriteで書き込んでください）\n\n"
                f"## 分析結果\n```json\n{analysis}\n```\n\n"
                f"## 生成されたJavaコード\n```java\n{java_code}\n```"
            )

        if stage == "verifier":
            analysis = json.dumps(
                artifacts.get("analyzer", {}),
                ensure_ascii=False, indent=2,
            )
            transformer_result = artifacts.get("transformer", {})
            java_code = transformer_result.get("target_code", "")
            fast_flag = "true" if fast_mode else "false"
            return (
                f"COBOL ASTとJava ASTを比較検証し、"
                f"指定のJSON形式で出力してください。"
                f"{cobol_ctx}{java_ctx}\n"
                f"（Glob(\"{java_src_dir}/src/**/*.java\") で実際のJavaファイルを探索できます）\n\n"
                f"## COBOL分析結果（COBOL AST）\n```json\n{analysis}\n```\n\n"
                f"## 生成されたJavaコード（Java AST）\n```java\n{java_code}\n```\n\n"
                f"fast_mode: {fast_flag}"
            )

        if stage == "quality_gate":
            all_artifacts = {
                k: v for k, v in artifacts.items()
                if k in ("analyzer", "designer", "transformer", "verifier")
            }
            artifacts_json = json.dumps(all_artifacts, ensure_ascii=False, indent=2)
            return (
                f"以下の全ステージ成果物を評価し、"
                f"指定のJSON形式で品質判定を出力してください。"
                f"{version_ctx}\n"
                f"（Glob + Readで成果物ファイルを直接確認できます）\n\n"
                f"## 全ステージ成果物\n```json\n{artifacts_json}\n```"
            )

        return f"ステージ {stage} を実行してください。入力: {cobol_content[:200]}"

    @staticmethod
    def _parse_stage_result(stage: str, result_text: str) -> dict[str, Any]:
        """ステージ結果テキストをパースしてdictに変換する.

        Args:
            stage: ステージ名
            result_text: エージェントの出力テキスト

        Returns:
            パースされた結果dict
        """
        if not result_text.strip():
            return {"stage": stage, "error": "空のレスポンス"}

        # JSON ブロックを抽出
        json_match = re.search(r"```(?:json)?\s*(\{.*?\})\s*```", result_text, re.DOTALL)
        if json_match:
            try:
                return json.loads(json_match.group(1))
            except json.JSONDecodeError:
                pass

        # JSON ブロックなしで全体をパースを試みる
        try:
            return dict(json.loads(result_text))
        except (json.JSONDecodeError, TypeError):
            pass

        # テキストとして保存（transformer ステージ等）
        if stage == "transformer":
            return {"target_code": result_text, "stage": stage}

        return {"raw_output": result_text, "stage": stage}

    @staticmethod
    def _build_report(
        program_name: str,
        artifacts: dict[str, Any],
    ) -> str:
        """移行レポートをMarkdown形式で生成する.

        Args:
            program_name: プログラム名
            artifacts: 全ステージの成果物

        Returns:
            Markdownレポート文字列
        """
        quality = artifacts.get("quality_gate", {})
        decision = quality.get("decision", "不明")
        reason = quality.get("reason", "不明")
        evidence = quality.get("evidence", {})

        verification = artifacts.get("verifier", {})
        equivalence = verification.get("equivalence", False)
        confidence = verification.get("confidence", 0.0)

        lines = [
            f"# 移行レポート: {program_name}",
            "",
            "## 品質判定結果",
            f"- 判定: **{decision}**",
            f"- 理由: {reason}",
            "",
            "## 検証結果",
            f"- 等価性: {'✓' if equivalence else '✗'}",
            f"- 信頼度: {confidence:.0%}",
            "",
            "## エビデンス",
        ]

        for key, val in evidence.items():
            lines.append(f"- {key}: {val}")

        return "\n".join(lines)


def run_migration_sync(
    cobol_file: COBOLFile,
    output_root: Path,
    fast_mode: bool = False,
    model: str = "claude-opus-4-6",
) -> PipelineResult:
    """同期版パイプライン実行（CLI用）.

    Args:
        cobol_file: 変換対象COBOLファイル
        output_root: 出力ルートディレクトリ
        fast_mode: 高速モードフラグ
        model: 使用するClaudeモデルID

    Returns:
        PipelineResult
    """
    engine = MigrationEngine(
        output_root=output_root,
        fast_mode=fast_mode,
        model=model,
    )

    events: list[SSEEvent] = []
    final_event: SSEEvent | None = None

    async def _collect_events() -> None:
        nonlocal final_event
        async for event in engine.run_file(cobol_file):
            events.append(event)
            _print_progress(event)
            if event.event_type == "complete":
                final_event = event

    anyio.run(_collect_events)

    decision = "UNKNOWN"
    output_dir = output_root / cobol_file.program_name

    if final_event is not None:
        decision = final_event.data.get("decision", "UNKNOWN")

    return PipelineResult(
        program_name=cobol_file.program_name,
        version=1,
        decision=decision,
        artifacts={},
        output_dir=output_dir,
        java_files=[],
        success=decision in ("PASSED", "KNOWN_LEGACY"),
    )


def _print_progress(event: SSEEvent) -> None:
    """コンソールに進捗を表示する."""
    icon_map = {
        "stage_start": "▶",
        "stage_complete": "✓",
        "evolution": "↺",
        "complete": "✅",
        "error": "✗",
    }
    icon = icon_map.get(event.event_type, "·")
    stage = event.stage or ""
    msg = event.data.get("message", "") or event.data.get("decision", "")
    print(f"  {icon} [{stage}] {msg}")
