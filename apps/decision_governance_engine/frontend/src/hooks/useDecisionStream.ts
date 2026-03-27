/**
 * SSE ストリーム用カスタムフック.
 *
 * 目的: AG-UI イベントのリアルタイム受信・状態管理
 * 使用場所: 進捗画面（ProcessingPage）
 *
 * 機能:
 *   - 自動再接続（最大3回）
 *   - 接続タイムアウト検出
 *   - 詳細エラーハンドリング
 */

import { useState, useCallback, useRef, useEffect } from "react";
import { decisionApi } from "../api/client";
import type { AGUIEvent, DecisionReport } from "../types";

/** 再接続設定 */
const RECONNECT_CONFIG = {
  maxRetries: 3,
  baseDelay: 2000,
  maxDelay: 10000,
};

/** 接続タイムアウト（30秒） */
const CONNECTION_TIMEOUT = 30000;

/** Ref で最新状態を追跡するためのヘルパー */
const useLatestRef = <T>(value: T) => {
  const ref = useRef(value);
  ref.current = value;
  return ref;
};

/** 開発時ログ（lint no-console 対応） */
const debugLog = (..._args: unknown[]): void => {};

/** Agent 進捗状態 */
export interface AgentProgress {
  id: string;
  name: string;
  label: string;
  status: "waiting" | "running" | "completed" | "failed";
  progress: number;
  message: string;
  result?: Record<string, unknown>;
}

/** LLM思考ログ */
export interface ThinkingLog {
  timestamp: number;
  agentId: string;
  agentName: string;
  content: string;
}

/** ストリーム状態 */
export interface StreamState {
  isConnected: boolean;
  isComplete: boolean;
  error: string | null;
  isRetryable: boolean;
  retryCount: number;
  agents: AgentProgress[];
  report: DecisionReport | null;
  /** 履歴照会・PDF出力用のリクエストID（UUID） */
  requestId: string | null;
  thinkingLogs: ThinkingLog[];
}

/** ストリーム開始オプション */
export interface StreamStartOptions {
  requestId?: string;
  resume?: boolean;
  preserveProgress?: boolean;
}

/** Agent ID → 英語クラス名 マッピング */
const agentClassNameMap: Record<string, string> = {
  cognitive_gate: "CognitiveGateAgent",
  gatekeeper: "GatekeeperAgent",
  clarification: "ClarificationAgent",
  dao: "DaoAgent",
  fa: "FaAgent",
  shu: "ShuAgent",
  qi: "QiAgent",
  review: "ReviewAgent",
};

/** 統一ログ名を生成（例: 門番：GatekeeperAgent） */
const getUnifiedLogName = (agentId: string, japaneseName: string): string => {
  const className = agentClassNameMap[agentId] || agentId;
  return `${japaneseName}：${className}`;
};

/** 初期 Agent 状態（認知前処理・門番・診断・道・法・術・器・検証の8 Agent） */
const initialAgents: AgentProgress[] = [
  {
    id: "cognitive_gate",
    name: "認知",
    label: "認知前処理",
    status: "waiting",
    progress: 0,
    message: "",
  },
  {
    id: "gatekeeper",
    name: "門番",
    label: "入口検証",
    status: "waiting",
    progress: 0,
    message: "",
  },
  {
    id: "clarification",
    name: "診断",
    label: "問題診断",
    status: "waiting",
    progress: 0,
    message: "",
  },
  {
    id: "dao",
    name: "道",
    label: "本質分析",
    status: "waiting",
    progress: 0,
    message: "",
  },
  {
    id: "fa",
    name: "法",
    label: "戦略選定",
    status: "waiting",
    progress: 0,
    message: "",
  },
  {
    id: "shu",
    name: "術",
    label: "実行計画",
    status: "waiting",
    progress: 0,
    message: "",
  },
  {
    id: "qi",
    name: "器",
    label: "技術実装",
    status: "waiting",
    progress: 0,
    message: "",
  },
  {
    id: "review",
    name: "検証",
    label: "最終検証",
    status: "waiting",
    progress: 0,
    message: "",
  },
];

/**
 * Decision SSE ストリームフック.
 */
export function useDecisionStream() {
  const [state, setState] = useState<StreamState>({
    isConnected: false,
    isComplete: false,
    error: null,
    isRetryable: false,
    retryCount: 0,
    agents: [...initialAgents],
    report: null,
    requestId: null,
    thinkingLogs: [],
  });

  // 最新の state を ref で追跡（stale closure 回避）
  const stateRef = useLatestRef(state);

  const eventSourceRef = useRef<EventSource | null>(null);
  const lastReviewVerdictRef = useRef<string | null>(null);
  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
  const lastParamsRef = useRef<{
    question: string;
    budget?: number;
    timeline?: number;
    stakeholders?: {
      product_owner?: string;
      tech_lead?: string;
      business_owner?: string;
      legal_reviewer?: string;
    };
    technicalConstraints?: string[];
    regulatoryConstraints?: string[];
    team?: string;
    streamOptions?: StreamStartOptions;
  } | null>(null);

  /** Agent 状態を更新 */
  const updateAgent = useCallback(
    (agentId: string, updates: Partial<AgentProgress>) => {
      setState((prev) => ({
        ...prev,
        agents: prev.agents.map((a) =>
          a.id === agentId ? { ...a, ...updates } : a,
        ),
      }));
    },
    [],
  );

  /** 思考ログを追加 */
  const addThinkingLog = useCallback(
    (agentId: string, agentName: string, content: string) => {
      setState((prev) => ({
        ...prev,
        thinkingLogs: [
          ...prev.thinkingLogs,
          { timestamp: Date.now(), agentId, agentName, content },
        ],
      }));
    },
    [],
  );

  /** AG-UI イベントハンドラー */
  const handleEvent = useCallback(
    (event: AGUIEvent) => {
      // 統一フォーマット: type → event_type に正規化
      const eventType =
        event.event_type || (event as unknown as { type?: string }).type;
      debugLog(
        "[useDecisionStream] handleEvent 受信:",
        eventType,
        event.node_id || "",
        JSON.stringify(event).slice(0, 200),
      );

      // type フィールドのイベントを処理（PipelineEngine形式）
      if (!event.event_type && (event as unknown as { type?: string }).type) {
        const legacyEvent = event as unknown as {
          type: string;
          data?: Record<string, unknown>;
        };
        switch (legacyEvent.type) {
          case "progress":
            // {type: "progress", data: {stage: "xxx", progress: 10, overall_progress: 87.5}}
            // progress: ステージ内の進捗（10% = 開始、100% = 完了）
            // overall_progress: フロー全体の進捗
            if (legacyEvent.data) {
              const stage = legacyEvent.data.stage as string;
              const stageProgress = legacyEvent.data.progress as number;
              // overall_progress は全体進捗（UI上部の表示用、今後対応可能）
              // const overallProgress = legacyEvent.data.overall_progress as number;
              if (stage && stageProgress !== undefined) {
                // ステージ内進捗のみ更新（status は node.complete で更新）
                updateAgent(stage, {
                  progress: stageProgress,
                  message: `${stageProgress}% 処理中...`,
                });
                addThinkingLog(stage, stage, `ステージ進捗: ${stageProgress}%`);
              }
            }
            return;
          case "result":
            // {type: "result", data: {status: "xxx", results: {...}}}
            if (legacyEvent.data) {
              const status = legacyEvent.data.status as string;
              const results = legacyEvent.data.results as Record<
                string,
                unknown
              >;
              if (status === "rejected") {
                setState((prev) => ({
                  ...prev,
                  isComplete: true,
                  error: "入力が拒否されました。質問を修正してください。",
                }));
                addThinkingLog("system", "System", "❌ 入力が拒否されました");
              } else if (results) {
                // 成功結果を構築
                setState((prev) => ({
                  ...prev,
                  isComplete: true,
                  report: results as unknown as DecisionReport,
                }));
                addThinkingLog("system", "System", "✅ 分析完了");
              }
            }
            return;
          case "gate_rejected":
            // ゲートで拒否された（詳細情報を含む）
            {
              const gateData = legacyEvent.data || {};
              const gateMessage = (gateData.rejection_message as string) || "";
              const gateReason = (gateData.rejection_reason as string) || "";
              const gateSuggest = (gateData.suggested_rephrase as string) || "";
              const gateStage = (gateData.stage as string) || "";

              if (gateMessage) {
                addThinkingLog("system", "System", `⚠️ ${gateMessage}`);
              } else {
                addThinkingLog(
                  "system",
                  "System",
                  `⚠️ ゲートチェック(${gateStage || "gatekeeper"})で処理が停止しました`,
                );
              }
              if (gateReason) {
                addThinkingLog("system", "System", `📋 理由: ${gateReason}`);
              }
              if (gateSuggest) {
                addThinkingLog("system", "System", `💡 提案: ${gateSuggest}`);
              }
            }
            return;
          case "early_return":
            // 早期リターン（Gate拒否のみ。COACH は early_return を発行しない）
            {
              const data = legacyEvent.data || {};
              const rejectionMessage = (data.rejection_message as string) || "";
              const rejectionReason = (data.rejection_reason as string) || "";
              const suggestedRephrase =
                (data.suggested_rephrase as string) || "";
              const category = (data.category as string) || "";

              // 拒否理由をログに表示
              if (rejectionMessage) {
                addThinkingLog("system", "System", `⚠️ ${rejectionMessage}`);
              } else {
                addThinkingLog(
                  "system",
                  "System",
                  "⚠️ この質問は意思決定支援の対象外です。",
                );
                addThinkingLog(
                  "system",
                  "System",
                  "企業の新事業・新製品/サービス投入に関する意思決定課題を入力してください。",
                );
              }
              if (rejectionReason) {
                addThinkingLog(
                  "system",
                  "System",
                  `📋 理由: ${rejectionReason}`,
                );
              }
              if (category) {
                addThinkingLog("system", "System", `📂 カテゴリ: ${category}`);
              }
              if (suggestedRephrase) {
                addThinkingLog(
                  "system",
                  "System",
                  `💡 提案: ${suggestedRephrase}`,
                );
              }

              // Gate拒否のエラーメッセージを構築
              let errorMessage = rejectionMessage;
              if (!errorMessage) {
                errorMessage = "この質問は意思決定支援の対象外です。\n\n";
                if (rejectionReason) {
                  errorMessage += `理由: ${rejectionReason}\n\n`;
                }
                errorMessage += "【対応可能な質問例】\n";
                errorMessage += "• 「SaaS市場に新規参入すべきか？」\n";
                errorMessage += "• 「新製品を来年Q1に投入すべきか？」\n";
                errorMessage += "• 「海外市場への進出タイミングは？」";
              }

              // 状態を設定（エラーとして表示し、再試行可能にする）
              setState((prev) => ({
                ...prev,
                isComplete: true,
                error: errorMessage,
                isRetryable: true,
                agents: prev.agents.map((a) =>
                  a.status === "running"
                    ? {
                        ...a,
                        status: "completed" as const,
                        progress: 100,
                        message: "拒否",
                        result: data,
                      }
                    : a,
                ),
              }));
              eventSourceRef.current?.close();
            }
            return;
          case "review_verdict":
            // Review判定結果
            {
              const data = legacyEvent.data || {};
              const verdict = data.verdict as string;
              lastReviewVerdictRef.current = verdict || null;
              if (verdict === "REVISE") {
                addThinkingLog(
                  "review",
                  "検証：ReviewAgent",
                  `⚠️ 指摘あり - 再分析を開始します`,
                );
              } else if (verdict === "PASS") {
                addThinkingLog("review", "検証：ReviewAgent", `✅ 分析品質OK`);
              } else if (verdict === "COACH") {
                addThinkingLog(
                  "review",
                  "検証：ReviewAgent",
                  `📋 改善指導あり - レポートで確認してください`,
                );
              }
            }
            return;
          case "revise":
            // REVISE時のロールバック処理
            // retry_from はステージインデックス（0=cognitive_gate, 1=gatekeeper, ...）
            // retry_fromより後のAgentの進捗をリセットする
            {
              const data = legacyEvent.data || {};
              const retryFromIdx = data.retry_from as number;
              // インデックスに対応するAgent ID
              addThinkingLog(
                "system",
                "System",
                `🔄 指摘に基づき再分析を実行（ステージ${retryFromIdx}から）`,
              );

              // retry_from以降のAgentをリセット（waitingに戻す）
              setState((prev) => ({
                ...prev,
                agents: prev.agents.map((a, idx) => {
                  if (idx >= retryFromIdx) {
                    // retry_from以降はリセット
                    return {
                      ...a,
                      status: "waiting" as const,
                      progress: 0,
                      message: "",
                    };
                  }
                  return a;
                }),
              }));
            }
            return;
        }
      }

      switch (event.event_type) {
        case "connection.established":
          // 接続確認イベント（サーバーから即座に送信される）
          debugLog("[useDecisionStream] 接続確認イベント受信");
          {
            const reqId = (event.data as Record<string, unknown> | undefined)
              ?.request_id;
            setState((prev) => ({
              ...prev,
              isConnected: true,
              requestId: typeof reqId === "string" ? reqId : prev.requestId,
            }));
          }
          addThinkingLog("system", "System", "🔗 サーバーに接続しました");
          break;

        case "resume.context":
          {
            const data =
              (event.data as Record<string, unknown> | undefined) || {};
            const completedStages = Array.isArray(data.completed_stages)
              ? data.completed_stages.filter(
                  (s): s is string => typeof s === "string",
                )
              : [];
            const resumeFromStage =
              typeof data.resume_from_stage === "string"
                ? data.resume_from_stage
                : null;
            setState((prev) => ({
              ...prev,
              agents: prev.agents.map((agent) =>
                completedStages.includes(agent.id)
                  ? {
                      ...agent,
                      status: "completed" as const,
                      progress: 100,
                      message: "完了（復元）",
                    }
                  : agent,
              ),
            }));
            addThinkingLog(
              "system",
              "System",
              resumeFromStage
                ? `♻️ 前回の続きから再開します（${resumeFromStage} 以降）`
                : "♻️ 前回の続きから再開します",
            );
          }
          break;

        case "flow.start":
          // 接続開始時、最初のagentをrunning状態に
          setState((prev) => ({
            ...prev,
            isConnected: true,
            agents: prev.agents.map((a, i) =>
              i === 0
                ? {
                    ...a,
                    status: "running" as const,
                    progress: 10,
                    message: "処理開始...",
                  }
                : a,
            ),
          }));
          break;

        case "node.start":
          if (event.node_id) {
            const agentStart = initialAgents.find(
              (a) => a.id === event.node_id,
            );
            const logNameStart = agentStart
              ? getUnifiedLogName(event.node_id, agentStart.name)
              : event.node_id;
            updateAgent(event.node_id, {
              status: "running",
              progress: 10,
              message: `${logNameStart} 処理開始...`,
            });
            // ログ追加（統一フォーマット: 日本語名：英語クラス名）
            addThinkingLog(event.node_id, logNameStart, "処理を開始しました");
          }
          break;

        case "progress":
          {
            const nodeId =
              event.node_id || (event.data as Record<string, unknown>)?.node_id;
            const message =
              event.message ||
              (event.data as Record<string, unknown>)?.message ||
              "";
            if (nodeId && event.percentage !== undefined) {
              updateAgent(nodeId as string, {
                progress: event.percentage,
                message: message as string,
              });
              // ログ追加（統一フォーマット）
              if (message) {
                const agentProg = initialAgents.find((a) => a.id === nodeId);
                const logNameProg = agentProg
                  ? getUnifiedLogName(nodeId as string, agentProg.name)
                  : (nodeId as string);
                addThinkingLog(
                  nodeId as string,
                  logNameProg,
                  message as string,
                );
              }
            }
          }
          break;

        case "log":
          // LLM思考ログイベント（統一フォーマット）
          {
            const nodeId = event.node_id || "system";
            const agentLog = initialAgents.find((a) => a.id === nodeId);
            const logNameLog = agentLog
              ? getUnifiedLogName(nodeId, agentLog.name)
              : event.node_name || "System";
            const content =
              event.message ||
              (event.data as Record<string, unknown>)?.content ||
              "";
            if (content) {
              addThinkingLog(nodeId, logNameLog, content as string);
            }
          }
          break;

        case "node.complete":
          if (event.node_id) {
            // 既にfailed状態の場合は上書きしない（error後のcomplete防止）
            const currentAgent = stateRef.current.agents.find(
              (a) => a.id === event.node_id,
            );
            if (currentAgent?.status === "failed") {
              debugLog(
                "[useDecisionStream] node.complete無視（既にfailed）:",
                event.node_id,
              );
              break;
            }
            const agentComplete = initialAgents.find(
              (a) => a.id === event.node_id,
            );
            const logNameComplete = agentComplete
              ? getUnifiedLogName(event.node_id, agentComplete.name)
              : event.node_id;
            updateAgent(event.node_id, {
              status: "completed",
              progress: 100,
              message: "完了",
              result: event.data as Record<string, unknown>,
            });
            // ログ追加（統一フォーマット）
            addThinkingLog(event.node_id, logNameComplete, "✓ 分析完了");
          }
          break;

        case "node.error":
          if (event.node_id) {
            // NodeErrorEvent は error_message フィールドを使用
            const errorMsg =
              event.error_message || event.message || "エラー発生";
            const agentError = initialAgents.find(
              (a) => a.id === event.node_id,
            );
            const logNameError = agentError
              ? getUnifiedLogName(event.node_id, agentError.name)
              : event.node_id;
            updateAgent(event.node_id, {
              status: "failed",
              message: errorMsg,
            });
            // ログ追加（統一フォーマット）
            addThinkingLog(event.node_id, logNameError, `❌ ${errorMsg}`);
          }
          break;

        case "flow.complete":
          setState((prev) => ({
            ...prev,
            isComplete: true,
            report: (event.result as unknown as DecisionReport) || null,
            requestId: prev.requestId,
          }));
          addThinkingLog("system", "System", "✅ 全分析が完了しました");
          eventSourceRef.current?.close();
          break;

        case "flow.error":
          // FlowErrorEvent は error_message フィールドを使用
          setState((prev) => ({
            ...prev,
            error: event.error_message || event.message || "フロー実行エラー",
          }));
          addThinkingLog(
            "system",
            "System",
            `🚨 ${event.error_message || event.message || "フロー実行エラー"}`,
          );
          eventSourceRef.current?.close();
          break;
      }
    },
    [updateAgent, addThinkingLog, stateRef],
  );

  /** タイムアウトをクリア */
  const clearConnectionTimeout = useCallback(() => {
    if (timeoutRef.current) {
      clearTimeout(timeoutRef.current);
      timeoutRef.current = null;
    }
  }, []);

  /** 接続タイムアウト設定 */
  const setConnectionTimeout = useCallback(() => {
    clearConnectionTimeout();
    timeoutRef.current = setTimeout(() => {
      // stateRef を使用して最新の状態を参照（stale closure 回避）
      const currentState = stateRef.current;
      debugLog("[useDecisionStream] タイムアウトチェック:", {
        isConnected: currentState.isConnected,
        isComplete: currentState.isComplete,
      });
      if (!currentState.isConnected && !currentState.isComplete) {
        debugLog("[useDecisionStream] タイムアウト発火 - 接続をクローズ");
        eventSourceRef.current?.close();
        setState((prev) => ({
          ...prev,
          error: "接続がタイムアウトしました。再試行してください。",
          isRetryable: true,
        }));
      }
    }, CONNECTION_TIMEOUT);
  }, [clearConnectionTimeout, stateRef]);

  /** SSE 接続成功ハンドラー */
  const handleOpen = useCallback(() => {
    debugLog("[useDecisionStream] handleOpen - 接続成功！");
    clearConnectionTimeout();
    setState((prev) => {
      debugLog("[useDecisionStream] setState: isConnected = true");
      return {
        ...prev,
        isConnected: true,
        error: null,
        retryCount: 0,
      };
    });
  }, [clearConnectionTimeout]);

  /** SSE 接続エラーハンドラー */
  const handleError = useCallback(
    (errorMessage: string, isRetryable?: boolean) => {
      debugLog("[useDecisionStream] handleError:", errorMessage);
      clearConnectionTimeout();
      setState((prev) => {
        debugLog(
          "[useDecisionStream] setState: isConnected = false, error =",
          errorMessage,
        );
        return {
          ...prev,
          isConnected: false,
          error: errorMessage,
          isRetryable: isRetryable ?? false,
        };
      });
    },
    [clearConnectionTimeout],
  );

  /** 自動再接続 */
  const attemptReconnect = useCallback(() => {
    const params = lastParamsRef.current;
    const currentRetryCount = stateRef.current.retryCount;
    if (!params || currentRetryCount >= RECONNECT_CONFIG.maxRetries) {
      return;
    }

    const delay = Math.min(
      RECONNECT_CONFIG.baseDelay * Math.pow(2, currentRetryCount),
      RECONNECT_CONFIG.maxDelay,
    );

    setTimeout(() => {
      setState((prev) => ({
        ...prev,
        retryCount: prev.retryCount + 1,
        error: null,
      }));

      eventSourceRef.current?.close();
      eventSourceRef.current = decisionApi.streamDecision(
        params.question,
        params.budget,
        params.timeline,
        handleEvent,
        handleError,
        handleOpen,
        params.stakeholders,
        params.technicalConstraints,
        params.regulatoryConstraints,
        params.team,
        params.streamOptions,
      );
      setConnectionTimeout();
    }, delay);
  }, [handleEvent, handleError, handleOpen, setConnectionTimeout, stateRef]);

  /** ストリーム開始 */
  const startStream = useCallback(
    (
      question: string,
      budget?: number,
      timelineMonths?: number,
      stakeholders?: {
        product_owner?: string;
        tech_lead?: string;
        business_owner?: string;
        legal_reviewer?: string;
      },
      technicalConstraints?: string[],
      regulatoryConstraints?: string[],
      team?: string,
      streamOptions?: StreamStartOptions,
    ) => {
      debugLog("🔘 [STEP4] startStream() 開始", {
        question: question?.slice(0, 50),
        budget,
        timelineMonths,
        streamOptions,
        existingConnection: eventSourceRef.current?.readyState,
      });

      // 既に接続中の場合はスキップ（React Strict Mode 対策）
      if (
        eventSourceRef.current &&
        eventSourceRef.current.readyState !== EventSource.CLOSED
      ) {
        debugLog(
          "🔘 [STEP4] ⚠️ 既存接続あり、スキップ readyState=",
          eventSourceRef.current.readyState,
        );
        return;
      }

      // 既存接続をクローズ
      eventSourceRef.current?.close();
      clearConnectionTimeout();

      // パラメータ保存（再接続用）
      lastParamsRef.current = {
        question,
        budget,
        timeline: timelineMonths,
        stakeholders,
        technicalConstraints,
        regulatoryConstraints,
        team,
        streamOptions,
      };
      lastReviewVerdictRef.current = null;

      const preserveProgress = streamOptions?.preserveProgress === true;
      if (preserveProgress) {
        setState((prev) => ({
          ...prev,
          isConnected: false,
          isComplete: false,
          error: null,
          isRetryable: false,
          retryCount: 0,
          requestId: streamOptions?.requestId || prev.requestId,
          agents: prev.agents.map((agent) =>
            agent.status === "failed"
              ? {
                  ...agent,
                  status: "waiting" as const,
                  progress: 0,
                  message: "再開待ち...",
                }
              : agent,
          ),
          thinkingLogs: [
            ...prev.thinkingLogs,
            {
              timestamp: Date.now(),
              agentId: "system",
              agentName: "System",
              content: streamOptions?.requestId
                ? `♻️ request_id=${streamOptions.requestId} で途中再開します...`
                : "♻️ 途中再開します...",
            },
          ],
        }));
      } else {
        // 状態リセット（最初のagentをrunning状態に）
        const startingAgents = initialAgents.map((a, i) =>
          i === 0
            ? {
                ...a,
                status: "running" as const,
                progress: 5,
                message: "接続中...",
              }
            : { ...a },
        );
        setState({
          isConnected: false,
          isComplete: false,
          error: null,
          isRetryable: false,
          retryCount: 0,
          agents: startingAgents,
          report: null,
          requestId: streamOptions?.requestId || null,
          thinkingLogs: [
            {
              timestamp: Date.now(),
              agentId: "system",
              agentName: "System",
              content: "🚀 分析を開始します...",
            },
          ],
        });
      }

      debugLog("🔘 [STEP4] → decisionApi.streamDecision() を呼び出し");

      // SSE 接続開始
      eventSourceRef.current = decisionApi.streamDecision(
        question,
        budget,
        timelineMonths,
        handleEvent,
        handleError,
        handleOpen,
        stakeholders,
        technicalConstraints,
        regulatoryConstraints,
        team,
        streamOptions,
      );

      debugLog(
        "🔘 [STEP4] EventSource 作成完了, readyState=",
        eventSourceRef.current?.readyState,
      );

      // タイムアウト設定
      setConnectionTimeout();
    },
    [
      handleEvent,
      handleError,
      handleOpen,
      clearConnectionTimeout,
      setConnectionTimeout,
    ],
  );

  /** ストリーム停止 */
  const stopStream = useCallback(() => {
    debugLog("[SSE] ストリーム停止");
    clearConnectionTimeout();
    if (eventSourceRef.current) {
      eventSourceRef.current.close();
      eventSourceRef.current = null;
    }
    lastParamsRef.current = null;
    lastReviewVerdictRef.current = null;
    setState((prev) => ({ ...prev, isConnected: false }));
  }, [clearConnectionTimeout]);

  /** クリーンアップ */
  useEffect(() => {
    return () => {
      clearConnectionTimeout();
      eventSourceRef.current?.close();
    };
  }, [clearConnectionTimeout]);

  return {
    ...state,
    startStream,
    stopStream,
    attemptReconnect,
  };
}
