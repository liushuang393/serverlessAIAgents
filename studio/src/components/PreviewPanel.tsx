import { useState, useCallback } from 'react';
import { useWorkflowStore } from '../stores/workflowStore';

/**
 * プレビューパネル - ワークフローのリアルタイム実行とデバッグ
 *
 * 機能:
 * - ワークフロー実行
 * - 入力データ編集
 * - 実行結果表示
 * - ログ表示
 */

interface LogEntry {
  type: 'info' | 'progress' | 'complete' | 'error';
  message?: string;
  node_id?: string;
  agent_type?: string;
  status?: string;
  timestamp: number;
}

interface PreviewResult {
  status: string;
  result: Record<string, unknown> | null;
  logs: LogEntry[];
  duration_ms: number | null;
  error: string | null;
}

export default function PreviewPanel() {
  const { workflow } = useWorkflowStore();

  const [input, setInput] = useState<string>('{\n  \n}');
  const [inputError, setInputError] = useState<string | null>(null);
  const [result, setResult] = useState<PreviewResult | null>(null);
  const [isRunning, setIsRunning] = useState(false);
  const [activeTab, setActiveTab] = useState<'input' | 'output' | 'logs'>('input');
  const [debugMode, setDebugMode] = useState(false);

  const validateJson = useCallback((json: string): boolean => {
    try {
      JSON.parse(json);
      setInputError(null);
      return true;
    } catch (e) {
      setInputError((e as Error).message);
      return false;
    }
  }, []);

  const handleInputChange = useCallback(
    (value: string) => {
      setInput(value);
      validateJson(value);
    },
    [validateJson],
  );

  const handleRun = useCallback(async () => {
    if (!validateJson(input)) {
      return;
    }

    setIsRunning(true);
    setResult(null);
    setActiveTab('output');

    try {
      const response = await fetch('/api/preview/run', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          workflow: {
            id: workflow.id,
            name: workflow.name,
            description: workflow.description,
            nodes: workflow.nodes,
            edges: workflow.edges,
          },
          input_data: JSON.parse(input),
          debug: debugMode,
        }),
      });

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      const data: PreviewResult = await response.json();
      setResult(data);

      if (data.logs && data.logs.length > 0) {
        setActiveTab('logs');
      }
    } catch (error) {
      setResult({
        status: 'error',
        result: null,
        logs: [],
        duration_ms: null,
        error: (error as Error).message,
      });
    } finally {
      setIsRunning(false);
    }
  }, [workflow, input, debugMode, validateJson]);

  const handleStreamRun = useCallback(async () => {
    if (!validateJson(input)) {
      return;
    }

    setIsRunning(true);
    setResult({
      status: 'running',
      result: null,
      logs: [],
      duration_ms: null,
      error: null,
    });
    setActiveTab('logs');

    try {
      const response = await fetch('/api/preview/stream', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          workflow: {
            id: workflow.id,
            name: workflow.name,
            description: workflow.description,
            nodes: workflow.nodes,
            edges: workflow.edges,
          },
          input_data: JSON.parse(input),
          debug: debugMode,
        }),
      });

      const reader = response.body?.getReader();
      if (!reader) {
        throw new Error('Failed to get response reader');
      }

      const decoder = new TextDecoder();
      const logs: LogEntry[] = [];

      while (true) {
        const { done, value } = await reader.read();
        if (done) {
          break;
        }

        const chunk = decoder.decode(value);
        const lines = chunk.split('\n');

        for (const line of lines) {
          if (line.startsWith('data: ')) {
            try {
              const event = JSON.parse(line.slice(6));
              logs.push({
                ...event,
                timestamp: Date.now(),
              });
              setResult((prev) => ({
                ...prev!,
                logs: [...logs],
                result: event.type === 'complete' ? event.result : prev?.result,
                status: event.type === 'complete' ? 'success' : 'running',
              }));
            } catch {
              // Invalid JSON, skip
            }
          }
        }
      }
    } catch (error) {
      setResult((prev) => ({
        ...prev!,
        status: 'error',
        error: (error as Error).message,
      }));
    } finally {
      setIsRunning(false);
    }
  }, [workflow, input, debugMode, validateJson]);

  return (
    <div className="w-80 border-l bg-background flex flex-col">
      {/* ヘッダー */}
      <div className="p-4 border-b">
        <div className="flex items-center justify-between mb-3">
          <h3 className="font-semibold text-sm">Preview</h3>
          <div className="flex gap-1">
            <button
              onClick={handleRun}
              disabled={isRunning || workflow.nodes.length === 0}
              className="px-3 py-1.5 bg-primary text-primary-foreground text-xs rounded-md hover:bg-primary/90 disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-1"
            >
              {isRunning ? (
                <>
                  <span className="animate-spin">⏳</span>
                  Running...
                </>
              ) : (
                <>
                  <span>▶</span>
                  Run
                </>
              )}
            </button>
            <button
              onClick={handleStreamRun}
              disabled={isRunning || workflow.nodes.length === 0}
              className="px-2 py-1.5 bg-secondary text-secondary-foreground text-xs rounded-md hover:bg-secondary/90 disabled:opacity-50 disabled:cursor-not-allowed"
              title="Stream 実行"
            >
              📡
            </button>
          </div>
        </div>

        {/* オプション */}
        <label className="flex items-center gap-2 text-xs text-muted-foreground">
          <input
            type="checkbox"
            checked={debugMode}
            onChange={(e) => setDebugMode(e.target.checked)}
            className="rounded"
          />
          Debug モード（中間結果を表示）
        </label>
      </div>

      {/* タブ */}
      <div className="flex border-b">
        {(['input', 'output', 'logs'] as const).map((tab) => (
          <button
            key={tab}
            onClick={() => setActiveTab(tab)}
            className={`flex-1 px-3 py-2 text-xs font-medium border-b-2 transition-colors ${
              activeTab === tab
                ? 'border-primary text-primary'
                : 'border-transparent text-muted-foreground hover:text-foreground'
            }`}
          >
            {tab === 'input' && '📝 Input'}
            {tab === 'output' && '📊 Output'}
            {tab === 'logs' && `📋 Logs ${result?.logs?.length ? `(${result.logs.length})` : ''}`}
          </button>
        ))}
      </div>

      {/* コンテンツ */}
      <div className="flex-1 overflow-auto p-3">
        {/* Input タブ */}
        {activeTab === 'input' && (
          <div className="space-y-2">
            <label className="text-xs text-muted-foreground">入力データ (JSON)</label>
            <textarea
              value={input}
              onChange={(e) => handleInputChange(e.target.value)}
              className={`w-full h-48 p-2 text-xs font-mono bg-muted rounded-md resize-none focus:outline-none focus:ring-2 ${
                inputError ? 'ring-2 ring-destructive' : 'focus:ring-primary'
              }`}
              placeholder='{\n  "key": "value"\n}'
            />
            {inputError && <p className="text-xs text-destructive">{inputError}</p>}
          </div>
        )}

        {/* Output タブ */}
        {activeTab === 'output' && (
          <div className="space-y-3">
            {result ? (
              <>
                {/* ステータス */}
                <div className="flex items-center gap-2 text-xs">
                  <span
                    className={`px-2 py-0.5 rounded-full ${
                      result.status === 'success'
                        ? 'bg-green-100 text-green-700'
                        : result.status === 'error'
                          ? 'bg-red-100 text-red-700'
                          : 'bg-yellow-100 text-yellow-700'
                    }`}
                  >
                    {result.status}
                  </span>
                  {result.duration_ms && (
                    <span className="text-muted-foreground">{result.duration_ms.toFixed(1)}ms</span>
                  )}
                </div>

                {/* 結果 */}
                {result.result && (
                  <div>
                    <label className="text-xs text-muted-foreground mb-1 block">実行結果</label>
                    <pre className="p-2 bg-muted rounded-md text-xs font-mono overflow-auto max-h-64">
                      {JSON.stringify(result.result, null, 2)}
                    </pre>
                  </div>
                )}

                {/* エラー */}
                {result.error && (
                  <div className="p-2 bg-destructive/10 border border-destructive/20 rounded-md">
                    <p className="text-xs text-destructive">{result.error}</p>
                  </div>
                )}
              </>
            ) : (
              <p className="text-xs text-muted-foreground text-center py-8">
                Run をクリックして実行結果を確認
              </p>
            )}
          </div>
        )}

        {/* Logs タブ */}
        {activeTab === 'logs' && (
          <div className="space-y-2">
            {result?.logs && result.logs.length > 0 ? (
              result.logs.map((log, index) => (
                <div
                  key={index}
                  className={`p-2 rounded-md text-xs ${
                    log.type === 'error'
                      ? 'bg-destructive/10 text-destructive'
                      : log.type === 'complete'
                        ? 'bg-green-50 text-green-700'
                        : log.type === 'progress'
                          ? 'bg-blue-50 text-blue-700'
                          : 'bg-muted'
                  }`}
                >
                  <div className="flex items-center gap-2">
                    <span>
                      {log.type === 'error' && '❌'}
                      {log.type === 'complete' && '✅'}
                      {log.type === 'progress' && '⏳'}
                      {log.type === 'info' && 'ℹ️'}
                    </span>
                    {log.node_id && (
                      <span className="font-mono text-xs bg-background px-1 rounded">
                        {log.node_id}
                      </span>
                    )}
                    {log.agent_type && (
                      <span className="text-muted-foreground">{log.agent_type}</span>
                    )}
                  </div>
                  {log.message && <p className="mt-1">{log.message}</p>}
                  {log.status && <p className="mt-1 text-muted-foreground">Status: {log.status}</p>}
                </div>
              ))
            ) : (
              <p className="text-xs text-muted-foreground text-center py-8">実行ログがありません</p>
            )}
          </div>
        )}
      </div>

      {/* フッター */}
      <div className="p-3 border-t bg-muted/30">
        <p className="text-xs text-muted-foreground">
          {workflow.nodes.length} ノード · {workflow.edges.length} エッジ
        </p>
      </div>
    </div>
  );
}
