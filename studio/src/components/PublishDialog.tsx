import { useState, useEffect, useCallback } from 'react';
import { useWorkflowStore } from '../stores/workflowStore';

/**
 * 発行ダイアログ - ワークフローのエクスポートとデプロイ
 *
 * v0.4.0: 動的設定フォームをサポート
 *
 * 機能:
 * - コードエクスポート（Frontend, Backend, Fullstack）
 * - 直接デプロイ（Vercel, Docker, AWS Lambda）
 * - 生成コードのプレビュー
 * - 動的設定フォーム
 */

interface PublishDialogProps {
  open: boolean;
  onClose: () => void;
}

interface OutputType {
  id: string;
  name: string;
  description: string;
  icon: string;
}

interface DeployTarget {
  id: string;
  name: string;
  description: string;
  icon: string;
  supports_direct_deploy: boolean;
}

interface ConfigField {
  name: string;
  label: string;
  type: 'string' | 'password' | 'select' | 'boolean' | 'number' | 'textarea';
  required: boolean;
  default?: unknown;
  options?: string[];
  description: string;
  placeholder?: string;
  group: string;
}

interface PreviewFile {
  content: string;
  lines: number;
  size: number;
}

interface TargetsResponse {
  output_types: OutputType[];
  deploy_targets: DeployTarget[];
}

export default function PublishDialog({ open, onClose }: PublishDialogProps) {
  const { workflow } = useWorkflowStore();

  // Output type state
  const [outputTypes, setOutputTypes] = useState<OutputType[]>([]);
  const [selectedOutputType, setSelectedOutputType] = useState<string>('backend');

  // Deploy target state
  const [deployTargets, setDeployTargets] = useState<DeployTarget[]>([]);
  const [selectedDeployTarget, setSelectedDeployTarget] = useState<string>('vercel');

  // Config fields state
  const [configFields, setConfigFields] = useState<ConfigField[]>([]);
  const [configValues, setConfigValues] = useState<Record<string, unknown>>({});

  // Common options
  const [appName, setAppName] = useState('');
  const [version, setVersion] = useState('1.0.0');
  const [includeTests, setIncludeTests] = useState(true);
  const [includeReadme, setIncludeReadme] = useState(true);

  // Preview state
  const [previewFiles, setPreviewFiles] = useState<Record<string, PreviewFile> | null>(null);
  const [previewLoading, setPreviewLoading] = useState(false);
  const [activePreviewFile, setActivePreviewFile] = useState<string | null>(null);

  // Export/Deploy state
  const [exporting, setExporting] = useState(false);
  const [deploying, setDeploying] = useState(false);
  const [deployLogs, setDeployLogs] = useState<string[]>([]);
  const [deployResult, setDeployResult] = useState<{
    status: string;
    url?: string;
    error?: string;
  } | null>(null);

  // Active tab
  const [activeTab, setActiveTab] = useState<'export' | 'deploy'>('export');

  // Fetch targets on open
  useEffect(() => {
    if (open) {
      fetch('/api/publish/targets')
        .then((res) => res.json())
        .then((data: TargetsResponse) => {
          setOutputTypes(data.output_types || []);
          setDeployTargets(data.deploy_targets || []);
        })
        .catch(console.error);
    }
  }, [open]);

  // Set default app name
  useEffect(() => {
    if (workflow.name && !appName) {
      setAppName(workflow.name.toLowerCase().replace(/\s+/g, '-'));
    }
  }, [workflow.name, appName]);

  // Fetch config fields when deploy target changes
  useEffect(() => {
    if (selectedDeployTarget && activeTab === 'deploy') {
      fetch(`/api/publish/config-fields/${selectedDeployTarget}`)
        .then((res) => res.json())
        .then((fields: ConfigField[]) => {
          setConfigFields(fields);
          // Set default values
          const defaults: Record<string, unknown> = {};
          fields.forEach((field) => {
            if (field.default !== undefined) {
              defaults[field.name] = field.default;
            }
          });
          setConfigValues((prev) => ({ ...defaults, ...prev }));
        })
        .catch(console.error);
    }
  }, [selectedDeployTarget, activeTab]);

  // Render config field
  const renderConfigField = (field: ConfigField) => {
    const value = configValues[field.name];

    switch (field.type) {
      case 'password':
        return (
          <input
            type="password"
            value={(value as string) || ''}
            onChange={(e) =>
              setConfigValues((prev) => ({ ...prev, [field.name]: e.target.value }))
            }
            className="w-full px-3 py-2 border rounded-md text-sm"
            placeholder={field.placeholder || '••••••••'}
          />
        );
      case 'select':
        return (
          <select
            value={String((value as string) || field.default || '')}
            onChange={(e) =>
              setConfigValues((prev) => ({ ...prev, [field.name]: e.target.value }))
            }
            className="w-full px-3 py-2 border rounded-md text-sm"
          >
            {field.options?.map((opt) => (
              <option key={opt} value={opt}>
                {opt}
              </option>
            ))}
          </select>
        );
      case 'boolean':
        return (
          <label className="flex items-center gap-2">
            <input
              type="checkbox"
              checked={Boolean(value)}
              onChange={(e) =>
                setConfigValues((prev) => ({ ...prev, [field.name]: e.target.checked }))
              }
              className="rounded"
            />
            <span className="text-sm">{field.description}</span>
          </label>
        );
      case 'number':
        return (
          <input
            type="number"
            value={Number((value as number) || field.default || 0)}
            onChange={(e) =>
              setConfigValues((prev) => ({ ...prev, [field.name]: parseInt(e.target.value) }))
            }
            className="w-full px-3 py-2 border rounded-md text-sm"
            placeholder={field.placeholder}
          />
        );
      case 'textarea':
        return (
          <textarea
            value={(value as string) || ''}
            onChange={(e) =>
              setConfigValues((prev) => ({ ...prev, [field.name]: e.target.value }))
            }
            className="w-full px-3 py-2 border rounded-md text-sm min-h-20"
            placeholder={field.placeholder}
          />
        );
      default:
        return (
          <input
            type="text"
            value={(value as string) || ''}
            onChange={(e) =>
              setConfigValues((prev) => ({ ...prev, [field.name]: e.target.value }))
            }
            className="w-full px-3 py-2 border rounded-md text-sm"
            placeholder={field.placeholder}
          />
        );
    }
  };

  // Group config fields
  const groupedFields = configFields.reduce((acc, field) => {
    const group = field.group || 'general';
    if (!acc[group]) {
acc[group] = [];
}
    acc[group].push(field);
    return acc;
  }, {} as Record<string, ConfigField[]>);

  // Preview code
  const handlePreview = useCallback(async () => {
    setPreviewLoading(true);
    setPreviewFiles(null);

    try {
      const response = await fetch('/api/publish/preview', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          workflow: {
            id: workflow.id,
            name: workflow.name,
            description: workflow.description,
            nodes: workflow.nodes,
            edges: workflow.edges,
          },
          target: selectedOutputType,
          app_name: appName,
          version,
          include_tests: includeTests,
          include_readme: includeReadme,
        }),
      });

      const data = await response.json();
      if (data.status === 'success') {
        setPreviewFiles(data.files);
        const firstFile = Object.keys(data.files)[0];
        setActivePreviewFile(firstFile || null);
      }
    } catch (error) {
      console.error('Preview failed:', error);
    } finally {
      setPreviewLoading(false);
    }
  }, [workflow, selectedOutputType, appName, version, includeTests, includeReadme]);

  // Export (download)
  const handleExport = useCallback(async () => {
    setExporting(true);

    try {
      const response = await fetch('/api/publish/export', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          workflow: {
            id: workflow.id,
            name: workflow.name,
            description: workflow.description,
            nodes: workflow.nodes,
            edges: workflow.edges,
          },
          target: selectedOutputType,
          app_name: appName,
          version,
          include_tests: includeTests,
          include_readme: includeReadme,
        }),
      });

      if (!response.ok) {
        throw new Error('Export failed');
      }

      const blob = await response.blob();
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `${appName || 'workflow'}-${selectedOutputType}.zip`;
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    } catch (error) {
      console.error('Export failed:', error);
      alert('エクスポートに失敗しました');
    } finally {
      setExporting(false);
    }
  }, [workflow, selectedOutputType, appName, version, includeTests, includeReadme]);

  // Deploy with streaming
  const handleDeploy = useCallback(async () => {
    setDeploying(true);
    setDeployResult(null);
    setDeployLogs([]);

    try {
      const response = await fetch('/api/publish/deploy/stream', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          workflow: {
            id: workflow.id,
            name: workflow.name,
            description: workflow.description,
            nodes: workflow.nodes,
            edges: workflow.edges,
          },
          target: selectedDeployTarget,
          app_name: appName || configValues.project_name,
          credentials: configValues,
        }),
      });

      const reader = response.body?.getReader();
      if (!reader) {
return;
}

      const decoder = new TextDecoder();
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
              setDeployLogs((prev) => [...prev, event.message]);

              if (event.type === 'success') {
                setDeployResult({
                  status: 'success',
                  url: event.data?.url,
                });
              } else if (event.type === 'error') {
                setDeployResult({
                  status: 'error',
                  error: event.message,
                });
              }
            } catch {
              // Invalid JSON
            }
          }
        }
      }
    } catch (error) {
      setDeployResult({
        status: 'error',
        error: (error as Error).message,
      });
    } finally {
      setDeploying(false);
    }
  }, [workflow, selectedDeployTarget, appName, configValues]);

  if (!open) {
return null;
}

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center">
      {/* Overlay */}
      <div className="absolute inset-0 bg-black/50" onClick={onClose} />

      {/* Dialog */}
      <div className="relative bg-background rounded-lg shadow-xl w-[900px] max-h-[85vh] flex flex-col">
        {/* Header */}
        <div className="p-4 border-b flex items-center justify-between">
          <div>
            <h2 className="text-lg font-semibold">ワークフローを発行</h2>
            <p className="text-sm text-muted-foreground">
              {workflow.name} をエクスポートまたはデプロイ
            </p>
          </div>
          <button onClick={onClose} className="p-2 hover:bg-muted rounded-md">
            ✕
          </button>
        </div>

        {/* Tabs */}
        <div className="flex border-b">
          <button
            onClick={() => setActiveTab('export')}
            className={`px-4 py-2 text-sm font-medium border-b-2 transition-colors ${
              activeTab === 'export'
                ? 'border-primary text-primary'
                : 'border-transparent hover:text-foreground'
            }`}
          >
            📦 エクスポート
          </button>
          <button
            onClick={() => setActiveTab('deploy')}
            className={`px-4 py-2 text-sm font-medium border-b-2 transition-colors ${
              activeTab === 'deploy'
                ? 'border-primary text-primary'
                : 'border-transparent hover:text-foreground'
            }`}
          >
            🚀 デプロイ
          </button>
        </div>

        {/* Content */}
        <div className="flex-1 overflow-auto p-4">
          <div className="grid grid-cols-2 gap-6">
            {/* Left Column: Settings */}
            <div className="space-y-4">
              {activeTab === 'export' ? (
                <>
                  {/* Output Type Selection */}
                  <div>
                    <label className="text-sm font-medium mb-2 block">出力タイプ</label>
                    <div className="grid grid-cols-1 gap-2">
                      {outputTypes.map((type) => (
                        <button
                          key={type.id}
                          onClick={() => setSelectedOutputType(type.id)}
                          className={`p-3 border rounded-lg text-left transition-colors ${
                            selectedOutputType === type.id
                              ? 'border-primary bg-primary/5'
                              : 'hover:border-muted-foreground/50'
                          }`}
                        >
                          <div className="flex items-center gap-2">
                            <span className="text-lg">{type.icon}</span>
                            <span className="font-medium text-sm">{type.name}</span>
                          </div>
                          <p className="text-xs text-muted-foreground mt-1">
                            {type.description}
                          </p>
                        </button>
                      ))}
                    </div>
                  </div>

                  {/* App Name */}
                  <div>
                    <label className="text-sm font-medium mb-1 block">
                      アプリケーション名
                    </label>
                    <input
                      type="text"
                      value={appName}
                      onChange={(e) => setAppName(e.target.value)}
                      className="w-full px-3 py-2 border rounded-md text-sm"
                      placeholder="my-workflow"
                    />
                  </div>

                  {/* Version */}
                  <div>
                    <label className="text-sm font-medium mb-1 block">バージョン</label>
                    <input
                      type="text"
                      value={version}
                      onChange={(e) => setVersion(e.target.value)}
                      className="w-full px-3 py-2 border rounded-md text-sm"
                      placeholder="1.0.0"
                    />
                  </div>

                  {/* Options */}
                  <div className="space-y-2">
                    <label className="flex items-center gap-2 text-sm">
                      <input
                        type="checkbox"
                        checked={includeTests}
                        onChange={(e) => setIncludeTests(e.target.checked)}
                        className="rounded"
                      />
                      テストコードを含める
                    </label>
                    <label className="flex items-center gap-2 text-sm">
                      <input
                        type="checkbox"
                        checked={includeReadme}
                        onChange={(e) => setIncludeReadme(e.target.checked)}
                        className="rounded"
                      />
                      README を含める
                    </label>
                  </div>
                </>
              ) : (
                <>
                  {/* Deploy Target Selection */}
                  <div>
                    <label className="text-sm font-medium mb-2 block">
                      デプロイ先
                    </label>
                    <div className="grid grid-cols-2 gap-2">
                      {deployTargets
                        .filter((t) => t.supports_direct_deploy)
                        .map((target) => (
                          <button
                            key={target.id}
                            onClick={() => setSelectedDeployTarget(target.id)}
                            className={`p-3 border rounded-lg text-left transition-colors ${
                              selectedDeployTarget === target.id
                                ? 'border-primary bg-primary/5'
                                : 'hover:border-muted-foreground/50'
                            }`}
                          >
                            <div className="flex items-center gap-2">
                              <span className="text-lg">{target.icon}</span>
                              <span className="font-medium text-sm">{target.name}</span>
                            </div>
                            <p className="text-xs text-muted-foreground mt-1">
                              {target.description}
                            </p>
                          </button>
                        ))}
                    </div>
                  </div>

                  {/* Dynamic Config Fields */}
                  {Object.entries(groupedFields).map(([group, fields]) => (
                    <div key={group} className="space-y-3">
                      <h4 className="text-sm font-medium capitalize">
                        {group === 'credentials' ? '🔐 認証情報' : '⚙️ 設定'}
                      </h4>
                      {fields.map((field) => (
                        <div key={field.name}>
                          <label className="text-sm text-muted-foreground mb-1 block">
                            {field.label}
                            {field.required && (
                              <span className="text-destructive ml-1">*</span>
                            )}
                          </label>
                          {renderConfigField(field)}
                          {field.description && field.type !== 'boolean' && (
                            <p className="text-xs text-muted-foreground mt-1">
                              {field.description}
                            </p>
                          )}
                        </div>
                      ))}
                    </div>
                  ))}

                  {/* Deploy Logs */}
                  {deployLogs.length > 0 && (
                    <div className="p-3 bg-muted/50 rounded-lg max-h-40 overflow-auto">
                      <h4 className="text-xs font-medium mb-2">デプロイログ</h4>
                      {deployLogs.map((log, i) => (
                        <p key={i} className="text-xs font-mono">
                          {log}
                        </p>
                      ))}
                    </div>
                  )}

                  {/* Deploy Result */}
                  {deployResult && (
                    <div
                      className={`p-3 rounded-md text-sm ${
                        deployResult.status === 'error'
                          ? 'bg-destructive/10 text-destructive'
                          : 'bg-green-50 text-green-700'
                      }`}
                    >
                      <p className="font-medium">
                        {deployResult.status === 'error'
                          ? '❌ デプロイ失敗'
                          : '✅ デプロイ完了'}
                      </p>
                      {deployResult.url && (
                        <a
                          href={deployResult.url}
                          target="_blank"
                          rel="noopener noreferrer"
                          className="text-primary hover:underline block mt-1"
                        >
                          {deployResult.url}
                        </a>
                      )}
                      {deployResult.error && (
                        <p className="mt-1">{deployResult.error}</p>
                      )}
                    </div>
                  )}
                </>
              )}
            </div>

            {/* Right Column: Preview */}
            <div className="space-y-3">
              <div className="flex items-center justify-between">
                <h3 className="font-medium text-sm">生成コードプレビュー</h3>
                <button
                  onClick={handlePreview}
                  disabled={previewLoading}
                  className="px-3 py-1.5 bg-secondary text-secondary-foreground rounded-md text-xs hover:bg-secondary/90 disabled:opacity-50"
                >
                  {previewLoading ? '読み込み中...' : 'プレビュー更新'}
                </button>
              </div>

              {previewFiles ? (
                <div className="border rounded-lg overflow-hidden">
                  {/* File tabs */}
                  <div className="flex overflow-x-auto bg-muted/50 border-b">
                    {Object.keys(previewFiles).map((filename) => (
                      <button
                        key={filename}
                        onClick={() => setActivePreviewFile(filename)}
                        className={`px-3 py-2 text-xs whitespace-nowrap border-b-2 transition-colors ${
                          activePreviewFile === filename
                            ? 'border-primary bg-background'
                            : 'border-transparent hover:bg-muted'
                        }`}
                      >
                        {filename}
                      </button>
                    ))}
                  </div>

                  {/* Code display */}
                  {activePreviewFile && previewFiles[activePreviewFile] && (
                    <div className="p-3 bg-background">
                      <div className="flex items-center justify-between text-xs text-muted-foreground mb-2">
                        <span>{previewFiles[activePreviewFile].lines} 行</span>
                        <span>
                          {(previewFiles[activePreviewFile].size / 1024).toFixed(1)} KB
                        </span>
                      </div>
                      <pre className="text-xs font-mono bg-muted p-3 rounded-md overflow-auto max-h-64">
                        {previewFiles[activePreviewFile].content}
                      </pre>
                    </div>
                  )}
                </div>
              ) : (
                <div className="border rounded-lg p-8 text-center text-muted-foreground">
                  <p className="text-sm">
                    「プレビュー更新」をクリックして
                    <br />
                    生成されるコードを確認
                  </p>
                </div>
              )}
            </div>
          </div>
        </div>

        {/* Footer */}
        <div className="p-4 border-t flex justify-end gap-3">
          <button
            onClick={onClose}
            className="px-4 py-2 border rounded-md text-sm hover:bg-muted"
          >
            キャンセル
          </button>
          {activeTab === 'export' ? (
            <button
              onClick={handleExport}
              disabled={exporting || workflow.nodes.length === 0}
              className="px-4 py-2 bg-primary text-primary-foreground rounded-md text-sm hover:bg-primary/90 disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-2"
            >
              {exporting ? (
                <>
                  <span className="animate-spin">⏳</span>
                  エクスポート中...
                </>
              ) : (
                <>
                  <span>📦</span>
                  ZIP ダウンロード
                </>
              )}
            </button>
          ) : (
            <button
              onClick={handleDeploy}
              disabled={deploying || workflow.nodes.length === 0}
              className="px-4 py-2 bg-primary text-primary-foreground rounded-md text-sm hover:bg-primary/90 disabled:opacity-50 disabled:cursor-not-allowed flex items-center gap-2"
            >
              {deploying ? (
                <>
                  <span className="animate-spin">⏳</span>
                  デプロイ中...
                </>
              ) : (
                <>
                  <span>🚀</span>
                  デプロイ
                </>
              )}
            </button>
          )}
        </div>
      </div>
    </div>
  );
}
