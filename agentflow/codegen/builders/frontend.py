# -*- coding: utf-8 -*-
"""FrontendBuilder - フロントエンドコードビルダー.

React ベースのフロントエンドコードを生成します。
"""

from __future__ import annotations

import json
from typing import TYPE_CHECKING

from agentflow.codegen.builders.base import BaseBuilder

if TYPE_CHECKING:
    from agentflow.core.interfaces import CodeGenOptions, WorkflowDefinition


class FrontendBuilder(BaseBuilder):
    """フロントエンドコードビルダー.

    React + TypeScript アプリケーションを生成します。
    """

    async def build(
        self,
        workflow: "WorkflowDefinition",
        options: "CodeGenOptions",
    ) -> dict[str, str]:
        """フロントエンドコードをビルド."""
        files: dict[str, str] = {}

        # Package files
        files["package.json"] = self._generate_package_json(workflow, options)
        files["tsconfig.json"] = self._generate_tsconfig()
        files["vite.config.ts"] = self._generate_vite_config()

        # Source files
        files["src/main.tsx"] = self._generate_main()
        files["src/App.tsx"] = self._generate_app(workflow, options)
        files["src/index.css"] = self._generate_css()

        # Components
        files["src/components/WorkflowRunner.tsx"] = self._generate_workflow_runner(workflow)
        files["src/components/ResultDisplay.tsx"] = self._generate_result_display()
        files["src/components/InputForm.tsx"] = self._generate_input_form()

        # API client
        files["src/api/client.ts"] = self._generate_api_client(options)

        # Types
        files["src/types/index.ts"] = self._generate_types()

        # HTML
        files["index.html"] = self._generate_html(workflow, options)

        # README
        if options.include_readme:
            files["README.md"] = self._generate_readme(workflow, options)

        return files

    def _generate_package_json(
        self,
        workflow: "WorkflowDefinition",
        options: "CodeGenOptions",
    ) -> str:
        """package.json を生成."""
        package = {
            "name": options.app_name,
            "version": options.version,
            "description": workflow.description,
            "type": "module",
            "scripts": {
                "dev": "vite",
                "build": "tsc && vite build",
                "preview": "vite preview",
                "lint": "eslint . --ext ts,tsx --report-unused-disable-directives --max-warnings 0",
            },
            "dependencies": {
                "react": "^18.2.0",
                "react-dom": "^18.2.0",
            },
            "devDependencies": {
                "@types/react": "^18.2.0",
                "@types/react-dom": "^18.2.0",
                "@vitejs/plugin-react": "^4.0.0",
                "typescript": "^5.0.0",
                "vite": "^5.0.0",
            },
        }
        return json.dumps(package, indent=2)

    def _generate_tsconfig(self) -> str:
        """tsconfig.json を生成."""
        config = {
            "compilerOptions": {
                "target": "ES2020",
                "useDefineForClassFields": True,
                "lib": ["ES2020", "DOM", "DOM.Iterable"],
                "module": "ESNext",
                "skipLibCheck": True,
                "moduleResolution": "bundler",
                "allowImportingTsExtensions": True,
                "resolveJsonModule": True,
                "isolatedModules": True,
                "noEmit": True,
                "jsx": "react-jsx",
                "strict": True,
                "noUnusedLocals": True,
                "noUnusedParameters": True,
                "noFallthroughCasesInSwitch": True,
            },
            "include": ["src"],
            "references": [{"path": "./tsconfig.node.json"}],
        }
        return json.dumps(config, indent=2)

    def _generate_vite_config(self) -> str:
        """vite.config.ts を生成."""
        return '''import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';

export default defineConfig({
  plugins: [react()],
  server: {
    proxy: {
      '/api': {
        target: 'http://localhost:8000',
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\\/api/, ''),
      },
    },
  },
});
'''

    def _generate_main(self) -> str:
        """main.tsx を生成."""
        return '''import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';
import './index.css';

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
'''

    def _generate_app(
        self,
        workflow: "WorkflowDefinition",
        options: "CodeGenOptions",
    ) -> str:
        """App.tsx を生成."""
        return f'''import {{ useState }} from 'react';
import WorkflowRunner from './components/WorkflowRunner';
import ResultDisplay from './components/ResultDisplay';
import InputForm from './components/InputForm';
import type {{ WorkflowResult }} from './types';

function App() {{
  const [input, setInput] = useState<Record<string, unknown>>({{}}); 
  const [result, setResult] = useState<WorkflowResult | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const handleRun = async () => {{
    setLoading(true);
    setError(null);
    try {{
      const response = await fetch('/api/run', {{
        method: 'POST',
        headers: {{ 'Content-Type': 'application/json' }},
        body: JSON.stringify({{ data: input }}),
      }});
      const data = await response.json();
      setResult(data);
    }} catch (e) {{
      setError((e as Error).message);
    }} finally {{
      setLoading(false);
    }}
  }};

  return (
    <div className="app">
      <header className="header">
        <h1>{workflow.name}</h1>
        <p>{workflow.description}</p>
      </header>

      <main className="main">
        <section className="input-section">
          <h2>Input</h2>
          <InputForm value={{input}} onChange={{setInput}} />
          <button onClick={{handleRun}} disabled={{loading}}>
            {{loading ? 'Running...' : 'Run Workflow'}}
          </button>
        </section>

        <section className="result-section">
          <h2>Result</h2>
          {{error && <div className="error">{{error}}</div>}}
          {{result && <ResultDisplay result={{result}} />}}
        </section>
      </main>

      <footer className="footer">
        <p>Generated by AgentFlow Studio v{options.version}</p>
      </footer>
    </div>
  );
}}

export default App;
'''

    def _generate_css(self) -> str:
        """index.css を生成."""
        return '''* {
  box-sizing: border-box;
  margin: 0;
  padding: 0;
}

body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  background: #f5f5f5;
  color: #333;
}

.app {
  max-width: 1200px;
  margin: 0 auto;
  padding: 2rem;
}

.header {
  text-align: center;
  margin-bottom: 2rem;
}

.header h1 {
  font-size: 2rem;
  margin-bottom: 0.5rem;
}

.header p {
  color: #666;
}

.main {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 2rem;
}

.input-section,
.result-section {
  background: white;
  padding: 1.5rem;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

h2 {
  margin-bottom: 1rem;
  font-size: 1.25rem;
}

button {
  background: #0070f3;
  color: white;
  border: none;
  padding: 0.75rem 1.5rem;
  border-radius: 4px;
  cursor: pointer;
  font-size: 1rem;
  margin-top: 1rem;
}

button:hover {
  background: #0060df;
}

button:disabled {
  background: #ccc;
  cursor: not-allowed;
}

.error {
  background: #fee;
  color: #c00;
  padding: 1rem;
  border-radius: 4px;
  margin-bottom: 1rem;
}

.footer {
  text-align: center;
  margin-top: 2rem;
  color: #666;
  font-size: 0.875rem;
}

textarea {
  width: 100%;
  min-height: 200px;
  padding: 1rem;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-family: monospace;
  font-size: 0.875rem;
}

pre {
  background: #f5f5f5;
  padding: 1rem;
  border-radius: 4px;
  overflow: auto;
  font-size: 0.875rem;
}

@media (max-width: 768px) {
  .main {
    grid-template-columns: 1fr;
  }
}
'''

    def _generate_workflow_runner(self, workflow: "WorkflowDefinition") -> str:
        """WorkflowRunner コンポーネントを生成."""
        return '''import { useState, useCallback } from 'react';
import type { WorkflowResult, ExecutionEvent } from '../types';

interface Props {
  onResult: (result: WorkflowResult) => void;
  onEvent?: (event: ExecutionEvent) => void;
}

export default function WorkflowRunner({ onResult, onEvent }: Props) {
  const [input, setInput] = useState('{}');
  const [running, setRunning] = useState(false);

  const handleRun = useCallback(async () => {
    setRunning(true);
    try {
      const response = await fetch('/api/run', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ data: JSON.parse(input) }),
      });
      const result = await response.json();
      onResult(result);
    } catch (e) {
      onResult({ status: 'error', error: (e as Error).message });
    } finally {
      setRunning(false);
    }
  }, [input, onResult]);

  const handleStream = useCallback(async () => {
    setRunning(true);
    try {
      const response = await fetch('/api/stream', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ data: JSON.parse(input) }),
      });

      const reader = response.body?.getReader();
      if (!reader) return;

      const decoder = new TextDecoder();
      while (true) {
        const { done, value } = await reader.read();
        if (done) break;

        const chunk = decoder.decode(value);
        const lines = chunk.split('\\n');
        for (const line of lines) {
          if (line.startsWith('data: ')) {
            try {
              const event = JSON.parse(line.slice(6));
              onEvent?.(event);
              if (event.type === 'complete') {
                onResult({ status: 'success', result: event.data });
              }
            } catch {
              // Invalid JSON
            }
          }
        }
      }
    } catch (e) {
      onResult({ status: 'error', error: (e as Error).message });
    } finally {
      setRunning(false);
    }
  }, [input, onResult, onEvent]);

  return (
    <div className="workflow-runner">
      <textarea
        value={input}
        onChange={(e) => setInput(e.target.value)}
        placeholder="Enter JSON input..."
      />
      <div className="actions">
        <button onClick={handleRun} disabled={running}>
          {running ? 'Running...' : 'Run'}
        </button>
        <button onClick={handleStream} disabled={running}>
          {running ? 'Streaming...' : 'Stream'}
        </button>
      </div>
    </div>
  );
}
'''

    def _generate_result_display(self) -> str:
        """ResultDisplay コンポーネントを生成."""
        return '''import type { WorkflowResult } from '../types';

interface Props {
  result: WorkflowResult;
}

export default function ResultDisplay({ result }: Props) {
  return (
    <div className="result-display">
      <div className={`status status-${result.status}`}>
        {result.status === 'success' ? '✓' : '✗'} {result.status}
      </div>
      {result.error && (
        <div className="error">{result.error}</div>
      )}
      {result.result && (
        <pre>{JSON.stringify(result.result, null, 2)}</pre>
      )}
    </div>
  );
}
'''

    def _generate_input_form(self) -> str:
        """InputForm コンポーネントを生成."""
        return '''import { useState, useCallback } from 'react';

interface Props {
  value: Record<string, unknown>;
  onChange: (value: Record<string, unknown>) => void;
}

export default function InputForm({ value, onChange }: Props) {
  const [text, setText] = useState(JSON.stringify(value, null, 2));
  const [error, setError] = useState<string | null>(null);

  const handleChange = useCallback((newText: string) => {
    setText(newText);
    try {
      const parsed = JSON.parse(newText);
      onChange(parsed);
      setError(null);
    } catch (e) {
      setError((e as Error).message);
    }
  }, [onChange]);

  return (
    <div className="input-form">
      <textarea
        value={text}
        onChange={(e) => handleChange(e.target.value)}
        placeholder="{}"
      />
      {error && <div className="parse-error">{error}</div>}
    </div>
  );
}
'''

    def _generate_api_client(self, options: "CodeGenOptions") -> str:
        """API クライアントを生成."""
        return '''const API_BASE = '/api';

export async function runWorkflow(input: Record<string, unknown>) {
  const response = await fetch(`${API_BASE}/run`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ data: input }),
  });
  return response.json();
}

export async function* streamWorkflow(input: Record<string, unknown>) {
  const response = await fetch(`${API_BASE}/stream`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ data: input }),
  });

  const reader = response.body?.getReader();
  if (!reader) return;

  const decoder = new TextDecoder();
  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    const chunk = decoder.decode(value);
    const lines = chunk.split('\\n');
    for (const line of lines) {
      if (line.startsWith('data: ')) {
        try {
          yield JSON.parse(line.slice(6));
        } catch {
          // Invalid JSON
        }
      }
    }
  }
}

export async function getWorkflowInfo() {
  const response = await fetch(`${API_BASE}/workflow/info`);
  return response.json();
}
'''

    def _generate_types(self) -> str:
        """型定義を生成."""
        return '''export interface WorkflowResult {
  status: 'success' | 'error';
  result?: Record<string, unknown>;
  error?: string;
}

export interface ExecutionEvent {
  type: 'start' | 'progress' | 'node_start' | 'node_complete' | 'complete' | 'error';
  node_id?: string;
  message?: string;
  progress?: number;
  data?: Record<string, unknown>;
}

export interface WorkflowInfo {
  id: string;
  name: string;
  description: string;
  nodes: number;
  edges: number;
}
'''

    def _generate_html(
        self,
        workflow: "WorkflowDefinition",
        options: "CodeGenOptions",
    ) -> str:
        """index.html を生成."""
        return f'''<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <link rel="icon" type="image/svg+xml" href="/vite.svg" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>{workflow.name}</title>
  </head>
  <body>
    <div id="root"></div>
    <script type="module" src="/src/main.tsx"></script>
  </body>
</html>
'''

    def _generate_readme(
        self,
        workflow: "WorkflowDefinition",
        options: "CodeGenOptions",
    ) -> str:
        """README を生成."""
        return f'''# {workflow.name} - Frontend

{workflow.description}

## Setup

```bash
npm install
```

## Development

```bash
npm run dev
```

## Build

```bash
npm run build
```

## Configuration

Edit `vite.config.ts` to change the API proxy target.

---

*Generated by AgentFlow Studio*
'''


__all__ = ["FrontendBuilder"]
