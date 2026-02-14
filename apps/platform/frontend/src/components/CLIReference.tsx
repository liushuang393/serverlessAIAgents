/**
 * CLIReference - CLI コマンドリファレンスページ.
 *
 * agentflow CLI と Platform CLI のユーザー向けコマンドを表示。
 * 内部コマンド（protocols 等）は除外。
 */

import { useState } from 'react';

/** CLI コマンド定義 */
interface CLICommand {
  command: string;
  description: string;
  usage: string;
  options?: string[];
}

/** CLI コマンドグループ */
interface CLIGroup {
  id: string;
  title: string;
  icon: string;
  description: string;
  prefix: string;
  commands: CLICommand[];
}

/** agentflow CLI コマンド一覧（ユーザー向けのみ） */
const CLI_GROUPS: CLIGroup[] = [
  {
    id: 'project',
    title: 'プロジェクト管理',
    icon: '📁',
    description: 'プロジェクトの初期化と管理',
    prefix: 'agentflow',
    commands: [
      {
        command: 'init <project-name>',
        description: '新しい AgentFlow プロジェクトを作成',
        usage: 'agentflow init my-agent',
        options: ['--template / -t: テンプレートを指定'],
      },
      {
        command: 'create agent <name>',
        description: '新しいエージェントを作成（対話形式）',
        usage: 'agentflow create agent my-agent',
        options: ['--template: テンプレートを指定', '--description: 説明を設定'],
      },
    ],
  },
  {
    id: 'execution',
    title: 'エージェント実行',
    icon: '▶️',
    description: 'エージェントの実行・チャット・一覧表示',
    prefix: 'agentflow',
    commands: [
      {
        command: 'run <agent-path>',
        description: 'エージェントを実行',
        usage: 'agentflow run my-agent --input \'{"text": "Hello"}\'',
        options: [
          '--input / -i: 入力データ（JSON 文字列またはファイルパス）',
          '--output / -o: 出力ファイルパス',
          '--json: JSON 形式で出力',
          '--agent-name / -n: @agent デコレータ名を指定',
          '--stream / -s: ストリームモード（SSE）',
        ],
      },
      {
        command: 'chat',
        description: '対話型チャットセッションを開始',
        usage: 'agentflow chat --system "あなたはコード専門家です"',
        options: ['--system / -s: システムプロンプト', '--model / -m: LLM モデル（デフォルト: gpt-5.2）'],
      },
      {
        command: 'list',
        description: 'インストール済みエージェントを一覧表示',
        usage: 'agentflow list',
      },
    ],
  },
  {
    id: 'flow',
    title: 'フロー実行',
    icon: '🔄',
    description: 'Workflow YAML を直接実行',
    prefix: 'agentflow',
    commands: [
      {
        command: 'flow run <workflow.yaml>',
        description: 'ワークフロー YAML を実行',
        usage: 'agentflow flow run workflow.yaml --input \'{"key": "value"}\'',
        options: ['--input / -i: 入力データ（JSON）', '--stream / -s: ストリームモード'],
      },
      {
        command: 'flow validate <workflow.yaml>',
        description: 'ワークフロー YAML のバリデーション',
        usage: 'agentflow flow validate workflow.yaml',
      },
    ],
  },
  {
    id: 'skills',
    title: 'スキル管理',
    icon: '🧩',
    description: 'スキルの一覧・詳細・作成・検索',
    prefix: 'agentflow',
    commands: [
      {
        command: 'skills list',
        description: '全スキルを一覧表示',
        usage: 'agentflow skills list',
      },
      {
        command: 'skills show <name>',
        description: 'スキル詳細を表示',
        usage: 'agentflow skills show web-search',
      },
      {
        command: 'skills create',
        description: '新しいスキルを作成（対話形式）',
        usage: 'agentflow skills create',
      },
      {
        command: 'skills search <query>',
        description: 'クエリでスキルを検索',
        usage: 'agentflow skills search "翻訳"',
      },
    ],
  },
  {
    id: 'marketplace',
    title: 'マーケットプレイス',
    icon: '🛒',
    description: 'エージェントの検索・インストール・公開',
    prefix: 'agentflow',
    commands: [
      {
        command: 'marketplace search [query]',
        description: 'マーケットプレイスでエージェントを検索',
        usage: 'agentflow marketplace search "PDF"',
        options: ['--category / -c: カテゴリフィルター', '--protocols / -p: プロトコルフィルター', '--limit / -l: 最大結果数'],
      },
      {
        command: 'marketplace install <agent-id>',
        description: 'エージェントをインストール',
        usage: 'agentflow marketplace install pdf-reader',
      },
    ],
  },
];

/** Platform CLI 追加グループ */
const PLATFORM_CLI_GROUPS: CLIGroup[] = [
  {
    id: 'platform-server',
    title: 'Platform サーバー',
    icon: '🖥️',
    description: 'Platform API サーバーの起動',
    prefix: 'python -m apps.platform.main',
    commands: [
      {
        command: 'serve',
        description: 'Platform API サーバーを起動',
        usage: 'python -m apps.platform.main serve --port 8001',
        options: ['--host: バインドホスト（デフォルト: 0.0.0.0）', '--port: ポート番号（開発: 8001 / 本番: 8000）'],
      },
    ],
  },
  {
    id: 'platform-ops',
    title: 'Platform 操作',
    icon: '🔧',
    description: 'Gallery 検索・コンポーネント管理・発布・ダッシュボード',
    prefix: 'python -m apps.platform.main',
    commands: [
      {
        command: 'search <query>',
        description: 'Gallery からコンポーネントを検索',
        usage: 'python -m apps.platform.main search "PDF"',
        options: ['--limit: 最大結果数（デフォルト: 10）'],
      },
      {
        command: 'publish <source> --target <target>',
        description: 'コンポーネントを発布（docker / vercel / aws_lambda / local / gallery）',
        usage: 'python -m apps.platform.main publish ./my-agent --target docker',
        options: ['--target: 発布先（デフォルト: docker）', '--name: 発布名', '--gallery: Gallery にも登録'],
      },
      {
        command: 'components list',
        description: 'コンポーネント一覧を表示',
        usage: 'python -m apps.platform.main components list --limit 20',
        options: ['--type: タイプフィルター（agent / flow / tool / skill / engine / template）', '--limit: 最大取得数'],
      },
      {
        command: 'dashboard <tenant-id>',
        description: 'テナントのダッシュボード統計を表示',
        usage: 'python -m apps.platform.main dashboard tenant-001',
      },
    ],
  },
  {
    id: 'platform-extra',
    title: 'その他',
    icon: '⚡',
    description: 'テンプレート・サンドボックス・ワークスペース・Studio',
    prefix: 'agentflow',
    commands: [
      {
        command: 'studio',
        description: 'AgentFlow Studio サーバーを起動',
        usage: 'agentflow studio --port 3000',
        options: ['--host / -h: ホスト', '--port / -p: ポート', '--reload: 自動リロード'],
      },
      {
        command: 'template list',
        description: '利用可能なテンプレートを一覧表示',
        usage: 'agentflow template list',
      },
      {
        command: 'template generate <template-name>',
        description: 'テンプレートからプロジェクトを生成',
        usage: 'agentflow template generate simple-qa --output ./my-agent',
      },
      {
        command: 'sandbox create',
        description: 'サンドボックスを作成',
        usage: 'agentflow sandbox create --provider docker',
        options: ['--provider / -p: プロバイダ（docker / microsandbox / e2b）'],
      },
      {
        command: 'sandbox list',
        description: 'サンドボックス一覧を表示',
        usage: 'agentflow sandbox list',
      },
      {
        command: 'workspace create',
        description: 'ワークスペースを作成',
        usage: 'agentflow workspace create --name my-project',
        options: ['--name / -n: ワークスペース名', '--provider / -p: プロバイダ'],
      },
      {
        command: 'workspace list',
        description: 'ワークスペース一覧を表示',
        usage: 'agentflow workspace list',
      },
    ],
  },
];

/** 全グループ統合 */
const ALL_GROUPS = [...CLI_GROUPS, ...PLATFORM_CLI_GROUPS];

/** コマンドをクリップボードにコピー */
async function copyToClipboard(text: string): Promise<void> {
  await navigator.clipboard.writeText(text);
}

/** コマンドカード */
function CommandCard({ cmd, prefix }: Readonly<{ cmd: CLICommand; prefix: string }>) {
  const [copied, setCopied] = useState(false);

  const handleCopy = async () => {
    const fullCommand = prefix ? `${prefix} ${cmd.command}` : cmd.command;
    await copyToClipboard(fullCommand);
    setCopied(true);
    setTimeout(() => setCopied(false), 1500);
  };

  return (
    <div className="bg-slate-800/50 border border-slate-700/50 rounded-lg p-4 hover:border-indigo-500/30 transition-colors">
      <div className="flex items-start justify-between gap-2 mb-2">
        <code className="text-sm text-indigo-300 font-mono break-all">
          {prefix} {cmd.command}
        </code>
        <button
          onClick={handleCopy}
          className="shrink-0 text-xs text-slate-500 hover:text-slate-300 transition-colors"
          title="コマンドをコピー"
        >
          {copied ? '✅' : '📋'}
        </button>
      </div>
      <p className="text-sm text-slate-400 mb-2">{cmd.description}</p>
      <div className="bg-slate-900/60 rounded px-3 py-2 mb-2">
        <code className="text-xs text-emerald-400 font-mono break-all">$ {cmd.usage}</code>
      </div>
      {cmd.options && cmd.options.length > 0 && (
        <div className="mt-2 space-y-1">
          {cmd.options.map((opt) => (
            <p key={opt} className="text-xs text-slate-500">
              <span className="text-slate-400 font-mono">{opt.split(':')[0]}:</span>
              {opt.split(':').slice(1).join(':')}
            </p>
          ))}
        </div>
      )}
    </div>
  );
}

/** CLI リファレンスページ */
export function CLIReference() {
  const [activeGroup, setActiveGroup] = useState<string | null>(null);

  const filteredGroups = activeGroup
    ? ALL_GROUPS.filter((g) => g.id === activeGroup)
    : ALL_GROUPS;

  return (
    <div className="p-6 max-w-6xl mx-auto">
      {/* ヘッダー */}
      <div className="mb-6">
        <h1 className="text-2xl font-bold text-slate-100 mb-1">📖 CLI リファレンス</h1>
        <p className="text-sm text-slate-400">
          AgentFlow CLI と Platform CLI のコマンド一覧です。ターミナルからフレームワーク機能を操作できます。
        </p>
      </div>

      {/* フィルターチップ */}
      <div className="flex flex-wrap gap-2 mb-6">
        <button
          onClick={() => setActiveGroup(null)}
          className={`px-3 py-1.5 text-xs rounded-full transition-colors ${
            activeGroup === null
              ? 'bg-indigo-600 text-white'
              : 'bg-slate-800 text-slate-400 hover:bg-slate-700'
          }`}
        >
          全て
        </button>
        {ALL_GROUPS.map((g) => (
          <button
            key={g.id}
            onClick={() => setActiveGroup(activeGroup === g.id ? null : g.id)}
            className={`px-3 py-1.5 text-xs rounded-full transition-colors ${
              activeGroup === g.id
                ? 'bg-indigo-600 text-white'
                : 'bg-slate-800 text-slate-400 hover:bg-slate-700'
            }`}
          >
            {g.icon} {g.title}
          </button>
        ))}
      </div>

      {/* コマンドグループ */}
      <div className="space-y-8">
        {filteredGroups.map((group) => (
          <section key={group.id}>
            <div className="flex items-center gap-2 mb-3">
              <span className="text-xl">{group.icon}</span>
              <h2 className="text-lg font-semibold text-slate-200">{group.title}</h2>
              <span className="text-xs text-slate-500 bg-slate-800 px-2 py-0.5 rounded-full">
                {group.commands.length} コマンド
              </span>
            </div>
            <p className="text-sm text-slate-400 mb-4">{group.description}</p>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {group.commands.map((cmd) => (
                <CommandCard key={cmd.command} cmd={cmd} prefix={group.prefix} />
              ))}
            </div>
          </section>
        ))}
      </div>

      {/* クイックスタート */}
      <div className="mt-10 bg-slate-800/30 border border-slate-700/50 rounded-lg p-6">
        <h2 className="text-lg font-semibold text-slate-200 mb-3">🚀 クイックスタート</h2>
        <div className="space-y-3 font-mono text-sm">
          <div className="bg-slate-900/60 rounded px-4 py-3">
            <p className="text-slate-500 mb-1"># conda 環境を有効化</p>
            <p className="text-emerald-400">$ conda activate agentflow</p>
          </div>
          <div className="bg-slate-900/60 rounded px-4 py-3">
            <p className="text-slate-500 mb-1"># バックエンド起動（ローカル開発）</p>
            <p className="text-emerald-400">$ python -m apps.platform.main serve --port 8001</p>
          </div>
          <div className="bg-slate-900/60 rounded px-4 py-3">
            <p className="text-slate-500 mb-1"># フロントエンド起動（別ターミナル）</p>
            <p className="text-emerald-400">$ cd apps/platform/frontend &amp;&amp; npm run dev</p>
          </div>
          <div className="bg-slate-900/60 rounded px-4 py-3">
            <p className="text-slate-500 mb-1"># 新しいエージェントを作成して実行</p>
            <p className="text-emerald-400">$ agentflow init my-agent</p>
            <p className="text-emerald-400">$ agentflow run my-agent --input '&#123;"question": "Hello"&#125;'</p>
          </div>
        </div>
      </div>
    </div>
  );
}

