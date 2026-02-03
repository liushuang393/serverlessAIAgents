import { useState } from 'react';
import {
  FolderOpen,
  Search,
  Copy,
  FileText,
  Image,
  Film,
  Music,
  Archive,
  Code,
  Folder,
  File,
  Loader2,
  AlertTriangle,
  CheckCircle,
  Play,
} from 'lucide-react';
import clsx from 'clsx';

interface DirectoryAnalysis {
  path: string;
  total_files: number;
  total_dirs: number;
  total_size_mb: number;
  by_category: Record<string, { count: number; size: number }>;
  by_extension: Record<string, number>;
  old_files_count: number;
  large_files_count: number;
  recommendations: string[];
}

interface OrganizationResult {
  files_moved: number;
  files_renamed: number;
  dirs_created: number;
  total_actions: number;
  dry_run: boolean;
  errors: string[];
}

interface DuplicateGroup {
  hash: string;
  size_mb: number;
  files: string[];
  duplicate_count: number;
  potential_savings_mb: number;
}

const categoryIcons: Record<string, typeof FileText> = {
  documents: FileText,
  images: Image,
  videos: Film,
  audio: Music,
  archives: Archive,
  code: Code,
  folders: Folder,
  others: File,
};

const categoryColors: Record<string, string> = {
  documents: 'bg-blue-100 text-blue-700',
  images: 'bg-green-100 text-green-700',
  videos: 'bg-purple-100 text-purple-700',
  audio: 'bg-pink-100 text-pink-700',
  archives: 'bg-yellow-100 text-yellow-700',
  code: 'bg-indigo-100 text-indigo-700',
  folders: 'bg-gray-100 text-gray-700',
  others: 'bg-gray-100 text-gray-600',
};

export default function FileOrganizer() {
  const [path, setPath] = useState('~/Downloads');
  const [analysis, setAnalysis] = useState<DirectoryAnalysis | null>(null);
  const [duplicates, setDuplicates] = useState<DuplicateGroup[]>([]);
  const [organizeResult, setOrganizeResult] = useState<OrganizationResult | null>(null);
  const [loading, setLoading] = useState(false);
  const [activeTab, setActiveTab] = useState<'analyze' | 'duplicates' | 'organize'>('analyze');
  const [daysOld, setDaysOld] = useState(30);

  const handleAnalyze = async () => {
    setLoading(true);
    setAnalysis(null);
    try {
      const response = await fetch('/api/file-organizer/analyze', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ path, days_old: daysOld }),
      });
      if (response.ok) {
        setAnalysis(await response.json());
      }
    } catch (error) {
      console.error('Analyze error:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleFindDuplicates = async () => {
    setLoading(true);
    setDuplicates([]);
    try {
      const response = await fetch('/api/file-organizer/duplicates', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ path }),
      });
      if (response.ok) {
        const data = await response.json();
        setDuplicates(data.duplicates || []);
      }
    } catch (error) {
      console.error('Duplicates error:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleOrganize = async (dryRun: boolean) => {
    setLoading(true);
    setOrganizeResult(null);
    try {
      const response = await fetch('/api/file-organizer/organize', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ path, dry_run: dryRun }),
      });
      if (response.ok) {
        setOrganizeResult(await response.json());
      }
    } catch (error) {
      console.error('Organize error:', error);
    } finally {
      setLoading(false);
    }
  };

  const renderCategoryCard = (category: string, data: { count: number; size: number }) => {
    const Icon = categoryIcons[category] || File;
    const colorClass = categoryColors[category] || categoryColors.others;
    return (
      <div key={category} className={clsx('rounded-lg p-4', colorClass)}>
        <div className="flex items-center gap-3">
          <Icon size={24} />
          <div>
            <p className="font-medium capitalize">{category}</p>
            <p className="text-sm opacity-75">
              {data.count} files - {(data.size / 1024 / 1024).toFixed(1)} MB
            </p>
          </div>
        </div>
      </div>
    );
  };

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-gray-900">File Organizer</h1>
        <p className="text-gray-600 mt-1">Analyze and organize directories</p>
      </div>

      <div className="bg-white rounded-lg shadow p-4">
        <div className="flex items-center gap-4">
          <FolderOpen className="text-gray-400" size={24} />
          <input
            type="text"
            value={path}
            onChange={(e) => setPath(e.target.value)}
            className="flex-1 border rounded-lg px-4 py-2"
            placeholder="Directory path (e.g. ~/Downloads)"
          />
          <div className="flex items-center gap-2">
            <label className="text-sm text-gray-600">Old files:</label>
            <input
              type="number"
              value={daysOld}
              onChange={(e) => setDaysOld(Number(e.target.value))}
              className="w-20 border rounded-lg px-2 py-2"
              min={1}
            />
            <span className="text-sm text-gray-600">days+</span>
          </div>
          <button
            onClick={handleAnalyze}
            disabled={loading || !path}
            className="flex items-center gap-2 px-4 py-2 bg-primary-500 text-white rounded-lg hover:bg-primary-600 disabled:opacity-50"
          >
            {loading ? <Loader2 className="animate-spin" size={18} /> : <Search size={18} />}
            Analyze
          </button>
        </div>
      </div>

      <div className="border-b border-gray-200">
        <div className="flex gap-4">
          {['analyze', 'duplicates', 'organize'].map((tab) => (
            <button
              key={tab}
              onClick={() => {
                setActiveTab(tab as typeof activeTab);
                if (tab === 'duplicates') handleFindDuplicates();
              }}
              className={clsx(
                'px-4 py-2 font-medium border-b-2 transition-colors capitalize',
                activeTab === tab
                  ? 'border-primary-500 text-primary-600'
                  : 'border-transparent text-gray-500 hover:text-gray-700'
              )}
            >
              {tab === 'analyze' && <Search size={18} className="inline mr-2" />}
              {tab === 'duplicates' && <Copy size={18} className="inline mr-2" />}
              {tab === 'organize' && <FolderOpen size={18} className="inline mr-2" />}
              {tab}
            </button>
          ))}
        </div>
      </div>

      {activeTab === 'analyze' && analysis && (
        <div className="space-y-6">
          <div className="grid grid-cols-4 gap-4">
            <div className="bg-white rounded-lg shadow p-4">
              <p className="text-sm text-gray-500">Total Files</p>
              <p className="text-2xl font-bold">{analysis.total_files}</p>
            </div>
            <div className="bg-white rounded-lg shadow p-4">
              <p className="text-sm text-gray-500">Total Size</p>
              <p className="text-2xl font-bold">{analysis.total_size_mb.toFixed(1)} MB</p>
            </div>
            <div className="bg-white rounded-lg shadow p-4">
              <p className="text-sm text-gray-500">Old Files</p>
              <p className="text-2xl font-bold text-orange-600">{analysis.old_files_count}</p>
            </div>
            <div className="bg-white rounded-lg shadow p-4">
              <p className="text-sm text-gray-500">Large Files</p>
              <p className="text-2xl font-bold text-purple-600">{analysis.large_files_count}</p>
            </div>
          </div>

          <div className="bg-white rounded-lg shadow p-4">
            <h3 className="font-medium mb-4">By Category</h3>
            <div className="grid grid-cols-4 gap-4">
              {Object.entries(analysis.by_category).map(([cat, data]) =>
                renderCategoryCard(cat, data)
              )}
            </div>
          </div>

          {analysis.recommendations.length > 0 && (
            <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
              <h3 className="font-medium text-yellow-800 flex items-center gap-2 mb-3">
                <AlertTriangle size={18} />
                Recommendations
              </h3>
              <ul className="space-y-2">
                {analysis.recommendations.map((rec, i) => (
                  <li key={i} className="text-yellow-700 text-sm">- {rec}</li>
                ))}
              </ul>
            </div>
          )}
        </div>
      )}

      {activeTab === 'duplicates' && (
        <div className="space-y-4">
          {loading ? (
            <div className="flex items-center justify-center py-12 bg-white rounded-lg shadow">
              <Loader2 className="animate-spin text-primary-500" size={32} />
            </div>
          ) : duplicates.length === 0 ? (
            <div className="text-center py-12 text-gray-500 bg-white rounded-lg shadow">
              <Copy size={48} className="mx-auto mb-4 text-gray-300" />
              <p>No duplicates found</p>
            </div>
          ) : (
            duplicates.map((group, i) => (
              <div key={i} className="bg-white rounded-lg shadow p-4">
                <div className="flex items-center justify-between mb-3">
                  <span className="font-medium">
                    {group.duplicate_count + 1} identical files ({group.size_mb.toFixed(1)} MB each)
                  </span>
                  <span className="text-sm text-green-600 font-medium">
                    Can save {group.potential_savings_mb.toFixed(1)} MB
                  </span>
                </div>
                <div className="space-y-1">
                  {group.files.map((file, j) => (
                    <div key={j} className={clsx('text-sm px-3 py-1 rounded', j === 0 ? 'bg-blue-50' : 'bg-gray-50')}>
                      {j === 0 && <span className="mr-2 text-blue-600">Keep</span>}
                      {file}
                    </div>
                  ))}
                </div>
              </div>
            ))
          )}
        </div>
      )}

      {activeTab === 'organize' && (
        <div className="space-y-6">
          <div className="bg-white rounded-lg shadow p-6">
            <h3 className="font-medium text-lg mb-4">Organize Files</h3>
            <p className="text-gray-600 mb-6">
              Sort files into category folders. Preview with dry run first.
            </p>
            <div className="flex gap-4">
              <button
                onClick={() => handleOrganize(true)}
                disabled={loading || !path}
                className="flex items-center gap-2 px-6 py-3 bg-blue-500 text-white rounded-lg hover:bg-blue-600 disabled:opacity-50"
              >
                {loading ? <Loader2 className="animate-spin" size={18} /> : <Search size={18} />}
                Dry Run (Preview)
              </button>
              <button
                onClick={() => handleOrganize(false)}
                disabled={loading || !path || !organizeResult?.dry_run}
                className="flex items-center gap-2 px-6 py-3 bg-green-500 text-white rounded-lg hover:bg-green-600 disabled:opacity-50"
              >
                {loading ? <Loader2 className="animate-spin" size={18} /> : <Play size={18} />}
                Execute
              </button>
            </div>
          </div>

          {organizeResult && (
            <div className={clsx('rounded-lg shadow p-6', organizeResult.dry_run ? 'bg-blue-50' : 'bg-green-50')}>
              <div className="flex items-center gap-2 mb-4">
                {organizeResult.dry_run ? <Search className="text-blue-600" size={24} /> : <CheckCircle className="text-green-600" size={24} />}
                <h3 className="font-medium text-lg">{organizeResult.dry_run ? 'Preview Result' : 'Completed'}</h3>
              </div>
              <div className="grid grid-cols-3 gap-4">
                <div className="bg-white rounded-lg p-3">
                  <p className="text-sm text-gray-500">Files to Move</p>
                  <p className="text-xl font-bold">{organizeResult.files_moved}</p>
                </div>
                <div className="bg-white rounded-lg p-3">
                  <p className="text-sm text-gray-500">Folders to Create</p>
                  <p className="text-xl font-bold">{organizeResult.dirs_created}</p>
                </div>
                <div className="bg-white rounded-lg p-3">
                  <p className="text-sm text-gray-500">Total Actions</p>
                  <p className="text-xl font-bold">{organizeResult.total_actions}</p>
                </div>
              </div>
              {organizeResult.errors.length > 0 && (
                <div className="bg-red-50 border border-red-200 rounded-lg p-3 mt-4">
                  <p className="text-red-700 font-medium mb-2">Errors:</p>
                  <ul className="text-sm text-red-600 space-y-1">
                    {organizeResult.errors.map((err, i) => (
                      <li key={i}>- {err}</li>
                    ))}
                  </ul>
                </div>
              )}
            </div>
          )}
        </div>
      )}

      {activeTab === 'analyze' && !analysis && !loading && (
        <div className="text-center py-12 text-gray-500 bg-white rounded-lg shadow">
          <FolderOpen size={48} className="mx-auto mb-4 text-gray-300" />
          <p>Enter a path and click Analyze</p>
        </div>
      )}
    </div>
  );
}
