import { useState } from 'react';
import { Download, FileJson, FileText, FileCode } from 'lucide-react';

/**
 * 会話履歴ページ
 *
 * 会話の閲覧とエクスポート機能
 */
export default function Conversations() {
  const [selectedFormat, setSelectedFormat] = useState<'json' | 'csv' | 'markdown'>('json');
  const [isExporting, setIsExporting] = useState(false);

  const handleExport = async () => {
    setIsExporting(true);
    try {
      const response = await fetch(`/api/export?format=${selectedFormat}`);
      const blob = await response.blob();

      // ダウンロード
      const url = window.URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `conversations.${selectedFormat === 'markdown' ? 'md' : selectedFormat}`;
      document.body.appendChild(a);
      a.click();
      window.URL.revokeObjectURL(url);
      a.remove();
    } catch (error) {
      console.error('Export failed:', error);
    } finally {
      setIsExporting(false);
    }
  };

  const formats = [
    { value: 'json', label: 'JSON', icon: FileJson, description: '構造化データ形式' },
    { value: 'csv', label: 'CSV', icon: FileText, description: 'スプレッドシート用' },
    { value: 'markdown', label: 'Markdown', icon: FileCode, description: 'ドキュメント用' },
  ] as const;

  return (
    <div>
      <h2 className="text-2xl font-bold mb-6">会話履歴</h2>

      {/* エクスポートセクション */}
      <div className="bg-white rounded-lg shadow p-6 mb-8">
        <h3 className="text-lg font-semibold mb-4">会話エクスポート</h3>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
          {formats.map(({ value, label, icon: Icon, description }) => (
            <button
              key={value}
              onClick={() => setSelectedFormat(value)}
              className={`p-4 rounded-lg border-2 text-left transition-all ${
                selectedFormat === value
                  ? 'border-primary-500 bg-primary-50'
                  : 'border-gray-200 hover:border-primary-300'
              }`}
            >
              <div className="flex items-center gap-3 mb-2">
                <Icon
                  className={
                    selectedFormat === value
                      ? 'text-primary-600'
                      : 'text-gray-500'
                  }
                  size={24}
                />
                <span className="font-medium">{label}</span>
              </div>
              <p className="text-sm text-gray-500">{description}</p>
            </button>
          ))}
        </div>

        <button
          onClick={handleExport}
          disabled={isExporting}
          className="flex items-center gap-2 px-6 py-3 bg-primary-500 text-white rounded-lg hover:bg-primary-600 transition-colors disabled:opacity-50"
        >
          <Download size={20} />
          {isExporting ? 'エクスポート中...' : 'エクスポート'}
        </button>
      </div>

      {/* プレースホルダー: 会話一覧 */}
      <div className="bg-white rounded-lg shadow p-6">
        <h3 className="text-lg font-semibold mb-4">最近の会話</h3>
        <div className="text-center py-8 text-gray-500">
          <p>会話履歴の閲覧機能は開発中です</p>
          <p className="text-sm mt-2">
            上記のエクスポート機能で会話をダウンロードできます
          </p>
        </div>
      </div>
    </div>
  );
}

