import { useState } from 'react';
import { Save, RefreshCw } from 'lucide-react';

/**
 * 設定ページ
 *
 * システム設定の管理
 */
export default function Settings() {
  const [settings, setSettings] = useState({
    sessionTimeout: 30,
    maxMessageLength: 4096,
    enableLogging: true,
    logLevel: 'info',
  });

  const handleSave = () => {
    // TODO: API call to save settings
    console.log('Saving settings:', settings);
  };

  return (
    <div>
      <h2 className="text-2xl font-bold mb-6">設定</h2>

      <div className="bg-white rounded-lg shadow p-6">
        <h3 className="text-lg font-semibold mb-6">一般設定</h3>

        <div className="space-y-6">
          {/* セッションタイムアウト */}
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              セッションタイムアウト（分）
            </label>
            <input
              type="number"
              value={settings.sessionTimeout}
              onChange={(e) =>
                setSettings({ ...settings, sessionTimeout: parseInt(e.target.value) })
              }
              className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
            />
          </div>

          {/* 最大メッセージ長 */}
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              最大メッセージ長（文字）
            </label>
            <input
              type="number"
              value={settings.maxMessageLength}
              onChange={(e) =>
                setSettings({ ...settings, maxMessageLength: parseInt(e.target.value) })
              }
              className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
            />
          </div>

          {/* ロギング有効化 */}
          <div className="flex items-center">
            <input
              type="checkbox"
              id="enableLogging"
              checked={settings.enableLogging}
              onChange={(e) =>
                setSettings({ ...settings, enableLogging: e.target.checked })
              }
              className="h-4 w-4 text-primary-600 focus:ring-primary-500 border-gray-300 rounded"
            />
            <label htmlFor="enableLogging" className="ml-2 text-sm text-gray-700">
              ロギングを有効化
            </label>
          </div>

          {/* ログレベル */}
          <div>
            <label className="block text-sm font-medium text-gray-700 mb-2">
              ログレベル
            </label>
            <select
              value={settings.logLevel}
              onChange={(e) =>
                setSettings({ ...settings, logLevel: e.target.value })
              }
              className="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-primary-500 focus:border-primary-500"
            >
              <option value="debug">Debug</option>
              <option value="info">Info</option>
              <option value="warning">Warning</option>
              <option value="error">Error</option>
            </select>
          </div>
        </div>

        {/* アクションボタン */}
        <div className="flex gap-4 mt-8">
          <button
            onClick={handleSave}
            className="flex items-center gap-2 px-6 py-2 bg-primary-500 text-white rounded-lg hover:bg-primary-600 transition-colors"
          >
            <Save size={20} />
            保存
          </button>
          <button
            onClick={() => window.location.reload()}
            className="flex items-center gap-2 px-6 py-2 border border-gray-300 rounded-lg hover:bg-gray-50 transition-colors"
          >
            <RefreshCw size={20} />
            リセット
          </button>
        </div>
      </div>
    </div>
  );
}

