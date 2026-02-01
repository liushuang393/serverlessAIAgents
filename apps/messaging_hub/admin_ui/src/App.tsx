import { Routes, Route } from 'react-router-dom';
import Layout from './components/Layout';
import Dashboard from './components/Dashboard';
import Platforms from './components/Platforms';
import Sessions from './components/Sessions';
import Conversations from './components/Conversations';
import Settings from './components/Settings';

/**
 * Messaging Hub 管理画面 メインアプリケーション
 */
function App() {
  return (
    <Routes>
      <Route path="/" element={<Layout />}>
        <Route index element={<Dashboard />} />
        <Route path="platforms" element={<Platforms />} />
        <Route path="sessions" element={<Sessions />} />
        <Route path="conversations" element={<Conversations />} />
        <Route path="settings" element={<Settings />} />
      </Route>
    </Routes>
  );
}

export default App;

