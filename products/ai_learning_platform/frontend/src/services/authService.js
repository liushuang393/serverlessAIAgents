// frontend/src/services/authService.js
// 認証サービス - ユーザー認証とトークン管理
import axios from 'axios';

// APIベースURL設定
const API_BASE_URL = process.env.REACT_APP_API_URL || 'http://localhost:8000/api/v1';

// Axiosインスタンスを作成
const apiClient = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    'Content-Type': 'application/json',
  },
});

// リクエストインターセプター - 認証トークンを自動付与
apiClient.interceptors.request.use(
  (config) => {
    const token = localStorage.getItem('access_token');
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error) => {
    return Promise.reject(error);
  }
);

// レスポンスインターセプター - エラーハンドリング
apiClient.interceptors.response.use(
  (response) => {
    return response;
  },
  (error) => {
    if (error.response?.status === 401) {
      // 認証エラーの場合、トークンを削除してログインページにリダイレクト
      localStorage.removeItem('access_token');
      window.location.href = '/login';
    }
    return Promise.reject(error);
  }
);

// 認証サービスクラス
class AuthService {
  /**
   * ユーザーログイン
   * @param {Object} credentials - ログイン認証情報
   * @param {string} credentials.username - ユーザー名
   * @param {string} credentials.password - パスワード
   * @returns {Promise<Object>} ログイン結果
   */
  async login(credentials) {
    try {
      const response = await apiClient.post('/auth/login', credentials);
      return response.data;
    } catch (error) {
      throw new Error(
        error.response?.data?.detail || 'ログインに失敗しました'
      );
    }
  }

  /**
   * ユーザー登録
   * @param {Object} userData - ユーザー登録データ
   * @param {string} userData.username - ユーザー名
   * @param {string} userData.email - メールアドレス
   * @param {string} userData.password - パスワード
   * @param {string} userData.full_name - フルネーム（オプション）
   * @returns {Promise<Object>} 登録結果
   */
  async register(userData) {
    try {
      const response = await apiClient.post('/auth/register', userData);
      return response.data;
    } catch (error) {
      throw new Error(
        error.response?.data?.detail || 'アカウント作成に失敗しました'
      );
    }
  }

  /**
   * 現在のユーザー情報を取得
   * @returns {Promise<Object>} ユーザー情報
   */
  async getCurrentUser() {
    try {
      const response = await apiClient.get('/auth/me');
      return response.data;
    } catch (error) {
      throw new Error(
        error.response?.data?.detail || 'ユーザー情報の取得に失敗しました'
      );
    }
  }

  /**
   * ログアウト
   * @returns {Promise<void>}
   */
  async logout() {
    try {
      await apiClient.post('/auth/logout');
    } catch (error) {
      console.error('ログアウトエラー:', error);
    } finally {
      localStorage.removeItem('access_token');
    }
  }

  /**
   * アクセストークンを更新
   * @returns {Promise<Object>} 新しいトークン情報
   */
  async refreshToken() {
    try {
      const response = await apiClient.post('/auth/refresh');
      const newToken = response.data.access_token;
      localStorage.setItem('access_token', newToken);
      return response.data;
    } catch (error) {
      localStorage.removeItem('access_token');
      throw new Error('トークンの更新に失敗しました');
    }
  }

  /**
   * 認証状態をチェック
   * @returns {boolean} 認証されているかどうか
   */
  isAuthenticated() {
    const token = localStorage.getItem('access_token');
    return !!token;
  }

  /**
   * 保存されたトークンを取得
   * @returns {string|null} アクセストークン
   */
  getToken() {
    return localStorage.getItem('access_token');
  }
}

// 認証サービスのインスタンスをエクスポート
export const authService = new AuthService();

// APIクライアントもエクスポート（他のサービスで使用）
export { apiClient };
