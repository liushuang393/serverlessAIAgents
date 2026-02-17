import { useEffect, useRef, useState, useCallback } from 'react';

/**
 * WebSocket メッセージ
 */
interface WSMessage {
  type: string;
  data: unknown;
}

/**
 * WebSocket フック
 *
 * リアルタイム更新用 WebSocket 接続を管理
 */
export function useWebSocket(url: string) {
  const wsRef = useRef<WebSocket | null>(null);
  const [isConnected, setIsConnected] = useState(false);
  const [lastMessage, setLastMessage] = useState<WSMessage | null>(null);

  const connect = useCallback(() => {
    if (wsRef.current?.readyState === WebSocket.OPEN) return;

    const apiKey =
      window.localStorage.getItem('MESSAGING_HUB_API_KEY')
      ?? new URLSearchParams(window.location.search).get('api_key')
      ?? '';
    const wsUrl = new URL(url, window.location.href);
    if (wsUrl.protocol === 'http:') {
      wsUrl.protocol = 'ws:';
    } else if (wsUrl.protocol === 'https:') {
      wsUrl.protocol = 'wss:';
    }
    if (apiKey) {
      wsUrl.searchParams.set('api_key', apiKey);
    }
    const ws = new WebSocket(wsUrl.toString());

    ws.onopen = () => {
      setIsConnected(true);
      console.log('WebSocket connected');
    };

    ws.onclose = () => {
      setIsConnected(false);
      console.log('WebSocket disconnected');
      // 3秒後に再接続
      setTimeout(connect, 3000);
    };

    ws.onerror = (error) => {
      console.error('WebSocket error:', error);
    };

    ws.onmessage = (event) => {
      try {
        const message = JSON.parse(event.data) as WSMessage;
        setLastMessage(message);
      } catch {
        console.error('Failed to parse WebSocket message');
      }
    };

    wsRef.current = ws;
  }, [url]);

  useEffect(() => {
    connect();

    return () => {
      wsRef.current?.close();
    };
  }, [connect]);

  const sendMessage = useCallback((message: WSMessage) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.send(JSON.stringify(message));
    }
  }, []);

  return { isConnected, lastMessage, sendMessage };
}
