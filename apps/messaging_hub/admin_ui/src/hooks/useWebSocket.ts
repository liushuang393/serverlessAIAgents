import { useCallback, useEffect, useRef, useState } from "react";
import { getMessagingHubApiKey } from "../shared/auth";

interface WSMessage {
  type: string;
  data: unknown;
}

/**
 * リアルタイム更新用 WebSocket 接続を管理する。
 */
export function useWebSocket(url: string) {
  const wsRef = useRef<WebSocket | null>(null);
  const reconnectTimerRef = useRef<number | null>(null);
  const reconnectEnabledRef = useRef(true);
  const [isConnected, setIsConnected] = useState(false);
  const [lastMessage, setLastMessage] = useState<WSMessage | null>(null);

  const closeSocket = useCallback(() => {
    if (reconnectTimerRef.current !== null) {
      window.clearTimeout(reconnectTimerRef.current);
      reconnectTimerRef.current = null;
    }
    wsRef.current?.close();
    wsRef.current = null;
  }, []);

  const connect = useCallback(() => {
    if (!url) {
      closeSocket();
      setIsConnected(false);
      return;
    }
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      return;
    }

    const apiKey = getMessagingHubApiKey();
    const wsUrl = new URL(url, window.location.href);
    if (wsUrl.protocol === "http:") {
      wsUrl.protocol = "ws:";
    } else if (wsUrl.protocol === "https:") {
      wsUrl.protocol = "wss:";
    }
    if (apiKey) {
      wsUrl.searchParams.set("api_key", apiKey);
    }

    const ws = new WebSocket(wsUrl.toString());
    ws.onopen = () => {
      setIsConnected(true);
    };
    ws.onclose = () => {
      setIsConnected(false);
      wsRef.current = null;
      if (!url || !reconnectEnabledRef.current) {
        return;
      }
      reconnectTimerRef.current = window.setTimeout(connect, 3000);
    };
    ws.onerror = () => {
      setIsConnected(false);
    };
    ws.onmessage = (event) => {
      try {
        const message = JSON.parse(event.data) as WSMessage;
        setLastMessage(message);
      } catch {
        // ignore parse errors from non-json payloads
      }
    };
    wsRef.current = ws;
  }, [closeSocket, url]);

  useEffect(() => {
    reconnectEnabledRef.current = true;
    connect();
    return () => {
      reconnectEnabledRef.current = false;
      closeSocket();
      setIsConnected(false);
    };
  }, [closeSocket, connect]);

  const sendMessage = useCallback((message: WSMessage) => {
    if (wsRef.current?.readyState === WebSocket.OPEN) {
      wsRef.current.send(JSON.stringify(message));
    }
  }, []);

  return { isConnected, lastMessage, sendMessage };
}
