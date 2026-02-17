import React from 'react';
import ReactDOM from 'react-dom/client';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { BrowserRouter } from 'react-router-dom';
import App from './App';
import './index.css';

function getMessagingHubApiKey(): string {
  return (
    window.localStorage.getItem('MESSAGING_HUB_API_KEY')
    ?? new URLSearchParams(window.location.search).get('api_key')
    ?? ''
  );
}

const nativeFetch = window.fetch.bind(window);
window.fetch = (input: RequestInfo | URL, init?: RequestInit): Promise<Response> => {
  const apiKey = getMessagingHubApiKey();
  if (!apiKey) {
    return nativeFetch(input, init);
  }
  const headers = new Headers(init?.headers);
  if (!headers.has('x-api-key')) {
    headers.set('x-api-key', apiKey);
  }
  return nativeFetch(input, { ...init, headers });
};

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 5000,
      refetchOnWindowFocus: false,
    },
  },
});

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <App />
      </BrowserRouter>
    </QueryClientProvider>
  </React.StrictMode>
);
