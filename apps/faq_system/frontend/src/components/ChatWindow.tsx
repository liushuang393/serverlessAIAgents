import React, { useState, useRef, useEffect } from 'react';

interface Message { id: string; role: 'user' | 'assistant'; content: string; chart?: any; sources?: any[]; suggestions?: string[]; }

export default function ChatWindow() {
  const [messages, setMessages] = useState<Message[]>([]);
  const [input, setInput] = useState('');
  const [loading, setLoading] = useState(false);
  const endRef = useRef<HTMLDivElement>(null);

  useEffect(() => { endRef.current?.scrollIntoView({ behavior: 'smooth' }); }, [messages]);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!input.trim() || loading) return;
    const userMsg: Message = { id: Date.now().toString(), role: 'user', content: input };
    setMessages(prev => [...prev, userMsg]);
    setInput('');
    setLoading(true);
    const assistantMsg: Message = { id: (Date.now() + 1).toString(), role: 'assistant', content: '' };
    setMessages(prev => [...prev, assistantMsg]);
    try {
      const res = await fetch('/api/faq/query/stream', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ question: input }) });
      const reader = res.body?.getReader();
      const decoder = new TextDecoder();
      while (reader) {
        const { done, value } = await reader.read();
        if (done) break;
        for (const line of decoder.decode(value).split('\n')) {
          if (line.startsWith('data: ')) {
            const event = JSON.parse(line.slice(6));
            if (event.type === 'answer') setMessages(prev => prev.map(m => m.id === assistantMsg.id ? { ...m, content: event.data.answer, chart: event.data.chart } : m));
            if (event.type === 'suggestions') setMessages(prev => prev.map(m => m.id === assistantMsg.id ? { ...m, suggestions: event.data.suggestions } : m));
          }
        }
      }
    } catch (e) { console.error(e); }
    setLoading(false);
  };

  return (
    <div className="flex flex-col h-full bg-gray-50">
      <div className="flex-1 overflow-y-auto p-4 space-y-4">
        {messages.map(msg => (
          <div key={msg.id} className={`flex ${msg.role === 'user' ? 'justify-end' : 'justify-start'}`}>
            <div className={`max-w-3xl rounded-lg p-4 ${msg.role === 'user' ? 'bg-blue-600 text-white' : 'bg-white shadow'}`}>
              <p className="whitespace-pre-wrap">{msg.content}</p>
              {msg.chart && <ChartDisplay chart={msg.chart} />}
              {msg.suggestions?.map((s, i) => <button key={i} onClick={() => setInput(s)} className="mt-2 mr-2 px-3 py-1 bg-gray-100 rounded-full text-sm">{s}</button>)}
            </div>
          </div>
        ))}
        <div ref={endRef} />
      </div>
      <form onSubmit={handleSubmit} className="p-4 border-t flex gap-2">
        <input value={input} onChange={e => setInput(e.target.value)} placeholder="Ask..." className="flex-1 px-4 py-2 border rounded-lg" disabled={loading} />
        <button type="submit" disabled={loading} className="px-6 py-2 bg-blue-600 text-white rounded-lg disabled:opacity-50">Send</button>
      </form>
    </div>
  );
}

function ChartDisplay({ chart }: { chart: any }) {
  const ref = useRef<HTMLDivElement>(null);
  useEffect(() => { if (ref.current) import('echarts').then(e => e.init(ref.current!).setOption(chart.data)); }, [chart]);
  return <div ref={ref} className="w-full h-64 mt-3" />;
}
