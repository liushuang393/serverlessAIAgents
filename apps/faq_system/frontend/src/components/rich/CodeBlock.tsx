import React, { useState } from 'react';
import { Check, Copy } from 'lucide-react';

interface CodeBlockProps {
    language: string;
    value: string;
}

export const CodeBlock: React.FC<CodeBlockProps> = ({ language, value }) => {
    const [copied, setCopied] = useState(false);

    const handleCopy = async () => {
        await navigator.clipboard.writeText(value);
        setCopied(true);
        setTimeout(() => setCopied(false), 2000);
    };

    return (
        <div className="relative group my-4 rounded-md overflow-hidden bg-[#0d0d0d] border border-gray-800">
            <div className="flex items-center justify-between px-4 py-2 bg-gray-900 border-b border-gray-800 text-xs text-gray-400">
                <span>{language}</span>
                <button
                    onClick={handleCopy}
                    className="flex items-center gap-1 hover:text-white transition-colors"
                >
                    {copied ? <Check size={14} /> : <Copy size={14} />}
                    {copied ? 'Copied!' : 'Copy code'}
                </button>
            </div>
            <div className="p-4 overflow-x-auto">
                <code className="font-mono text-sm text-gray-200 whitespace-pre">
                    {value}
                </code>
            </div>
        </div>
    );
};
