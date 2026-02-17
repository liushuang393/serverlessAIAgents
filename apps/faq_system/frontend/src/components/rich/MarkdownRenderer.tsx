import React from 'react';
import ReactMarkdown from 'react-markdown';
import remarkGfm from 'remark-gfm';
import rehypeKatex from 'rehype-katex';
import 'katex/dist/katex.min.css';
import { CodeBlock } from './CodeBlock';
import { Mermaid } from './Mermaid';

interface MarkdownRendererProps {
    content: string;
}

export const MarkdownRenderer: React.FC<MarkdownRendererProps> = ({ content }) => {
    return (
        <div className="markdown-content">
            <ReactMarkdown
                remarkPlugins={[remarkGfm]}
                rehypePlugins={[rehypeKatex]}
                components={{
                    code: ({ node, className, children, ...props }: any) => {
                        const match = /language-(\w+)/.exec(className || '');
                        const language = match ? match[1] : '';
                        const isInline = !match && !String(children).includes('\n');
                        const value = String(children).replace(/\n$/, '');

                        if (!isInline && language === 'mermaid') {
                            return <Mermaid chart={value} />;
                        }

                        return !isInline && match ? (
                            <CodeBlock
                                language={language}
                                value={value}
                            />
                        ) : (
                            <code className={className} {...props}>
                                {children}
                            </code>
                        );
                    },
                    // Custom renderers for other elements
                    a: ({ children, href }: any) => (
                        <a href={href} target="_blank" rel="noopener noreferrer" className="text-blue-500 hover:underline">
                            {children}
                        </a>
                    ),
                    img: ({ src, alt }: any) => (
                        <img src={src} alt={alt} className="max-w-full rounded-md my-2 border border-gray-200" loading="lazy" />
                    ),
                }}
            >
                {content}
            </ReactMarkdown>
        </div>
    );
};
