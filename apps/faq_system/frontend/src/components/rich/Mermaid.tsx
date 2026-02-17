import React, { useEffect, useRef, useState } from 'react';
import mermaid from 'mermaid';

interface MermaidProps {
    chart: string;
}

export const Mermaid: React.FC<MermaidProps> = ({ chart }) => {
    const ref = useRef<HTMLDivElement>(null);
    const [svg, setSvg] = useState<string>('');
    const [error, setError] = useState<string>('');

    useEffect(() => {
        mermaid.initialize({
            startOnLoad: false,
            theme: 'dark', // or 'default', based on current theme, but we default to dark for this app style
            securityLevel: 'loose',
        });
    }, []);

    useEffect(() => {
        const renderChart = async () => {
            if (!chart) return;
            try {
                const { svg } = await mermaid.render(`mermaid-${Math.random().toString(36).substr(2, 9)}`, chart);
                setSvg(svg);
                setError('');
            } catch (err) {
                console.error("Mermaid render error:", err);
                setError('Failed to render diagram');
                // Mermaid might modify the DOM on error, causing issues.
            }
        };

        renderChart();
    }, [chart]);

    if (error) {
        return <div className="text-red-500 text-sm border border-red-500/50 p-2 rounded">{error}</div>;
    }

    return (
        <div
            ref={ref}
            className="mermaid-chart my-4 overflow-x-auto bg-white/5 p-4 rounded-md flex justify-center"
            dangerouslySetInnerHTML={{ __html: svg }}
        />
    );
};
