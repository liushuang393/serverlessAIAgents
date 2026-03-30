import React, { useEffect, useRef } from "react";
import * as echarts from "echarts/core";
import { BarChart, LineChart, PieChart, ScatterChart } from "echarts/charts";
import {
  GridComponent,
  TooltipComponent,
  LegendComponent,
  TitleComponent,
} from "echarts/components";
import { CanvasRenderer } from "echarts/renderers";

// ECharts モジュール登録（1回のみ）
echarts.use([
  BarChart,
  LineChart,
  PieChart,
  ScatterChart,
  GridComponent,
  TooltipComponent,
  LegendComponent,
  TitleComponent,
  CanvasRenderer,
]);

interface ChartRendererProps {
  /** バックエンドが返す chart オブジェクト */
  chart: Record<string, unknown>;
}

/**
 * バックエンドの chart データを ECharts でレンダリングする。
 *
 * バックエンドが返す chart 構造:
 * { type, title, data: { labels, datasets, xAxis, yAxis } }
 */
export const ChartRenderer: React.FC<ChartRendererProps> = ({ chart }) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const instanceRef = useRef<echarts.ECharts | null>(null);

  useEffect(() => {
    if (!containerRef.current || !chart) return;

    // 既存インスタンスを破棄
    if (instanceRef.current) {
      instanceRef.current.dispose();
    }

    const ec = echarts.init(containerRef.current, "dark");
    instanceRef.current = ec;

    const option = buildOption(chart);
    ec.setOption(option);

    // リサイズ対応
    const onResize = () => ec.resize();
    window.addEventListener("resize", onResize);

    return () => {
      window.removeEventListener("resize", onResize);
      ec.dispose();
      instanceRef.current = null;
    };
  }, [chart]);

  return (
    <div className="my-4 rounded-lg border border-white/10 overflow-hidden bg-[#0d1117]">
      <div ref={containerRef} className="w-full" style={{ height: 320 }} />
    </div>
  );
};

/** バックエンドの chart 構造を ECharts option に変換 */
function buildOption(chart: Record<string, unknown>): echarts.EChartsCoreOption {
  const chartType = (chart.type as string) || "bar";
  const title = (chart.title as string) || "";
  const rawData = (chart.data as Record<string, unknown>) || {};
  const labels = (rawData.labels as string[]) || [];
  const datasets = (rawData.datasets as Array<Record<string, unknown>>) || [];

  const baseOption: echarts.EChartsCoreOption = {
    backgroundColor: "transparent",
    title: title
      ? {
          text: title,
          left: "center",
          textStyle: { color: "#c9d1d9", fontSize: 14 },
        }
      : undefined,
    tooltip: { trigger: chartType === "pie" ? "item" : "axis" },
    grid: { left: "8%", right: "4%", bottom: "12%", top: title ? "18%" : "8%" },
  };

  if (chartType === "pie") {
    const values = datasets[0]?.data as number[] | undefined;
    return {
      ...baseOption,
      series: [
        {
          type: "pie",
          radius: ["35%", "65%"],
          center: ["50%", "55%"],
          data: labels.map((name, i) => ({
            name,
            value: values?.[i] ?? 0,
          })),
          label: { color: "#c9d1d9", fontSize: 11 },
          emphasis: { itemStyle: { shadowBlur: 10 } },
        },
      ],
    };
  }

  // bar / line / scatter
  return {
    ...baseOption,
    xAxis: {
      type: "category",
      data: labels,
      axisLabel: {
        color: "#8b949e",
        fontSize: 11,
        rotate: labels.length > 6 ? 30 : 0,
      },
      axisLine: { lineStyle: { color: "#30363d" } },
    },
    yAxis: {
      type: "value",
      axisLabel: { color: "#8b949e", fontSize: 11 },
      splitLine: { lineStyle: { color: "#21262d" } },
    },
    series: datasets.map((ds) => ({
      type: chartType as "bar" | "line" | "scatter",
      data: ds.data as number[],
      itemStyle: {
        borderRadius: chartType === "bar" ? [4, 4, 0, 0] : undefined,
      },
      smooth: chartType === "line",
    })),
  };
}
