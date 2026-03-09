import {
  CartesianGrid,
  Line,
  LineChart,
  ResponsiveContainer,
  Tooltip,
  XAxis,
  YAxis,
} from "recharts";

type PlatformChartPoint = {
  platform: string;
  messages: number;
};

interface PlatformMessageChartProps {
  data: PlatformChartPoint[];
}

export default function PlatformMessageChart({
  data,
}: PlatformMessageChartProps) {
  if (data.length === 0) {
    return (
      <div className="h-[300px] flex items-center justify-center text-sm text-muted">
        表示できるデータがありません
      </div>
    );
  }

  return (
    <ResponsiveContainer width="100%" height={300}>
      <LineChart data={data}>
        <CartesianGrid strokeDasharray="3 3" />
        <XAxis dataKey="platform" />
        <YAxis />
        <Tooltip />
        <Line
          type="monotone"
          dataKey="messages"
          stroke="#0ea5e9"
          strokeWidth={2}
        />
      </LineChart>
    </ResponsiveContainer>
  );
}
