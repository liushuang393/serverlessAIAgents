/**
 * レポート画面コンポーネント.
 *
 * 目的: 生成されたレポートを読みやすい形式で表示し、配布形式へ出力する
 * I/O:
 *   - Input: なし (ストアから取得)
 *   - Output: レポート一覧UI
 */

import React, { useEffect, useMemo } from 'react';
import {
  Box,
  Paper,
  Typography,
  CircularProgress,
  Alert,
  Accordion,
  AccordionSummary,
  AccordionDetails,
  Chip,
  Stack,
  Grid,
  Button,
  Divider,
  Tooltip,
} from '@mui/material';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import DownloadIcon from '@mui/icons-material/Download';
import PictureAsPdfIcon from '@mui/icons-material/PictureAsPdf';
import SlideshowIcon from '@mui/icons-material/Slideshow';
import { useAppStore } from '@/store/useAppStore';
import { apiClient } from '@/api/client';
import type { Report, Trend } from '@/types';
import { format } from 'date-fns';

const safeFormatDate = (value: string, pattern: string = 'yyyy/MM/dd HH:mm'): string => {
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) {
    return '-';
  }
  return format(date, pattern);
};

const stripMarkdownSyntax = (text: string): string =>
  text
    .replace(/^#{1,6}\s+/gm, '')
    .replace(/^[-*]\s+/gm, '')
    .replace(/^\d+\.\s+/gm, '')
    .replace(/\*\*(.*?)\*\*/g, '$1')
    .replace(/`(.*?)`/g, '$1')
    .trim();

const escapeHtml = (value: string): string =>
  value
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');

const getGrowthLabel = (trend: Trend): string => {
  const growthState =
    typeof trend.metadata?.growth_state === 'string'
      ? trend.metadata.growth_state
      : '';

  if (growthState === 'new') {
    return 'NEW';
  }
  if (growthState === 'insufficient_history' || growthState === 'no_signal') {
    return 'N/A';
  }
  return `${(trend.growth_rate * 100).toFixed(1)}%`;
};

const getGrowthHint = (trend: Trend): string => {
  const growthExplanation =
    typeof trend.metadata?.growth_explanation === 'string'
      ? trend.metadata.growth_explanation
      : '';
  return growthExplanation || '成長率は現期間と前期間の比較値です。';
};

const buildReportMarkdown = (report: Report): string => {
  const lines: string[] = [];
  lines.push(`# ${report.title}`);
  lines.push('');
  lines.push(`- 生成日時: ${safeFormatDate(report.created_at)}`);
  lines.push(`- 対象期間: ${safeFormatDate(report.period_start, 'yyyy/MM/dd')} - ${safeFormatDate(report.period_end, 'yyyy/MM/dd')}`);
  lines.push('');
  lines.push('## エグゼクティブサマリー');
  lines.push(report.summary || 'サマリーなし');
  lines.push('');

  if (report.trends.length > 0) {
    lines.push('## トレンド指標');
    lines.push('| トピック | スコア | 記事数 | 成長 |');
    lines.push('|---|---:|---:|---|');
    report.trends.forEach((trend) => {
      lines.push(`| ${trend.topic} | ${trend.score.toFixed(2)} | ${trend.articles_count} | ${getGrowthLabel(trend)} |`);
    });
    lines.push('');
  }

  report.sections.forEach((section) => {
    lines.push(`## ${section.title}`);
    lines.push(section.content || '(本文なし)');
    lines.push('');
  });

  return lines.join('\n');
};

const buildPptOutlineMarkdown = (report: Report): string => {
  const topTrends = report.trends.slice(0, 3).map((trend) => trend.topic).join(' / ') || '主要トレンドなし';

  return [
    '---',
    'marp: true',
    'theme: default',
    `paginate: true`,
    `title: ${report.title}`,
    '---',
    '',
    `# ${report.title}`,
    '',
    `- 生成日時: ${safeFormatDate(report.created_at)}`,
    `- 期間: ${safeFormatDate(report.period_start, 'yyyy/MM/dd')} - ${safeFormatDate(report.period_end, 'yyyy/MM/dd')}`,
    '',
    '---',
    '',
    '# 主要トピック',
    '',
    `- ${topTrends}`,
    '- NEW は前期間に基準データがない新規検知',
    '',
    '---',
    '',
    '# 意思決定ポイント',
    '',
    ...report.sections.slice(0, 3).map((section) => `- ${section.title}: ${stripMarkdownSyntax(section.content).slice(0, 80)}`),
    '',
  ].join('\n');
};

const downloadText = (filename: string, content: string): void => {
  const blob = new Blob([content], { type: 'text/markdown;charset=utf-8' });
  const url = URL.createObjectURL(blob);
  const anchor = document.createElement('a');
  anchor.href = url;
  anchor.download = filename;
  document.body.appendChild(anchor);
  anchor.click();
  document.body.removeChild(anchor);
  URL.revokeObjectURL(url);
};

const downloadBlob = (filename: string, blob: Blob): void => {
  const url = URL.createObjectURL(blob);
  const anchor = document.createElement('a');
  anchor.href = url;
  anchor.download = filename;
  document.body.appendChild(anchor);
  anchor.click();
  document.body.removeChild(anchor);
  URL.revokeObjectURL(url);
};

const renderInlineMarkdown = (text: string): React.ReactNode[] => {
  const chunks = text.split(/(\*\*[^*]+\*\*)/g).filter(Boolean);
  return chunks.map((chunk, index) => {
    if (chunk.startsWith('**') && chunk.endsWith('**')) {
      return <strong key={`${chunk}-${index}`}>{chunk.slice(2, -2)}</strong>;
    }
    return <React.Fragment key={`${chunk}-${index}`}>{chunk}</React.Fragment>;
  });
};

const MarkdownContent: React.FC<{ content: string }> = ({ content }) => {
  const lines = content.replace(/\r/g, '').split('\n');
  const elements: React.ReactNode[] = [];

  for (let i = 0; i < lines.length; i += 1) {
    const line = lines[i].trim();

    if (!line) {
      continue;
    }

    if (line.startsWith('### ')) {
      elements.push(
        <Typography key={`h3-${i}`} variant="subtitle1" sx={{ mt: 1.5, fontWeight: 700 }}>
          {line.slice(4)}
        </Typography>
      );
      continue;
    }

    if (line.startsWith('## ')) {
      elements.push(
        <Typography key={`h2-${i}`} variant="h6" sx={{ mt: 2, fontWeight: 700 }}>
          {line.slice(3)}
        </Typography>
      );
      continue;
    }

    if (line.startsWith('# ')) {
      elements.push(
        <Typography key={`h1-${i}`} variant="h5" sx={{ mt: 2, fontWeight: 700 }}>
          {line.slice(2)}
        </Typography>
      );
      continue;
    }

    if (line.startsWith('|') && i + 2 < lines.length && lines[i + 1].includes('|---')) {
      const tableLines: string[] = [];
      tableLines.push(lines[i]);
      tableLines.push(lines[i + 1]);
      let pointer = i + 2;
      while (pointer < lines.length && lines[pointer].trim().startsWith('|')) {
        tableLines.push(lines[pointer]);
        pointer += 1;
      }

      const parsedRows = tableLines
        .map((raw) => raw.trim().split('|').slice(1, -1).map((cell) => cell.trim()))
        .filter((cells) => cells.length > 0);
      const header = parsedRows[0] || [];
      const rows = parsedRows.slice(2);

      elements.push(
        <Box key={`table-${i}`} sx={{ overflowX: 'auto', mt: 1.5 }}>
          <table style={{ width: '100%', borderCollapse: 'collapse' }}>
            <thead>
              <tr>
                {header.map((cell) => (
                  <th
                    key={`th-${cell}`}
                    style={{
                      textAlign: 'left',
                      padding: '8px 10px',
                      borderBottom: '1px solid rgba(255,255,255,0.18)',
                      color: '#cbd5f5',
                    }}
                  >
                    {cell}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {rows.map((row, rowIndex) => (
                <tr key={`row-${rowIndex}`}>
                  {row.map((cell, colIndex) => (
                    <td
                      key={`td-${rowIndex}-${colIndex}`}
                      style={{
                        padding: '8px 10px',
                        borderBottom: '1px solid rgba(255,255,255,0.08)',
                        color: '#e2e8f0',
                      }}
                    >
                      {cell}
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </Box>
      );

      i = pointer - 1;
      continue;
    }

    if (line.startsWith('- ')) {
      const items: string[] = [line.slice(2)];
      let pointer = i + 1;
      while (pointer < lines.length && lines[pointer].trim().startsWith('- ')) {
        items.push(lines[pointer].trim().slice(2));
        pointer += 1;
      }

      elements.push(
        <Box key={`ul-${i}`} component="ul" sx={{ pl: 3, mt: 1, mb: 1 }}>
          {items.map((item, index) => (
            <li key={`${item}-${index}`}>
              <Typography variant="body2">{renderInlineMarkdown(item)}</Typography>
            </li>
          ))}
        </Box>
      );

      i = pointer - 1;
      continue;
    }

    if (/^\d+\.\s+/.test(line)) {
      const items: string[] = [line.replace(/^\d+\.\s+/, '')];
      let pointer = i + 1;
      while (pointer < lines.length && /^\d+\.\s+/.test(lines[pointer].trim())) {
        items.push(lines[pointer].trim().replace(/^\d+\.\s+/, ''));
        pointer += 1;
      }

      elements.push(
        <Box key={`ol-${i}`} component="ol" sx={{ pl: 3, mt: 1, mb: 1 }}>
          {items.map((item, index) => (
            <li key={`${item}-${index}`}>
              <Typography variant="body2">{renderInlineMarkdown(item)}</Typography>
            </li>
          ))}
        </Box>
      );

      i = pointer - 1;
      continue;
    }

    elements.push(
      <Typography key={`p-${i}`} variant="body2" sx={{ mt: 0.8, lineHeight: 1.8 }}>
        {renderInlineMarkdown(line)}
      </Typography>
    );
  }

  return <Box>{elements}</Box>;
};

const Reports: React.FC = () => {
  const { reports, loading, error, fetchReports } = useAppStore();

  useEffect(() => {
    fetchReports();
  }, [fetchReports]);

  const reportMetrics = useMemo(() => {
    const totalReports = reports.length;
    const latest = reports[0];
    const totalTrends = reports.reduce((acc, report) => acc + report.trends.length, 0);
    return {
      totalReports,
      latestAt: latest ? safeFormatDate(latest.created_at) : '-',
      averageTrends:
        totalReports > 0 ? (totalTrends / totalReports).toFixed(1) : '0.0',
    };
  }, [reports]);

  const handleDownloadMarkdown = (report: Report): void => {
    const slug = report.title.replace(/[^a-zA-Z0-9_-]/g, '_');
    downloadText(`${slug || report.id}.md`, buildReportMarkdown(report));
  };

  const handleDownloadPptOutline = async (report: Report): Promise<void> => {
    try {
      const exported = await apiClient.exportReport(report.id, 'pptx');
      downloadBlob(exported.filename, exported.blob);
    } catch {
      const slug = report.title.replace(/[^a-zA-Z0-9_-]/g, '_');
      downloadText(`${slug || report.id}_slides.md`, buildPptOutlineMarkdown(report));
    }
  };

  const handleExportPdf = async (report: Report): Promise<void> => {
    try {
      const exported = await apiClient.exportReport(report.id, 'pdf');
      downloadBlob(exported.filename, exported.blob);
      return;
    } catch {
      // API が未対応環境の場合は既存フォールバック（ブラウザ印刷）を維持
    }

    const markdown = buildReportMarkdown(report);
    const printWindow = window.open('', '_blank', 'noopener,noreferrer,width=1000,height=800');
    if (!printWindow) {
      return;
    }

    const html = `<!doctype html>
<html>
<head>
  <meta charset="utf-8" />
  <title>${escapeHtml(report.title)}</title>
  <style>
    body { font-family: "Noto Sans JP", sans-serif; margin: 28px; color: #111827; }
    h1 { font-size: 24px; margin-bottom: 12px; }
    pre { white-space: pre-wrap; line-height: 1.6; font-size: 13px; }
    @media print { body { margin: 18px; } }
  </style>
</head>
<body>
  <h1>${escapeHtml(report.title)}</h1>
  <pre>${escapeHtml(markdown)}</pre>
</body>
</html>`;

    printWindow.document.open();
    printWindow.document.write(html);
    printWindow.document.close();
    printWindow.focus();
    printWindow.print();
  };

  if (loading && reports.length === 0) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" minHeight="400px">
        <CircularProgress />
      </Box>
    );
  }

  if (error) {
    return (
      <Box p={3}>
        <Alert severity="error">{error}</Alert>
      </Box>
    );
  }

  return (
    <Box p={3}>
      <Paper
        sx={{
          p: { xs: 3, md: 4 },
          mb: 3,
          background:
            'linear-gradient(120deg, rgba(30,58,138,0.35), rgba(15,23,42,0.8), rgba(56,189,248,0.15))',
        }}
      >
        <Typography variant="h4" gutterBottom>
          レポートセンター
        </Typography>
        <Typography variant="body2" color="text.secondary">
          指標の根拠を明示しながら、配布用の Markdown / PDF / PPT 下書きを出力できます。
        </Typography>

        <Grid container spacing={2} sx={{ mt: 1 }}>
          <Grid item xs={12} md={4}>
            <Paper sx={{ p: 2.2, backgroundColor: 'rgba(15,23,42,0.8)' }}>
              <Typography variant="overline" color="text.secondary">レポート件数</Typography>
              <Typography variant="h5">{reportMetrics.totalReports}</Typography>
            </Paper>
          </Grid>
          <Grid item xs={12} md={4}>
            <Paper sx={{ p: 2.2, backgroundColor: 'rgba(15,23,42,0.8)' }}>
              <Typography variant="overline" color="text.secondary">直近更新</Typography>
              <Typography variant="h6">{reportMetrics.latestAt}</Typography>
            </Paper>
          </Grid>
          <Grid item xs={12} md={4}>
            <Paper sx={{ p: 2.2, backgroundColor: 'rgba(15,23,42,0.8)' }}>
              <Typography variant="overline" color="text.secondary">平均トレンド数</Typography>
              <Typography variant="h5">{reportMetrics.averageTrends}</Typography>
            </Paper>
          </Grid>
        </Grid>
      </Paper>

      {reports.length === 0 ? (
        <Paper
          sx={{
            p: 3,
            border: '1px dashed rgba(255,255,255,0.2)',
            backgroundColor: '#0f1117',
          }}
        >
          <Typography color="text.secondary">レポートがありません</Typography>
        </Paper>
      ) : (
        <Box>
          {reports.map((report) => (
            <Accordion
              key={report.id}
              sx={{
                mb: 2,
                borderRadius: 3,
                backgroundColor: '#10141f',
                border: '1px solid rgba(148,163,184,0.18)',
                '&:before': { display: 'none' },
              }}
            >
              <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                <Box sx={{ width: '100%' }}>
                  <Stack
                    direction={{ xs: 'column', md: 'row' }}
                    alignItems={{ xs: 'flex-start', md: 'center' }}
                    justifyContent="space-between"
                    spacing={1}
                  >
                    <Typography variant="h6">{report.title}</Typography>
                    <Typography variant="caption" color="text.secondary">
                      {safeFormatDate(report.created_at)} | 期間: {safeFormatDate(report.period_start, 'MM/dd')} -{' '}
                      {safeFormatDate(report.period_end, 'MM/dd')}
                    </Typography>
                  </Stack>
                </Box>
              </AccordionSummary>

              <AccordionDetails sx={{ backgroundColor: '#0b1220' }}>
                <Stack
                  direction={{ xs: 'column', md: 'row' }}
                  spacing={1.2}
                  justifyContent="flex-end"
                  sx={{ mb: 2 }}
                >
                  <Button
                    size="small"
                    variant="outlined"
                    startIcon={<DownloadIcon />}
                    onClick={() => handleDownloadMarkdown(report)}
                  >
                    Markdown
                  </Button>
                  <Button
                    size="small"
                    variant="outlined"
                    startIcon={<PictureAsPdfIcon />}
                    onClick={() => handleExportPdf(report)}
                  >
                    PDF
                  </Button>
                  <Button
                    size="small"
                    variant="outlined"
                    startIcon={<SlideshowIcon />}
                    onClick={() => handleDownloadPptOutline(report)}
                  >
                    PPT下書き
                  </Button>
                </Stack>

                <Paper sx={{ p: 2.5, mb: 2, backgroundColor: 'rgba(15,23,42,0.72)' }}>
                  <Typography variant="subtitle2" gutterBottom color="text.secondary">
                    サマリー
                  </Typography>
                  <MarkdownContent content={report.summary || 'サマリー情報なし'} />
                </Paper>

                {report.sections.map((section, index) => (
                  <Paper
                    key={`${section.title}-${index}`}
                    sx={{ p: 2.5, mb: 2, backgroundColor: 'rgba(15,23,42,0.65)' }}
                  >
                    <Typography variant="subtitle1" sx={{ fontWeight: 700 }}>
                      {section.title}
                    </Typography>
                    <Divider sx={{ my: 1.5, borderColor: 'rgba(148,163,184,0.25)' }} />
                    <MarkdownContent content={section.content} />
                  </Paper>
                ))}

                <Box mt={2}>
                  <Typography variant="subtitle2" gutterBottom>
                    関連トレンド
                  </Typography>
                  <Stack direction="row" gap={1} flexWrap="wrap">
                    {report.trends.map((trend) => (
                      <Tooltip key={trend.id} title={getGrowthHint(trend)}>
                        <Chip
                          label={`${trend.topic} | score ${trend.score.toFixed(2)} | ${getGrowthLabel(trend)}`}
                          size="small"
                          sx={{
                            backgroundColor: 'rgba(56,189,248,0.16)',
                            color: '#dbeafe',
                          }}
                        />
                      </Tooltip>
                    ))}
                  </Stack>
                </Box>
              </AccordionDetails>
            </Accordion>
          ))}
        </Box>
      )}
    </Box>
  );
};

export default Reports;
