/**
 * レポート画面コンポーネント.
 * 
 * 目的: 生成されたレポートを表示
 * I/O:
 *   - Input: なし (ストアから取得)
 *   - Output: レポート一覧UI
 */

import React, { useEffect } from 'react';
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
} from '@mui/material';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';
import { useAppStore } from '@/store/useAppStore';
import { format } from 'date-fns';

const Reports: React.FC = () => {
  const { reports, loading, error, fetchReports } = useAppStore();

  useEffect(() => {
    fetchReports();
  }, [fetchReports]);

  if (loading && reports.length === 0) {
    return (
      <Box
        display="flex"
        justifyContent="center"
        alignItems="center"
        minHeight="400px"
      >
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
      <Typography variant="h4" gutterBottom>
        レポート
      </Typography>

      {reports.length === 0 ? (
        <Paper sx={{ p: 3 }}>
          <Typography>レポートがありません</Typography>
        </Paper>
      ) : (
        <Box>
          {reports.map((report) => (
            <Accordion key={report.id} sx={{ mb: 2 }}>
              <AccordionSummary expandIcon={<ExpandMoreIcon />}>
                <Box sx={{ width: '100%' }}>
                  <Typography variant="h6">{report.title}</Typography>
                  <Typography variant="caption" color="text.secondary">
                    {format(new Date(report.created_at), 'yyyy/MM/dd HH:mm')} |
                    期間: {format(new Date(report.period_start), 'MM/dd')} -{' '}
                    {format(new Date(report.period_end), 'MM/dd')}
                  </Typography>
                </Box>
              </AccordionSummary>
              <AccordionDetails>
                <Box>
                  <Typography variant="body1" paragraph>
                    {report.summary}
                  </Typography>

                  {report.sections.map((section, index) => (
                    <Box key={index} mb={2}>
                      <Typography variant="h6" gutterBottom>
                        {section.title}
                      </Typography>
                      <Typography variant="body2" paragraph>
                        {section.content}
                      </Typography>
                    </Box>
                  ))}

                  <Box mt={2}>
                    <Typography variant="subtitle2" gutterBottom>
                      関連トレンド:
                    </Typography>
                    <Box display="flex" gap={1} flexWrap="wrap">
                      {report.trends.map((trend) => (
                        <Chip
                          key={trend.id}
                          label={`${trend.topic} (${trend.score.toFixed(1)})`}
                          size="small"
                        />
                      ))}
                    </Box>
                  </Box>
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

