import React from 'react';
import { Card, CardContent, Typography, Box, Chip } from '@mui/material';
import AutoAwesomeIcon from '@mui/icons-material/AutoAwesome';
import { useI18n } from '../i18n';

interface AISummaryCardProps {
    summary: string;
    actionRequired: boolean;
    status: 'positive' | 'warning' | 'neutral';
}

const AISummaryCard: React.FC<AISummaryCardProps> = ({ summary, actionRequired, status }) => {
    const { t } = useI18n();
    const getStatusColor = () => {
        switch (status) {
            case 'positive': return '#4caf50';
            case 'warning': return '#ff9800';
            default: return '#2196f3';
        }
    };

    return (
        <Card
            sx={{
                mb: 3,
                background: 'rgba(255, 255, 255, 0.03)',
                border: '1px solid rgba(255, 255, 255, 0.08)',
                borderRadius: 4,
                overflow: 'visible',
                position: 'relative'
            }}
        >
            <Box
                sx={{
                    position: 'absolute',
                    top: -12,
                    left: 20,
                    bgcolor: getStatusColor(),
                    px: 1.5,
                    py: 0.5,
                    borderRadius: 2,
                    display: 'flex',
                    alignItems: 'center',
                    gap: 0.5,
                    boxShadow: '0 4px 12px rgba(0,0,0,0.2)'
                }}
            >
                <AutoAwesomeIcon sx={{ fontSize: 16, color: 'white' }} />
                <Typography variant="caption" sx={{ color: 'white', fontWeight: 'bold' }}>
                    {t('ai_summary.badge')}
                </Typography>
            </Box>
            <CardContent sx={{ pt: 3 }}>
                <Typography variant="body1" sx={{ lineHeight: 1.8, color: 'rgba(255,255,255,0.9)' }}>
                    {summary}
                </Typography>
                {actionRequired && (
                    <Box sx={{ mt: 2, display: 'flex', alignItems: 'center', gap: 1 }}>
                        <Chip
                            label={t('ai_summary.action_badge')}
                            size="small"
                            sx={{ bgcolor: 'rgba(99, 102, 241, 0.2)', color: '#a5b4fc', fontWeight: 'bold' }}
                        />
                        <Typography variant="body2" color="text.secondary">
                            {t('ai_summary.action_desc')}
                        </Typography>
                    </Box>
                )}
            </CardContent>
        </Card>
    );
};

export default AISummaryCard;
