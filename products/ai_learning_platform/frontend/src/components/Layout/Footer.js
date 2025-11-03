// frontend/src/components/Layout/Footer.js
// フッターコンポーネント - ページ下部の情報表示
import React from 'react';
import {
  Box,
  Container,
  Typography,
  Link,
  Grid,
  Divider,
} from '@mui/material';
import { GitHub, Email, School } from '@mui/icons-material';

const Footer = () => {
  const currentYear = new Date().getFullYear();

  return (
    <Box
      component="footer"
      sx={{
        backgroundColor: 'primary.main',
        color: 'white',
        py: 4,
        mt: 'auto',
      }}
    >
      <Container maxWidth="lg">
        <Grid container spacing={4}>
          {/* プラットフォーム情報 */}
          <Grid item xs={12} md={4}>
            <Typography variant="h6" gutterBottom>
              AI学習プラットフォーム
            </Typography>
            <Typography variant="body2" sx={{ mb: 2 }}>
              個人化されたAI・機械学習の学習体験を提供します。
              技能診断から始まり、あなたに最適な学習パスを提案します。
            </Typography>
            <Box sx={{ display: 'flex', gap: 1 }}>
              <School sx={{ fontSize: 20 }} />
              <Typography variant="body2">
                効果的な学習をサポート
              </Typography>
            </Box>
          </Grid>

          {/* クイックリンク */}
          <Grid item xs={12} md={4}>
            <Typography variant="h6" gutterBottom>
              クイックリンク
            </Typography>
            <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
              <Link 
                href="/" 
                color="inherit" 
                underline="hover"
                sx={{ '&:hover': { opacity: 0.8 } }}
              >
                ホーム
              </Link>
              <Link 
                href="/skill-assessment" 
                color="inherit" 
                underline="hover"
                sx={{ '&:hover': { opacity: 0.8 } }}
              >
                技能診断
              </Link>
              <Link 
                href="/learning-path" 
                color="inherit" 
                underline="hover"
                sx={{ '&:hover': { opacity: 0.8 } }}
              >
                学習パス
              </Link>
              <Link 
                href="/dashboard" 
                color="inherit" 
                underline="hover"
                sx={{ '&:hover': { opacity: 0.8 } }}
              >
                ダッシュボード
              </Link>
            </Box>
          </Grid>

          {/* お問い合わせ・サポート */}
          <Grid item xs={12} md={4}>
            <Typography variant="h6" gutterBottom>
              サポート
            </Typography>
            <Box sx={{ display: 'flex', flexDirection: 'column', gap: 1 }}>
              <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                <Email sx={{ fontSize: 16 }} />
                <Link 
                  href="mailto:support@ai-learning-platform.com" 
                  color="inherit" 
                  underline="hover"
                  sx={{ '&:hover': { opacity: 0.8 } }}
                >
                  support@ai-learning-platform.com
                </Link>
              </Box>
              <Box sx={{ display: 'flex', alignItems: 'center', gap: 1 }}>
                <GitHub sx={{ fontSize: 16 }} />
                <Link 
                  href="https://github.com/ai-learning-platform" 
                  color="inherit" 
                  underline="hover"
                  target="_blank"
                  rel="noopener noreferrer"
                  sx={{ '&:hover': { opacity: 0.8 } }}
                >
                  GitHub
                </Link>
              </Box>
            </Box>
            
            <Typography variant="body2" sx={{ mt: 2 }}>
              ご質問やフィードバックがございましたら、
              お気軽にお問い合わせください。
            </Typography>
          </Grid>
        </Grid>

        <Divider sx={{ my: 3, borderColor: 'rgba(255, 255, 255, 0.2)' }} />

        {/* コピーライト */}
        <Box
          sx={{
            display: 'flex',
            justifyContent: 'space-between',
            alignItems: 'center',
            flexWrap: 'wrap',
            gap: 2,
          }}
        >
          <Typography variant="body2">
            © {currentYear} AI学習プラットフォーム. All rights reserved.
          </Typography>
          
          <Box sx={{ display: 'flex', gap: 2 }}>
            <Link 
              href="/privacy" 
              color="inherit" 
              underline="hover"
              variant="body2"
              sx={{ '&:hover': { opacity: 0.8 } }}
            >
              プライバシーポリシー
            </Link>
            <Link 
              href="/terms" 
              color="inherit" 
              underline="hover"
              variant="body2"
              sx={{ '&:hover': { opacity: 0.8 } }}
            >
              利用規約
            </Link>
          </Box>
        </Box>
      </Container>
    </Box>
  );
};

export default Footer;
