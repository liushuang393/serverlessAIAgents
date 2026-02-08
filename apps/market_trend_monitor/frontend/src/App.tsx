/**
 * メインアプリケーションコンポーネント.
 * 
 * 目的: ルーティングとレイアウトを管理
 */

import React from 'react';
import { BrowserRouter, Routes, Route, Navigate } from 'react-router-dom';
import { ThemeProvider, createTheme } from '@mui/material/styles';
import CssBaseline from '@mui/material/CssBaseline';
import Layout from './components/Layout';
import Dashboard from './components/Dashboard';
import Settings from './components/Settings';
import Reports from './components/Reports';

const theme = createTheme({
  palette: {
    mode: 'dark',
    primary: {
      main: '#6366f1',
      light: '#818cf8',
      dark: '#4338ca',
    },
    secondary: {
      main: '#a855f7',
      light: '#c084fc',
      dark: '#7e22ce',
    },
    background: {
      default: '#0a0a0f',
      paper: '#12121a',
    },
    text: {
      primary: '#f8fafc',
      secondary: '#94a3b8',
    },
    divider: 'rgba(255,255,255,0.08)',
  },
  shape: {
    borderRadius: 16,
  },
  typography: {
    fontFamily: '"Sora", "Noto Sans JP", "Hiragino Kaku Gothic ProN", sans-serif',
    h1: {
      fontWeight: 600,
      letterSpacing: '-0.02em',
    },
    h2: {
      fontWeight: 600,
      letterSpacing: '-0.02em',
    },
    h3: {
      fontWeight: 600,
      letterSpacing: '-0.02em',
    },
    h4: {
      fontWeight: 600,
      letterSpacing: '-0.01em',
    },
    h5: {
      fontWeight: 600,
      letterSpacing: '-0.01em',
    },
    h6: {
      fontWeight: 600,
      letterSpacing: '-0.01em',
    },
    button: {
      fontWeight: 600,
      letterSpacing: '0.02em',
    },
  },
  components: {
    MuiCssBaseline: {
      styleOverrides: `
        @import url('https://fonts.googleapis.com/css2?family=Noto+Sans+JP:wght@400;500;600;700&family=Sora:wght@400;500;600;700&display=swap');
        :root {
          --shadow-elev: 0 30px 80px rgba(0, 0, 0, 0.55);
          --shadow-card: 0 16px 40px rgba(0, 0, 0, 0.4);
        }
        body {
          background-color: #0a0a0f;
          background-image:
            radial-gradient(circle at 15% 15%, rgba(99, 102, 241, 0.18), transparent 40%),
            radial-gradient(circle at 85% 5%, rgba(168, 85, 247, 0.16), transparent 45%),
            linear-gradient(rgba(255, 255, 255, 0.04) 1px, transparent 1px),
            linear-gradient(90deg, rgba(255, 255, 255, 0.04) 1px, transparent 1px);
          background-size: auto, auto, 32px 32px, 32px 32px;
          color: #f8fafc;
        }
        #root {
          min-height: 100vh;
        }
        @keyframes riseIn {
          from {
            opacity: 0;
            transform: translateY(18px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
      `,
    },
    MuiPaper: {
      styleOverrides: {
        root: {
          backgroundColor: '#12121a',
          boxShadow: 'var(--shadow-card)',
          border: '1px solid rgba(255, 255, 255, 0.06)',
        },
      },
    },
    MuiButton: {
      styleOverrides: {
        root: {
          borderRadius: 14,
          textTransform: 'none',
          paddingLeft: 20,
          paddingRight: 20,
        },
        contained: {
          background: 'linear-gradient(135deg, #6366f1, #a855f7)',
          boxShadow: '0 14px 28px rgba(99, 102, 241, 0.25)',
          '&:hover': {
            background: 'linear-gradient(135deg, #818cf8, #c084fc)',
            boxShadow: '0 16px 32px rgba(99, 102, 241, 0.35)',
          },
        },
        containedPrimary: {
          background: 'linear-gradient(135deg, #6366f1, #a855f7)',
          boxShadow: '0 14px 28px rgba(99, 102, 241, 0.25)',
          '&:hover': {
            background: 'linear-gradient(135deg, #818cf8, #c084fc)',
            boxShadow: '0 16px 32px rgba(99, 102, 241, 0.35)',
          },
        },
        outlined: {
          borderColor: 'rgba(255,255,255,0.18)',
          color: '#e2e8f0',
          '&:hover': {
            borderColor: 'rgba(129,140,248,0.7)',
            backgroundColor: 'rgba(99,102,241,0.12)',
          },
        },
      },
    },
    MuiOutlinedInput: {
      styleOverrides: {
        root: {
          backgroundColor: '#0f1117',
          borderRadius: 12,
          '& fieldset': {
            borderColor: 'rgba(255,255,255,0.12)',
          },
          '&:hover fieldset': {
            borderColor: 'rgba(99,102,241,0.6)',
          },
          '&.Mui-focused fieldset': {
            borderColor: 'rgba(99,102,241,0.8)',
            boxShadow: '0 0 0 3px rgba(99,102,241,0.2)',
          },
        },
        input: {
          color: '#e2e8f0',
        },
      },
    },
    MuiInputLabel: {
      styleOverrides: {
        root: {
          color: '#94a3b8',
          '&.Mui-focused': {
            color: '#c7d2fe',
          },
        },
      },
    },
    MuiChip: {
      styleOverrides: {
        root: {
          backgroundColor: 'rgba(99, 102, 241, 0.18)',
          color: '#e2e8f0',
        },
        outlined: {
          borderColor: 'rgba(255,255,255,0.2)',
        },
      },
    },
    MuiCheckbox: {
      styleOverrides: {
        root: {
          color: 'rgba(255,255,255,0.5)',
          '&.Mui-checked': {
            color: '#818cf8',
          },
        },
      },
    },
    MuiAlert: {
      styleOverrides: {
        standardSuccess: {
          backgroundColor: 'rgba(16,185,129,0.12)',
          color: '#a7f3d0',
          border: '1px solid rgba(16,185,129,0.35)',
        },
      },
    },
    MuiAccordion: {
      styleOverrides: {
        root: {
          backgroundColor: '#12121a',
          borderRadius: 12,
          border: '1px solid rgba(255,255,255,0.06)',
        },
      },
    },
  },
});

const App: React.FC = () => {
  return (
    <ThemeProvider theme={theme}>
      <CssBaseline />
      <BrowserRouter>
        <Layout>
          <Routes>
            <Route path="/" element={<Navigate to="/dashboard" replace />} />
            <Route path="/dashboard" element={<Dashboard />} />
            <Route path="/reports" element={<Reports />} />
            <Route path="/settings" element={<Settings />} />
          </Routes>
        </Layout>
      </BrowserRouter>
    </ThemeProvider>
  );
};

export default App;
