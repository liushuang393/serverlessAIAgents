/**
 * レイアウトコンポーネント.
 * 
 * 目的: ナビゲーションとコンテンツエリアを提供
 */

import React, { useState } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import { LocaleSwitcher, useI18n } from '../i18n';
import {
  Box,
  Drawer,
  AppBar,
  Toolbar,
  List,
  Typography,
  Divider,
  IconButton,
  ListItem,
  ListItemButton,
  ListItemIcon,
  ListItemText,
} from '@mui/material';
import {
  Menu as MenuIcon,
  Dashboard as DashboardIcon,
  Assessment as ReportIcon,
  Settings as SettingsIcon,
  FindInPage as EvidenceIcon,
  Sensors as SignalIcon,
  TrackChanges as PredictionIcon,
  Groups as CompetitorIcon,
} from '@mui/icons-material';

const drawerWidth = 260;

interface LayoutProps {
  children: React.ReactNode;
}

const Layout: React.FC<LayoutProps> = ({ children }) => {
  const { t } = useI18n();
  const [mobileOpen, setMobileOpen] = useState(false);
  const navigate = useNavigate();
  const location = useLocation();

  const handleDrawerToggle = () => {
    setMobileOpen(!mobileOpen);
  };

  /** ロケール変化時に再計算されるメニュー項目 */
  const menuItems = [
    { text: t('nav.dashboard'), secondary: t('nav.dashboard_desc'), icon: <DashboardIcon />, path: '/dashboard' },
    { text: t('nav.signals'),   secondary: t('nav.signals_desc'),   icon: <SignalIcon />,    path: '/signals' },
    { text: t('nav.evidence'),  secondary: t('nav.evidence_desc'),  icon: <EvidenceIcon />,  path: '/evidence' },
    { text: t('nav.predictions'),secondary: t('nav.predictions_desc'),icon: <PredictionIcon />,path: '/predictions' },
    { text: t('nav.competitor'),secondary: t('nav.competitor_desc'),icon: <CompetitorIcon />,path: '/competitors' },
    { text: t('nav.reports'),   secondary: t('nav.reports_desc'),   icon: <ReportIcon />,    path: '/reports' },
    { text: t('nav.settings'),  secondary: t('nav.settings_desc'),  icon: <SettingsIcon />,  path: '/settings' },
  ];

  const drawer = (
    <div>
      <Toolbar sx={{ alignItems: 'center', gap: 1.4 }}>
        <Box
          sx={{
            width: 40,
            height: 40,
            borderRadius: '14px',
            background: 'linear-gradient(135deg, #6366f1, #a855f7)',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            color: '#fff',
            fontWeight: 700,
            boxShadow: '0 12px 24px rgba(99, 102, 241, 0.35)',
          }}
        >
          MT
        </Box>
        <Box>
          <Typography variant="h6" noWrap component="div">
            Market Trend
          </Typography>
          <Typography variant="caption" color="text.secondary">
            Decision Intelligence
          </Typography>
        </Box>
      </Toolbar>
      <Divider sx={{ borderColor: 'rgba(255, 255, 255, 0.08)' }} />
      <List sx={{ px: 1.5, pt: 1 }}>
        {menuItems.map((item) => (
          <ListItem key={item.path} disablePadding sx={{ mb: 0.5 }}>
            <ListItemButton
              selected={location.pathname === item.path}
              onClick={() => {
                navigate(item.path);
                setMobileOpen(false);
              }}
              sx={{
                borderRadius: 3,
                '&.Mui-selected': {
                  background: 'rgba(99, 102, 241, 0.16)',
                  color: 'primary.light',
                  border: '1px solid rgba(99, 102, 241, 0.3)',
                },
                '&.Mui-selected .MuiListItemIcon-root': {
                  color: 'primary.light',
                },
              }}
            >
              <ListItemIcon sx={{ minWidth: 40 }}>{item.icon}</ListItemIcon>
              <ListItemText
                primary={item.text}
                secondary={item.secondary}
                secondaryTypographyProps={{ fontSize: '0.7rem', color: 'text.secondary' }}
              />
            </ListItemButton>
          </ListItem>
        ))}
      </List>
    </div>
  );

  return (
    <Box sx={{ display: 'flex' }}>
      <AppBar
        position="fixed"
        elevation={0}
        sx={{
          width: { sm: `calc(100% - ${drawerWidth}px)` },
          ml: { sm: `${drawerWidth}px` },
          background: 'rgba(10, 10, 15, 0.78)',
          backdropFilter: 'blur(16px)',
          borderBottom: '1px solid rgba(255, 255, 255, 0.08)',
          color: 'text.primary',
        }}
      >
        <Toolbar>
          <IconButton
            color="inherit"
            edge="start"
            onClick={handleDrawerToggle}
            sx={{ mr: 2, display: { sm: 'none' } }}
          >
            <MenuIcon />
          </IconButton>
          <Typography
            variant="h6"
            noWrap
            component="div"
            sx={{
              background: 'linear-gradient(90deg, #e2e8f0, #a5b4fc)',
              WebkitBackgroundClip: 'text',
              WebkitTextFillColor: 'transparent',
            }}
          >
            {t('app.title')}
          </Typography>
          {/* スペーサー + 言語切り替え */}
          <Box sx={{ flexGrow: 1 }} />
          <LocaleSwitcher style={{ background: 'transparent', border: '1px solid rgba(255,255,255,0.15)', borderRadius: 8, padding: '4px 8px', fontSize: '0.75rem', color: '#94a3b8', cursor: 'pointer' }} />
        </Toolbar>
      </AppBar>
      <Box
        component="nav"
        sx={{ width: { sm: drawerWidth }, flexShrink: { sm: 0 } }}
      >
        <Drawer
          variant="temporary"
          open={mobileOpen}
          onClose={handleDrawerToggle}
          ModalProps={{ keepMounted: true }}
          sx={{
            display: { xs: 'block', sm: 'none' },
            '& .MuiDrawer-paper': {
              boxSizing: 'border-box',
              width: drawerWidth,
              background: '#0a0a0f',
              borderRight: '1px solid rgba(255, 255, 255, 0.08)',
              boxShadow: '0 20px 40px rgba(0,0,0,0.35)',
            },
          }}
        >
          {drawer}
        </Drawer>
        <Drawer
          variant="permanent"
          sx={{
            display: { xs: 'none', sm: 'block' },
            '& .MuiDrawer-paper': {
              boxSizing: 'border-box',
              width: drawerWidth,
              background: '#0a0a0f',
              borderRight: '1px solid rgba(255, 255, 255, 0.08)',
              boxShadow: '0 20px 40px rgba(0,0,0,0.35)',
            },
          }}
          open
        >
          {drawer}
        </Drawer>
      </Box>
      <Box
        component="main"
        sx={{
          flexGrow: 1,
          p: 3,
          width: { sm: `calc(100% - ${drawerWidth}px)` },
          minHeight: '100vh',
          animation: 'riseIn 0.6s ease both',
        }}
      >
        <Toolbar />
        {children}
      </Box>
    </Box>
  );
};

export default Layout;
