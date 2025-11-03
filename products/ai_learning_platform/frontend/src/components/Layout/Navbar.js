// frontend/src/components/Layout/Navbar.js
// ナビゲーションバーコンポーネント - ヘッダーナビゲーション
import React, { useState } from 'react';
import { useNavigate, useLocation } from 'react-router-dom';
import {
  AppBar,
  Toolbar,
  Typography,
  Button,
  IconButton,
  Menu,
  MenuItem,
  Avatar,
  Box,
  useTheme,
  useMediaQuery,
  Drawer,
  List,
  ListItem,
  ListItemIcon,
  ListItemText,
  Divider,
} from '@mui/material';
import {
  Menu as MenuIcon,
  AccountCircle,
  Dashboard,
  Assessment,
  School,
  Person,
  Logout,
  Home,
} from '@mui/icons-material';

const Navbar = ({ user, onLogout }) => {
  const navigate = useNavigate();
  const location = useLocation();
  const theme = useTheme();
  const isMobile = useMediaQuery(theme.breakpoints.down('md'));

  // 状態管理
  const [anchorEl, setAnchorEl] = useState(null);
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);

  // ユーザーメニューの開閉
  const handleUserMenuOpen = (event) => {
    setAnchorEl(event.currentTarget);
  };

  const handleUserMenuClose = () => {
    setAnchorEl(null);
  };

  // モバイルメニューの開閉
  const handleMobileMenuToggle = () => {
    setMobileMenuOpen(!mobileMenuOpen);
  };

  // ナビゲーション処理
  const handleNavigation = (path) => {
    navigate(path);
    setMobileMenuOpen(false);
    handleUserMenuClose();
  };

  // ログアウト処理
  const handleLogout = () => {
    onLogout();
    handleUserMenuClose();
    navigate('/');
  };

  // ナビゲーションアイテム
  const navigationItems = [
    { label: 'ホーム', path: '/', icon: <Home />, public: true },
    { label: 'ダッシュボード', path: '/dashboard', icon: <Dashboard />, auth: true },
    { label: '技能診断', path: '/skill-assessment', icon: <Assessment />, auth: true },
    { label: '学習パス', path: '/learning-path', icon: <School />, auth: true },
  ];

  // 現在のパスがアクティブかチェック
  const isActivePath = (path) => {
    return location.pathname === path;
  };

  // デスクトップ用ナビゲーションボタン
  const renderDesktopNavigation = () => (
    <Box sx={{ display: { xs: 'none', md: 'flex' }, gap: 1 }}>
      {navigationItems
        .filter(item => item.public || (item.auth && user))
        .map((item) => (
          <Button
            key={item.path}
            color="inherit"
            onClick={() => handleNavigation(item.path)}
            sx={{
              backgroundColor: isActivePath(item.path) ? 'rgba(255, 255, 255, 0.1)' : 'transparent',
              '&:hover': {
                backgroundColor: 'rgba(255, 255, 255, 0.1)',
              },
            }}
          >
            {item.label}
          </Button>
        ))}
    </Box>
  );

  // モバイル用ドロワーメニュー
  const renderMobileDrawer = () => (
    <Drawer
      anchor="left"
      open={mobileMenuOpen}
      onClose={handleMobileMenuToggle}
      sx={{
        '& .MuiDrawer-paper': {
          width: 250,
        },
      }}
    >
      <Box sx={{ p: 2 }}>
        <Typography variant="h6" component="div">
          AI学習プラットフォーム
        </Typography>
      </Box>
      <Divider />
      <List>
        {navigationItems
          .filter(item => item.public || (item.auth && user))
          .map((item) => (
            <ListItem
              button
              key={item.path}
              onClick={() => handleNavigation(item.path)}
              selected={isActivePath(item.path)}
            >
              <ListItemIcon>{item.icon}</ListItemIcon>
              <ListItemText primary={item.label} />
            </ListItem>
          ))}
      </List>
      {user && (
        <>
          <Divider />
          <List>
            <ListItem button onClick={() => handleNavigation('/profile')}>
              <ListItemIcon><Person /></ListItemIcon>
              <ListItemText primary="プロフィール" />
            </ListItem>
            <ListItem button onClick={handleLogout}>
              <ListItemIcon><Logout /></ListItemIcon>
              <ListItemText primary="ログアウト" />
            </ListItem>
          </List>
        </>
      )}
    </Drawer>
  );

  return (
    <>
      <AppBar position="sticky">
        <Toolbar>
          {/* モバイルメニューボタン */}
          {isMobile && (
            <IconButton
              edge="start"
              color="inherit"
              aria-label="menu"
              onClick={handleMobileMenuToggle}
              sx={{ mr: 2 }}
            >
              <MenuIcon />
            </IconButton>
          )}

          {/* ロゴ・タイトル */}
          <Typography
            variant="h6"
            component="div"
            sx={{ 
              flexGrow: 1, 
              cursor: 'pointer',
              '&:hover': { opacity: 0.8 }
            }}
            onClick={() => handleNavigation('/')}
          >
            AI学習プラットフォーム
          </Typography>

          {/* デスクトップナビゲーション */}
          {renderDesktopNavigation()}

          {/* ユーザーメニュー */}
          {user ? (
            <Box sx={{ ml: 2 }}>
              <IconButton
                size="large"
                aria-label="user account"
                aria-controls="user-menu"
                aria-haspopup="true"
                onClick={handleUserMenuOpen}
                color="inherit"
              >
                <Avatar sx={{ width: 32, height: 32 }}>
                  {user.full_name ? user.full_name.charAt(0) : user.username.charAt(0)}
                </Avatar>
              </IconButton>
              <Menu
                id="user-menu"
                anchorEl={anchorEl}
                anchorOrigin={{
                  vertical: 'bottom',
                  horizontal: 'right',
                }}
                keepMounted
                transformOrigin={{
                  vertical: 'top',
                  horizontal: 'right',
                }}
                open={Boolean(anchorEl)}
                onClose={handleUserMenuClose}
              >
                <MenuItem onClick={() => handleNavigation('/profile')}>
                  <Person sx={{ mr: 1 }} />
                  プロフィール
                </MenuItem>
                <MenuItem onClick={handleLogout}>
                  <Logout sx={{ mr: 1 }} />
                  ログアウト
                </MenuItem>
              </Menu>
            </Box>
          ) : (
            <Box sx={{ display: { xs: 'none', md: 'flex' }, gap: 1 }}>
              <Button color="inherit" onClick={() => handleNavigation('/login')}>
                ログイン
              </Button>
              <Button 
                color="inherit" 
                variant="outlined"
                onClick={() => handleNavigation('/register')}
                sx={{ 
                  borderColor: 'white',
                  '&:hover': { 
                    borderColor: 'white',
                    backgroundColor: 'rgba(255, 255, 255, 0.1)'
                  }
                }}
              >
                新規登録
              </Button>
            </Box>
          )}
        </Toolbar>
      </AppBar>

      {/* モバイルドロワー */}
      {renderMobileDrawer()}
    </>
  );
};

export default Navbar;
