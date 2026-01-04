/**
 * 判子（ハンコ）シールコンポーネント.
 *
 * 目的: 日本式の美しい電子印鑑を表示
 * 設計: SVGベース、縦書き対応、伝統的な朱色
 */

import React from 'react';

interface HankoSealProps {
  /** 表示する名前（姓名） */
  name: string;
  /** 日付（オプション） */
  date?: string;
  /** サイズ (px) */
  size?: number;
  /** アニメーション */
  animated?: boolean;
}

/**
 * 日本式判子シールコンポーネント.
 * 
 * 名前を縦書きで表示し、伝統的な朱色（vermillion）で描画。
 */
export const HankoSeal: React.FC<HankoSealProps> = ({
  name,
  date,
  size = 80,
  animated = false,
}) => {
  // 名前を2文字ずつ（姓・名）に分割して縦書き表示
  const extractSurname = (fullName: string): string => {
    // 日本語名から姓を抽出（スペースで区切る）
    const parts = fullName.trim().split(/\s+/);
    if (parts.length >= 2) {
      // "田中 一郎" → "田中"
      return parts[0];
    }
    // 姓名がスペースなしの場合、最初の2文字
    return fullName.slice(0, 2);
  };

  const surname = extractSurname(name);
  
  // 縦書き用に文字を分割
  const chars = surname.split('');
  
  // 朱色のバリエーション（リアルな印鑑風）
  const vermillion = '#C53030';
  const vermillionDark = '#9B2C2C';
  const vermillionLight = '#E53E3E';

  return (
    <div
      className={`inline-flex flex-col items-center ${animated ? 'animate-stamp' : ''}`}
      style={{ width: size }}
    >
      <svg
        viewBox="0 0 100 100"
        width={size}
        height={size}
        className="drop-shadow-lg"
        style={{
          filter: animated ? undefined : 'drop-shadow(2px 2px 4px rgba(0,0,0,0.3))',
        }}
      >
        <defs>
          {/* 印鑑のテクスチャ（かすれ効果） */}
          <filter id="hanko-texture">
            <feTurbulence
              type="fractalNoise"
              baseFrequency="0.04"
              numOctaves="5"
              result="noise"
            />
            <feDisplacementMap
              in="SourceGraphic"
              in2="noise"
              scale="2"
              xChannelSelector="R"
              yChannelSelector="G"
            />
          </filter>
          
          {/* グラデーション */}
          <radialGradient id="hanko-gradient" cx="30%" cy="30%" r="70%">
            <stop offset="0%" stopColor={vermillionLight} />
            <stop offset="100%" stopColor={vermillionDark} />
          </radialGradient>

          {/* 内側の影 */}
          <filter id="inner-shadow">
            <feOffset dx="0" dy="1" />
            <feGaussianBlur stdDeviation="1" result="offset-blur" />
            <feComposite operator="out" in="SourceGraphic" in2="offset-blur" result="inverse" />
            <feFlood floodColor={vermillionDark} floodOpacity="0.5" result="color" />
            <feComposite operator="in" in="color" in2="inverse" result="shadow" />
            <feComposite operator="over" in="shadow" in2="SourceGraphic" />
          </filter>
        </defs>

        {/* 外枠（円形） */}
        <circle
          cx="50"
          cy="50"
          r="46"
          fill="none"
          stroke={vermillion}
          strokeWidth="3"
          filter="url(#hanko-texture)"
          opacity="0.95"
        />

        {/* 内枠 */}
        <circle
          cx="50"
          cy="50"
          r="40"
          fill="none"
          stroke={vermillion}
          strokeWidth="1.5"
          filter="url(#hanko-texture)"
          opacity="0.8"
        />

        {/* 文字（縦書き） */}
        <g filter="url(#inner-shadow)">
          {chars.length === 1 ? (
            // 1文字の場合は中央に大きく
            <text
              x="50"
              y="58"
              textAnchor="middle"
              fill={vermillion}
              fontSize="38"
              fontFamily="'Noto Serif JP', 'Yu Mincho', 'MS Mincho', serif"
              fontWeight="700"
              filter="url(#hanko-texture)"
            >
              {chars[0]}
            </text>
          ) : chars.length === 2 ? (
            // 2文字の場合は縦に並べる
            <>
              <text
                x="50"
                y="42"
                textAnchor="middle"
                fill={vermillion}
                fontSize="28"
                fontFamily="'Noto Serif JP', 'Yu Mincho', 'MS Mincho', serif"
                fontWeight="700"
                filter="url(#hanko-texture)"
              >
                {chars[0]}
              </text>
              <text
                x="50"
                y="72"
                textAnchor="middle"
                fill={vermillion}
                fontSize="28"
                fontFamily="'Noto Serif JP', 'Yu Mincho', 'MS Mincho', serif"
                fontWeight="700"
                filter="url(#hanko-texture)"
              >
                {chars[1]}
              </text>
            </>
          ) : (
            // 3文字以上の場合
            chars.slice(0, 3).map((char, i) => (
              <text
                key={i}
                x="50"
                y={30 + i * 24}
                textAnchor="middle"
                fill={vermillion}
                fontSize="22"
                fontFamily="'Noto Serif JP', 'Yu Mincho', 'MS Mincho', serif"
                fontWeight="700"
                filter="url(#hanko-texture)"
              >
                {char}
              </text>
            ))
          )}
        </g>
      </svg>

      {/* 日付表示 */}
      {date && (
        <div
          className="text-xs mt-1 font-mono"
          style={{ color: vermillion, opacity: 0.9 }}
        >
          {date}
        </div>
      )}

      {/* アニメーション用スタイル */}
      <style>{`
        @keyframes stamp {
          0% {
            transform: scale(1.5) rotate(-10deg);
            opacity: 0;
          }
          50% {
            transform: scale(0.95) rotate(2deg);
          }
          70% {
            transform: scale(1.02) rotate(-1deg);
          }
          100% {
            transform: scale(1) rotate(0deg);
            opacity: 1;
          }
        }
        .animate-stamp {
          animation: stamp 0.5s cubic-bezier(0.22, 1, 0.36, 1) forwards;
        }
      `}</style>
    </div>
  );
};

/**
 * 署名エリアコンポーネント.
 * 
 * 判子と署名者情報を組み合わせて表示。
 */
interface SignatureAreaProps {
  /** 署名者名 */
  signerName: string;
  /** 部署 */
  department?: string;
  /** 役職 */
  position?: string;
  /** 署名日時 */
  signedAt?: string;
  /** アニメーション */
  animated?: boolean;
}

export const SignatureArea: React.FC<SignatureAreaProps> = ({
  signerName,
  department,
  position,
  signedAt,
  animated = false,
}) => {
  return (
    <div className="flex items-center gap-6 p-6 bg-gradient-to-r from-amber-50/5 to-orange-50/5 rounded-xl border border-amber-500/10">
      {/* 判子 */}
      <HankoSeal name={signerName} animated={animated} size={72} />

      {/* 署名情報 */}
      <div className="flex-1">
        <div className="text-lg font-bold text-white mb-1">
          {signerName}
        </div>
        {(department || position) && (
          <div className="text-sm text-slate-400">
            {department}
            {department && position && ' / '}
            {position}
          </div>
        )}
        {signedAt && (
          <div className="text-xs text-slate-500 mt-2 flex items-center gap-1">
            <span>🕐</span>
            {signedAt}
          </div>
        )}
      </div>

      {/* 承認マーク */}
      <div className="flex flex-col items-center">
        <div className="w-16 h-16 rounded-full bg-emerald-500/10 border-2 border-emerald-500/30 flex items-center justify-center">
          <span className="text-2xl">✓</span>
        </div>
        <div className="text-xs text-emerald-400 mt-1 font-medium">承認済</div>
      </div>
    </div>
  );
};

