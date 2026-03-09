import { Download, Smartphone } from "lucide-react";
import { useEffect, useState } from "react";

interface BeforeInstallPromptEvent extends Event {
  prompt: () => Promise<void>;
  userChoice: Promise<{ outcome: "accepted" | "dismissed"; platform: string }>;
}

function isStandaloneMode(): boolean {
  const iosNavigator = navigator as Navigator & { standalone?: boolean };
  return (
    window.matchMedia("(display-mode: standalone)").matches ||
    iosNavigator.standalone === true
  );
}

export default function InstallAppButton() {
  const [deferredPrompt, setDeferredPrompt] =
    useState<BeforeInstallPromptEvent | null>(null);
  const [isInstalled, setIsInstalled] = useState<boolean>(isStandaloneMode());
  const [isInstalling, setIsInstalling] = useState(false);
  const [showManualHint, setShowManualHint] = useState(false);

  useEffect(() => {
    const displayModeMedia = window.matchMedia("(display-mode: standalone)");

    const onDisplayModeChange = () => {
      setIsInstalled(isStandaloneMode());
    };
    const onBeforeInstallPrompt = (event: Event) => {
      event.preventDefault();
      setDeferredPrompt(event as BeforeInstallPromptEvent);
    };
    const onAppInstalled = () => {
      setIsInstalled(true);
      setDeferredPrompt(null);
      setShowManualHint(false);
    };

    displayModeMedia.addEventListener("change", onDisplayModeChange);
    window.addEventListener("beforeinstallprompt", onBeforeInstallPrompt);
    window.addEventListener("appinstalled", onAppInstalled);

    return () => {
      displayModeMedia.removeEventListener("change", onDisplayModeChange);
      window.removeEventListener("beforeinstallprompt", onBeforeInstallPrompt);
      window.removeEventListener("appinstalled", onAppInstalled);
    };
  }, []);

  const handleInstallClick = async () => {
    if (isInstalled) {
      return;
    }
    if (!deferredPrompt) {
      setShowManualHint((prev) => !prev);
      return;
    }

    setIsInstalling(true);
    try {
      await deferredPrompt.prompt();
      const choice = await deferredPrompt.userChoice;
      if (choice.outcome === "accepted") {
        setIsInstalled(true);
      }
    } finally {
      setDeferredPrompt(null);
      setIsInstalling(false);
    }
  };

  return (
    <div className="glass-panel px-3 py-2 text-xs">
      <div className="flex items-center gap-2">
        <button
          onClick={() => void handleInstallClick()}
          className="inline-flex items-center gap-1.5 px-3 py-1.5 rounded border bg-white/80 hover:bg-white text-slate-700"
          disabled={isInstalling}
          title="ホーム画面またはデスクトップにインストール"
        >
          {isInstalled ? <Smartphone size={13} /> : <Download size={13} />}
          {isInstalled
            ? "インストール済み"
            : deferredPrompt
              ? "アプリをインストール"
              : "インストール方法"}
        </button>
        {!isInstalled && deferredPrompt && (
          <span className="text-emerald-700">
            この端末はワンクリックでインストール可能です
          </span>
        )}
      </div>
      {!isInstalled && showManualHint && (
        <p className="mt-2 text-[11px] text-slate-600">
          Chrome/Edge はアドレスバー右側のインストールアイコン、iOS Safari
          は共有メニューから「ホーム画面に追加」を選択してください。
        </p>
      )}
    </div>
  );
}
