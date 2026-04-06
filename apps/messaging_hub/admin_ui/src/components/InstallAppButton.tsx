import { Download, Smartphone } from "lucide-react";
import { useEffect, useState } from "react";
import clsx from "clsx";

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

interface InstallAppButtonProps {
  variant?: "default" | "sidebar" | "icon";
  className?: string;
}

export default function InstallAppButton({
  variant = "default",
  className,
}: InstallAppButtonProps) {
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

  const isIconOnly = variant === "icon";

  return (
    <div className={clsx("flex flex-col gap-1", className)}>
      <div className="flex items-center gap-2">
        <button
          onClick={() => void handleInstallClick()}
          className={clsx(
            "inline-flex items-center gap-1.5 transition-all text-xs",
            variant === "default" &&
              "px-3 py-1.5 rounded border bg-white/80 hover:bg-white text-slate-700 glass-panel",
            variant === "sidebar" &&
              "w-full px-3 py-2 rounded-xl text-slate-500 hover:text-primary-700 hover:bg-white/60",
            variant === "icon" &&
              "p-2 rounded-xl text-slate-500 hover:text-primary-700 hover:bg-white/60",
            isInstalled && "opacity-60 cursor-default",
          )}
          disabled={isInstalling || isInstalled}
          title={
            isInstalled
              ? "インストール済み"
              : deferredPrompt
                ? "Webアプリをインストール"
                : "インストール済み"
          }
        >
          {isInstalled ? <Smartphone size={16} /> : <Download size={16} />}
          {!isIconOnly && (
            <span className="font-medium whitespace-nowrap">
              {isInstalled
                ? "済み"
                : deferredPrompt
                  ? "Webアプリをインストール"
                  : "インストール済み"}
            </span>
          )}
        </button>
      </div>
      {!isIconOnly && !isInstalled && showManualHint && (
        <div className="glass-panel p-3 border border-blue-200 bg-blue-50/80 absolute z-50 mt-10 w-64 shadow-xl">
          <p className="text-[11px] text-slate-600 leading-relaxed">
            Chrome/Edge はアドレスバー右側のインストールアイコン、iOS Safari
            は共有メニューから「ホーム画面に追加」を選択してください。
          </p>
        </div>
      )}
    </div>
  );
}
