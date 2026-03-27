/**
 * アクセス制御ページ.
 *
 * コレクション別のロールベースアクセス制御を表示する（読み取り専用）。
 * 編集は auth_service で管理するため、ここでは概要のみ表示。
 */
import { Shield, Lock, Users } from "lucide-react";
import { useI18n } from "../../i18n";

const ROLE_KB_MAP = [
  {
    role: "admin",
    kbs: ["internal", "external", "confidential"],
    color: "text-rose-400",
  },
  { role: "manager", kbs: ["internal", "external"], color: "text-amber-400" },
  { role: "employee", kbs: ["internal", "external"], color: "text-blue-400" },
  { role: "guest", kbs: ["external"], color: "text-slate-400" },
];

export function AccessControlPage() {
  const { t } = useI18n();

  return (
    <div className="space-y-6">
      {/* 説明 */}
      <div className="flex items-start gap-3 p-4 rounded-xl bg-indigo-500/5 border border-indigo-500/10">
        <Shield size={18} className="text-indigo-400 flex-shrink-0 mt-0.5" />
        <div>
          <p className="text-sm text-white mb-1">
            {t("rag.access_control_title")}
          </p>
          <p className="text-xs text-[var(--text-muted)]">
            {t("rag.access_control_description")}
          </p>
        </div>
      </div>

      {/* ロール → KB マッピング */}
      <div className="rounded-xl bg-white/[0.03] border border-white/5">
        <div className="p-4 border-b border-white/5">
          <h3 className="text-xs font-semibold text-[var(--text-muted)] uppercase flex items-center gap-1.5">
            <Users size={14} />
            {t("rag.role_kb_mapping")}
          </h3>
        </div>

        <div className="divide-y divide-white/5">
          {ROLE_KB_MAP.map(({ role, kbs, color }) => (
            <div key={role} className="flex items-center justify-between p-4">
              <div className="flex items-center gap-2">
                <Lock size={14} className={color} />
                <span className="text-sm text-white font-medium capitalize">
                  {role}
                </span>
              </div>
              <div className="flex gap-1.5">
                {kbs.map((kb) => (
                  <span
                    key={kb}
                    className="px-2 py-0.5 rounded-full bg-white/5 text-xs text-[var(--text-muted)] font-mono"
                  >
                    {kb}
                  </span>
                ))}
              </div>
            </div>
          ))}
        </div>
      </div>

      {/* 注意事項 */}
      <div className="p-4 rounded-xl bg-white/[0.02] border border-white/5 text-xs text-[var(--text-muted)]">
        <p className="mb-1 font-medium text-white">
          {t("rag.access_note_title")}
        </p>
        <ul className="list-disc list-inside space-y-1">
          <li>{t("rag.access_note_1")}</li>
          <li>{t("rag.access_note_2")}</li>
          <li>{t("rag.access_note_3")}</li>
        </ul>
      </div>
    </div>
  );
}
