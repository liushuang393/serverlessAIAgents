# 内部向けドキュメント

対象: プラットフォーム開発者、運用担当、セキュリティ担当、Plugin 開発者

## 推奨閲覧順

1. `platform-dev-handbook.md`
2. `layer-design.md`
3. `tools-skills-plugins-guide.md`
4. `plugin-extension-handbook.md`
5. `core-extension-playbook.md`
6. `architecture-kernel.md`
7. `env-bootstrap-and-tenant-invite-security.md`

## 方針

- 対外は 3 Studio を主線にし、内部は Kernel と Plugin 契約を主線にする。
- 副作用機能は `Policy -> Governance -> Audit` を通す。
- 機能追加は Core 直入れではなく Plugin First を基本とする。
