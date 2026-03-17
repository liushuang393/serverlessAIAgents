# Studios

BizCore Studios は `apps/` 配下の製品群です。

## Primary Studios

- Migration Studio: `apps/code_migration_assistant`
- Enterprise FAQ Studio: `apps/faq_system`
- Computer Assistant Studio: `apps/messaging_hub`

## Rule

- `apps/` は 7コア層の外側にある製品層
- app 専用 skill は各 app 配下に置く
- framework 共通の正本は `control_plane/` またはコア層に置く
