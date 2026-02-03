# Task File Manager

Self-contained task file management using JSON task files on disk.

## Layout

- Root dir: `.task_store` by default
- Task files: `.task_store/tasks/task_<id>.json`

## CLI

```bash
python -m task_manager.cli init
python -m task_manager.cli add --title "Write spec" --tags "docs,design"
python -m task_manager.cli list
python -m task_manager.cli show --id <task_id>
python -m task_manager.cli update --id <task_id> --status in_progress
python -m task_manager.cli done --id <task_id>
python -m task_manager.cli delete --id <task_id>
```
