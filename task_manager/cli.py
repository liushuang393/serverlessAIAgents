from __future__ import annotations

import argparse
import json
from pathlib import Path

from .storage import TaskStore


def _parse_tags(raw: str | None) -> list[str] | None:
    if raw is None:
        return None
    tags = [t.strip() for t in raw.split(",") if t.strip()]
    return tags


def _print_json(payload: object) -> None:
    print(json.dumps(payload, ensure_ascii=True, indent=2, sort_keys=True))


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Task file manager")
    parser.add_argument("--dir", default=".task_store", help="Root directory for task files")

    sub = parser.add_subparsers(dest="command", required=True)

    sub.add_parser("init", help="Initialize the task store")

    add = sub.add_parser("add", help="Add a task")
    add.add_argument("--title", required=True)
    add.add_argument("--description", default="")
    add.add_argument("--priority", default="medium")
    add.add_argument("--status", default="todo")
    add.add_argument("--tags", default=None)
    add.add_argument("--due", default=None)

    list_cmd = sub.add_parser("list", help="List tasks")
    list_cmd.add_argument("--status", default=None)

    show = sub.add_parser("show", help="Show task details")
    show.add_argument("--id", required=True)

    update = sub.add_parser("update", help="Update task fields")
    update.add_argument("--id", required=True)
    update.add_argument("--title")
    update.add_argument("--description")
    update.add_argument("--priority")
    update.add_argument("--status")
    update.add_argument("--tags")
    update.add_argument("--due")

    done = sub.add_parser("done", help="Mark task as done")
    done.add_argument("--id", required=True)

    delete = sub.add_parser("delete", help="Delete a task")
    delete.add_argument("--id", required=True)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    store = TaskStore(Path(args.dir))

    if args.command == "init":
        store.init()
        _print_json({"status": "ok", "dir": str(store.tasks_dir)})
        return 0

    store._ensure_ready()

    if args.command == "add":
        task = store.create_task(
            title=args.title,
            description=args.description,
            status=args.status,
            priority=args.priority,
            tags=_parse_tags(args.tags),
            due_date=args.due,
        )
        _print_json(task.to_dict())
        return 0

    if args.command == "list":
        tasks = [t.to_dict() for t in store.list_tasks(status=args.status)]
        _print_json({"count": len(tasks), "tasks": tasks})
        return 0

    if args.command == "show":
        task = store.get_task(args.id)
        _print_json(task.to_dict())
        return 0

    if args.command == "update":
        task = store.update_task(
            args.id,
            title=args.title,
            description=args.description,
            status=args.status,
            priority=args.priority,
            tags=_parse_tags(args.tags),
            due_date=args.due,
        )
        _print_json(task.to_dict())
        return 0

    if args.command == "done":
        task = store.set_status(args.id, "done")
        _print_json(task.to_dict())
        return 0

    if args.command == "delete":
        store.delete_task(args.id)
        _print_json({"status": "deleted", "id": args.id})
        return 0

    parser.print_help()
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
