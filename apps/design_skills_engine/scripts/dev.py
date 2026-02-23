#!/usr/bin/env python3
import os
import shutil
import subprocess
import sys
from pathlib import Path


def ensure_env_file(app_dir: Path):
    """
    Check if .env exists. If not, try to copy from .env.example or similar.
    """
    env_path = app_dir / ".env"
    if env_path.exists():
        return

    # Candidates for example files
    candidates = [".env.example", "env.example", "example.env", "sample.env"]

    for candidate in candidates:
        candidate_path = app_dir / candidate
        if candidate_path.exists():
            print(f"Creating .env from {candidate}...")
            shutil.copy(candidate_path, env_path)
            return

    # If no example file found, create an empty one or just skip
    print("Warning: No .env or .env.example found. Application might fail if it requires environment variables.")


def start_docker_dependencies(app_dir: Path):
    """
    Check for docker-compose.yml and start infrastructure services (DB, Redis, etc.).
    Excludes the main app service (backend/frontend) if possible.
    """
    compose_file = app_dir / "docker-compose.yml"
    if not compose_file.exists():
        return

    print(f"Checking for infrastructure services in {compose_file}...")

    try:
        # Check if CPU override is needed (e.g. on WSL without NVIDIA or explicitly requested)
        cpu_override = app_dir / "docker-compose.cpu.yml"
        compose_cmd = ["docker", "compose"]
        if compose_file.exists():
            compose_cmd.extend(["-f", compose_file.name])
        if cpu_override.exists():
            # Check for NVIDIA GPU
            has_gpu = False
            try:
                subprocess.run(["nvidia-smi"], capture_output=True, check=True)
                has_gpu = True
            except (subprocess.CalledProcessError, FileNotFoundError):
                has_gpu = False
            
            if not has_gpu:
                print("No GPU detected, using CPU override...")
                compose_cmd.extend(["-f", cpu_override.name])

        result = subprocess.run(
            compose_cmd + ["config", "--services"], cwd=str(app_dir), capture_output=True, text=True, check=True
        )
        all_services = result.stdout.strip().split("\n")

        exclusions = {"backend", "frontend", "app", "api", "web", "worker", "celery", app_dir.name}
        services_to_start = [s for s in all_services if s and s not in exclusions]

        if not services_to_start:
            print("No infrastructure services found to start.")
            return

        print(f"Starting infrastructure services: {', '.join(services_to_start)}")
        subprocess.run(compose_cmd + ["up", "-d", *services_to_start], cwd=str(app_dir), check=True)
        print("Infrastructure services started.")

    except subprocess.CalledProcessError as e:
        print(f"Warning: Failed to manage docker services: {e}")
        print("Continuing with local startup...")
    except FileNotFoundError:
        print("Warning: 'docker' command not found. Skipping docker startup.")


def main():
    script_dir = Path(__file__).resolve().parent
    app_dir = script_dir.parent

    # 0. Ensure .env file
    ensure_env_file(app_dir)

    # 1. Start Docker Dependencies (if any)
    start_docker_dependencies(app_dir)

    # 2. Run the Application
    target_module = "apps.design_skills_engine.main"
    cmd = [sys.executable, "-m", target_module]

    if len(sys.argv) > 1:
        cmd.extend(sys.argv[1:])

    print(f"Starting application: {' '.join(cmd)}")

    # Replace current process
    if sys.platform != "win32":
        os.execvp(cmd[0], cmd)
    else:
        subprocess.run(cmd)


if __name__ == "__main__":
    main()
