import { spawn } from 'node:child_process';
import { dirname, resolve } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { fileURLToPath } from 'node:url';
import { GEO_E2E_BASE_URL, GEO_E2E_PORT } from './playwright.runtime';

const frontendDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(frontendDir, '../../..');
const pythonBin = resolve(repoRoot, '.venv/bin/python');

async function isHealthy(): Promise<boolean> {
  try {
    const response = await fetch(`${GEO_E2E_BASE_URL}/api/health`);
    return response.ok;
  } catch {
    return false;
  }
}

export default async function globalSetup(): Promise<() => Promise<void>> {
  if (await isHealthy()) {
    return async () => {};
  }

  let stdout = '';
  let stderr = '';
  const child = spawn(
    pythonBin,
    ['-m', 'apps.Legacy_modernization_geo_platform.main', '--host', '127.0.0.1', '--port', String(GEO_E2E_PORT)],
    {
      cwd: repoRoot,
      env: {
        ...process.env,
        GEO_PLATFORM_PORT: String(GEO_E2E_PORT),
        GEO_PLATFORM_PUBLIC_BASE_URL: GEO_E2E_BASE_URL,
        GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE: '1',
      },
      stdio: ['ignore', 'pipe', 'pipe'],
    },
  );

  child.stdout?.on('data', (chunk: Buffer) => {
    stdout += chunk.toString();
  });
  child.stderr?.on('data', (chunk: Buffer) => {
    stderr += chunk.toString();
  });

  const deadline = Date.now() + 120_000;
  while (Date.now() < deadline) {
    if (await isHealthy()) {
      return async () => {
        if (child.exitCode === null) {
          child.kill('SIGTERM');
          await delay(500);
        }
      };
    }
    if (child.exitCode !== null) {
      throw new Error(
        `GEO backend exited before becoming healthy.\nstdout:\n${stdout}\nstderr:\n${stderr}`,
      );
    }
    await delay(500);
  }

  if (child.exitCode === null) {
    child.kill('SIGTERM');
  }
  throw new Error(
    `Timed out waiting for GEO backend health check.\nstdout:\n${stdout}\nstderr:\n${stderr}`,
  );
}
