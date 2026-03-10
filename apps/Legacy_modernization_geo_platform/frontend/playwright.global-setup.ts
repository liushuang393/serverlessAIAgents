import { spawn } from 'node:child_process';
import { dirname, resolve } from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';
import { fileURLToPath } from 'node:url';

const frontendDir = dirname(fileURLToPath(import.meta.url));
const repoRoot = resolve(frontendDir, '../../..');
const pythonBin = resolve(repoRoot, '.venv/bin/python');
const baseUrl = 'http://127.0.0.1:18010';

async function isHealthy(): Promise<boolean> {
  try {
    const response = await fetch(`${baseUrl}/api/health`);
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
    ['-m', 'apps.Legacy_modernization_geo_platform.main', '--host', '127.0.0.1', '--port', '18010'],
    {
      cwd: repoRoot,
      env: {
        ...process.env,
        GEO_PLATFORM_PORT: '18010',
        GEO_PLATFORM_PUBLIC_BASE_URL: baseUrl,
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
